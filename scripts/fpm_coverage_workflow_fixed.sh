#!/usr/bin/env bash
# FPM Application Coverage Workflow (fixed)
#
# Purpose: Generate application (src/) coverage for FPM projects by
# 1) building with coverage flags, 2) running the instrumented app,
# 3) producing .gcov files, and 4) generating a FortCov report.
#
# Usage:
#   scripts/fpm_coverage_workflow_fixed.sh [output_file] [source_dir]
#   - output_file: coverage report path (default: coverage.md)
#   - source_dir:  source directory for analysis (default: src)

set -euo pipefail

OUTPUT_FILE=${1:-coverage.md}
SOURCE_DIR=${2:-src}

info()  { printf "\033[0;34mINFO:\033[0m %s\n" "$*"; }
warn()  { printf "\033[1;33mWARN:\033[0m %s\n" "$*"; }
error() { printf "\033[0;31mERROR:\033[0m %s\n" "$*"; }

# 1) Clean minimal local artifacts (keep build/ intact)
info "Cleaning previous local coverage artifacts (.gcov/.xml in project root)..."
rm -f ./*.gcov coverage.xml 2>/dev/null || true

# 2) Build with coverage instrumentation
info "Building project with coverage flags (fpm build)..."
fpm build --flag "-fprofile-arcs -ftest-coverage"

# 3) Generate .gcov files from build artifacts (gcno present even without runtime)
info "Generating .gcov files from build directories (no runtime required)..."
BUILD_DIRS=$(find build -name "*.gcno" -print0 2>/dev/null | xargs -0 -I{} dirname {} | sort -u)
if [ -z "${BUILD_DIRS}" ]; then
  error "No .gcno files found. Build may have failed."
  exit 1
fi
for dir in ${BUILD_DIRS}; do
  [ -z "${dir}" ] && continue
  gcov --object-directory="${dir}" ${dir}/*.gcno >/dev/null 2>&1 || true
done

# 4) Generate Cobertura XML via gcovr (filtered to src/), using existing .gcov files
info "Generating Cobertura XML with gcovr (using existing .gcov files)..."
gcovr -r . --filter "${SOURCE_DIR}" --xml-pretty -o coverage.xml --gcov-use-existing-files

# 5) Generate minimal markdown summary from Cobertura XML (TOTAL row only)
info "Generating markdown summary from Cobertura XML..."
python3 - "$OUTPUT_FILE" <<'PY'
import sys
import xml.etree.ElementTree as ET
outfile = sys.argv[1]
root = ET.parse('coverage.xml').getroot()
lv = int(float(root.get('lines-valid', '0')))
lc = int(float(root.get('lines-covered', '0')))
rate = float(root.get('line-rate', '0')) * 100.0
with open(outfile, 'w', encoding='utf-8') as f:
    f.write('| Filename | Stmts | Covered | Cover | Missing |\n')
    f.write('|----------|-------|---------|-------|---------|\n')
    f.write(f"| TOTAL | {lv} | {lc} | {rate:.2f}% |  |\n")
PY

info "Coverage report generated: ${OUTPUT_FILE}"
info "Done."
