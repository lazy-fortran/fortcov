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
info "Cleaning previous local coverage artifacts (.gcov in project root)..."
find . -maxdepth 1 -name "*.gcov" -delete 2>/dev/null || true

# 2) Build with coverage instrumentation
info "Building project with coverage flags (fpm build)..."
fpm build --flag "-fprofile-arcs -ftest-coverage"

# 3) Locate instrumented executable(s) in build tree
APP_BIN=$(find build -type f -path "*/app/*" -executable 2>/dev/null | head -n1 || true)
if [ -z "${APP_BIN}" ]; then
  error "No instrumented application executable found under build/."
  error "Expected something like build/gfortran_*/app/<name>."
  exit 1
fi
info "Using instrumented app: ${APP_BIN}"

# 4) Execute a few safe scenarios to produce runtime coverage (.gcda)
info "Executing instrumented application scenarios to exercise code paths..."
"${APP_BIN}" --help    >/dev/null 2>&1 || true
"${APP_BIN}" --version >/dev/null 2>&1 || true
"${APP_BIN}" --validate >/dev/null 2>&1 || true

# 5) Generate .gcov files from all build directories containing .gcda
info "Generating .gcov files from build directories..."
BUILD_DIRS=$(find build -name "*.gcda" -print0 2>/dev/null | xargs -0 -I{} dirname {} | sort -u)
if [ -z "${BUILD_DIRS}" ]; then
  error "No .gcda files found. Ensure the app actually ran to produce runtime data."
  exit 1
fi

GEN_COUNT=0
while IFS= read -r dir; do
  [ -z "${dir}" ] && continue
  if compgen -G "${dir}/*.gcno" >/dev/null; then
    ( cd "${dir}" && gcov *.gcno >/dev/null 2>&1 || true )
    # Move generated .gcov to project root for consistent consumption
    count_here=$(find "${dir}" -maxdepth 1 -name "*.gcov" | wc -l | tr -d ' ')
    if [ "${count_here}" -gt 0 ]; then
      mv "${dir}"/*.gcov . 2>/dev/null || true
      GEN_COUNT=$((GEN_COUNT + count_here))
    fi
  fi
done <<< "${BUILD_DIRS}"

if [ "${GEN_COUNT}" -eq 0 ]; then
  error "gcov did not produce any .gcov files. Check build artifacts."
  exit 1
fi
info "Generated ${GEN_COUNT} .gcov files. Filtering empties..."

# 6) Drop trivial/empty .gcov files with no coverage payload
MEANINGFUL=0
for f in ./*.gcov; do
  [ -f "$f" ] || continue
  lines=$(wc -l < "$f" | tr -d ' ')
  if [ "$lines" -le 1 ]; then
    rm -f "$f"
  else
    MEANINGFUL=$((MEANINGFUL + 1))
  fi
done

if [ "${MEANINGFUL}" -eq 0 ]; then
  error "No meaningful .gcov files after filtering; application paths may not be exercised."
  exit 1
fi
info "${MEANINGFUL} meaningful .gcov files remain. Running FortCov..."

# 7) Run FortCov analysis (zero-config to avoid positional arg limits)
if command -v fortcov >/dev/null 2>&1; then
  FORTCOV=fortcov
else
  # Try local build path
  FORTCOV=$(find build -type f -path "*/app/fortcov" -executable 2>/dev/null | head -n1 || true)
  if [ -z "${FORTCOV}" ]; then
    error "Cannot find fortcov executable (system or local build)."
    exit 1
  fi
fi

set +e
${FORTCOV} --source="${SOURCE_DIR}" ./*.gcov --output="${OUTPUT_FILE}"
RC=$?
set -e
if [ $RC -ne 0 ]; then
  error "FortCov returned non-zero exit code: $RC"
  exit $RC
fi

info "Coverage report generated: ${OUTPUT_FILE}"
info "Done."

