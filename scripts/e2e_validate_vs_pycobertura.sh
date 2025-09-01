#!/usr/bin/env bash
set -euo pipefail

# End-to-end validation comparing FortCov markdown vs pycobertura markdown
#
# Requirements:
# - fpm, gcov, gcovr, pycobertura must be available on PATH
# - Run from repository root
#
# Outputs (in build/):
# - coverage.md          (FortCov)
# - coverage.xml         (Cobertura via gcovr)
# - pycobertura.md       (pycobertura)

TEST_TIMEOUT=${TEST_TIMEOUT:-300}

echo "[1/5] Running tests with coverage (timeout=${TEST_TIMEOUT}s)"
timeout "${TEST_TIMEOUT}" fpm test --flag "-fprofile-arcs -ftest-coverage" >/dev/null

echo "[2/5] Generating .gcov artifacts"
# Ensure a clean gcov collection directory
rm -rf build/gcov
mkdir -p build/gcov
find build -name "*.gcda" | xargs -r dirname | sort -u | while read -r d; do
    gcov --object-directory="$d" "$d"/*.gcno >/dev/null 2>&1 || true
done
# Move generated .gcov files from repo root into build/gcov for stable discovery
find . -maxdepth 1 -name "*.gcov" -print0 | xargs -0 -r -I{} mv "{}" build/gcov/

echo "[3/5] Rendering FortCov markdown"
FORTCOV_BIN=$(ls -d build/gfortran_*/app/fortcov 2>/dev/null | head -n1 || true)
if [[ -z "${FORTCOV_BIN}" ]] || [[ ! -x "${FORTCOV_BIN}" ]]; then
    echo "Error: FortCov binary not found under build/gfortran_*/app" >&2
    exit 2
fi
"${FORTCOV_BIN}" --source=src --output=build/coverage.md --quiet

echo "[4/5] Producing Cobertura XML via gcovr"
if ! command -v gcovr >/dev/null 2>&1; then
    echo "Error: gcovr not found on PATH" >&2
    exit 3
fi
# Scope to repo root and src to avoid noisy test/example artifacts; ignore gcov edge errors
gcovr --root . --filter 'src/.*' \
      --object-directory build \
      --xml-pretty -o build/coverage.xml \
      --gcov-ignore-errors=all >/dev/null

echo "[5/5] Rendering pycobertura markdown"
if ! command -v pycobertura >/dev/null 2>&1; then
    echo "Error: pycobertura not found on PATH" >&2
    exit 4
fi
pycobertura show -f markdown build/coverage.xml > build/pycobertura.md

echo "Computing totals and comparing"

# Extract FortCov total percent from markdown TOTAL row
FORTCOV_PCT=$(awk -F'|' '/^\| TOTAL \|/ { gsub(/ /, ""); sub(/%/, "", $5); print $5; exit }' build/coverage.md || true)

# Extract pycobertura total percent from markdown TOTAL row
PYCO_PCT=$(awk -F'|' '/^\| TOTAL/ { gsub(/ /, ""); sub(/%/, "", $5); print $5; exit }' build/pycobertura.md || true)

if [[ -z "${FORTCOV_PCT}" ]] || [[ -z "${PYCO_PCT}" ]]; then
    echo "Error: failed to parse totals (FortCov='${FORTCOV_PCT}', pycobertura='${PYCO_PCT}')" >&2
    exit 5
fi

printf "FortCov total:     %s%%\n" "${FORTCOV_PCT}"
printf "pycobertura total: %s%%\n" "${PYCO_PCT}"

# Compare with tolerance (percentage points)
TOL=${TOL:-0.5}
DIFF=$(python3 - <<PY
f = float("${FORTCOV_PCT}")
p = float("${PYCO_PCT}")
print(abs(f - p))
PY
)

awk -v d="${DIFF}" -v tol="${TOL}" 'BEGIN { if (d>tol) { exit 1 } else { exit 0 } }' || {
    echo "Mismatch exceeds tolerance ${TOL}pp (diff=${DIFF}pp)" >&2
    exit 6
}

echo "Match within tolerance (${TOL}pp)."
