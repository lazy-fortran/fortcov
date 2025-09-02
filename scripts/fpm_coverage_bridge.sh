#!/usr/bin/env bash
set -euo pipefail

# Lightweight auto mode bridge: run tests and generate .gcov files
# - Tries FPM first; falls back to CTest if present
# - Then runs gcov over discovered .gcda directories

log() { printf '%s\n' "$*"; }

run_fpm_tests() {
  if command -v fpm >/dev/null 2>&1; then
    log "[fortcov] Running fpm tests (if any)"
    # Do not fail the bridge if tests fail; discovery can still proceed
    FPM_BACKTRACE=0 fpm test || true
  fi
}

run_ctest() {
  if command -v ctest >/dev/null 2>&1; then
    if [ -d build ] || [ -d _build ] || rg -q "add_test\(" -S CMakeLists.txt 2>/dev/null; then
      log "[fortcov] Running ctest (if configured)"
      (cd build 2>/dev/null && ctest --output-on-failure || true) || true
      (cd _build 2>/dev/null && ctest --output-on-failure || true) || true
    fi
  fi
}

generate_gcov() {
  if ! command -v gcov >/dev/null 2>&1; then
    log "[fortcov] gcov not found in PATH; skipping generation"
    return 0
  fi
  log "[fortcov] Generating .gcov files from discovered .gcda"
  # Find unique directories containing .gcda files and run gcov there
  mapfile -t dirs < <(find . -type f -name '*.gcda' -printf '%h\n' 2>/dev/null | sort -u)
  for d in "${dirs[@]:-}"; do
    [ -n "$d" ] || continue
    ( cd "$d" && gcov -b -c . >/dev/null 2>&1 || true ) || true
  done
}

run_fpm_tests || true
run_ctest || true
generate_gcov || true

exit 0

