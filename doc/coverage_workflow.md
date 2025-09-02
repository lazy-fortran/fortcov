# FortCov Coverage Workflow (FPM)

This document provides a single, canonical coverage workflow for FPM-based
projects. It consolidates prior guidance and ensures copy-paste runnable
commands.

FortCov consumes `.gcov` files produced by `gcov`. It can also auto-discover
build directories and generate `.gcov` via the built-in bridge when invoked
with `--gcov` (alias: `--discover-and-gcov`).

## Options

- Single command (recommended): Just run `fortcov` and let it do everything.
- Manual steps: Equivalent explicit commands if you prefer precise control.

## A) Single Command (Recommended)

Requirements: `gfortran`, `gcov`, `fpm`.

From the root of your FPM project:

```bash
# Auto-detect FPM, run tests with coverage, generate .gcov, produce coverage.md
fortcov --gcov   # alias: --discover-and-gcov
```
Outputs (defaults):
- `.gcov` files in the working directory
- `build/coverage/coverage.md`: Markdown report (project summary and per-file stats)

Exit codes (FortCov): `0` success, `2` threshold not met, `3` no coverage data.

## B) Manual Steps

Run these from the root of your FPM project:

```bash
# 1) Instrument and run (tests or app) to produce .gcda/.gcno
fpm test --flag "-fprofile-arcs -ftest-coverage"
# Or run the instrumented app to collect application coverage:
# fpm build --flag "-fprofile-arcs -ftest-coverage"
# ./build/gfortran_*/app/your_app ...

# 2) Generate .gcov files from FPM build directories
find build -name "*.gcda" | xargs dirname | sort -u | while read d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done

# 3) Generate FortCov report from .gcov files
fortcov --source=src *.gcov --output=coverage.md
```

Notes:
- For application source coverage, ensure you run the instrumented application
  (not only the tests) so that `.gcda` files are produced for sources in `src/`.
- For CI, add `--fail-under <min>` and `--quiet` if desired.

## CI Example

```bash
set -e
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done
fortcov --fail-under 80 --source=src *.gcov --output=coverage.md --quiet
```

## Notes on built-in bridge

By default, `fortcov` analyzes existing `.gcov` files and does not invoke
`gcov` or run your tests. Use the built-in bridge `--gcov` (alias:
`--discover-and-gcov`) to auto-discover FPM/CMake build directories, run under
coverage, generate `.gcov`, and then analyze them.

Test-environment guard: When `fortcov` detects it is running under a test
harness (e.g., `fpm test`), it skips auto-running tests to avoid recursion.
In controlled CI jobs where this is desired, set the environment variable
`FORTCOV_ALLOW_AUTOTEST=1` to explicitly allow the auto-test bridge.
