# FortCov Coverage Workflow (FPM)

This document provides a single, canonical coverage workflow for FPM-based
projects. It consolidates prior guidance and ensures copy-paste runnable
commands.

FortCov consumes `.gcov` files produced by `gcov`; it does not invoke `gcov` itself.

## Options

- Bridge script (recommended): Simple, copy-paste friendly helper that runs the
  typical steps and then FortCov. Best for CI and local use.
- Manual steps: Equivalent explicit commands if you prefer not to use scripts.

## A) Bridge Script (Recommended)

Requirements: `gfortran`, `gcov`, `fpm`.

From the root of your FPM project:

```bash
# Generate application coverage end-to-end and write coverage.md
./scripts/fpm_coverage_workflow_fixed.sh coverage.md src

# Note: If `./scripts/fpm_coverage_workflow_fixed.sh` is not present in your
# project, copy it from the FortCov repository's `scripts/` directory or invoke
# it via a full path to that script.

# Or: quick bridge that assumes you already produced .gcda/.gcno
./scripts/fpm_coverage_bridge.sh src

# Note: If `./scripts/fpm_coverage_bridge.sh` is not present in your project,
# copy it from the FortCov repository's `scripts/` directory or invoke it via a
# full path to that script.
```

Outputs:
- `coverage.md`: Markdown report (project summary and per-file stats)
- `.gcov` files in the working directory

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

## Future: Built-in auto-gcov

An integrated "auto-gcov" mode may be added to FortCov in the future. Until
then, prefer the bridge scripts above or use the manual steps.
