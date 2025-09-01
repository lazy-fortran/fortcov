# FortCov Coverage Workflow (FPM)

This document provides a single, canonical coverage workflow for FPM-based
projects. It consolidates prior guidance and ensures copy-paste runnable
commands.

FortCov consumes `.gcov` files produced by `gcov`. It can also auto-discover
build directories and generate `.gcov` via the built-in bridge when invoked
with `--gcov` (alias: `--discover-and-gcov`).

## Options

- Bridge script (recommended): Simple, copy-paste friendly helper that runs the
  typical steps and then FortCov. Best for CI and local use.
- Manual steps: Equivalent explicit commands if you prefer not to use scripts.

## A) Built-in Bridge (Recommended)

Requirements: `gfortran`, `gcov`, `fpm`.

From the root of your FPM project:

```bash
## Option 1: FortCov built-in

```bash
# Auto-discover build dirs, generate .gcov, produce coverage.md
fortcov --gcov --output=coverage.md  # alias: --discover-and-gcov
```

## Option 2: Bridge scripts

```bash
# Generate application coverage end-to-end and write coverage.md
./scripts/fpm_coverage_workflow_fixed.sh coverage.md src

# Or: quick bridge that assumes you already produced .gcda/.gcno
./scripts/fpm_coverage_bridge.sh src
```

Notes:
- If a script is not present in your project, copy it from this repository's
  `scripts/` directory or run it via a full path from the FortCov repository.
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

## Notes on built-in bridge

The built-in bridge uses safe, internal discovery and does not execute user-
provided `gcov` commands. Prefer `--gcov` for the simplest setup, and use the
bridge scripts or manual steps when you need explicit control.
