# FortCov – Precise FPM Coverage

FortCov turns gfortran/gcov coverage into clear reports. It analyzes `.gcov` files produced by `gcov`; it does not invoke `gcov` itself. For FPM builds, use `gcov` (or the helper script in `scripts/`) to generate `.gcov` files, then run FortCov to create the report.

## Requirements

- `gfortran` and `gcov` (part of GCC)
- `fpm` (Fortran Package Manager)

## Install FortCov

```bash
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov
fpm build --profile release
sudo install -m 0755 "$(find build -type f -path '*/app/fortcov' | head -n1)" /usr/local/bin/fortcov
```

## FPM Quick Start (Copy/Paste)

Run these from the root of your FPM project:

```bash
# 1) Instrument and run (tests or app) to produce .gcda/.gcno
fpm test --flag "-fprofile-arcs -ftest-coverage"  # or: fpm build --flag ... && ./build/gfortran_*/app/your_app ...

# 2) Generate .gcov files from FPM build directories
find build -name "*.gcda" | xargs dirname | sort -u | while read d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done

# 3) Generate FortCov report from .gcov files
fortcov --source=src *.gcov --output=coverage.md
```

The file `coverage.md` contains a project summary and per-file stats.

Tip: A convenience script is available: `scripts/fpm_coverage_bridge.sh src` will perform steps 1–3 and then run FortCov. For end-to-end application coverage, see `scripts/fpm_coverage_workflow_fixed.sh` and `FPM_COVERAGE_WORKFLOW_FIX.md`.

## CI Recipe (Fail Under Threshold)

```bash
set -e
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done
fortcov --fail-under 80 --source=src *.gcov --output=coverage.md --quiet
```

Exit codes: `0` success, `2` threshold not met, `3` no coverage data.

## Example Output

```bash
$ fortcov --source=src *.gcov --output=coverage.md
Coverage Statistics:
  Line Coverage:  72.9%
  Lines Covered: 51 of 70 lines
```

## Notes

- Works with FPM default build layout; also supports CMake/Make projects.
- Use `fortcov --help` for additional options (JSON/HTML output, quiet mode, thresholds, etc.).

## License

MIT. See `LICENSE`.
