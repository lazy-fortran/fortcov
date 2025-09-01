# FortCov – Precise FPM Coverage

FortCov turns gfortran/gcov coverage into clear reports. In FPM projects it automatically discovers coverage artifacts and securely invokes `gcov` for you — no manual `gcov` steps required.

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
# 1) Instrument and run tests (FPM has no --coverage flag; use --flag)
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 2) Generate coverage report (auto-discovers build and runs gcov)
fortcov --output=coverage.md
```

The file `coverage.md` contains a project summary and per-file stats.

## CI Recipe (Fail Under Threshold)

```bash
set -e
fpm test --flag "-fprofile-arcs -ftest-coverage"
fortcov --fail-under 80 --output=coverage.md --quiet
```

Exit codes: `0` success, `2` threshold not met, `3` no coverage data.

## Example Output

```bash
$ fortcov --output=coverage.md
📊 Analyzing coverage...
🔍 Orchestrating coverage file discovery...
📦 Using build system integration for discovery: fpm
✅ Discovered 3 coverage files

Coverage Statistics:
  Line Coverage:  72.9%
  Lines Covered: 51 of 70 lines
```

## Notes

- Works with FPM default build layout; also supports CMake/Make projects.
- Use `fortcov --help` for additional options (JSON/HTML output, quiet mode, thresholds, etc.).

## License

MIT. See `LICENSE`.
