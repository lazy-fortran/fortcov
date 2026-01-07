# FortCov – Precise FPM Coverage

> **Note**: This project is experimental and subject to major changes. APIs may change without notice.

FortCov turns gfortran/gcov coverage into clear reports. By default it analyzes
existing `.gcov` files. You can also use the built-in bridge with `--gcov`
(alias: `--discover-and-gcov`) to auto-discover FPM/CMake build dirs and
generate `.gcov` from coverage artifacts before analysis. For manual control,
use `gcov` (or helper scripts in `scripts/`) to generate `.gcov`, then run
FortCov to create the report.

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

## Quick Start

FortCov analyzes existing `.gcov` files by default. In zero-config mode
(no arguments), it also runs your build-system tests unless you pass
`--no-auto-test`. FortCov does not add coverage flags, so you still need
to build or test with coverage instrumentation. Use one of the following
flows:

- Manual `.gcov` flow (explicit control)
- Auto-discovery + gcov bridge (`--gcov`)

```bash
# 1) Instrument and run (tests or app) to produce .gcda/.gcno
fpm test --flag "-fprofile-arcs -ftest-coverage"  # or: fpm build --flag ... && ./build/gfortran_*/app/your_app ...

# 2) Generate .gcov files from build directories
find build -name "*.gcda" | xargs dirname | sort -u | while read d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done

# 3) Generate FortCov report from .gcov files
fortcov --source=src *.gcov --output=coverage.md
```

Note: The file `coverage.md` (default `build/coverage/coverage.md` in
auto mode with `--gcov`) contains a project summary and per-file stats.

## CMake Quick Start (Out-of-Source)

If you build with CMake, you can generate coverage in a separate build
directory and feed `.gcov` files to FortCov:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_Fortran_FLAGS="-fprofile-arcs -ftest-coverage" \
  -DCMAKE_EXE_LINKER_FLAGS="--coverage"
cmake --build build
ctest --test-dir build

find build -name "*.gcda" | xargs dirname | sort -u | while read d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done

fortcov --source=src *.gcov --output=coverage.md
```

### Auto-Discovery + gcov (single command)

If you want FortCov to auto-discover FPM/CMake build dirs, run tests (unless
disabled), and generate `.gcov` before analysis, use the built‑in bridge:

```bash
fortcov --gcov   # alias: --discover-and-gcov
```

Ensure your tests or builds use coverage flags (for example,
`fpm test --flag "-fprofile-arcs -ftest-coverage"`), or gcov will have no
coverage data to process.

Outputs by default:
- `.gcov` files in the working directory
- `build/coverage/coverage.md` Markdown report

## CI Recipe (Fail Under Threshold)

```bash
set -e
# Single-command CI-friendly execution (auto test + gcov + report)
fortcov --gcov --fail-under 80 --quiet
```

Exit codes: `0` success, `2` threshold not met, `3` no coverage data.

## Example Output

```bash
$ fortcov --source=src *.gcov --output=coverage.md
| Filename | Stmts | Covered | Cover | Missing |
|----------|-------|---------|-------|---------|
| src/example.f90 | 10 | 7 | 70.00% | 3-4, 9 |
| TOTAL | 10 | 7 | 70.00% | |
```

Advanced: You can still use the built-in bridge explicitly when needed:

```bash
$ fortcov --gcov --output=coverage.md  # alias: --discover-and-gcov
# Auto-discovers build dirs, generates .gcov, and produces coverage.md
```

## Notes

- Developed and tested with FPM default build layout.
- For non-FPM builds, generate `.gcov` with your build system and invoke FortCov as shown above.
- Set `FORTCOV_USE_SYNTHETIC_GCOV=1` to force synthetic gcov output for tests.
- `FORTCOV_USE_REAL_GCOV=0` is a deprecated alias for synthetic gcov output.
- Use `fortcov --help` for additional options (quiet mode, thresholds, etc.).

## Example (Minimal FPM Project)

See `examples/minimal` for a tiny FPM project and a helper script `generate_coverage.sh` that builds with coverage, generates `.gcov`, and creates a Markdown report via FortCov.

## License

MIT. See `LICENSE`.
