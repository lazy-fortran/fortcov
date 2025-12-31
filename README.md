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

FortCov, by default, analyzes existing `.gcov` files and does not invoke
`gcov` or run your tests. Use one of the following flows:

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

### Auto-Discovery + gcov (single command)

If you want FortCov to auto-discover FPM/CMake build dirs, run tests under
coverage, and generate `.gcov` before analysis, use the built‑in bridge:

```bash
fortcov --gcov   # alias: --discover-and-gcov
```

Outputs by default:
- `.gcov` files in the working directory
- `build/coverage/coverage.md` Markdown report

## CI Recipe (Fail Under Threshold)

```bash
set -e
# Single-command CI-friendly execution (auto FPM + gcov + report)
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
- Use `fortcov --help` for additional options (quiet mode, thresholds, etc.).

## Example (Minimal FPM Project)

See `examples/minimal` for a tiny FPM project and a helper script `generate_coverage.sh` that builds with coverage, generates `.gcov`, and creates a Markdown report via FortCov.

## License

MIT. See `LICENSE`.
