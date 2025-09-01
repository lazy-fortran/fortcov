# FortCov – Precise FPM Coverage

FortCov turns gfortran/gcov coverage into clear reports. By default it analyzes
existing `.gcov` files. You can also use the built-in bridge with `--gcov`
(alias: `--discover-and-gcov`) to auto-discover FPM/CMake build dirs and
generate `.gcov` from coverage artifacts before analysis. For manual control,
use `gcov` (or helper scripts in `scripts/`) to generate `.gcov`, then run
FortCov to create the report.

See the consolidated coverage workflow: `doc/coverage_workflow.md`.

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

## Quick Start (Single Command)

From the root of your project, simply run:

```bash
fortcov
```

What happens:
- Auto-detects `fpm.toml` and runs `fpm test` with coverage flags.
- Auto-discovers or generates `.gcov` from coverage artifacts.
- Produces a coverage report at `build/coverage/coverage.md` by default.

That’s it — one binary, one command.

If you prefer manual control (or a non-FPM build), you can still follow the
explicit steps below. See `doc/coverage_workflow.md` for details.

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
zero-config mode) contains a project summary and per-file stats.

## CI Recipe (Fail Under Threshold)

```bash
set -e
# Single-command CI-friendly execution (auto FPM + gcov + report)
fortcov --fail-under 80 --quiet
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

- Works with FPM default build layout; also supports CMake/Make projects.
- Meson is supported via example-based integration; see
  `examples/build_systems/meson/` for a working template.
- Use `fortcov --help` for additional options (JSON/HTML output, quiet mode, thresholds, etc.).

## License

MIT. See `LICENSE`.
