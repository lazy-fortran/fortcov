# Usage details

## Manual flow

FortCov analyses existing `.gcov` files by default. Produce them yourself, then run:

```bash
fortcov --source=src *.gcov --output=coverage.md
```

`--source` may be given more than once and accepts directories or files.

## CMake projects

Build out of source with coverage flags, run the tests, generate `.gcov` files and
pass them to FortCov:

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

For other build systems, generate `.gcov` files in the same way and invoke FortCov
as above. FortCov is developed and tested with the default fpm build layout.

## Automatic mode

```bash
fortcov --gcov          # alias: --discover-and-gcov
```

This discovers fpm and CMake build directories, runs the build-system tests,
generates `.gcov` files and writes the report. Without arguments FortCov also runs
in this zero-configuration mode and runs the tests unless `--no-auto-test` is
given. The tests must already be built with coverage flags (for example
`fpm test --flag "-fprofile-arcs -ftest-coverage"`); otherwise gcov has no data.

Defaults in automatic mode:

- `.gcov` files are written to the working directory;
- the report is written to `build/coverage/coverage.md`.

## Options

| Option | Meaning |
|---|---|
| `-s, --source PATH` | source directory or file to include |
| `-o, --output PATH` | report file (default `coverage.md`) |
| `-f, --format FORMAT` | `markdown` or `cobertura` |
| `--fail-under PERCENT` | exit with code 2 if total coverage is below `PERCENT` |
| `--gcov` | discover builds and run gcov before analysis |
| `--auto-test`, `--no-auto-test` | run or skip build-system tests |
| `--gcov-executable PATH` | gcov executable to use (alias `--gcov-cmd`) |
| `--gcov-output-dir PATH` | directory for gcov output (default `build/gcov`) |
| `-q, --quiet`, `-v, --verbose` | output verbosity |
| `-h, --help`, `-V, --version` | help and version |

Example with Cobertura output for CI systems that read it:

```bash
fortcov --format=cobertura --output=coverage.xml
```

## Exit codes

| Code | Meaning |
|---|---|
| 0 | success |
| 1 | general failure |
| 2 | coverage below the `--fail-under` threshold |
| 3 | no coverage data found |

## Environment variables

- `FORTCOV_USE_SYNTHETIC_GCOV=1` forces synthetic gcov output; used by the test
  suite.
- `FORTCOV_USE_REAL_GCOV=0` is a deprecated alias for the same setting.
