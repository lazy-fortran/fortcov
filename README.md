# FortCov

FortCov turns gfortran/gcov coverage data into Markdown or Cobertura XML reports
for Fortran projects built with fpm or CMake.

Status: experimental; options and output may change.

## Features

- Reads existing `.gcov` files and writes a per-file summary with missing lines.
- Optionally discovers fpm/CMake build directories, runs the tests and calls `gcov`
  itself (`--gcov`).
- Markdown (default) or Cobertura XML output.
- Coverage threshold for CI (`--fail-under`), with distinct exit codes.

## Requirements

- `gfortran` and `gcov` (part of GCC)
- [fpm](https://fpm.fortran-lang.org)

## Install

```bash
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov
fpm build --profile release
sudo install -m 0755 "$(find build -type f -path '*/app/fortcov' | head -n1)" /usr/local/bin/fortcov
```

## Usage

FortCov does not add coverage flags; build and run your tests with instrumentation
first.

```bash
# 1. Build and run tests with coverage instrumentation
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 2. Generate .gcov files from the build directories
find build -name "*.gcda" | xargs dirname | sort -u | while read d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done

# 3. Write the report
fortcov --source=src *.gcov --output=coverage.md
```

Output:

```
| Filename | Stmts | Covered | Cover | Missing |
|----------|-------|---------|-------|---------|
| src/example.f90 | 10 | 7 | 70.00% | 3-4, 9 |
| TOTAL | 10 | 7 | 70.00% | |
```

Steps 2 and 3 can be replaced by `fortcov --gcov`, which discovers the build
directories, runs the tests (unless `--no-auto-test` is given), generates the
`.gcov` files and writes `build/coverage/coverage.md`. In CI:

```bash
fortcov --gcov --fail-under 80 --quiet
```

`examples/minimal` contains a small fpm project with a `generate_coverage.sh`
script that performs the manual steps above.

## Documentation

- [Usage details](docs/usage.md): CMake projects, automatic mode, options, exit
  codes, environment variables
- `fortcov --help` lists all options
- [Changelog](CHANGELOG.md)

## License

MIT. See [LICENSE](LICENSE).
