# FortCov

A modern coverage analysis tool specifically designed for Fortran projects.

## Installation

```bash
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov
fpm build --profile release
```

## Run gvoc on itself

```bash
# From FortCov repository root
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fpm run fortcov -- --exclude='build/*,test/*' --output=gvoc-self-coverage.md
```

## Run fortcov on itself

```bash
# From FortCov repository root
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fpm run fortcov -- --exclude='build/*,test/*' --output=fortcov-self-coverage.md
```

## Alternative: Using helper script

```bash
# Simpler approach using provided script
./scripts/fpm_coverage_bridge.sh root self-coverage.md
```