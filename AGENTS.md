# Repository Guidelines

## Project Structure & Module Organization
- `app/`: Entry point (`main.f90`).
- `src/`: Core modules by domain: `config/`, `core/`, `coverage/`, `gcov/`,
  `utils/`, `reporters/`, `syntax/`, `security/`, `zero_config/`.
- `test/`: Unit, integration, security, performance, and CLI tests.
- `scripts/`: CI helpers and utilities (`run_ci_tests.sh`, coverage bridge).
- `build/`: Compiler outputs (ignored in VCS).

## Build, Test, and Development Commands
```bash
# Build
fpm build

# Build with coverage
fpm build --flag "-fprofile-arcs -ftest-coverage"

# Curated CI-safe test suite (timeouts, verified exclusions)
./run_ci_tests.sh

# Run a specific/verbose test
fpm test test_name
fpm test --verbose

# Generate repo coverage
fpm test --flag "-fprofile-arcs -ftest-coverage"
find build -name "*.gcda" | xargs dirname | sort -u | while read d; do
  gcov --object-directory="$d" "$d"/*.gcno 2>/dev/null || true
done
./build/gfortran_*/app/fortcov --source=src *.gcov --output=coverage.md
```

## Coding Style & Naming Conventions
- 4-space indent, 88 cols, UNIX newlines.
- Small units: functions <50 lines; modules <500.
- Fortran: derived types as `typename_t`; prefer `move_alloc()`; avoid returning
  allocatables; implement deep-copy for nested members.
- No commented-out code or secrets.

## Testing Guidelines
- FPM-based tests in `test/`; do not skip/xfail without linked issue.
- Primary runner: `./run_ci_tests.sh` (timeouts, fork-bomb prevention).
- Treat local vs CI differences as blockers; attach logs to PRs.
- Produce `coverage.md` when validating coverage changes.

## Commit & Pull Request Guidelines
- Conventional Commits (e.g., `feat: add TUI summary`, `fix: handle gcov err`).
- Stage specific files only (avoid `git add .`); keep commits single-scope.
- Reference issues (e.g., `fixes #123`).
- PRs: clear rationale, commands run, and test/coverage outputs; no merges with
  failing CI.

## Security & Configuration Tips
- Respect fork-bomb prevention markers and safe execution utilities in `utils/`.
- Validate paths/inputs; avoid unvalidated shell execution.
- Config via `fortcov.nml`; prefer CLI flags in CI (`--quiet`, `--fail-under`).

