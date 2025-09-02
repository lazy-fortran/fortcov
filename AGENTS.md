# Repository Guidelines

## Project Structure & Module Organization
- `app/`: Entry point (`main.f90`).
- `src/`: Core modules by domain: `config/`, `core/`, `coverage/`, `gcov/`,
  `utils/`, `reporters/`, `syntax/`, `security/`, `zero_config/`.
- `test/`: Unit, integration, security, performance, and CLI tests.
- `scripts/`: CI helpers and utilities.
- `build/`: Compiler outputs (ignored in VCS).

## Build, Test, and Development Commands
```bash
# Build
fpm build

# Build with coverage
fpm build --flag "-fprofile-arcs -ftest-coverage"

# Run all tests
fpm test

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
- Use `fpm test` locally and in CI.
- Keep tests deterministic and self-contained; do not skip/xfail without a linked issue.
- Treat local vs CI differences as blockers; attach logs to PRs.
- Generate `coverage.md` when validating coverage changes, if relevant.

### CI
- CI runs `fpm test` directly. Avoid custom wrappers unless strictly necessary.

## Commit & Pull Request Guidelines
- Conventional Commits (e.g., `feat: add TUI summary`, `fix: handle gcov err`).
- Stage specific files only (avoid `git add .`); keep commits single-scope.
- Reference issues (e.g., `fixes #123`).
- PRs: clear rationale, commands run, and test/coverage outputs; no merges with
  failing CI.

## Security & Configuration Tips
- Validate paths/inputs; avoid unvalidated shell execution.
- Config via `fortcov.nml`; prefer CLI flags in CI (`--quiet`, `--fail-under`).
