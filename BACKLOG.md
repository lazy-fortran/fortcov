# Development Backlog

## CURRENT SPRINT (Critical Defects & Architecture)
### CRITICAL (Test Suite Blockers)

### HIGH (Architecture & Security)

### MEDIUM (Dead Code Cleanup)
- [ ] #284: Remove dead code: orphaned test reference in fpm.toml
- [ ] #286: Remove dead code: build artifacts in repository (CONSOLIDATED)
- [ ] #288: Remove dead code: typo in test performance script
- [ ] #289: Remove dead code: potentially unused error handling functions
- [ ] #292: Dead code audit summary: multiple categories of obsolete code found

### Sprint 1 Features (Deferred - Focus on Defects First)
- [ ] #299: Architecture: Sprint 1 auto-discovery features not yet implemented
- [ ] #278: Implement build system detection for auto-discovery
- [ ] #280: Add auto-discovery configuration options
- [ ] #279: Add auto-test execution to coverage workflows
- [ ] #281: Enhance zero-configuration mode with auto-discovery integration
- [ ] #277: Add auto-discovery of test build and gcov processing

## DOING (Current Work)
- [ ] #284: Remove dead code: orphaned test reference in fpm.toml

## DONE (Completed Work)
- [x] #283: Remove dead code: disabled test files and diff-related code (CONSOLIDATED)
- [x] #246: CLI flag parsing completely broken - all options silently ignored (completed in PR #316)
- [x] #245: Multiple test suite segmentation faults indicate memory safety issues (fixed with allocation guards)
- [x] #297: Security: File deletion vulnerabilities allow temp file persistence (completed in PR #311)
- [x] #296: Security: Multiple input validation vulnerabilities remain unpatched (completed in PR #308)
- [x] #298: Architecture: fortcov_config.f90 violates 1000-line module size limit (completed in PR #306)
- [x] #294: Bug: Test suite hangs with infinite loops causing 3+ minute timeouts (completed in PR #303)
- [x] #293: Bug: Find command syntax error causes test suite hangs and failures (completed in PR #302)
- [x] #300: cleanup: rescue test_format.nml from main branch (completed in PR #301)
- [x] #257: Integration validation script failures and missing patterns (completed in PR #276)
- [x] Circular dependency compilation error: coverage_model.mod corrupted at line 1573 column 24 (completed in PR #282)
- [x] #285: TODO comments consolidated into #283
- [x] #287: Commented-out code consolidated into #283
- [x] #290: Compiled files consolidated into #286
- [x] #291: Placeholder implementations consolidated into #283

## FUTURE SPRINTS (High-level Planning)

### Sprint 2: Documentation and Quality Improvements
- Goal: Resolve documentation inconsistencies and improve code quality
- Approach: Systematic review and cleanup of documentation, refactor problematic code patterns
- Key decisions: Standardize terminology (fpm not fmp), improve error handling granularity

### Sprint 3: Security and Testing Enhancements
- Goal: Strengthen security validation and expand test coverage
- Approach: Review command injection vulnerabilities, consolidate test files
- Key decisions: Enhanced input validation, unified security patterns

## TODO (Future Work - Ordered by Priority)

### MEDIUM (Documentation Errors)
- [ ] #256: Documentation shows incorrect --format flag instead of --output-format
- [ ] #255: find command syntax error in secure file deletion test
- [ ] #254: Typo in troubleshooting section - fmp vs fpm
- [ ] #253: Typo in GitLab CI documentation - fmp vs fpm
- [ ] #252: Typo in installation instructions - fmp vs fpm
- [ ] #262: Typo in troubleshooting.md - 'fmp' should be 'fpm'
- [ ] #263: Remove excessively long test filename in examples
- [ ] #264: Improve fortcov path detection in test_readme_workflow_issue_260.f90
- [ ] #266: Fix ineffective directory change in test_zero_configuration_issue_249
- [ ] #267: Potential command injection in test shell commands
- [ ] #268: Consolidate redundant issue #249 test files

### LOW (Code Quality Improvements)
- [ ] #270: Refactor hardcoded array size in namelist parsing
- [ ] #271: Improve iostat error handling granularity
- [ ] #275: Remove unused tempfile import in test_makefile_syntax.py
- [ ] #312: Optimize pattern matching performance in security assessment
- [ ] #313: Enhance security assessment documentation and risk prioritization
- [ ] #314: Fix remaining 2 security test pattern matching issues