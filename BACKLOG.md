# Development Backlog

## CURRENT SPRINT (Auto-Discovery Feature)
- [ ] #277: Add auto-discovery of test build and gcov processing
- [ ] #278: Implement build system detection for auto-discovery
- [ ] #279: Add auto-test execution to coverage workflows
- [ ] #280: Add auto-discovery configuration options
- [ ] #281: Enhance zero-configuration mode with auto-discovery integration
- [ ] #246: CLI flag parsing completely broken - all options silently ignored
- [ ] #245: Multiple test suite segmentation faults indicate memory safety issues

## DOING (Current Work)

## DONE (Completed Work)
- [x] #257: Integration validation script failures and missing patterns (completed in PR #276)
- [x] Circular dependency compilation error: coverage_model.mod corrupted at line 1573 column 24 (completed in PR #282)

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