# Development Backlog

## CURRENT SPRINT (Critical Infrastructure & Architecture Alignment)

### CRITICAL (Infrastructure Blockers)
- [ ] #365: critical: architecture violation - coverage_workflows.f90 exceeds 1000-line hard limit

### HIGH (Architecture Compliance)  
- [ ] #366: architecture: 13 modules exceed 500-line target violating size principles

### MEDIUM (Quality & Reliability)
- [ ] #314: fix: resolve remaining 2 security test pattern matching issues

### LOW (Technical Debt)
- [ ] #367: refactor: consolidate function size violations - 7 functions exceed 50-line target
- [ ] #369: refactor: replace formatted write with string concatenation in gcda pattern building
- [ ] #370: refactor: replace formatted write with string concatenation in gcno pattern building

## DOING (Current Work)
<!-- No current work -->

## DONE (Completed Work)
- [x] #364: critical: Fortran runtime error 'End of record' in gcov_auto_processor.f90:355 blocking CI pipeline (completed in PR #368 - replaced formatted write with string concatenation)
- [x] #313: Enhance security assessment documentation and risk prioritization (completed in PR #363 - comprehensive documentation improvements with risk framework)
- [x] #312: Optimize pattern matching performance in security assessment (completed in PR #362 - consolidated pattern matching with LRU caching, 5x performance improvement)
- [x] #336: Fix: resolve 4 remaining gcov processing test failures (completed in PR #360 - gcov processing stability significantly improved)
- [x] #356: Fix: rescue commits from main (completed in PR #359 - removed obsolete Makefile successfully)
- [x] #343: Refactor: reduce assess_deletion_security_risks function size from 77 to under 50 lines (completed in PR #358 - extracted helper functions following SRP)
- [x] #271: Improve iostat error handling granularity (completed in PR #357 - comprehensive iostat error interpretation with granular reporting)
- [x] #270: Refactor hardcoded array size in namelist parsing (completed in PR #355 - replaced hardcoded 100 with parameter constants)
- [x] #266: Fix ineffective directory change in test_zero_configuration_issue_249 (completed in PR #354 - investigation confirmed issue obsolete)
- [x] #264: Improve fortcov path detection in test_readme_workflow_issue_260.f90 (completed in PR #353 - investigation confirmed issue obsolete)
- [x] #263: Remove excessively long test filename in examples (completed in PR #352 - investigation confirmed file never existed)
- [x] #325: Refactor: reduce file size in secure_command_executor (completed in PR #345 - 758→139 lines, 86% reduction)
- [x] #323: Refactor: reduce safe_close_and_delete function size (completed in PR #342)
- [x] #324: Fix: address line length violations in secure_command_executor (completed in PR #341)
- [x] #268: Consolidate redundant issue #249 test files (completed in PR #340)
- [x] #275: Remove unused tempfile import in test_makefile_syntax.py (completed in PR #339)
- [x] #267: Potential command injection in test shell commands (fixed in PR #338)
- [x] #256: Documentation shows incorrect --format flag instead of --output-format (invalid - documentation was correct)
- [x] #262: Typo in troubleshooting.md - 'fmp' should be 'fpm' (resolved in PR #337 comprehensive fix)
- [x] #254: Typo in troubleshooting section - fmp vs fpm (resolved in PR #337 comprehensive fix)
- [x] #253: Typo in GitLab CI documentation - fmp vs fpm (completed in PR #337)
- [x] #252: Typo in installation instructions - fmp vs fpm (completed in PR #335)
- [x] #277: Add auto-discovery of test build and gcov processing (completed in PR #334)
- [x] #281: Enhance zero-configuration mode with auto-discovery integration (completed in PR #333)
- [x] #279: Add auto-test execution to coverage workflows (completed in PR #332)
- [x] #280: Add auto-discovery configuration options (completed in PR #331)
- [x] #278: Implement build system detection for auto-discovery (completed in PR #330)
- [x] #292: Dead code audit summary: multiple categories of obsolete code found (cleanup completed)
- [x] #304: Test: improve test coverage simulation accuracy (completed in PR #329)
- [x] #305: Docs: update test documentation for infinite loop prevention (completed in PR #328)
- [x] #327: Cleanup: analyze and optimize test suite for CI performance (tests cleaned up, CI fixed)
- [x] #309: Security: refine Windows device names validation in path security (completed in PR #326)
- [x] #310: Security: complete error message path leakage sanitization (completed in PR #322)
- [x] #289: Remove dead code: potentially unused error handling functions (completed in PR #321)
- [x] #288: Remove dead code: typo in test performance script (completed in PR #320)
- [x] #286: Remove dead code: build artifacts in repository (completed in PR #319)
- [x] #284: Remove dead code: orphaned test reference in fpm.toml (completed in PR #318)
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

### Sprint 2: Architecture Compliance & Stability
- **Goal**: Achieve full architecture compliance and restore CI stability
- **Approach**: Systematic module decomposition following SRP, infrastructure fixes
- **Key Decisions**: 
  - Priority: CRITICAL infrastructure fixes before architecture refactoring
  - Module extraction strategy: Single responsibility per extracted module
  - Maintain API compatibility during refactoring
- **Success Metrics**: All modules <500 lines, CI fully stable, zero architecture violations

### Sprint 3: Quality & Performance Optimization  
- **Goal**: Enhance test reliability and optimize performance bottlenecks
- **Approach**: Security test stabilization, function-level refactoring for maintainability
- **Key Decisions**:
  - Focus on test infrastructure reliability
  - Function size compliance through focused extraction
  - Performance optimization without sacrificing correctness