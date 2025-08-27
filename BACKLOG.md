# Development Backlog

## SPRINT_BACKLOG (User Experience Excellence - Sprint 6)

### EPIC: Critical User Experience Defects (HIGHEST PRIORITY)
- [ ] #576: docs: getting-started guide uses incorrect fpm command
- [ ] #579: bug: getting-started guide function mismatch breaks compilation
- [ ] #581: bug: make example contains failing test for palindrome function
- [ ] #583: bug: unused variable warning in make example affects user experience
- [ ] #585: docs: getting-started guide lacks error recovery instructions
- [ ] #586: enhancement: examples should verify fortcov availability before use
- [ ] #587: bug: test suite fails with missing test executable
- [ ] #588: bug: FPM integration examples fail validation patterns
- [ ] #551: bug: --exclude option does not filter files from coverage report

### EPIC: Code Quality & Standards Compliance (HIGH PRIORITY)
- [ ] #572: refactor: remove commented-out debug code in foundation_utils
- [ ] #573: fix: syntax highlighter references undefined types
- [ ] #574: refactor: eliminate duplicate type initialization procedures
- [ ] #575: security: replace hardcoded /tmp paths with portable temporary directory detection
- [ ] #577: refactor: standardize utility module naming convention
- [ ] #582: defect: source files approaching size limits need attention
- [ ] #584: fix: incorrect XML well-formed validation logic

### EPIC: Technical Debt Consolidation (MEDIUM PRIORITY)
- [ ] #520: defect: consolidate duplicate MAX_PATH constants across modules
- [ ] #521: defect: consolidate duplicate MAX_FILES constants
- [ ] #522: defect: consolidate duplicate MAX_FILENAME_LENGTH constants
- [ ] #523: defect: consolidate duplicate MAX_COMMAND_LENGTH constants
- [ ] #524: defect: missing error handling for memory allocation in main.f90
- [ ] #525: defect: missing error handling for memory allocations in gcov_line_parser.f90
- [ ] #544: defect: duplicate assert functions across test files
- [ ] #549: defect: excessive return statements indicating complex control flow
- [ ] #552: defect: facade modules that only re-export functionality
- [ ] #553: defect: unsafe execute_command_line usage in tests

### EPIC: Sprint Completion & Validation (FINAL)
- [ ] #589: docs: Sprint 6 user experience excellence validation and consolidation

## DOING (Current Work)

## PRODUCT_BACKLOG (High-level Features)
- [ ] Advanced Coverage Analytics & Reporting
- [ ] Performance Optimization & Scalability  
- [ ] Enhanced Integration & Compatibility
- [ ] User Experience & Developer Productivity

## DONE (Completed Product Features)
- [x] Infrastructure Stabilization (Sprint 2 Phase 2 - All critical infrastructure bugs resolved)
- [x] User Experience & Documentation (Sprint 2 Phase 3 - All documentation consolidated and validated)
- [x] Architecture Compliance (Sprint 2 Phase 4 - All architecture violations resolved and compliance verified)
- [x] Documentation Defects (HIGH - All documentation fixes completed)
- [x] Code Cleanup (Sprint 2 Phase 5 - All cleanup tasks completed including final style fixes)
- [x] Critical Functionality Recovery (Sprint 2 - Core fortcov functionality restored and auto-discovery working)
- [x] Code Quality & Defect Resolution (Sprint 3 - Architecture compliance progress, style fixes, test improvements)
- [x] Architecture Compliance & User Experience (Sprint 4 - FAILED due to build system regression)
- [x] Critical Build Recovery (Sprint 5 Phase 1 - All critical build failures resolved)
- [x] Architecture Compliance Recovery (Sprint 5 Phase 2 - Consistent module naming patterns implemented, 108 modules renamed)
- [x] Critical Build Fix & Architecture Recovery (Sprint 5 - Outstanding success with foundation ready for user focus)

## FUTURE SPRINTS (High-level Planning)

### Sprint 3: Architecture Compliance & Code Quality
- **Goal**: Achieve architecture compliance after core functionality recovery
- **Approach**: Systematic module decomposition following SRP, technical debt reduction
- **Key Decisions**: 
  - Priority: Complete only after Sprint 2 critical functionality recovery
  - Module extraction strategy: Single responsibility per extracted module
  - Maintain API compatibility during refactoring
- **Success Metrics**: All modules <500 lines, functions <50 lines, zero architecture violations

### Sprint 4: Performance & Integration Enhancement  
- **Goal**: Optimize performance and expand build system integration
- **Approach**: Performance profiling, additional build system support, advanced features
- **Key Decisions**:
  - Focus on performance bottlenecks after stability achieved
  - Expand beyond FPM to CMake, Make, Meson integration
  - Advanced reporting and analytics features