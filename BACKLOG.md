# Development Backlog

## SPRINT_BACKLOG (Critical Build Fix & Architecture Recovery - Sprint 5)

### EPIC: Architecture Compliance Recovery (HIGH PRIORITY)
- [ ] #548: code: source files exceed recommended size limits
- [ ] #543: architecture: inconsistent module naming patterns violate architectural standards

### EPIC: User Experience & Documentation Defects (HIGH PRIORITY)
- [ ] #550: docs: GitHub Actions example tries to upload non-existent coverage.json
- [ ] #547: docs: configuration guide references non-existent --validate option
- [ ] #546: docs: incorrect CLI option names in usage-guide.md
- [ ] #541: docs: getting-started guide function mismatch with default fpm template
- [ ] #537: docs: incorrect fpm command in getting-started guide
- [ ] #526: bug: getting started guide creates broken workflow due to function name mismatch
- [ ] #527: bug: README example output does not match actual CLI behavior
- [ ] #528: bug: example scripts assume fortcov is in PATH without verification
- [ ] #551: bug: --exclude option does not filter files from coverage report

### EPIC: Code Quality & Technical Debt (MEDIUM PRIORITY)
- [ ] #520: defect: consolidate duplicate MAX_PATH constants across modules
- [ ] #521: defect: consolidate duplicate MAX_FILES constants
- [ ] #522: defect: consolidate duplicate MAX_FILENAME_LENGTH constants
- [ ] #523: defect: consolidate duplicate MAX_COMMAND_LENGTH constants
- [ ] #524: defect: missing error handling for memory allocation in main.f90
- [ ] #525: defect: missing error handling for memory allocations in gcov_line_parser.f90
- [ ] #542: defect: commented-out code in coverage_analysis.f90
- [ ] #544: defect: duplicate assert functions across test files
- [ ] #549: defect: excessive return statements indicating complex control flow
- [ ] #552: defect: facade modules that only re-export functionality
- [ ] #553: defect: unsafe execute_command_line usage in tests

### EPIC: Dead Code & Cleanup (LOW PRIORITY)
- [ ] #545: defect: placeholder test file check.f90 with no implementation
- [ ] #554: defect: PLAY workflow dead code audit summary

### EPIC: Sprint Completion & Documentation (FINAL)
- [ ] #555: docs: Sprint 5 final documentation consolidation and validation

## DOING (Current Work)
- [ ] #518: defect: file exceeds QADS size limit - test_auto_discovery_end_to_end_validation.f90 (601 lines) (branch: arch-518) [EPIC: Architecture Compliance Recovery]

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