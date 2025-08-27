# Development Backlog

## SPRINT_BACKLOG (Architecture Compliance & User Experience - Sprint 4)

### EPIC: Architecture Compliance (CRITICAL PRIORITY)
- [ ] #529: architecture: zero_configuration_manager.f90 exceeds 500-line QADS limit (556 lines)
- [ ] #530: architecture: test file exceeds 500-line QADS limit - requires decomposition
- [ ] #531: architecture: 6 modules approaching 500-line limit risk future violations

### EPIC: User Experience & Documentation (HIGH PRIORITY)
- [ ] #532: docs: consolidate and fix documentation defects identified in Sprint 3 review
- [ ] #526: bug: getting started guide creates broken workflow due to function name mismatch
- [ ] #527: bug: README example output does not match actual CLI behavior
- [ ] #528: bug: example scripts assume fortcov is in PATH without verification

### EPIC: Technical Debt Reduction (MEDIUM PRIORITY)
- [ ] #520: defect: consolidate duplicate MAX_PATH constants across modules
- [ ] #521: defect: consolidate duplicate MAX_FILES constants
- [ ] #522: defect: consolidate duplicate MAX_FILENAME_LENGTH constants
- [ ] #523: defect: consolidate duplicate MAX_COMMAND_LENGTH constants
- [ ] #524: defect: missing error handling for memory allocation in main.f90
- [ ] #525: defect: missing error handling for memory allocations in gcov_line_parser.f90
- [ ] #519: defect: remove commented-out code in coverage_analysis.f90

### EPIC: Code Style Compliance (LOW PRIORITY)
- [ ] #515: style: fix minor line length violations in validation test files

### EPIC: Sprint Completion & Documentation (FINAL)
- [ ] #533: docs: Sprint 4 final documentation consolidation and validation

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