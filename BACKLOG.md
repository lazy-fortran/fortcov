# Development Backlog

## SPRINT_BACKLOG (Sprint 8: Architectural Recovery & Core Functionality Restoration)

### EPIC: Critical Functionality Recovery
- [ ] #622: Fix coverage workflow to generate actual output files (CRITICAL - core value proposition)
- [ ] #618: Fix format inconsistencies and missing output file handling 
- [ ] #614: Fix markdown report generation with empty output file path
- [ ] #613: Fix test suite failures (directory removal and missing executable errors)

### EPIC: Architectural Debt Resolution  
- [ ] #621: Address excessive modularization (43 coverage modules, 42 _impl modules)
- [ ] #607: Remove unnecessary interface/implementation separation
- [ ] #605: Consolidate excessive coverage module fragmentation
- [ ] #603: Merge 9 JSON modules into coherent functionality
- [ ] #604: Consolidate 7 config parser modules
- [ ] #600: Remove duplicate namelist parser modules
- [ ] #601: Remove unused modules (syntax_token_types, system_diff_impl)

### EPIC: Infrastructure Stabilization
- [ ] #617: Fix coverage workflow gcda/gcno file compatibility issues
- [ ] #616: Fix broken reference to non-existent test file
- [ ] #615: Move misplaced test file from src/utils to test directory
- [ ] #619: Fix help text argument order inconsistencies

### EPIC: Documentation & User Experience
- [ ] #593: Document complete coverage workflow for user success
- [ ] #606: Fix getting-started tutorial gcov generation commands  
- [ ] #602: Fix documentation error (fpm init → fpm new)
- [ ] #598: Improve error message for invalid namelist config format

### EPIC: Sprint 8 Consolidation
- [ ] #623: Final Sprint 8 findings integration and documentation consolidation

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
- [x] User Experience Excellence (Sprint 6 - PARTIALLY FAILED due to coverage infrastructure collapse)
- [x] Critical Infrastructure Recovery (Sprint 7 - OUTSTANDING SUCCESS - All 5 critical issues resolved: CI failures, compiler errors, build system configuration, auto-discovery functionality, and coverage workflow architecture)
- [x] File Organization Recovery (Sprint 7 - SUCCESS - 118 source files organized into 5 proper subdirectories meeting QADS <30 file limit)