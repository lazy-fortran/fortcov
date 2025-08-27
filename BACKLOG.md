# Development Backlog

## SPRINT_BACKLOG (Sprint 9: CRITICAL DEFECT RESOLUTION)

### EPIC: CRITICAL ARCHITECTURAL VIOLATIONS (URGENT)
- [ ] #643: CRITICAL - json_core.f90 at 966 lines (approaching limit)
- [ ] #642: CRITICAL - coverage_types.f90 at 960 lines (approaching limit)

### EPIC: CORE FUNCTIONALITY DEFECTS (HIGH)
- [ ] #658: JSON output format completely broken - no file created
- [ ] #657: fortcov lies about output locations - creates in build directory
- [ ] #660: --output flag broken for JSON format
- [ ] #659: inconsistent file output locations across formats
- [ ] #663: documented coverage workflow fails - gcov produces no files
- [ ] #640: test_memory_allocation_bug_issue_243 build failure

### EPIC: SECURITY & QUALITY DEFECTS (HIGH)
- [ ] #644: 150+ execute_command_line calls - SECURITY RISK
- [ ] #653: hardcoded /tmp/ path violations
- [ ] #646: 20 raw deallocate calls without guards
- [ ] #649: sleep 0.1 in production code - performance killer
- [ ] #656: 42 bare STOP statements - poor error handling

## DOING (Current Work)

- [ ] #641: CRITICAL - config_parser.f90 size violation (duplicate of #665) - **EPIC: CRITICAL ARCHITECTURAL VIOLATIONS**

## PRODUCT_BACKLOG (Deferred Low Priority)
- [ ] Documentation & User Experience Excellence
- [ ] Infrastructure Stabilization 
- [ ] Code Quality & Maintenance
- [ ] Advanced Coverage Analytics & Reporting
- [ ] Performance Optimization & Scalability  
- [ ] Enhanced Integration & Compatibility

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
- [x] Architectural Recovery & Core Functionality Restoration (Sprint 8 - SUCCESS - Output generation working, 83% modularization reduction achieved: 43→12 coverage modules, 42→0 _impl modules, 9→2 JSON modules)