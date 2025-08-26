# Development Backlog

## SPRINT_BACKLOG (Critical Defect Resolution - Sprint 1 Failed)

### EPIC: Core Functionality Restoration (MUST HAVE - Sprint 1 Goals)
- [ ] #463: Critical: Multiple test failures with EXECUTE_COMMAND_LINE runtime errors
- [ ] #469: Auto-discovery fails to find gcov files in build/gcov/ directory (zero-configuration broken)
- [ ] #470: Coverage parsing shows 0.00% even with valid gcov files containing coverage data
- [ ] #472: CLI argument parsing bug: --source flag not recognized, breaking documented examples

### EPIC: Critical Infrastructure & Security Defects
- [ ] #464: Security bug: URL-encoded attack validation test failing  
- [ ] #465: Permission denied error when creating test directories
- [ ] #467: Fork bomb prevention marker file not cleaned up, blocking normal operation
- [ ] #473: Input validation bug: minimum threshold accepts invalid values (-1, 999999)

### EPIC: Documentation & CLI Consistency
- [ ] #474: Documentation consolidation: Fix all CLI help mismatches and example errors (consolidates #466, #468, #471)

### EPIC: Architecture Size Compliance (Post-Critical Fixes)
- [ ] #457: refactor: split zero_configuration_manager.f90 to reduce file size (646 lines)
- [ ] #461: refactor: reduce function size in zero_configuration_manager.f90

### EPIC: Dead Code Elimination (Low Priority)
- [ ] #458: cleanup: remove dead C interface files (~2000+ lines unused)
- [ ] #459: cleanup: remove obsolete documentation and investigation files
- [ ] #460: refactor: implement stub functionality in test_build_auto_discovery.f90
- [ ] #462: refactor: evaluate wrapper modules for dead code elimination

## DOING (Current Work)

## PRODUCT_BACKLOG (High-level Features)
- [ ] Advanced Coverage Analytics & Reporting
- [ ] Performance Optimization & Scalability  
- [ ] Enhanced Integration & Compatibility
- [ ] User Experience & Developer Productivity

## DONE (Completed Product Features)

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