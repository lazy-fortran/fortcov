# Development Backlog

## SPRINT_BACKLOG (Critical Functionality Recovery - Sprint 2)

### EPIC: Infrastructure Stabilization (HIGH - Sprint 2 Phase 2)
- [x] #464: Security bug: URL-encoded attack validation test failing  
- [x] #465: Permission denied error when creating test directories
- [ ] #467: Fork bomb prevention marker file not cleaned up, blocking normal operation
- [ ] #473: Input validation bug: minimum threshold accepts invalid values (-1, 999999)

### EPIC: User Experience & Documentation (MEDIUM - Sprint 2 Phase 3)
- [ ] #474: Documentation consolidation: Fix all CLI help mismatches and example errors (consolidates #466, #468, #471)
- [ ] #475: Sprint 2 final documentation consolidation and validation

### EPIC: Architecture Compliance (LOW - Sprint 2 Phase 4 - Only after Critical fixes)
- [ ] #457: refactor: split zero_configuration_manager.f90 to reduce file size (646 lines)
- [ ] #461: refactor: reduce function size in zero_configuration_manager.f90
- [ ] #478: fix: QADS line length violations in zero_configuration_manager.f90

### EPIC: Code Cleanup (LOWEST - Sprint 2 Phase 5 - Only after Core Recovery)
- [ ] #458: cleanup: remove dead C interface files (~2000+ lines unused)
- [ ] #459: cleanup: remove obsolete documentation and investigation files
- [ ] #460: refactor: implement stub functionality in test_build_auto_discovery.f90
- [ ] #462: refactor: evaluate wrapper modules for dead code elimination
- [ ] #480: style: fix line length violations in coverage_file_processor.f90
- [ ] #482: style: fix line length violation in test_cli_flag_parsing_issue_472.f90

## DOING (Current Work)
- [ ] #467: Fork bomb prevention marker file not cleaned up, blocking normal operation [EPIC: Infrastructure Stabilization]

## PRODUCT_BACKLOG (High-level Features)
- [ ] Advanced Coverage Analytics & Reporting
- [ ] Performance Optimization & Scalability  
- [ ] Enhanced Integration & Compatibility
- [ ] User Experience & Developer Productivity

## DONE (Completed Product Features)

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