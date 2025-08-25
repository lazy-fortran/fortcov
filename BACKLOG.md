# Development Backlog

## SPRINT_BACKLOG (Critical Defect Resolution & Architecture Alignment)

### EPIC: Critical Infrastructure Defects

### EPIC: Core Functionality Restoration
- [ ] #394: Diff option documented in help but not implemented in parser
- [ ] #390: Output format mapping bug: format parameter values incorrectly converted
- [ ] #388: Invalid option produces confusing error message

### EPIC: Architecture Size Compliance
- [ ] #398: Architecture violation: 12 modules exceed 500-line target compromising maintainability
- [ ] #399: Architecture risk: 5 modules approaching 1000-line hard limit requiring immediate decomposition

### EPIC: Code Quality & Dead Code Elimination
- [ ] #386: cleanup: massive code duplication in int_to_string functions
- [ ] #387: cleanup: duplicate real_to_string functions
- [ ] #376: cleanup: remove unused config_file_handler module
- [ ] #385: cleanup: remove disabled fork bomb test files
- [ ] #377: cleanup: remove unused coverage_line_types module
- [ ] #378: cleanup: remove unused platform_detection module
- [ ] #379: cleanup: remove unused secure_temp_file_types module
- [ ] #380: cleanup: remove unused temp_file_c_interfaces module
- [ ] #381: cleanup: remove unused temp_file_error_handling module
- [ ] #382: cleanup: remove unused sanitize_filename function
- [ ] #383: cleanup: remove unused is_safe_path function
- [ ] #384: cleanup: remove unused validate_string_input function

## DOING (Current Work)
- [ ] #397: README contains multiple non-functional command examples [EPIC: Critical Infrastructure Defects]

## PRODUCT_BACKLOG (High-level Features)
- [ ] Advanced Coverage Analytics & Reporting
- [ ] Performance Optimization & Scalability  
- [ ] Enhanced Integration & Compatibility
- [ ] User Experience & Developer Productivity

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