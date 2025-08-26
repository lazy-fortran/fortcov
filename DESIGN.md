# fortcov Architecture Design

## Sprint 1: Auto-Discovery Feature Architecture

### Objective
Transform fortcov from a manual 3-step process to a single-command solution that automatically handles test execution and gcov processing.

### Current Manual Workflow (Problem Statement)
```bash
# Current: 3 manual steps
1. fpm test --flag "-fprofile-arcs -ftest-coverage"
2. find build -name "*.gcda" | while read gcda_file; do gcov -b "$gcda_file" 2>/dev/null || true; done  
3. fortcov

# Goal: Single command
fortcov  # Auto-handles everything above
```

### Architectural Approach

#### 1. Build System Detection Layer
**Module**: `build_system_detector.f90`
**Responsibility**: Detect project build system and provide appropriate commands

```fortran
type :: build_system_info_t
  character(len=20) :: system_type     ! "fpm", "cmake", "make", "meson"
  character(len=256) :: test_command   ! Build-specific coverage test command
  character(len=256) :: build_file     ! Marker file (fpm.toml, etc.)
  logical :: tool_available            ! Tool available in PATH
end type
```

**Detection Priority**: FPM → CMake → Make → Meson
**Integration Point**: Called by zero_configuration_manager

#### 2. Auto-Test Execution Layer  
**Module**: Enhanced `coverage_workflows.f90`
**Responsibility**: Execute tests with coverage flags when no .gcov files exist

**Workflow Integration**:
1. Check for existing .gcov files
2. If none found AND auto_discovery enabled:
   - Detect build system
   - Execute appropriate test command with coverage flags
   - Handle execution with timeout and error recovery
3. Proceed with existing gcov generation workflow

**Error Handling**: Graceful degradation to manual workflow guidance

#### 3. Configuration Layer
**Module**: Enhanced `fortcov_config.f90`
**Responsibility**: Control auto-discovery behavior

**New Configuration Options**:
```fortran
type :: config_t
  ! ... existing fields ...
  logical :: auto_discovery = .true.
  logical :: auto_test_execution = .true.
  integer :: test_timeout_seconds = 300
end type
```

**Command Line Interface**:
```bash
fortcov                     # Auto-discovery enabled (default)
fortcov --no-auto-discovery # Disable auto-discovery  
fortcov --no-auto-test     # Skip auto-test execution
fortcov --test-timeout=600  # Extend test timeout
```

#### 4. Orchestration Layer
**Module**: Enhanced `zero_configuration_manager.f90`
**Responsibility**: Coordinate auto-discovery workflow

**Enhanced Workflow**:
1. **Build System Detection**: Use build_system_detector
2. **Test Execution**: Use coverage_workflows auto-test execution  
3. **GCov Generation**: Use existing auto_generate_gcov_files
4. **Coverage Analysis**: Continue with standard workflow

### Key Design Decisions

#### Decision 1: Default Behavior
**Choice**: Auto-discovery enabled by default
**Rationale**: Provides immediate value to users, aligns with "zero-configuration" philosophy
**Risk Mitigation**: Provide clear disable options for users who need manual control

#### Decision 2: Build System Priority
**Choice**: FPM → CMake → Make → Meson detection order
**Rationale**: FPM is primary target, CMake most common, Make most generic
**Implementation**: Stop at first detected system, no multi-system support in MVP

#### Decision 3: Graceful Degradation
**Choice**: Fall back to manual workflow guidance on auto-discovery failure
**Rationale**: Maintain current functionality, provide helpful error messages
**Implementation**: Enhanced error messages with specific manual commands

#### Decision 4: Security-First Command Execution
**Choice**: Use existing secure_command_executor for all test commands
**Rationale**: Maintain security standards, prevent injection attacks
**Implementation**: Validate and sanitize all build system commands

### Integration Points

#### With Existing Architecture
- **Preserves**: Current public interface stability
- **Enhances**: Zero-configuration mode with build system awareness
- **Maintains**: All existing explicit workflow options
- **Extends**: Configuration system with auto-discovery controls

#### Module Dependencies
```
build_system_detector.f90
    ↓
zero_configuration_manager.f90 ← fortcov_config.f90 (enhanced)
    ↓
coverage_workflows.f90 (enhanced)
    ↓  
coverage_orchestrator.f90 (existing)
```

### Performance Considerations

#### Build System Detection
- **Cost**: File system checks (4 file existence tests)
- **Optimization**: Cache detection results per session
- **Impact**: Negligible overhead (~1-2ms)

#### Test Execution
- **Cost**: Full test suite execution with coverage instrumentation  
- **Timeout**: Configurable (default 300 seconds)
- **Optimization**: Only when no .gcov files exist

#### Memory Impact
- **Additional Config Fields**: ~20 bytes
- **Build System Info**: ~500 bytes  
- **Total Impact**: <1KB additional memory

### Risk Assessment

#### High Risk: Test Execution Failures
- **Mitigation**: Timeout handling, graceful error recovery
- **Fallback**: Clear manual workflow guidance
- **Monitoring**: Verbose logging of execution steps

#### Medium Risk: Build System Detection False Positives
- **Mitigation**: Tool availability validation, command verification
- **Fallback**: Allow manual override via configuration
- **Testing**: Comprehensive test cases for each build system

#### Low Risk: Performance Impact
- **Mitigation**: Efficient file existence checks, caching
- **Monitoring**: Measure auto-discovery overhead
- **Optimization**: Skip detection when explicit paths provided

### Success Metrics

#### User Experience
- **Primary**: Single `fortcov` command works out-of-the-box
- **Secondary**: Clear error messages guide manual workflow when needed
- **Tertiary**: Maintain <5 second startup time including auto-discovery

#### Technical Compliance
- **Architecture**: All modules <500 lines, functions <50 lines
- **Security**: All commands use secure_command_executor
- **Compatibility**: Zero breaking changes to existing API

## Sprint 1 Assessment (FAILED)

### Critical Failures Identified
**SPRINT GOAL NOT ACHIEVED**: Auto-discovery single-command workflow is broken

#### Core Functionality Failures (CRITICAL)
- **Issue #463**: Multiple test failures with EXECUTE_COMMAND_LINE runtime errors
- **Issue #469**: Auto-discovery fails to find gcov files in build/gcov/ directory (CORE FEATURE BROKEN)
- **Issue #470**: Coverage parsing shows 0.00% even with valid gcov files (BASIC FUNCTIONALITY BROKEN)
- **Issue #472**: CLI argument parsing bug breaks documented examples

#### Infrastructure & Security Issues (HIGH PRIORITY)
- **Issue #464**: Security validation test failing (HIGH SECURITY RISK)
- **Issue #465**: Permission denied errors blocking operation
- **Issue #467**: Fork bomb cleanup bug prevents normal operation
- **Issue #473**: Input validation accepts invalid values

#### Architectural Debt (DEFER UNTIL CRITICAL FIXED)
- **Issue #457**: zero_configuration_manager.f90 exceeds 500-line target (646 lines)
- **Issue #461**: Function size violations in zero_configuration_manager.f90
- **Issue #458**: ~2000+ lines dead C interface code

### Root Cause Analysis
1. **Auto-Discovery Implementation Incomplete**: Core feature fails to locate gcov files
2. **Test Infrastructure Unstable**: Runtime errors prevent validation of fixes
3. **CLI Interface Inconsistent**: Argument parsing breaks documented workflows
4. **Coverage Engine Defective**: 0.00% parsing indicates fundamental parsing failures

### Recovery Strategy for Sprint 2
**PRIORITY**: Critical functionality restoration before any architecture work

1. **Phase 1 - Critical Fixes (MUST COMPLETE FIRST)**:
   - Fix test runtime errors to enable validation workflow
   - Restore auto-discovery gcov file detection
   - Fix coverage parsing to show actual percentages
   - Repair CLI argument parsing for documented examples

2. **Phase 2 - Infrastructure Stabilization**: 
   - Fix security test failures
   - Resolve permission errors
   - Clean up fork bomb prevention issues

3. **Phase 3 - Architecture Compliance (ONLY AFTER CRITICAL FIXED)**:
   - Decompose zero_configuration_manager.f90 
   - Remove dead C interface code
   - Function size compliance

### Lessons Learned
- **Architecture work was premature**: Should have verified core functionality first
- **Test infrastructure critical**: Cannot validate fixes without working tests  
- **Auto-discovery requires end-to-end testing**: Unit tests insufficient for integration feature
- **Documentation consistency essential**: CLI mismatches block user adoption

### Implementation Sequence

1. **#278**: Build system detection (foundational)
2. **#280**: Configuration options (enablement)  
3. **#279**: Auto-test execution (core functionality)
4. **#281**: Zero-config integration (orchestration)
5. **#277**: Final integration and testing (completion)

This architecture transforms fortcov into a true single-command solution while maintaining security, performance, and backward compatibility standards.

## Sprint 2: Critical Functionality Recovery

### Sprint Goal
**PRIMARY OBJECTIVE**: Restore core fortcov functionality to working state and achieve sprint 1 auto-discovery goals

**CRITICAL SUCCESS CRITERIA**:
1. **Auto-discovery workflow functional**: `fortcov` command works end-to-end without manual intervention
2. **Coverage parsing accuracy**: Shows actual coverage percentages from valid gcov files
3. **CLI consistency**: All documented examples work as specified
4. **Test infrastructure stable**: All tests pass reliably enabling validation workflow

### Definition of Done (Sprint 2)

#### PHASE 1: Core Functionality Recovery (CRITICAL - MUST COMPLETE)
- [ ] **Test Infrastructure Stable**: All `fpm test --flag "-fprofile-arcs -ftest-coverage"` tests pass without runtime errors
- [ ] **Auto-discovery Functional**: Zero-config mode finds gcov files in `build/gcov/` directory successfully
- [ ] **Coverage Parsing Accurate**: Shows actual coverage percentages instead of 0.00%
- [ ] **CLI Arguments Working**: `--source` flag and all documented examples work correctly
- [ ] **Fork Bomb Prevention**: Marker file cleanup works properly, doesn't block normal operation

#### PHASE 2: Infrastructure Stabilization (HIGH - COMPLETE AFTER PHASE 1)
- [ ] **Security Tests Pass**: URL-encoded attack validation and all security tests pass
- [ ] **Permission Handling**: Test directory creation uses proper temporary directories
- [ ] **Input Validation**: CLI accepts only valid threshold values with proper error messages

#### PHASE 3: User Experience & Documentation (MEDIUM - AFTER CRITICAL FIXES)
- [ ] **Documentation Consistency**: All CLI help matches implementation, all examples work
- [ ] **Configuration Valid**: Default fortcov.nml references existing directories only
- [ ] **Error Messages Clear**: Users get actionable feedback for common errors

#### PHASE 4: Architecture Compliance (LOW - ONLY AFTER CORE RECOVERY)
- [ ] **File Size Compliance**: zero_configuration_manager.f90 under 500 lines
- [ ] **Function Size Compliance**: All functions under 50 lines
- [ ] **Module Structure**: Clear separation of responsibilities

#### PHASE 5: Code Cleanup (LOWEST - FINAL PHASE)
- [ ] **Dead Code Removal**: C interfaces and obsolete files eliminated
- [ ] **Stub Implementation**: test_build_auto_discovery.f90 properly implemented or removed
- [ ] **Documentation Cleanup**: Investigation files and old documentation removed

### Success Metrics

#### Functional Requirements
- **Primary Workflow**: `fortcov` command completes successfully showing actual coverage
- **Zero Configuration**: Auto-discovery finds files without manual specification
- **Documentation Accuracy**: All README examples work copy-paste
- **Test Reliability**: CI passes consistently without manual intervention

#### Quality Requirements
- **Performance**: Single command completes within 5 seconds for small projects
- **Security**: All security validation tests pass
- **Usability**: Error messages provide clear resolution guidance
- **Architecture**: Code follows size and structure standards

### Risk Mitigation

#### Critical Path Dependencies
1. **Test Infrastructure FIRST**: Must fix test failures before validating other fixes
2. **Auto-discovery Core**: Fix file discovery before coverage parsing
3. **CLI Consistency**: Fix argument parsing before documentation updates

#### Rollback Strategy
If Phase 1 cannot be completed:
- Document minimum working manual workflow
- Provide clear troubleshooting guide
- Plan incremental recovery in subsequent sprints

### Technical Approach

#### Phase 1 Implementation Strategy
1. **Fix Test Runtime Errors**: Address EXECUTE_COMMAND_LINE failures and permission issues
2. **Debug Auto-discovery**: Add logging to file discovery process, verify path resolution
3. **Fix Coverage Parsing**: Debug gcov file parsing and statistics calculation
4. **Repair CLI Parsing**: Fix argument parsing to match documented interface

#### Integration Testing Strategy
- **End-to-end Validation**: Test complete workflow from clean state to coverage report
- **Multiple Build Systems**: Verify FPM integration works properly
- **Documentation Examples**: Test all README examples as integration tests

### Learning from Sprint 1 Failure

#### Root Cause Analysis
1. **Assumed Working Foundation**: Attempted architecture work before verifying core functionality
2. **Insufficient Integration Testing**: Unit tests passed but end-to-end workflow was broken
3. **Documentation Drift**: Examples didn't match implementation

#### Process Improvements
- **Test-First Recovery**: Fix all test failures before feature work
- **End-to-end Validation**: Require working examples before completion
- **Continuous Integration**: Ensure CI validates documented workflows