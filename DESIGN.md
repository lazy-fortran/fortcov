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

### Sprint 2 Recovery Results (COMPLETED)

**STATUS**: Sprint 2 objectives successfully achieved with critical functionality restored.

#### Phase 1 - Critical Fixes ✅ COMPLETED
- ✅ **Test Infrastructure Stable**: All test runtime errors resolved, validation workflow functional
- ✅ **Auto-discovery Operational**: Zero-config mode successfully finds gcov files 
- ✅ **Coverage Parsing Accurate**: Shows actual coverage percentages from valid gcov files
- ✅ **CLI Arguments Fixed**: All documented examples work correctly with proper argument parsing

#### Phase 2 - Infrastructure Stabilization ✅ COMPLETED  
- ✅ **Security Tests Pass**: URL-encoded attack validation and all security tests passing
- ✅ **Permission Handling Fixed**: Test directory creation uses proper temporary directories
- ✅ **Fork Bomb Prevention Fixed**: Marker file cleanup works properly without blocking operation

#### Phase 3 - User Experience & Documentation ⏳ IN PROGRESS
- ✅ **CLI Help Consistency**: All CLI help matches implementation after issue #474 fixes
- ✅ **Configuration Validation**: Default fortcov.nml references valid directories only  
- ✅ **Error Messages Clear**: Users get actionable feedback for common error conditions
- 🔄 **Final Documentation Consolidation**: Issue #475 - Sprint 2 final documentation review

### Sprint 2 Success Metrics Assessment

#### Functional Requirements ✅ ACHIEVED
- **Primary Workflow**: `fortcov` command completes successfully showing actual coverage percentages
- **Zero Configuration**: Auto-discovery finds gcov files without manual specification
- **Documentation Accuracy**: All README examples work copy-paste as documented
- **Test Reliability**: CI passes consistently without manual intervention

#### Quality Requirements ✅ ACHIEVED  
- **Performance**: Single command completes within 5 seconds for small projects
- **Security**: All security validation tests pass with proper input sanitization
- **Usability**: Error messages provide clear resolution guidance to users
- **Architecture**: Core functionality restored, architecture compliance planned for Sprint 3

### Architectural Decisions Made in Sprint 2

#### Decision 1: Prioritize Functionality Over Architecture
**Choice**: Complete functional recovery before architectural compliance
**Rationale**: Sprint 1 failure demonstrated that architecture work without working foundation fails
**Result**: Successful approach - core functionality fully restored

#### Decision 2: Maintain Manual Workflow Support
**Choice**: Keep explicit 3-step workflow alongside auto-discovery
**Rationale**: Users need reliability when auto-discovery fails, provides fallback option
**Implementation**: Both workflows documented and functional

#### Decision 3: Security-First Error Handling  
**Choice**: Comprehensive input validation with actionable error messages
**Rationale**: Security validation failures in Sprint 1 showed gaps in defensive programming
**Implementation**: All user inputs validated, injection attacks prevented

#### Decision 4: Documentation-Implementation Consistency
**Choice**: Align all documentation with actual implementation behavior
**Rationale**: CLI mismatches in Sprint 1 blocked user adoption
**Implementation**: Issue #474 systematically fixed all CLI/documentation inconsistencies

### Lessons Learned
- **Test-driven recovery approach**: Fix test infrastructure first enables validation of all other fixes
- **End-to-end validation essential**: Unit tests passed while integration workflows were broken
- **Documentation accuracy critical**: CLI mismatches completely block user adoption
- **Incremental recovery works**: Phase-based approach allowed systematic progress tracking

### Implementation Sequence

1. **#278**: Build system detection (foundational)
2. **#280**: Configuration options (enablement)  
3. **#279**: Auto-test execution (core functionality)
4. **#281**: Zero-config integration (orchestration)
5. **#277**: Final integration and testing (completion)

This architecture transforms fortcov into a true single-command solution while maintaining security, performance, and backward compatibility standards.

## Sprint 2: Critical Functionality Recovery ✅ COMPLETED

### Sprint Goal
**PRIMARY OBJECTIVE**: Restore core fortcov functionality to working state and achieve sprint 1 auto-discovery goals

**RESULT**: ✅ **SUCCESSFUL** - All critical functionality restored, auto-discovery working, Sprint 1 goals achieved

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

## Sprint 3 Readiness Assessment

### Core Functionality Status ✅ READY
- **Auto-discovery**: Fully functional single-command workflow  
- **Coverage analysis**: Accurate parsing and terminal display
- **Security**: All validation tests pass, injection prevention working
- **CLI interface**: All documented examples work correctly
- **Test infrastructure**: Stable and reliable validation workflow

### Architecture Compliance Targets (Sprint 3)
- **File size compliance**: zero_configuration_manager.f90 (646 lines → <500 lines target)
- **Function size compliance**: Large functions in zero_configuration_manager.f90 
- **Module decomposition**: Extract specialized functionality following SRP
- **Dead code removal**: ~2000+ lines unused C interface code
- **Line length compliance**: QADS violations in multiple modules

### Foundation for Future Development
Sprint 2 successfully established a solid foundation:
- **Working core functionality** enables safe refactoring in Sprint 3
- **Comprehensive test coverage** provides safety net for architectural changes  
- **Consistent documentation** ensures user experience remains stable
- **Security validation** framework protects against regression
- **Quality gates** prevent breaking changes from reaching users

**RECOMMENDATION**: Sprint 3 can safely proceed with architecture compliance work, as all critical functionality is verified working.

## Sprint 4: Architecture Compliance & User Experience (CURRENT)

### Sprint Goal
**PRIMARY OBJECTIVE**: Complete architectural compliance and eliminate user experience barriers

**SPRINT 4 DEFINITION OF DONE**:
1. **Architecture Compliance**: All modules comply with QADS 500-line limit with safety buffer (<400 lines target)
2. **User Experience Excellence**: All documentation workflows work copy-paste without errors
3. **Technical Debt Resolution**: Consolidated constants and comprehensive error handling
4. **Code Quality**: All style violations resolved and architectural patterns consistent

### Key Architectural Decisions (Sprint 4)

#### Decision 1: SRP-Driven Module Decomposition Strategy
**Choice**: Extract specialized modules from zero_configuration_manager.f90 following Single Responsibility Principle
**Rationale**: 556 lines indicates multiple concerns mixed - auto-discovery, command execution, file processing, error handling
**Implementation Approach**:
- Extract auto-discovery logic → dedicated discovery module (~150-200 lines)
- Extract command execution utilities → shared utility module (~100-150 lines)  
- Extract file processing logic → specialized processor module (~100-150 lines)
- Retain core orchestration in zero_configuration_manager.f90 (target <200 lines)

#### Decision 2: Proactive Architecture Maintenance
**Choice**: Refactor 6 modules approaching 500-line limit to 300-400 line target range
**Rationale**: Provides safety buffer for future development without reactive crisis refactoring
**Priority**: LOW - Complete after critical violations resolved

#### Decision 3: User Experience First Approach  
**Choice**: Prioritize documentation fixes as HIGH priority alongside architecture compliance
**Rationale**: Broken user onboarding blocks adoption regardless of technical excellence
**Focus Areas**:
- Getting started guide workflow repair
- README example accuracy with CLI behavior
- Example script PATH handling and error recovery

#### Decision 4: Technical Debt Systematic Consolidation
**Choice**: Consolidate all duplicate constants to foundation_constants.f90 with consistent naming
**Rationale**: DRY violation cleanup improves maintainability and prevents behavioral inconsistencies
**Standards**:
- MAX_PATH_LENGTH = 4096 (most conservative)
- MAX_FILES = 10000 (foundation_constants.f90 value)
- MAX_FILENAME_LENGTH = 512 (most conservative)
- MAX_COMMAND_LENGTH = 8192 (consistent security module value)

### Implementation Strategy (Sprint 4)

#### Phase 1: Critical Architecture Compliance (CRITICAL)
- **Issue #529**: zero_configuration_manager.f90 decomposition using SRP extraction
- **Issue #530**: test_auto_discovery_end_to_end_validation.f90 split by test phases

#### Phase 2: User Experience Excellence (HIGH PRIORITY)
- **Issue #532**: Comprehensive documentation defect consolidation  
- **Issue #526-528**: Getting started guide repair, README accuracy, PATH handling

#### Phase 3: Technical Debt Resolution (MEDIUM PRIORITY)
- **Issues #520-525**: Constant consolidation and error handling completion
- **Issue #519**: Remove commented-out code

#### Phase 4: Architecture Maintenance (LOW PRIORITY)
- **Issue #531**: Proactive refactoring of 6 modules approaching limits
- **Issue #515**: Minor style compliance fixes

### Success Metrics (Sprint 4)

#### Architecture Compliance
- **Primary**: zero_configuration_manager.f90 < 400 lines (556 → <400)
- **Secondary**: All test files < 500 lines with logical decomposition
- **Tertiary**: All modules have >100 line safety buffer from QADS limits

#### User Experience  
- **Primary**: All documentation examples work copy-paste from clean environment
- **Secondary**: Getting started guide completes without errors
- **Tertiary**: Example scripts handle missing fortcov gracefully

#### Quality Assurance
- **Primary**: All QADS architectural violations resolved
- **Secondary**: No duplicate constants or missing error handling
- **Tertiary**: Consistent code style and patterns across codebase

### Risk Assessment (Sprint 4)

#### High Risk: Module Decomposition Complexity
- **Mitigation**: Incremental refactoring with comprehensive test validation after each extraction
- **Monitoring**: Maintain API compatibility and ensure no functional regression
- **Fallback**: Revert to working state if decomposition introduces instability

#### Medium Risk: Documentation-Implementation Synchronization  
- **Mitigation**: Test all documentation examples as part of CI pipeline
- **Validation**: End-to-end workflow testing from clean environment
- **Quality Gate**: No PR merge until all examples work copy-paste

#### Low Risk: Technical Debt Consolidation
- **Mitigation**: Systematic approach with single constant source of truth
- **Testing**: Verify no behavioral changes during constant consolidation  
- **Monitoring**: Build system validation after each constant migration

### Integration with Future Sprints

Sprint 4 establishes architectural foundation for advanced features:
- **Clean module structure** enables safe feature additions
- **Comprehensive documentation** supports user adoption and contribution
- **Technical debt resolution** prevents accumulation during feature development
- **Quality standards** maintain codebase health as complexity grows