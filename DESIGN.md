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

### Implementation Sequence

1. **#278**: Build system detection (foundational)
2. **#280**: Configuration options (enablement)  
3. **#279**: Auto-test execution (core functionality)
4. **#281**: Zero-config integration (orchestration)
5. **#277**: Final integration and testing (completion)

This architecture transforms fortcov into a true single-command solution while maintaining security, performance, and backward compatibility standards.