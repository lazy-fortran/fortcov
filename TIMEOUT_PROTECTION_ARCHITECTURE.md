# Command Execution Timeout Protection Architecture
## Issue #140 - Complete Implementation Design

### Executive Summary

This document defines the complete architecture for implementing command execution timeout protection in fortcov. The solution prevents DoS attacks and resource exhaustion by implementing configurable timeouts with cross-platform process management abstractions.

### Core Architecture

#### 1. Module Structure

```fortran
! src/command_timeout_manager.f90
module command_timeout_manager
    use iso_c_binding
    use error_handling
    implicit none
    private
    
    ! Core timeout management type
    type, public :: timeout_command_executor_t
        character(len=:), allocatable :: command
        integer :: timeout_seconds = 30
        integer(c_int) :: process_id = -1
        real(8) :: start_time = 0.0_8
        integer :: status = STATUS_PENDING
        logical :: is_background = .false.
        character(len=:), allocatable :: working_directory
    end type timeout_command_executor_t
    
    ! Status enumeration
    integer, parameter, public :: STATUS_PENDING = 0
    integer, parameter, public :: STATUS_RUNNING = 1
    integer, parameter, public :: STATUS_COMPLETED = 2
    integer, parameter, public :: STATUS_TIMEOUT = 3
    integer, parameter, public :: STATUS_ERROR = 4
    integer, parameter, public :: STATUS_TERMINATED = 5
    
    ! Public interfaces
    public :: execute_with_timeout
    public :: monitor_timeout
    public :: terminate_process_tree
    public :: cleanup_process_resources
    public :: create_timeout_executor
    public :: destroy_timeout_executor
```

#### 2. C Interface Layer

```c
// src/c_interfaces/process_timeout_unix.c
#ifdef __unix__
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

typedef struct {
    pid_t pid;
    int status;
    time_t start_time;
    int timeout_seconds;
} process_handle_t;

int create_process_with_timeout(const char* command, 
                               const char* working_dir,
                               int timeout_seconds,
                               process_handle_t* handle);

int monitor_process_timeout(process_handle_t* handle, int* timed_out);
int terminate_process_tree(process_handle_t* handle, int graceful);
int cleanup_process_resources(process_handle_t* handle);
#endif

// src/c_interfaces/process_timeout_windows.c
#ifdef _WIN32
#include <windows.h>
#include <tlhelp32.h>

typedef struct {
    HANDLE process_handle;
    HANDLE job_handle;
    DWORD process_id;
    DWORD start_time;
    int timeout_seconds;
} process_handle_t;

int create_process_with_timeout(const char* command,
                               const char* working_dir, 
                               int timeout_seconds,
                               process_handle_t* handle);

int monitor_process_timeout(process_handle_t* handle, int* timed_out);
int terminate_process_tree(process_handle_t* handle, int graceful);
int cleanup_process_resources(process_handle_t* handle);
#endif
```

#### 3. Security Features

##### Process Tree Management
- **Unix**: Uses process groups to track child processes
- **Windows**: Uses job objects to contain process trees
- **Cleanup**: Ensures all child processes are terminated
- **Resource Protection**: Prevents zombie processes and handle leaks

##### Timeout Mechanisms
- **Precision**: Millisecond-level timeout checking
- **Non-blocking**: Async monitoring without blocking main thread
- **Graceful Termination**: SIGTERM → wait → SIGKILL sequence
- **Resource Cleanup**: Automatic cleanup on timeout/termination

#### 4. Integration Architecture

```fortran
! Updated secure_command_executor.f90 integration
module secure_command_executor
    use command_timeout_manager
    
    ! Enhanced safe execution with timeout protection
    subroutine safe_execute_gcov_with_timeout(gcov_executable, source_file, 
                                            working_dir, branch_coverage, 
                                            output_file, timeout_seconds, 
                                            error_ctx)
        character(len=*), intent(in) :: gcov_executable, source_file
        character(len=*), intent(in) :: working_dir, output_file
        logical, intent(in) :: branch_coverage
        integer, intent(in) :: timeout_seconds
        type(error_context_t), intent(out) :: error_ctx
        
        type(timeout_command_executor_t) :: executor
        character(len=:), allocatable :: safe_command
        
        ! Create timeout-protected executor
        call create_timeout_executor(executor, timeout_seconds, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Build and validate safe command
        call build_safe_timeout_command(gcov_executable, source_file,
                                       working_dir, branch_coverage,
                                       output_file, safe_command, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) return
        
        ! Execute with timeout protection
        call execute_with_timeout(executor, safe_command, error_ctx)
        
        ! Cleanup resources
        call destroy_timeout_executor(executor, error_ctx)
    end subroutine safe_execute_gcov_with_timeout
```

### Implementation Phases

#### Phase 1: Core Infrastructure (3 days)
1. **C Interface Implementation**
   - Unix process management (fork, exec, waitpid, kill)
   - Windows process management (CreateProcess, job objects)
   - Cross-platform abstraction layer
   
2. **Fortran Module Foundation**
   - `timeout_command_executor_t` type definition
   - Basic create/destroy procedures
   - Status enumeration and management

#### Phase 2: Timeout Monitoring (2 days)
1. **Timing Mechanisms**
   - Precise timeout calculation
   - Non-blocking status checking
   - Cross-platform time handling
   
2. **Process Monitoring**
   - Background timeout monitoring
   - Process status tracking
   - Resource usage monitoring

#### Phase 3: Termination Handling (2 days)
1. **Graceful Termination**
   - SIGTERM/TerminateProcess sequence
   - Grace period management
   - Forced termination fallback
   
2. **Process Tree Cleanup**
   - Child process enumeration
   - Recursive termination
   - Resource cleanup verification

#### Phase 4: Integration (2 days)
1. **Secure Command Executor Integration**
   - Update existing interfaces
   - Timeout parameter integration
   - Backward compatibility maintenance
   
2. **Configuration Support**
   - Timeout configuration in fortcov.nml
   - Per-command timeout overrides
   - Default timeout management

#### Phase 5: Testing & Validation (3 days)
1. **Unit Tests**
   - Timeout accuracy verification
   - Process termination testing
   - Resource cleanup validation
   
2. **Integration Tests**
   - Real command execution
   - DoS attack simulation
   - Cross-platform verification

### Test-Driven Design

#### Core Test Scenarios

```fortran
! test/test_command_timeout_manager.f90
module test_command_timeout_manager
    use command_timeout_manager
    use error_handling
    
    ! Test timeout accuracy within acceptable tolerance
    subroutine test_timeout_accuracy()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        real(8) :: start_time, end_time, elapsed
        
        call create_timeout_executor(executor, 2, error_ctx)
        assert(error_ctx%error_code == ERROR_SUCCESS)
        
        start_time = get_wall_time()
        call execute_with_timeout(executor, "sleep 5", error_ctx)
        end_time = get_wall_time()
        elapsed = end_time - start_time
        
        ! Timeout should occur within 10% tolerance
        assert(elapsed >= 1.8 .and. elapsed <= 2.2)
        assert(executor%status == STATUS_TIMEOUT)
    end subroutine test_timeout_accuracy
    
    ! Test process tree termination
    subroutine test_process_tree_termination()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        
        call create_timeout_executor(executor, 1, error_ctx)
        
        ! Command that spawns child processes
        call execute_with_timeout(executor, 
            "bash -c 'sleep 10 & sleep 10 & sleep 10 & wait'", error_ctx)
        
        ! Verify all processes are terminated
        assert(executor%status == STATUS_TIMEOUT)
        call verify_no_child_processes(executor%process_id)
    end subroutine test_process_tree_termination
    
    ! Test resource cleanup
    subroutine test_resource_cleanup()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        integer :: initial_handles, final_handles
        
        initial_handles = count_open_handles()
        
        call create_timeout_executor(executor, 1, error_ctx)
        call execute_with_timeout(executor, "sleep 5", error_ctx)
        call destroy_timeout_executor(executor, error_ctx)
        
        final_handles = count_open_handles()
        
        ! No handle/resource leaks
        assert(final_handles == initial_handles)
    end subroutine test_resource_cleanup
```

#### Security Test Scenarios

```fortran
    ! Test DoS protection
    subroutine test_dos_protection()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        real(8) :: start_time, end_time
        
        call create_timeout_executor(executor, 1, error_ctx)
        start_time = get_wall_time()
        
        ! CPU-intensive command that would run indefinitely
        call execute_with_timeout(executor, 
            "yes | head -c 1G > /dev/null", error_ctx)
        
        end_time = get_wall_time()
        
        ! Should terminate within timeout regardless of CPU load
        assert(end_time - start_time <= 1.5)
        assert(executor%status == STATUS_TIMEOUT)
    end subroutine test_dos_protection
    
    ! Test injection protection with timeout
    subroutine test_injection_with_timeout()
        type(timeout_command_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: malicious_command
        
        ! Command injection attempt
        malicious_command = "gcov test.f90; rm -rf / #"
        
        call create_timeout_executor(executor, 5, error_ctx)
        call execute_with_timeout(executor, malicious_command, error_ctx)
        
        ! Should fail safely without executing destructive command
        assert(error_ctx%error_code /= ERROR_SUCCESS)
        assert(executor%status == STATUS_ERROR)
    end subroutine test_injection_with_timeout
```

### Performance Considerations

#### Monitoring Overhead
- **Polling Frequency**: 100ms intervals for timeout checking
- **Memory Usage**: < 1KB per monitored process
- **CPU Impact**: < 0.1% overhead for monitoring loop

#### Scalability Limits
- **Concurrent Processes**: Up to 100 simultaneous commands
- **Timeout Range**: 1 second to 1 hour
- **Process Tree Depth**: Unlimited with recursive cleanup

### Configuration Integration

```fortran
! fortcov.nml example
&timeout_config
    default_command_timeout = 30
    gcov_timeout = 60
    find_timeout = 30
    mkdir_timeout = 10
    max_concurrent_commands = 50
    timeout_check_interval_ms = 100
/
```

### Error Handling Integration

```fortran
! Additional error codes for timeout protection
integer, parameter, public :: ERROR_COMMAND_TIMEOUT = 1012
integer, parameter, public :: ERROR_PROCESS_CREATION_FAILED = 1013
integer, parameter, public :: ERROR_PROCESS_MONITORING_FAILED = 1014
integer, parameter, public :: ERROR_TERMINATION_FAILED = 1015
integer, parameter, public :: ERROR_RESOURCE_CLEANUP_FAILED = 1016

! Enhanced error context for timeout scenarios
subroutine handle_command_timeout(command, timeout_seconds, error_ctx)
    character(len=*), intent(in) :: command
    integer, intent(in) :: timeout_seconds
    type(error_context_t), intent(out) :: error_ctx
    
    error_ctx%error_code = ERROR_COMMAND_TIMEOUT
    error_ctx%recoverable = .true.
    
    write(error_ctx%message, '(A,A,A,I0,A)') &
        "Command '", trim(command), "' timed out after ", &
        timeout_seconds, " seconds"
    
    write(error_ctx%suggestion, '(A)') &
        "Increase timeout value or optimize command execution"
    
    write(error_ctx%context, '(A)') "Command timeout protection"
end subroutine handle_command_timeout
```

### Implementation Quality Gates

#### Sergei's TDD Implementation Checklist
- [ ] All test interfaces defined before implementation
- [ ] RED/GREEN/REFACTOR cycle for each procedure
- [ ] Test coverage > 95% for timeout manager module
- [ ] Memory safety verification for all allocations
- [ ] Cross-platform compatibility testing

#### Patrick's Security Review Checklist
- [ ] No shell injection vulnerabilities in timeout commands
- [ ] Process tree termination prevents privilege escalation
- [ ] Resource cleanup prevents DoS through resource exhaustion
- [ ] Timeout bypass protection (no SIGALRM interference)
- [ ] Race condition analysis for concurrent process management

#### Vicky's Acceptance Testing Checklist
- [ ] Timeout accuracy within ±100ms on all supported platforms
- [ ] DoS attack protection verified with resource-intensive commands
- [ ] Integration with existing secure_command_executor seamless
- [ ] Configuration file integration working correctly
- [ ] Error messages provide actionable information for users

### Deployment Strategy

#### Backward Compatibility
- Existing `safe_execute_gcov` functions maintain current signatures
- New timeout variants added with `_with_timeout` suffix
- Default timeout values prevent breaking existing workflows

#### Migration Path
1. Deploy timeout manager as optional dependency
2. Update secure_command_executor with timeout variants
3. Integrate timeout configuration in fortcov.nml
4. Update documentation and examples
5. Enable timeout protection by default

### Success Criteria

1. **Security**: Zero successful DoS attacks through command execution
2. **Performance**: < 2% overhead for timeout monitoring
3. **Reliability**: 99.9% accurate timeout enforcement
4. **Compatibility**: Works on Linux, macOS, Windows
5. **Integration**: Seamless integration with existing secure executor

This architecture provides a complete, production-ready solution for Issue #140 that follows TDD principles, maintains SOLID design, and provides comprehensive security protection against DoS attacks through command execution timeouts.