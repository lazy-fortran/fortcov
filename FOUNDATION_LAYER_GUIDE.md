# Foundation Layer Development Guide

## Overview

The FortCov foundation layer provides essential infrastructure for clean modular architecture. This guide demonstrates how to use foundation patterns, utilities, and constants when developing new features or maintaining existing code.

## Foundation Components

### 1. Foundation Constants (`foundation_constants.f90`)

Centralized constants for consistency across all modules.

#### Usage Examples

**Exit Codes**:
```fortran
use foundation_constants

! Use standardized exit codes instead of magic numbers
if (validation_failed) then
    stop EXIT_VALIDATION_ERROR  ! Instead of: stop 7
end if

! Handle threshold failures
if (coverage < threshold) then
    stop EXIT_THRESHOLD_NOT_MET  ! Instead of: stop 2
end if
```

**String Length Constants**:
```fortran
use foundation_constants

! Use consistent string lengths
character(len=PATH_STRING_LEN) :: file_path     ! Instead of: character(len=2048)
character(len=MEDIUM_STRING_LEN) :: error_msg  ! Instead of: character(len=256)
```

**Configuration Defaults**:
```fortran
use foundation_constants

! Use centralized defaults
logical :: verbose = DEFAULT_VERBOSE          ! Instead of: logical :: verbose = .false.
real :: threshold = DEFAULT_THRESHOLD         ! Instead of: real :: threshold = 80.0
```

### 2. Foundation Layer Utils (`foundation_layer_utils.f90`)

Core utilities for safe module operations and decomposition support.

#### Standard Result Type

**Basic Usage**:
```fortran
use foundation_layer_utils

function parse_config_file(filename) result(parse_result)
    character(len=*), intent(in) :: filename
    type(status_result_t) :: parse_result
    
    parse_result%success = .false.
    parse_result%error_code = FOUNDATION_SUCCESS
    
    ! Attempt parsing
    if (file_exists(filename)) then
        ! Parse logic here
        parse_result%success = .true.
        parse_result%error_message = "Configuration parsed successfully"
    else
        parse_result%error_code = FOUNDATION_ERROR
        parse_result%error_message = "Configuration file not found: " // trim(filename)
        parse_result%context = "parse_config_file"
    end if
end function parse_config_file
```

**Error Handling Pattern**:
```fortran
! Calling code
type(status_result_t) :: result
result = parse_config_file("fortcov.nml")

if (.not. result%success) then
    write(*, '(A)') "ERROR: " // result%error_message
    if (allocated(result%context)) then
        write(*, '(A)') "Context: " // result%context
    end if
    stop result%error_code
end if
```

#### Interface Compatibility Validation

**Module Decomposition Safety**:
```fortran
use foundation_layer_utils

! Validate that decomposed modules preserve original interface
type(module_interface_t) :: original, decomposed(3)
logical :: is_compatible

! Define original interface
original%module_name = "coverage_engine"
allocate(original%public_procedures(2))
original%public_procedures(1) = "process_coverage"
original%public_procedures(2) = "validate_coverage"

! Define decomposed interfaces
decomposed(1)%module_name = "coverage_orchestrator"
decomposed(2)%module_name = "coverage_workflows"
decomposed(3)%module_name = "coverage_operations"

! Validate compatibility
is_compatible = validate_interface_compatibility(original, decomposed)
if (.not. is_compatible) then
    call error_handler("Interface compatibility validation failed")
end if
```

#### Memory Safety Validation

**Safe Operation Patterns**:
```fortran
use foundation_layer_utils

! Validate memory safety before critical operations
logical :: is_safe
is_safe = ensure_memory_safety("large_file_processing")

if (.not. is_safe) then
    call handle_memory_error("Insufficient memory for operation")
    return
end if

! Proceed with safe operation
allocate(large_array(size_needed))
```

### 3. Architectural Patterns (`architectural_patterns.f90`)

Abstract interfaces and design patterns for consistent module development.

#### Processor Pattern

**Implementing a New Processor**:
```fortran
use architectural_patterns

! Define concrete processor
type, extends(processor_interface_t) :: json_processor_t
    character(len=:), allocatable :: input_data
    character(len=:), allocatable :: output_data
contains
    procedure :: process => process_json
    procedure :: validate_input => validate_json_input
    procedure :: cleanup => cleanup_json_processor
end type json_processor_t

contains

function process_json(this, input_data) result(output_data)
    class(json_processor_t), intent(inout) :: this
    class(*), intent(in) :: input_data
    class(*), allocatable :: output_data
    
    ! Type-safe input handling
    select type(input_data)
    type is (character(*))
        this%input_data = input_data
        ! Process JSON
        this%output_data = process_json_data(this%input_data)
        allocate(output_data, source=this%output_data)
    end select
end function process_json

function validate_json_input(this, input_data) result(is_valid)
    class(json_processor_t), intent(in) :: this
    class(*), intent(in) :: input_data
    logical :: is_valid
    
    select type(input_data)
    type is (character(*))
        is_valid = is_valid_json(input_data)
    class default
        is_valid = .false.
    end select
end function validate_json_input

subroutine cleanup_json_processor(this)
    class(json_processor_t), intent(inout) :: this
    if (allocated(this%input_data)) deallocate(this%input_data)
    if (allocated(this%output_data)) deallocate(this%output_data)
end subroutine cleanup_json_processor
```

#### Validator Pattern

**Implementing Input Validation**:
```fortran
use architectural_patterns

type, extends(validator_interface_t) :: config_validator_t
    character(len=:), allocatable :: last_error
contains
    procedure :: validate => validate_config
    procedure :: get_error_message => get_config_error_message
end type config_validator_t

contains

function validate_config(this, data) result(is_valid)
    class(config_validator_t), intent(inout) :: this
    class(*), intent(in) :: data
    logical :: is_valid
    
    select type(data)
    type is (character(*))
        if (len_trim(data) == 0) then
            is_valid = .false.
            this%last_error = "Configuration data is empty"
        else
            is_valid = .true.
            this%last_error = ""
        end if
    class default
        is_valid = .false.
        this%last_error = "Invalid data type for configuration"
    end select
end function validate_config

function get_config_error_message(this) result(message)
    class(config_validator_t), intent(in) :: this
    character(len=:), allocatable :: message
    message = this%last_error
end function get_config_error_message
```

#### Command Pattern

**Implementing Undoable Operations**:
```fortran
use architectural_patterns

! Create and execute commands
type(command_pattern_t) :: file_operation
character(len=32) :: params(2)

! Setup command
file_operation%command_name = "copy_file"
params(1) = "source.txt"
params(2) = "dest.txt"
allocate(file_operation%parameters(2))
file_operation%parameters = params

! Execute command
call file_operation%execute()
if (file_operation%result%success) then
    write(*, '(A)') "File operation successful"
else
    write(*, '(A)') "File operation failed"
end if

! Undo if needed
if (file_operation%can_undo()) then
    call file_operation%undo()
end if
```

#### Strategy Pattern

**Algorithm Selection**:
```fortran
use architectural_patterns

type(strategy_pattern_t) :: diff_strategy

! Select algorithm based on data size
if (file_count < 100) then
    call diff_strategy%set_algorithm("simple_diff")
else
    call diff_strategy%set_algorithm("optimized_diff")
end if

call diff_strategy%execute_strategy()
```

## Foundation Layer Development Patterns

### 1. Module Decomposition Pattern

**When decomposing large modules**, follow this pattern:

```fortran
! Step 1: Create interface module (preserve compatibility)
module original_module
    use foundation_constants
    use foundation_layer_utils
    use decomposed_core_module
    use decomposed_validator_module
    implicit none
    
    ! Preserve original public interface
    public :: original_public_function
    
contains
    
    function original_public_function(input) result(output)
        ! Delegate to decomposed modules
        output = core_processing_function(input)
    end function original_public_function
    
end module original_module

! Step 2: Create focused decomposed modules
module decomposed_core_module
    use foundation_constants
    use architectural_patterns
    implicit none
    private
    
    public :: core_processing_function
    
contains
    
    function core_processing_function(input) result(output)
        ! Focused implementation
    end function core_processing_function
    
end module decomposed_core_module
```

### 2. Error Handling Pattern

**Consistent error handling** across modules:

```fortran
use foundation_constants
use foundation_layer_utils

function process_data(input_data) result(result)
    character(len=*), intent(in) :: input_data
    type(status_result_t) :: result
    
    ! Initialize result
    result%success = .false.
    result%error_code = FOUNDATION_SUCCESS
    
    ! Validate input
    if (len_trim(input_data) == 0) then
        result%error_code = FOUNDATION_INVALID_INPUT
        result%error_message = ERROR_INVALID_INPUT
        result%context = "process_data"
        return
    end if
    
    ! Process data
    if (process_successful) then
        result%success = .true.
        result%error_message = STATUS_SUCCESS
    else
        result%error_code = FOUNDATION_ERROR
        result%error_message = "Data processing failed"
        result%context = "process_data"
    end if
end function process_data
```

### 3. Memory Safety Pattern

**Safe allocation and deallocation**:

```fortran
use foundation_constants
use foundation_layer_utils

subroutine safe_allocate_array(array, size_needed)
    real, allocatable, intent(out) :: array(:)
    integer, intent(in) :: size_needed
    integer :: alloc_stat
    
    ! Validate size limits
    if (size_needed > MAX_FILES) then
        call handle_error("Array size exceeds safety limits")
        return
    end if
    
    ! Log memory operation
    call log_decomposition_event("MEMORY_ALLOC", "safe_allocate_array", &
                                "Allocating array")
    
    ! Safe allocation
    allocate(array(size_needed), stat=alloc_stat)
    if (alloc_stat /= 0) then
        call handle_error("Memory allocation failed")
        return
    end if
    
    ! Validate memory safety
    if (.not. ensure_memory_safety("safe_allocate_array")) then
        deallocate(array)
        call handle_error("Memory safety validation failed")
    end if
end subroutine safe_allocate_array
```

## Best Practices

### 1. Foundation Layer Integration

**DO**:
```fortran
use foundation_constants      ! Use centralized constants
use foundation_layer_utils   ! Use standard result types
use architectural_patterns   ! Implement standard interfaces

! Use foundation patterns
type(status_result_t) :: result
character(len=PATH_STRING_LEN) :: filename
```

**DON'T**:
```fortran
! Avoid magic numbers
stop 7                      ! Use: stop EXIT_VALIDATION_ERROR

! Avoid custom result types  
type :: my_custom_result_t  ! Use: status_result_t from foundation

! Avoid hardcoded strings
character(len=2048) :: path ! Use: character(len=PATH_STRING_LEN)
```

### 2. Interface Design

**Standard Interface Implementation**:
```fortran
! Implement foundation interfaces
type, extends(processor_interface_t) :: my_processor_t
contains
    procedure :: process => my_process
    procedure :: validate_input => my_validate_input
    procedure :: cleanup => my_cleanup
end type my_processor_t

! Use foundation result types
function my_process(this, input_data) result(output_data)
    class(my_processor_t), intent(inout) :: this
    class(*), intent(in) :: input_data
    class(*), allocatable :: output_data
    
    ! Implementation follows foundation patterns
end function my_process
```

### 3. Error Handling

**Consistent Error Reporting**:
```fortran
use foundation_constants

! Use standard error codes
if (file_not_found) then
    result%error_code = EXIT_FILE_ACCESS_ERROR
    result%error_message = ERROR_FILE_NOT_FOUND
end if

! Use standard status messages
if (operation_successful) then
    result%error_message = STATUS_SUCCESS
else
    result%error_message = STATUS_ERROR
end if
```

### 4. Memory Management

**Foundation Memory Patterns**:
```fortran
! Always validate before allocation
if (.not. ensure_memory_safety("large_operation")) then
    return
end if

! Use foundation size limits
if (array_size > MAX_FILES) then
    call handle_error("Array size exceeds limits")
    return
end if

! Log memory operations for debugging
call log_decomposition_event("MEMORY", "operation_name", "Allocating arrays")
```

## Testing Foundation Components

### Unit Testing Foundation Patterns

**Test Foundation Utilities**:
```fortran
! test_foundation_layer_utils.f90
subroutine test_status_result_initialization()
    type(status_result_t) :: result
    
    ! Test default initialization
    assert(.not. result%success)
    assert(result%error_code == FOUNDATION_SUCCESS)
    assert(.not. allocated(result%error_message))
end subroutine test_status_result_initialization

subroutine test_interface_compatibility_validation()
    type(module_interface_t) :: original, decomposed(2)
    logical :: is_compatible
    
    ! Setup test interfaces
    original%module_name = "test_module"
    allocate(original%public_procedures(1))
    original%public_procedures(1) = "test_function"
    
    decomposed(1)%module_name = "test_core"
    allocate(decomposed(1)%public_procedures(1))
    decomposed(1)%public_procedures(1) = "test_function"
    
    ! Test validation
    is_compatible = validate_interface_compatibility(original, decomposed)
    assert(is_compatible)
end subroutine test_interface_compatibility_validation
```

**Test Architectural Patterns**:
```fortran
! test_architectural_patterns.f90
subroutine test_command_pattern()
    type(command_pattern_t) :: command
    
    command%command_name = "test_command"
    
    ! Test execution
    call command%execute()
    assert(command%executed)
    assert(command%result%success)
    
    ! Test undo
    assert(command%can_undo())
    call command%undo()
    assert(.not. command%executed)
end subroutine test_command_pattern
```

### Integration Testing

**Test Foundation Integration**:
```fortran
! test_foundation_integration.f90
subroutine test_end_to_end_foundation_usage()
    type(status_result_t) :: result
    character(len=PATH_STRING_LEN) :: test_file
    logical :: safety_valid
    
    ! Test complete foundation workflow
    test_file = "test_data.json"
    
    ! Memory safety validation
    safety_valid = ensure_memory_safety("integration_test")
    assert(safety_valid)
    
    ! Standard result handling
    result = process_test_data(test_file)
    assert(result%success)
    assert(result%error_code == FOUNDATION_SUCCESS)
end subroutine test_end_to_end_foundation_usage
```

## Migration Guide

### Migrating Existing Code to Foundation Patterns

**Step 1: Replace Magic Numbers**:
```fortran
! OLD
stop 1                    ! Magic number
character(len=256) :: msg ! Hardcoded length

! NEW  
use foundation_constants
stop EXIT_FAILURE         ! Named constant
character(len=MEDIUM_STRING_LEN) :: msg ! Foundation constant
```

**Step 2: Standardize Result Types**:
```fortran
! OLD
logical :: success
integer :: error_code
character(len=512) :: error_msg

! NEW
use foundation_layer_utils
type(status_result_t) :: result
```

**Step 3: Implement Foundation Interfaces**:
```fortran
! OLD
! Custom interface definitions

! NEW
use architectural_patterns
type, extends(processor_interface_t) :: my_processor_t
! Standard interface implementation
```

### Compatibility Preservation

**Maintaining Backward Compatibility**:
```fortran
! Original module interface (preserved)
module coverage_engine
    use coverage_orchestrator  ! New decomposed module
    implicit none
    
    ! Original public interface preserved
    public :: process_coverage
    
contains
    
    function process_coverage(config) result(success)
        ! Delegate to new decomposed implementation
        success = orchestrate_coverage_analysis(config)
    end function process_coverage
    
end module coverage_engine
```

## Summary

The foundation layer provides:

✅ **Consistent Patterns**: Standard interfaces across all modules  
✅ **Error Handling**: Unified result types and error codes  
✅ **Memory Safety**: Validation and logging utilities  
✅ **Maintainability**: Centralized constants and utilities  
✅ **Extensibility**: Abstract patterns for new features  

**Key Benefits**:
- **Reduced Duplication**: Common patterns centralized
- **Improved Safety**: Memory and interface validation  
- **Enhanced Maintainability**: Standard patterns across codebase
- **Future-Proof Architecture**: Extensible foundation for growth

Use foundation patterns consistently to maintain architectural quality and enable sustainable development.