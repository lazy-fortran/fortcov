module error_handling_core
    !! Error Handling - Backward Compatibility Layer
    !!
    !! Decomposed for SRP compliance (Issue #718 proactive size management).
    !! Re-exports all functionality from specialized modules to maintain
    !! backward compatibility with existing code.
    !!
    !! Original size: 422 lines -> Now: ~50 lines
    !! Implementation moved to specialized modules:
    !! - error_types.f90
    !! - error_handlers.f90
    !! - error_utils.f90
    use error_types
    use error_handlers
    use error_utils
    implicit none
    
    ! Re-export all types for backward compatibility
    public :: error_context_t
    
    ! Re-export error constants
    public :: ERROR_SUCCESS, ERROR_MISSING_SOURCE_FILE, ERROR_PERMISSION_DENIED
    public :: ERROR_OUT_OF_MEMORY, ERROR_INVALID_CONFIG, ERROR_INCOMPLETE_COVERAGE
    public :: ERROR_THRESHOLD_NOT_MET, ERROR_FILE_ACCESS, ERROR_MISSING_FILE
    public :: ERROR_FILE_TOO_LARGE, ERROR_EMPTY_FILE, ERROR_INVALID_DATA
    public :: ERROR_INVALID_PATH, ERROR_MEMORY_EXHAUSTION, ERROR_FILE_OPERATION_FAILED
    public :: ERROR_FATAL
    
    ! Re-export string length constants
    public :: MAX_MESSAGE_LEN, MAX_SUGGESTION_LEN, MAX_CONTEXT_LEN, MAX_STACK_TRACE_LEN
    
    ! Re-export all public procedures for backward compatibility
    public :: handle_missing_source
    public :: handle_permission_denied
    public :: handle_out_of_memory
    public :: handle_invalid_config
    public :: handle_incomplete_coverage
    public :: handle_fatal_error_with_trace
    public :: handle_no_coverage_files
    public :: handle_gcov_not_found
    public :: handle_invalid_arguments
    public :: handle_threshold_not_met
    public :: log_error
    public :: format_error_message
    public :: is_recoverable_error
    public :: clear_error_context
    public :: safe_write_message
    public :: safe_write_suggestion
    public :: safe_write_context

end module error_handling_core