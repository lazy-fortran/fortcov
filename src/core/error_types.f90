module error_types
    !! Error types and constants extracted from error_handling_core
    !! 
    !! Focused on error code definitions and the error context type.
    !! Provides foundation for error handling without implementation details.
    use iso_fortran_env, only: error_unit
    implicit none
    
    ! Error code enumeration
    integer, parameter, public :: ERROR_SUCCESS = 0
    integer, parameter, public :: ERROR_MISSING_SOURCE_FILE = 1004
    integer, parameter, public :: ERROR_PERMISSION_DENIED = 1005
    integer, parameter, public :: ERROR_OUT_OF_MEMORY = 1006
    integer, parameter, public :: ERROR_INVALID_CONFIG = 1007
    integer, parameter, public :: ERROR_INCOMPLETE_COVERAGE = 1010
    integer, parameter, public :: ERROR_THRESHOLD_NOT_MET = 1011
    
    ! Input validation error codes (Issue #122)
    integer, parameter, public :: ERROR_FILE_ACCESS = 1020
    integer, parameter, public :: ERROR_MISSING_FILE = 1021
    integer, parameter, public :: ERROR_FILE_TOO_LARGE = 1022
    integer, parameter, public :: ERROR_EMPTY_FILE = 1023
    integer, parameter, public :: ERROR_INVALID_DATA = 1024
    integer, parameter, public :: ERROR_INVALID_PATH = 1025
    integer, parameter, public :: ERROR_MEMORY_EXHAUSTION = 1026
    integer, parameter, public :: ERROR_FILE_OPERATION_FAILED = 1027
    
    integer, parameter, public :: ERROR_FATAL = 1999
    
    ! Maximum string lengths for error context
    integer, parameter, public :: MAX_MESSAGE_LEN = 512
    integer, parameter, public :: MAX_SUGGESTION_LEN = 512
    integer, parameter, public :: MAX_CONTEXT_LEN = 256
    integer, parameter, public :: MAX_STACK_TRACE_LEN = 2048
    
    ! Error context type for comprehensive error information
    type, public :: error_context_t
        integer :: error_code = ERROR_SUCCESS
        character(len=MAX_MESSAGE_LEN) :: message = ""
        character(len=MAX_SUGGESTION_LEN) :: suggestion = ""
        character(len=MAX_CONTEXT_LEN) :: context = ""
        character(len=MAX_STACK_TRACE_LEN) :: stack_trace = ""
        logical :: recoverable = .false.
        logical :: logged = .false.
    end type error_context_t
    
end module error_types