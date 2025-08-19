module foundation_constants
    !! Foundation Constants for Architectural Decomposition
    !! 
    !! Centralizes constants and parameters used across decomposed modules
    !! to prevent duplication and ensure consistency during refactoring.
    implicit none
    private
    
    ! Exit codes for consistency across all modules  
    integer, parameter, public :: EXIT_SUCCESS = 0
    integer, parameter, public :: EXIT_FAILURE = 1
    integer, parameter, public :: EXIT_THRESHOLD_NOT_MET = 2
    integer, parameter, public :: EXIT_NO_COVERAGE_DATA = 3
    integer, parameter, public :: EXIT_INVALID_CONFIG = 4
    integer, parameter, public :: EXIT_FILE_ACCESS_ERROR = 5
    integer, parameter, public :: EXIT_MEMORY_ERROR = 6
    integer, parameter, public :: EXIT_VALIDATION_ERROR = 7
    
    ! Architectural size limits and quality metrics
    integer, parameter, public :: MAX_MODULE_LINES = 400
    integer, parameter, public :: MAX_FUNCTION_LINES = 50
    integer, parameter, public :: MAX_COMPLEXITY_SCORE = 10
    integer, parameter, public :: MIN_TEST_COVERAGE = 80
    
    ! String length constants for consistency
    integer, parameter, public :: SHORT_STRING_LEN = 64
    integer, parameter, public :: MEDIUM_STRING_LEN = 256
    integer, parameter, public :: LONG_STRING_LEN = 1024
    integer, parameter, public :: PATH_STRING_LEN = 2048
    
    ! File system and path constants
    character(len=1), parameter, public :: PATH_SEPARATOR = '/'
    character(len=3), parameter, public :: FORTRAN_EXTENSION = '.f90'
    character(len=5), parameter, public :: GCOV_EXTENSION = '.gcov'
    character(len=5), parameter, public :: JSON_EXTENSION = '.json'
    
    ! Coverage analysis constants
    real, parameter, public :: DEFAULT_THRESHOLD = 80.0
    real, parameter, public :: MINIMUM_THRESHOLD = 0.0
    real, parameter, public :: MAXIMUM_THRESHOLD = 100.0
    real, parameter, public :: PRECISION_EPSILON = 1.0e-6
    
    ! Array size limits for safety
    integer, parameter, public :: MAX_FILES = 10000
    integer, parameter, public :: MAX_EXCLUDES = 1000
    integer, parameter, public :: MAX_DEPENDENCIES = 100
    integer, parameter, public :: MAX_PUBLIC_INTERFACES = 50
    
    ! JSON parsing constants
    integer, parameter, public :: JSON_MAX_DEPTH = 20
    integer, parameter, public :: JSON_MAX_KEYS = 1000
    integer, parameter, public :: JSON_BUFFER_SIZE = 65536
    
    ! Performance and timeout constants
    integer, parameter, public :: DEFAULT_TIMEOUT_SECONDS = 300
    integer, parameter, public :: MAX_RETRY_ATTEMPTS = 3
    integer, parameter, public :: PERFORMANCE_SAMPLE_SIZE = 1000
    
    ! Validation constants
    integer, parameter, public :: MIN_VALID_COVERAGE = 0
    integer, parameter, public :: MAX_VALID_COVERAGE = 100
    integer, parameter, public :: MIN_FILENAME_LENGTH = 1
    integer, parameter, public :: MAX_FILENAME_LENGTH = 255
    
    ! Configuration defaults
    logical, parameter, public :: DEFAULT_VERBOSE = .false.
    logical, parameter, public :: DEFAULT_QUIET = .false.
    logical, parameter, public :: DEFAULT_EXCLUDE_TESTS = .true.
    logical, parameter, public :: DEFAULT_SHOW_SUMMARY = .true.
    logical, parameter, public :: DEFAULT_FAIL_ON_THRESHOLD = .true.
    
    ! Error message constants
    character(len=*), parameter, public :: ERROR_INVALID_INPUT = &
        "Invalid input parameter provided"
    character(len=*), parameter, public :: ERROR_FILE_NOT_FOUND = &
        "Required file not found"
    character(len=*), parameter, public :: ERROR_MEMORY_ALLOCATION = &
        "Memory allocation failed"
    character(len=*), parameter, public :: ERROR_PARSE_FAILURE = &
        "Parsing operation failed"
    character(len=*), parameter, public :: ERROR_VALIDATION_FAILURE = &
        "Validation check failed"
    
    ! Status message constants
    character(len=*), parameter, public :: STATUS_SUCCESS = "Operation completed successfully"
    character(len=*), parameter, public :: STATUS_WARNING = "Operation completed with warnings"
    character(len=*), parameter, public :: STATUS_ERROR = "Operation failed"
    character(len=*), parameter, public :: STATUS_CRITICAL = "Critical error encountered"
    
    ! Decomposition safety constants
    integer, parameter, public :: MAX_DECOMPOSITION_MODULES = 10
    integer, parameter, public :: MIN_INTERFACE_COMPATIBILITY = 100  ! Percentage
    integer, parameter, public :: SAFETY_VALIDATION_SAMPLES = 100
    
end module foundation_constants