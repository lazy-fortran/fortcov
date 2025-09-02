module config_types
    !! Core configuration type definitions
    !! 
    !! This module defines the fundamental data structures for configuration
    !! management, extracted from the monolithic fortcov_config module to
    !! maintain single responsibility and size compliance.

    implicit none
    private

    ! Public type and constants
    public :: config_t
    public :: MAX_ARRAY_SIZE

    ! Array size constant
    integer, parameter :: MAX_ARRAY_SIZE = 100

    ! Main configuration type
    type :: config_t
        character(len=:), allocatable :: input_format
        character(len=:), allocatable :: output_format
        character(len=:), allocatable :: output_path
        character(len=:), allocatable :: source_paths(:)
        character(len=:), allocatable :: exclude_patterns(:)
        character(len=:), allocatable :: include_patterns(:)
        character(len=:), allocatable :: coverage_files(:)
        ! SECURITY FIX Issue #963: gcov_executable REMOVED - shell injection vulnerability
        real :: minimum_coverage
        real :: fail_under_threshold
        integer :: threads
        logical :: verbose
        logical :: quiet
        logical :: show_help
        logical :: show_version
        logical :: validate_config_only
        character(len=:), allocatable :: config_file
        logical :: enable_diff
        character(len=:), allocatable :: diff_baseline_file
        character(len=:), allocatable :: diff_current_file
        logical :: include_unchanged
        real :: diff_threshold
        character(len=:), allocatable :: import_file
        logical :: keep_gcov_files
        character(len=:), allocatable :: gcov_args
        logical :: strict_mode
        logical :: zero_configuration_mode
        integer :: max_files  ! Maximum number of files to process
        logical :: auto_discovery  ! Enable auto-discovery of source files/tests
        logical :: auto_test_execution  ! Enable automatic test execution
        integer :: test_timeout_seconds  ! Timeout for test execution in seconds
        
    end type config_t

end module config_types
