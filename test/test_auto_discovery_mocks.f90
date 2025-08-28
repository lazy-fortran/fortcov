module test_auto_discovery_mocks
    !! Mock implementations for auto-discovery testing
    !! Provides controlled test environments and production module replacements
    !! to eliminate circular dependencies between test and production code
    
    use error_handling_core, only: error_context_t, clear_error_context, ERROR_SUCCESS
    use config_core, only: config_t
    implicit none
    private
    
    ! Mock build system info type (matches production interface)
    type :: mock_build_system_info_t
        character(len=20) :: system_type = 'unknown'
        character(len=256) :: test_command = ''
        character(len=256) :: build_file = ''
        logical :: tool_available = .false.
    end type mock_build_system_info_t
    
    type :: mock_discovery_result_t
        logical :: success = .false.
        character(len=256) :: message = ''
    end type mock_discovery_result_t
    
    ! Public interface for mock implementations
    public :: mock_build_system_info_t, mock_discovery_result_t
    public :: create_mock_discovery_environment, cleanup_mock_discovery_environment
    public :: mock_detect_build_system, mock_execute_auto_test_workflow
    public :: mock_enhance_zero_config_with_auto_discovery
    public :: mock_execute_zero_config_complete_workflow
    public :: mock_run_coverage_analysis, mock_detect_build_system_simple
    public :: mock_discover_coverage_files, mock_launch_coverage_tui_mode
    public :: mock_perform_coverage_diff_analysis
    public :: mock_evaluate_exclude_patterns, mock_filter_coverage_files_by_patterns
    public :: mock_get_coverage_test_command
    
contains
    
    subroutine create_mock_discovery_environment()
        !! Create mock environment for testing
        call execute_command_line('mkdir -p mock_test_env')
        call execute_command_line('touch mock_test_env/test.gcov')
        call execute_command_line('touch mock_test_env/src.f90')
    end subroutine create_mock_discovery_environment
    
    subroutine cleanup_mock_discovery_environment()
        !! Clean up mock environment
        call execute_command_line('rm -rf mock_test_env')
    end subroutine cleanup_mock_discovery_environment
    
    subroutine mock_detect_build_system(project_path, build_info, error_ctx)
        !! Mock implementation of build system detection
        !! Returns predictable results for test validation
        character(len=*), intent(in) :: project_path
        type(mock_build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        ! Mock detection logic based on known test patterns
        if (index(project_path, 'fpm') > 0 .or. &
            len_trim(project_path) > 0) then
            build_info%system_type = 'fpm'
            build_info%test_command = 'fpm test'
            build_info%build_file = 'fpm.toml'
            build_info%tool_available = .true.
        else
            build_info%system_type = 'unknown'
            build_info%test_command = ''
            build_info%build_file = ''
            build_info%tool_available = .false.
        end if
        
        error_ctx%error_code = ERROR_SUCCESS
    end subroutine mock_detect_build_system
    
    function mock_execute_auto_test_workflow(config) result(exit_code)
        !! Mock implementation of auto test workflow execution
        !! Returns success code for controlled testing
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        ! Mock successful execution
        if (config%auto_test_execution) then
            exit_code = 0  ! Success
        else
            exit_code = 1  ! No auto test configured
        end if
    end function mock_execute_auto_test_workflow
    
    subroutine mock_enhance_zero_config_with_auto_discovery(config, exit_code)
        !! Mock implementation of zero-config enhancement
        type(config_t), intent(inout) :: config
        integer, intent(out) :: exit_code
        
        ! Mock enhancement - mark as auto-discovered
        config%auto_discovery = .true.
        exit_code = 0  ! Success
    end subroutine mock_enhance_zero_config_with_auto_discovery
    
    subroutine mock_execute_zero_config_complete_workflow(config, exit_code)
        !! Mock implementation of complete workflow execution
        type(config_t), intent(in) :: config
        integer, intent(out) :: exit_code
        
        ! Mock workflow execution based on config state
        if (config%auto_discovery) then
            exit_code = 0  ! Success with auto-discovery
        else
            exit_code = 2  ! Success without auto-discovery
        end if
    end subroutine mock_execute_zero_config_complete_workflow
    
    function mock_run_coverage_analysis(config) result(exit_code)
        !! Mock implementation of coverage analysis
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        ! Mock analysis based on quiet mode and other settings
        if (config%quiet) then
            exit_code = 0  ! Quiet mode success
        else
            exit_code = 1  ! Verbose mode with warnings
        end if
    end function mock_run_coverage_analysis
    
    subroutine mock_detect_build_system_simple(workspace_path, build_info, detected)
        !! Simple wrapper for mock build system detection
        character(len=*), intent(in) :: workspace_path
        type(mock_build_system_info_t), intent(out) :: build_info
        logical, intent(out) :: detected
        
        type(error_context_t) :: error_ctx
        
        call mock_detect_build_system(workspace_path, build_info, error_ctx)
        detected = (error_ctx%error_code == ERROR_SUCCESS)
    end subroutine mock_detect_build_system_simple
    
    function mock_discover_coverage_files(config) result(files)
        !! Mock implementation of coverage file discovery
        type(config_t), intent(in) :: config
        character(len=256), allocatable :: files(:)
        
        ! Mock file list for testing
        allocate(files(2))
        files(1) = 'mock_test1.gcov'
        files(2) = 'mock_test2.gcov'
    end function mock_discover_coverage_files
    
    function mock_launch_coverage_tui_mode(config) result(exit_code)
        !! Mock implementation of TUI mode launch
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        ! Mock TUI launch - always succeeds for test
        if (config%quiet) then
            exit_code = 0  ! Quiet mode success
        else
            exit_code = 1  ! Normal mode with status
        end if
    end function mock_launch_coverage_tui_mode
    
    function mock_perform_coverage_diff_analysis(config) result(exit_code)
        !! Mock implementation of coverage diff analysis
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        ! Mock diff analysis - always succeeds for test
        if (config%quiet) then
            exit_code = 0  ! Quiet mode success
        else
            exit_code = 1  ! Normal mode with status
        end if
    end function mock_perform_coverage_diff_analysis
    
    function mock_evaluate_exclude_patterns(filename, config) result(should_exclude)
        !! Mock implementation of exclude pattern evaluation
        character(len=*), intent(in) :: filename
        type(config_t), intent(in) :: config
        logical :: should_exclude
        
        ! Simple mock logic - exclude files containing 'test'
        should_exclude = index(filename, 'test') > 0
    end function mock_evaluate_exclude_patterns
    
    function mock_filter_coverage_files_by_patterns(files, config) result(filtered)
        !! Mock implementation of file filtering by patterns
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: filtered(:)
        
        integer :: i, count
        
        ! Simple mock - return first 2 files for testing
        count = min(2, size(files))
        allocate(character(len=len(files)) :: filtered(count))
        
        do i = 1, count
            filtered(i) = files(i)
        end do
    end function mock_filter_coverage_files_by_patterns
    
    subroutine mock_get_coverage_test_command(system_type, command, error_ctx)
        !! Mock implementation of test command generation
        character(len=*), intent(in) :: system_type
        character(len=*), intent(out) :: command
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        ! Mock command based on system type
        select case (trim(system_type))
        case ('fpm')
            command = 'fpm test --coverage'
        case ('cmake')
            command = 'make test'
        case ('make')
            command = 'make test'
        case default
            command = 'unknown'
        end select
        
        error_ctx%error_code = ERROR_SUCCESS
    end subroutine mock_get_coverage_test_command
    
end module test_auto_discovery_mocks