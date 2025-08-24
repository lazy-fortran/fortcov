module test_build_gcov_auto_discovery
    !! Test Build and Gcov Auto-Discovery Module (Issue #277)
    !!
    !! Completes the auto-discovery ecosystem by providing comprehensive
    !! test build discovery and automatic gcov processing. This is the final
    !! Sprint 1 feature that integrates all auto-discovery components into
    !! a seamless workflow.
    !!
    !! Key Features:
    !! - Auto-discovery of test build capabilities using build system detection
    !! - Automatic execution of tests with appropriate coverage flags
    !! - Auto-processing of gcov files with build system context
    !! - Complete workflow integration from command to coverage report
    !! - Graceful degradation when tools are unavailable
    !! - Comprehensive error handling and user guidance
    !!
    !! Integration Points:
    !! - Uses build_system_detector for build system detection
    !! - Uses coverage_workflows for test execution
    !! - Uses secure_command_executor for safe command execution
    !! - Integrates with zero_config_auto_discovery_integration

    use config_types, only: config_t
    use build_system_detector, only: build_system_info_t, detect_build_system, &
                                     get_coverage_test_command, &
                                     validate_build_tool_available
    use coverage_workflows, only: execute_auto_test_workflow
    use secure_command_executor, only: escape_shell_argument, &
                                       validate_executable_path
    use error_handling, only: error_context_t, ERROR_SUCCESS, &
                              ERROR_INVALID_CONFIG, ERROR_FILE_OPERATION_FAILED, &
                              clear_error_context
    use file_utils, only: file_exists, find_files
    use string_utils, only: trim_string
    use foundation_constants, only: EXIT_SUCCESS, EXIT_FAILURE
    implicit none
    private

    ! Public interface for test build and gcov auto-discovery
    public :: auto_discover_test_build
    public :: auto_process_gcov_files  
    public :: execute_complete_auto_workflow

    ! Public result types for comprehensive feedback
    public :: test_build_result_t
    public :: gcov_result_t
    public :: source_mapping_t
    public :: complete_workflow_result_t

    ! Result type definitions
    type :: test_build_result_t
        logical :: success = .false.
        character(len=20) :: build_system = 'unknown'
        character(len=512) :: test_command = ''
        logical :: tool_available = .false.
        character(len=256) :: error_message = ''
        character(len=512) :: guidance_message = ''
    end type test_build_result_t

    type :: source_mapping_t
        character(len=256) :: source_file = ''
        character(len=256) :: gcov_file = ''
        character(len=256) :: build_path = ''
    end type source_mapping_t

    type :: gcov_result_t
        logical :: success = .false.
        character(len=:), allocatable :: gcov_files(:)
        type(source_mapping_t), allocatable :: source_mappings(:)
        logical :: used_build_context = .false.
        character(len=256) :: error_message = ''
        character(len=512) :: guidance_message = ''
    end type gcov_result_t

    type :: complete_workflow_result_t
        logical :: success = .false.
        logical :: test_executed = .false.
        logical :: tests_passed = .false.
        logical :: gcov_processed = .false.
        logical :: coverage_generated = .false.
        logical :: auto_discovery_used = .false.
        logical :: used_manual_files = .false.
        logical :: timed_out = .false.
        character(len=512) :: error_message = ''
    end type complete_workflow_result_t

    ! Constants
    integer, parameter :: MAX_GCOV_FILES = 1000
    integer, parameter :: MAX_SOURCE_MAPPINGS = 1000
    character(len=*), parameter :: GCDA_PATTERN = '**/*.gcda'
    character(len=*), parameter :: GCNO_PATTERN = '**/*.gcno'
    character(len=*), parameter :: GCOV_PATTERN = '*.gcov'

contains

    subroutine auto_discover_test_build(project_path, config, result)
        !! Auto-discover test build capabilities
        !!
        !! Detects build system and determines appropriate test command
        !! with coverage flags. Validates that build tools are available
        !! and provides guidance when build system cannot be used.
        !!
        !! Args:
        !!   project_path: Directory to scan for build system
        !!   config: Configuration with auto-discovery settings
        !!   result: Populated with discovery results and guidance

        character(len=*), intent(in) :: project_path
        type(config_t), intent(in) :: config
        type(test_build_result_t), intent(out) :: result

        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx
        logical :: path_valid

        ! Initialize result
        result%success = .false.
        result%build_system = 'unknown'
        result%test_command = ''
        result%tool_available = .false.
        result%error_message = ''
        result%guidance_message = ''

        ! Skip if auto-discovery disabled
        if (.not. config%auto_discovery) then
            result%guidance_message = 'Auto-discovery disabled in configuration'
            return
        end if

        ! Validate project path
        call validate_project_path(project_path, path_valid, result%error_message)
        if (.not. path_valid) return

        ! Detect build system
        call detect_build_system(project_path, build_info, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            result%error_message = trim(error_ctx%message)
            call provide_build_system_guidance(result)
            return
        end if

        ! Process detection results
        result%build_system = build_info%system_type
        result%tool_available = build_info%tool_available
        
        if (build_info%system_type == 'unknown') then
            call handle_unknown_build_system(result)
        else if (.not. build_info%tool_available) then
            call handle_unavailable_build_tool(build_info, result)
        else
            call configure_test_command(build_info, result)
        end if

    end subroutine auto_discover_test_build

    subroutine auto_process_gcov_files(project_path, config, result)
        !! Auto-process gcov files with build system context
        !!
        !! Finds .gcda files automatically after test execution and
        !! processes them using gcov with appropriate build system context.
        !! Auto-discovers source file mappings and provides comprehensive
        !! error handling.
        !!
        !! Args:
        !!   project_path: Project root directory
        !!   config: Configuration with gcov settings
        !!   result: Populated with processing results and file mappings

        character(len=*), intent(in) :: project_path  
        type(config_t), intent(in) :: config
        type(gcov_result_t), intent(out) :: result

        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: build_dirs(:)
        logical :: path_valid

        ! Initialize result
        result%success = .false.
        result%used_build_context = .false.
        result%error_message = ''
        result%guidance_message = ''

        ! Skip if auto-discovery disabled
        if (.not. config%auto_discovery) then
            result%guidance_message = 'Auto-discovery disabled in configuration'
            return
        end if

        ! Validate project path
        call validate_project_path(project_path, path_valid, result%error_message)
        if (.not. path_valid) return

        ! Find .gcda files (indicates executed coverage data)
        call find_gcda_files(project_path, gcda_files)
        
        if (.not. allocated(gcda_files) .or. size(gcda_files) == 0) then
            call handle_no_gcda_files(result)
            return
        end if

        ! Extract build directories from .gcda file paths
        call extract_build_directories(gcda_files, build_dirs)
        
        ! Process gcov files in build directories
        call process_gcov_in_build_dirs(build_dirs, config, result)

        if (result%success) then
            ! Discover source file mappings
            call discover_source_mappings(result%gcov_files, result%source_mappings)
            result%used_build_context = allocated(build_dirs) .and. size(build_dirs) > 0
        end if

    end subroutine auto_process_gcov_files

    subroutine execute_complete_auto_workflow(config, result)
        !! Execute complete auto-discovery workflow
        !!
        !! Orchestrates the complete workflow from auto-discovery to coverage
        !! report generation. Integrates test build discovery, test execution,
        !! gcov processing, and coverage analysis into a seamless experience.
        !!
        !! Workflow Steps:
        !! 1. Auto-discover test build capabilities
        !! 2. Execute tests with coverage flags (if available)
        !! 3. Auto-process gcov files from test execution
        !! 4. Generate coverage analysis and reports
        !!
        !! Args:
        !!   config: Configuration for the complete workflow
        !!   result: Comprehensive workflow execution results

        type(config_t), intent(in) :: config
        type(complete_workflow_result_t), intent(out) :: result

        type(test_build_result_t) :: test_result
        type(gcov_result_t) :: gcov_result
        integer :: test_exit_code
        logical :: manual_files_specified

        ! Initialize result
        result%success = .false.
        result%test_executed = .false.
        result%tests_passed = .false.
        result%gcov_processed = .false.
        result%coverage_generated = .false.
        result%auto_discovery_used = .false.
        result%used_manual_files = .false.
        result%timed_out = .false.
        result%error_message = ''

        ! Check if manual coverage files are specified
        manual_files_specified = allocated(config%coverage_files)
        result%used_manual_files = manual_files_specified

        ! Skip auto-discovery workflow if disabled
        if (.not. config%auto_discovery .and. .not. config%zero_configuration_mode) then
            result%success = .true.  ! Success, just not using auto-discovery
            result%auto_discovery_used = .false.
            return
        end if

        result%auto_discovery_used = .true.

        ! Step 1: Auto-discover test build capabilities
        call auto_discover_test_build('.', config, test_result)

        ! Step 2: Execute tests with coverage if build system available
        if (test_result%success .and. config%auto_test_execution) then
            test_exit_code = execute_auto_test_workflow(config)
            result%test_executed = .true.
            result%tests_passed = (test_exit_code == EXIT_SUCCESS)
            
            if (test_exit_code == 124) then  ! Standard timeout exit code
                result%timed_out = .true.
                result%error_message = 'Test execution timed out'
            else if (test_exit_code /= EXIT_SUCCESS) then
                result%error_message = 'Tests failed during execution'
            end if
        end if

        ! Step 3: Auto-process gcov files (even if tests failed/skipped)
        if (.not. manual_files_specified) then
            call auto_process_gcov_files('.', config, gcov_result)
            result%gcov_processed = gcov_result%success
            
            if (.not. gcov_result%success) then
                ! Append gcov error to workflow error message
                if (len_trim(result%error_message) > 0) then
                    result%error_message = trim(result%error_message) // '; ' // &
                                          trim(gcov_result%error_message)
                else
                    result%error_message = trim(gcov_result%error_message)
                end if
            end if
        else
            ! Using manual files, mark as processed
            result%gcov_processed = .true.
        end if

        ! Step 4: Determine overall success
        call determine_workflow_success(result, test_result, gcov_result)

        ! Mark coverage as generated if we have processable files
        result%coverage_generated = result%gcov_processed .or. result%used_manual_files

    end subroutine execute_complete_auto_workflow

    ! Internal helper subroutines

    subroutine validate_project_path(project_path, valid, error_message)
        !! Validate project path for security and accessibility
        character(len=*), intent(in) :: project_path
        logical, intent(out) :: valid
        character(len=*), intent(out) :: error_message

        valid = .false.
        error_message = ''

        if (len_trim(project_path) == 0) then
            error_message = 'Empty project path provided'
            return
        end if

        if (len_trim(project_path) > 4096) then
            error_message = 'Project path too long'
            return
        end if

        ! Check if directory exists and is accessible
        inquire(file=trim(project_path), exist=valid)
        if (.not. valid) then
            error_message = 'Project directory not found or not accessible: ' // &
                           trim(project_path)
            return
        end if

        valid = .true.
    end subroutine validate_project_path

    subroutine provide_build_system_guidance(result)
        !! Provide guidance when build system detection fails
        type(test_build_result_t), intent(inout) :: result

        result%guidance_message = &
            'Build system detection failed. Supported systems: FPM, CMake, Make, Meson. ' // &
            'Ensure build files exist (fpm.toml, CMakeLists.txt, Makefile, meson.build).'
    end subroutine provide_build_system_guidance

    subroutine handle_unknown_build_system(result)
        !! Handle detection of unknown build system
        type(test_build_result_t), intent(inout) :: result

        result%success = .false.
        result%build_system = 'unknown'
        result%guidance_message = &
            'No known build system detected. Auto-test execution disabled. ' // &
            'Supported: FPM (fpm.toml), CMake (CMakeLists.txt), Make (Makefile), ' // &
            'Meson (meson.build). Coverage analysis will continue with existing files.'
    end subroutine handle_unknown_build_system

    subroutine handle_unavailable_build_tool(build_info, result)
        !! Handle unavailable build tool
        type(build_system_info_t), intent(in) :: build_info
        type(test_build_result_t), intent(inout) :: result

        result%success = .false.
        result%error_message = 'Build tool not available: ' // trim(build_info%system_type)
        result%guidance_message = &
            'Install ' // trim(build_info%system_type) // ' build tool and ensure ' // &
            'it is available in PATH. Auto-test execution disabled.'
    end subroutine handle_unavailable_build_tool

    subroutine configure_test_command(build_info, result)
        !! Configure test command with coverage flags
        type(build_system_info_t), intent(in) :: build_info
        type(test_build_result_t), intent(inout) :: result

        type(error_context_t) :: error_ctx

        call get_coverage_test_command(build_info%system_type, &
                                      result%test_command, error_ctx)
        
        if (error_ctx%error_code == ERROR_SUCCESS) then
            result%success = .true.
            result%guidance_message = 'Test command configured with coverage flags: ' // &
                                     trim(result%test_command)
        else
            result%success = .false.
            result%error_message = trim(error_ctx%message)
            result%guidance_message = &
                'Failed to configure test command for ' // trim(build_info%system_type)
        end if
    end subroutine configure_test_command

    subroutine find_gcda_files(project_path, gcda_files)
        !! Find .gcda files in project directory and subdirectories
        character(len=*), intent(in) :: project_path
        character(len=:), allocatable, intent(out) :: gcda_files(:)

        character(len=512) :: search_pattern
        character(len=:), allocatable :: found_files(:)

        ! Build search pattern for .gcda files
        write(search_pattern, '(A,A,A)') trim(project_path), '/', GCDA_PATTERN

        ! Use file_utils to find files
        found_files = find_files(search_pattern)

        if (allocated(found_files)) then
            gcda_files = found_files
        end if
    end subroutine find_gcda_files

    subroutine handle_no_gcda_files(result)
        !! Handle case when no .gcda files are found
        type(gcov_result_t), intent(inout) :: result

        result%success = .false.
        result%error_message = 'No .gcda files found'
        result%guidance_message = &
            'No coverage data files (.gcda) found. This usually means: ' // &
            '1) Tests have not been run with coverage flags, or ' // &
            '2) Coverage instrumentation is not enabled. ' // &
            'Try running tests with coverage flags (e.g., fpm test --flag ' // &
            '"-fprofile-arcs -ftest-coverage") or enable auto-test execution.'
    end subroutine handle_no_gcda_files

    subroutine extract_build_directories(gcda_files, build_dirs)
        !! Extract unique build directories from .gcda file paths
        character(len=*), intent(in) :: gcda_files(:)
        character(len=:), allocatable, intent(out) :: build_dirs(:)

        character(len=512) :: unique_dirs(size(gcda_files))
        integer :: i, j, num_dirs, last_slash
        logical :: already_added

        num_dirs = 0

        ! Extract directory from each .gcda file path
        do i = 1, size(gcda_files)
            last_slash = index(gcda_files(i), '/', back=.true.)
            if (last_slash > 0) then
                ! Check if directory already added
                already_added = .false.
                do j = 1, num_dirs
                    if (trim(unique_dirs(j)) == gcda_files(i)(1:last_slash-1)) then
                        already_added = .true.
                        exit
                    end if
                end do

                if (.not. already_added) then
                    num_dirs = num_dirs + 1
                    unique_dirs(num_dirs) = gcda_files(i)(1:last_slash-1)
                end if
            end if
        end do

        ! Allocate result array
        if (num_dirs > 0) then
            allocate(character(len=512) :: build_dirs(num_dirs))
            do i = 1, num_dirs
                build_dirs(i) = trim(unique_dirs(i))
            end do
        end if
    end subroutine extract_build_directories

    subroutine process_gcov_in_build_dirs(build_dirs, config, result)
        !! Process gcov files in build directories
        character(len=*), intent(in) :: build_dirs(:)
        type(config_t), intent(in) :: config
        type(gcov_result_t), intent(inout) :: result

        character(len=:), allocatable :: gcov_exe
        character(len=1024) :: command
        character(len=:), allocatable :: generated_files(:)
        integer :: i, exit_status
        logical :: any_success

        any_success = .false.

        ! Determine gcov executable
        if (allocated(config%gcov_executable)) then
            gcov_exe = trim(config%gcov_executable)
        else
            gcov_exe = 'gcov'
        end if

        ! Validate gcov executable availability
        block
            character(len=:), allocatable :: safe_gcov_path
            type(error_context_t) :: gcov_error_ctx
            
            call validate_executable_path(gcov_exe, safe_gcov_path, gcov_error_ctx)
            if (gcov_error_ctx%error_code /= ERROR_SUCCESS) then
                result%error_message = 'Gcov executable not found: ' // gcov_exe
                result%guidance_message = &
                    'Install gcov (usually part of GCC) and ensure it is in PATH.'
                return
            end if
        end block

        ! Process each build directory
        do i = 1, size(build_dirs)
            call process_single_build_dir(build_dirs(i), gcov_exe, &
                                         exit_status, generated_files)
            
            if (exit_status == 0 .and. allocated(generated_files)) then
                any_success = .true.
                call append_generated_files(result%gcov_files, generated_files)
            end if
        end do

        if (any_success) then
            result%success = .true.
        else
            result%error_message = 'Gcov processing failed in all build directories'
            result%guidance_message = &
                'Gcov failed to process coverage data. Check that .gcno files exist ' // &
                'and gcov executable is compatible with coverage data format.'
        end if
    end subroutine process_gcov_in_build_dirs

    subroutine process_single_build_dir(build_dir, gcov_exe, exit_status, &
                                       generated_files)
        !! Process gcov in single build directory
        character(len=*), intent(in) :: build_dir, gcov_exe
        integer, intent(out) :: exit_status
        character(len=:), allocatable, intent(out) :: generated_files(:)

        character(len=1024) :: command
        character(len=256) :: gcov_pattern

        ! Build gcov command to process all .gcno files in directory
        write(command, '(A)') 'cd ' // escape_shell_argument(trim(build_dir)) // &
                              ' && find . -name "*.gcno" -exec ' // &
                              escape_shell_argument(trim(gcov_exe)) // ' {} \; 2>/dev/null'

        ! Execute gcov command
        call execute_command_line(command, exitstat=exit_status)

        if (exit_status == 0) then
            ! Find generated .gcov files in build directory
            write(gcov_pattern, '(A,A,A)') trim(build_dir), '/', GCOV_PATTERN
            generated_files = find_files(gcov_pattern)
        end if
    end subroutine process_single_build_dir

    subroutine append_generated_files(all_files, new_files)
        !! Append new files to all_files array
        character(len=:), allocatable, intent(inout) :: all_files(:)
        character(len=:), allocatable, intent(in) :: new_files(:)

        character(len=:), allocatable :: temp_files(:)
        integer :: old_size, new_size, i

        if (.not. allocated(all_files)) then
            all_files = new_files
            return
        end if

        old_size = size(all_files)
        new_size = old_size + size(new_files)

        ! Create temporary array with combined size
        allocate(character(len=max(len(all_files), len(new_files))) :: temp_files(new_size))

        ! Copy existing files
        do i = 1, old_size
            temp_files(i) = all_files(i)
        end do

        ! Copy new files
        do i = 1, size(new_files)
            temp_files(old_size + i) = new_files(i)
        end do

        ! Replace all_files with expanded array
        call move_alloc(temp_files, all_files)
    end subroutine append_generated_files

    subroutine discover_source_mappings(gcov_files, source_mappings)
        !! Discover source file mappings from gcov files
        character(len=*), intent(in) :: gcov_files(:)
        type(source_mapping_t), allocatable, intent(out) :: source_mappings(:)

        integer :: i, num_mappings
        character(len=256) :: source_file
        
        if (size(gcov_files) == 0) then
            return
        end if

        num_mappings = size(gcov_files)
        allocate(source_mappings(num_mappings))

        ! For each gcov file, extract source file mapping
        do i = 1, num_mappings
            call extract_source_from_gcov(gcov_files(i), source_file)
            
            source_mappings(i)%gcov_file = trim(gcov_files(i))
            source_mappings(i)%source_file = trim(source_file)
            source_mappings(i)%build_path = extract_build_path(gcov_files(i))
        end do
    end subroutine discover_source_mappings

    subroutine extract_source_from_gcov(gcov_file, source_file)
        !! Extract source file name from gcov file name
        character(len=*), intent(in) :: gcov_file
        character(len=*), intent(out) :: source_file

        integer :: last_slash, last_dot
        character(len=256) :: basename

        ! Extract basename from path
        last_slash = index(gcov_file, '/', back=.true.)
        if (last_slash > 0) then
            basename = gcov_file(last_slash+1:)
        else
            basename = gcov_file
        end if

        ! Remove .gcov extension
        last_dot = index(basename, '.gcov', back=.true.)
        if (last_dot > 0) then
            source_file = basename(1:last_dot-1)
        else
            source_file = basename
        end if
    end subroutine extract_source_from_gcov

    function extract_build_path(gcov_file) result(build_path)
        !! Extract build directory path from gcov file path
        character(len=*), intent(in) :: gcov_file
        character(len=256) :: build_path

        integer :: last_slash

        last_slash = index(gcov_file, '/', back=.true.)
        if (last_slash > 0) then
            build_path = gcov_file(1:last_slash-1)
        else
            build_path = '.'
        end if
    end function extract_build_path

    subroutine determine_workflow_success(result, test_result, gcov_result)
        !! Determine overall workflow success based on component results
        type(complete_workflow_result_t), intent(inout) :: result
        type(test_build_result_t), intent(in) :: test_result
        type(gcov_result_t), intent(in) :: gcov_result

        ! Workflow succeeds if:
        ! 1. Tests passed (if executed), AND
        ! 2. Gcov processing succeeded (if attempted) OR manual files used

        if (result%test_executed .and. .not. result%tests_passed) then
            ! Tests failed - workflow fails
            result%success = .false.
        else if (.not. result%used_manual_files .and. .not. result%gcov_processed) then
            ! No files to process - workflow fails
            result%success = .false.
        else
            ! Either tests passed/skipped and gcov succeeded, or manual files used
            result%success = .true.
        end if

        ! Special case: timeout always indicates failure
        if (result%timed_out) then
            result%success = .false.
        end if
    end subroutine determine_workflow_success

end module test_build_gcov_auto_discovery