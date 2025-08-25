module coverage_workflows
    !! Coverage Workflows and File Management (Decomposed from coverage_engine.f90)
    !! 
    !! Focused on coverage file discovery, filtering, and workflow operations.
    !! Provides specialized workflow management separated from core analysis.
    use foundation_constants
    use foundation_layer_utils
    use coverage_model
    use fortcov_config
    use coverage_diff
    use file_utils
    use string_utils, only: format_integer, to_lower, matches_pattern
    use error_handling
    use zero_configuration_manager, only: auto_discover_coverage_files_priority
    use build_system_detector
    use shell_utils, only: escape_shell_argument
    implicit none
    private
    
    public :: discover_coverage_files
    public :: evaluate_exclude_patterns
    public :: perform_coverage_diff_analysis
    public :: launch_coverage_tui_mode
    public :: filter_coverage_files_by_patterns
    public :: execute_auto_test_workflow
    
contains
    
    function discover_coverage_files(config) result(files)
        !! Coverage file discovery implementation
        !! Extracted from original find_coverage_files function
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: files(:)
        
        character(len=:), allocatable :: all_files(:)
        character(len=:), allocatable :: filtered_files(:)
        
        ! Find coverage files based on configuration
        call determine_coverage_files_source(config, all_files)
        
        ! Apply filtering if needed
        call apply_coverage_file_filtering(all_files, config, filtered_files)
        
        ! Validate and limit files
        call validate_and_limit_files(filtered_files, config, files)
        
    end function discover_coverage_files
    
    function evaluate_exclude_patterns(filepath, config) result(should_exclude)
        !! Pattern evaluation implementation for both include and exclude patterns
        !! Enhanced from original check_exclude_patterns function
        character(len=*), intent(in) :: filepath
        type(config_t), intent(in) :: config
        logical :: should_exclude
        
        character(len=:), allocatable :: normalized_path, basename
        logical :: matches_include
        
        should_exclude = .false.
        
        ! Prepare normalized paths for pattern matching
        call prepare_pattern_paths(filepath, normalized_path, basename)
        
        ! Check include patterns first
        call check_include_patterns(normalized_path, basename, config, matches_include)
        if (.not. matches_include) then
            should_exclude = .true.
            return
        end if
        
        ! Check exclude patterns
        call check_exclude_patterns(normalized_path, basename, config, should_exclude)
        if (should_exclude) return
        
        ! Check test file exclusion patterns
        call check_test_file_exclusion(normalized_path, config, should_exclude)
        
    end function evaluate_exclude_patterns
    
    function perform_coverage_diff_analysis(config) result(exit_code)
        !! Coverage diff analysis workflow implementation
        !! Extracted from original analyze_coverage_diff function
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(coverage_diff_t) :: diff_result
        logical :: diff_success
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "📊 Analyzing coverage differences..."
            if (allocated(config%diff_baseline_file)) then
                print *, "   Baseline: " // trim(config%diff_baseline_file)
            end if
            if (allocated(config%diff_current_file)) then
                print *, "   Compare:  " // trim(config%diff_current_file)
            end if
        end if
        
        ! Perform coverage diff analysis
        ! Note: This would need actual coverage data loaded from baseline and current files
        ! For now, just set a success flag and empty result
        diff_success = .true.
        
        if (.not. diff_success) then
            if (.not. config%quiet) then
                print *, "❌ Coverage diff analysis failed"
            end if
            exit_code = EXIT_FAILURE
            return
        end if
        
        ! Output diff summary
        call output_coverage_diff_summary(diff_result, config)
        
        ! Apply threshold validation for diff
        if (config%minimum_coverage > 0.0) then
            if (diff_result%current_coverage < config%minimum_coverage) then
                if (.not. config%quiet) then
                    print *, "❌ Coverage threshold not met in comparison"
                    write(*, '(A, F5.1, A, F5.1, A)') &
                        "   Required: ", config%minimum_coverage, "%, Current: ", &
                        diff_result%current_coverage, "%"
                end if
                exit_code = EXIT_THRESHOLD_NOT_MET
            end if
        end if
        
    end function perform_coverage_diff_analysis
    
    function launch_coverage_tui_mode(config) result(exit_code)
        !! TUI mode launch workflow implementation
        !! Extracted from original launch_tui_mode function
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        logical :: tui_success
        
        exit_code = EXIT_SUCCESS
        
        if (.not. config%quiet) then
            print *, "🖥️  Launching Terminal User Interface..."
        end if
        
        ! Launch TUI with configuration
        call start_tui_interface(config, tui_success)
        
        if (.not. tui_success) then
            if (.not. config%quiet) then
                print *, "❌ TUI launch failed"
            end if
            exit_code = EXIT_FAILURE
        end if
        
    end function launch_coverage_tui_mode
    
    function filter_coverage_files_by_patterns(files, config) result(filtered)
        !! File filtering by patterns implementation
        !! Extracted from original filter_files_by_patterns function
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: filtered(:)
        
        character(len=:), allocatable :: temp_files(:)
        integer :: i, filtered_count
        logical :: should_exclude
        
        ! Count files that pass filtering
        filtered_count = 0
        do i = 1, size(files)
            should_exclude = evaluate_exclude_patterns(files(i), config)
            if (.not. should_exclude) then
                filtered_count = filtered_count + 1
            end if
        end do
        
        ! Allocate filtered array
        if (filtered_count > 0) then
            allocate(character(len=len(files(1))) :: filtered(filtered_count))
            
            ! Copy non-excluded files
            filtered_count = 0
            do i = 1, size(files)
                should_exclude = evaluate_exclude_patterns(files(i), config)
                if (.not. should_exclude) then
                    filtered_count = filtered_count + 1
                    filtered(filtered_count) = files(i)
                end if
            end do
        end if
        
    end function filter_coverage_files_by_patterns
    
    subroutine discover_gcov_files(config, files)
        !! Discovers .gcov files in configured source paths with automatic gcov generation
        !! Implements "sane default mode" - auto-discovers coverage files and runs gcov
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: files(:)
        
        character(len=:), allocatable :: search_paths(:)
        character(len=:), allocatable :: found_files(:)
        character(len=:), allocatable :: generated_files(:)
        
        ! Determine search paths based on configuration
        call determine_gcov_search_paths(config, search_paths, found_files)
        
        ! Search for existing .gcov files if needed
        call search_existing_gcov_files(search_paths, found_files)
        
        ! Generate gcov files if none found and in auto-discovery mode
        call attempt_gcov_generation(config, found_files, generated_files)
        
        ! Set final result
        call finalize_gcov_file_result(found_files, generated_files, files)
        
    end subroutine discover_gcov_files
    
    subroutine auto_generate_gcov_files(config, generated_files)
        !! Automatically discovers and generates .gcov files from build directories
        !! Implements the core "sane default mode" functionality for Issue #196
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: generated_files(:)
        
        character(len=:), allocatable :: build_dirs(:)
        character(len=:), allocatable :: all_gcov_files(:)
        logical :: success
        
        ! Find build directories with coverage data
        call find_coverage_build_directories(build_dirs)
        
        if (.not. allocated(build_dirs) .or. size(build_dirs) == 0) then
            return ! No build directories found
        end if
        
        ! Generate gcov files from build directories
        call generate_gcov_from_build_dirs(config, build_dirs, success)
        
        if (success) then
            ! Copy generated .gcov files to project root and collect them
            call collect_generated_gcov_files(build_dirs, all_gcov_files)
            if (allocated(all_gcov_files)) then
                generated_files = all_gcov_files
            end if
        end if
        
    end subroutine auto_generate_gcov_files
    
    subroutine find_coverage_build_directories(build_dirs)
        !! Finds build directories containing coverage data files
        character(len=:), allocatable, intent(out) :: build_dirs(:)
        
        character(len=:), allocatable :: gcda_files(:)
        character(len=500) :: dir_path
        integer :: i
        
        ! Find all .gcda files (indicates executed coverage data)
        gcda_files = find_files("build/**/fortcov/*.gcda")
        
        if (allocated(gcda_files) .and. size(gcda_files) > 0) then
            ! Use first .gcda file to determine build directory structure
            ! For simplicity, assume all build dirs follow same pattern
            dir_path = gcda_files(1)
            
            ! Extract directory path (remove filename)
            i = index(dir_path, '/', back=.true.)
            if (i > 0) then
                dir_path = dir_path(1:i-1)
                
                ! Allocate and set single build directory
                allocate(character(len=500) :: build_dirs(1))
                build_dirs(1) = trim(dir_path)
            end if
        end if
        
    end subroutine find_coverage_build_directories
    
    subroutine collect_generated_gcov_files(build_dirs, gcov_files)
        !! Collects generated .gcov files and copies them to build/gcov directory
        character(len=*), intent(in) :: build_dirs(:)
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        
        character(len=:), allocatable :: found_gcov_files(:)
        character(len=300) :: command
        character(len=256), parameter :: GCOV_OUTPUT_DIR = "build/gcov"
        integer :: i, stat
        logical :: dir_exists
        
        ! Ensure the gcov output directory exists
        inquire(file=GCOV_OUTPUT_DIR, exist=dir_exists)
        if (.not. dir_exists) then
            call execute_command_line("mkdir -p " // escape_shell_argument(GCOV_OUTPUT_DIR), exitstat=stat)
        end if
        
        ! Copy all .gcov files from build directories to build/gcov
        do i = 1, size(build_dirs)
            write(command, '(A)') 'find ' // escape_shell_argument(trim(build_dirs(i))) // &
                ' -name "*.gcov" -exec cp {} ' // escape_shell_argument(GCOV_OUTPUT_DIR) // '/ \; 2>/dev/null'
            call execute_command_line(command)
        end do
        
        ! Now find the .gcov files in the gcov output directory
        found_gcov_files = find_files(GCOV_OUTPUT_DIR // "/*.gcov")
        
        if (allocated(found_gcov_files)) then
            gcov_files = found_gcov_files
        end if
        
    end subroutine collect_generated_gcov_files
    
    
    function is_test_file(filepath) result(is_test)
        !! Checks if file appears to be a test file
        character(len=*), intent(in) :: filepath
        logical :: is_test
        
        character(len=:), allocatable :: lower_path
        
        lower_path = to_lower(filepath)
        
        is_test = (index(lower_path, 'test') > 0) .or. &
                  (index(lower_path, 'spec') > 0) .or. &
                  (index(lower_path, 'check') > 0)
        
    end function is_test_file
    
    function normalize_path(filepath) result(normalized)
        !! Normalizes file path for consistent processing
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable :: normalized
        integer :: i
        
        ! Basic path normalization
        normalized = trim(filepath)
        
        ! Convert backslashes to forward slashes for consistency
        ! Simple string replacement - replace '\' with '/'
        do i = 1, len(normalized)
            if (normalized(i:i) == '\') then
                normalized(i:i) = '/'
            end if
        end do
        
    end function normalize_path
    
    subroutine output_coverage_diff_summary(diff_result, config)
        !! Outputs coverage diff analysis summary
        type(coverage_diff_t), intent(in) :: diff_result
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "📊 Coverage Diff Analysis Results:"
            write(*, '(A, F5.1, A)') "   Baseline Coverage: ", &
                diff_result%baseline_coverage, "%"
            write(*, '(A, F5.1, A)') "   Current Coverage:  ", &
                diff_result%current_coverage, "%"
            write(*, '(A, F5.1, A)') "   Coverage Change:   ", &
                diff_result%coverage_change, "%"
            
            if (diff_result%coverage_change > 0.0) then
                print *, "   ✅ Coverage improved"
            else if (diff_result%coverage_change < 0.0) then
                print *, "   ⚠️  Coverage decreased"
            else
                print *, "   ➡️  Coverage unchanged"
            end if
        end if
        
    end subroutine output_coverage_diff_summary
    
    subroutine start_tui_interface(config, success)
        !! Starts the terminal user interface
        type(config_t), intent(in) :: config
        logical, intent(out) :: success
        
        ! This would launch the actual TUI
        ! For now, just indicate success
        success = .true.
        
    end subroutine start_tui_interface
    
    subroutine search_gcov_files_in_paths(search_paths, found_files)
        !! Searches for .gcov files in specified search paths
        character(len=*), intent(in) :: search_paths(:)
        character(len=:), allocatable, intent(out) :: found_files(:)
        
        character(len=:), allocatable :: path_files(:)
        character(len=:), allocatable :: all_files(:)
        integer :: i, total_files, current_size
        logical :: path_exists
        
        ! Initialize to empty
        allocate(character(len=256) :: all_files(0))
        total_files = 0
        
        ! Search each path for .gcov files
        do i = 1, size(search_paths)
            ! Check if path exists before searching
            inquire(file=trim(search_paths(i)), exist=path_exists)
            if (path_exists) then
                ! Search for .gcov files in this path
                path_files = find_files(trim(search_paths(i)) // "/*" // GCOV_EXTENSION)
                
                ! Merge with existing files
                if (allocated(path_files) .and. size(path_files) > 0) then
                    current_size = size(all_files)
                    call expand_file_array(all_files, path_files)
                    total_files = total_files + size(path_files)
                end if
            end if
        end do
        
        ! Return the found files
        if (total_files > 0) then
            found_files = all_files
        else
            ! Return empty array
            allocate(character(len=256) :: found_files(0))
        end if
        
    end subroutine search_gcov_files_in_paths
    
    subroutine expand_file_array(all_files, new_files)
        !! Expands file array to include new files
        character(len=:), allocatable, intent(inout) :: all_files(:)
        character(len=*), intent(in) :: new_files(:)
        
        character(len=:), allocatable :: temp_files(:)
        integer :: old_size, new_size, i
        
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
        
    end subroutine expand_file_array
    
    subroutine resize_file_array(files, new_size)
        !! Resizes file array to specified size (truncates if necessary)
        character(len=:), allocatable, intent(inout) :: files(:)
        integer, intent(in) :: new_size
        
        character(len=:), allocatable :: temp_files(:)
        integer :: i, copy_size
        
        if (.not. allocated(files)) return
        if (new_size <= 0) then
            deallocate(files)
            return
        end if
        
        copy_size = min(size(files), new_size)
        allocate(character(len=len(files)) :: temp_files(new_size))
        
        do i = 1, copy_size
            temp_files(i) = files(i)
        end do
        
        call move_alloc(temp_files, files)
        
    end subroutine resize_file_array
    
    function execute_auto_test_workflow(config) result(exit_code)
        !! Execute automatic test workflow with build system detection
        !!
        !! Integrates build system detection with automatic test execution.
        !! Detects the build system, generates appropriate test command with
        !! coverage flags, executes tests with timeout handling, and provides
        !! comprehensive error management.
        !!
        !! Args:
        !!   config: Configuration object with auto_test_execution settings
        !!
        !! Returns:
        !!   exit_code: 0 for success, non-zero for various failure conditions
        !!     - 0: Success or skipped (when auto_test_execution = .false.)
        !!     - 1: Test execution failed
        !!     - 2: Build system detection failed
        !!     - 124: Test execution timed out (standard timeout exit code)
        
        type(config_t), intent(in) :: config
        integer :: exit_code
        
        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx
        integer :: test_exit_code
        logical :: execution_success
        
        exit_code = EXIT_SUCCESS
        
        ! Skip auto-test execution if disabled
        if (.not. config%auto_test_execution) then
            call report_auto_test_disabled(config)
            return
        end if
        
        call report_workflow_start(config)
        
        ! Detect and validate build system
        exit_code = detect_and_validate_build_system(config, build_info, error_ctx)
        if (exit_code /= EXIT_SUCCESS) return
        
        ! Check if build system is usable (tool available and not unknown)
        if (build_info%system_type == 'unknown' .or. &
            .not. build_info%tool_available) then
            ! Already reported by detect_and_validate_build_system, exit gracefully
            return
        end if
        
        ! Execute tests with proper timeout handling
        call execute_tests_with_timeout(build_info%test_command, config, &
                                       test_exit_code, execution_success)
        
        ! Report results and set final exit code
        exit_code = handle_test_execution_results(config, test_exit_code, execution_success)
        
    end function execute_auto_test_workflow
    
    subroutine execute_tests_with_timeout(test_command, config, exit_code, &
                                         success)
        !! Execute test command with timeout handling
        !!
        !! Uses system timeout command to limit test execution time and prevent
        !! hanging tests. Provides secure command execution with proper
        !! argument escaping.
        !!
        !! Args:
        !!   test_command: The test command to execute
        !!   config: Configuration with timeout settings
        !!   exit_code: Exit code from test execution
        !!   success: True if tests passed, false if failed or timed out
        
        character(len=*), intent(in) :: test_command
        type(config_t), intent(in) :: config
        integer, intent(out) :: exit_code
        logical, intent(out) :: success
        
        character(len=1024) :: full_command
        character(len=32) :: timeout_str
        
        success = .false.
        
        ! Build timeout command with proper escaping
        write(timeout_str, '(I0)') config%test_timeout_seconds
        full_command = 'timeout ' // trim(timeout_str) // ' ' // &
                      escape_shell_argument(test_command)
        
        if (.not. config%quiet) then
            print *, "🔧 Executing: " // trim(test_command)
            print *, "⏱️  Timeout: " // trim(timeout_str) // " seconds"
        end if
        
        ! Execute the command with timeout
        call execute_command_line(full_command, exitstat=exit_code)
        
        ! Check results
        if (exit_code == 0) then
            success = .true.
        else if (exit_code == 124) then
            ! Standard timeout exit code
            success = .false.
        else
            ! Test failure or other error
            success = .false.
        end if
        
    end subroutine execute_tests_with_timeout
    
    function format_timeout_message(seconds) result(message)
        !! Format timeout duration for user-friendly display
        integer, intent(in) :: seconds
        character(len=64) :: message
        
        if (seconds < 60) then
            write(message, '(I0,A)') seconds, ' seconds'
        else if (seconds < 3600) then
            write(message, '(I0,A,I0,A)') seconds/60, ' minutes ', &
                                         mod(seconds, 60), ' seconds'
        else
            write(message, '(I0,A,I0,A,I0,A)') seconds/3600, ' hours ', &
                                              mod(seconds/60, 60), ' minutes ', &
                                              mod(seconds, 60), ' seconds'
        end if
    end function format_timeout_message
    
    subroutine report_auto_test_disabled(config)
        !! Report that auto-test execution is disabled
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "ℹ️  Auto-test execution disabled"
        end if
    end subroutine report_auto_test_disabled
    
    subroutine report_workflow_start(config)
        !! Report start of auto test workflow
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "🚀 Executing automatic test workflow..."
        end if
    end subroutine report_workflow_start
    
    function detect_and_validate_build_system(config, build_info, error_ctx) result(exit_code)
        !! Detect build system and validate it's usable
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        integer :: exit_code
        
        exit_code = EXIT_SUCCESS
        
        ! Detect build system
        call detect_build_system('.', build_info, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call report_build_detection_failed(config, error_ctx)
            exit_code = 2  ! Build system detection failed
            return
        end if
        
        ! Handle unknown build system
        if (build_info%system_type == 'unknown') then
            call report_unknown_build_system(config)
            return  ! Skip gracefully
        end if
        
        ! Check if build tool is available
        if (.not. build_info%tool_available) then
            call report_build_tool_unavailable(config, build_info)
            return  ! Skip gracefully
        end if
        
        call report_build_system_detected(config, build_info)
        
    end function detect_and_validate_build_system
    
    subroutine report_build_detection_failed(config, error_ctx)
        !! Report build system detection failure
        type(config_t), intent(in) :: config
        type(error_context_t), intent(in) :: error_ctx
        
        if (.not. config%quiet) then
            print *, "❌ Build system detection failed: " // &
                     trim(error_ctx%message)
        end if
    end subroutine report_build_detection_failed
    
    subroutine report_unknown_build_system(config)
        !! Report unknown build system detected
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "⚠️  No known build system detected, skipping tests"
            print *, "   Supported: FPM, CMake, Make, Meson"
        end if
    end subroutine report_unknown_build_system
    
    subroutine report_build_tool_unavailable(config, build_info)
        !! Report build tool not available
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "⚠️  Build tool not available for " // &
                     trim(build_info%system_type) // ", skipping tests"
        end if
    end subroutine report_build_tool_unavailable
    
    subroutine report_build_system_detected(config, build_info)
        !! Report successful build system detection
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "📦 Build system detected: " // trim(build_info%system_type)
        end if
    end subroutine report_build_system_detected
    
    function handle_test_execution_results(config, test_exit_code, execution_success) result(exit_code)
        !! Handle test execution results and report outcome
        type(config_t), intent(in) :: config
        integer, intent(in) :: test_exit_code
        logical, intent(in) :: execution_success
        integer :: exit_code
        
        if (.not. execution_success) then
            exit_code = test_exit_code
            call report_test_failure(config, test_exit_code)
        else
            exit_code = EXIT_SUCCESS
            call report_test_success(config)
        end if
    end function handle_test_execution_results
    
    subroutine report_test_failure(config, test_exit_code)
        !! Report test execution failure
        type(config_t), intent(in) :: config
        integer, intent(in) :: test_exit_code
        
        if (.not. config%quiet) then
            if (test_exit_code == 124) then
                print *, "⏱️  Test execution timed out after " // &
                         format_timeout_message(config%test_timeout_seconds)
            else
                print *, "❌ Test execution failed with exit code: " // &
                         format_integer(test_exit_code)
            end if
        end if
    end subroutine report_test_failure
    
    subroutine report_test_success(config)
        !! Report test execution success
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "✅ Tests completed successfully"
        end if
    end subroutine report_test_success
    
    subroutine determine_coverage_files_source(config, files)
        !! Determine source of coverage files based on configuration
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: files(:)
        
        if (allocated(config%coverage_files)) then
            ! Use explicitly specified coverage files
            files = config%coverage_files
        else if (config%zero_configuration_mode) then
            ! Use zero-configuration auto-discovery
            files = auto_discover_coverage_files_priority()
        else
            ! Discover coverage files in source paths
            call discover_gcov_files(config, files)
        end if
    end subroutine determine_coverage_files_source
    
    subroutine apply_coverage_file_filtering(all_files, config, filtered_files)
        !! Apply filtering to coverage files if needed
        character(len=:), allocatable, intent(in) :: all_files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: filtered_files(:)
        
        ! Filter files by exclude patterns
        if (allocated(all_files)) then
            ! Bypass filtering for empty arrays to avoid allocation issues
            if (size(all_files) == 0) then
                allocate(character(len=256) :: filtered_files(0))
            else
                filtered_files = filter_coverage_files_by_patterns(all_files, config)
            end if
        end if
    end subroutine apply_coverage_file_filtering
    
    subroutine validate_and_limit_files(input_files, config, output_files)
        !! Validate file existence and apply max_files limit
        character(len=:), allocatable, intent(in) :: input_files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: output_files(:)
        
        integer :: i, file_count
        logical :: file_exists
        
        if (.not. allocated(input_files)) then
            return
        end if
        
        output_files = input_files
        
        ! Count existing files
        file_count = 0
        do i = 1, size(output_files)
            inquire(file=trim(output_files(i)), exist=file_exists)
            if (file_exists) then
                file_count = file_count + 1
            end if
        end do
        
        if (file_count == 0) then
            deallocate(output_files)
        else if (file_count > config%max_files) then
            call report_file_count_limit(config, file_count)
            call resize_file_array(output_files, config%max_files)
        end if
    end subroutine validate_and_limit_files
    
    subroutine report_file_count_limit(config, file_count)
        !! Report file count limitation
        type(config_t), intent(in) :: config
        integer, intent(in) :: file_count
        
        if (.not. config%quiet) then
            print *, "⚠️  Limiting coverage files from", file_count, "to", config%max_files
        end if
    end subroutine report_file_count_limit
    
    subroutine prepare_pattern_paths(filepath, normalized_path, basename)
        !! Prepare normalized path and basename for pattern matching
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: normalized_path, basename
        
        integer :: last_slash
        
        ! Normalize path for consistent matching
        normalized_path = normalize_path(filepath)
        
        ! Extract basename for pattern matching
        last_slash = index(normalized_path, '/', back=.true.)
        if (last_slash > 0) then
            basename = normalized_path(last_slash+1:)
        else
            basename = normalized_path
        end if
    end subroutine prepare_pattern_paths
    
    subroutine check_include_patterns(normalized_path, basename, config, matches_include)
        !! Check if file matches any include patterns
        character(len=*), intent(in) :: normalized_path, basename
        type(config_t), intent(in) :: config
        logical, intent(out) :: matches_include
        
        integer :: i
        
        matches_include = .true.  ! Default to include if no include patterns
        
        ! Check against include patterns first - if specified, file must match at least one
        if (allocated(config%include_patterns)) then
            matches_include = .false.  ! Now require explicit match
            do i = 1, size(config%include_patterns)
                ! Check both full path and basename for patterns
                if (matches_pattern(normalized_path, config%include_patterns(i)) .or. &
                    matches_pattern(basename, config%include_patterns(i))) then
                    matches_include = .true.
                    exit  ! Found a match, no need to continue
                end if
            end do
        end if
    end subroutine check_include_patterns
    
    subroutine check_exclude_patterns(normalized_path, basename, config, should_exclude)
        !! Check if file matches any exclude patterns
        character(len=*), intent(in) :: normalized_path, basename
        type(config_t), intent(in) :: config
        logical, intent(out) :: should_exclude
        
        integer :: i
        
        should_exclude = .false.
        
        ! Check against exclude patterns
        if (allocated(config%exclude_patterns)) then
            do i = 1, size(config%exclude_patterns)
                ! Check both full path and basename for patterns
                if (matches_pattern(normalized_path, config%exclude_patterns(i)) .or. &
                    matches_pattern(basename, config%exclude_patterns(i))) then
                    should_exclude = .true.
                    return
                end if
            end do
        end if
    end subroutine check_exclude_patterns
    
    subroutine check_test_file_exclusion(normalized_path, config, should_exclude)
        !! Check for test file exclusion using patterns
        character(len=*), intent(in) :: normalized_path
        type(config_t), intent(in) :: config
        logical, intent(out) :: should_exclude
        
        integer :: i
        
        should_exclude = .false.
        
        ! Check for test file exclusion using patterns
        if (allocated(config%exclude_patterns)) then
            if (is_test_file(normalized_path)) then
                ! Check if test files match any exclude pattern
                do i = 1, size(config%exclude_patterns)
                    if (index(normalized_path, trim(config%exclude_patterns(i))) > 0) then
                        should_exclude = .true.
                        return
                    end if
                end do
            end if
        end if
    end subroutine check_test_file_exclusion
    
    subroutine determine_gcov_search_paths(config, search_paths, found_files)
        !! Determine search paths for gcov files based on configuration
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: search_paths(:)
        character(len=:), allocatable, intent(out) :: found_files(:)
        
        character(len=256), parameter :: GCOV_OUTPUT_DIR = "build/gcov"
        logical :: dir_exists
        
        ! If explicit source paths are provided, respect them exclusively
        if (allocated(config%source_paths)) then
            search_paths = config%source_paths
        else
            ! Only check build/gcov directory when no explicit source paths provided
            inquire(file=GCOV_OUTPUT_DIR, exist=dir_exists)
            if (dir_exists) then
                found_files = find_files(GCOV_OUTPUT_DIR // "/*" // GCOV_EXTENSION)
            end if
            
            ! If no .gcov files found in build/gcov, default to current directory
            if (.not. allocated(found_files) .or. size(found_files) == 0) then
                allocate(character(len=1) :: search_paths(1))
                search_paths(1) = "."
            end if
        end if
    end subroutine determine_gcov_search_paths
    
    subroutine search_existing_gcov_files(search_paths, found_files)
        !! Search for existing .gcov files in configured search paths
        character(len=:), allocatable, intent(in) :: search_paths(:)
        character(len=:), allocatable, intent(inout) :: found_files(:)
        
        ! Search for existing .gcov files in configured search paths
        if ((.not. allocated(found_files) .or. size(found_files) == 0) .and. &
            allocated(search_paths)) then
            call search_gcov_files_in_paths(search_paths, found_files)
        end if
    end subroutine search_existing_gcov_files
    
    subroutine attempt_gcov_generation(config, found_files, generated_files)
        !! Attempt automatic generation only in auto-discovery mode
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(in) :: found_files(:)
        character(len=:), allocatable, intent(out) :: generated_files(:)
        
        ! Attempt automatic generation only in auto-discovery mode
        ! When explicit source paths are specified, respect user intent
        if ((.not. allocated(found_files) .or. size(found_files) == 0) .and. &
            .not. allocated(config%source_paths)) then
            call auto_generate_gcov_files(config, generated_files)
        end if
    end subroutine attempt_gcov_generation
    
    subroutine finalize_gcov_file_result(found_files, generated_files, files)
        !! Set final result for gcov file discovery
        character(len=:), allocatable, intent(in) :: found_files(:)
        character(len=:), allocatable, intent(in) :: generated_files(:)
        character(len=:), allocatable, intent(out) :: files(:)
        
        if (allocated(generated_files)) then
            files = generated_files
        else if (allocated(found_files)) then
            files = found_files
        else
            ! CRITICAL FIX: Ensure files is always allocated to prevent memory issues
            ! If no files found and no files generated, allocate empty array
            allocate(character(len=256) :: files(0))
        end if
    end subroutine finalize_gcov_file_result
    
    subroutine generate_gcov_from_build_dirs(config, build_dirs, success)
        !! Generate gcov files from build directories
        type(config_t), intent(in) :: config
        character(len=*), intent(in) :: build_dirs(:)
        logical, intent(out) :: success
        
        character(len=300) :: command
        character(len=1000) :: build_path
        character(len=:), allocatable :: gcov_exe
        integer :: i, exit_status
        
        success = .false.
        
        ! Use custom gcov executable if specified, otherwise default to 'gcov'
        if (allocated(config%gcov_executable)) then
            gcov_exe = trim(config%gcov_executable)
        else
            gcov_exe = "gcov"
        end if
        
        ! Process each build directory
        do i = 1, size(build_dirs)
            build_path = trim(build_dirs(i))
            
            ! Generate gcov files for this directory (check if .gcno files exist)
            write(command, '(A)') 'find ' // escape_shell_argument(trim(build_path)) // ' -name "src_*.f90.gcno" -execdir ' // &
                                  escape_shell_argument(trim(gcov_exe)) // ' {} \; 2>/dev/null'
            call execute_command_line(command, exitstat=exit_status)
            
            if (exit_status == 0) then
                success = .true.
            end if
        end do
    end subroutine generate_gcov_from_build_dirs
    
end module coverage_workflows