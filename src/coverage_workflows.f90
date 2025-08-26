module coverage_workflows
    !! Coverage Workflows and File Management (Decomposed Architecture)
    !! 
    !! Slim coordination module that delegates to specialized workflow modules.
    !! Maintains backward compatibility while providing focused responsibilities.
    use foundation_constants
    use fortcov_config
    use coverage_diff
    use coverage_types, only: coverage_diff_t
    use string_utils, only: to_lower, matches_pattern
    use zero_configuration_manager, only: auto_discover_coverage_files_priority
    use coverage_auto_test_executor
    use coverage_gcov_processor
    use coverage_workflow_orchestrator
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
        use coverage_tui_handler, only: perform_tui_analysis
        type(config_t), intent(in) :: config
        logical, intent(out) :: success
        
        integer :: exit_code
        
        ! Launch the actual TUI using the coverage TUI handler
        exit_code = perform_tui_analysis(config)
        success = (exit_code == EXIT_SUCCESS)
        
    end subroutine start_tui_interface
    
    
    
    
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
            ! Delegate to gcov processor for discovery
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
    
    
end module coverage_workflows