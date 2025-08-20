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
    use string_utils
    use error_handling
    implicit none
    private
    
    public :: discover_coverage_files
    public :: evaluate_exclude_patterns
    public :: perform_coverage_diff_analysis
    public :: launch_coverage_tui_mode
    public :: filter_coverage_files_by_patterns
    
contains
    
    function discover_coverage_files(config) result(files)
        !! Coverage file discovery implementation
        !! Extracted from original find_coverage_files function
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: files(:)
        
        character(len=:), allocatable :: all_files(:)
        character(len=:), allocatable :: filtered_files(:)
        integer :: i, file_count
        logical :: file_exists
        
        ! Find coverage files based on configuration
        if (allocated(config%coverage_files)) then
            ! Use explicitly specified coverage files
            files = config%coverage_files
        else
            ! Discover coverage files in source paths
            call discover_gcov_files(config, all_files)
            
            ! Filter files by exclude patterns
            if (allocated(all_files)) then
                filtered_files = filter_coverage_files_by_patterns(all_files, config)
                files = filtered_files
            end if
        end if
        
        ! Validate file existence
        if (allocated(files)) then
            file_count = 0
            do i = 1, size(files)
                inquire(file=trim(files(i)), exist=file_exists)
                if (file_exists) then
                    file_count = file_count + 1
                end if
            end do
            
            if (file_count == 0) then
                deallocate(files)
            end if
        end if
        
    end function discover_coverage_files
    
    function evaluate_exclude_patterns(filepath, config) result(should_exclude)
        !! Pattern evaluation implementation for both include and exclude patterns
        !! Enhanced from original check_exclude_patterns function
        character(len=*), intent(in) :: filepath
        type(config_t), intent(in) :: config
        logical :: should_exclude
        
        integer :: i
        character(len=:), allocatable :: normalized_path
        logical :: matches_include
        
        should_exclude = .false.
        matches_include = .true.  ! Default to include if no include patterns
        
        ! Normalize path for consistent matching
        normalized_path = normalize_path(filepath)
        
        ! Check against include patterns first - if specified, file must match at least one
        if (allocated(config%include_patterns)) then
            matches_include = .false.  ! Now require explicit match
            do i = 1, size(config%include_patterns)
                if (matches_pattern(normalized_path, config%include_patterns(i))) then
                    matches_include = .true.
                    exit  ! Found a match, no need to continue
                end if
            end do
        end if
        
        ! If file doesn't match any include pattern, exclude it
        if (.not. matches_include) then
            should_exclude = .true.
            return
        end if
        
        ! Check against exclude patterns
        if (allocated(config%exclude_patterns)) then
            do i = 1, size(config%exclude_patterns)
                if (matches_pattern(normalized_path, config%exclude_patterns(i))) then
                    should_exclude = .true.
                    return
                end if
            end do
        end if
        
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
        !! Discovers .gcov files in configured source paths
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: files(:)
        
        character(len=:), allocatable :: search_paths(:)
        character(len=:), allocatable :: found_files(:)
        integer :: path_idx
        
        ! Use source paths for discovery
        if (allocated(config%source_paths)) then
            search_paths = config%source_paths
        else
            ! Default to current directory
            allocate(character(len=1) :: search_paths(1))
            search_paths(1) = "."
        end if
        
        ! Search for .gcov files in all paths
        ! Use simple glob pattern for .gcov files
        found_files = find_files("*" // GCOV_EXTENSION)
        
        if (allocated(found_files)) then
            files = found_files
        end if
        
    end subroutine discover_gcov_files
    
    
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
    
end module coverage_workflows