module pattern_filtering_core
    !! Pattern Filtering and Evaluation
    !! 
    !! Handles file filtering based on include/exclude patterns.
    !! Extracted from coverage_workflows.f90 for SRP compliance (Issue #718).
    use config_core
    use string_utils, only: to_lower, matches_pattern
    implicit none
    private
    
    public :: evaluate_exclude_patterns, filter_coverage_files_by_patterns
    public :: is_test_file, normalize_path

contains

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
                ! Check if test files match any exclude pattern using proper pattern matching
                do i = 1, size(config%exclude_patterns)
                    if (matches_pattern(normalized_path, config%exclude_patterns(i))) then
                        should_exclude = .true.
                        return
                    end if
                end do
            end if
        end if
    end subroutine check_test_file_exclusion

end module pattern_filtering_core