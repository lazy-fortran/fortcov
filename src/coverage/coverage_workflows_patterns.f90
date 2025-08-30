module coverage_workflows_patterns
    !! Pattern matching and validation utilities for coverage workflows
    !!
    !! Provides comprehensive pattern matching capabilities for include/exclude
    !! patterns, test file detection, and path-based filtering logic.
    
    use config_core
    use string_utils, only: matches_pattern
    use coverage_workflows_utils, only: normalize_path, is_test_file
    implicit none
    private
    
    public :: prepare_pattern_paths
    public :: check_include_patterns
    public :: check_exclude_patterns
    public :: check_test_file_exclusion
    
contains

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

end module coverage_workflows_patterns