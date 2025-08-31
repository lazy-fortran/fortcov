module coverage_data_filter
    !! Coverage Data Filtering
    !!
    !! Handles filtering of coverage data based on criteria.
    !! Extracted from report_engine_impl.f90 for SRP compliance.
    use coverage_model_core
    use report_config_core
    use string_utils, only: matches_pattern
    implicit none
    private

    ! Public procedures
    public :: should_include_file
    public :: apply_filter_criteria

contains

    ! Determine if a file should be included based on filter criteria
    subroutine should_include_file(file, criteria, include)
        type(coverage_file_t), intent(in) :: file
        type(filter_criteria_t), intent(in) :: criteria
        logical, intent(out) :: include

        real :: file_coverage
        integer :: i
        logical :: matches_include, matches_exclude

        include = .true.

        ! Check coverage threshold
        file_coverage = file%get_line_coverage()
        if (file_coverage < criteria%min_coverage_threshold) then
            include = .false.
            return
        end if

        ! Check include patterns using proper pattern matching
        ! Issue #886 fix: Use matches_pattern for consistent wildcard support
        if (allocated(criteria%include_patterns) .and. &
            size(criteria%include_patterns) > 0) then
            matches_include = .false.
            do i = 1, size(criteria%include_patterns)
                if (matches_pattern(file%filename, criteria%include_patterns(i))) then
                    matches_include = .true.
                    exit
                end if
            end do
            if (.not. matches_include) then
                include = .false.
                return
            end if
        end if

        ! Check exclude patterns using proper pattern matching
        if (allocated(criteria%exclude_patterns) .and. &
            size(criteria%exclude_patterns) > 0) then
            matches_exclude = .false.
            do i = 1, size(criteria%exclude_patterns)
                if (matches_pattern(file%filename, criteria%exclude_patterns(i))) then
                    matches_exclude = .true.
                    exit
                end if
            end do
            if (matches_exclude) then
                include = .false.
                return
            end if
        end if
    end subroutine should_include_file
    
    ! Apply filter criteria to coverage data
    subroutine apply_filter_criteria(input_data, criteria, filtered_data, &
                                    success, error_msg)
        type(coverage_data_t), intent(in) :: input_data
        type(filter_criteria_t), intent(in) :: criteria
        type(coverage_data_t), intent(out) :: filtered_data
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(coverage_file_t), allocatable :: temp_files(:)
        integer :: i, filtered_count
        logical :: include_file
        
        success = .false.
        error_msg = ""
        
        if (.not. allocated(input_data%files)) then
            error_msg = "No input data to filter"
            return
        end if
        
        ! First pass: count files that pass filter
        filtered_count = 0
        do i = 1, size(input_data%files)
            call should_include_file(input_data%files(i), criteria, include_file)
            if (include_file) filtered_count = filtered_count + 1
        end do
        
        if (filtered_count == 0) then
            call filtered_data%init()
            success = .true.
            return
        end if
        
        ! Second pass: collect filtered files
        allocate(temp_files(filtered_count))
        filtered_count = 0
        
        do i = 1, size(input_data%files)
            call should_include_file(input_data%files(i), criteria, include_file)
            if (include_file) then
                filtered_count = filtered_count + 1
                temp_files(filtered_count) = input_data%files(i)
            end if
        end do
        
        call filtered_data%init()
        filtered_data%files = temp_files
        success = .true.
    end subroutine apply_filter_criteria

end module coverage_data_filter