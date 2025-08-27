module coverage_data_filter_impl
    !! Coverage Data Filtering
    !!
    !! Handles filtering of coverage data based on criteria.
    !! Extracted from report_engine_impl.f90 for SRP compliance.
    use coverage_model_core
    use report_config_core
    implicit none
    private

    ! Public procedures
    public :: should_include_file

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

        ! Check include patterns
        if (allocated(criteria%include_patterns) .and. &
            size(criteria%include_patterns) > 0) then
            matches_include = .false.
            do i = 1, size(criteria%include_patterns)
                if (index(file%filename, trim(criteria%include_patterns(i))) > 0) then
                    matches_include = .true.
                    exit
                end if
            end do
            if (.not. matches_include) then
                include = .false.
                return
            end if
        end if

        ! Check exclude patterns
        if (allocated(criteria%exclude_patterns) .and. &
            size(criteria%exclude_patterns) > 0) then
            matches_exclude = .false.
            do i = 1, size(criteria%exclude_patterns)
                if (index(file%filename, trim(criteria%exclude_patterns(i))) > 0) then
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

end module coverage_data_filter_impl