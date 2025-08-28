module report_engine_filters
    !! Report Engine Data Filtering
    !! 
    !! Handles data filtering functionality for coverage reports.
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use coverage_data_filter
    implicit none
    private
    
    public :: apply_coverage_filtering
    public :: should_include_file

contains

    ! Apply coverage filtering
    subroutine apply_coverage_filtering(input_data, criteria, filtered_data, &
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
    end subroutine apply_coverage_filtering
    
    ! Check if file should be included based on filter criteria
    subroutine should_include_file(file_data, criteria, include_file)
        type(coverage_file_t), intent(in) :: file_data
        type(filter_criteria_t), intent(in) :: criteria
        logical, intent(out) :: include_file
        
        real(kind=real64) :: file_coverage
        integer :: i, covered_lines, total_lines
        logical :: matches_pattern
        
        include_file = .true.
        
        ! Calculate file coverage percentage
        if (allocated(file_data%lines)) then
            total_lines = size(file_data%lines)
            covered_lines = count(file_data%lines > 0)
            
            if (total_lines > 0) then
                file_coverage = real(covered_lines, kind=real64) / &
                               real(total_lines, kind=real64) * 100.0_real64
            else
                file_coverage = 0.0_real64
            end if
        else
            file_coverage = 0.0_real64
        end if
        
        ! Check coverage threshold
        if (file_coverage < criteria%min_coverage_threshold) then
            if (.not. criteria%show_only_uncovered) then
                include_file = .false.
                return
            end if
        end if
        
        ! Check show_only_uncovered filter
        if (criteria%show_only_uncovered .and. file_coverage >= 100.0_real64) then
            include_file = .false.
            return
        end if
        
        ! Check include patterns
        if (allocated(criteria%include_patterns) .and. &
            size(criteria%include_patterns) > 0) then
            matches_pattern = .false.
            do i = 1, size(criteria%include_patterns)
                if (index(file_data%filename, trim(criteria%include_patterns(i))) > 0) then
                    matches_pattern = .true.
                    exit
                end if
            end do
            if (.not. matches_pattern) then
                include_file = .false.
                return
            end if
        end if
        
        ! Check exclude patterns
        if (allocated(criteria%exclude_patterns)) then
            do i = 1, size(criteria%exclude_patterns)
                if (index(file_data%filename, trim(criteria%exclude_patterns(i))) > 0) then
                    include_file = .false.
                    return
                end if
            end do
        end if
        
    end subroutine should_include_file

end module report_engine_filters