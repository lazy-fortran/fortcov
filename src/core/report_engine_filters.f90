module report_engine_filters
    !! Report Engine Data Filtering
    !! 
    !! Handles data filtering functionality for coverage reports.
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use coverage_data_filter
    use report_config_core
    implicit none
    private
    
    public :: apply_coverage_filtering

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

end module report_engine_filters