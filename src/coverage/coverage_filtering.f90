module coverage_filtering
    !! Coverage data filtering and searching operations
    !! 
    !! Extracted from coverage_operations_core for SRP compliance.
    !! Focused solely on filtering coverage data and finding specific elements.
    use constants_core
    use foundation_utils
    use coverage_data_core
    use coverage_data_utils
    use coverage_calculations, only: calculate_overall_coverage
    implicit none
    private
    
    public :: filter_coverage_by_threshold
    public :: find_uncovered_lines
    
contains
    
    subroutine filter_coverage_by_threshold(coverage_data, threshold, filtered_data)
        !! Filters coverage data by threshold
        type(coverage_data_t), intent(in) :: coverage_data
        real, intent(in) :: threshold
        type(coverage_data_t), intent(out) :: filtered_data
        
        type(coverage_file_t), allocatable :: temp_files(:)
        integer :: i, filtered_count
        
        if (.not. allocated(coverage_data%files)) return
        
        allocate(temp_files(size(coverage_data%files)))
        filtered_count = 0
        
        do i = 1, size(coverage_data%files)
            if (coverage_data%files(i)%line_coverage < threshold) then
                filtered_count = filtered_count + 1
                temp_files(filtered_count) = coverage_data%files(i)
            end if
        end do
        
        if (filtered_count > 0) then
            call allocate_files_array(filtered_data, filtered_count)
            filtered_data%files(1:filtered_count) = temp_files(1:filtered_count)
        end if
        
        ! Delegate coverage calculation to specialized module
        call calculate_overall_coverage(filtered_data)
        
    end subroutine filter_coverage_by_threshold
    
    subroutine find_uncovered_lines(coverage_data, uncovered_lines)
        !! Finds all uncovered lines in the coverage data
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_line_t), allocatable, intent(out) :: uncovered_lines(:)
        
        type(coverage_line_t), allocatable :: temp_lines(:)
        integer :: i, j, total_count, uncovered_count
        
        if (.not. allocated(coverage_data%files)) return
        
        ! Count total uncovered lines
        total_count = 0
        do i = 1, size(coverage_data%files)
            if (allocated(coverage_data%files(i)%lines)) then
                do j = 1, size(coverage_data%files(i)%lines)
                    if (coverage_data%files(i)%lines(j)%is_executable .and. &
                        .not. coverage_data%files(i)%lines(j)%is_covered()) then
                        total_count = total_count + 1
                    end if
                end do
            end if
        end do
        
        if (total_count == 0) return
        
        allocate(temp_lines(total_count))
        uncovered_count = 0
        
        ! Collect uncovered lines
        do i = 1, size(coverage_data%files)
            if (allocated(coverage_data%files(i)%lines)) then
                do j = 1, size(coverage_data%files(i)%lines)
                    if (coverage_data%files(i)%lines(j)%is_executable .and. &
                        .not. coverage_data%files(i)%lines(j)%is_covered()) then
                        uncovered_count = uncovered_count + 1
                        temp_lines(uncovered_count) = coverage_data%files(i)%lines(j)
                    end if
                end do
            end if
        end do
        
        allocate(uncovered_lines(uncovered_count))
        uncovered_lines(1:uncovered_count) = temp_lines(1:uncovered_count)
        
    end subroutine find_uncovered_lines
    
end module coverage_filtering