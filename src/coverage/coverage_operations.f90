module coverage_operations
    !! Coverage Operations - Consolidated Module
    !! 
    !! Consolidated module combining coverage calculations, filtering, and comparison
    !! operations. Originally split across coverage_calculations.f90, coverage_filtering.f90,
    !! and coverage_comparison.f90 for excessive SRP compliance.
    !!
    !! Consolidation benefits:
    !! - Reduces 3 modules to 1 module (cognitive overhead reduction)  
    !! - Maintains all functionality with clear sectioning
    !! - Eliminates unnecessary module boundaries
    !! - Preserves complete backward compatibility
    use constants_core
    use foundation_utils
    use coverage_data_core
    use coverage_data_utils
    use coverage_stats_core, only: extended_coverage_stats_t
    use coverage_merging, only: merge_coverage_data
    implicit none
    private
    
    ! Coverage calculations (from coverage_calculations.f90)
    public :: calculate_coverage_statistics
    public :: calculate_file_coverage
    public :: calculate_overall_coverage
    
    ! Coverage filtering (from coverage_filtering.f90)
    public :: filter_coverage_by_threshold
    public :: find_uncovered_lines
    
    ! Coverage comparison (from coverage_comparison.f90)
    public :: compare_coverage_data
    
    ! Re-exported merging operations (from coverage_merging.f90)
    public :: merge_coverage_data
    
contains
    
    ! ========================================================================
    ! Coverage Calculations Section (from coverage_calculations.f90)
    ! ========================================================================
    
    subroutine calculate_coverage_statistics(coverage_data, stats)
        !! Calculates comprehensive coverage statistics
        type(coverage_data_t), intent(in) :: coverage_data
        type(extended_coverage_stats_t), intent(out) :: stats
        
        integer :: i, j
        
        ! Initialize statistics
        stats%total_lines = 0
        stats%covered_lines = 0
        stats%total_branches = 0
        stats%covered_branches = 0
        stats%total_functions = 0
        stats%covered_functions = 0
        stats%total_files = 0
        stats%covered_files = 0
        
        if (.not. allocated(coverage_data%files)) return
        
        stats%total_files = size(coverage_data%files)
        
        ! Calculate statistics for each file
        do i = 1, size(coverage_data%files)
            associate(file => coverage_data%files(i))
                call calculate_file_statistics(file, stats)
                
                ! Count covered files
                if (file%line_coverage > 0.0) then
                    stats%covered_files = stats%covered_files + 1
                end if
            end associate
        end do
        
        ! Calculate overall percentages
        if (stats%total_lines > 0) then
            stats%line_coverage = real(stats%covered_lines) / real(stats%total_lines) * 100.0
        else
            stats%line_coverage = 0.0
        end if
        
        if (stats%total_branches > 0) then
            stats%branch_coverage = real(stats%covered_branches) / real(stats%total_branches) * 100.0
        else
            stats%branch_coverage = 0.0
        end if
        
        if (stats%total_functions > 0) then
            stats%function_coverage = real(stats%covered_functions) / real(stats%total_functions) * 100.0
        else
            stats%function_coverage = 0.0
        end if
        
    end subroutine calculate_coverage_statistics
    
    subroutine calculate_file_statistics(file, stats)
        !! Helper to calculate statistics for a single file
        type(coverage_file_t), intent(in) :: file
        type(extended_coverage_stats_t), intent(inout) :: stats
        
        integer :: i
        
        if (.not. allocated(file%lines)) return
        
        ! Count lines
        do i = 1, size(file%lines)
            associate(line => file%lines(i))
                if (line%is_executable) then
                    stats%total_lines = stats%total_lines + 1
                    if (line%execution_count > 0) then
                        stats%covered_lines = stats%covered_lines + 1
                    end if
                end if
            end associate
        end do
        
        ! Count functions (if available)
        if (allocated(file%functions)) then
            do i = 1, size(file%functions)
                stats%total_functions = stats%total_functions + 1
                if (file%functions(i)%execution_count > 0) then
                    stats%covered_functions = stats%covered_functions + 1
                end if
            end do
        end if
        
        ! Count branches (if available) 
        if (allocated(file%branches)) then
            do i = 1, size(file%branches)
                stats%total_branches = stats%total_branches + 1
                if (file%branches(i)%taken_count > 0) then
                    stats%covered_branches = stats%covered_branches + 1
                end if
            end do
        end if
        
    end subroutine calculate_file_statistics
    
    real function calculate_file_coverage(file) result(coverage)
        !! Calculates line coverage for a single file
        type(coverage_file_t), intent(in) :: file
        
        integer :: total_lines, covered_lines, i
        
        coverage = 0.0
        total_lines = 0
        covered_lines = 0
        
        if (.not. allocated(file%lines)) return
        
        ! Count executable and covered lines
        do i = 1, size(file%lines)
            if (file%lines(i)%is_executable) then
                total_lines = total_lines + 1
                if (file%lines(i)%execution_count > 0) then
                    covered_lines = covered_lines + 1
                end if
            end if
        end do
        
        if (total_lines > 0) then
            coverage = real(covered_lines) / real(total_lines) * 100.0
        end if
        
    end function calculate_file_coverage
    
    real function calculate_overall_coverage(coverage_data) result(coverage)
        !! Calculates overall coverage percentage
        type(coverage_data_t), intent(in) :: coverage_data
        
        integer :: total_lines, covered_lines, i
        
        coverage = 0.0
        total_lines = 0
        covered_lines = 0
        
        if (.not. allocated(coverage_data%files)) return
        
        ! Sum all executable and covered lines
        do i = 1, size(coverage_data%files)
            call count_file_lines(coverage_data%files(i), total_lines, covered_lines)
        end do
        
        if (total_lines > 0) then
            coverage = real(covered_lines) / real(total_lines) * 100.0
        end if
        
    end function calculate_overall_coverage
    
    subroutine count_file_lines(file, total_lines, covered_lines)
        !! Helper to count lines in a file
        type(coverage_file_t), intent(in) :: file
        integer, intent(inout) :: total_lines, covered_lines
        
        integer :: i
        
        if (.not. allocated(file%lines)) return
        
        do i = 1, size(file%lines)
            if (file%lines(i)%is_executable) then
                total_lines = total_lines + 1
                if (file%lines(i)%execution_count > 0) then
                    covered_lines = covered_lines + 1
                end if
            end if
        end do
        
    end subroutine count_file_lines
    
    ! ========================================================================
    ! Coverage Filtering Section (from coverage_filtering.f90)
    ! ========================================================================
    
    subroutine filter_coverage_by_threshold(coverage_data, threshold, filtered_data)
        !! Filters coverage data by threshold
        type(coverage_data_t), intent(in) :: coverage_data
        real, intent(in) :: threshold
        type(coverage_data_t), intent(out) :: filtered_data
        
        integer :: i, count
        logical, allocatable :: keep_file(:)
        
        if (.not. allocated(coverage_data%files)) return
        
        ! Determine which files to keep
        allocate(keep_file(size(coverage_data%files)))
        count = 0
        
        do i = 1, size(coverage_data%files)
            keep_file(i) = (coverage_data%files(i)%line_coverage < threshold)
            if (keep_file(i)) count = count + 1
        end do
        
        ! Create filtered dataset
        if (count > 0) then
            allocate(filtered_data%files(count))
            count = 0
            do i = 1, size(coverage_data%files)
                if (keep_file(i)) then
                    count = count + 1
                    filtered_data%files(count) = coverage_data%files(i)
                end if
            end do
        end if
        
        ! Update metadata
        filtered_data%overall_coverage = calculate_overall_coverage(filtered_data)
        
    end subroutine filter_coverage_by_threshold
    
    subroutine find_uncovered_lines(coverage_data, uncovered_lines)
        !! Finds all uncovered lines in coverage data (copied from coverage_filtering.f90)
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
                        coverage_data%files(i)%lines(j)%execution_count == 0) then
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
                        coverage_data%files(i)%lines(j)%execution_count == 0) then
                        uncovered_count = uncovered_count + 1
                        temp_lines(uncovered_count) = coverage_data%files(i)%lines(j)
                    end if
                end do
            end if
        end do
        
        allocate(uncovered_lines(uncovered_count))
        uncovered_lines(1:uncovered_count) = temp_lines(1:uncovered_count)
        
    end subroutine find_uncovered_lines
    
    ! ========================================================================
    ! Coverage Comparison Section (from coverage_comparison.f90)
    ! ========================================================================
    
    subroutine compare_coverage_data(baseline, current, diff_result)
        !! Compares two coverage datasets and produces diff
        type(coverage_data_t), intent(in) :: baseline, current
        type(coverage_diff_t), intent(out) :: diff_result
        
        ! Initialize diff result
        diff_result%baseline_coverage = baseline%overall_coverage
        diff_result%current_coverage = current%overall_coverage
        diff_result%coverage_change = current%overall_coverage - baseline%overall_coverage
        
        ! Compare files and generate file diffs
        call generate_file_diffs(baseline, current, diff_result)
        
    end subroutine compare_coverage_data
    
    subroutine generate_file_diffs(baseline, current, diff_result)
        !! Generates file-level diffs between datasets
        type(coverage_data_t), intent(in) :: baseline, current
        type(coverage_diff_t), intent(inout) :: diff_result
        
        ! Implementation would generate detailed file diffs
        ! For now, just initialize
        diff_result%added_lines = 0
        diff_result%removed_lines = 0
        diff_result%modified_lines = 0
        
    end subroutine generate_file_diffs

end module coverage_operations