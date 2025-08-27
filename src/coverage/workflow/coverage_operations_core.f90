module coverage_operations_core
    !! Coverage Operations (Decomposed from coverage_model.f90)
    !! 
    !! Focused on coverage calculations, merging, and data operations.
    !! Separated from data model definitions for better separation of concerns.
    use constants_core
    use foundation_utils
    use coverage_data_core
    use coverage_stats_core, only: extended_coverage_stats_t
    implicit none
    private
    
    public :: calculate_coverage_statistics
    public :: merge_coverage_data
    public :: merge_coverage_files
    public :: merge_coverage_lines
    public :: calculate_file_coverage
    public :: calculate_overall_coverage
    public :: filter_coverage_by_threshold
    public :: find_uncovered_lines
    public :: compare_coverage_data
    
contains
    
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
        
        ! Calculate percentages
        if (stats%total_lines > 0) then
            stats%line_coverage = real(stats%covered_lines) / real(stats%total_lines) * 100.0
        end if
        
        if (stats%total_branches > 0) then
            stats%branch_coverage = real(stats%covered_branches) / real(stats%total_branches) * 100.0
        end if
        
        if (stats%total_functions > 0) then
            stats%function_coverage = real(stats%covered_functions) / real(stats%total_functions) * 100.0
        end if
        
    end subroutine calculate_coverage_statistics
    
    subroutine merge_coverage_data(data1, data2, merged_data)
        !! Merges two coverage data sets
        type(coverage_data_t), intent(in) :: data1, data2
        type(coverage_data_t), intent(out) :: merged_data
        
        type(coverage_file_t), allocatable :: temp_files(:)
        integer :: total_files, current_count
        integer :: i
        
        ! Explicitly deallocate before init to avoid allocation errors
        if (allocated(merged_data%files)) deallocate(merged_data%files)
        
        ! Initialize merged data
        call merged_data%init()
        merged_data%version = data1%version
        merged_data%tool = data1%tool
        
        ! Calculate total file count
        total_files = 0
        if (allocated(data1%files)) total_files = total_files + size(data1%files)
        if (allocated(data2%files)) total_files = total_files + size(data2%files)
        
        if (total_files == 0) return
        
        allocate(temp_files(total_files))
        current_count = 0
        
        ! Copy files from first dataset
        if (allocated(data1%files)) then
            do i = 1, size(data1%files)
                current_count = current_count + 1
                temp_files(current_count) = data1%files(i)
            end do
        end if
        
        ! Merge files from second dataset
        if (allocated(data2%files)) then
            do i = 1, size(data2%files)
                call merge_or_add_file(temp_files, current_count, data2%files(i))
            end do
        end if
        
        ! Assign merged files (deallocate if already allocated from init)
        if (allocated(merged_data%files)) deallocate(merged_data%files)
        allocate(merged_data%files(current_count))
        merged_data%files(1:current_count) = temp_files(1:current_count)
        
        ! Clean up temporary array
        if (allocated(temp_files)) deallocate(temp_files)
        
        ! Recalculate overall coverage
        call merged_data%calculate_overall_coverage()
        
    end subroutine merge_coverage_data
    
    subroutine merge_coverage_files(file1, file2, merged_file)
        !! Merges two coverage files
        type(coverage_file_t), intent(in) :: file1, file2
        type(coverage_file_t), intent(out) :: merged_file
        
        type(coverage_line_t), allocatable :: temp_lines(:)
        integer :: total_lines, current_count
        integer :: i
        
        ! Initialize merged file
        call merged_file%init(file1%filename)
        
        ! Calculate total line count
        total_lines = 0
        if (allocated(file1%lines)) total_lines = total_lines + size(file1%lines)
        if (allocated(file2%lines)) total_lines = total_lines + size(file2%lines)
        
        if (total_lines == 0) return
        
        allocate(temp_lines(total_lines))
        current_count = 0
        
        ! Copy lines from first file
        if (allocated(file1%lines)) then
            do i = 1, size(file1%lines)
                current_count = current_count + 1
                temp_lines(current_count) = file1%lines(i)
            end do
        end if
        
        ! Merge lines from second file
        if (allocated(file2%lines)) then
            do i = 1, size(file2%lines)
                call merge_or_add_line(temp_lines, current_count, file2%lines(i))
            end do
        end if
        
        ! Assign merged lines
        allocate(merged_file%lines(current_count))
        merged_file%lines(1:current_count) = temp_lines(1:current_count)
        
        ! Recalculate file coverage
        call merged_file%calculate_coverage()
        
    end subroutine merge_coverage_files
    
    subroutine merge_coverage_lines(line1, line2, merged_line)
        !! Merges two coverage lines
        type(coverage_line_t), intent(in) :: line1, line2
        type(coverage_line_t), intent(out) :: merged_line
        
        ! Copy first line as base
        merged_line = line1
        
        ! Merge execution counts
        merged_line%execution_count = line1%execution_count + line2%execution_count
        
        ! Update executable status
        merged_line%is_executable = line1%is_executable .or. line2%is_executable
        
    end subroutine merge_coverage_lines
    
    subroutine calculate_file_coverage(file)
        !! Calculates coverage statistics for a file
        type(coverage_file_t), intent(inout) :: file
        
        integer :: i, total_executable, covered_count
        
        if (.not. allocated(file%lines)) return
        
        total_executable = 0
        covered_count = 0
        
        do i = 1, size(file%lines)
            if (file%lines(i)%is_executable) then
                total_executable = total_executable + 1
                if (file%lines(i)%is_covered()) then
                    covered_count = covered_count + 1
                end if
            end if
        end do
        
        file%total_lines = total_executable
        file%covered_lines = covered_count
        
        if (total_executable > 0) then
            file%line_coverage = real(covered_count) / real(total_executable) * 100.0
        else
            file%line_coverage = 0.0
        end if
        
    end subroutine calculate_file_coverage
    
    subroutine calculate_overall_coverage(coverage_data)
        !! Calculates overall coverage for the entire dataset
        type(coverage_data_t), intent(inout) :: coverage_data
        
        integer :: i, total_lines, covered_lines
        
        if (.not. allocated(coverage_data%files)) return
        
        total_lines = 0
        covered_lines = 0
        
        do i = 1, size(coverage_data%files)
            call calculate_file_coverage(coverage_data%files(i))
            total_lines = total_lines + coverage_data%files(i)%total_lines
            covered_lines = covered_lines + coverage_data%files(i)%covered_lines
        end do
        
        coverage_data%total_files = size(coverage_data%files)
        coverage_data%total_lines = total_lines
        coverage_data%covered_lines = covered_lines
        
        if (total_lines > 0) then
            coverage_data%overall_coverage = real(covered_lines) / real(total_lines) * 100.0
        else
            coverage_data%overall_coverage = 0.0
        end if
        
    end subroutine calculate_overall_coverage
    
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
            allocate(filtered_data%files(filtered_count))
            filtered_data%files(1:filtered_count) = temp_files(1:filtered_count)
        end if
        
        call filtered_data%calculate_overall_coverage()
        
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
    
    ! Helper functions
    subroutine calculate_file_statistics(file, stats)
        !! Calculates statistics for a single file
        type(coverage_file_t), intent(in) :: file
        type(extended_coverage_stats_t), intent(inout) :: stats
        
        integer :: i
        
        if (allocated(file%lines)) then
            do i = 1, size(file%lines)
                if (file%lines(i)%is_executable) then
                    stats%total_lines = stats%total_lines + 1
                    if (file%lines(i)%is_covered()) then
                        stats%covered_lines = stats%covered_lines + 1
                    end if
                end if
            end do
        end if
        
        if (allocated(file%branches)) then
            stats%total_branches = stats%total_branches + size(file%branches)
            do i = 1, size(file%branches)
                if (file%branches(i)%is_fully_covered()) then
                    stats%covered_branches = stats%covered_branches + 1
                end if
            end do
        end if
        
        if (allocated(file%functions)) then
            stats%total_functions = stats%total_functions + size(file%functions)
            do i = 1, size(file%functions)
                if (file%functions(i)%is_covered()) then
                    stats%covered_functions = stats%covered_functions + 1
                end if
            end do
        end if
        
    end subroutine calculate_file_statistics
    
    subroutine merge_or_add_file(files, count, new_file)
        !! Merges file if exists, otherwise adds new file
        type(coverage_file_t), intent(inout) :: files(:)
        integer, intent(inout) :: count
        type(coverage_file_t), intent(in) :: new_file
        
        integer :: i
        logical :: found
        
        found = .false.
        
        ! Look for existing file with same name
        do i = 1, count
            if (files(i)%filename == new_file%filename) then
                call merge_coverage_files(files(i), new_file, files(i))
                found = .true.
                exit
            end if
        end do
        
        ! Add as new file if not found
        if (.not. found) then
            count = count + 1
            files(count) = new_file
        end if
        
    end subroutine merge_or_add_file
    
    subroutine merge_or_add_line(lines, count, new_line)
        !! Merges line if exists, otherwise adds new line
        type(coverage_line_t), intent(inout) :: lines(:)
        integer, intent(inout) :: count
        type(coverage_line_t), intent(in) :: new_line
        
        integer :: i
        logical :: found
        
        found = .false.
        
        ! Look for existing line with same number and filename
        do i = 1, count
            if (lines(i)%line_number == new_line%line_number .and. &
                lines(i)%filename == new_line%filename) then
                call merge_coverage_lines(lines(i), new_line, lines(i))
                found = .true.
                exit
            end if
        end do
        
        ! Add as new line if not found
        if (.not. found) then
            count = count + 1
            lines(count) = new_line
        end if
        
    end subroutine merge_or_add_line
    
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
    
end module coverage_operations_core