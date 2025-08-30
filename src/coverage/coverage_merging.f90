module coverage_merging
    !! Coverage data merging operations extracted from coverage_operations_core
    !! 
    !! Focused solely on merging coverage datasets, files, and lines.
    !! Provides clean separation of merging logic from calculations and filtering.
    use constants_core
    use foundation_utils
    use coverage_data_core
    use coverage_calculations, only: calculate_file_coverage
    implicit none
    private
    
    public :: merge_coverage_data
    public :: merge_coverage_files
    public :: merge_coverage_lines
    
contains
    
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
        
        ! Recalculate file coverage (delegates to coverage_calculations)
        call calculate_file_coverage(merged_file)
        
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
    
    ! Helper functions
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
    
end module coverage_merging