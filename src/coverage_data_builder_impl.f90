module coverage_data_builder_impl
    use coverage_model_core
    implicit none
    private
    
    ! Public procedures
    public :: build_coverage_data_from_files
    public :: add_file_to_array
    public :: resize_files_array
    public :: normalize_execution_count
    
contains

    ! Build final coverage data from collected files
    subroutine build_coverage_data_from_files(coverage_data, files_array, files_count, &
                                             error_flag)
        type(coverage_data_t), intent(inout) :: coverage_data
        type(coverage_file_t), allocatable, intent(in) :: files_array(:)
        integer, intent(in) :: files_count
        logical, intent(out) :: error_flag
        
        if (files_count > 0) then
            call coverage_data%init()
            coverage_data%files = files_array(1:files_count)
            error_flag = .false.
        else
            error_flag = .true.
        end if
    end subroutine build_coverage_data_from_files

    ! Helper subroutine to add a file to the files array
    subroutine add_file_to_array(files_array, files_count, filename, lines)
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        integer, intent(inout) :: files_count
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(in) :: lines(:)
        
        files_count = files_count + 1
        
        ! Resize array if needed
        if (files_count > size(files_array)) then
            call resize_files_array(files_array)
        end if
        
        ! Initialize the new file with lines and calculate coverage
        call files_array(files_count)%init(filename, lines)
        call files_array(files_count)%calculate_coverage()
    end subroutine add_file_to_array
    
    ! Helper subroutine to resize files array when needed
    subroutine resize_files_array(files_array)
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        type(coverage_file_t), allocatable :: temp_array(:)
        integer :: current_size, new_size
        
        current_size = size(files_array)
        new_size = current_size * 2
        
        ! Copy existing data to temporary array
        allocate(temp_array(current_size))
        temp_array = files_array
        
        ! Reallocate with larger size
        deallocate(files_array)
        allocate(files_array(new_size))
        
        ! Copy data back
        files_array(1:current_size) = temp_array
        
        deallocate(temp_array)
    end subroutine resize_files_array
    
    ! Normalize execution count to handle edge cases
    function normalize_execution_count(exec_count) result(normalized)
        integer, intent(in) :: exec_count
        integer :: normalized
        
        ! Handle negative values and very large values
        if (exec_count < 0) then
            normalized = 0
        else if (exec_count > 999999) then
            normalized = 999999  ! Cap at reasonable maximum
        else
            normalized = exec_count
        end if
    end function normalize_execution_count

end module coverage_data_builder_impl