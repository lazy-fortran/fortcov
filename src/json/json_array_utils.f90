module json_array_utils
    !! JSON array utilities extracted from json_parser
    !! 
    !! Focused on dynamic array management for JSON parsing.
    !! Provides clean separation of array growth logic from parsing operations.
    use coverage_model_core, only: file_coverage_t, line_coverage_t
    implicit none
    private
    
    ! Public interface
    public :: grow_files_array
    public :: grow_lines_array
    
contains
    
    subroutine grow_files_array(temp_files, capacity)
        !! Grows the temporary files array when needed
        type(file_coverage_t), allocatable, intent(inout) :: temp_files(:)
        integer, intent(inout) :: capacity
        type(file_coverage_t), allocatable :: new_array(:)
        
        capacity = capacity * 2
        allocate(new_array(capacity))
        
        if (allocated(temp_files)) then
            new_array(1:size(temp_files)) = temp_files
            deallocate(temp_files)
        end if
        
        call move_alloc(new_array, temp_files)
    end subroutine grow_files_array
    
    subroutine grow_lines_array(temp_lines, capacity)
        !! Grows the temporary lines array when needed
        type(line_coverage_t), allocatable, intent(inout) :: temp_lines(:)
        integer, intent(inout) :: capacity
        type(line_coverage_t), allocatable :: new_array(:)
        
        capacity = capacity * 2
        allocate(new_array(capacity))
        
        if (allocated(temp_lines)) then
            new_array(1:size(temp_lines)) = temp_lines
            deallocate(temp_lines)
        end if
        
        call move_alloc(new_array, temp_lines)
    end subroutine grow_lines_array
    
end module json_array_utils