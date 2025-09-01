module xml_utils_core
    !! XML Utility Functions
    !! 
    !! Extracted from xml_utils.f90 for SRP compliance (Issue #718).
    !! Handles utility functions for XML processing and file operations.
    use coverage_model_core
    use string_utils, only: int_to_string
    use timestamp_utils, only: get_current_timestamp
    implicit none
    private
    
    public :: get_current_timestamp
    public :: get_directory_path
    public :: get_base_name
    
contains
    
    
    function get_directory_path(filename) result(dir_path)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dir_path
        integer :: last_slash
        
        last_slash = index(filename, '/', back=.true.)
        if (last_slash > 0) then
            dir_path = filename(1:last_slash-1)
        else
            dir_path = '.'
        end if
        
    end function get_directory_path
    
    function get_base_name(filename) result(base_name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: base_name
        integer :: last_slash, last_dot, name_start
        
        last_slash = index(filename, '/', back=.true.)
        last_dot = index(filename, '.', back=.true.)
        
        ! Determine start of the name component (after the last slash, if any)
        if (last_slash > 0) then
            name_start = last_slash + 1
        else
            name_start = 1
        end if
        
        ! If the dot is after the start of the name, treat as extension separator;
        ! otherwise (including hidden dotfiles like ".bashrc" or "dir/.hidden"),
        ! return the full name component
        if (last_dot > name_start) then
            base_name = filename(name_start:last_dot-1)
        else
            base_name = filename(name_start:)
        end if
        
    end function get_base_name
    
end module xml_utils_core
