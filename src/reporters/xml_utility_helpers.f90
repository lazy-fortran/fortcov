module xml_utility_helpers
    !! XML Utility Functions and Helpers
    !! 
    !! Handles utility functions for paths, timestamps, and string processing.
    !! Extracted from xml_utils.f90 for SRP compliance (Issue #718).
    use timestamp_utils, only: get_current_timestamp
    implicit none
    private
    
    public :: get_current_timestamp, get_directory_path, get_base_name

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
        
        if (last_slash > 0) then
            name_start = last_slash + 1
        else
            name_start = 1
        end if
        
        if (last_dot > name_start) then
            base_name = filename(name_start:last_dot-1)
        else
            base_name = filename(name_start:)
        end if
        
    end function get_base_name

end module xml_utility_helpers
