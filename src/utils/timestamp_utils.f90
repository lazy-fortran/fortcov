module timestamp_utils
    !! Centralized timestamp utility functions
    !! Provides standardized ISO 8601 timestamp generation for all components
    !! Eliminates duplication of get_current_timestamp across multiple modules
    
    implicit none
    private
    
    public :: get_current_timestamp, get_current_timestamp_with_space
    
contains
    
    function get_current_timestamp() result(timestamp)
        !! Generate current timestamp in ISO 8601 format (YYYY-MM-DDTHH:MM:SS)
        !! This is the standard format used by most modules
        character(len=19) :: timestamp
        integer :: values(8)
        
        call date_and_time(values=values)
        write(timestamp, '(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') &
            values(1), '-', values(2), '-', values(3), 'T', &
            values(5), ':', values(6), ':', values(7)
            
    end function get_current_timestamp
    
    function get_current_timestamp_with_space() result(timestamp)
        !! Generate current timestamp with space separator (YYYY-MM-DD HH:MM:SS)
        !! Used by HTML reporter for display purposes
        character(len=:), allocatable :: timestamp
        character(len=8) :: date_str
        character(len=10) :: time_str
        
        call date_and_time(date_str, time_str)
        timestamp = date_str(1:4) // '-' // date_str(5:6) // '-' // date_str(7:8) // &
                   ' ' // time_str(1:2) // ':' // time_str(3:4) // ':' // time_str(5:6)
    end function get_current_timestamp_with_space
    
end module timestamp_utils