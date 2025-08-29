module timestamp_utils
    !! Consolidated Timestamp Utilities
    !! 
    !! Extracted from xml_utils_core.f90 to provide consistent timestamp
    !! functionality across the codebase. Eliminates hardcoded fake timestamps
    !! and provides proper ISO 8601 timestamp generation.
    implicit none
    private
    
    public :: get_current_timestamp
    
contains
    
    function get_current_timestamp() result(timestamp)
        !! Get current timestamp in ISO 8601 format (YYYY-MM-DDTHH:MM:SS)
        character(len=19) :: timestamp
        integer :: values(8)
        
        call date_and_time(values=values)
        write(timestamp, '(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') &
            values(1), '-', values(2), '-', values(3), 'T', &
            values(5), ':', values(6), ':', values(7)
            
    end function get_current_timestamp
    
end module timestamp_utils