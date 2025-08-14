module string_utils
    implicit none
    private
    
    public :: concatenate_strings
    public :: reverse_string
    
contains
    
    function concatenate_strings(str1, str2) result(result_str)
        character(len=*), intent(in) :: str1, str2
        character(len=len(str1)+len(str2)) :: result_str
        
        result_str = trim(str1) // trim(str2)
    end function concatenate_strings
    
    function reverse_string(input_str) result(reversed)
        character(len=*), intent(in) :: input_str
        character(len=len(input_str)) :: reversed
        integer :: i, n
        
        n = len(input_str)
        do i = 1, n
            reversed(i:i) = input_str(n-i+1:n-i+1)
        end do
    end function reverse_string
    
end module string_utils