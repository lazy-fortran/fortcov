module path_string_utils
    implicit none
    private
    
    ! Public procedures
    public :: to_lowercase
    public :: to_uppercase
    public :: starts_with_ignore_case
    
contains

    ! Helper to convert string to lowercase
    subroutine to_lowercase(input, output)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer :: i
        
        output = input
        do i = 1, len_trim(input)
            if (input(i:i) >= 'A' .and. input(i:i) <= 'Z') then
                output(i:i) = char(ichar(input(i:i)) + 32)
            end if
        end do
    end subroutine to_lowercase
    
    ! Helper to convert string to uppercase
    subroutine to_uppercase(input, output)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer :: i
        
        output = input
        do i = 1, len_trim(input)
            if (input(i:i) >= 'a' .and. input(i:i) <= 'z') then
                output(i:i) = char(ichar(input(i:i)) - 32)
            end if
        end do
    end subroutine to_uppercase
    
    ! PERFORMANCE: Fast case-insensitive prefix check
    pure function starts_with_ignore_case(str, prefix) result(starts)
        character(len=*), intent(in) :: str, prefix
        logical :: starts
        integer :: i, prefix_len
        character :: c1, c2
        
        starts = .false.
        prefix_len = len_trim(prefix)
        
        if (len_trim(str) < prefix_len) return
        
        ! Fast character-by-character comparison with case conversion
        do i = 1, prefix_len
            c1 = str(i:i)
            c2 = prefix(i:i)
            
            ! Convert to lowercase for comparison
            if (c1 >= 'A' .and. c1 <= 'Z') c1 = char(ichar(c1) + 32)
            if (c2 >= 'A' .and. c2 <= 'Z') c2 = char(ichar(c2) + 32)
            
            if (c1 /= c2) return
        end do
        
        starts = .true.
    end function starts_with_ignore_case

end module path_string_utils