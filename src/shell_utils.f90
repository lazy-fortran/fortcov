module shell_utils
    !! Shell utilities module for command escaping and safe execution
    !!
    !! This module provides shell command utilities that can be used across
    !! multiple security modules without circular dependencies.
    implicit none
    private
    
    ! Public procedures
    public :: escape_shell_argument
    
contains

    ! Shell argument escaping
    function escape_shell_argument(arg) result(escaped_arg)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: escaped_arg
        
        integer :: i, new_len, pos
        ! Use allocatable for dynamic sizing
        character(len=:), allocatable :: temp_arg
        integer :: buffer_size
        
        ! Calculate required buffer size: worst case is all single quotes
        ! Each ' becomes '\'' (4 chars), plus 2 for surrounding quotes
        buffer_size = len(arg)*4 + 2
        
        ! Allocate buffer with calculated size
        allocate(character(len=buffer_size) :: temp_arg)
        
        ! Initialize the entire buffer with spaces first
        temp_arg = repeat(' ', buffer_size)
        
        ! Simple shell escaping by surrounding with single quotes
        ! and escaping any single quotes in the argument
        temp_arg(1:1) = "'"
        pos = 2
        
        do i = 1, len_trim(arg)
            if (arg(i:i) == "'") then
                ! Replace ' with '\'' - requires 4 characters
                ! Add bounds check for safety
                if (pos + 3 <= buffer_size) then
                    temp_arg(pos:pos+3) = "'\'''"
                    pos = pos + 4
                end if
            else
                ! Add bounds check for safety
                if (pos <= buffer_size) then
                    temp_arg(pos:pos) = arg(i:i)
                    pos = pos + 1
                end if
            end if
        end do
        
        ! Add final quote with bounds check
        if (pos <= buffer_size) then
            temp_arg(pos:pos) = "'"
            new_len = pos
        else
            new_len = buffer_size
        end if
        
        escaped_arg = temp_arg(1:new_len)
        deallocate(temp_arg)
    end function escape_shell_argument

end module shell_utils