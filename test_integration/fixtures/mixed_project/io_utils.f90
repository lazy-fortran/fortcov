module io_utils
    implicit none
    private
    
    public :: write_message
    public :: unused_read_file  ! This will remain uncovered
    
contains
    
    subroutine write_message(message)
        character(len=*), intent(in) :: message
        
        write(*, '(A)') "Output: " // trim(message)
    end subroutine write_message
    
    subroutine unused_read_file(filename, content)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: content
        
        ! This subroutine is never called - creates uncovered code
        content = "File content from " // trim(filename)
    end subroutine unused_read_file
    
end module io_utils