module module_with_uncovered
    implicit none
    private
    
    public :: used_procedure
    public :: unused_procedure
    
contains
    
    subroutine used_procedure(value)
        integer, intent(inout) :: value
        
        value = value * 2
    end subroutine used_procedure
    
    subroutine unused_procedure(value)
        integer, intent(inout) :: value
        
        ! This procedure is never called in the test
        value = value + 100
    end subroutine unused_procedure
    
end module module_with_uncovered