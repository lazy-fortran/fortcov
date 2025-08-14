module nested_module
    implicit none
    private
    
    public :: outer_procedure
    public :: module_level_function
    
contains
    
    function module_level_function(x) result(y)
        integer, intent(in) :: x
        integer :: y
        
        y = x + 1
    end function module_level_function
    
    subroutine outer_procedure(value)
        integer, intent(inout) :: value
        
        ! Call internal procedure
        call inner_procedure(value)
        
        ! Call nested function
        value = value + nested_function(10)
        
    contains
        
        subroutine inner_procedure(val)
            integer, intent(inout) :: val
            
            val = val * 3
        end subroutine inner_procedure
        
        function nested_function(input) result(output)
            integer, intent(in) :: input
            integer :: output
            
            output = input / 2
            
            ! Call deeply nested procedure
            call deeply_nested_procedure()
            
        contains
            
            subroutine deeply_nested_procedure()
                ! This represents deeply nested structure
                value = value + 1
            end subroutine deeply_nested_procedure
            
        end function nested_function
        
    end subroutine outer_procedure
    
end module nested_module