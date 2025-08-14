module select_case_module
    implicit none
    private
    
    public :: process_value
    public :: categorize_number
    
contains
    
    function process_value(input) result(output)
        integer, intent(in) :: input
        character(len=20) :: output
        
        select case (input)
        case (1)
            output = "one"
        case (2:5)
            output = "few"
        case (6:10)
            output = "several"
        case (11:)
            output = "many"
        case default
            output = "none"
        end select
    end function process_value
    
    subroutine categorize_number(num, category)
        integer, intent(in) :: num
        character(len=10), intent(out) :: category
        
        select case (num)
        case (:-1)
            category = "negative"
        case (0)
            category = "zero"
        case (1:100)
            category = "small"
        case (101:1000)
            category = "medium"
        case (1001:)
            category = "large"
        end select
    end subroutine categorize_number
    
end module select_case_module