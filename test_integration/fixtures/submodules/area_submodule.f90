submodule (parent_module) area_submodule
    implicit none
    
contains
    
    module function calculate_area(length, width) result(area)
        real, intent(in) :: length, width
        real :: area
        
        area = length * width
        
        ! Additional processing
        if (area < 0.0) then
            area = 0.0
        end if
    end function calculate_area
    
end submodule area_submodule