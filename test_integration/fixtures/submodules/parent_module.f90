module parent_module
    implicit none
    private
    
    public :: calculate_area
    public :: calculate_volume
    
    interface
        module function calculate_area(length, width) result(area)
            real, intent(in) :: length, width
            real :: area
        end function calculate_area
        
        module function calculate_volume(length, width, height) result(volume)
            real, intent(in) :: length, width, height
            real :: volume
        end function calculate_volume
    end interface
    
end module parent_module