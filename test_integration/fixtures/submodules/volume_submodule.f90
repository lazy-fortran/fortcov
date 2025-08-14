submodule (parent_module) volume_submodule
    implicit none
    
contains
    
    module function calculate_volume(length, width, height) result(volume)
        real, intent(in) :: length, width, height
        real :: volume
        
        ! Use the area calculation from the parent module
        volume = calculate_area(length, width) * height
        
        ! Additional validation
        if (volume < 0.0) then
            volume = 0.0
        end if
    end function calculate_volume
    
end submodule volume_submodule