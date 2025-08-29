module config_detector_format
    !! Config file format detection module
    implicit none
    private
    
    public :: detect_config_format
    
contains

    subroutine detect_config_format(filename, is_namelist, success, error_message)
        !! Detect whether config file is namelist or key=value format
        character(len=*), intent(in) :: filename
        logical, intent(out) :: is_namelist
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: unit, iostat
        character(len=1024) :: line
        
        success = .true.
        error_message = ""
        is_namelist = .false.
        
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            success = .false.
            error_message = "Config file not found: " // trim(filename)
            return
        end if
        
        ! Read through file looking for namelist markers
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = adjustl(line)
            
            ! Skip empty lines and comments
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '!' .or. line(1:1) == '#') cycle
            
            ! Check for namelist start marker
            if (line(1:1) == '&') then
                is_namelist = .true.
                exit
            end if
            
            ! If we find a key=value line without namelist marker, it's key=value format
            if (index(line, '=') > 0) then
                is_namelist = .false.
                exit
            end if
        end do
        
        close(unit)
    end subroutine detect_config_format

end module config_detector_format