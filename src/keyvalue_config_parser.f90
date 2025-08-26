module keyvalue_config_parser
    !! Key-value configuration file parser
    use fortcov_config, only: config_t
    use config_file_parser, only: process_config_file_option
    implicit none
    private
    
    public :: parse_keyvalue_config_file
    public :: parse_config_real_value
    public :: parse_config_integer_value
    
contains

    subroutine parse_keyvalue_config_file(config, success, error_message)
        !! Parse configuration file in simple key=value format
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=1024) :: line
        character(len=256) :: key, value
        integer :: iostat, equal_pos
        integer :: unit
        
        success = .true.
        error_message = ""
        
        ! Open and read config file
        open(newunit=unit, file=config%config_file, status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file: " // config%config_file
            return
        end if
        
        ! Parse file line by line
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = adjustl(line)
            if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
            
            equal_pos = index(line, '=')
            if (equal_pos == 0) cycle
            
            key = adjustl(line(1:equal_pos-1))
            value = adjustl(line(equal_pos+1:))
            
            call process_config_file_option(key, value, config, success, error_message)
            if (.not. success) then
                close(unit)
                return
            end if
        end do
        
        close(unit)
        
    end subroutine parse_keyvalue_config_file
    
    subroutine parse_config_real_value(str, value, success)
        !! Parses string to real value for config file
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
    end subroutine parse_config_real_value
    
    subroutine parse_config_integer_value(str, value, success)
        !! Parses string to integer value for config file
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
    end subroutine parse_config_integer_value

end module keyvalue_config_parser