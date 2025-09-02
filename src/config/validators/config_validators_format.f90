module config_validators_format
    implicit none
    private
    
    ! Public procedures
    public :: is_supported_output_format
    public :: is_valid_coverage_file_format
    public :: is_valid_import_file_format
    public :: is_file_readable
    
contains

    function is_supported_output_format(format) result(is_supported)
        !! Check if output format is supported
        character(len=*), intent(in) :: format
        logical :: is_supported

        character(len=:), allocatable :: lower_format

        lower_format = to_lower(trim(format))

        is_supported = (lower_format == "markdown")

    end function is_supported_output_format

    function is_valid_coverage_file_format(file_path) result(is_valid)
        !! Check if coverage file has valid format based on extension
        character(len=*), intent(in) :: file_path
        logical :: is_valid

        character(len=:), allocatable :: extension
        integer :: dot_pos

        is_valid = .false.

        ! Extract file extension
        dot_pos = index(file_path, ".", back=.true.)
        if (dot_pos > 0 .and. dot_pos < len_trim(file_path)) then
            extension = to_lower(file_path(dot_pos:))
            
            ! Only native .gcov coverage files are supported
            if (extension == ".gcov") is_valid = .true.
        end if

    end function is_valid_coverage_file_format

    function is_valid_import_file_format(file_path) result(is_valid)
        !! Check if import file has valid format
        character(len=*), intent(in) :: file_path
        logical :: is_valid

        character(len=:), allocatable :: extension
        integer :: dot_pos

        ! Import of JSON/XML has been removed; always return false
        is_valid = .false.

    end function is_valid_import_file_format

    function is_file_readable(file_path) result(is_readable)
        !! Check if file is readable
        character(len=*), intent(in) :: file_path
        logical :: is_readable

        integer :: unit, iostat

        is_readable = .false.

        ! Try to open file for reading
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        if (iostat == 0) then
            is_readable = .true.
            close(unit)
        end if

    end function is_file_readable
    
    ! Helper function to convert to lowercase
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: lower_str
        integer :: i
        
        lower_str = trim(str)
        do i = 1, len(lower_str)
            if (lower_str(i:i) >= 'A' .and. lower_str(i:i) <= 'Z') then
                lower_str(i:i) = char(ichar(lower_str(i:i)) + 32)
            end if
        end do
    end function to_lower

end module config_validators_format
