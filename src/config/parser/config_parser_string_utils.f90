module config_parser_string_utils
    !! String parsing and type conversion utilities
    !!
    !! This module provides core string parsing functions for configuration
    !! values, including type conversions with comprehensive error handling.

    use constants_core, only: MEDIUM_STRING_LEN

    implicit none
    private

    ! Type conversion utilities
    public :: parse_real_with_error
    public :: parse_integer_with_error
    public :: parse_threshold_with_error

contains

    subroutine parse_real_with_error(str, value, value_name, success, error_message)
        !! Parse real value with error handling
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: iostat

        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        if (.not. success) then
            value = 0.0
            error_message = "Invalid " // trim(value_name) // ": '" // trim(str) // "'"
        else
            error_message = ""
        end if

    end subroutine parse_real_with_error

    subroutine parse_integer_with_error(str, value, value_name, success, error_message)
        !! Parse integer value with error handling
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: iostat

        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        if (.not. success) then
            value = 0
            error_message = "Invalid " // trim(value_name) // ": '" // trim(str) // "'"
        else
            error_message = ""
        end if

    end subroutine parse_integer_with_error

    subroutine parse_threshold_with_error(str, value, value_name, success, &
                                          error_message)
        !! Parse threshold value with range validation (0.0-100.0)
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: iostat

        ! First parse as regular real number
        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        if (.not. success) then
            value = 0.0
            error_message = "Invalid " // trim(value_name) // ": '" // &
                           trim(str) // "'"
            return
        end if

        ! Validate range (0.0 to 100.0)
        if (value < 0.0 .or. value > 100.0) then
            success = .false.
            error_message = "Invalid " // trim(value_name) // ": '" // &
                           trim(str) // "' (must be between 0.0 and 100.0)"
        else
            error_message = ""
        end if

    end subroutine parse_threshold_with_error

end module config_parser_string_utils