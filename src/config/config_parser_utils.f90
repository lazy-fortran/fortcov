module config_parser_utils
    !! Parser utility functions for configuration processing
    !! 
    !! This module provides utility functions for parsing various data types
    !! and managing configuration arrays.

    use config_types, only: config_t, MAX_ARRAY_SIZE
    use constants_core
    use string_utils

    implicit none
    private

    public :: parse_real_with_error
    public :: parse_integer_with_error
    public :: parse_real_value
    public :: parse_integer_value
    public :: add_string_to_array
    public :: add_source_path
    public :: add_exclude_pattern
    public :: add_include_pattern
    public :: is_flag_argument
    public :: get_long_form_option
    public :: flag_requires_value
    public :: parse_diff_files
    public :: parse_threshold_with_error

contains

    subroutine parse_real_with_error(str, value, value_name, success, error_message)
        !! Parse real value with error handling
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call parse_real_value(str, value, success)
        if (.not. success) then
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

        call parse_integer_value(str, value, success)
        if (.not. success) then
            error_message = "Invalid " // trim(value_name) // ": '" // trim(str) // "'"
        else
            error_message = ""
        end if

    end subroutine parse_integer_with_error

    subroutine parse_real_value(str, value, success)
        !! Parse real value from string
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        logical, intent(out) :: success

        integer :: iostat

        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        if (.not. success) then
            value = 0.0
        end if

    end subroutine parse_real_value

    subroutine parse_integer_value(str, value, success)
        !! Parse integer value from string
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        logical, intent(out) :: success

        integer :: iostat

        read(str, *, iostat=iostat) value
        success = (iostat == 0)

        if (.not. success) then
            value = 0
        end if

    end subroutine parse_integer_value

    subroutine add_string_to_array(item, array, max_size, item_type, success, error_message)
        !! Add string to allocatable array with size checking
        character(len=*), intent(in) :: item
        character(len=:), allocatable, intent(inout) :: array(:)
        integer, intent(in) :: max_size
        character(len=*), intent(in) :: item_type
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size, i

        success = .true.
        error_message = ""

        if (.not. allocated(array)) then
            ! Initialize array with first item
            allocate(character(len=len(item)) :: array(1))
            array(1) = item
        else
            current_size = size(array)

            ! Check size limit
            if (current_size >= max_size) then
                success = .false.
                write(error_message, '(A,A,A,I0,A)') &
                    "Too many ", trim(item_type), " (max ", max_size, ")"
                return
            end if

            ! Allocate temporary array with increased size
            new_size = current_size + 1
            allocate(character(len=max(len(array), len(item))) :: temp_array(new_size))

            ! Copy existing items
            do i = 1, current_size
                temp_array(i) = array(i)
            end do

            ! Add new item
            temp_array(new_size) = item

            ! Replace array with temp_array
            call move_alloc(temp_array, array)
        end if

    end subroutine add_string_to_array

    subroutine add_source_path(path, config, success, error_message)
        !! Add source path to configuration
        character(len=*), intent(in) :: path
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(path, config%source_paths, MAX_ARRAY_SIZE, &
                                 "source paths", success, error_message)

    end subroutine add_source_path

    subroutine add_exclude_pattern(pattern, config, success, error_message)
        !! Add exclude pattern to configuration
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(pattern, config%exclude_patterns, MAX_ARRAY_SIZE, &
                                 "exclude patterns", success, error_message)

    end subroutine add_exclude_pattern

    subroutine add_include_pattern(pattern, config, success, error_message)
        !! Add include pattern to configuration
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(pattern, config%include_patterns, MAX_ARRAY_SIZE, &
                                 "include patterns", success, error_message)

    end subroutine add_include_pattern

    function is_flag_argument(arg) result(is_flag)
        !! Check if argument is a flag (starts with - or --)
        character(len=*), intent(in) :: arg
        logical :: is_flag

        is_flag = (len_trim(arg) > 1 .and. arg(1:1) == '-')

    end function is_flag_argument

    function get_long_form_option(short_arg) result(long_form)
        !! Convert short option to long form
        character(len=*), intent(in) :: short_arg
        character(len=:), allocatable :: long_form

        select case (trim(short_arg))
        case ("-h")
            long_form = "--help"
        case ("-v")
            long_form = "--verbose"
        case ("-V")
            long_form = "--version"
        case ("-q")
            long_form = "--quiet"
        case ("-o")
            long_form = "--output"
        case ("-f")
            long_form = "--format"
        case ("-s")
            long_form = "--source"
        case ("-e")
            long_form = "--exclude"
        case ("-i")
            long_form = "--include"
        case ("-m")
            long_form = "--minimum"
        case ("-t")
            long_form = "--threads"
        case ("-c")
            long_form = "--config"
        case default
            long_form = short_arg
        end select

    end function get_long_form_option

    function flag_requires_value(flag) result(requires_value)
        !! Check if flag requires a value
        character(len=*), intent(in) :: flag
        logical :: requires_value

        select case (trim(flag))
        ! Flags that do NOT require values
        case ("--help", "-h", "--version", "-V", "--verbose", "-v", &
              "--quiet", "-q", "--validate", "--keep-gcov-files", &
              "--tui", "--strict", "--include-unchanged", &
              "--auto-discovery", "--no-auto-discovery", &
              "--auto-test", "--no-auto-test")
            requires_value = .false.
        ! Flags that DO require values
        case ("--source", "-s", "--exclude", "-e", "--include", "-i", &
              "--output", "-o", "--format", "-f", "--config", "-c", &
              "--import", "--gcov-executable", "--gcov-args", &
              "--minimum", "-m", "--threshold", "--fail-under", &
              "--threads", "-t", "--diff", "--diff-threshold", &
              "--test-timeout")
            requires_value = .true.
        ! Unknown flags do not require values (will be caught as invalid later)
        case default
            requires_value = .false.
        end select

    end function flag_requires_value

    subroutine parse_diff_files(value, config, success, error_message)
        !! Parse diff file specification (baseline.json,current.json)
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: comma_pos
        character(len=:), allocatable :: baseline_file, current_file
        logical :: baseline_exists, current_exists
        integer :: baseline_len, current_len

        success = .true.
        error_message = ""

        ! Find comma separator
        comma_pos = index(value, ',')
        if (comma_pos == 0) then
            success = .false.
            error_message = "Invalid --diff format. Expected: --diff=baseline.json,current.json"
            return
        end if

        ! Extract baseline and current files
        baseline_file = trim(adjustl(value(1:comma_pos-1)))
        current_file = trim(adjustl(value(comma_pos+1:)))

        ! Validate that both files are specified
        if (len_trim(baseline_file) == 0) then
            success = .false.
            error_message = "Baseline file not specified in --diff format"
            return
        end if

        if (len_trim(current_file) == 0) then
            success = .false.
            error_message = "Current file not specified in --diff format"
            return
        end if

        ! Validate file path lengths to prevent buffer overflow
        baseline_len = len_trim(baseline_file)
        current_len = len_trim(current_file)
        if (baseline_len > MEDIUM_STRING_LEN) then
            success = .false.
            error_message = "Baseline file path too long (max 512 characters)"
            return
        end if
        if (current_len > MEDIUM_STRING_LEN) then
            success = .false.
            error_message = "Current file path too long (max 512 characters)"
            return
        end if

        ! Note: File existence validation is deferred to configuration validation stage
        ! to allow flag recognition even when files don't exist yet

        ! Set the configuration - safe assignment after validation
        config%diff_baseline_file = baseline_file
        config%diff_current_file = current_file

    end subroutine parse_diff_files

    subroutine parse_threshold_with_error(str, value, value_name, success, &
                                          error_message)
        !! Parse threshold value with range validation (0.0-100.0)
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        character(len=*), intent(in) :: value_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        ! First parse as regular real number
        call parse_real_value(str, value, success)
        
        if (.not. success) then
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

end module config_parser_utils