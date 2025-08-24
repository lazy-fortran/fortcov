module config_file_parser
    !! Configuration file parsing
    !! 
    !! This module handles parsing of configuration files in namelist format,
    !! providing a clean interface for loading settings from disk.

    use config_types, only: config_t, MAX_ARRAY_SIZE
    use config_parser_utils
    use foundation_constants
    use string_utils
    use file_utils

    implicit none
    private

    public :: parse_config_file
    public :: process_config_file_option

contains

    subroutine parse_config_file(config, success, error_message)
        !! Parse configuration from file
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: unit, iostat, line_num
        character(len=512) :: line, key, value
        integer :: equals_pos
        logical :: file_exists

        success = .true.
        error_message = ""

        ! Check if config file is specified
        if (len_trim(config%config_file) == 0) then
            ! No config file, use defaults
            return
        end if

        ! Check file exists
        inquire(file=trim(config%config_file), exist=file_exists)
        if (.not. file_exists) then
            success = .false.
            error_message = "Configuration file not found: " // trim(config%config_file)
            return
        end if

        ! Open config file
        open(newunit=unit, file=trim(config%config_file), status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            success = .false.
            error_message = "Cannot open configuration file: " // trim(config%config_file)
            return
        end if

        ! Parse line by line
        line_num = 0
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            line_num = line_num + 1

            ! Skip empty lines and comments
            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#' .or. line(1:1) == '!') cycle

            ! Find equals sign
            equals_pos = index(line, '=')
            if (equals_pos == 0) then
                ! Try colon as alternative separator
                equals_pos = index(line, ':')
                if (equals_pos == 0) cycle
            end if

            ! Extract key and value
            key = adjustl(line(1:equals_pos-1))
            value = adjustl(line(equals_pos+1:))

            ! Remove quotes from value if present
            if (len_trim(value) >= 2) then
                if ((value(1:1) == '"' .and. value(len_trim(value):len_trim(value)) == '"') .or. &
                    (value(1:1) == "'" .and. value(len_trim(value):len_trim(value)) == "'")) then
                    value = value(2:len_trim(value)-1)
                end if
            end if

            ! Process the option
            call process_config_file_option(key, value, config, success, error_message)
            if (.not. success) then
                write(error_message, '(A,I0,A,A)') &
                    "Error at line ", line_num, ": ", trim(error_message)
                close(unit)
                return
            end if
        end do

        close(unit)

    end subroutine parse_config_file

    subroutine process_config_file_option(key, value, config, success, error_message)
        !! Process a single configuration file option
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        success = .true.
        error_message = ""

        select case (trim(adjustl(key)))
        case ("input_format", "input-format")
            config%input_format = trim(value)

        case ("output_format", "output-format", "format")
            config%output_format = trim(value)

        case ("output_path", "output-path", "output")
            config%output_path = trim(value)

        case ("source_path", "source-path", "source")
            call add_source_path(trim(value), config, success, error_message)

        case ("exclude_pattern", "exclude-pattern", "exclude")
            call add_exclude_pattern(trim(value), config, success, error_message)

        case ("include_pattern", "include-pattern", "include")
            call add_include_pattern(trim(value), config, success, error_message)

        case ("gcov_executable", "gcov-executable")
            config%gcov_executable = trim(value)

        case ("gcov_args", "gcov-args")
            config%gcov_args = trim(value)

        case ("minimum_coverage", "minimum-coverage", "minimum")
            call parse_real_with_error(value, config%minimum_coverage, &
                                       "minimum coverage", success, error_message)

        case ("fail_under", "fail-under")
            call parse_real_with_error(value, config%fail_under_threshold, &
                                       "fail threshold", success, error_message)

        case ("threads")
            call parse_integer_with_error(value, config%threads, &
                                          "thread count", success, error_message)

        case ("verbose")
            config%verbose = parse_boolean_value(value)

        case ("quiet")
            config%quiet = parse_boolean_value(value)

        case ("keep_gcov_files", "keep-gcov-files")
            config%keep_gcov_files = parse_boolean_value(value)

        case ("tui_mode", "tui-mode", "tui")
            config%tui_mode = parse_boolean_value(value)

        case ("strict_mode", "strict-mode", "strict")
            config%strict_mode = parse_boolean_value(value)

        case ("diff_threshold", "diff-threshold")
            call parse_real_with_error(value, config%diff_threshold, &
                                       "diff threshold", success, error_message)

        case ("include_unchanged", "include-unchanged")
            config%include_unchanged = parse_boolean_value(value)

        case ("import_file", "import-file", "import")
            config%import_file = trim(value)

        case ("max_files", "max-files")
            call parse_integer_with_error(value, config%max_files, &
                                          "max files", success, error_message)

        case default
            ! Unknown option - warn but don't fail
            if (config%verbose) then
                print '(A)', "Warning: Unknown configuration option: " // trim(key)
            end if
        end select

    contains

        function parse_boolean_value(str) result(bool_val)
            !! Parse boolean value from string
            character(len=*), intent(in) :: str
            logical :: bool_val

            select case (trim(adjustl(str)))
            case ("true", "True", "TRUE", "yes", "Yes", "YES", "1", "on", "On", "ON")
                bool_val = .true.
            case default
                bool_val = .false.
            end select

        end function parse_boolean_value

    end subroutine process_config_file_option

end module config_file_parser