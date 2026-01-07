module config_parser_files
    !! Configuration file parsing for various formats
    !!
    !! This module handles parsing of configuration files in both key-value
    !! and Fortran namelist formats, providing unified configuration loading.

    use config_types, only: config_t, MAX_ARRAY_SIZE
    use config_detector_format, only: detect_config_format
    use config_parser_utils, only: transfer_string_array
    use constants_core

    implicit none
    private

    ! File parsing interface
    public :: parse_config_file
    public :: process_config_file_option

contains

    subroutine parse_config_file(config, success, error_message)
        !! Parse configuration file based on detected format
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        logical :: is_namelist_format
        logical :: detect_success
        character(len=256) :: detect_error

        ! Detect configuration file format
        call detect_config_format(config%config_file, is_namelist_format, detect_success, detect_error)
        if (.not. detect_success) then
            success = .false.
            error_message = trim(detect_error)
            return
        end if

        if (is_namelist_format) then
            call parse_namelist_config_file(config, success, error_message)
        else
            call parse_keyvalue_config_file(config, success, error_message)
        end if

    end subroutine parse_config_file

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

    subroutine parse_namelist_config_file(config, success, error_message)
        !! Parse configuration file in Fortran namelist format
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        type :: namelist_config_data_t
            character(len=256) :: input_format
            character(len=256) :: output_format
            character(len=256) :: output_path
            character(len=256), dimension(MAX_ARRAY_SIZE) :: source_paths
            character(len=256), dimension(MAX_ARRAY_SIZE) :: exclude_patterns
            character(len=256), dimension(MAX_ARRAY_SIZE) :: include_patterns
            character(len=256) :: gcov_executable
            character(len=256) :: gcov_args
            real :: minimum_coverage
            real :: fail_under_threshold
            integer :: threads
            logical :: verbose
            logical :: quiet
            ! TUI removed
            logical :: strict_mode
            logical :: enable_diff
            character(len=256) :: diff_baseline_file
            logical :: include_unchanged
            real :: diff_threshold
            logical :: keep_gcov_files
            integer :: max_files
        end type namelist_config_data_t

        type(namelist_config_data_t) :: namelist_data
        integer :: unit, iostat
        character(len=256) :: iomsg

        ! Namelist variables (required for namelist reading)
        character(len=256) :: input_format, output_format, output_path
        character(len=256), dimension(MAX_ARRAY_SIZE) :: source_paths, exclude_patterns, include_patterns
        character(len=256) :: gcov_executable, gcov_args, diff_baseline_file
        real :: minimum_coverage, fail_under_threshold, diff_threshold
        integer :: threads, max_files
        logical :: verbose, quiet, strict_mode, enable_diff, include_unchanged, keep_gcov_files

        ! Define the namelist
        namelist /fortcov_config/ &
            input_format, output_format, output_path, source_paths, &
            exclude_patterns, include_patterns, gcov_executable, gcov_args, &
            minimum_coverage, fail_under_threshold, threads, &
            verbose, quiet, strict_mode, enable_diff, &
            diff_baseline_file, include_unchanged, diff_threshold, &
            keep_gcov_files, max_files

        success = .true.
        error_message = ""

        ! Initialize namelist data with defaults
        call init_namelist_data(namelist_data)

        ! Initialize local namelist variables
        input_format = namelist_data%input_format
        output_format = namelist_data%output_format
        output_path = namelist_data%output_path
        source_paths = namelist_data%source_paths
        exclude_patterns = namelist_data%exclude_patterns
        include_patterns = namelist_data%include_patterns
        gcov_executable = namelist_data%gcov_executable
        gcov_args = namelist_data%gcov_args
        minimum_coverage = namelist_data%minimum_coverage
        fail_under_threshold = namelist_data%fail_under_threshold
        threads = namelist_data%threads
        verbose = namelist_data%verbose
        quiet = namelist_data%quiet
        strict_mode = namelist_data%strict_mode
        enable_diff = namelist_data%enable_diff
        diff_baseline_file = namelist_data%diff_baseline_file
        include_unchanged = namelist_data%include_unchanged
        diff_threshold = namelist_data%diff_threshold
        keep_gcov_files = namelist_data%keep_gcov_files
        max_files = namelist_data%max_files

        ! Open the config file
        open(newunit=unit, file=config%config_file, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file: " // trim(iomsg)
            return
        end if

        ! Read the namelist
        read(unit, nml=fortcov_config, iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            success = .false.
            call format_namelist_error(iostat, iomsg, error_message)
            close(unit)
            return
        end if

        close(unit)

        ! Transfer back to namelist_data
        namelist_data%input_format = input_format
        namelist_data%output_format = output_format
        namelist_data%output_path = output_path
        namelist_data%source_paths = source_paths
        namelist_data%exclude_patterns = exclude_patterns
        namelist_data%include_patterns = include_patterns
        namelist_data%gcov_executable = gcov_executable
        namelist_data%gcov_args = gcov_args
        namelist_data%minimum_coverage = minimum_coverage
        namelist_data%fail_under_threshold = fail_under_threshold
        namelist_data%threads = threads
        namelist_data%verbose = verbose
        namelist_data%quiet = quiet
        namelist_data%strict_mode = strict_mode
        namelist_data%enable_diff = enable_diff
        namelist_data%diff_baseline_file = diff_baseline_file
        namelist_data%include_unchanged = include_unchanged
        namelist_data%diff_threshold = diff_threshold
        namelist_data%keep_gcov_files = keep_gcov_files
        namelist_data%max_files = max_files

        ! Transfer data to config structure
        call transfer_data_to_config(namelist_data, config)

    contains

        subroutine init_namelist_data(namelist_data)
            !! Initialize namelist data with default values
            type(namelist_config_data_t), intent(out) :: namelist_data

            namelist_data%input_format = ""
            namelist_data%output_format = ""
            namelist_data%output_path = ""
            namelist_data%source_paths = ""
            namelist_data%exclude_patterns = ""
            namelist_data%include_patterns = ""
            namelist_data%gcov_executable = ""
            namelist_data%gcov_args = ""
            namelist_data%minimum_coverage = -1.0
            namelist_data%fail_under_threshold = -1.0
            namelist_data%threads = -1
            namelist_data%verbose = .false.
            namelist_data%quiet = .false.
            namelist_data%strict_mode = .false.
            namelist_data%enable_diff = .false.
            namelist_data%diff_baseline_file = ""
            namelist_data%include_unchanged = .false.
            namelist_data%diff_threshold = -1.0
            namelist_data%keep_gcov_files = .false.
            namelist_data%max_files = -1

        end subroutine init_namelist_data

        subroutine format_namelist_error(iostat, iomsg, error_message)
            !! Format namelist error message
            integer, intent(in) :: iostat
            character(len=*), intent(in) :: iomsg
            character(len=*), intent(out) :: error_message

            if (iostat > 0) then
                error_message = "Error reading namelist: " // trim(iomsg)
            else if (iostat < 0) then
                error_message = "End of file reached while reading namelist"
            else
                error_message = "Unknown error reading namelist"
            end if
        end subroutine format_namelist_error

        subroutine transfer_data_to_config(namelist_data, config)
            !! Transfer namelist data to config structure
            type(namelist_config_data_t), intent(in) :: namelist_data
            type(config_t), intent(inout) :: config

            ! Transfer string values (only if not empty)
            if (len_trim(namelist_data%input_format) > 0) config%input_format = trim(namelist_data%input_format)
            if (len_trim(namelist_data%output_format) > 0) config%output_format = trim(namelist_data%output_format)
            if (len_trim(namelist_data%output_path) > 0) config%output_path = trim(namelist_data%output_path)
            if (len_trim(namelist_data%gcov_executable) > 0) then
                config%gcov_executable = trim(namelist_data%gcov_executable)
            end if
            if (len_trim(namelist_data%gcov_args) > 0) config%gcov_args = trim(namelist_data%gcov_args)
            if (len_trim(namelist_data%diff_baseline_file) > 0) config%diff_baseline_file = trim(namelist_data%diff_baseline_file)

            ! Transfer numeric values (only if set)
            if (namelist_data%minimum_coverage >= 0.0) config%minimum_coverage = namelist_data%minimum_coverage
            if (namelist_data%fail_under_threshold >= 0.0) config%fail_under_threshold = namelist_data%fail_under_threshold
            if (namelist_data%threads > 0) config%threads = namelist_data%threads
            if (namelist_data%max_files > 0) config%max_files = namelist_data%max_files
            if (namelist_data%diff_threshold >= 0.0) config%diff_threshold = namelist_data%diff_threshold

            ! Transfer logical values
            config%verbose = namelist_data%verbose
            config%quiet = namelist_data%quiet
            config%strict_mode = namelist_data%strict_mode
            config%enable_diff = namelist_data%enable_diff
            config%include_unchanged = namelist_data%include_unchanged
            config%keep_gcov_files = namelist_data%keep_gcov_files

            ! Transfer arrays from fixed-length to allocatable arrays
            call convert_fixed_to_allocatable_array(namelist_data%source_paths, config%source_paths)
            call convert_fixed_to_allocatable_array(namelist_data%exclude_patterns, config%exclude_patterns)
            call convert_fixed_to_allocatable_array(namelist_data%include_patterns, config%include_patterns)

        end subroutine transfer_data_to_config

    end subroutine parse_namelist_config_file

    subroutine process_config_file_option(key, value, config, success, error_message)
        !! Process a single configuration file option (key-value format)
        character(len=*), intent(in) :: key, value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        success = .true.
        error_message = ""

        select case (trim(adjustl(key)))
        case ("input_format")
            config%input_format = trim(value)
        case ("output_format")
            config%output_format = trim(value)
        case ("output_path")
            config%output_path = trim(value)
        case ("gcov_executable", "gcov_cmd")
            config%gcov_executable = trim(value)
        case ("gcov_args")
            config%gcov_args = trim(value)
        case ("minimum_coverage")
            call parse_real_with_error_local(value, config%minimum_coverage, &
                                       "minimum coverage", success, error_message)
        case ("fail_under_threshold")
            call parse_real_with_error_local(value, config%fail_under_threshold, &
                                       "fail threshold", success, error_message)
        case ("threads")
            call parse_integer_with_error_local(value, config%threads, &
                                          "thread count", success, error_message)
        case ("verbose")
            config%verbose = (trim(adjustl(value)) == "true")
        case ("quiet")
            config%quiet = (trim(adjustl(value)) == "true")
        case ("strict_mode")
            config%strict_mode = (trim(adjustl(value)) == "true")
        case ("keep_gcov_files")
            config%keep_gcov_files = (trim(adjustl(value)) == "true")
        case ("auto_discovery")
            config%auto_discovery = (trim(adjustl(value)) == "true")
        case ("auto-test-execution")
            config%auto_test_execution = (trim(adjustl(value)) == "yes")
        case ("auto-test")
            config%auto_test_execution = (trim(adjustl(value)) /= "no")
        case ("test-timeout-seconds")
            call parse_integer_with_error_local(value, config%test_timeout_seconds, &
                                          "test timeout seconds", success, error_message)
        case ("test_timeout_seconds")
            call parse_integer_with_error_local(value, config%test_timeout_seconds, &
                                          "test timeout seconds", success, error_message)
        case ("test-timeout")
            call parse_integer_with_error_local(value, config%test_timeout_seconds, &
                                          "test timeout", success, error_message)
        case default
            ! Unknown option - warn but continue
            if (config%verbose) then
                print '(A)', "Warning: Unknown config option: " // trim(key)
            end if
        end select

    contains

        subroutine parse_real_with_error_local(str, value, value_name, success, error_message)
            !! Local parse real with error handling
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
        end subroutine parse_real_with_error_local

        subroutine parse_integer_with_error_local(str, value, value_name, success, error_message)
            !! Local parse integer with error handling
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
        end subroutine parse_integer_with_error_local

    end subroutine process_config_file_option

    subroutine convert_fixed_to_allocatable_array(fixed_array, allocatable_array)
        !! Convert fixed-length array to allocatable deferred-length array
        !! Only includes non-empty strings from the fixed array
        character(len=*), dimension(:), intent(in) :: fixed_array
        character(len=:), allocatable, intent(out) :: allocatable_array(:)
        
        integer :: i, non_empty_count
        character(len=:), allocatable :: temp_array(:)
        integer :: max_len
        
        ! Count non-empty strings and find maximum length
        non_empty_count = 0
        max_len = 0
        do i = 1, size(fixed_array)
            if (len_trim(fixed_array(i)) > 0) then
                non_empty_count = non_empty_count + 1
                max_len = max(max_len, len_trim(fixed_array(i)))
            end if
        end do
        
        ! Handle empty case
        if (non_empty_count == 0) then
            allocate(character(len=0) :: allocatable_array(0))
            return
        end if
        
        ! Allocate with deferred length
        allocate(character(len=max_len) :: allocatable_array(non_empty_count))
        
        ! Copy non-empty strings
        non_empty_count = 0
        do i = 1, size(fixed_array)
            if (len_trim(fixed_array(i)) > 0) then
                non_empty_count = non_empty_count + 1
                allocatable_array(non_empty_count) = trim(fixed_array(i))
            end if
        end do
    end subroutine convert_fixed_to_allocatable_array

end module config_parser_files
