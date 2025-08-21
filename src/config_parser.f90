module config_parser
    !! Configuration Parser (Decomposed from fortcov_config.f90)
    !! 
    !! Focused on parsing command-line arguments and configuration files.
    !! Separated from validation and storage for better separation of concerns.
    use foundation_constants
    use foundation_layer_utils
    use string_utils
    use file_utils
    use error_handling
    use input_validation
    use zero_configuration_manager
    implicit none
    private
    
    public :: parse_command_line_config
    public :: parse_config_file
    public :: classify_command_arguments
    public :: process_flag_arguments
    public :: process_positional_arguments
    public :: is_flag_argument
    public :: get_long_form_option
    public :: initialize_default_config
    public :: has_input_related_arguments
    
    ! Configuration parsing types (moved from main config)
    type, public :: config_t
        character(len=:), allocatable :: input_format
        character(len=:), allocatable :: output_format
        character(len=:), allocatable :: output_path
        character(len=:), allocatable :: source_paths(:)
        character(len=:), allocatable :: exclude_patterns(:)
        character(len=:), allocatable :: include_patterns(:)
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: gcov_executable
        real :: minimum_coverage
        real :: fail_under_threshold
        integer :: threads
        logical :: verbose
        logical :: quiet
        logical :: show_help
        logical :: show_version
        logical :: validate_config_only
        character(len=:), allocatable :: config_file
        logical :: enable_diff
        character(len=:), allocatable :: diff_baseline_file
        character(len=:), allocatable :: diff_current_file
        logical :: include_unchanged
        real :: diff_threshold
        character(len=:), allocatable :: import_file
        logical :: keep_gcov_files
        character(len=:), allocatable :: gcov_args
        logical :: tui_mode
        logical :: strict_mode
        logical :: zero_configuration_mode
    end type config_t
    
contains
    
    subroutine parse_command_line_config(args, config, success, error_message)
        !! Main command-line configuration parsing function
        !! Extracted from original parse_config function
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: flags(:)
        character(len=:), allocatable :: positionals(:)
        integer :: flag_count, positional_count
        logical :: is_zero_config
        
        ! Initialize config with defaults
        call initialize_default_config(config)
        
        success = .true.
        error_message = ""
        
        ! Process help/version/quiet flags first
        call process_special_flags(args, config)
        
        ! If help or version requested, skip zero-config mode and normal parsing
        if (config%show_help .or. config%show_version) then
            return
        end if
        
        ! Check for zero-configuration mode
        is_zero_config = should_use_zero_config(args)
        
        if (is_zero_config) then
            call handle_zero_configuration_mode(config)
            return
        end if
        
        ! Normal argument parsing for non-zero-configuration mode
        call handle_normal_configuration(args, config, success, error_message)
        
    end subroutine parse_command_line_config
    
    subroutine process_special_flags(args, config)
        !! Process help, version, and quiet flags
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        integer :: i
        
        ! Check for help/version/quiet flags - these override zero-configuration mode
        do i = 1, size(args)
            if (len_trim(args(i)) > 0) then
                if (trim(args(i)) == '--help' .or. trim(args(i)) == '-h') then
                    config%show_help = .true.
                else if (trim(args(i)) == '--version' .or. trim(args(i)) == '-V') then
                    config%show_version = .true.
                else if (trim(args(i)) == '--quiet' .or. trim(args(i)) == '-q') then
                    config%quiet = .true.
                end if
            end if
        end do
    end subroutine process_special_flags
    
    logical function should_use_zero_config(args) result(is_zero_config)
        !! Determine if zero-configuration mode should be used
        character(len=*), intent(in) :: args(:)
        integer :: i
        
        ! No arguments means zero-config
        is_zero_config = (size(args) == 0)
        
        if (.not. is_zero_config .and. size(args) > 0) then
            ! Check if all arguments are empty strings
            is_zero_config = .true.
            do i = 1, size(args)
                if (len_trim(args(i)) > 0) then
                    is_zero_config = .false.
                    exit
                end if
            end do
            
            ! If arguments exist but none are input-related, use zero-config
            if (.not. is_zero_config) then
                is_zero_config = .not. has_input_related_arguments(args)
            end if
        end if
    end function should_use_zero_config
    
    subroutine handle_zero_configuration_mode(config)
        !! Handle zero-configuration mode setup
        type(config_t), intent(inout) :: config
        character(len=:), allocatable :: temp_files(:), temp_paths(:)
        
        ! Mark as zero-configuration mode
        config%zero_configuration_mode = .true.
        
        ! Apply zero-configuration defaults
        call apply_zero_configuration_defaults(config%output_path, &
                                              config%output_format, &
                                              config%input_format, &
                                              config%exclude_patterns)
        
        ! Auto-discover coverage files
        temp_files = auto_discover_coverage_files_priority()
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            config%coverage_files = temp_files
        end if
        
        ! Auto-discover source paths
        temp_paths = auto_discover_source_files_priority()
        if (allocated(temp_paths) .and. size(temp_paths) > 0) then
            config%source_paths = temp_paths
        end if
        
        ! Ensure output directory structure exists
        call ensure_zero_config_output_directory(config)
    end subroutine handle_zero_configuration_mode
    
    subroutine handle_normal_configuration(args, config, success, error_message)
        !! Handle normal command-line configuration
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: flags(:)
        character(len=:), allocatable :: positionals(:)
        integer :: flag_count, positional_count
        
        ! Two-pass parsing: classify arguments first
        call classify_command_arguments(args, flags, flag_count, positionals, &
                                      positional_count)
        
        ! Pass 1: Process flag arguments
        call process_flag_arguments(flags, flag_count, config, success, error_message)
        if (.not. success) return
        
        ! Pass 2: Process positional arguments as coverage files
        call process_positional_arguments(positionals, positional_count, &
                                        config, success, error_message)
        if (.not. success) return
        
        ! Pass 3: Apply HTML default filename logic
        call apply_html_default_filename(config)
    end subroutine handle_normal_configuration
    
    subroutine parse_config_file(config, success, error_message)
        !! Configuration file parsing implementation
        !! Extracted from original load_config_file function
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=1024) :: line
        character(len=256) :: key, value
        integer :: iostat, equal_pos
        integer :: unit
        logical :: file_exists
        
        success = .true.
        error_message = ""
        
        ! Check if config file is specified and exists
        if (.not. allocated(config%config_file)) then
            return  ! No config file specified, not an error
        end if
        
        inquire(file=config%config_file, exist=file_exists)
        if (.not. file_exists) then
            success = .false.
            error_message = "Config file not found: " // config%config_file
            return
        end if
        
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
        
    end subroutine parse_config_file
    
    subroutine classify_command_arguments(args, flags, flag_count, positionals, &
                                        positional_count)
        !! Classifies command line arguments into flags and positionals
        !! Extracted from original classify_arguments function
        character(len=*), intent(in) :: args(:)
        character(len=:), allocatable, intent(out) :: flags(:)
        integer, intent(out) :: flag_count
        character(len=:), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: positional_count
        
        integer :: i, n
        logical :: is_flag, is_value_for_prev_flag
        
        n = size(args)
        flag_count = 0
        positional_count = 0
        is_value_for_prev_flag = .false.
        
        ! Count flags and positionals first
        do i = 1, n
            if (is_value_for_prev_flag) then
                is_value_for_prev_flag = .false.
                cycle
            end if
            
            is_flag = is_flag_argument(args(i))
            if (is_flag) then
                flag_count = flag_count + 1
                ! For flags with '=', don't expect next arg as value
                if (index(args(i), '=') == 0 .and. flag_requires_value(args(i))) then
                    is_value_for_prev_flag = .true.
                end if
            else
                positional_count = positional_count + 1
            end if
        end do
        
        ! Allocate arrays
        if (flag_count > 0) then
            allocate(character(len=MEDIUM_STRING_LEN) :: flags(flag_count))
        end if
        if (positional_count > 0) then
            allocate(character(len=MEDIUM_STRING_LEN) :: positionals(positional_count))
        end if
        
        ! Fill arrays
        flag_count = 0
        positional_count = 0
        is_value_for_prev_flag = .false.
        
        do i = 1, n
            if (is_value_for_prev_flag) then
                is_value_for_prev_flag = .false.
                cycle
            end if
            
            is_flag = is_flag_argument(args(i))
            if (is_flag) then
                flag_count = flag_count + 1
                flags(flag_count) = args(i)
                ! For flags with '=', don't append next arg as value
                if (index(args(i), '=') == 0 .and. flag_requires_value(args(i)) .and. i < n) then
                    flags(flag_count) = trim(flags(flag_count)) // "=" // trim(args(i+1))
                    is_value_for_prev_flag = .true.
                end if
            else
                positional_count = positional_count + 1
                positionals(positional_count) = args(i)
            end if
        end do
        
    end subroutine classify_command_arguments
    
    subroutine process_flag_arguments(flags, flag_count, config, success, error_message)
        !! Processes flag arguments and updates configuration
        !! Extracted from original process_flags function
        character(len=*), intent(in) :: flags(:)
        integer, intent(in) :: flag_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: i, equal_pos
        character(len=256) :: flag, value
        
        success = .true.
        error_message = ""
        
        do i = 1, flag_count
            equal_pos = index(flags(i), '=')
            if (equal_pos > 0) then
                flag = flags(i)(1:equal_pos-1)
                value = flags(i)(equal_pos+1:)
            else
                flag = flags(i)
                value = ""
            end if
            
            call process_single_flag(flag, value, config, success, error_message)
            if (.not. success) return
        end do
        
    end subroutine process_flag_arguments
    
    subroutine process_positional_arguments(positionals, positional_count, &
                                          config, success, error_message)
        !! Processes positional arguments as coverage files
        !! Extracted from original process_positional_arguments function
        character(len=*), intent(in) :: positionals(:)
        integer, intent(in) :: positional_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: i
        
        success = .true.
        error_message = ""
        
        if (positional_count > 0) then
            allocate(character(len=MEDIUM_STRING_LEN) :: config%coverage_files(positional_count))
            do i = 1, positional_count
                config%coverage_files(i) = positionals(i)
            end do
        end if
        
    end subroutine process_positional_arguments
    
    function is_flag_argument(arg) result(is_flag)
        !! Checks if argument is a flag (starts with - or --)
        character(len=*), intent(in) :: arg
        logical :: is_flag
        
        is_flag = (len_trim(arg) > 1 .and. arg(1:1) == '-')
        
    end function is_flag_argument
    
    function get_long_form_option(short_arg) result(long_form)
        !! Converts short option to long form
        !! Extracted from original get_long_form function
        character(len=*), intent(in) :: short_arg
        character(len=:), allocatable :: long_form
        
        select case (trim(short_arg))
        case ('-v')
            long_form = '--verbose'
        case ('-q')
            long_form = '--quiet'
        case ('-h')
            long_form = '--help'
        case ('-V')
            long_form = '--version'
        case ('-o')
            long_form = '--output'
        case ('-f')
            long_form = '--format'
        case ('-t')
            long_form = '--threshold'
        case ('-s')
            long_form = '--source'
        case ('-e')
            long_form = '--exclude'
        case default
            long_form = short_arg
        end select
        
    end function get_long_form_option
    
    function flag_requires_value(flag) result(requires_value)
        !! Checks if flag requires a value
        character(len=*), intent(in) :: flag
        logical :: requires_value
        
        character(len=:), allocatable :: long_flag
        
        long_flag = get_long_form_option(flag)
        
        select case (trim(long_flag))
        case ('--output', '--format', '--output-format', '--threshold', '--source', '--exclude', &
              '--include', '--fail-under', '--threads', '--config', '--diff-baseline', &
              '--diff-current', '--import', '--gcov-executable', '--gcov-args')
            requires_value = .true.
        case default
            requires_value = .false.
        end select
        
    end function flag_requires_value
    
    subroutine initialize_default_config(config)
        !! Initializes configuration with default values
        type(config_t), intent(out) :: config
        
        config%input_format = "gcov"
        config%output_format = "markdown"
        config%minimum_coverage = 0.0
        config%fail_under_threshold = 0.0
        config%threads = 1
        config%verbose = .false.
        config%quiet = .false.
        config%show_help = .false.
        config%show_version = .false.
        config%validate_config_only = .false.
        config%enable_diff = .false.
        config%include_unchanged = .false.
        config%diff_threshold = 0.0
        config%keep_gcov_files = .false.
        config%tui_mode = .false.
        config%strict_mode = .false.
        config%zero_configuration_mode = .false.
        
    end subroutine initialize_default_config
    
    subroutine process_single_flag(flag, value, config, success, error_message)
        !! Processes a single flag and its value
        character(len=*), intent(in) :: flag, value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: long_flag
        real :: real_value
        
        success = .true.
        error_message = ""
        
        long_flag = get_long_form_option(flag)
        
        select case (trim(long_flag))
        case ('--verbose')
            config%verbose = .true.
        case ('--quiet')
            config%quiet = .true.
        case ('--help')
            config%show_help = .true.
        case ('--version')
            config%show_version = .true.
        case ('--output')
            config%output_path = value
        case ('--format', '--output-format')
            config%output_format = value
        case ('--threshold')
            call parse_real_value(value, real_value, success)
            if (success) then
                config%minimum_coverage = real_value
            else
                error_message = "Invalid threshold value: " // trim(value)
            end if
        case ('--tui')
            config%tui_mode = .true.
        case ('--strict')
            config%strict_mode = .true.
        case ('--keep-gcov')
            config%keep_gcov_files = .true.
        case ('--source')
            call add_source_path(value, config, success, error_message)
        case ('--exclude')
            call add_exclude_pattern(value, config, success, error_message)
        case ('--config')
            config%config_file = value
        case ('--gcov-executable')
            config%gcov_executable = value
        case ('--gcov-args')
            config%gcov_args = value
        case ('--diff')
            config%enable_diff = .true.
        case ('--diff-baseline')
            config%diff_baseline_file = value
        case ('--diff-current')
            config%diff_current_file = value
        case ('--include-unchanged')
            config%include_unchanged = .true.
        case ('--import')
            config%import_file = value
        case ('--include')
            call add_include_pattern(value, config, success, error_message)
        case ('--fail-under')
            call parse_real_value(value, real_value, success)
            if (success) then
                config%fail_under_threshold = real_value
            else
                error_message = "Invalid fail-under threshold value: " // trim(value)
            end if
        case ('--threads')
            call parse_integer_value(value, config%threads, success)
            if (.not. success) then
                error_message = "Invalid threads value: " // trim(value)
            end if
        case ('--validate-config')
            config%validate_config_only = .true.
        case default
            success = .false.
            error_message = "Unknown flag: " // trim(flag)
        end select
        
    end subroutine process_single_flag
    
    subroutine process_config_file_option(key, value, config, success, error_message)
        !! Processes a single option from config file
        character(len=*), intent(in) :: key, value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        ! Map config file keys to config fields
        select case (trim(key))
        case ('output_format')
            config%output_format = trim(value)
        case ('output_path')
            config%output_path = trim(value)
        case ('threshold')
            call parse_real_value(value, config%minimum_coverage, success)
            if (.not. success) then
                error_message = "Invalid threshold in config file: " // trim(value)
                return
            end if
        case ('verbose')
            config%verbose = (trim(value) == 'true')
        case ('quiet')
            config%quiet = (trim(value) == 'true')
        case default
            ! Ignore unknown options in config file
        end select
        
        success = .true.
        error_message = ""
        
    end subroutine process_config_file_option
    
    subroutine parse_real_value(str, value, success)
        !! Parses string to real value
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
    end subroutine parse_real_value
    
    subroutine apply_html_default_filename(config)
        !! Applies HTML default filename logic
        type(config_t), intent(inout) :: config
        
        if (trim(config%output_format) == 'html' .and. &
            .not. allocated(config%output_path)) then
            config%output_path = 'coverage.html'
        end if
        
    end subroutine apply_html_default_filename
    
    subroutine add_source_path(path, config, success, error_message)
        !! Adds a source path to the config
        character(len=*), intent(in) :: path
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size
        
        success = .true.
        error_message = ""
        
        if (len_trim(path) == 0) then
            success = .false.
            error_message = "Empty source path provided"
            return
        end if
        
        if (allocated(config%source_paths)) then
            current_size = size(config%source_paths)
            
            ! Check size limits
            if (current_size >= MAX_FILES) then
                success = .false.
                write(error_message, '(A, I0, A)') &
                    "Maximum source path count exceeded (", MAX_FILES, ")"
                return
            end if
            
            ! Reallocate with increased size
            new_size = current_size + 1
            allocate(character(len=max(len(config%source_paths), len_trim(path))) :: temp_array(new_size))
            temp_array(1:current_size) = config%source_paths
            temp_array(new_size) = trim(path)
            call move_alloc(temp_array, config%source_paths)
        else
            ! Initial allocation
            allocate(character(len=len_trim(path)) :: config%source_paths(1))
            config%source_paths(1) = trim(path)
        end if
        
    end subroutine add_source_path
    
    subroutine add_exclude_pattern(pattern, config, success, error_message)
        !! Adds an exclude pattern to the config
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size
        
        success = .true.
        error_message = ""
        
        if (len_trim(pattern) == 0) then
            success = .false.
            error_message = "Empty exclude pattern provided"
            return
        end if
        
        if (allocated(config%exclude_patterns)) then
            current_size = size(config%exclude_patterns)
            
            ! Check size limits
            if (current_size >= MAX_EXCLUDES) then
                success = .false.
                write(error_message, '(A, I0, A)') &
                    "Maximum exclude pattern count exceeded (", MAX_EXCLUDES, ")"
                return
            end if
            
            ! Reallocate with increased size
            new_size = current_size + 1
            allocate(character(len=max(len(config%exclude_patterns), len_trim(pattern))) :: temp_array(new_size))
            temp_array(1:current_size) = config%exclude_patterns
            temp_array(new_size) = trim(pattern)
            call move_alloc(temp_array, config%exclude_patterns)
        else
            ! Initial allocation
            allocate(character(len=len_trim(pattern)) :: config%exclude_patterns(1))
            config%exclude_patterns(1) = trim(pattern)
        end if
        
    end subroutine add_exclude_pattern
    
    subroutine add_include_pattern(pattern, config, success, error_message)
        !! Adds an include pattern to the config
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size
        
        success = .true.
        error_message = ""
        
        if (len_trim(pattern) == 0) then
            success = .false.
            error_message = "Empty include pattern provided"
            return
        end if
        
        if (allocated(config%include_patterns)) then
            current_size = size(config%include_patterns)
            
            ! Check size limits
            if (current_size >= MAX_EXCLUDES) then
                success = .false.
                write(error_message, '(A, I0, A)') &
                    "Maximum include pattern count exceeded (", MAX_EXCLUDES, ")"
                return
            end if
            
            ! Reallocate with increased size
            new_size = current_size + 1
            allocate(character(len=max(len(config%include_patterns), len_trim(pattern))) :: temp_array(new_size))
            temp_array(1:current_size) = config%include_patterns
            temp_array(new_size) = trim(pattern)
            call move_alloc(temp_array, config%include_patterns)
        else
            ! Initial allocation
            allocate(character(len=len_trim(pattern)) :: config%include_patterns(1))
            config%include_patterns(1) = trim(pattern)
        end if
        
    end subroutine add_include_pattern
    
    subroutine parse_integer_value(str, value, success)
        !! Parses string to integer value
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
        ! Validate that threads value is positive
        if (success .and. value <= 0) then
            success = .false.
        end if
        
    end subroutine parse_integer_value
    
    subroutine ensure_zero_config_output_directory(config)
        !! Ensures output directory exists for zero-configuration mode
        use zero_configuration_manager, only: ensure_output_directory_structure
        use error_handling, only: error_context_t
        type(config_t), intent(in) :: config
        
        type(error_context_t) :: error_ctx
        
        if (allocated(config%output_path)) then
            call ensure_output_directory_structure(config%output_path, error_ctx)
            ! Silently handle directory creation errors in zero-config mode
            ! The error will be reported later during file writing if needed
        end if
        
    end subroutine ensure_zero_config_output_directory
    
    function has_input_related_arguments(args) result(has_input_args)
        !! Checks if any input-related arguments are provided
        !! Input-related arguments are those that specify input sources
        character(len=*), intent(in) :: args(:)
        logical :: has_input_args
        
        integer :: i, equal_pos
        character(len=256) :: arg, flag_part, long_form
        logical :: is_value_for_prev_flag
        
        has_input_args = .false.
        is_value_for_prev_flag = .false.
        
        do i = 1, size(args)
            arg = trim(args(i))
            
            ! Skip empty arguments
            if (len_trim(arg) == 0) cycle
            
            ! Skip if this argument is a value for the previous flag
            if (is_value_for_prev_flag) then
                is_value_for_prev_flag = .false.
                cycle
            end if
            
            ! Check if it's a flag
            if (is_flag_argument(arg)) then
                ! Extract flag part (before '=' if present)
                equal_pos = index(arg, '=')
                if (equal_pos > 0) then
                    flag_part = arg(1:equal_pos-1)
                else
                    flag_part = arg
                end if
                
                ! Convert to long form for consistent checking
                long_form = get_long_form_option(flag_part)
                
                ! Check for input-related flags
                select case (trim(long_form))
                case ('--source', '--import', '--gcov-executable', '--gcov-args')
                    has_input_args = .true.
                    return
                end select
                
                ! If flag requires value and doesn't have '=', next arg is value
                if (equal_pos == 0 .and. flag_requires_value(flag_part)) then
                    is_value_for_prev_flag = .true.
                end if
            else
                ! Positional arguments are considered input-related (coverage files)
                has_input_args = .true.
                return
            end if
        end do
        
    end function has_input_related_arguments
    
end module config_parser