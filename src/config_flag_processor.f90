module config_flag_processor
    !! Configuration Flag Processing Module
    !! 
    !! Focused on processing command-line flags and their validation.
    !! Extracted from config_parser.f90 to maintain SRP and size limits.
    use foundation_constants
    use foundation_layer_utils
    use string_utils
    use error_handling
    use input_validation
    use fortcov_config, only: config_t
    implicit none
    private
    
    public :: process_flag_arguments
    public :: process_single_flag
    public :: flag_requires_value
    public :: get_long_form_option
    public :: parse_real_with_error
    public :: parse_integer_with_error
    public :: add_source_path
    public :: add_exclude_pattern
    public :: add_include_pattern
    
    
contains
    
    subroutine process_flag_arguments(flags, flag_count, config, success, error_message)
        !! Processes flag arguments and updates configuration
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
    
    subroutine process_single_flag(flag, value, config, success, error_message)
        !! Processes a single flag and its value
        character(len=*), intent(in) :: flag, value
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: long_flag
        
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
            call parse_real_with_error(value, config%minimum_coverage, &
                                     "threshold", success, error_message)
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
            call parse_real_with_error(value, config%fail_under_threshold, &
                                     "fail-under threshold", success, error_message)
        case ('--threads')
            call parse_integer_with_error(value, config%threads, &
                                        "threads", success, error_message)
        case ('--max-files')
            call parse_integer_with_error(value, config%max_files, &
                                        "max-files", success, error_message)
        case ('--validate-config')
            config%validate_config_only = .true.
        case default
            success = .false.
            error_message = "Unknown flag: " // trim(flag)
        end select
        
    end subroutine process_single_flag
    
    function get_long_form_option(short_arg) result(long_form)
        !! Converts short option to long form
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
              '--diff-current', '--import', '--gcov-executable', '--gcov-args', '--max-files')
            requires_value = .true.
        case default
            requires_value = .false.
        end select
        
    end function flag_requires_value
    
    subroutine parse_real_with_error(str, value, value_name, success, error_message)
        !! Generic real parsing with error message generation
        character(len=*), intent(in) :: str, value_name
        real, intent(out) :: value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_real_value(str, value, success)
        if (.not. success) then
            error_message = "Invalid " // trim(value_name) // " value: " // trim(str)
        else
            ! Validate threshold ranges if this is a threshold value
            if (index(value_name, 'threshold') > 0) then
                if (value < 0.0 .or. value > 100.0) then
                    success = .false.
                    write(error_message, '(A, A, A, F6.2, A)') &
                        "Invalid ", trim(value_name), " value: ", value, &
                        "% (must be between 0-100%)"
                    return
                end if
            end if
            error_message = ""
        end if
        
    end subroutine parse_real_with_error
    
    subroutine parse_integer_with_error(str, value, value_name, success, error_message)
        !! Generic integer parsing with error message generation
        character(len=*), intent(in) :: str, value_name
        integer, intent(out) :: value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_integer_value(str, value, success)
        if (.not. success) then
            error_message = "Invalid " // trim(value_name) // " value: " // trim(str)
        else
            error_message = ""
        end if
        
    end subroutine parse_integer_with_error
    
    subroutine parse_real_value(str, value, success)
        !! Parses string to real value
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
    end subroutine parse_real_value
    
    subroutine parse_integer_value(str, value, success)
        !! Parses string to integer value
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
        ! Validate that value is positive for threads
        if (success .and. value <= 0) then
            success = .false.
        end if
        
    end subroutine parse_integer_value
    
    subroutine add_string_to_array(item, array, max_size, item_type, success, error_message)
        !! Generic subroutine to add string to allocatable array
        character(len=*), intent(in) :: item
        character(len=:), allocatable, intent(inout) :: array(:)
        integer, intent(in) :: max_size
        character(len=*), intent(in) :: item_type
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size
        
        success = .true.
        error_message = ""
        
        if (len_trim(item) == 0) then
            success = .false.
            error_message = "Empty " // trim(item_type) // " provided"
            return
        end if
        
        if (allocated(array)) then
            current_size = size(array)
            
            ! Check size limits
            if (current_size >= max_size) then
                success = .false.
                write(error_message, '(A, A, A, I0, A)') &
                    "Maximum ", trim(item_type), " count exceeded (", max_size, ")"
                return
            end if
            
            ! Reallocate with increased size
            new_size = current_size + 1
            allocate(character(len=max(len(array), len_trim(item))) :: temp_array(new_size))
            temp_array(1:current_size) = array
            temp_array(new_size) = trim(item)
            call move_alloc(temp_array, array)
        else
            ! Initial allocation
            allocate(character(len=len_trim(item)) :: array(1))
            array(1) = trim(item)
        end if
        
    end subroutine add_string_to_array
    
    subroutine add_source_path(path, config, success, error_message)
        !! Adds a source path to the config
        character(len=*), intent(in) :: path
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call add_string_to_array(path, config%source_paths, MAX_FILES, &
                               "source path", success, error_message)
        
    end subroutine add_source_path
    
    subroutine add_exclude_pattern(pattern, config, success, error_message)
        !! Adds an exclude pattern to the config
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call add_string_to_array(pattern, config%exclude_patterns, MAX_EXCLUDES, &
                               "exclude pattern", success, error_message)
        
    end subroutine add_exclude_pattern
    
    subroutine add_include_pattern(pattern, config, success, error_message)
        !! Adds an include pattern to the config
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call add_string_to_array(pattern, config%include_patterns, MAX_EXCLUDES, &
                               "include pattern", success, error_message)
        
    end subroutine add_include_pattern
    
end module config_flag_processor