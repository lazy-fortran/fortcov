module config_arg_classifier
    !! Configuration Argument Classifier Module
    !! 
    !! Focused on classifying and organizing command-line arguments.
    !! Extracted from config_parser.f90 to maintain SRP and size limits.
    use foundation_constants
    use foundation_layer_utils
    use fortcov_config, only: config_t
    use config_flag_processor, only: flag_requires_value, get_long_form_option
    implicit none
    private
    
    public :: classify_command_arguments
    public :: process_positional_arguments
    public :: has_input_related_arguments
    public :: has_output_related_arguments
    public :: is_flag_argument
    
contains
    
    subroutine classify_command_arguments(args, flags, flag_count, positionals, &
                                        positional_count)
        !! Classifies command line arguments into flags and positionals
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
    
    subroutine process_positional_arguments(positionals, positional_count, &
                                          config, success, error_message)
        !! Processes positional arguments as coverage files
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
    
    function has_input_related_arguments(args) result(has_input_args)
        !! Checks if any input-related arguments are provided
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
                case ('--source', '--import', '--gcov-executable', '--gcov-args', '--tui', '--diff', &
                      '--diff-baseline', '--diff-current', '--include-unchanged')
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
    
    function has_output_related_arguments(args) result(has_output_args)
        !! Checks if any output-related arguments are provided
        character(len=*), intent(in) :: args(:)
        logical :: has_output_args
        
        integer :: i, equal_pos
        character(len=256) :: arg, flag_part, long_form
        logical :: is_value_for_prev_flag
        
        has_output_args = .false.
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
                
                ! Check for output-related flags
                select case (trim(long_form))
                case ('--output', '--format', '--output-format', '--threshold', '--verbose', '--quiet', &
                      '--fail-under', '--threads', '--strict', '--keep-gcov', '--config', &
                      '--include', '--exclude', '--max-files', '--validate-config')
                    has_output_args = .true.
                    return
                end select
                
                ! If flag requires value and doesn't have '=', next arg is value
                if (equal_pos == 0 .and. flag_requires_value(flag_part)) then
                    is_value_for_prev_flag = .true.
                end if
            end if
        end do
        
    end function has_output_related_arguments
    
    function is_flag_argument(arg) result(is_flag)
        !! Checks if argument is a flag (starts with - or --)
        character(len=*), intent(in) :: arg
        logical :: is_flag
        
        is_flag = (len_trim(arg) > 1 .and. arg(1:1) == '-')
        
    end function is_flag_argument
    
end module config_arg_classifier