module config_argument_classifier
    !! Argument classification for command-line parsing
    !!
    !! This module handles classification of command-line arguments into
    !! flags and positional arguments, with proper value association.

    use config_types, only: MAX_ARRAY_SIZE
    use config_parser_utils, only: is_flag_argument, get_long_form_option, &
                                    flag_requires_value
    implicit none
    private

    public :: classify_command_arguments

contains

    subroutine classify_command_arguments(args, flags, flag_count, positionals, &
                                           positional_count, success, error_message)
        !! Classify arguments into flags and positionals
        character(len=*), intent(in) :: args(:)
        character(len=1024), allocatable, intent(out) :: flags(:)
        integer, intent(out) :: flag_count
        character(len=1024), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: positional_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        logical :: expecting_value

        ! Initialize classification state
        call initialize_argument_classification(flags, positionals, flag_count, &
                                               positional_count, expecting_value, &
                                               success, error_message)

        ! Process all arguments
        call process_all_arguments(args, flags, flag_count, positionals, &
                                  positional_count, expecting_value, success, error_message)
        if (.not. success) return

        ! Final validation
        call validate_argument_completion(flags, flag_count, expecting_value, &
                                         success, error_message)

    end subroutine classify_command_arguments
    
    subroutine initialize_argument_classification(flags, positionals, flag_count, &
                                                 positional_count, expecting_value, &
                                                 success, error_message)
        !! Initialize argument classification arrays and state
        character(len=1024), allocatable, intent(out) :: flags(:)
        character(len=1024), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: flag_count, positional_count
        logical, intent(out) :: expecting_value, success
        character(len=*), intent(out) :: error_message
        
        success = .true.
        error_message = ""
        flag_count = 0
        positional_count = 0
        expecting_value = .false.
        
        allocate(character(len=1024) :: flags(MAX_ARRAY_SIZE))
        allocate(character(len=1024) :: positionals(MAX_ARRAY_SIZE))
    end subroutine initialize_argument_classification
    
    subroutine process_all_arguments(args, flags, flag_count, positionals, &
                                    positional_count, expecting_value, success, error_message)
        !! Process all command line arguments
        character(len=*), intent(in) :: args(:)
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        character(len=1024), allocatable, intent(inout) :: positionals(:)
        integer, intent(inout) :: positional_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: i
        character(len=:), allocatable :: arg
        
        success = .true.
        error_message = ""
        
        do i = 1, size(args)
            arg = trim(adjustl(args(i)))
            if (len_trim(arg) == 0) cycle
            
            call process_single_argument(arg, flags, flag_count, positionals, &
                                        positional_count, expecting_value, success, error_message)
            if (.not. success) return
        end do
    end subroutine process_all_arguments
    
    subroutine process_single_argument(arg, flags, flag_count, positionals, &
                                      positional_count, expecting_value, success, error_message)
        !! Process a single command line argument
        character(len=*), intent(in) :: arg
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        character(len=1024), allocatable, intent(inout) :: positionals(:)
        integer, intent(inout) :: positional_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        if (expecting_value) then
            call add_flag_value(arg, flags, flag_count, expecting_value, success, error_message)
        else if (is_flag_argument(arg)) then
            call process_flag_argument(arg, flags, flag_count, expecting_value, &
                                      success, error_message)
        else
            call add_positional_argument(arg, positionals, positional_count, &
                                        success, error_message)
        end if
    end subroutine process_single_argument
    
    subroutine add_flag_value(value, flags, flag_count, expecting_value, success, error_message)
        !! Add flag value to flags array
        character(len=*), intent(in) :: value
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        flag_count = flag_count + 1
        if (flag_count > MAX_ARRAY_SIZE) then
            success = .false.
            error_message = "Too many flag arguments"
            return
        end if
        
        flags(flag_count) = value
        expecting_value = .false.
        success = .true.
        error_message = ""
    end subroutine add_flag_value
    
    subroutine process_flag_argument(arg, flags, flag_count, expecting_value, &
                                    success, error_message)
        !! Process flag argument (with or without equals)
        character(len=*), intent(in) :: arg
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: equals_pos
        
        equals_pos = index(arg, '=')
        if (equals_pos > 0) then
            call process_flag_with_equals(arg, equals_pos, flags, flag_count, &
                                         success, error_message)
        else
            call process_regular_flag(arg, flags, flag_count, expecting_value, &
                                     success, error_message)
        end if
    end subroutine process_flag_argument
    
    subroutine process_flag_with_equals(arg, equals_pos, flags, flag_count, &
                                       success, error_message)
        !! Process flag with equals sign (--flag=value)
        character(len=*), intent(in) :: arg
        integer, intent(in) :: equals_pos
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: flag, value
        
        flag = arg(1:equals_pos-1)
        value = arg(equals_pos+1:)
        
        call add_flag_to_array(flag, flags, flag_count, success, error_message)
        if (.not. success) return
        
        call add_flag_to_array(value, flags, flag_count, success, error_message)
    end subroutine process_flag_with_equals
    
    subroutine process_regular_flag(arg, flags, flag_count, expecting_value, &
                                   success, error_message)
        !! Process regular flag without value
        character(len=*), intent(in) :: arg
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(inout) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: flag
        
        flag = get_long_form_option(arg)
        call add_flag_to_array(flag, flags, flag_count, success, error_message)
        if (.not. success) return
        
        if (flag_requires_value(flag)) then
            expecting_value = .true.
        end if
    end subroutine process_regular_flag
    
    subroutine add_flag_to_array(flag, flags, flag_count, success, error_message)
        !! Add flag to flags array with bounds checking
        character(len=*), intent(in) :: flag
        character(len=1024), allocatable, intent(inout) :: flags(:)
        integer, intent(inout) :: flag_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        flag_count = flag_count + 1
        if (flag_count > MAX_ARRAY_SIZE) then
            success = .false.
            error_message = "Too many flag arguments"
            return
        end if
        
        flags(flag_count) = flag
        success = .true.
        error_message = ""
    end subroutine add_flag_to_array
    
    subroutine add_positional_argument(arg, positionals, positional_count, &
                                      success, error_message)
        !! Add positional argument with bounds checking
        character(len=*), intent(in) :: arg
        character(len=1024), allocatable, intent(inout) :: positionals(:)
        integer, intent(inout) :: positional_count
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        positional_count = positional_count + 1
        if (positional_count > MAX_ARRAY_SIZE) then
            success = .false.
            error_message = "Too many positional arguments"
            return
        end if
        
        positionals(positional_count) = arg
        success = .true.
        error_message = ""
    end subroutine add_positional_argument
    
    subroutine validate_argument_completion(flags, flag_count, expecting_value, &
                                           success, error_message)
        !! Validate that argument processing completed correctly
        character(len=1024), allocatable, intent(in) :: flags(:)
        integer, intent(in) :: flag_count
        logical, intent(in) :: expecting_value
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        if (expecting_value) then
            success = .false.
            error_message = "Missing value for flag: " // trim(flags(flag_count))
        else
            success = .true.
            error_message = ""
        end if
    end subroutine validate_argument_completion

end module config_argument_classifier