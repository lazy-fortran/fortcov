program test_debug_fork_bomb
    !! Debug the fork bomb issue by tracing the exact logic
    
    use config_parser, only: has_input_related_arguments, has_output_related_arguments
    use config_core, only: parse_config, config_t
    implicit none
    
    character(len=128) :: args(3)
    logical :: has_input_args, has_output_args  
    type(config_t) :: config
    logical :: success
    character(len=512) :: error_message
    
    print *, "Debugging fork bomb for: fortcov test.gcov --format=json --verbose"
    print *, "======================================================================"
    
    ! Set up the exact arguments from the issue
    args(1) = "test.gcov"
    args(2) = "--format=json" 
    args(3) = "--verbose"
    
    print *, "Arguments:"
    print *, "  args(1) = '", trim(args(1)), "'"
    print *, "  args(2) = '", trim(args(2)), "'"
    print *, "  args(3) = '", trim(args(3)), "'"
    print *, ""
    
    ! Test the individual logic components
    has_input_args = has_input_related_arguments(args)
    has_output_args = has_output_related_arguments(args)
    
    print *, "Logic check:"
    print *, "  has_input_related_arguments(args)  =", has_input_args
    print *, "  has_output_related_arguments(args) =", has_output_args
    print *, ""
    
    ! Test the should_use_zero_config logic manually
    print *, "should_use_zero_config logic simulation:"
    print *, "  size(args) == 0                    =", (size(args) == 0)
    print *, "  has_output_flags .and. .not. has_input_sources"
    print *, "    = ", has_output_args, " .and. .not. ", has_input_args
    print *, "    = ", (has_output_args .and. .not. has_input_args)
    print *, ""
    
    ! Test full configuration parsing
    call parse_config(args, config, success, error_message)
    
    if (success) then
        print *, "Configuration parsing result:"
        print *, "  success                         =", success
        print *, "  zero_configuration_mode         =", config%zero_configuration_mode
        print *, "  auto_test_execution             =", config%auto_test_execution
        if (allocated(config%coverage_files)) then
            print *, "  coverage_files size             =", size(config%coverage_files)
            if (size(config%coverage_files) > 0) then
                print *, "  coverage_files(1)               = '", trim(config%coverage_files(1)), "'"
            end if
        end if
        print *, ""
        
        if (config%zero_configuration_mode .and. config%auto_test_execution) then
            print *, "❌ FORK BOMB CONDITION DETECTED!"
            print *, "   Zero-config mode enabled WITH auto-test execution"
            print *, "   This will trigger: fpm test -> infinite recursion"
        else if (config%zero_configuration_mode) then
            print *, "⚠️  Zero-config mode enabled but auto-test disabled"
            print *, "   This is the correct fix behavior"
        else
            print *, "✅ Zero-config mode disabled - normal processing"
        end if
    else
        print *, "failed:"
        print *, "  error:", trim(error_message)
    end if
    
end program test_debug_fork_bomb