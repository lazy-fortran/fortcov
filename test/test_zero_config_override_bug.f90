program test_zero_config_override_bug
    !!
    !! Given-When-Then Test Documentation:
    !! 
    !! GIVEN: CLI arguments that are NOT input-related
    !! WHEN: Config parsing processes these arguments
    !! THEN: Parsed values should be preserved (not overridden by zero-config)
    !! 
    !! This test demonstrates the ROOT CAUSE of Issue #231:
    !! Zero-configuration defaults override correctly parsed CLI flag values
    !! for output-related flags like --output, --format, --verbose, --threshold
    !!
    use config_core
    implicit none
    
    type(config_t) :: config
    character(len=:), allocatable :: args(:)
    logical :: success
    character(len=256) :: error_message
    
    print *, "============================================="
    print *, "Zero-Config Override Bug Demonstration"
    print *, "============================================="
    print *, ""
    print *, "Root cause of Issue #231: Zero-configuration logic"
    print *, "overrides correctly parsed CLI flag values"
    print *, ""
    
    ! Test Case 1: Output format override
    print *, "Test 1: --format=json should be preserved"
    allocate(character(len=256) :: args(1))
    args(1) = "--format=json"
    
    call parse_command_line_config(args, config, success, error_message)
    
    if (.not. success) then
        print *, "   failed: ", trim(error_message)
    else
        print *, "   Parse successful"
        if (allocated(config%output_format)) then
            print *, "   Parsed format: ", trim(config%output_format)
            if (trim(config%output_format) == "json") then
                print *, "   ✅ Format correctly preserved"
            else
                print *, "   ❌ BUG: Format overridden to ", trim(config%output_format)
                print *, "       Expected: json"
            end if
        else
            print *, "   ❌ BUG: Format not allocated"
        end if
    end if
    print *, ""
    
    ! Test Case 2: Output path override  
    print *, "Test 2: --output=custom.json should be preserved"
    deallocate(args)
    allocate(character(len=256) :: args(1))
    args(1) = "--output=custom.json"
    
    call parse_command_line_config(args, config, success, error_message)
    
    if (.not. success) then
        print *, "   failed: ", trim(error_message)
    else
        print *, "   Parse successful"
        if (allocated(config%output_path)) then
            print *, "   Parsed path: ", trim(config%output_path)
            if (trim(config%output_path) == "custom.json") then
                print *, "   ✅ Path correctly preserved"
            else
                print *, "   ❌ BUG: Path overridden to ", trim(config%output_path)
                print *, "       Expected: custom.json"
            end if
        else
            print *, "   ❌ BUG: Path not allocated"
        end if
    end if
    print *, ""
    
    ! Test Case 3: Threshold override
    print *, "Test 3: --threshold=95.5 should be preserved"
    deallocate(args)
    allocate(character(len=256) :: args(1))
    args(1) = "--threshold=95.5"
    
    call parse_command_line_config(args, config, success, error_message)
    
    if (.not. success) then
        print *, "   failed: ", trim(error_message)
    else
        print *, "   Parse successful"
        print *, "   Parsed threshold: ", config%minimum_coverage
        if (abs(config%minimum_coverage - 95.5) < 0.1) then
            print *, "   ✅ Threshold correctly preserved"
        else
            print *, "   ❌ BUG: Threshold overridden to ", config%minimum_coverage
            print *, "       Expected: 95.5"
        end if
    end if
    print *, ""
    
    print *, "============================================="
    print *, "Root Cause Analysis"
    print *, "============================================="
    print *, ""
    print *, "The bug is in config_parser.f90:141:"
    print *, "   is_zero_config = .not. has_input_related_arguments(args)"
    print *, ""
    print *, "has_input_related_arguments() only considers these as 'input-related':"
    print *, "   --source, --import, --gcov-executable, --gcov-args"
    print *, ""
    print *, "All other flags are considered 'not input-related', so:"
    print *, "   fortcov --output=test.json   → zero-config mode (WRONG!)"
    print *, "   fortcov --format=json        → zero-config mode (WRONG!)"
    print *, "   fortcov --verbose            → zero-config mode (WRONG!)"
    print *, "   fortcov --threshold=95       → zero-config mode (WRONG!)"
    print *, ""
    print *, "Zero-config mode calls apply_zero_configuration_defaults()"
    print *, "which overwrites the correctly parsed CLI flag values."
    print *, ""
    print *, "FIX: Modify should_use_zero_config() logic to only trigger"
    print *, "zero-config when NO arguments are provided, not when"
    print *, "arguments exist but are 'not input-related'."
    
end program test_zero_config_override_bug
