program test_cli_flag_parsing_issue_472
    !! Comprehensive test for issue #472 - CLI argument parsing with --source flag
    !! 
    !! Tests both --source value and --source=value syntax to ensure compatibility
    !! with documented examples
    use config_types, only: config_t
    use config_core, only: parse_config
    use test_framework_utilities
    implicit none
    
    type(test_counter_t) :: counter
    
    print *, "========================================="
    print *, "Test: CLI Flag Parsing Issue #472"
    print *, "========================================="
    print *, ""
    
    call init_test_counter(counter)
    
    ! Test space-separated syntax (used in examples)
    call test_source_flag_space_syntax(counter)
    
    ! Test equals syntax (alternative format)
    call test_source_flag_equals_syntax(counter)
    
    ! Test short form syntax
    call test_source_flag_short_form(counter)
    
    ! Test complex command from examples
    call test_complex_command_from_examples(counter)
    
    ! Test multiple source directories
    call test_multiple_source_directories(counter)
    
    ! Test edge cases
    call test_edge_cases(counter)
    
    call print_test_summary(counter, "CLI Flag Parsing Issue #472")
    
    if (counter%failed > 0) then
        call exit(1)
    else
        call exit(0)
    end if
    
contains
    
    subroutine test_source_flag_space_syntax(counter)
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        print *, "Test: --source . syntax (space-separated, used in examples)"
        
        allocate(args(2))
        args(1) = "--source"
        args(2) = "."
        
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            call increment_fail(counter)
            print *, "  failed to parse"
            print *, "    Error: ", trim(error_message)
        else if (.not. allocated(config%source_paths)) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: --source . parsed but no source paths in config"
        else if (size(config%source_paths) == 0) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: --source . parsed but source paths array is empty"
        else if (trim(config%source_paths(1)) /= ".") then
            call increment_fail(counter)
            print *, "  ❌ FAIL: --source . parsed but source path is incorrect"
            print *, "    Expected: '.', Got: '", trim(config%source_paths(1)), "'"
        else
            call increment_pass(counter)
            print *, "  ✅ PASS: --source . syntax works correctly"
        end if
        
        deallocate(args)
    end subroutine test_source_flag_space_syntax
    
    subroutine test_source_flag_equals_syntax(counter)
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        print *, "Test: --source=. syntax (equals format)"
        
        allocate(args(1))
        args(1) = "--source=."
        
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            call increment_fail(counter)
            print *, "  failed to parse"
            print *, "    Error: ", trim(error_message)
        else if (.not. allocated(config%source_paths)) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: --source=. parsed but no source paths in config"
        else if (size(config%source_paths) == 0) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: --source=. parsed but source paths array is empty"
        else if (trim(config%source_paths(1)) /= ".") then
            call increment_fail(counter)
            print *, "  ❌ FAIL: --source=. parsed but source path is incorrect"
            print *, "    Expected: '.', Got: '", trim(config%source_paths(1)), "'"
        else
            call increment_pass(counter)
            print *, "  ✅ PASS: --source=. syntax works correctly"
        end if
        
        deallocate(args)
    end subroutine test_source_flag_equals_syntax
    
    subroutine test_source_flag_short_form(counter)
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        print *, "Test: -s . syntax (short form)"
        
        allocate(args(2))
        args(1) = "-s"
        args(2) = "."
        
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            call increment_fail(counter)
            print *, "  failed to parse"
            print *, "    Error: ", trim(error_message)
        else if (.not. allocated(config%source_paths)) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: -s . parsed but no source paths in config"
        else if (size(config%source_paths) == 0) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: -s . parsed but source paths array is empty"
        else if (trim(config%source_paths(1)) /= ".") then
            call increment_fail(counter)
            print *, "  ❌ FAIL: -s . parsed but source path is incorrect"
            print *, "    Expected: '.', Got: '", trim(config%source_paths(1)), "'"
        else
            call increment_pass(counter)
            print *, "  ✅ PASS: -s . syntax works correctly"
        end if
        
        deallocate(args)
    end subroutine test_source_flag_short_form
    
    subroutine test_complex_command_from_examples(counter)
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        print *, "Test: Complex command from examples"
        print *, "      --source . --exclude build/* --exclude test/*"
        
        allocate(args(6))
        args(1) = "--source"
        args(2) = "."
        args(3) = "--exclude"
        args(4) = "build/*"
        args(5) = "--exclude"
        args(6) = "test/*"
        
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            call increment_fail(counter)
            print *, "  failed to parse"
            print *, "    Error: ", trim(error_message)
        else if (.not. allocated(config%source_paths) .or. &
                 size(config%source_paths) == 0 .or. &
                 trim(config%source_paths(1)) /= ".") then
            call increment_fail(counter)
            print *, "  ❌ FAIL: Complex command parsed but source path incorrect"
        else if (.not. allocated(config%exclude_patterns) .or. &
                 size(config%exclude_patterns) < 2) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: Complex command parsed but exclude patterns missing"
        else if (trim(config%exclude_patterns(1)) /= "build/*" .or. &
                 trim(config%exclude_patterns(2)) /= "test/*") then
            call increment_fail(counter)
            print *, "  ❌ FAIL: Complex command parsed but exclude patterns incorrect"
        else
            call increment_pass(counter)
            print *, "  ✅ PASS: Complex command works correctly"
        end if
        
        deallocate(args)
    end subroutine test_complex_command_from_examples
    
    subroutine test_multiple_source_directories(counter)
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        print *, "Test: Multiple source directories"
        print *, "      --source src --source lib"
        
        allocate(args(4))
        args(1) = "--source"
        args(2) = "src"
        args(3) = "--source"
        args(4) = "lib"
        
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            call increment_fail(counter)
            print *, "  failed to parse"
            print *, "    Error: ", trim(error_message)
        else if (.not. allocated(config%source_paths) .or. &
                 size(config%source_paths) < 2) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: Multiple source directories parsed but &
                     &array size incorrect"
        else if (trim(config%source_paths(1)) /= "src" .or. &
                 trim(config%source_paths(2)) /= "lib") then
            call increment_fail(counter)
            print *, "  ❌ FAIL: Multiple source directories parsed but paths incorrect"
            print *, "    Expected: 'src', 'lib'"
            print *, "    Got: '", trim(config%source_paths(1)), "', '", &
                     trim(config%source_paths(2)), "'"
        else
            call increment_pass(counter)
            print *, "  ✅ PASS: Multiple source directories work correctly"
        end if
        
        deallocate(args)
    end subroutine test_multiple_source_directories
    
    subroutine test_edge_cases(counter)
        type(test_counter_t), intent(inout) :: counter
        
        ! Test missing value for --source
        call test_missing_source_value(counter)
        
        ! Test empty string as source value
        call test_empty_source_value(counter)
        
        ! Test source with spaces in path
        call test_source_with_spaces(counter)
    end subroutine test_edge_cases
    
    subroutine test_missing_source_value(counter)
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        print *, "Test: Missing value for --source (should fail)"
        
        allocate(args(1))
        args(1) = "--source"
        
        call parse_config(args, config, success, error_message)
        
        if (success) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: --source without value should fail but succeeded"
        else
            call increment_pass(counter)
            print *, "  ✅ PASS: --source without value correctly failed"
        end if
        
        deallocate(args)
    end subroutine test_missing_source_value
    
    subroutine test_empty_source_value(counter)
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        print *, "Test: Empty string as source value"
        
        allocate(args(2))
        args(1) = "--source"
        args(2) = ""
        
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            call increment_pass(counter)
            print *, "  ✅ PASS: Empty source value correctly handled"
        else if (.not. allocated(config%source_paths) .or. &
                 size(config%source_paths) == 0) then
            call increment_pass(counter)
            print *, "  ✅ PASS: Empty source value resulted in no source paths"
        else
            call increment_fail(counter)
            print *, "  ❌ FAIL: Empty source value should not create valid path"
        end if
        
        deallocate(args)
    end subroutine test_empty_source_value
    
    subroutine test_source_with_spaces(counter)
        type(test_counter_t), intent(inout) :: counter
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        print *, "Test: Source path with spaces"
        
        allocate(args(2))
        args(1) = "--source"
        args(2) = "path with spaces"
        
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            call increment_fail(counter)
            print *, "  failed to parse"
            print *, "    Error: ", trim(error_message)
        else if (.not. allocated(config%source_paths) .or. &
                 size(config%source_paths) == 0) then
            call increment_fail(counter)
            print *, "  ❌ FAIL: Source with spaces parsed but no paths in config"
        else if (trim(config%source_paths(1)) /= "path with spaces") then
            call increment_fail(counter)
            print *, "  ❌ FAIL: Source with spaces parsed but path incorrect"
            print *, "    Expected: 'path with spaces'"
            print *, "    Got: '", trim(config%source_paths(1)), "'"
        else
            call increment_pass(counter)
            print *, "  ✅ PASS: Source path with spaces works correctly"
        end if
        
        deallocate(args)
    end subroutine test_source_with_spaces
    
end program test_cli_flag_parsing_issue_472
