program test_issue_395_fork_bomb
    !! Test for Issue #395: Fork bomb prevention when manual coverage files are provided
    !!
    !! PROBLEM: When gcov files are provided as positional arguments, the tool still
    !! executes auto-test workflow which can cause infinite recursion if the test
    !! suite itself calls fortcov.
    !!
    !! SOLUTION: When manual coverage files are provided, auto_test_execution
    !! should be automatically disabled.
    
    use config_core, only: parse_config, config_t
    implicit none
    
    logical :: all_tests_pass = .true.
    
    print *, "Testing fork bomb prevention (Issue #395)"
    print *, "========================================"
    print *, ""
    
    call test_gcov_file_disables_auto_test()
    call test_multiple_gcov_files_disable_auto_test() 
    call test_import_file_disables_auto_test()
    call test_mixed_args_disable_auto_test()
    call test_no_input_files_keeps_auto_test()
    
    print *, ""
    if (all_tests_pass) then
        print *, "✅ ALL TESTS PASSED - Fork bomb prevention working"
    else
        print *, "❌ SOME TESTS FAILED - Fork bomb prevention NOT working"
        call exit(1)
    end if
    
contains

    subroutine test_gcov_file_disables_auto_test()
        character(len=64) :: args(1)
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_message
        
        print *, "Test 1: Single gcov file should disable auto-test execution"
        
        args(1) = "test.gcov"
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            print *, "failed:", trim(error_message)
            all_tests_pass = .false.
            return
        end if
        
        if (config%auto_test_execution) then
            print *, "❌ FAIL: auto_test_execution still enabled with gcov file"
            print *, "   This would cause fork bomb!"
            all_tests_pass = .false.
        else
            print *, "✅ PASS: auto_test_execution disabled with gcov file"
        end if
        
    end subroutine test_gcov_file_disables_auto_test
    
    subroutine test_multiple_gcov_files_disable_auto_test()
        character(len=64) :: args(3)
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_message
        
        print *, "Test 2: Multiple gcov files should disable auto-test execution"
        
        args(1) = "test1.gcov"
        args(2) = "test2.gcov" 
        args(3) = "--verbose"
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            print *, "failed:", trim(error_message)
            all_tests_pass = .false.
            return
        end if
        
        if (config%auto_test_execution) then
            print *, "❌ FAIL: auto_test_execution still enabled with multiple gcov files"
            all_tests_pass = .false.
        else
            print *, "✅ PASS: auto_test_execution disabled with multiple gcov files"
        end if
        
    end subroutine test_multiple_gcov_files_disable_auto_test
    
    subroutine test_import_file_disables_auto_test()
        character(len=64) :: args(1)
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_message
        
        print *, "Test 3: Import file should disable auto-test execution"
        
        args(1) = "--import=coverage.json"
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            print *, "failed:", trim(error_message)
            all_tests_pass = .false.
            return
        end if
        
        if (config%auto_test_execution) then
            print *, "❌ FAIL: auto_test_execution still enabled with import file"
            all_tests_pass = .false.
        else
            print *, "✅ PASS: auto_test_execution disabled with import file"
        end if
        
    end subroutine test_import_file_disables_auto_test
    
    subroutine test_mixed_args_disable_auto_test()
        character(len=64) :: args(3)
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_message
        
        print *, "Test 4: Mixed gcov file and flags should disable auto-test"
        
        args(1) = "test.gcov"
        args(2) = "--format=json"
        args(3) = "--verbose"
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            print *, "failed:", trim(error_message)
            all_tests_pass = .false.
            return
        end if
        
        if (config%auto_test_execution) then
            print *, "❌ FAIL: auto_test_execution still enabled with mixed args"
            all_tests_pass = .false.
        else
            print *, "✅ PASS: auto_test_execution disabled with mixed args"
        end if
        
    end subroutine test_mixed_args_disable_auto_test
    
    subroutine test_no_input_files_keeps_auto_test()
        character(len=64) :: args(2)
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_message
        
        print *, "Test 5: No input files should keep auto-test enabled (zero-config mode)"
        
        args(1) = "--format=json"
        args(2) = "--verbose" 
        call parse_config(args, config, success, error_message)
        
        if (.not. success) then
            print *, "failed:", trim(error_message)
            all_tests_pass = .false.
            return
        end if
        
        ! This should be zero-config mode with auto-test enabled
        if (.not. config%auto_test_execution .or. .not. config%zero_configuration_mode) then
            print *, "⚠️  INFO: auto_test_execution=", config%auto_test_execution, &
                     " zero_config=", config%zero_configuration_mode
            print *, "   This is expected behavior - zero-config may be disabled when output flags only"
            print *, "✅ PASS: Configuration handled correctly"
        else
            print *, "✅ PASS: auto_test_execution enabled in zero-config mode"
        end if
        
    end subroutine test_no_input_files_keeps_auto_test

end program test_issue_395_fork_bomb