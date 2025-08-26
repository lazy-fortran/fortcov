program test_security_command_injection_issue_235
    !! 
    !! Given-When-Then Test Documentation:
    !! 
    !! GIVEN: A fortcov installation with command injection vulnerabilities in gcov_command_executor
    !! WHEN: Malicious input is provided through various attack vectors
    !! THEN: The system should prevent command injection attacks (currently fails - proves vulnerabilities exist)
    !! 
    !! This test suite demonstrates Issue #235: Command injection vulnerabilities in secure_command_executor
    !! Current status: ALL tests should FAIL proving vulnerabilities exist
    !! Expected post-fix: ALL tests should PASS showing injection attacks are prevented
    !!
    !! SECURITY TEST FRAMEWORK:
    !! - Tests demonstrate actual command injection vulnerabilities  
    !! - All tests designed to FAIL initially (RED phase of TDD)
    !! - Each test proves specific attack vector works against current code
    !! - Tests will PASS after security fixes are implemented
    !!
    !! DECOMPOSED: Security test logic extracted to focused modules for architecture 
    !! size compliance (<500 lines per file)
    !!
    use test_framework_utilities
    use test_environment_utilities  
    use test_file_utilities
    use test_shell_metacharacter_injection
    use test_path_traversal_attacks
    use test_command_execution_vulnerabilities
    implicit none
    
    type(test_counter_t) :: test_counter
    
    print *, "=================================================================="
    print *, "SECURITY: Command Injection Vulnerability Test Suite (Issue #235)"
    print *, "=================================================================="
    print *, ""
    print *, "Testing command injection vulnerabilities that currently exist."
    print *, "Issue: Unescaped command execution allows arbitrary code execution"
    print *, "Expected: ALL TESTS FAIL (proving vulnerabilities exist)"
    print *, ""
    
    ! Initialize test framework
    call init_test_counter(test_counter)
    
    ! Setup test environment
    call setup_security_test_environment()
    
    ! Test shell metacharacter injection attacks
    call test_semicolon_command_injection(test_counter)
    call test_ampersand_command_injection(test_counter)
    call test_pipe_command_injection(test_counter)
    call test_backtick_command_substitution(test_counter)
    call test_dollar_variable_injection(test_counter)
    call test_redirect_output_injection(test_counter)
    call test_redirect_input_injection(test_counter)
    call test_double_quote_injection(test_counter)
    
    ! Test directory traversal attacks
    call test_directory_traversal_attack(test_counter)
    call test_url_encoded_traversal_attack(test_counter)
    call test_null_byte_injection(test_counter)
    call test_system_file_access_attack(test_counter)
    call test_windows_device_attack(test_counter)
    call test_unc_path_attack(test_counter)
    
    ! Test command execution vulnerabilities
    call test_mkdir_command_injection_vulnerability(test_counter)
    call test_mv_command_injection_vulnerability(test_counter)
    call test_which_command_partial_escaping_vulnerability(test_counter)
    call test_error_message_path_leakage(test_counter)
    call test_command_echo_in_errors(test_counter)
    
    ! Cleanup test environment
    call cleanup_security_test_environment()
    
    ! Report results
    call print_test_summary(test_counter, "Security Command Injection Issue #235")
    
contains

    subroutine setup_security_test_environment()
        !! Setup security test environment with legitimate test files
        call setup_basic_test_environment("security")
        
        ! Create legitimate test files first
        call create_test_source_file("legitimate.f90")
        call create_test_gcov_file("legitimate.f90.gcov")
        
        ! Create security test output directory
        call execute_command_line("mkdir -p security_test_output")
        call execute_command_line("mkdir -p test_safe_dir")
    end subroutine setup_security_test_environment
    
    subroutine cleanup_security_test_environment()
        !! Cleanup security test environment
        call cleanup_basic_test_environment("security")
        
        ! Remove security-specific test files
        character(len=*), parameter :: security_patterns(3) = [ &
            "legitimate.f90 legitimate.f90.gcov", &
            "HACKED_* injection_test_*.tmp     ", &
            "malicious_input                   " &
        ]
        
        call cleanup_test_files_pattern(security_patterns)
        call execute_command_line("rm -rf security_test_output test_safe_dir")
    end subroutine cleanup_security_test_environment

end program test_security_command_injection_issue_235