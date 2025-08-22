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
    use gcov_command_executor
    use secure_command_executor  
    use error_handling
    use file_utils
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: test_count = 0
    integer :: failed_count = 0
    logical :: test_result
    
    print *, "=================================================================="
    print *, "SECURITY: Command Injection Vulnerability Test Suite (Issue #235)"
    print *, "=================================================================="
    print *, ""
    print *, "Testing command injection vulnerabilities that currently exist."
    print *, "Issue: Unescaped command execution allows arbitrary code execution"
    print *, "Expected: ALL TESTS FAIL (proving vulnerabilities exist)"
    print *, ""
    
    ! Setup test environment
    call setup_security_test_environment()
    
    ! Test shell metacharacter injection attacks
    call test_semicolon_command_injection()
    call test_ampersand_command_injection()  
    call test_pipe_command_injection()
    call test_backtick_command_substitution()
    call test_dollar_variable_injection()
    call test_redirect_output_injection()
    call test_redirect_input_injection()
    call test_double_quote_injection()
    
    ! Test directory traversal attacks
    call test_directory_traversal_attack()
    call test_url_encoded_traversal_attack()
    call test_null_byte_injection()
    
    ! Test dangerous path injection
    call test_system_file_access_attack()
    call test_windows_device_attack()
    call test_unc_path_attack()
    
    ! Test specific vulnerability points
    call test_mkdir_command_injection_vulnerability()
    call test_mv_command_injection_vulnerability()
    call test_which_command_partial_escaping_vulnerability()
    
    ! Test error message information leakage
    call test_error_message_path_leakage()
    call test_command_echo_in_errors()
    
    ! Cleanup test environment
    call cleanup_security_test_environment()
    
    ! Report results
    print *, ""
    print *, "=================================================================="
    print *, "Security Test Results Summary"
    print *, "=================================================================="
    write(*, '(A, I0, A, I0, A)') "Tests run: ", test_count, ", Failed: ", failed_count, &
        " (expected to fail until vulnerabilities are fixed)"
    
    if (failed_count == test_count) then
        print *, ""
        print *, "🚨 CRITICAL SECURITY VULNERABILITIES CONFIRMED"
        print *, "   All command injection tests failed as expected"
        print *, ""
        print *, "Vulnerability analysis:"
        print *, "• gcov_command_executor.f90:76-77 - Unescaped mkdir command"
        print *, "• gcov_command_executor.f90:118-119 - Unescaped mv command"  
        print *, "• secure_command_executor.f90:417 - Partial escaping in which command"
        print *, "• Direct string concatenation enables shell injection attacks"
        print *, "• Attack vectors: ; & | ` $ > < "" path traversal system files"
        print *, ""
        print *, "IMMEDIATE ACTION REQUIRED: Implement security fixes"
    else
        print *, ""
        print *, "⚠️  UNEXPECTED TEST RESULTS - Some vulnerabilities may be fixed"
        print *, "   This indicates partial security improvements or different issues"
        write(*, '(A, I0, A, I0, A)') "   ", (test_count - failed_count), " tests passed unexpectedly out of ", test_count, " total"
    end if
    
contains

    subroutine setup_security_test_environment()
        !! 
        !! Given-When-Then: Setup security test environment
        !! 
        !! GIVEN: A clean test environment for security testing
        !! WHEN: Malicious test files and directories are created
        !! THEN: Security tests can run against realistic attack scenarios
        !!
        logical :: dir_exists
        
        print *, "Setting up security test environment..."
        
        ! Create legitimate test files first
        call create_test_source_file("legitimate.f90")
        call create_test_gcov_file("legitimate.f90.gcov")
        
        ! Create test directories for attack scenarios
        inquire(file="security_test_output", exist=dir_exists)
        if (.not. dir_exists) then
            call execute_command_line("mkdir -p security_test_output")
        end if
        
        ! Create a safe directory for testing directory operations
        inquire(file="test_safe_dir", exist=dir_exists) 
        if (.not. dir_exists) then
            call execute_command_line("mkdir -p test_safe_dir")
        end if
        
        print *, "✓ Security test environment ready"
        print *, ""
    end subroutine setup_security_test_environment
    
    subroutine cleanup_security_test_environment()
        !!
        !! Given-When-Then: Cleanup security test environment
        !! 
        !! GIVEN: Security test files and directories exist
        !! WHEN: Tests are complete
        !! THEN: Test artifacts are safely cleaned up
        !!
        print *, "Cleaning up security test environment..."
        
        ! Remove test files safely
        call execute_command_line("rm -f legitimate.f90 legitimate.f90.gcov")
        call execute_command_line("rm -rf security_test_output test_safe_dir")
        call execute_command_line("rm -f injection_test_*.tmp")
        call execute_command_line("rm -f vulnerability_proof.txt")
        
        print *, "✓ Security test environment cleaned"
    end subroutine cleanup_security_test_environment

    subroutine test_semicolon_command_injection()
        !! 
        !! Given-When-Then: Test semicolon command injection
        !! 
        !! GIVEN: gcov_command_executor with unescaped mkdir command
        !! WHEN: gcov_output_dir contains "; touch vulnerability_proof.txt"
        !! THEN: Command injection should be prevented (currently fails - creates file)
        !!
        type(gcov_executor_t) :: executor
        character(len=:), allocatable :: malicious_dir
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        logical :: vulnerability_file_exists
        
        call start_test("Semicolon Command Injection Attack")
        
        ! Create malicious directory path with command injection
        malicious_dir = "test_safe_dir; touch vulnerability_proof.txt; echo pwned"
        
        ! Configure executor with malicious output directory
        call executor%set_gcov_output_directory(malicious_dir)
        
        ! Attempt to execute gcov with malicious directory
        ! This will trigger the vulnerable mkdir command at line 76-77
        call executor%execute_gcov("legitimate.f90", gcov_files, error_ctx)
        
        ! Check if command injection succeeded (proves vulnerability)
        inquire(file="vulnerability_proof.txt", exist=vulnerability_file_exists)
        
        if (vulnerability_file_exists) then
            call fail_test("CRITICAL VULNERABILITY: Semicolon injection succeeded - arbitrary commands executed")
            ! Clean up evidence
            call execute_command_line("rm -f vulnerability_proof.txt")
        else
            call pass_test("Semicolon injection prevented - command execution secured")
        end if
        
    end subroutine test_semicolon_command_injection
    
    subroutine test_ampersand_command_injection()
        !! 
        !! Given-When-Then: Test ampersand command injection  
        !! 
        !! GIVEN: gcov_command_executor with unescaped mkdir command
        !! WHEN: gcov_output_dir contains "& echo injection_successful"
        !! THEN: Background command execution should be prevented
        !!
        type(gcov_executor_t) :: executor
        character(len=:), allocatable :: malicious_dir
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        logical :: vulnerability_exists
        
        call start_test("Ampersand Background Command Injection")
        
        ! Create malicious directory with background command injection
        malicious_dir = "test_safe_dir & echo injection_successful > injection_test_amp.tmp"
        
        call executor%set_gcov_output_directory(malicious_dir)
        call executor%execute_gcov("legitimate.f90", gcov_files, error_ctx)
        
        ! Check for evidence of command execution
        inquire(file="injection_test_amp.tmp", exist=vulnerability_exists)
        
        if (vulnerability_exists) then
            call fail_test("CRITICAL VULNERABILITY: Ampersand injection succeeded - background command executed")
            call execute_command_line("rm -f injection_test_amp.tmp")
        else
            call pass_test("Ampersand injection prevented")
        end if
        
    end subroutine test_ampersand_command_injection
    
    subroutine test_pipe_command_injection()
        !! 
        !! Given-When-Then: Test pipe command injection
        !! 
        !! GIVEN: gcov_command_executor with unescaped mv command
        !! WHEN: File path contains "| echo piped_command"
        !! THEN: Pipe command execution should be prevented
        !!
        type(gcov_executor_t) :: executor  
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        logical :: vulnerability_exists
        
        call start_test("Pipe Command Injection Attack")
        
        ! Create a source file that will be moved with pipe injection
        call create_malicious_gcov_file("source.f90.gcov| echo pipe_injection > injection_test_pipe.tmp")
        
        call executor%set_gcov_output_directory("security_test_output")
        
        ! This will trigger the vulnerable mv command at line 118-119
        call executor%execute_gcov("source.f90", gcov_files, error_ctx)
        
        inquire(file="injection_test_pipe.tmp", exist=vulnerability_exists)
        
        if (vulnerability_exists) then
            call fail_test("CRITICAL VULNERABILITY: Pipe injection succeeded")
            call execute_command_line("rm -f injection_test_pipe.tmp")
        else
            call pass_test("Pipe injection prevented")
        end if
        
        ! Clean up malicious file
        call execute_command_line("rm -f 'source.f90.gcov| echo pipe_injection > injection_test_pipe.tmp'")
        
    end subroutine test_pipe_command_injection
    
    subroutine test_backtick_command_substitution()
        !! 
        !! Given-When-Then: Test backtick command substitution
        !! 
        !! GIVEN: secure_command_executor with command construction vulnerabilities
        !! WHEN: Path contains backtick command substitution
        !! THEN: Command substitution should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        logical :: vulnerability_exists
        
        call start_test("Backtick Command Substitution Attack")
        
        ! Test backtick injection in path validation
        malicious_path = "safe_dir`touch injection_test_backtick.tmp`"
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        ! Check if command substitution was executed during validation
        inquire(file="injection_test_backtick.tmp", exist=vulnerability_exists)
        
        if (vulnerability_exists) then
            call fail_test("CRITICAL VULNERABILITY: Backtick command substitution succeeded")
            call execute_command_line("rm -f injection_test_backtick.tmp")
        else if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Backtick injection properly blocked")
        else
            call fail_test("Backtick validation unclear - may be vulnerable")
        end if
        
    end subroutine test_backtick_command_substitution
    
    subroutine test_dollar_variable_injection()
        !! 
        !! Given-When-Then: Test dollar variable injection
        !! 
        !! GIVEN: Command execution with unescaped variables
        !! WHEN: Path contains $() or ${} variable expansion
        !! THEN: Variable expansion should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        logical :: vulnerability_exists
        
        call start_test("Dollar Variable Injection Attack")
        
        ! Test dollar variable expansion
        malicious_path = "test_dir$(touch injection_test_dollar.tmp)"
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        inquire(file="injection_test_dollar.tmp", exist=vulnerability_exists)
        
        if (vulnerability_exists) then
            call fail_test("CRITICAL VULNERABILITY: Dollar variable injection succeeded")  
            call execute_command_line("rm -f injection_test_dollar.tmp")
        else if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Dollar injection properly blocked")
        else
            call fail_test("Dollar validation unclear - may be vulnerable")
        end if
        
    end subroutine test_dollar_variable_injection
    
    subroutine test_redirect_output_injection()
        !! 
        !! Given-When-Then: Test output redirection injection
        !! 
        !! GIVEN: Command execution with unescaped redirection
        !! WHEN: Path contains "> /tmp/malicious_output"
        !! THEN: Output redirection should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        logical :: vulnerability_exists
        
        call start_test("Output Redirection Injection Attack")
        
        malicious_path = "test_dir > injection_test_redirect.tmp"
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        ! For redirection, we can't easily test file creation, but we can test blocking
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Output redirection properly blocked")
        else
            call fail_test("VULNERABILITY: Output redirection not blocked in path validation")
        end if
        
    end subroutine test_redirect_output_injection
    
    subroutine test_redirect_input_injection()
        !! 
        !! Given-When-Then: Test input redirection injection
        !! 
        !! GIVEN: Command execution with unescaped input redirection
        !! WHEN: Path contains "< /etc/passwd"
        !! THEN: Input redirection should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        
        call start_test("Input Redirection Injection Attack")
        
        malicious_path = "test_dir < /etc/passwd"
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Input redirection properly blocked")
        else
            call fail_test("VULNERABILITY: Input redirection not blocked in path validation")
        end if
        
    end subroutine test_redirect_input_injection
    
    subroutine test_double_quote_injection()
        !! 
        !! Given-When-Then: Test double quote injection
        !! 
        !! GIVEN: Command construction using string concatenation
        !! WHEN: Path contains double quotes to break out of arguments
        !! THEN: Quote injection should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        
        call start_test('Double Quote Injection Attack')
        
        malicious_path = 'test_dir" && touch injection_test_quote.tmp && echo "'
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Double quote injection properly blocked")
        else
            call fail_test("VULNERABILITY: Double quote not blocked in path validation")
        end if
        
    end subroutine test_double_quote_injection
    
    subroutine test_directory_traversal_attack()
        !! 
        !! Given-When-Then: Test directory traversal attack
        !! 
        !! GIVEN: Path validation with traversal protection
        !! WHEN: Path contains "../../../etc/passwd"
        !! THEN: Directory traversal should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        
        call start_test("Directory Traversal Attack")
        
        malicious_path = "../../../etc/passwd"
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Directory traversal properly blocked")
        else
            call fail_test("VULNERABILITY: Directory traversal not blocked")
        end if
        
    end subroutine test_directory_traversal_attack
    
    subroutine test_url_encoded_traversal_attack()
        !! 
        !! Given-When-Then: Test URL-encoded traversal attack
        !! 
        !! GIVEN: Path validation with URL decoding
        !! WHEN: Path contains "%2e%2e%2f" (URL-encoded ../)  
        !! THEN: Encoded traversal should be detected and prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        
        call start_test("URL-Encoded Directory Traversal Attack")
        
        malicious_path = "%2e%2e%2f%2e%2e%2fetc%2fpasswd"
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("URL-encoded traversal properly blocked")
        else
            call fail_test("VULNERABILITY: URL-encoded traversal not blocked")
        end if
        
    end subroutine test_url_encoded_traversal_attack
    
    subroutine test_null_byte_injection()
        !! 
        !! Given-When-Then: Test null byte injection
        !! 
        !! GIVEN: Path validation with null byte detection
        !! WHEN: Path contains null bytes to truncate validation
        !! THEN: Null byte injection should be detected and prevented  
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        
        call start_test("Null Byte Injection Attack")
        
        ! Construct path with null byte
        malicious_path = "safe_dir" // char(0) // "/../../../etc/passwd"
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("Null byte injection properly blocked")
        else
            call fail_test("VULNERABILITY: Null byte injection not blocked")  
        end if
        
    end subroutine test_null_byte_injection
    
    subroutine test_system_file_access_attack()
        !! 
        !! Given-When-Then: Test system file access attack
        !! 
        !! GIVEN: Path validation with system path protection
        !! WHEN: Path targets sensitive system directories
        !! THEN: System file access should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_paths(4)
        integer :: i
        logical :: all_blocked
        
        call start_test("System File Access Attack")
        
        malicious_paths(1) = "/etc/passwd"
        malicious_paths(2) = "/proc/version"
        malicious_paths(3) = "/sys/kernel" 
        malicious_paths(4) = "/dev/null"
        
        all_blocked = .true.
        
        do i = 1, 4
            call validate_path_security(malicious_paths(i), safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                all_blocked = .false.
                exit
            end if
        end do
        
        if (all_blocked) then
            call pass_test("System file access properly blocked")
        else
            call fail_test("VULNERABILITY: System file access not fully blocked")
        end if
        
    end subroutine test_system_file_access_attack
    
    subroutine test_windows_device_attack()
        !! 
        !! Given-When-Then: Test Windows device name attack
        !! 
        !! GIVEN: Path validation with Windows device protection  
        !! WHEN: Path contains Windows device names (CON, PRN, NUL, etc.)
        !! THEN: Device access should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: device_names(6)
        integer :: i
        logical :: all_blocked
        
        call start_test("Windows Device Name Attack")
        
        device_names(1) = "CON"
        device_names(2) = "PRN"
        device_names(3) = "NUL"
        device_names(4) = "COM1"
        device_names(5) = "LPT1"
        device_names(6) = "AUX"
        
        all_blocked = .true.
        
        do i = 1, 6
            call validate_path_security(device_names(i), safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                all_blocked = .false.
                exit
            end if
        end do
        
        if (all_blocked) then
            call pass_test("Windows device names properly blocked")
        else
            call fail_test("VULNERABILITY: Windows device access not fully blocked")
        end if
        
    end subroutine test_windows_device_attack
    
    subroutine test_unc_path_attack()
        !! 
        !! Given-When-Then: Test UNC path attack
        !! 
        !! GIVEN: Path validation with UNC path protection
        !! WHEN: Path contains Windows network paths (\\server\share)
        !! THEN: UNC path access should be prevented
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_path
        
        call start_test("UNC Path Attack")
        
        malicious_path = "\\\\malicious-server\\admin$\\system32"
        
        call validate_path_security(malicious_path, safe_path, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("UNC path properly blocked")
        else
            call fail_test("VULNERABILITY: UNC path not blocked")
        end if
        
    end subroutine test_unc_path_attack
    
    subroutine test_mkdir_command_injection_vulnerability()
        !! 
        !! Given-When-Then: Test specific mkdir vulnerability
        !! 
        !! GIVEN: gcov_command_executor.f90 line 76-77 unescaped mkdir command
        !! WHEN: Malicious gcov_output_dir is used in mkdir
        !! THEN: Command injection through mkdir should be prevented
        !!
        type(gcov_executor_t) :: executor
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        logical :: vulnerability_exists
        
        call start_test("SPECIFIC: mkdir Command Injection (line 76-77)")
        
        ! Test the exact vulnerability at line 76-77
        call executor%set_gcov_output_directory("test_safe_dir; echo mkdir_injection > injection_test_mkdir.tmp")
        call executor%execute_gcov("legitimate.f90", gcov_files, error_ctx)
        
        inquire(file="injection_test_mkdir.tmp", exist=vulnerability_exists)
        
        if (vulnerability_exists) then
            call fail_test("CRITICAL: Line 76-77 mkdir injection vulnerability CONFIRMED")
            call execute_command_line("rm -f injection_test_mkdir.tmp")
        else
            call pass_test("mkdir command injection prevented")
        end if
        
    end subroutine test_mkdir_command_injection_vulnerability
    
    subroutine test_mv_command_injection_vulnerability()
        !! 
        !! Given-When-Then: Test specific mv vulnerability  
        !! 
        !! GIVEN: gcov_command_executor.f90 line 118-119 unescaped mv command
        !! WHEN: Malicious file paths are used in mv command
        !! THEN: Command injection through mv should be prevented
        !!
        type(gcov_executor_t) :: executor
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        logical :: vulnerability_exists
        
        call start_test("SPECIFIC: mv Command Injection (line 118-119)")
        
        ! Create malicious source file that will trigger mv vulnerability
        call create_malicious_gcov_file("malicious.f90.gcov; echo mv_injection > injection_test_mv.tmp; true")
        
        call executor%set_gcov_output_directory("security_test_output")
        call executor%execute_gcov("malicious.f90", gcov_files, error_ctx)
        
        inquire(file="injection_test_mv.tmp", exist=vulnerability_exists)
        
        if (vulnerability_exists) then
            call fail_test("CRITICAL: Line 118-119 mv injection vulnerability CONFIRMED")
            call execute_command_line("rm -f injection_test_mv.tmp")
        else  
            call pass_test("mv command injection prevented")
        end if
        
        ! Clean up malicious file
        call execute_command_line("rm -f 'malicious.f90.gcov; echo mv_injection > injection_test_mv.tmp; true'")
        
    end subroutine test_mv_command_injection_vulnerability
    
    subroutine test_which_command_partial_escaping_vulnerability()
        !! 
        !! Given-When-Then: Test partial escaping vulnerability
        !! 
        !! GIVEN: secure_command_executor.f90 line 417 partial escaping in which command
        !! WHEN: Executable validation with redirection manipulation  
        !! THEN: Redirection manipulation should be prevented
        !!
        character(len=:), allocatable :: safe_executable
        type(error_context_t) :: error_ctx
        character(len=256) :: malicious_executable
        logical :: vulnerability_exists
        
        call start_test("SPECIFIC: which Command Partial Escaping (line 417)")
        
        ! Test the partial escaping vulnerability in which command
        ! Even though executable is escaped, redirection is not
        malicious_executable = "gcov; echo which_injection > injection_test_which.tmp"
        
        call validate_executable_path(malicious_executable, safe_executable, error_ctx)
        
        inquire(file="injection_test_which.tmp", exist=vulnerability_exists)
        
        if (vulnerability_exists) then
            call fail_test("CRITICAL: Line 417 which command vulnerability CONFIRMED")
            call execute_command_line("rm -f injection_test_which.tmp")
        else if (error_ctx%error_code /= ERROR_SUCCESS) then
            call pass_test("which command injection prevented")
        else
            call fail_test("which command validation unclear - may be vulnerable")
        end if
        
    end subroutine test_which_command_partial_escaping_vulnerability
    
    subroutine test_error_message_path_leakage()
        !! 
        !! Given-When-Then: Test error message information leakage
        !! 
        !! GIVEN: Error handling that may leak sensitive information
        !! WHEN: Security violations generate error messages
        !! THEN: Error messages should not leak sensitive path information
        !!
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=256) :: sensitive_path
        
        call start_test("Error Message Information Leakage")
        
        ! Test with sensitive path information
        sensitive_path = "/home/user/.ssh/id_rsa"
        
        call validate_path_security(sensitive_path, safe_path, error_ctx)
        
        ! Check if error message contains sensitive information
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (len_trim(error_ctx%message) > 0) then
                if (index(error_ctx%message, "/home/user/.ssh") > 0) then
                    call fail_test("VULNERABILITY: Error message leaks sensitive path information")
                else
                    call pass_test("Error message does not leak sensitive path")
                end if
            else
                call pass_test("Error message properly sanitized (no message)")
            end if
        else
            call fail_test("Sensitive path not blocked - different issue")
        end if
        
    end subroutine test_error_message_path_leakage
    
    subroutine test_command_echo_in_errors()
        !! 
        !! Given-When-Then: Test command echoing in error messages
        !! 
        !! GIVEN: Command execution error handling
        !! WHEN: Commands fail during execution
        !! THEN: Full commands should not be echoed in error messages  
        !!
        type(gcov_executor_t) :: executor
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        
        call start_test("Command Echo in Error Messages")
        
        ! Force an error condition to check error message content
        call executor%set_gcov_output_directory("/root/impossible_directory_no_permission")
        call executor%execute_gcov("nonexistent.f90", gcov_files, error_ctx)
        
        ! Check if full command is leaked in error messages
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (len_trim(error_ctx%message) > 0) then
                if (index(error_ctx%message, "mkdir -p") > 0) then
                    call fail_test("VULNERABILITY: Full command leaked in error message")
                else
                    call pass_test("Command details not leaked in error message")
                end if
            else
                call pass_test("Error message properly handled")
            end if
        else
            call fail_test("Error condition not triggered - test inconclusive")
        end if
        
    end subroutine test_command_echo_in_errors
    
    ! Helper subroutines for test setup
    subroutine create_test_source_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') "program test_security"
        write(unit, '(A)') "    print *, 'Security test file'"
        write(unit, '(A)') "end program test_security"
        close(unit)
    end subroutine create_test_source_file
    
    subroutine create_test_gcov_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') "        -:    0:Source:legitimate.f90"
        write(unit, '(A)') "        -:    0:Graph:legitimate.gcno"
        write(unit, '(A)') "        -:    0:Data:legitimate.gcda"
        write(unit, '(A)') "        -:    0:Runs:1"
        write(unit, '(A)') "        -:    0:Programs:1"
        write(unit, '(A)') "        -:    1:program test_security"
        write(unit, '(A)') "        1:    2:    print *, 'Security test'"
        write(unit, '(A)') "        1:    3:end program test_security"
        close(unit)
    end subroutine create_test_gcov_file
    
    subroutine create_malicious_gcov_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        ! Create gcov file with malicious name for testing mv vulnerability
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') "        -:    0:Source:malicious.f90"
        write(unit, '(A)') "        -:    0:Graph:malicious.gcno"
        write(unit, '(A)') "        -:    0:Data:malicious.gcda"
        write(unit, '(A)') "        -:    1:program malicious"
        write(unit, '(A)') "        1:    2:    print *, 'malicious'"
        write(unit, '(A)') "        1:    3:end program malicious"
        close(unit)
    end subroutine create_malicious_gcov_file
    
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') "Test ", test_count, ": ", test_name
    end subroutine start_test
    
    subroutine pass_test(message)
        character(len=*), intent(in) :: message
        print *, "   ✅ PASS: " // trim(message)
        print *, ""
    end subroutine pass_test
    
    subroutine fail_test(message)
        character(len=*), intent(in) :: message
        failed_count = failed_count + 1
        print *, "   🚨 FAIL: " // trim(message)
        print *, ""
    end subroutine fail_test

end program test_security_command_injection_issue_235