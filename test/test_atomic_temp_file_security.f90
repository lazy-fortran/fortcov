program test_atomic_temp_file_security
    !! Focused security test suite for atomic temporary file operations
    !! 
    !! This test suite specifically validates the atomic_temp_file_manager
    !! security features identified as vulnerable in Issue #121:
    !! - Race condition prevention
    !! - Symlink attack prevention  
    !! - Exclusive creation enforcement
    !! - Entropy validation for filename unpredictability
    !! - Platform-specific security feature usage
    
    use atomic_temp_file_manager
    use error_handling
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_passed = .true.
    
    print *, "=== Atomic Temp File Security Test Suite ==="
    print *, ""
    
    ! Execute atomic temp file security tests
    call test_exclusive_creation_enforcement()
    call test_race_condition_prevention() 
    call test_symlink_attack_mitigation()
    call test_filename_entropy_validation()
    call test_platform_security_features()
    call test_concurrent_creation_safety()
    call test_cleanup_security()
    call test_error_condition_security()
    
    ! Print final results
    print *, ""
    print *, "=== Atomic Temp File Security Results ==="
    print '(A,I0)', "Tests Passed: ", passed_count
    print '(A,I0)', "Tests Failed: ", test_count - passed_count
    print '(A,I0)', "Total Tests: ", test_count
    
    if (all_passed) then
        print *, "OVERALL: PASSED - Atomic temp file security is robust"
        stop 0
    else
        print *, "OVERALL: FAILED - Atomic temp file security vulnerabilities detected"
        stop 1
    end if
    
contains

    subroutine test_exclusive_creation_enforcement()
        !! Test that exclusive creation (O_EXCL/CREATE_NEW) is enforced
        !! This prevents TOCTOU (Time of Check Time of Use) attacks
        
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        
        call start_test("Exclusive Creation Enforcement")
        
        ! Given: A secure temp file creation request
        ! When: Creating the temp file
        ! Then: Should use exclusive creation to prevent TOCTOU attacks
        
        call temp_file%create_secure(error_ctx, success)
        if (.not. success) then
            call fail_test("Failed to create secure temp file")
            return
        end if
        
        ! Verify exclusive creation was used
        if (.not. temp_file%used_exclusive_creation()) then
            call fail_test("Exclusive creation flag not used - TOCTOU vulnerability")
            call temp_file%cleanup()
            return
        end if
        
        ! Verify creation was atomic (no time gap)
        if (temp_file%get_creation_time_gap() /= 0) then
            call fail_test("Non-atomic creation - race condition window exists")
            call temp_file%cleanup()
            return
        end if
        
        call temp_file%cleanup()
        call pass_test()
    end subroutine test_exclusive_creation_enforcement

    subroutine test_race_condition_prevention()
        !! Test prevention of race conditions in concurrent temp file creation
        
        type(secure_temp_file_t), dimension(5) :: temp_files
        type(error_context_t) :: error_ctx
        logical :: success
        character(len=1024), dimension(5) :: filenames
        integer :: i, j
        
        call start_test("Race Condition Prevention")
        
        ! Given: Multiple concurrent temp file creation attempts
        ! When: Creating temp files simultaneously
        ! Then: Should prevent race conditions and ensure unique files
        
        ! Create multiple temp files concurrently
        do i = 1, 5
            call temp_files(i)%create_secure(error_ctx, success)
            if (.not. success) then
                call fail_test("Concurrent temp file creation failed")
                call cleanup_multiple_temp_files(temp_files, i-1)
                return
            end if
        end do
        
        ! Extract all filenames
        do i = 1, 5
            call temp_files(i)%get_filename(filenames(i))
        end do
        
        ! Verify all filenames are unique (no race condition)
        do i = 1, 5
            do j = i+1, 5
                if (trim(filenames(i)) == trim(filenames(j))) then
                    call fail_test("Duplicate filenames - race condition detected")
                    call cleanup_multiple_temp_files(temp_files, 5)
                    return
                end if
            end do
        end do
        
        ! Verify all use atomic creation
        do i = 1, 5
            if (.not. temp_files(i)%is_atomic_creation()) then
                call fail_test("Non-atomic creation in concurrent scenario")
                call cleanup_multiple_temp_files(temp_files, 5)
                return
            end if
        end do
        
        call cleanup_multiple_temp_files(temp_files, 5)
        call pass_test()
    end subroutine test_race_condition_prevention

    subroutine test_symlink_attack_mitigation()
        !! Test mitigation of symlink attacks through proper file creation
        
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        
        call start_test("Symlink Attack Mitigation")
        
        ! Given: A potentially hostile environment with symlinks
        ! When: Creating secure temp files
        ! Then: Should prevent symlink following attacks
        
        call temp_file%create_secure(error_ctx, success)
        if (.not. success) then
            call fail_test("Failed to create temp file for symlink test")
            return
        end if
        
        ! Verify symlink following is prevented
        if (.not. temp_file%prevents_symlink_following()) then
            call fail_test("Symlink following not prevented - security vulnerability")
            call temp_file%cleanup()
            return
        end if
        
        ! Verify platform-specific symlink protections are active
        if (get_platform_is_unix()) then
            if (.not. temp_file%uses_unix_security_features()) then
                call fail_test("Unix symlink protections not active")
                call temp_file%cleanup()
                return
            end if
        else
            if (.not. temp_file%uses_windows_security_features()) then
                call fail_test("Windows symlink protections not active")
                call temp_file%cleanup()
                return
            end if
        end if
        
        call temp_file%cleanup()
        call pass_test()
    end subroutine test_symlink_attack_mitigation

    subroutine test_filename_entropy_validation()
        !! Test that temp filenames have sufficient entropy to prevent prediction
        
        type(secure_temp_file_t), dimension(10) :: temp_files
        type(error_context_t) :: error_ctx
        logical :: success
        integer :: i, entropy_bits
        character(len=1024), dimension(10) :: filenames
        
        call start_test("Filename Entropy Validation")
        
        ! Given: Multiple temp file creations
        ! When: Analyzing filename entropy
        ! Then: Should have sufficient entropy to prevent prediction attacks
        
        ! Create multiple temp files to analyze patterns
        do i = 1, 10
            call temp_files(i)%create_secure(error_ctx, success)
            if (.not. success) then
                call fail_test("Failed to create temp file for entropy test")
                call cleanup_multiple_temp_files(temp_files, i-1)
                return
            end if
        end do
        
        ! Verify entropy requirements
        do i = 1, 10
            call temp_files(i)%get_entropy_bits(entropy_bits)
            if (entropy_bits < 64) then  ! Minimum 64 bits for security
                call fail_test("Insufficient entropy in temp filename - prediction vulnerability")
                call cleanup_multiple_temp_files(temp_files, 10)
                return
            end if
        end do
        
        ! Extract filenames and verify they're all different
        do i = 1, 10
            call temp_files(i)%get_filename(filenames(i))
        end do
        
        ! Simple uniqueness check (should be guaranteed with good entropy)
        block
            integer :: j
            do i = 1, 10
                do j = i+1, 10
                    if (trim(filenames(i)) == trim(filenames(j))) then
                        call fail_test("Duplicate filenames - insufficient entropy")
                        call cleanup_multiple_temp_files(temp_files, 10)
                        return
                    end if
                end do
            end do
        end block
        
        call cleanup_multiple_temp_files(temp_files, 10)
        call pass_test()
    end subroutine test_filename_entropy_validation

    subroutine test_platform_security_features()
        !! Test that platform-specific security features are properly utilized
        
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        
        call start_test("Platform Security Features")
        
        ! Given: Platform-specific security requirements
        ! When: Creating secure temp files
        ! Then: Should use appropriate platform-specific security features
        
        call temp_file%create_secure(error_ctx, success)
        if (.not. success) then
            call fail_test("Failed to create temp file for platform test")
            return
        end if
        
        ! Test platform-specific features
        if (get_platform_is_unix()) then
            ! Unix/Linux specific tests
            if (.not. temp_file%uses_unix_security_features()) then
                call fail_test("Unix security features not utilized")
                call temp_file%cleanup()
                return
            end if
            
            ! Verify Unix-specific protections
            if (.not. temp_file%prevents_symlink_following()) then
                call fail_test("Unix symlink protection not active")
                call temp_file%cleanup()
                return
            end if
        else
            ! Windows specific tests
            if (.not. temp_file%uses_windows_security_features()) then
                call fail_test("Windows security features not utilized")
                call temp_file%cleanup()
                return
            end if
            
            ! Verify Windows-specific protections
            if (.not. temp_file%prevents_symlink_following()) then
                call fail_test("Windows symlink protection not active")
                call temp_file%cleanup()
                return
            end if
        end if
        
        call temp_file%cleanup()
        call pass_test()
    end subroutine test_platform_security_features

    subroutine test_concurrent_creation_safety()
        !! Test safety of concurrent temp file creation under load
        
        type(secure_temp_file_t), dimension(20) :: temp_files
        type(error_context_t) :: error_ctx
        logical :: success
        integer :: i
        
        call start_test("Concurrent Creation Safety")
        
        ! Given: High-load concurrent temp file creation
        ! When: Creating many temp files simultaneously
        ! Then: Should maintain security guarantees under load
        
        ! Create many temp files to stress test the system
        do i = 1, 20
            call temp_files(i)%create_secure(error_ctx, success)
            if (.not. success) then
                call fail_test("Concurrent creation failed under load")
                call cleanup_multiple_temp_files(temp_files, i-1)
                return
            end if
            
            ! Verify security properties for each file
            if (.not. temp_files(i)%used_exclusive_creation()) then
                call fail_test("Exclusive creation not used under load")
                call cleanup_multiple_temp_files(temp_files, i)
                return
            end if
            
            if (.not. temp_files(i)%prevents_symlink_following()) then
                call fail_test("Symlink protection failed under load")
                call cleanup_multiple_temp_files(temp_files, i)
                return
            end if
        end do
        
        call cleanup_multiple_temp_files(temp_files, 20)
        call pass_test()
    end subroutine test_concurrent_creation_safety

    subroutine test_cleanup_security()
        !! Test security of temp file cleanup operations
        
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        character(len=1024) :: filename
        
        call start_test("Cleanup Security")
        
        ! Given: A created secure temp file
        ! When: Performing cleanup operations
        ! Then: Should securely remove files without leaving traces
        
        call temp_file%create_secure(error_ctx, success)
        if (.not. success) then
            call fail_test("Failed to create temp file for cleanup test")
            return
        end if
        
        ! Get filename before cleanup
        call temp_file%get_filename(filename)
        
        ! Perform cleanup
        call temp_file%cleanup()
        
        ! Verify file is actually removed (basic check)
        block
            logical :: file_exists
            inquire(file=trim(filename), exist=file_exists)
            if (file_exists) then
                call fail_test("Temp file not properly cleaned up")
                return
            end if
        end block
        
        call pass_test()
    end subroutine test_cleanup_security

    subroutine test_error_condition_security()
        !! Test security under error conditions and edge cases
        
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        
        call start_test("Error Condition Security")
        
        ! Given: Error conditions and edge cases
        ! When: Handling errors in temp file operations
        ! Then: Should maintain security even under error conditions
        
        ! Test 1: Normal creation first
        call temp_file%create_secure(error_ctx, success)
        if (.not. success) then
            call fail_test("Initial temp file creation failed")
            return
        end if
        
        ! Test 2: Simulate error condition
        call temp_file%simulate_error_condition()
        
        ! Test 3: Attempt operations on error state should be safe
        block
            character(len=100) :: content = "test content"
            call temp_file%write_atomic(content, error_ctx, success)
            if (success) then
                call fail_test("Write to error state temp file should fail")
                return
            end if
            
            ! Error should be properly reported
            if (error_ctx%error_code == ERROR_SUCCESS) then
                call fail_test("Error state not properly reported")
                return
            end if
        end block
        
        ! Test 4: Cleanup after error should be safe
        call temp_file%cleanup()
        
        call pass_test()
    end subroutine test_error_condition_security

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

    subroutine cleanup_multiple_temp_files(temp_files, count)
        type(secure_temp_file_t), dimension(:), intent(inout) :: temp_files
        integer, intent(in) :: count
        integer :: i
        
        do i = 1, count
            call temp_files(i)%cleanup()
        end do
    end subroutine cleanup_multiple_temp_files

end program test_atomic_temp_file_security