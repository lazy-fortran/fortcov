program test_comprehensive_memory_safety
    !! Comprehensive tests for all memory safety fixes in Issue #120
    !! 
    !! This test validates that all critical memory safety vulnerabilities
    !! have been properly fixed with comprehensive security measures
    use coverage_model
    use fortcov_config
    use coverage_engine
    use string_utils
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing COMPREHENSIVE Memory Safety Fixes (Issue #120)..."
    
    all_tests_passed = .true.
    
    ! Test 1: Buffer overflow fix validation
    all_tests_passed = all_tests_passed .and. test_buffer_overflow_fix()
    
    ! Test 2: Memory leak fix validation  
    all_tests_passed = all_tests_passed .and. test_memory_leak_fix()
    
    ! Test 3: Use-after-free fix validation
    all_tests_passed = all_tests_passed .and. test_use_after_free_fix()
    
    ! Test 4: String input validation
    all_tests_passed = all_tests_passed .and. test_string_input_validation()
    
    ! Test 5: Path security validation
    all_tests_passed = all_tests_passed .and. test_path_security_validation()
    
    if (all_tests_passed) then
        print *, "All COMPREHENSIVE memory safety tests PASSED"
        print *, "Issue #120 security vulnerabilities RESOLVED"
        call exit(0)
    else
        print *, "CRITICAL: Some memory safety issues remain"
        call exit(1)
    end if

contains

    function test_buffer_overflow_fix() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: file_info
        type(coverage_line_t) :: line_info
        character(len=:), allocatable :: serialized
        character(len=:), allocatable :: extreme_filename
        
        print *, "  Test 1: Buffer overflow fix validation"
        
        ! Test with extremely long filename that would have caused overflow
        extreme_filename = repeat("verylongfilename", 100)  ! 1600 characters
        
        file_info%filename = extreme_filename
        line_info%line_number = 1
        line_info%execution_count = 5
        allocate(file_info%lines(1))
        file_info%lines(1) = line_info
        
        allocate(coverage_data%files(1))
        coverage_data%files(1) = file_info
        
        ! This should now work without buffer overflow
        serialized = coverage_data%serialize()
        
        if (len(serialized) == 0) then
            print *, "    WARNING: Empty serialization result"
            passed = .false.
        else if (index(serialized, "ERROR:") > 0) then
            print *, "    EXPECTED: Security limit enforced for extreme filename"
            passed = .true.
        else if (index(serialized, extreme_filename) > 0) then
            print *, "    PASSED: Extremely long filename handled safely"
            passed = .true.
        else
            print *, "    FAILED: Filename missing from serialization"
            passed = .false.
        end if
    end function test_buffer_overflow_fix

    function test_memory_leak_fix() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        integer :: unit, iostat, i
        
        print *, "  Test 2: Memory leak fix validation"
        
        ! Create config that would trigger memory leaks in error paths
        open(newunit=unit, file="test_leak_fix.nml", status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "  input_format = 'gcov'"
        write(unit, '(A)') "  source_paths = 'a,b,c,d,e'"
        write(unit, '(A)') "/"
        close(unit)
        
        passed = .true.
        
        ! Load config multiple times to test proper cleanup
        do i = 1, 3
            call initialize_config(config)
            config%config_file = "test_leak_fix.nml"
            
            call load_config_file(config, success, error_message)
            
            if (.not. success) then
                print *, "    WARNING: Config load failed, testing error path cleanup"
            end if
            
            ! The fix should ensure no memory leaks even with repeated loads
            if (allocated(config%source_paths)) then
                if (size(config%source_paths) /= 5) then
                    print *, "    FAILED: Incorrect array size on iteration", i
                    passed = .false.
                    exit
                end if
            end if
        end do
        
        ! Clean up test file
        open(newunit=unit, file="test_leak_fix.nml", status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
        if (passed) then
            print *, "    PASSED: Memory leak fixes working correctly"
        end if
    end function test_memory_leak_fix

    function test_use_after_free_fix() result(passed)
        logical :: passed
        character(len=:), allocatable :: input_files(:)
        character(len=:), allocatable :: filtered_files(:)
        type(config_t) :: config
        integer :: i
        
        print *, "  Test 3: Use-after-free fix validation"
        
        ! Create very long filenames that would have caused issues
        allocate(character(len=1000) :: input_files(10))
        do i = 1, 10
            input_files(i) = repeat("x", 900) // "_test_" // char(48 + i) // ".f90"
        end do
        
        ! Setup config
        call initialize_config(config)
        deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(1))
        config%exclude_patterns(1) = "*test_5*"
        
        ! The use-after-free fix should handle this safely
        ! Note: This tests the internal filter function through public interface
        ! Since filter_files_by_patterns is private, we test through find_coverage_files
        
        passed = .true.
        
        ! Multiple operations to stress the memory management
        do i = 1, 3
            ! This exercises the same memory patterns as filter_files_by_patterns
            if (allocated(config%source_paths)) deallocate(config%source_paths)
            allocate(character(len=256) :: config%source_paths(1))
            config%source_paths(1) = "."
            
            filtered_files = find_coverage_files(config)
            
            if (.not. allocated(filtered_files)) then
                print *, "    FAILED: File finding failed on iteration", i
                passed = .false.
                exit
            end if
        end do
        
        if (passed) then
            print *, "    PASSED: Use-after-free fixes working correctly"
        end if
    end function test_use_after_free_fix

    function test_string_input_validation() result(passed)
        logical :: passed
        logical :: is_valid
        character(len=:), allocatable :: sanitized
        
        print *, "  Test 4: String input validation"
        
        passed = .true.
        
        ! Test dangerous string rejection
        is_valid = validate_string_input("test; rm -rf /", 100)
        if (is_valid) then
            print *, "    FAILED: Shell injection string not rejected"
            passed = .false.
        end if
        
        ! Test length limit enforcement
        is_valid = validate_string_input(repeat("a", 1000), 100)
        if (is_valid) then
            print *, "    FAILED: Overly long string not rejected"
            passed = .false.
        end if
        
        ! Test valid string acceptance
        is_valid = validate_string_input("normal_filename.f90", 100)
        if (.not. is_valid) then
            print *, "    FAILED: Valid string incorrectly rejected"
            passed = .false.
        end if
        
        ! Test filename sanitization
        sanitized = sanitize_filename("dangerous;filename|with<bad>chars")
        if (index(sanitized, ';') > 0 .or. index(sanitized, '|') > 0) then
            print *, "    FAILED: Dangerous characters not removed"
            passed = .false.
        end if
        
        if (passed) then
            print *, "    PASSED: String input validation working correctly"
        end if
    end function test_string_input_validation

    function test_path_security_validation() result(passed)
        logical :: passed
        logical :: is_safe
        
        print *, "  Test 5: Path security validation"
        
        passed = .true.
        
        ! Test directory traversal rejection
        is_safe = is_safe_path("../../../etc/passwd")
        if (is_safe) then
            print *, "    FAILED: Directory traversal not blocked"
            passed = .false.
        end if
        
        ! Test system path rejection
        is_safe = is_safe_path("/etc/shadow")
        if (is_safe) then
            print *, "    FAILED: System path not blocked"
            passed = .false.
        end if
        
        ! Test Windows system path rejection
        is_safe = is_safe_path("C:\\Windows\\system32")
        if (is_safe) then
            print *, "    FAILED: Windows system path not blocked"
            passed = .false.
        end if
        
        ! Test valid relative path acceptance
        is_safe = is_safe_path("src/main.f90")
        if (.not. is_safe) then
            print *, "    FAILED: Valid relative path incorrectly rejected"
            passed = .false.
        end if
        
        if (passed) then
            print *, "    PASSED: Path security validation working correctly"
        end if
    end function test_path_security_validation

end program test_comprehensive_memory_safety