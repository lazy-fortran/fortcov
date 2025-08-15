program test_memory_safety_buffer_overflow
    !! Test for buffer overflow vulnerability in coverage_model.f90:463-480
    !! 
    !! Tests the serialize function with filenames exceeding 200 characters
    !! to expose buffer overflow vulnerabilities in line_str buffer
    use coverage_model
    use string_utils
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing Buffer Overflow Vulnerability in coverage_model serialize..."
    
    all_tests_passed = .true.
    
    ! Test 1: Filename exactly at 200 character limit
    all_tests_passed = all_tests_passed .and. test_filename_200_chars()
    
    ! Test 2: Filename exceeding 200 character limit (triggers overflow)
    all_tests_passed = all_tests_passed .and. test_filename_overflow()
    
    ! Test 3: Multiple long filenames to stress buffer reuse
    all_tests_passed = all_tests_passed .and. test_multiple_long_filenames()
    
    ! Test 4: Extremely long filename (1000+ chars)
    all_tests_passed = all_tests_passed .and. test_extreme_filename_length()
    
    if (all_tests_passed) then
        print *, "All buffer overflow tests PASSED"
        call exit(0)
    else
        print *, "Buffer overflow vulnerability DETECTED"
        call exit(1)
    end if

contains

    function test_filename_200_chars() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: file_info
        type(coverage_line_t) :: line_info
        character(len=:), allocatable :: serialized
        character(len=200) :: long_filename
        
        print *, "  Test 1: Filename at 200 character limit"
        
        ! Create filename exactly 200 characters long
        long_filename = repeat("a", 200)
        
        ! Create coverage data with long filename
        file_info%filename = long_filename
        line_info%line_number = 1
        line_info%execution_count = 5
        allocate(file_info%lines(1))
        file_info%lines(1) = line_info
        
        allocate(coverage_data%files(1))
        coverage_data%files(1) = file_info
        
        ! This should work but test for buffer safety
        serialized = coverage_data%serialize()
        
        ! Verify result contains the long filename
        if (index(serialized, long_filename) == 0) then
            print *, "    FAILED: 200-char filename not found in serialized output"
            passed = .false.
        else
            print *, "    PASSED: 200-char filename handled correctly"
            passed = .true.
        end if
    end function test_filename_200_chars

    function test_filename_overflow() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: file_info
        type(coverage_line_t) :: line_info
        character(len=:), allocatable :: serialized
        character(len=250) :: overflow_filename
        
        print *, "  Test 2: Filename exceeding 200 character limit (overflow test)"
        
        ! Create filename 250 characters long (50 chars over buffer)
        overflow_filename = repeat("x", 250)
        
        ! Create coverage data with overflowing filename
        file_info%filename = overflow_filename
        line_info%line_number = 1
        line_info%execution_count = 10
        allocate(file_info%lines(1))
        file_info%lines(1) = line_info
        
        allocate(coverage_data%files(1))
        coverage_data%files(1) = file_info
        
        ! This SHOULD expose buffer overflow vulnerability
        ! The 200-char line_str buffer cannot hold the full output
        serialized = coverage_data%serialize()
        
        ! If overflow occurs, serialized might be truncated or corrupted
        if (len(serialized) < 250) then
            print *, "    POTENTIAL BUFFER OVERFLOW: Serialized output truncated"
            print *, "    Expected length > 250, got:", len(serialized)
            passed = .false.
        else if (index(serialized, overflow_filename) == 0) then
            print *, "    POTENTIAL BUFFER OVERFLOW: Long filename not found in output"
            passed = .false.
        else
            print *, "    WARNING: Long filename handled (possible overflow masked)"
            passed = .true.
        end if
    end function test_filename_overflow

    function test_multiple_long_filenames() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: file_info
        type(coverage_line_t) :: line_info
        character(len=:), allocatable :: serialized
        character(len=220) :: long_filename1, long_filename2
        integer :: i
        
        print *, "  Test 3: Multiple long filenames (buffer reuse stress)"
        
        ! Create two different long filenames
        long_filename1 = repeat("a", 220)
        long_filename2 = repeat("b", 220)
        
        ! Create coverage data with multiple long filenames
        allocate(coverage_data%files(2))
        
        ! First file with long filename
        file_info%filename = long_filename1
        line_info%line_number = 1
        line_info%execution_count = 5
        allocate(file_info%lines(1))
        file_info%lines(1) = line_info
        coverage_data%files(1) = file_info
        
        ! Second file with different long filename
        file_info%filename = long_filename2
        line_info%line_number = 2
        line_info%execution_count = 10
        deallocate(file_info%lines)
        allocate(file_info%lines(1))
        file_info%lines(1) = line_info
        coverage_data%files(2) = file_info
        
        ! Test serialization with multiple overflows
        serialized = coverage_data%serialize()
        
        ! Check both filenames are present (would fail with buffer overflow)
        if (index(serialized, long_filename1) == 0) then
            print *, "    FAILED: First long filename missing from output"
            passed = .false.
        else if (index(serialized, long_filename2) == 0) then
            print *, "    FAILED: Second long filename missing from output"
            passed = .false.
        else
            print *, "    PASSED: Multiple long filenames handled"
            passed = .true.
        end if
    end function test_multiple_long_filenames

    function test_extreme_filename_length() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: file_info
        type(coverage_line_t) :: line_info
        character(len=:), allocatable :: serialized
        character(len=:), allocatable :: extreme_filename
        
        print *, "  Test 4: Extreme filename length (1000+ characters)"
        
        ! Create extremely long filename (1000 characters)
        extreme_filename = repeat("z", 1000)
        
        ! Create coverage data with extreme filename
        file_info%filename = extreme_filename
        line_info%line_number = 999
        line_info%execution_count = 42
        allocate(file_info%lines(1))
        file_info%lines(1) = line_info
        
        allocate(coverage_data%files(1))
        coverage_data%files(1) = file_info
        
        ! This WILL cause buffer overflow with 200-char buffer
        serialized = coverage_data%serialize()
        
        ! The 200-char buffer cannot hold this output
        if (index(serialized, extreme_filename) == 0) then
            print *, "    CRITICAL BUFFER OVERFLOW: Extreme filename truncated/lost"
            passed = .false.
        else
            print *, "    WARNING: Extreme filename handled (check for memory corruption)"
            passed = .true.
        end if
    end function test_extreme_filename_length

end program test_memory_safety_buffer_overflow