program test_string_utils_comprehensive
    use string_utils
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing String Utilities - Comprehensive Edge Cases and Security..."
    
    ! Security function tests (critical gaps)
    all_tests_passed = all_tests_passed .and. test_validate_string_input_security()
    all_tests_passed = all_tests_passed .and. test_sanitize_filename_security()
    all_tests_passed = all_tests_passed .and. test_is_safe_path_security()
    
    ! Edge case tests for existing functions
    all_tests_passed = all_tests_passed .and. test_format_percentage_edge_cases()
    all_tests_passed = all_tests_passed .and. test_compress_ranges_edge_cases()
    all_tests_passed = all_tests_passed .and. test_split_string_edge_cases()
    all_tests_passed = all_tests_passed .and. test_trim_string_edge_cases()
    
    ! Error condition tests
    all_tests_passed = all_tests_passed .and. test_format_percentage_invalid_precision()
    all_tests_passed = all_tests_passed .and. test_split_empty_delimiter()
    
    if (all_tests_passed) then
        print *, "All comprehensive tests PASSED"
        call exit(0)
    else
        print *, "Some comprehensive tests FAILED"
        call exit(1)
    end if

contains

    function test_validate_string_input_security() result(passed)
        logical :: passed
        logical :: result1, result2, result3, result4, result5
        
        print *, "  Test: String input security validation"
        print *, "    Given: Various potentially dangerous string inputs"
        print *, "    When: Validating each string for security constraints"
        print *, "    Then: Dangerous strings should be rejected, safe strings accepted"
        
        ! Test 1: Command injection attempts should be rejected
        result1 = .not. validate_string_input("file.txt; rm -rf /", 50)
        
        ! Test 2: Shell metacharacters should be rejected
        result2 = .not. validate_string_input("file.txt|cat /etc/passwd", 50)
        
        ! Test 3: Excessive length should be rejected
        result3 = .not. validate_string_input("very_long_string_exceeding_limit", 10)
        
        ! Test 4: Control characters should be rejected (except tab/newline)
        result4 = .not. validate_string_input("file" // char(0) // ".txt", 50)
        
        ! Test 5: Safe strings should be accepted
        result5 = validate_string_input("normal_file.txt", 50)
        
        passed = result1 .and. result2 .and. result3 .and. result4 .and. result5
        
        if (.not. passed) then
            print *, "    FAILED: Security validation not working correctly"
            print *, "      Command injection blocked:", result1
            print *, "      Shell metacharacters blocked:", result2
            print *, "      Length limit enforced:", result3
            print *, "      Control chars blocked:", result4
            print *, "      Safe string accepted:", result5
        else
            print *, "    PASSED: Security validation working correctly"
        end if
    end function test_validate_string_input_security

    function test_sanitize_filename_security() result(passed)
        logical :: passed
        character(len=:), allocatable :: result1, result2, result3
        logical :: test1, test2, test3
        
        print *, "  Test: Filename sanitization security"
        print *, "    Given: Potentially dangerous filenames with illegal characters"
        print *, "    When: Sanitizing filenames for security"
        print *, "    Then: Dangerous characters should be removed, safe chars preserved"
        
        ! Test 1: Shell metacharacters should be removed
        result1 = sanitize_filename("file;rm-rf.txt")
        test1 = (result1 == "filerm-rf.txt")
        
        ! Test 2: Directory traversal should be handled
        result2 = sanitize_filename("../../../etc/passwd")
        test2 = (result2 == "../../../etc/passwd")  ! Path traversal detection is in is_safe_path
        
        ! Test 3: Normal filename should be preserved
        result3 = sanitize_filename("normal_file-name.f90")
        test3 = (result3 == "normal_file-name.f90")
        
        passed = test1 .and. test2 .and. test3
        
        if (.not. passed) then
            print *, "    FAILED: Filename sanitization not working correctly"
            print *, "      Shell chars removed:", test1, " Got: '", result1, "'"
            print *, "      Traversal handled:", test2, " Got: '", result2, "'"
            print *, "      Normal preserved:", test3, " Got: '", result3, "'"
        else
            print *, "    PASSED: Filename sanitization working correctly"
        end if
    end function test_sanitize_filename_security

    function test_is_safe_path_security() result(passed)
        logical :: passed
        logical :: result1, result2, result3, result4, result5
        
        print *, "  Test: Path security validation"
        print *, "    Given: Various potentially dangerous file paths"
        print *, "    When: Checking path safety for directory traversal and system paths"
        print *, "    Then: Dangerous paths should be rejected, safe paths accepted"
        
        ! Test 1: Directory traversal should be rejected
        result1 = .not. is_safe_path("../../../etc/passwd")
        
        ! Test 2: System directories should be rejected
        result2 = .not. is_safe_path("/etc/shadow")
        
        ! Test 3: Windows system paths should be rejected
        result3 = .not. is_safe_path("C:\\Windows\\System32\\cmd.exe")
        
        ! Test 4: Root directory access should be rejected
        result4 = .not. is_safe_path("/root/.ssh/id_rsa")
        
        ! Test 5: Normal relative paths should be accepted
        result5 = is_safe_path("src/module.f90")
        
        passed = result1 .and. result2 .and. result3 .and. result4 .and. result5
        
        if (.not. passed) then
            print *, "    FAILED: Path security validation not working correctly"
            print *, "      Directory traversal blocked:", result1
            print *, "      System directories blocked:", result2
            print *, "      Windows system blocked:", result3
            print *, "      Root access blocked:", result4
            print *, "      Normal paths accepted:", result5
        else
            print *, "    PASSED: Path security validation working correctly"
        end if
    end function test_is_safe_path_security

    function test_format_percentage_edge_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: result1, result2, result3, result4
        logical :: test1, test2, test3, test4
        
        print *, "  Test: Percentage formatting edge cases"
        print *, "    Given: Edge case values like negative numbers and extreme precision"
        print *, "    When: Formatting percentages with various precisions"
        print *, "    Then: Should handle edge cases gracefully with proper formatting"
        
        ! Test 1: Negative percentage
        result1 = format_percentage(-15.5, 1)
        test1 = (index(result1, "-15.5") > 0)
        
        ! Test 2: Very high precision
        result2 = format_percentage(33.333333, 6)
        test2 = (index(result2, "33.333333") > 0)
        
        ! Test 3: Zero precision
        result3 = format_percentage(75.678, 0)
        test3 = (index(result3, "76") > 0)  ! Should round
        
        ! Test 4: Very large number
        result4 = format_percentage(999.99, 2)
        test4 = (index(result4, "999.99") > 0)
        
        passed = test1 .and. test2 .and. test3 .and. test4
        
        if (.not. passed) then
            print *, "    FAILED: Percentage formatting edge cases not handled correctly"
            print *, "      Negative handled:", test1, " Got: '", result1, "'"
            print *, "      High precision handled:", test2, " Got: '", result2, "'"
            print *, "      Zero precision handled:", test3, " Got: '", result3, "'"
            print *, "      Large number handled:", test4, " Got: '", result4, "'"
        else
            print *, "    PASSED: Percentage formatting edge cases handled correctly"
        end if
    end function test_format_percentage_edge_cases

    function test_compress_ranges_edge_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: result1, result2, result3, result4
        integer :: single(1), large(1000), unsorted(5), duplicates(6)
        logical :: test1, test2, test3, test4
        integer :: i
        
        print *, "  Test: Range compression edge cases"
        print *, "    Given: Edge case arrays like single elements, large arrays, unsorted data"
        print *, "    When: Compressing ranges"
        print *, "    Then: Should handle all edge cases correctly"
        
        ! Test 1: Single element array
        single = [42]
        result1 = compress_ranges(single)
        test1 = (result1 == "42")
        
        ! Test 2: Large array (performance test)
        do i = 1, 1000
            large(i) = i
        end do
        result2 = compress_ranges(large)
        test2 = (result2 == "1-1000")
        
        ! Test 3: Unsorted array (should still work if pre-sorted by caller)
        unsorted = [5, 1, 3, 2, 4]
        result3 = compress_ranges(unsorted)
        test3 = (len(result3) > 0)  ! Just check it doesn't crash
        
        ! Test 4: Array with duplicates
        duplicates = [1, 1, 2, 2, 3, 3]
        result4 = compress_ranges(duplicates)
        test4 = (len(result4) > 0)  ! Just check it doesn't crash
        
        passed = test1 .and. test2 .and. test3 .and. test4
        
        if (.not. passed) then
            print *, "    FAILED: Range compression edge cases not handled correctly"
            print *, "      Single element:", test1, " Got: '", result1, "'"
            print *, "      Large array:", test2, " Got: '", result2, "'"
            print *, "      Unsorted array:", test3, " Got: '", result3, "'"
            print *, "      Duplicates:", test4, " Got: '", result4, "'"
        else
            print *, "    PASSED: Range compression edge cases handled correctly"
        end if
    end function test_compress_ranges_edge_cases

    function test_split_string_edge_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: parts1(:), parts2(:), parts3(:), parts4(:)
        logical :: test1, test2, test3, test4
        
        print *, "  Test: String splitting edge cases"
        print *, "    Given: Edge case strings like empty strings, no delimiters, consecutive delimiters"
        print *, "    When: Splitting strings"
        print *, "    Then: Should handle edge cases without crashing"
        
        ! Test 1: String with no delimiter
        parts1 = split("no_delimiter_here", "/")
        test1 = (size(parts1) == 1 .and. parts1(1) == "no_delimiter_here")
        
        ! Test 2: String ending with delimiter
        parts2 = split("path/to/", "/")
        test2 = (size(parts2) >= 2)  ! Should handle trailing delimiter
        
        ! Test 3: Consecutive delimiters
        parts3 = split("path//to//file", "/")
        test3 = (size(parts3) >= 3)  ! Should handle empty parts
        
        ! Test 4: Single character string
        parts4 = split("a", "b")
        test4 = (size(parts4) == 1 .and. parts4(1) == "a")
        
        passed = test1 .and. test2 .and. test3 .and. test4
        
        if (.not. passed) then
            print *, "    FAILED: String splitting edge cases not handled correctly"
            print *, "      No delimiter:", test1
            print *, "      Trailing delimiter:", test2
            print *, "      Consecutive delimiters:", test3
            print *, "      Single char:", test4
        else
            print *, "    PASSED: String splitting edge cases handled correctly"
        end if
    end function test_split_string_edge_cases

    function test_trim_string_edge_cases() result(passed)
        logical :: passed
        character(len=:), allocatable :: result1, result2, result3, result4
        logical :: test1, test2, test3, test4
        
        print *, "  Test: String trimming edge cases"
        print *, "    Given: Edge case strings with various whitespace patterns"
        print *, "    When: Trimming whitespace"
        print *, "    Then: Should handle all whitespace patterns correctly"
        
        ! Test 1: String with only spaces
        result1 = trim_string("     ")
        test1 = (len(result1) == 0)
        
        ! Test 2: String with tabs and spaces
        result2 = trim_string("	 hello	 ")
        test2 = (result2 == "hello")
        
        ! Test 3: Empty string
        result3 = trim_string("")
        test3 = (len(result3) == 0)
        
        ! Test 4: String with internal spaces (should preserve)
        result4 = trim_string("  hello world  ")
        test4 = (result4 == "hello world")
        
        passed = test1 .and. test2 .and. test3 .and. test4
        
        if (.not. passed) then
            print *, "    FAILED: String trimming edge cases not handled correctly"
            print *, "      Only spaces:", test1, " Got: '", result1, "'"
            print *, "      Tabs/spaces:", test2, " Got: '", result2, "'"
            print *, "      Empty string:", test3, " Got: '", result3, "'"
            print *, "      Internal spaces:", test4, " Got: '", result4, "'"
        else
            print *, "    PASSED: String trimming edge cases handled correctly"
        end if
    end function test_trim_string_edge_cases

    function test_format_percentage_invalid_precision() result(passed)
        logical :: passed
        character(len=:), allocatable :: result1, result2
        logical :: test1, test2
        
        print *, "  Test: Invalid precision handling"
        print *, "    Given: Invalid precision values (negative)"
        print *, "    When: Formatting percentage"
        print *, "    Then: Should handle gracefully without crashing"
        
        ! Test 1: Negative precision (should not crash)
        result1 = format_percentage(50.0, -1)
        test1 = (len(result1) > 0)  ! Just check it doesn't crash
        
        ! Test 2: Very large precision (should not crash)
        result2 = format_percentage(50.0, 20)
        test2 = (len(result2) > 0)  ! Just check it doesn't crash
        
        passed = test1 .and. test2
        
        if (.not. passed) then
            print *, "    FAILED: Invalid precision not handled gracefully"
        else
            print *, "    PASSED: Invalid precision handled gracefully"
        end if
    end function test_format_percentage_invalid_precision

    function test_split_empty_delimiter() result(passed)
        logical :: passed
        character(len=:), allocatable :: parts(:)
        logical :: test1
        
        print *, "  Test: Empty delimiter handling"
        print *, "    Given: Empty delimiter string"
        print *, "    When: Splitting string"
        print *, "    Then: Should handle gracefully without infinite loop"
        
        ! Test: Empty delimiter (should not cause infinite loop)
        parts = split("test", "")
        test1 = (size(parts) >= 1)  ! Should return something reasonable
        
        passed = test1
        
        if (.not. passed) then
            print *, "    FAILED: Empty delimiter not handled gracefully"
        else
            print *, "    PASSED: Empty delimiter handled gracefully"
        end if
    end function test_split_empty_delimiter

end program test_string_utils_comprehensive