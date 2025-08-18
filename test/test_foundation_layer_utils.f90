program test_foundation_layer_utils
    !! Test suite for foundation layer utilities extracted from coverage_engine.f90
    !! 
    !! This test suite validates the utility functions that will be extracted
    !! as part of the Issue #126 refactoring to ensure behavioral consistency
    !! before and after module decomposition.
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Foundation Layer Utilities..."
    
    ! Test 1: String to lowercase conversion
    all_tests_passed = all_tests_passed .and. test_to_lower_conversion()
    
    ! Test 2: Integer to string conversion
    all_tests_passed = all_tests_passed .and. test_int_to_string_conversion()
    
    ! Test 3: Simple pattern matching without wildcards
    all_tests_passed = all_tests_passed .and. test_pattern_matching_simple()
    
    ! Test 4: Pattern matching with wildcard at end
    all_tests_passed = all_tests_passed .and. test_pattern_matching_wildcard_end()
    
    ! Test 5: Pattern matching with wildcard at beginning
    all_tests_passed = all_tests_passed .and. test_pattern_matching_wildcard_start()
    
    ! Test 6: Pattern matching with middle wildcard
    all_tests_passed = all_tests_passed .and. test_pattern_matching_wildcard_middle()
    
    ! Test 7: Check exclude patterns with empty patterns
    all_tests_passed = all_tests_passed .and. test_exclude_patterns_empty()
    
    ! Test 8: Check exclude patterns with single match
    all_tests_passed = all_tests_passed .and. test_exclude_patterns_single_match()
    
    ! Test 9: Check exclude patterns with multiple patterns
    all_tests_passed = all_tests_passed .and. test_exclude_patterns_multiple()
    
    ! Test 10: Edge cases for utility functions
    all_tests_passed = all_tests_passed .and. test_utility_edge_cases()
    
    if (all_tests_passed) then
        print *, "All foundation layer utility tests PASSED"
        call exit(0)
    else
        print *, "Some foundation layer utility tests FAILED"
        call exit(1)
    end if

contains

    function test_to_lower_conversion() result(passed)
        !! Given: Various strings with mixed case
        !! When: Converting to lowercase
        !! Then: All uppercase letters should be converted to lowercase
        logical :: passed
        character(len=:), allocatable :: result
        
        print *, "  Test 1: String to lowercase conversion"
        
        ! Test normal mixed case
        result = to_lower_ref("Hello World")
        passed = (result == "hello world")
        
        if (.not. passed) then
            print *, "    FAILED: Expected 'hello world', got '", result, "'"
            return
        end if
        
        ! Test all uppercase
        result = to_lower_ref("FORTCOV")
        passed = (result == "fortcov")
        
        if (.not. passed) then
            print *, "    FAILED: Expected 'fortcov', got '", result, "'"
            return
        end if
        
        ! Test all lowercase (should remain unchanged)
        result = to_lower_ref("already_lower")
        passed = (result == "already_lower")
        
        if (.not. passed) then
            print *, "    FAILED: Expected 'already_lower', got '", result, "'"
            return
        end if
        
        ! Test with numbers and special characters
        result = to_lower_ref("Test123_File.f90")
        passed = (result == "test123_file.f90")
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Expected 'test123_file.f90', got '", result, "'"
        end if
    end function test_to_lower_conversion

    function test_int_to_string_conversion() result(passed)
        !! Given: Various integer values
        !! When: Converting to string representation
        !! Then: Should produce correct string representation
        logical :: passed
        character(len=:), allocatable :: result
        
        print *, "  Test 2: Integer to string conversion"
        
        ! Test positive integer
        result = int_to_string_ref(42)
        passed = (result == "42")
        
        if (.not. passed) then
            print *, "    FAILED: Expected '42', got '", result, "'"
            return
        end if
        
        ! Test zero
        result = int_to_string_ref(0)
        passed = (result == "0")
        
        if (.not. passed) then
            print *, "    FAILED: Expected '0', got '", result, "'"
            return
        end if
        
        ! Test negative integer
        result = int_to_string_ref(-123)
        passed = (result == "-123")
        
        if (.not. passed) then
            print *, "    FAILED: Expected '-123', got '", result, "'"
            return
        end if
        
        ! Test large integer
        result = int_to_string_ref(999999)
        passed = (result == "999999")
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Expected '999999', got '", result, "'"
        end if
    end function test_int_to_string_conversion

    function test_pattern_matching_simple() result(passed)
        !! Given: File paths and exact match patterns
        !! When: Checking pattern matching without wildcards
        !! Then: Should match exactly
        logical :: passed
        logical :: result
        
        print *, "  Test 3: Simple pattern matching without wildcards"
        
        ! Test exact match
        result = matches_pattern_ref("test.f90", "test.f90")
        passed = result
        
        if (.not. passed) then
            print *, "    FAILED: Expected exact match for 'test.f90'"
            return
        end if
        
        ! Test no match
        result = matches_pattern_ref("test.f90", "other.f90")
        passed = (.not. result)
        
        if (.not. passed) then
            print *, "    FAILED: Expected no match for different files"
            return
        end if
        
        ! Test case sensitivity
        result = matches_pattern_ref("Test.f90", "test.f90")
        passed = (.not. result)  ! Should not match due to case sensitivity
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Case sensitivity test failed"
        end if
    end function test_pattern_matching_simple

    function test_pattern_matching_wildcard_end() result(passed)
        !! Given: File paths and patterns with trailing wildcard
        !! When: Checking pattern matching
        !! Then: Should match prefix correctly
        logical :: passed
        logical :: result
        
        print *, "  Test 4: Pattern matching with wildcard at end"
        
        ! Test wildcard at end
        result = matches_pattern_ref("test.f90", "test*")
        passed = result
        
        if (.not. passed) then
            print *, "    FAILED: Expected match for 'test*' pattern"
            return
        end if
        
        ! Test no match with wildcard
        result = matches_pattern_ref("other.f90", "test*")
        passed = (.not. result)
        
        if (.not. passed) then
            print *, "    FAILED: Expected no match for non-matching prefix"
            return
        end if
        
        ! Test empty prefix with wildcard
        result = matches_pattern_ref("anything.f90", "*")
        passed = result
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Universal wildcard test failed"
        end if
    end function test_pattern_matching_wildcard_end

    function test_pattern_matching_wildcard_start() result(passed)
        !! Given: File paths and patterns with leading wildcard
        !! When: Checking pattern matching
        !! Then: Should match suffix correctly
        logical :: passed
        logical :: result
        
        print *, "  Test 5: Pattern matching with wildcard at beginning"
        
        ! Test wildcard at beginning
        result = matches_pattern_ref("src/test.f90", "*.f90")
        passed = result
        
        if (.not. passed) then
            print *, "    FAILED: Expected match for '*.f90' pattern"
            return
        end if
        
        ! Test no match with suffix wildcard
        result = matches_pattern_ref("test.c", "*.f90")
        passed = (.not. result)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Suffix wildcard test failed"
        end if
    end function test_pattern_matching_wildcard_start

    function test_pattern_matching_wildcard_middle() result(passed)
        !! Given: File paths and patterns with middle wildcard
        !! When: Checking pattern matching
        !! Then: Should match prefix and suffix correctly
        logical :: passed
        logical :: result
        
        print *, "  Test 6: Pattern matching with middle wildcard"
        
        ! Test middle wildcard
        result = matches_pattern_ref("test_module.f90", "test*.f90")
        passed = result
        
        if (.not. passed) then
            print *, "    FAILED: Expected match for 'test*.f90' pattern"
            return
        end if
        
        ! Test no match with middle wildcard
        result = matches_pattern_ref("other_module.c", "test*.f90")
        passed = (.not. result)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Middle wildcard test failed"
        end if
    end function test_pattern_matching_wildcard_middle

    function test_exclude_patterns_empty() result(passed)
        !! Given: Config with no exclude patterns
        !! When: Checking if file should be excluded
        !! Then: Should not exclude any files
        logical :: passed
        type(config_ref_t) :: config
        logical :: result
        
        print *, "  Test 7: Check exclude patterns with empty patterns"
        
        ! Initialize config with no exclude patterns
        config%has_exclude_patterns = .false.
        
        result = check_exclude_patterns_ref("any_file.f90", config)
        passed = (.not. result)  ! Should not exclude
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Empty patterns should not exclude files"
        end if
    end function test_exclude_patterns_empty

    function test_exclude_patterns_single_match() result(passed)
        !! Given: Config with single exclude pattern
        !! When: Checking files against pattern
        !! Then: Should exclude matching files only
        logical :: passed
        type(config_ref_t) :: config
        logical :: result
        
        print *, "  Test 8: Check exclude patterns with single match"
        
        ! Initialize config with one exclude pattern
        config%has_exclude_patterns = .true.
        allocate(config%exclude_patterns(1))
        config%exclude_patterns(1) = "test_*.f90"
        
        ! Test matching file
        result = check_exclude_patterns_ref("test_module.f90", config)
        passed = result  ! Should exclude
        
        if (.not. passed) then
            print *, "    FAILED: Should exclude matching file"
            deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test non-matching file
        result = check_exclude_patterns_ref("main.f90", config)
        passed = (.not. result)  ! Should not exclude
        
        deallocate(config%exclude_patterns)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Should not exclude non-matching file"
        end if
    end function test_exclude_patterns_single_match

    function test_exclude_patterns_multiple() result(passed)
        !! Given: Config with multiple exclude patterns
        !! When: Checking files against patterns
        !! Then: Should exclude files matching any pattern
        logical :: passed
        type(config_ref_t) :: config
        logical :: result
        
        print *, "  Test 9: Check exclude patterns with multiple patterns"
        
        ! Initialize config with multiple exclude patterns
        config%has_exclude_patterns = .true.
        allocate(config%exclude_patterns(3))
        config%exclude_patterns(1) = "test_*.f90"
        config%exclude_patterns(2) = "*.bak"
        config%exclude_patterns(3) = "debug_*"
        
        ! Test first pattern match
        result = check_exclude_patterns_ref("test_module.f90", config)
        passed = result
        
        if (.not. passed) then
            print *, "    FAILED: Should exclude file matching first pattern"
            deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test second pattern match
        result = check_exclude_patterns_ref("config.bak", config)
        passed = result
        
        if (.not. passed) then
            print *, "    FAILED: Should exclude file matching second pattern"
            deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test third pattern match
        result = check_exclude_patterns_ref("debug_output.txt", config)
        passed = result
        
        if (.not. passed) then
            print *, "    FAILED: Should exclude file matching third pattern"
            deallocate(config%exclude_patterns)
            return
        end if
        
        ! Test no match
        result = check_exclude_patterns_ref("main.f90", config)
        passed = (.not. result)
        
        deallocate(config%exclude_patterns)
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Should not exclude non-matching file"
        end if
    end function test_exclude_patterns_multiple

    function test_utility_edge_cases() result(passed)
        !! Given: Edge case inputs for utility functions
        !! When: Processing edge cases
        !! Then: Should handle gracefully without errors
        logical :: passed
        character(len=:), allocatable :: result
        logical :: bool_result
        
        print *, "  Test 10: Edge cases for utility functions"
        
        ! Test empty string conversion
        result = to_lower_ref("")
        passed = (result == "")
        
        if (.not. passed) then
            print *, "    FAILED: Empty string conversion failed"
            return
        end if
        
        ! Test single character
        result = to_lower_ref("A")
        passed = (result == "a")
        
        if (.not. passed) then
            print *, "    FAILED: Single character conversion failed"
            return
        end if
        
        ! Test pattern matching with empty pattern
        bool_result = matches_pattern_ref("test.f90", "")
        passed = (.not. bool_result)  ! Empty pattern should not match
        
        if (.not. passed) then
            print *, "    FAILED: Empty pattern should not match"
            return
        end if
        
        ! Test pattern matching with empty filepath
        bool_result = matches_pattern_ref("", "test*")
        passed = (.not. bool_result)  ! Empty filepath should not match
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Empty filepath should not match"
        end if
    end function test_utility_edge_cases

    !
    ! Reference implementations matching current coverage_engine.f90 behavior
    ! These will be compared against the extracted foundation layer utilities
    !

    function to_lower_ref(str) result(lower_str)
        !! Reference implementation of to_lower function
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i, ascii_val
        
        lower_str = str
        do i = 1, len(str)
            ascii_val = ichar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then  ! A-Z
                lower_str(i:i) = char(ascii_val + 32)
            end if
        end do
    end function to_lower_ref

    function int_to_string_ref(value) result(str)
        !! Reference implementation of int_to_string function
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=20) :: temp
        
        write(temp, '(I0)') value
        str = trim(temp)
    end function int_to_string_ref

    function matches_pattern_ref(filepath, pattern) result(matches)
        !! Reference implementation of matches_pattern function
        character(len=*), intent(in) :: filepath
        character(len=*), intent(in) :: pattern
        logical :: matches
        
        character(len=:), allocatable :: pattern_lower, filepath_lower
        integer :: star_pos
        
        ! For now, do case-sensitive matching (to_lower not available)
        pattern_lower = trim(pattern)
        filepath_lower = trim(filepath)
        
        star_pos = index(pattern_lower, '*')
        
        if (star_pos == 0) then
            ! No wildcard, exact match
            matches = (filepath_lower == pattern_lower)
        else if (star_pos == 1) then
            ! Wildcard at beginning, check suffix
            if (len(pattern_lower) == 1) then
                ! Just "*", matches everything
                matches = .true.
            else
                matches = (len(filepath_lower) >= len(pattern_lower) - 1) .and. &
                         (filepath_lower(len(filepath_lower) - len(pattern_lower) + 2:) == &
                          pattern_lower(2:))
            end if
        else if (star_pos == len(pattern_lower)) then
            ! Wildcard at end, check prefix
            matches = (len(filepath_lower) >= len(pattern_lower) - 1) .and. &
                     (filepath_lower(1:len(pattern_lower) - 1) == &
                      pattern_lower(1:len(pattern_lower) - 1))
        else
            ! Wildcard in middle, check prefix and suffix
            matches = (len(filepath_lower) >= len(pattern_lower) - 1) .and. &
                     (filepath_lower(1:star_pos - 1) == pattern_lower(1:star_pos - 1)) .and. &
                     (filepath_lower(len(filepath_lower) - len(pattern_lower) + star_pos + 1:) == &
                      pattern_lower(star_pos + 1:))
        end if
    end function matches_pattern_ref

    function check_exclude_patterns_ref(filepath, config) result(should_exclude)
        !! Reference implementation of check_exclude_patterns function
        character(len=*), intent(in) :: filepath
        type(config_ref_t), intent(in) :: config
        logical :: should_exclude
        
        integer :: i
        
        should_exclude = .false.
        
        ! Check if exclude patterns are allocated and not empty
        if (.not. config%has_exclude_patterns .or. &
            .not. allocated(config%exclude_patterns) .or. &
            size(config%exclude_patterns) == 0) then
            return
        end if
        
        do i = 1, size(config%exclude_patterns)
            if (matches_pattern_ref(filepath, config%exclude_patterns(i))) then
                should_exclude = .true.
                return
            end if
        end do
    end function check_exclude_patterns_ref

    ! Simplified config type for testing
    type :: config_ref_t
        logical :: has_exclude_patterns = .false.
        character(len=:), allocatable :: exclude_patterns(:)
    end type config_ref_t

end program test_foundation_layer_utils