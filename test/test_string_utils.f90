program test_string_utils
    use string_utils
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing String Utilities..."
    
    ! Test 1: Compress consecutive line numbers
    all_tests_passed = all_tests_passed .and. test_compress_ranges_consecutive()
    
    ! Test 2: Compress single numbers
    all_tests_passed = all_tests_passed .and. test_compress_ranges_singles()
    
    ! Test 3: Format percentage with 2 decimal places
    all_tests_passed = all_tests_passed .and. test_format_percentage_precision()
    
    ! Test 4: Format 100% coverage
    all_tests_passed = all_tests_passed .and. test_format_percentage_100()
    
    ! Test 5: Split string by delimiter
    all_tests_passed = all_tests_passed .and. test_split_string()
    
    ! Test 6: Trim whitespace
    all_tests_passed = all_tests_passed .and. test_trim_whitespace()
    
    ! Test 7: Handle empty range array
    all_tests_passed = all_tests_passed .and. test_compress_empty_array()
    
    ! Test 8: Format zero percentage
    all_tests_passed = all_tests_passed .and. test_format_percentage_zero()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_compress_ranges_consecutive() result(passed)
        logical :: passed
        integer :: numbers(7)
        character(len=:), allocatable :: result
        character(len=*), parameter :: expected = "1-3, 5-7, 10"
        
        print *, "  Test 1: Compress consecutive line numbers"
        
        numbers = [1, 2, 3, 5, 6, 7, 10]
        result = compress_ranges(numbers)
        
        passed = (result == expected)
        
        if (.not. passed) then
            print *, "    FAILED: Expected '", expected, "', got '", result, "'"
        else
            print *, "    PASSED"
        end if
    end function test_compress_ranges_consecutive

    function test_compress_ranges_singles() result(passed)
        logical :: passed
        integer :: numbers(4)
        character(len=:), allocatable :: result
        character(len=*), parameter :: expected = "1, 3, 5, 7"
        
        print *, "  Test 2: Compress single numbers"
        
        numbers = [1, 3, 5, 7]
        result = compress_ranges(numbers)
        
        passed = (result == expected)
        
        if (.not. passed) then
            print *, "    FAILED: Expected '", expected, "', got '", result, "'"
        else
            print *, "    PASSED"
        end if
    end function test_compress_ranges_singles

    function test_format_percentage_precision() result(passed)
        logical :: passed
        real :: value
        character(len=:), allocatable :: result
        character(len=*), parameter :: expected = "66.67%"
        
        print *, "  Test 3: Format percentage with 2 decimal places"
        
        value = 66.6666667
        result = format_percentage(value, 2)
        
        passed = (result == expected)
        
        if (.not. passed) then
            print *, "    FAILED: Expected '", expected, "', got '", result, "'"
        else
            print *, "    PASSED"
        end if
    end function test_format_percentage_precision

    function test_format_percentage_100() result(passed)
        logical :: passed
        real :: value
        character(len=:), allocatable :: result
        character(len=*), parameter :: expected = "100.00%"
        
        print *, "  Test 4: Format 100% coverage"
        
        value = 100.0
        result = format_percentage(value, 2)
        
        passed = (result == expected)
        
        if (.not. passed) then
            print *, "    FAILED: Expected '", expected, "', got '", result, "'"
        else
            print *, "    PASSED"
        end if
    end function test_format_percentage_100

    function test_split_string() result(passed)
        logical :: passed
        character(len=*), parameter :: input = "path/to/file.f90"
        character(len=*), parameter :: delimiter = "/"
        character(len=:), allocatable :: parts(:)
        
        print *, "  Test 5: Split string by delimiter"
        
        parts = split(input, delimiter)
        
        passed = (size(parts) == 3) .and. &
                 (parts(1) == "path") .and. &
                 (parts(2) == "to") .and. &
                 (parts(3) == "file.f90")
        
        if (.not. passed) then
            print *, "    FAILED: Split failed or incorrect parts"
        else
            print *, "    PASSED"
        end if
    end function test_split_string

    function test_trim_whitespace() result(passed)
        logical :: passed
        character(len=*), parameter :: input = "  hello world  "
        character(len=:), allocatable :: result
        character(len=*), parameter :: expected = "hello world"
        
        print *, "  Test 6: Trim whitespace"
        
        result = trim_string(input)
        
        passed = (result == expected)
        
        if (.not. passed) then
            print *, "    FAILED: Expected '", expected, "', got '", result, "'"
        else
            print *, "    PASSED"
        end if
    end function test_trim_whitespace

    function test_compress_empty_array() result(passed)
        logical :: passed
        integer, allocatable :: numbers(:)
        character(len=:), allocatable :: result
        character(len=*), parameter :: expected = ""
        
        print *, "  Test 7: Handle empty range array"
        
        allocate(numbers(0))
        result = compress_ranges(numbers)
        
        passed = (result == expected)
        
        if (.not. passed) then
            print *, "    FAILED: Expected empty string, got '", result, "'"
        else
            print *, "    PASSED"
        end if
    end function test_compress_empty_array

    function test_format_percentage_zero() result(passed)
        logical :: passed
        real :: value
        character(len=:), allocatable :: result
        character(len=*), parameter :: expected = "0.00%"
        
        print *, "  Test 8: Format zero percentage"
        
        value = 0.0
        result = format_percentage(value, 2)
        
        passed = (result == expected)
        
        if (.not. passed) then
            print *, "    FAILED: Expected '", expected, "', got '", result, "'"
        else
            print *, "    PASSED"
        end if
    end function test_format_percentage_zero

end program test_string_utils