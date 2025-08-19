! Test suite for Makefile demo utils module
program test_demo
    use demo_utils
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "Running Makefile Demo Utils Tests"
    write(*,*) "================================="
    
    ! Test string length function
    call test_string_length()
    
    ! Test reverse string function
    call test_reverse_string()
    
    ! Test palindrome function
    call test_is_palindrome()
    
    ! Test word count function
    call test_count_words()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "TESTS FAILED"
        stop 1
    else
        write(*,*) "All tests passed"
    end if

contains

    subroutine test_string_length()
        call assert_int_equal(string_length("hello"), 5, "string_length('hello')")
        call assert_int_equal(string_length(""), 0, "string_length('')")
        call assert_int_equal(string_length("test  "), 4, "string_length('test  ')")
    end subroutine test_string_length

    subroutine test_reverse_string()
        call assert_string_equal(trim(reverse_string("hello")), "olleh", "reverse_string('hello')")
        call assert_string_equal(trim(reverse_string("abc")), "cba", "reverse_string('abc')")
        call assert_string_equal(trim(reverse_string("a")), "a", "reverse_string('a')")
    end subroutine test_reverse_string

    subroutine test_is_palindrome()
        call assert_logical_equal(is_palindrome("racecar"), .true., "is_palindrome('racecar')")
        call assert_logical_equal(is_palindrome("hello"), .false., "is_palindrome('hello')")
        call assert_logical_equal(is_palindrome("a"), .true., "is_palindrome('a')")
        call assert_logical_equal(is_palindrome("race a car"), .true., "is_palindrome('race a car')")
    end subroutine test_is_palindrome

    subroutine test_count_words()
        call assert_int_equal(count_words("hello world"), 2, "count_words('hello world')")
        call assert_int_equal(count_words("one"), 1, "count_words('one')")
        call assert_int_equal(count_words("the quick brown fox"), 4, "count_words('the quick brown fox')")
        call assert_int_equal(count_words(""), 0, "count_words('')")
    end subroutine test_count_words

    subroutine assert_int_equal(actual, expected, test_name)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (actual == expected) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,I0,A,I0)') "✗ ", test_name, " FAILED (expected ", expected, ", got ", actual, ")"
            failed_count = failed_count + 1
        end if
    end subroutine assert_int_equal

    subroutine assert_string_equal(actual, expected, test_name)
        character(len=*), intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (actual == expected) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,A,A,A)') "✗ ", test_name, " FAILED (expected '", expected, "', got '", actual, "')"
            failed_count = failed_count + 1
        end if
    end subroutine assert_string_equal

    subroutine assert_logical_equal(actual, expected, test_name)
        logical, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (actual .eqv. expected) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,L1,A,L1)') "✗ ", test_name, " FAILED (expected ", expected, ", got ", actual, ")"
            failed_count = failed_count + 1
        end if
    end subroutine assert_logical_equal

end program test_demo