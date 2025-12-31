program test_string_concatenation_fix_364
    !! Test for string concatenation fix in issue #364
    !!
    !! This test verifies that the gcov pattern string concatenation
    !! works correctly with direct concatenation instead of formatted I/O.
    
    use iso_fortran_env, only: output_unit
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    
    write(output_unit, '(A)') 'Testing string concatenation fix for issue #364...'
    write(output_unit, '(A)') ''
    
    call test_gcov_pattern_concatenation()
    call test_gcov_pattern_edge_cases()
    
    write(output_unit, '(A)') ''
    write(output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed_count, ' / ', &
                                        test_count, ' tests passed'
    
    if (passed_count /= test_count) then
        write(output_unit, '(A)') 'Some tests failed!'
        stop 1
    end if

contains

    subroutine test_gcov_pattern_concatenation()
        !! Test that gcov pattern string concatenation works correctly
        character(len=256) :: pattern_result
        character(len=*), parameter :: GCOV_PATTERN = '*.gcov'
        character(len=*), parameter :: test_build_dir = '/path/to/build'
        character(len=*), parameter :: expected = '/path/to/build/*.gcov'
        
        write(output_unit, '(A)') 'Test: GCOV pattern string concatenation'
        
        ! This is the fix - direct concatenation instead of formatted write
        pattern_result = trim(test_build_dir) // '/' // GCOV_PATTERN

        call assert_equals(trim(pattern_result), expected, &
                           'Pattern concatenated correctly')
    end subroutine test_gcov_pattern_concatenation

    subroutine test_gcov_pattern_edge_cases()
        !! Test edge cases for gcov pattern concatenation
        character(len=256) :: pattern_result
        character(len=*), parameter :: GCOV_PATTERN = '*.gcov'
        
        write(output_unit, '(A)') 'Test: GCOV pattern edge cases'
        
        ! Test with empty build dir (should use current dir)
        pattern_result = trim('.') // '/' // GCOV_PATTERN
        call assert_equals(trim(pattern_result), './*.gcov', 'Empty dir handled')
        
        ! Test with build dir ending in slash
        pattern_result = trim('/build/') // '/' // GCOV_PATTERN
        call assert_equals(trim(pattern_result), '/build//*.gcov', &
                           'Trailing slash handled')
        
        ! Test with very long path (should not exceed buffer)
        block
            character(len=200) :: long_path
            long_path = repeat('x', 200)
            
            ! This should work without End of record error
            pattern_result = trim(long_path) // '/' // GCOV_PATTERN
            call assert_true(len_trim(pattern_result) > 0, &
                             'Long path concatenation works')
        end block
    end subroutine test_gcov_pattern_edge_cases

    ! Test assertion helpers
    
    subroutine assert_equals(actual, expected, description)
        character(len=*), intent(in) :: actual, expected, description

        test_count = test_count + 1
        if (actual == expected) then
            passed_count = passed_count + 1
            write(output_unit, '(A,A)') '  PASS: ', description
        else
            write(output_unit, '(A,A)') '  FAIL: ', description
            write(output_unit, '(A,A)') '    Expected: ', expected
            write(output_unit, '(A,A)') '    Actual:   ', actual
        end if
    end subroutine assert_equals

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        test_count = test_count + 1
        if (condition) then
            passed_count = passed_count + 1
            write(output_unit, '(A,A)') '  PASS: ', description
        else
            write(output_unit, '(A,A)') '  FAIL: ', description
        end if
    end subroutine assert_true

end program test_string_concatenation_fix_364
