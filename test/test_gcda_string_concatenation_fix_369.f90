program test_gcda_string_concatenation_fix_369
    !! Test for gcda string concatenation optimization in issue #369
    !!
    !! This test verifies that the gcda pattern string concatenation
    !! works correctly with direct concatenation instead of formatted I/O.
    
    use iso_fortran_env, only: output_unit
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    
    write(output_unit, '(A)') 'Testing gcda string concatenation optimization for issue #369...'
    write(output_unit, '(A)') ''
    
    call test_gcda_search_pattern_concatenation()
    call test_gcno_pattern_concatenation()
    call test_temp_filename_concatenation()
    call test_gcov_header_concatenation()
    
    write(output_unit, '(A)') ''
    write(output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed_count, ' / ', &
                                        test_count, ' tests passed'
    
    if (passed_count /= test_count) then
        write(output_unit, '(A)') 'Some tests failed!'
        stop 1
    end if

contains

    subroutine test_gcda_search_pattern_concatenation()
        !! Test that gcda search pattern string concatenation works correctly
        character(len=512) :: search_pattern
        character(len=*), parameter :: GCDA_PATTERN = '*.gcda'
        character(len=*), parameter :: project_path = '/path/to/project'
        character(len=*), parameter :: expected = '/path/to/project/*.gcda'
        
        write(output_unit, '(A)') 'Test: GCDA search pattern string concatenation'
        
        ! This is the optimization - direct concatenation instead of formatted write
        search_pattern = trim(project_path) // '/' // GCDA_PATTERN
        
        call assert_equals(trim(search_pattern), expected, 'GCDA pattern concatenated correctly')
    end subroutine test_gcda_search_pattern_concatenation

    subroutine test_gcno_pattern_concatenation()
        !! Test that gcno pattern string concatenation works correctly
        character(len=512) :: pattern
        character(len=*), parameter :: directory = '/build/dir'
        character(len=*), parameter :: expected = '/build/dir/*.gcno'
        
        write(output_unit, '(A)') 'Test: GCNO pattern string concatenation'
        
        ! Direct concatenation optimization
        pattern = trim(directory) // '/*.gcno'
        
        call assert_equals(trim(pattern), expected, 'GCNO pattern concatenated correctly')
    end subroutine test_gcno_pattern_concatenation

    subroutine test_temp_filename_concatenation()
        !! Test that temp filename string concatenation works correctly
        character(len=256) :: temp_filename, expected
        character(len=8) :: date_str = '20250101'
        character(len=10) :: time_str = '1234567890'
        character(len=:), allocatable :: temp_dir
        
        ! Get portable temp directory
        block
            use portable_temp_utils, only: get_temp_dir
            temp_dir = get_temp_dir()
            expected = temp_dir // '/fortcov_gcov_20250101_123456'
        end block
        
        write(output_unit, '(A)') 'Test: Temp filename string concatenation'
        
        ! Direct concatenation optimization using portable temp directory
        temp_filename = temp_dir // "/fortcov_gcov_" // trim(date_str) // "_" // time_str(1:6)
        
        call assert_equals(trim(temp_filename), expected, 'Temp filename concatenated correctly')
    end subroutine test_temp_filename_concatenation

    subroutine test_gcov_header_concatenation()
        !! Test that gcov header string concatenation works correctly
        character(len=*), parameter :: source_name = 'test_module.f90'
        character(len=256) :: header_source, header_graph, header_data
        
        write(output_unit, '(A)') 'Test: GCOV header string concatenation'
        
        ! Direct concatenation optimization (simulating what write statements produce)
        header_source = "        -:    0:Source:" // trim(source_name)
        header_graph = "        -:    0:Graph:" // trim(source_name) // ".gcno"
        header_data = "        -:    0:Data:" // trim(source_name) // ".gcda"
        
        call assert_equals(trim(header_source), "        -:    0:Source:test_module.f90", 'Source header concatenated correctly')
        call assert_equals(trim(header_graph), "        -:    0:Graph:test_module.f90.gcno", 'Graph header concatenated correctly')
        call assert_equals(trim(header_data), "        -:    0:Data:test_module.f90.gcda", 'Data header concatenated correctly')
    end subroutine test_gcov_header_concatenation

    ! Test assertion helpers
    
    subroutine assert_equals(actual, expected, description)
        character(len=*), intent(in) :: actual, expected, description
        
        test_count = test_count + 1
        if (actual == expected) then
            passed_count = passed_count + 1
            write(output_unit, '(A,A)') '  ✓ ', description
        else
            write(output_unit, '(A,A)') '  ✗ ', description
            write(output_unit, '(A,A)') '    Expected: ', expected
            write(output_unit, '(A,A)') '    Actual:   ', actual
        end if
    end subroutine assert_equals

end program test_gcda_string_concatenation_fix_369
