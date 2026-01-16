program test_source_path_file_detection
    !! Source path classification: file vs directory.
    !! Regression test for issue 1237.
    use coverage_analysis_core, only: is_source_file_path
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    write(output_unit,'(A)') ''
    write(output_unit,'(A)') '=============================================='
    write(output_unit,'(A)') 'Issue #1237: Source File Path Handling Test'
    write(output_unit,'(A)') '=============================================='
    write(output_unit,'(A)') ''

    call test_is_source_file_path_fortran()
    call test_is_source_file_path_c()
    call test_is_source_file_path_other()
    call test_is_source_file_path_directories()
    call test_is_source_file_path_edge_cases()

    write(output_unit,'(A)') ''
    write(output_unit,'(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', tests, ' passed'

    if (passed /= tests) then
        write(error_unit,'(A)') 'TESTS FAILED'
        stop 1
    end if

    write(output_unit,'(A)') 'ALL TESTS PASSED'
    stop 0

contains

    subroutine test_is_source_file_path_fortran()
        tests = tests + 1
        write(output_unit,'(A)') 'Testing Fortran file extensions...'

        if (is_source_file_path('src/demo.f90') .and. &
            is_source_file_path('test.F90') .and. &
            is_source_file_path('lib/module.f95') .and. &
            is_source_file_path('old/legacy.f') .and. &
            is_source_file_path('src/main.f08')) then
            passed = passed + 1
            write(output_unit,'(A)') '  [PASS] Fortran extensions recognized'
        else
            write(output_unit,'(A)') '  [FAIL] Fortran extensions not recognized'
        end if
    end subroutine test_is_source_file_path_fortran

    subroutine test_is_source_file_path_c()
        tests = tests + 1
        write(output_unit,'(A)') 'Testing C/C++ file extensions...'

        if (is_source_file_path('src/main.c') .and. &
            is_source_file_path('include/header.h') .and. &
            is_source_file_path('src/class.cpp') .and. &
            is_source_file_path('inc/template.hpp')) then
            passed = passed + 1
            write(output_unit,'(A)') '  [PASS] C/C++ extensions recognized'
        else
            write(output_unit,'(A)') '  [FAIL] C/C++ extensions not recognized'
        end if
    end subroutine test_is_source_file_path_c

    subroutine test_is_source_file_path_other()
        tests = tests + 1
        write(output_unit,'(A)') 'Testing other language extensions...'

        if (is_source_file_path('script.py') .and. &
            is_source_file_path('app.js') .and. &
            is_source_file_path('lib.rs') .and. &
            is_source_file_path('Main.java')) then
            passed = passed + 1
            write(output_unit,'(A)') '  [PASS] Other language extensions recognized'
        else
            write(output_unit,'(A)') '  [FAIL] Other language extensions not recognized'
        end if
    end subroutine test_is_source_file_path_other

    subroutine test_is_source_file_path_directories()
        tests = tests + 1
        write(output_unit,'(A)') &
            'Testing directory paths are not recognized as files...'

        if (.not. is_source_file_path('src/') .and. &
            .not. is_source_file_path('lib/subdir/') .and. &
            .not. is_source_file_path('build') .and. &
            .not. is_source_file_path('src/subdir')) then
            passed = passed + 1
            write(output_unit,'(A)') '  [PASS] Directories not recognized as files'
        else
            write(output_unit,'(A)') &
                '  [FAIL] Directories incorrectly recognized as files'
        end if
    end subroutine test_is_source_file_path_directories

    subroutine test_is_source_file_path_edge_cases()
        tests = tests + 1
        write(output_unit,'(A)') 'Testing edge cases...'

        if (.not. is_source_file_path('') .and. &
            .not. is_source_file_path('.') .and. &
            .not. is_source_file_path('..') .and. &
            .not. is_source_file_path('file.unknown') .and. &
            .not. is_source_file_path('src/.hidden') .and. &
            .not. is_source_file_path('README')) then
            passed = passed + 1
            write(output_unit,'(A)') '  [PASS] Edge cases handled correctly'
        else
            write(output_unit,'(A)') '  [FAIL] Edge cases not handled correctly'
        end if
    end subroutine test_is_source_file_path_edge_cases

end program test_source_path_file_detection
