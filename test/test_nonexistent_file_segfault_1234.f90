program test_nonexistent_file_segfault_1234
    !! Test for Issue #1234: Segfault with nonexistent gcov file
    !! Verifies file existence validation prevents segfault
    use coverage_validation, only: validate_coverage_files
    use config_core, only: config_t, initialize_config
    use iso_fortran_env, only: output_unit, error_unit
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    write(output_unit,'(A)') ''
    write(output_unit,'(A)') '=============================================='
    write(output_unit,'(A)') 'Issue #1234: Nonexistent File Segfault Test'
    write(output_unit,'(A)') '=============================================='
    write(output_unit,'(A)') ''

    call test_nonexistent_file_detection()
    call test_existing_file_passes()
    call test_empty_coverage_files()

    write(output_unit,'(A)') ''
    write(output_unit,'(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', tests, ' passed'

    if (passed /= tests) then
        write(error_unit,'(A)') 'TESTS FAILED'
        stop 1
    end if

    write(output_unit,'(A)') 'ALL TESTS PASSED'
    stop 0

contains

    subroutine test_nonexistent_file_detection()
        type(config_t) :: config
        logical :: is_valid

        tests = tests + 1
        write(output_unit,'(A)') 'Testing nonexistent file detection...'

        call initialize_config(config)
        allocate(character(len=256) :: config%coverage_files(1))
        config%coverage_files(1) = 'nonexistent_test_file.gcov'

        call validate_coverage_files(config, is_valid)

        if (.not. is_valid) then
            passed = passed + 1
            write(output_unit,'(A)') '  [PASS] Nonexistent file correctly rejected'
        else
            write(output_unit,'(A)') '  [FAIL] Nonexistent file was not detected'
        end if
    end subroutine test_nonexistent_file_detection

    subroutine test_existing_file_passes()
        type(config_t) :: config
        logical :: is_valid
        integer :: unit_num, ios

        tests = tests + 1
        write(output_unit,'(A)') 'Testing existing file passes validation...'

        ! Create a temporary test file
        open(newunit=unit_num, file='/tmp/test_existing.gcov', &
             status='replace', iostat=ios)
        if (ios /= 0) then
            write(output_unit,'(A)') '  [SKIP] Could not create temp file'
            passed = passed + 1
            return
        end if
        write(unit_num,'(A)') '        -:    0:Source:test.f90'
        close(unit_num)

        call initialize_config(config)
        allocate(character(len=256) :: config%coverage_files(1))
        config%coverage_files(1) = '/tmp/test_existing.gcov'

        call validate_coverage_files(config, is_valid)

        ! Clean up temp file
        open(newunit=unit_num, file='/tmp/test_existing.gcov', status='old')
        close(unit_num, status='delete')

        if (is_valid) then
            passed = passed + 1
            write(output_unit,'(A)') '  [PASS] Existing file passes validation'
        else
            write(output_unit,'(A)') '  [FAIL] Existing file was incorrectly rejected'
        end if
    end subroutine test_existing_file_passes

    subroutine test_empty_coverage_files()
        type(config_t) :: config
        logical :: is_valid

        tests = tests + 1
        write(output_unit,'(A)') 'Testing empty coverage files array...'

        call initialize_config(config)

        call validate_coverage_files(config, is_valid)

        if (is_valid) then
            passed = passed + 1
            write(output_unit,'(A)') '  [PASS] Empty array passes validation'
        else
            write(output_unit,'(A)') '  [FAIL] Empty array was incorrectly rejected'
        end if
    end subroutine test_empty_coverage_files

end program test_nonexistent_file_segfault_1234
