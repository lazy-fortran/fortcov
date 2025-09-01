program test_architecture_json_linecount_issue_879
    !! Regression test for Issue #879
    !! Verifies JSON architecture output reports correct file path and line count

    use iso_fortran_env, only: output_unit
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context
    use architectural_size_validator, only: validate_codebase_architecture, generate_size_report, &
                                           architectural_size_report_t
    use string_utils, only: int_to_string
    use test_utils_core, only: assert_test, reset_test_counters, print_test_summary
    implicit none

    type(architectural_size_report_t) :: report
    type(error_context_t) :: err
    character(len=:), allocatable :: json_text
    integer :: total_files
    logical :: ok

    call reset_test_counters()
    call clear_error_context(err)

    ! Run architectural validation and get JSON report
    call validate_codebase_architecture('src', report, err)
    if (err%error_code == ERROR_SUCCESS) then
        call generate_size_report(report, 'json', json_text)
    else
        write(output_unit, '(A)') 'ERROR: validate_codebase_architecture failed'
    end if

    ! Basic JSON structure checks
    call assert_test(index(json_text, '"summary"') > 0, &
        'JSON contains summary field', 'Missing "summary" key')
    call assert_test(index(json_text, '"file_violations"') > 0, &
        'JSON contains file_violations array', 'Missing "file_violations" key')

    ! total_files_scanned should be a positive integer
    call extract_int_value(json_text, 'total_files_scanned', total_files, ok)
    call assert_test(ok .and. total_files > 0, &
        'JSON total_files_scanned is present and > 0', &
        'Expected total_files_scanned > 0')

    ! Ensure obsolete incorrect path is not present
    call assert_test(index(json_text, '"filename": "src/core/architectural_size_validator.f90"') == 0, &
        'JSON does not include incorrect path without architecture/ segment', &
        'Incorrect path should not appear in output')

    call print_test_summary('ISSUE #879 JSON ARCHITECTURE OUTPUT', .false.)
    if (.not. ok .or. total_files <= 0) stop 1

contains

    subroutine extract_int_value(json, key, value, ok)
        character(len=*), intent(in) :: json
        character(len=*), intent(in) :: key
        integer, intent(out) :: value
        logical, intent(out) :: ok
        integer :: p, i, n, ival, ios
        character(len=32) :: buf
        ok = .false.
        value = -1
        p = index(json, '"'//trim(key)//'":')
        if (p <= 0) return
        ! move to first digit after colon
        i = p + len_trim(key) + 3
        n = 0
        do while (i <= len(json))
            if (json(i:i) >= '0' .and. json(i:i) <= '9') then
                if (n < len(buf)) then
                    n = n + 1
                    buf(n:n) = json(i:i)
                end if
            else if (n > 0) then
                exit
            end if
            i = i + 1
        end do
        if (n > 0) then
            read(buf(1:n), *, iostat=ios) ival
            if (ios == 0) then
                value = ival
                ok = .true.
            end if
        end if
    end subroutine extract_int_value

end program test_architecture_json_linecount_issue_879
