program test_json_escape_trailing_spaces_issue_1109
    !! Verify JSON escape preserves trailing spaces (Issue #1109)
    use size_report_generator, only: architectural_size_report_t, generate_report_in_format
    implicit none

    type(architectural_size_report_t) :: report
    character(len=:), allocatable :: json
    integer :: pos

    ! Prepare minimal report with intentional trailing space
    report%has_violations = .false.
    report%has_warnings = .false.
    report%total_files_scanned = 0
    report%total_directories_scanned = 0
    allocate(report%file_violations(0))
    allocate(report%directory_violations(0))
    report%summary_message = 'ends-with-space '
    report%ci_exit_recommendation = 'EXIT_SUCCESS'

    call generate_report_in_format(report, 'json', json)
    if (.not. allocated(json)) then
        print *, 'ERROR: JSON was not generated'
        stop 1
    end if

    ! Expect summary field to retain the trailing space before closing quote
    pos = index(json, '"summary": "ends-with-space "')
    if (pos == 0) then
        print *, 'ERROR: Trailing space lost in JSON summary field'
        print *, 'JSON:', trim(json)
        stop 1
    end if

    print *, '✅ PASS: Trailing spaces preserved in JSON escape'
end program test_json_escape_trailing_spaces_issue_1109
