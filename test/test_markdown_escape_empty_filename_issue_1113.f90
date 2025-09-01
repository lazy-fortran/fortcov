program test_markdown_escape_empty_filename_issue_1113
    !! Repro for issue #1113: escape_markdown should handle empty input

    use iso_fortran_env, only: output_unit
    use test_utils_core, only: assert_test, reset_test_counters, &
                               print_test_header, print_test_summary
    use coverage_data_core, only: coverage_file_t, coverage_data_t, &
                                  file_init_simple, data_init_with_files
    use markdown_reporter, only: markdown_report_options_t, generate_markdown_report
    implicit none

    type(coverage_file_t) :: file
    type(coverage_file_t), allocatable :: files(:)
    type(coverage_data_t) :: data
    type(markdown_report_options_t) :: opts
    character(len=:), allocatable :: report

    call reset_test_counters()
    call print_test_header("Markdown Reporter Escape Empty Filename")

    ! GIVEN: a coverage file with an empty filename
    call file_init_simple(file, "")
    allocate(files(1))
    files(1) = file
    call data_init_with_files(data, files)

    call opts%init()

    ! WHEN: generating a markdown report
    report = generate_markdown_report(data, opts)

    ! THEN: report is produced without runtime errors and has expected header
    call assert_test(len_trim(report) > 0, &
        "generate_markdown_report returns non-empty report for empty filename", &
        "Expected a non-empty markdown report")
    call assert_test(index(report, "| Filename | Stmts | Covered | Cover | Missing |") > 0, &
        "markdown report contains table header", &
        "Expected standard table header present")

    call print_test_summary("MARKDOWN REPORTER ESCAPE EMPTY FILENAME")

end program test_markdown_escape_empty_filename_issue_1113

