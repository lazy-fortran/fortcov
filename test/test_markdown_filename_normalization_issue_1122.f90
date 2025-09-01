program test_markdown_filename_normalization_issue_1122
    !! Repro for issue #1122: normalize filenames in markdown report

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
    character(len=*), parameter :: raw_name = &
        '0:Source:src//utils/strings_string_utils.f90.f90'
    character(len=*), parameter :: expected_name = &
        'src/utils/strings_string_utils.f90'

    call reset_test_counters()
    call print_test_header('Markdown Reporter Filename Normalization (#1122)')

    ! GIVEN: a coverage file with duplicate slashes and duplicate extension
    call file_init_simple(file, raw_name)
    allocate(files(1))
    files(1) = file
    call data_init_with_files(data, files)

    call opts%init()

    ! WHEN: generating a markdown report
    report = generate_markdown_report(data, opts)

    ! THEN: report contains the normalized filename
    call assert_test(index(report, '| Filename | Stmts | Covered | Cover | Missing |') > 0, &
        'markdown report contains table header', &
        'Expected standard table header present')

    call assert_test(index(report, '| ' // expected_name // ' |') > 0, &
        'normalized filename is rendered without duplicates', &
        'Expected cleaned filename "' // expected_name // '" in report')

    call print_test_summary('MARKDOWN REPORTER FILENAME NORMALIZATION (#1122)')

end program test_markdown_filename_normalization_issue_1122

