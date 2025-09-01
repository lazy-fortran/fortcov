program test_markdown_filename_prefix_only_issue_1122
    !! Repro for issue #1122: strip gcov 0:Source: prefix

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
        '0:Source:src/core/main.f90'
    character(len=*), parameter :: expected_name = &
        'src/core/main.f90'

    call reset_test_counters()
    call print_test_header('Markdown Reporter Prefix Only Normalization (#1122)')

    ! GIVEN: a coverage file with only the gcov prefix present
    call file_init_simple(file, raw_name)
    allocate(files(1))
    files(1) = file
    call data_init_with_files(data, files)

    call opts%init()

    ! WHEN: generating a markdown report
    report = generate_markdown_report(data, opts)

    ! THEN: report contains the normalized filename without the prefix
    call assert_test(index(report, '| ' // expected_name // ' |') > 0, &
        'normalized filename strips 0:Source: prefix', &
        'Expected cleaned filename "' // expected_name // '" in report')

    call print_test_summary('MARKDOWN REPORTER PREFIX NORMALIZATION (#1122)')

end program test_markdown_filename_prefix_only_issue_1122

