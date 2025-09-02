program test_markdown_minimal
    use coverage_data_core, only: coverage_file_t, coverage_data_t, &
                                   file_init_with_lines, data_init_with_files
    use coverage_types, only: coverage_line_t
    use markdown_reporter, only: markdown_report_options_t, generate_markdown_report
    implicit none

    type(coverage_line_t), allocatable :: lines(:)
    type(coverage_file_t) :: file
    type(coverage_file_t), allocatable :: files(:)
    type(coverage_data_t) :: data
    type(markdown_report_options_t) :: opts
    character(len=:), allocatable :: report
    integer :: i

    allocate(lines(5))
    do i = 1, 5
        call lines(i)%init('src/foo.f90', i, merge(1, 0, i <= 3), .true.)
    end do

    call file_init_with_lines(file, '0:Source:src/foo.f90', lines)
    allocate(files(1))
    files(1) = file
    call data_init_with_files(data, files)

    call opts%init()
    report = generate_markdown_report(data, opts)

    if (index(report, '| Filename | Stmts | Covered | Cover | Missing |') <= 0) then
        print *, 'Missing table header'
        stop 1
    end if

    if (index(report, '| src/foo.f90 | 5 | 3 | 60.00% |') <= 0) then
        print *, 'Unexpected row content: ', trim(report)
        stop 2
    end if

    print *, 'OK: markdown minimal'
end program test_markdown_minimal

