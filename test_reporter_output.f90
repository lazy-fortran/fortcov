program test_reporter_output
    use coverage_reporter
    use coverage_model
    implicit none
    
    type(markdown_reporter_t) :: reporter
    type(coverage_data_t) :: test_data
    type(coverage_file_t), allocatable :: files(:)
    type(coverage_line_t), allocatable :: lines1(:), lines2(:)
    logical :: error_flag
    integer :: i
    
    ! Create test data with multiple files
    allocate(lines1(5))
    lines1(1) = coverage_line_t(10, 1, "src/module1.f90", .true.)
    lines1(2) = coverage_line_t(0, 2, "src/module1.f90", .true.)
    lines1(3) = coverage_line_t(5, 3, "src/module1.f90", .true.)
    lines1(4) = coverage_line_t(0, 4, "src/module1.f90", .false.) ! Not executable
    lines1(5) = coverage_line_t(3, 5, "src/module1.f90", .true.)
    
    allocate(lines2(3))
    lines2(1) = coverage_line_t(15, 1, "src/utils.f90", .true.)
    lines2(2) = coverage_line_t(20, 2, "src/utils.f90", .true.)
    lines2(3) = coverage_line_t(8, 3, "src/utils.f90", .true.)
    
    allocate(files(2))
    files(1) = coverage_file_t("src/module1.f90", lines1)
    files(2) = coverage_file_t("src/utils.f90", lines2)
    
    test_data = coverage_data_t(files=files)
    
    ! Generate report to file
    call reporter%generate_report(test_data, "test_coverage_report.md", error_flag)
    
    if (error_flag) then
        print *, "ERROR: Failed to generate report"
    else
        print *, "Report generated successfully to test_coverage_report.md"
    end if
    
end program test_reporter_output
