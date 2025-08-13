program test_md_output
    use coverage_reporter
    use coverage_model
    implicit none
    
    type(markdown_reporter_t) :: reporter
    type(coverage_data_t) :: test_data
    type(coverage_file_t), allocatable :: files(:)
    type(coverage_line_t), allocatable :: lines(:)
    logical :: error_flag
    
    ! Test with empty coverage (0%)
    allocate(lines(3))
    lines(1) = coverage_line_t(0, 1, "test.f90", .true.)
    lines(2) = coverage_line_t(0, 2, "test.f90", .true.)
    lines(3) = coverage_line_t(0, 3, "test.f90", .true.)
    
    allocate(files(1))
    files(1) = coverage_file_t("test.f90", lines)
    
    test_data = coverage_data_t(files=files)
    
    call reporter%generate_report(test_data, "test_zero.md", error_flag)
    
    ! Test with no executable lines
    deallocate(lines)
    allocate(lines(2))
    lines(1) = coverage_line_t(0, 1, "comment.f90", .false.)
    lines(2) = coverage_line_t(0, 2, "comment.f90", .false.)
    
    files(1) = coverage_file_t("comment.f90", lines)
    test_data = coverage_data_t(files=files)
    
    call reporter%generate_report(test_data, "test_no_exec.md", error_flag)
    
end program test_md_output
