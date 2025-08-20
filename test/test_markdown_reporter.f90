program test_markdown_reporter
    use coverage_model
    use coverage_statistics
    use markdown_reporter
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running markdown reporter tests..."
    
    ! Core tests for basic functionality
    call test_generate_basic_coverage_table()
    call test_format_coverage_percentages()
    call test_compress_missing_ranges()
    call test_generate_total_row()
    call test_handle_100_percent_coverage()
    call test_handle_zero_coverage()
    call test_table_header_formatting()
    
    ! Report results
    write(*,*) ""
    if (test_count > 0) then
        write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
                   pass_count, " (", (pass_count * 100) / test_count, "%)"
    else
        write(*,*) "No tests run"
    end if
    
    if (pass_count /= test_count) then
        stop 1  ! Exit with error code
    end if
    
contains

    subroutine assert(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: expected, actual
        
        test_count = test_count + 1
        if (condition) then
            write(*,'(A,A)') "PASS: ", test_name
            pass_count = pass_count + 1
        else
            write(*,'(A,A)') "FAIL: ", test_name
            write(*,'(A,A)') "  Expected: ", expected
            write(*,'(A,A)') "  Actual:   ", actual
        end if
    end subroutine assert

    ! Test 1: Generate basic coverage table
    subroutine test_generate_basic_coverage_table()
        type(coverage_data_t) :: coverage_data
        type(markdown_report_options_t) :: options
        character(len=:), allocatable :: report
        type(coverage_line_t) :: lines_a(3), lines_b(2)
        type(coverage_file_t) :: files(2)
        
        ! Given: Two files with coverage data
        lines_a(1) = coverage_line_t(execution_count=1, line_number=1, &
                                   filename="file_a.f90", is_executable=.true.)
        lines_a(2) = coverage_line_t(execution_count=0, line_number=2, &
                                   filename="file_a.f90", is_executable=.true.)
        lines_a(3) = coverage_line_t(execution_count=1, line_number=3, &
                                   filename="file_a.f90", is_executable=.true.)
        
        lines_b(1) = coverage_line_t(execution_count=1, line_number=1, &
                                   filename="file_b.f90", is_executable=.true.)
        lines_b(2) = coverage_line_t(execution_count=1, line_number=2, &
                                   filename="file_b.f90", is_executable=.true.)
        
        call files(1)%init("file_a.f90", lines_a)
        call files(2)%init("file_b.f90", lines_b)
        call coverage_data%init(files)
        
        call options%init()
        
        ! When: Generating markdown report
        report = generate_markdown_report(coverage_data, options)
        
        ! Then: Should contain the expected table format
        call assert(index(report, "| Filename ") > 0, &
                   "basic table header", "contains table header", "found")
        call assert(index(report, "file_a.f90") > 0, &
                   "basic table file A", "contains file_a.f90", "found")
        call assert(index(report, "file_b.f90") > 0, &
                   "basic table file B", "contains file_b.f90", "found")
    end subroutine test_generate_basic_coverage_table

    ! Test 2: Format coverage percentages
    subroutine test_format_coverage_percentages()
        type(coverage_data_t) :: coverage_data
        type(markdown_report_options_t) :: options
        character(len=:), allocatable :: report
        type(coverage_line_t) :: lines(20)
        type(coverage_file_t) :: file_cov
        integer :: i
        
        ! Given: File with 17/20 lines covered (85.0%)
        do i = 1, 20
            if (i <= 17) then
                lines(i) = coverage_line_t(execution_count=1, line_number=i, &
                                         filename="test.f90", is_executable=.true.)
            else
                lines(i) = coverage_line_t(execution_count=0, line_number=i, &
                                         filename="test.f90", is_executable=.true.)
            end if
        end do
        
        call file_cov%init("test.f90", lines)
        call coverage_data%init([file_cov])
        
        call options%init()
        
        ! When: Generating report
        report = generate_markdown_report(coverage_data, options)
        
        ! Then: Should format percentage with 2 decimal places
        call assert(index(report, "85.00%") > 0, &
                   "percentage formatting", "85.00%", "found format")
    end subroutine test_format_coverage_percentages

    ! Test 3: Compress missing line ranges
    subroutine test_compress_missing_ranges()
        type(coverage_data_t) :: coverage_data
        type(markdown_report_options_t) :: options
        character(len=:), allocatable :: report
        type(coverage_line_t) :: lines(30)
        type(coverage_file_t) :: file_cov
        integer :: i
        integer :: uncovered_lines(6) = [10, 11, 12, 20, 21, 30]
        
        ! Given: File with specific uncovered lines
        do i = 1, 30
            if (any(uncovered_lines == i)) then
                lines(i) = coverage_line_t(execution_count=0, line_number=i, &
                                         filename="test.f90", is_executable=.true.)
            else
                lines(i) = coverage_line_t(execution_count=1, line_number=i, &
                                         filename="test.f90", is_executable=.true.)
            end if
        end do
        
        call file_cov%init("test.f90", lines)
        call coverage_data%init([file_cov])
        
        call options%init()
        
        ! When: Generating report
        report = generate_markdown_report(coverage_data, options)
        
        ! Then: Should compress missing ranges properly
        call assert(index(report, "10-12, 20-21, 30") > 0, &
                   "compress missing ranges", "10-12, 20-21, 30", "found format")
    end subroutine test_compress_missing_ranges

    ! Test 4: Generate TOTAL row
    subroutine test_generate_total_row()
        type(coverage_data_t) :: coverage_data
        type(markdown_report_options_t) :: options
        character(len=:), allocatable :: report
        type(coverage_line_t) :: lines_a(2), lines_b(3)
        type(coverage_file_t) :: files(2)
        
        ! Given: Two files with different coverage
        lines_a(1) = coverage_line_t(execution_count=1, line_number=1, &
                                   filename="a.f90", is_executable=.true.)
        lines_a(2) = coverage_line_t(execution_count=0, line_number=2, &
                                   filename="a.f90", is_executable=.true.)
        
        lines_b(1) = coverage_line_t(execution_count=1, line_number=1, &
                                   filename="b.f90", is_executable=.true.)
        lines_b(2) = coverage_line_t(execution_count=1, line_number=2, &
                                   filename="b.f90", is_executable=.true.)
        lines_b(3) = coverage_line_t(execution_count=1, line_number=3, &
                                   filename="b.f90", is_executable=.true.)
        
        call files(1)%init("a.f90", lines_a)
        call files(2)%init("b.f90", lines_b)
        call coverage_data%init(files)
        
        call options%init()
        
        ! When: Generating report
        report = generate_markdown_report(coverage_data, options)
        
        ! Then: Should contain TOTAL row
        call assert(index(report, "TOTAL") > 0, &
                   "total row present", "contains TOTAL", "found")
    end subroutine test_generate_total_row

    ! Test 5: Handle 100% coverage
    subroutine test_handle_100_percent_coverage()
        type(coverage_data_t) :: coverage_data
        type(markdown_report_options_t) :: options
        character(len=:), allocatable :: report
        type(coverage_line_t) :: lines(3)
        type(coverage_file_t) :: file_cov
        integer :: i
        
        ! Given: File with 100% coverage
        do i = 1, 3
            lines(i) = coverage_line_t(execution_count=1, line_number=i, &
                                     filename="perfect.f90", is_executable=.true.)
        end do
        
        call file_cov%init("perfect.f90", lines)
        call coverage_data%init([file_cov])
        
        call options%init()
        
        ! When: Generating report
        report = generate_markdown_report(coverage_data, options)
        
        ! Then: Should show 100% and empty missing column
        call assert(index(report, "100.00%") > 0, &
                   "100% coverage", "100.00%", "found")
    end subroutine test_handle_100_percent_coverage

    ! Test 6: Handle 0% coverage
    subroutine test_handle_zero_coverage()
        type(coverage_data_t) :: coverage_data
        type(markdown_report_options_t) :: options
        character(len=:), allocatable :: report
        type(coverage_line_t) :: lines(3)
        type(coverage_file_t) :: file_cov
        integer :: i
        
        ! Given: File with 0% coverage
        do i = 1, 3
            lines(i) = coverage_line_t(execution_count=0, line_number=i, &
                                     filename="uncovered.f90", is_executable=.true.)
        end do
        
        call file_cov%init("uncovered.f90", lines)
        call coverage_data%init([file_cov])
        
        call options%init()
        
        ! When: Generating report
        report = generate_markdown_report(coverage_data, options)
        
        ! Then: Should show 0% and list all missing lines
        call assert(index(report, "0.00%") > 0, &
                   "0% coverage", "0.00%", "found")
        call assert(index(report, "1-3") > 0, &
                   "all lines missing", "1-3", "found")
    end subroutine test_handle_zero_coverage

    ! Test 7: Table header formatting
    subroutine test_table_header_formatting()
        type(coverage_data_t) :: coverage_data
        type(markdown_report_options_t) :: options
        character(len=:), allocatable :: report
        type(coverage_line_t) :: line
        type(coverage_file_t) :: file_cov
        
        ! Given: Simple coverage data
        line = coverage_line_t(execution_count=1, line_number=1, &
                              filename="test.f90", is_executable=.true.)
        call file_cov%init("test.f90", [line])
        call coverage_data%init([file_cov])
        
        call options%init()
        
        ! When: Generating report
        report = generate_markdown_report(coverage_data, options)
        
        ! Then: Should contain header and separator
        call assert(index(report, "| Filename ") > 0, &
                   "table header", "contains header", "found")
        call assert(index(report, "|-------") > 0, &
                   "table separator", "contains separator", "found")
    end subroutine test_table_header_formatting

end program test_markdown_reporter