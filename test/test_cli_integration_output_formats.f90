program test_cli_integration_output_formats
    use coverage_model
    use coverage_reporter
    use coverage_statistics
    use file_utils
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running CLI integration tests for all output formats..."
    
    ! Test 1: Generate actual reports for all formats
    call test_markdown_report_generation()
    call test_json_report_generation()
    call test_xml_report_generation()
    
    ! Test 2: Verify output format consistency
    call test_format_consistency()
    
    ! Test 3: Test reporter factory completeness
    call test_reporter_factory_coverage()
    
    ! Report results
    write(*,*) ""
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
               pass_count, " (", (pass_count * 100) / test_count, "%)"
    
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
    
    ! Test markdown report generation (Issue #85)
    ! Given: Sample coverage data
    ! When: Generating markdown report
    ! Then: Should create valid markdown output
    subroutine test_markdown_report_generation()
        class(coverage_reporter_t), allocatable :: reporter
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Given: Sample coverage data
        call create_sample_coverage_data(coverage_data)
        
        ! When: Create markdown reporter and generate report
        call create_reporter("markdown", reporter, error_flag)
        if (error_flag) then
            call assert(.false., "markdown reporter creation", "success", "error")
            return
        end if
        
        call reporter%generate_report(coverage_data, "-", error_flag)
        
        ! Then: Should not have error
        call assert(.not. error_flag, "markdown report generation", &
                   "no error", merge("error   ", "no error", error_flag))
        
        ! And: Should have correct format name
        call assert(trim(reporter%get_format_name()) == "markdown", &
                   "markdown format name", "markdown", &
                   trim(reporter%get_format_name()))
    end subroutine test_markdown_report_generation
    
    ! Test JSON report generation (Issue #85)
    ! Given: Sample coverage data
    ! When: Generating JSON report
    ! Then: Should create valid JSON output
    subroutine test_json_report_generation()
        class(coverage_reporter_t), allocatable :: reporter
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Given: Sample coverage data
        call create_sample_coverage_data(coverage_data)
        
        ! When: Create JSON reporter and generate report
        call create_reporter("json", reporter, error_flag)
        if (error_flag) then
            call assert(.false., "json reporter creation", "success", "error")
            return
        end if
        
        call reporter%generate_report(coverage_data, "-", error_flag)
        
        ! Then: Should not have error
        call assert(.not. error_flag, "json report generation", &
                   "no error", merge("error   ", "no error", error_flag))
        
        ! And: Should have correct format name
        call assert(trim(reporter%get_format_name()) == "json", &
                   "json format name", "json", &
                   trim(reporter%get_format_name()))
        
        ! And: Should support diff functionality
        call assert(reporter%supports_diff(), &
                   "json supports diff", "true", &
                   merge("true ", "false", reporter%supports_diff()))
    end subroutine test_json_report_generation
    
    ! Test XML report generation (Issue #85)
    ! Given: Sample coverage data
    ! When: Generating XML report
    ! Then: Should create valid XML output
    subroutine test_xml_report_generation()
        class(coverage_reporter_t), allocatable :: reporter
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Given: Sample coverage data
        call create_sample_coverage_data(coverage_data)
        
        ! When: Create XML reporter and generate report
        call create_reporter("xml", reporter, error_flag)
        if (error_flag) then
            call assert(.false., "xml reporter creation", "success", "error")
            return
        end if
        
        call reporter%generate_report(coverage_data, "-", error_flag)
        
        ! Then: Should not have error
        call assert(.not. error_flag, "xml report generation", &
                   "no error", merge("error   ", "no error", error_flag))
        
        ! And: Should have correct format name
        call assert(trim(reporter%get_format_name()) == "xml", &
                   "xml format name", "xml", &
                   trim(reporter%get_format_name()))
        
        ! And: Should support diff functionality
        call assert(reporter%supports_diff(), &
                   "xml supports diff", "true", &
                   merge("true ", "false", reporter%supports_diff()))
    end subroutine test_xml_report_generation
    
    ! Test format consistency (Issue #85)
    ! Given: All supported formats
    ! When: Generating reports
    ! Then: All should produce valid output with consistent data
    subroutine test_format_consistency()
        class(coverage_reporter_t), allocatable :: md_reporter, json_reporter, &
                                                   xml_reporter
        type(coverage_data_t) :: coverage_data
        logical :: md_error, json_error, xml_error
        type(coverage_stats_t) :: stats
        
        ! Given: Sample coverage data
        call create_sample_coverage_data(coverage_data)
        stats = calculate_line_coverage(coverage_data)
        
        ! When: Create all reporter types
        call create_reporter("markdown", md_reporter, md_error)
        call create_reporter("json", json_reporter, json_error)
        call create_reporter("xml", xml_reporter, xml_error)
        
        ! Then: All should be created successfully
        call assert(.not. md_error .and. .not. json_error .and. &
                   .not. xml_error, &
                   "all reporters created", "all success", &
                   "creation status")
        
        ! And: Generate reports without errors
        if (.not. md_error .and. .not. json_error .and. .not. xml_error) then
            call md_reporter%generate_report(coverage_data, "-", md_error)
            call json_reporter%generate_report(coverage_data, "-", json_error)
            call xml_reporter%generate_report(coverage_data, "-", xml_error)
            
            call assert(.not. md_error .and. .not. json_error .and. &
                       .not. xml_error, &
                       "all reports generated", "all success", &
                       "generation status")
        end if
    end subroutine test_format_consistency
    
    ! Test reporter factory coverage (Issue #85)
    ! Given: All advertised formats and some invalid ones
    ! When: Creating reporters
    ! Then: Valid formats work, invalid ones fail appropriately
    subroutine test_reporter_factory_coverage()
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag
        character(len=20) :: formats(7) = ["markdown ", "md       ", &
                                           "json     ", "xml      ", &
                                           "invalid  ", "html     ", &
                                           "mock     "]
        logical :: expected_results(7) = [.false., .false., .false., .false., &
                                         .true., .false., .false.]
        integer :: i
        logical :: all_correct = .true.
        
        ! Test each format
        do i = 1, size(formats)
            call create_reporter(trim(formats(i)), reporter, error_flag)
            if (error_flag .neqv. expected_results(i)) then
                all_correct = .false.
                exit
            end if
        end do
        
        ! Then: All results should match expectations
        call assert(all_correct, "reporter factory coverage", &
                   "all formats handled correctly", &
                   merge("correct  ", "incorrect", all_correct))
    end subroutine test_reporter_factory_coverage
    
    ! Helper to create sample coverage data for testing
    subroutine create_sample_coverage_data(coverage_data)
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_line_t) :: lines(5)
        type(coverage_file_t) :: file_cov
        
        ! Create sample lines with mixed coverage
        lines(1) = coverage_line_t(execution_count=1, line_number=1, &
                                 filename="sample.f90", is_executable=.true.)
        lines(2) = coverage_line_t(execution_count=0, line_number=2, &
                                 filename="sample.f90", is_executable=.true.)
        lines(3) = coverage_line_t(execution_count=3, line_number=3, &
                                 filename="sample.f90", is_executable=.true.)
        lines(4) = coverage_line_t(execution_count=0, line_number=4, &
                                 filename="sample.f90", is_executable=.false.)
        lines(5) = coverage_line_t(execution_count=2, line_number=5, &
                                 filename="sample.f90", is_executable=.true.)
        
        file_cov = coverage_file_t("sample.f90", lines)
        coverage_data = coverage_data_t([file_cov])
    end subroutine create_sample_coverage_data
    
end program test_cli_integration_output_formats