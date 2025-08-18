program test_coverage_reporter
    use coverage_reporter
    use coverage_model
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Coverage Reporter Abstraction..."
    
    ! Test 1: Abstract reporter interface compliance (tested by compilation)
    all_tests_passed = all_tests_passed .and. test_interface_compliance()
    
    ! Test 2: Reporter identifies format name
    all_tests_passed = all_tests_passed .and. test_format_name()
    
    ! Test 3: Reporter diff support check
    all_tests_passed = all_tests_passed .and. test_diff_support()
    
    ! Test 4: Reporter factory creates markdown reporter
    all_tests_passed = all_tests_passed .and. test_factory_create_markdown()
    
    ! Test 5: Reporter factory handles unknown format
    all_tests_passed = all_tests_passed .and. test_factory_unknown_format()
    
    ! Test 6: Mock reporter for testing
    all_tests_passed = all_tests_passed .and. test_mock_reporter()
    
    ! Test 7: Reporter output path handling
    all_tests_passed = all_tests_passed .and. test_output_path_handling()
    
    ! Test 8: Reporter stdout output
    all_tests_passed = all_tests_passed .and. test_stdout_output()
    
    ! Test 9: Markdown content validation (no DRY violation)
    all_tests_passed = all_tests_passed .and. test_markdown_content_validation()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_interface_compliance() result(passed)
        logical :: passed
        type(mock_reporter_t) :: reporter
        
        print *, "  Test 1: Abstract reporter interface compliance"
        
        ! Given: A concrete reporter implementation (mock_reporter)
        ! When: Extending coverage_reporter_t
        ! Then: Must implement generate_report(), get_format_name(), supports_diff()
        
        ! This test passes if the code compiles (interface compliance)
        passed = .true.
        
        print *, "    PASSED - Mock reporter implements required interface"
    end function test_interface_compliance

    function test_format_name() result(passed)
        logical :: passed
        type(markdown_reporter_t) :: reporter
        character(len=:), allocatable :: format_name
        
        print *, "  Test 2: Reporter identifies format name"
        
        ! Given: A markdown_reporter instance
        ! When: Calling get_format_name()
        ! Then: Should return "markdown"
        format_name = reporter%get_format_name()
        passed = (trim(format_name) == "markdown")
        
        if (.not. passed) then
            print *, "    FAILED: Expected 'markdown', got '", format_name, "'"
        else
            print *, "    PASSED"
        end if
    end function test_format_name

    function test_diff_support() result(passed)
        logical :: passed
        type(markdown_reporter_t) :: reporter
        logical :: supports_diff
        
        print *, "  Test 3: Reporter diff support check"
        
        ! Given: A markdown_reporter instance
        ! When: Calling supports_diff()
        ! Then: Should return .true. or .false. based on capability
        supports_diff = reporter%supports_diff()
        
        ! The test passes as long as it returns a boolean (no specific requirement)
        passed = .true.
        
        print *, "    PASSED - Diff support:", supports_diff
    end function test_diff_support

    function test_factory_create_markdown() result(passed)
        logical :: passed
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag
        character(len=:), allocatable :: format_name
        
        print *, "  Test 4: Reporter factory creates markdown reporter"
        
        ! Given: Format string "markdown"
        ! When: Calling create_reporter(format)
        ! Then: Should return markdown_reporter instance
        call create_reporter("markdown", reporter, error_flag)
        
        passed = (.not. error_flag) .and. allocated(reporter)
        if (passed) then
            format_name = reporter%get_format_name()
            passed = (trim(format_name) == "markdown")
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Factory should create markdown reporter"
            print *, "    Error flag:", error_flag
            print *, "    Allocated:", allocated(reporter)
        else
            print *, "    PASSED"
        end if
    end function test_factory_create_markdown

    function test_factory_unknown_format() result(passed)
        logical :: passed
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag
        
        print *, "  Test 5: Reporter factory handles unknown format"
        
        ! Given: Format string "unknown"
        ! When: Calling create_reporter(format)
        ! Then: Should return null with error
        call create_reporter("unknown", reporter, error_flag)
        
        passed = error_flag .and. (.not. allocated(reporter))
        
        if (.not. passed) then
            print *, "    FAILED: Factory should fail for unknown format"
            print *, "    Error flag:", error_flag
            print *, "    Allocated:", allocated(reporter)
        else
            print *, "    PASSED"
        end if
    end function test_factory_unknown_format

    function test_mock_reporter() result(passed)
        logical :: passed
        type(mock_reporter_t) :: reporter
        type(coverage_data_t) :: test_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        logical :: error_flag
        
        print *, "  Test 6: Mock reporter for testing"
        
        ! Given: A test_reporter that captures calls
        allocate(lines(1))
        lines(1) = coverage_line_t(execution_count=5, line_number=10, &
                                   filename="test.f90", is_executable=.true.)
        
        allocate(files(1))
        files(1) = coverage_file_t(filename="test.f90", lines=lines)
        
        test_data = coverage_data_t(files=files)
        
        ! When: Calling generate_report()
        call reporter%generate_report(test_data, "mock_output.txt", error_flag, .false.)
        
        ! Then: Should record the coverage_data passed
        passed = (.not. error_flag) .and. reporter%was_called
        if (passed) then
            passed = (size(reporter%captured_data%files) == 1)
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Mock reporter should capture call data"
            print *, "    Error flag:", error_flag
            print *, "    Was called:", reporter%was_called
        else
            print *, "    PASSED"
        end if
    end function test_mock_reporter

    function test_output_path_handling() result(passed)
        logical :: passed
        type(markdown_reporter_t) :: reporter
        type(coverage_data_t) :: test_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        logical :: error_flag
        character(len=*), parameter :: test_file = "test_output.md"
        logical :: file_exists
        integer :: unit, iostat
        
        print *, "  Test 7: Reporter output path handling"
        
        ! Clean up any existing test file
        open(newunit=unit, file=test_file, iostat=iostat)
        if (iostat == 0) close(unit, status='delete')
        
        ! Create test data
        allocate(lines(1))
        lines(1) = coverage_line_t(execution_count=3, line_number=5, &
                                   filename="test.f90", is_executable=.true.)
        
        allocate(files(1))
        files(1) = coverage_file_t(filename="test.f90", lines=lines)
        
        test_data = coverage_data_t(files=files)
        
        ! Given: Output path "test_output.md"
        ! When: Calling generate_report()
        call reporter%generate_report(test_data, test_file, error_flag, .false.)
        
        ! Then: Should create file at specified path
        inquire(file=test_file, exist=file_exists)
        passed = (.not. error_flag) .and. file_exists
        
        ! Clean up
        if (file_exists) then
            open(newunit=unit, file=test_file, status='old')
            close(unit, status='delete')
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Should create output file"
            print *, "    Error flag:", error_flag
            print *, "    File exists:", file_exists
        else
            print *, "    PASSED"
        end if
    end function test_output_path_handling

    function test_stdout_output() result(passed)
        logical :: passed
        type(markdown_reporter_t) :: reporter
        type(coverage_data_t) :: test_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        logical :: error_flag
        
        print *, "  Test 8: Reporter stdout output"
        
        ! Create test data
        allocate(lines(1))
        lines(1) = coverage_line_t(execution_count=7, line_number=3, &
                                   filename="stdout_test.f90", is_executable=.true.)
        
        allocate(files(1))
        files(1) = coverage_file_t(filename="stdout_test.f90", lines=lines)
        
        test_data = coverage_data_t(files=files)
        
        ! Given: Output path "-" (stdout indicator)
        ! When: Calling generate_report()
        call reporter%generate_report(test_data, "-", error_flag, .false.)
        
        ! Then: Should write to standard output (and not error)
        passed = .not. error_flag
        
        if (.not. passed) then
            print *, "    FAILED: Stdout output should not error"
            print *, "    Error flag:", error_flag
        else
            print *, "    PASSED"
        end if
    end function test_stdout_output

    function test_markdown_content_validation() result(passed)
        logical :: passed
        type(markdown_reporter_t) :: reporter
        type(coverage_data_t) :: test_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        logical :: error_flag
        character(len=*), parameter :: test_file = "content_validation.md"
        character(len=1000) :: content
        integer :: unit, iostat
        real :: expected_coverage
        logical :: file_exists
        
        print *, "  Test 9: Markdown content validation (no DRY violation)"
        
        ! Clean up any existing test file
        open(newunit=unit, file=test_file, iostat=iostat)
        if (iostat == 0) close(unit, status='delete')
        
        ! Create test data with known coverage including non-executable lines
        allocate(lines(5))
        lines(1) = coverage_line_t(execution_count=5, line_number=10, &
                                   filename="test.f90", is_executable=.true.)
        lines(2) = coverage_line_t(execution_count=0, line_number=11, &
                                   filename="test.f90", is_executable=.true.)
        lines(3) = coverage_line_t(execution_count=0, line_number=12, &
                                   filename="test.f90", is_executable=.false.) ! Comment line
        lines(4) = coverage_line_t(execution_count=3, line_number=13, &
                                   filename="test.f90", is_executable=.true.)
        lines(5) = coverage_line_t(execution_count=0, line_number=14, &
                                   filename="test.f90", is_executable=.false.) ! Comment line
        
        allocate(files(1))
        files(1) = coverage_file_t(filename="test.f90", lines=lines)
        test_data = coverage_data_t(files=files)
        
        ! Expected: 2 out of 3 executable lines covered = 66.67%
        expected_coverage = files(1)%get_line_coverage_percentage()
        
        ! Generate report
        call reporter%generate_report(test_data, test_file, error_flag, .false.)
        
        ! Read and validate content
        inquire(file=test_file, exist=file_exists)
        passed = (.not. error_flag) .and. file_exists
        
        if (passed) then
            open(newunit=unit, file=test_file, status='old')
            ! Skip header lines
            read(unit, '(A)')  ! "# Coverage Report"
            read(unit, '(A)')  ! empty line
            read(unit, '(A)')  ! table header
            read(unit, '(A)')  ! table separator
            read(unit, '(A)') content  ! data line
            close(unit)
            
            ! Check that only executable lines are counted (3 total, not 5)
            ! This will detect the DRY violation where manual counting differs
            passed = (index(content, " 3 |") > 0) .and. &
                     (index(content, " 2 |") > 0) .and. &
                     ((index(content, "66.7") > 0) .or. (index(content, "66.6") > 0))
        end if
        
        ! Clean up
        if (file_exists) then
            open(newunit=unit, file=test_file, status='old', iostat=iostat)
            if (iostat == 0) close(unit, status='delete')
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Markdown should show 3 total, 2 covered, ~66.7%"
            print *, "    Content:", trim(content)
        else
            print *, "    PASSED - Markdown uses consistent coverage calculation"
        end if
    end function test_markdown_content_validation
    
    function format_percentage(value) result(formatted)
        real, intent(in) :: value
        character(len=10) :: formatted
        write(formatted, '(F5.1)') value
        formatted = adjustl(formatted)
    end function format_percentage

end program test_coverage_reporter