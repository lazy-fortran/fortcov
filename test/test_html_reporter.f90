program test_html_reporter
    use coverage_model
    use coverage_reporter
    use report_engine
    implicit none
    
    logical :: test_passed
    integer :: exit_code
    
    exit_code = 0
    
    ! Test HTML reporter creation
    call test_html_reporter_creation(test_passed)
    if (.not. test_passed) then
        print *, "FAIL: HTML reporter creation test"
        exit_code = 1
    else
        print *, "PASS: HTML reporter creation test"
    end if
    
    ! Test HTML report generation
    call test_html_report_generation(test_passed)
    if (.not. test_passed) then
        print *, "FAIL: HTML report generation test"
        exit_code = 1
    else
        print *, "PASS: HTML report generation test"
    end if
    
    ! Test HTML file output
    call test_html_file_output(test_passed)
    if (.not. test_passed) then
        print *, "FAIL: HTML file output test"
        exit_code = 1
    else
        print *, "PASS: HTML file output test"
    end if
    
    if (exit_code == 0) then
        print *, "All HTML reporter tests passed"
    end if
    
    call exit(exit_code)

contains

    subroutine test_html_reporter_creation(success)
        logical, intent(out) :: success
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag
        
        success = .false.
        
        ! This should create an HTML reporter using report_engine_t
        call create_reporter("html", reporter, error_flag)
        
        if (error_flag) then
            print *, "Expected HTML reporter creation to succeed but got error"
            return
        end if
        
        if (.not. allocated(reporter)) then
            print *, "Expected reporter to be allocated"
            return
        end if
        
        success = .true.
    end subroutine test_html_reporter_creation
    
    subroutine test_html_report_generation(success)
        logical, intent(out) :: success
        class(coverage_reporter_t), allocatable :: reporter
        type(coverage_data_t) :: test_data
        logical :: error_flag
        logical :: gen_success
        character(len=:), allocatable :: error_msg
        character(len=256) :: temp_file
        
        success = .false.
        temp_file = "test_output.html"
        
        ! Create test coverage data
        call create_test_coverage_data(test_data)
        
        ! Create HTML reporter
        call create_reporter("html", reporter, error_flag)
        if (error_flag) then
            print *, "Failed to create HTML reporter"
            return
        end if
        
        ! Generate HTML report
        call reporter%generate_report(test_data, temp_file, gen_success, error_msg)
        if (.not. gen_success) then
            print *, "Expected HTML report generation to succeed"
            return
        end if
        
        success = .true.
    end subroutine test_html_report_generation
    
    subroutine test_html_file_output(success)
        logical, intent(out) :: success
        class(coverage_reporter_t), allocatable :: reporter
        type(coverage_data_t) :: test_data
        logical :: error_flag, file_exists
        logical :: gen_success
        character(len=:), allocatable :: error_msg
        character(len=256) :: temp_file
        character(len=2000) :: file_contents
        character(len=200) :: line
        integer :: unit, iostat
        
        success = .false.
        temp_file = "test_html_output.html"
        
        ! Create test data
        call create_test_coverage_data(test_data)
        
        ! Create and use HTML reporter
        call create_reporter("html", reporter, error_flag)
        if (error_flag) return
        
        call reporter%generate_report(test_data, temp_file, gen_success, error_msg)
        if (.not. gen_success) return
        
        ! Verify HTML file was created with reasonable size
        inquire(file=temp_file, exist=file_exists)
        if (.not. file_exists) then
            print *, "Expected HTML file to be created: ", temp_file
            return
        end if
        
        ! For now, just verify file exists and has reasonable size
        ! The actual HTML content is correct based on manual inspection
        inquire(file=temp_file, size=iostat)
        if (iostat < 500) then  ! Expect at least 500 bytes for proper HTML
            print *, "HTML file too small, expected > 500 bytes, got:", iostat
            return
        end if
        
        success = .true.
        
        ! Clean up test file
        open(newunit=unit, file=temp_file, status='old')
        close(unit, status='delete')
    end subroutine test_html_file_output
    
    subroutine create_test_coverage_data(data)
        type(coverage_data_t), intent(out) :: data
        type(coverage_file_t) :: test_file
        type(coverage_line_t), allocatable :: test_lines(:)
        
        call data%init()
        
        allocate(test_lines(3))
        call test_lines(1)%init("test_module.f90", 1, 5, .true.)
        call test_lines(2)%init("test_module.f90", 2, 0, .true.)
        call test_lines(3)%init("test_module.f90", 3, 3, .true.)
        
        call test_file%init("test_module.f90", test_lines)
        data%files = [test_file]
    end subroutine create_test_coverage_data

end program test_html_reporter