program test_report_engine_tui
    use report_engine
    use coverage_model
    implicit none
    
    logical :: test_passed = .true.
    
    print *, "Running report engine TUI integration tests..."
    
    call test_tui_integration_non_interactive()
    call test_tui_initialization_cleanup()
    
    if (test_passed) then
        print *, "All report engine TUI tests passed"
        call exit(0)
    else
        print *, "Some report engine TUI tests failed"
        call exit(1)
    end if

contains

    ! Test TUI integration in non-interactive mode
    subroutine test_tui_integration_non_interactive()
        type(report_engine_t) :: engine
        type(terminal_session_t) :: session
        logical :: success
        character(len=:), allocatable :: error_msg
        type(coverage_file_t) :: test_file
        
        print *, "Test: TUI integration non-interactive mode..."
        
        ! Initialize engine
        call engine%init(success, error_msg)
        if (.not. success) then
            print *, "FAIL: Engine initialization failed:", error_msg
            test_passed = .false.
            return
        end if
        
        ! Add some test data to avoid empty file list error
        block
            type(coverage_line_t), allocatable :: test_lines(:)
            allocate(test_lines(0))
            call test_file%init("test.f90")
        test_file%lines = test_lines
        end block
        call engine%source_data%init([test_file])
        
        ! Launch terminal browser in non-interactive mode
        call engine%launch_terminal_browser(session, .false., success, error_msg)
        
        if (.not. success) then
            print *, "FAIL: Terminal browser launch failed:", error_msg
            test_passed = .false.
            return
        end if
        
        ! Verify session properties
        if (.not. session%is_active) then
            print *, "FAIL: Session should be active"
            test_passed = .false.
            return
        end if
        
        if (.not. allocated(session%display_buffer)) then
            print *, "FAIL: Display buffer should be allocated"
            test_passed = .false.
            return
        end if
        
        call session%cleanup()
        print *, "PASS: TUI integration non-interactive mode"
    end subroutine test_tui_integration_non_interactive
    
    ! Test TUI initialization and cleanup
    subroutine test_tui_initialization_cleanup()
        type(report_engine_t) :: engine
        type(terminal_session_t) :: session
        logical :: success
        character(len=:), allocatable :: error_msg
        
        print *, "Test: TUI initialization and cleanup..."
        
        ! Initialize engine
        call engine%init(success, error_msg)
        if (.not. success) then
            print *, "FAIL: Engine initialization failed:", error_msg
            test_passed = .false.
            return
        end if
        
        ! Initialize session
        call session%init()
        
        ! Verify initial state
        if (session%is_active) then
            print *, "FAIL: Session should not be active initially"
            test_passed = .false.
            return
        end if
        
        if (session%terminal_width /= 80) then
            print *, "FAIL: Default terminal width should be 80"
            test_passed = .false.
            return
        end if
        
        if (session%terminal_height /= 24) then
            print *, "FAIL: Default terminal height should be 24"
            test_passed = .false.
            return
        end if
        
        ! Test cleanup
        call session%cleanup()
        
        if (session%is_active) then
            print *, "FAIL: Session should be inactive after cleanup"
            test_passed = .false.
            return
        end if
        
        print *, "PASS: TUI initialization and cleanup"
    end subroutine test_tui_initialization_cleanup

end program test_report_engine_tui