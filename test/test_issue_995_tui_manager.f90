program test_issue_995_tui_manager
    !! Tests for Issue #995 - TUI manager resource cleanup and error boundaries
    use iso_fortran_env, only: real64
    use report_config_core, only: terminal_session_t
    use tui_manager_core,   only: start_interactive_tui
    implicit none

    type(terminal_session_t) :: session
    character(len=:), allocatable :: err
    logical :: ok
    real(real64) :: t0, t1, elapsed

    print *, "=========================================="
    print *, " Issue #995: TUI Manager Robustness Tests"
    print *, "=========================================="

    ! Test 1: Configurable timeout honored (no 30s hard limit)
    print *, "Test 1: Configurable max runtime honored"
    call session%init()
    session%is_active = .true.
    session%colors_enabled = .false.
    call cpu_time(t0)
    call start_interactive_tui(session, "Test content", ok, err, 0.05_real64)
    call cpu_time(t1)
    elapsed = t1 - t0
    if (ok) then
        print *, "  PASS: start_interactive_tui returned successfully"
    else
        print *, "  FAIL: start_interactive_tui failed: ", trim(err)
    end if
    if (elapsed < 1.0_real64) then
        print *, "  PASS: Runtime bounded (", elapsed, "s)"
    else
        print *, "  FAIL: Runtime too long (", elapsed, "s)"
    end if

    ! Test 2: Session state verification prevents unsafe startup
    print *, "Test 2: Session state verification"
    call session%cleanup()
    session%is_active = .false.
    session%colors_enabled = .false.
    call start_interactive_tui(session, "Test content", ok, err, 0.01_real64)
    if (.not. ok) then
        print *, "  PASS: Inactive session correctly rejected"
    else
        print *, "  FAIL: Inactive session unexpectedly accepted"
    end if

end program test_issue_995_tui_manager

