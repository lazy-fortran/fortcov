program test_tui_main_loop
    use iso_fortran_env, only: real64, int32
    use tui_main_loop
    implicit none
    
    logical :: test_passed = .true.
    
    print *, "Running TUI main loop tests..."
    
    call test_tui_frame_rate_limiting()
    call test_tui_input_handling_timeout()
    call test_tui_escape_sequence_flooding()
    call test_tui_infinite_loop_prevention()
    
    if (test_passed) then
        print *, "All TUI main loop tests passed"
        call exit(0)
    else
        print *, "Some TUI main loop tests failed"
        call exit(1)
    end if

contains

    ! Test that TUI properly limits frame rate to prevent infinite loops
    subroutine test_tui_frame_rate_limiting()
        type(tui_engine_t) :: tui
        type(tui_config_t) :: config
        real(real64) :: start_time, end_time, elapsed_time
        
        print *, "Test: TUI frame rate limiting..."
        
        ! Configure TUI for testing
        call config%init()
        config%debug_mode = .false.
        config%max_iterations = 100  ! Limit for test
        
        call tui%init(config)
        
        call cpu_time(start_time)
        
        ! Run TUI for a short time to test frame rate limiting
        call tui%run_main_loop(.false., 0.2_real64)  ! Non-interactive, 200ms max
        
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        ! Should have completed without excessive CPU usage
        ! Frame rate should be limited to reasonable levels
        if (tui%frame_count > 20 .and. elapsed_time < 0.1_real64) then
            print *, "FAIL: Frame rate too high, potential infinite loop"
            print *, "Frames:", tui%frame_count, "Time:", elapsed_time
            test_passed = .false.
        else
            print *, "PASS: Frame rate limiting working"
            print *, "Frames:", tui%frame_count, "Time:", elapsed_time
        end if
        
        call tui%cleanup()
    end subroutine test_tui_frame_rate_limiting
    
    ! Test that TUI input handling has proper timeout
    subroutine test_tui_input_handling_timeout()
        type(tui_engine_t) :: tui
        type(tui_config_t) :: config
        type(tui_input_event_t) :: event
        real(real64) :: start_time, end_time, elapsed_time
        logical :: continue_loop
        integer :: timeout_ms = 50
        
        print *, "Test: TUI input handling timeout..."
        
        ! Configure TUI for testing
        call config%init()
        config%input_timeout_ms = timeout_ms
        config%debug_mode = .false.
        
        call tui%init(config)
        
        call cpu_time(start_time)
        
        ! Test input handling with timeout
        call tui%handle_input(event, continue_loop)
        
        call cpu_time(end_time)
        elapsed_time = (end_time - start_time) * 1000.0_real64
        
        ! Should have timed out around the configured timeout
        if (elapsed_time < real(timeout_ms, real64) * 0.5_real64 .or. &
            elapsed_time > real(timeout_ms, real64) * 3.0_real64) then
            print *, "FAIL: Input timeout not working properly"
            print *, "Expected ~", timeout_ms, "ms, got:", elapsed_time, "ms"
            test_passed = .false.
        else
            print *, "PASS: Input handling timeout working"
            print *, "Timeout:", elapsed_time, "ms (expected ~", timeout_ms, "ms)"
        end if
        
        call tui%cleanup()
    end subroutine test_tui_input_handling_timeout
    
    ! Test that ANSI escape sequences don't flood terminal
    subroutine test_tui_escape_sequence_flooding()
        type(tui_engine_t) :: tui
        type(tui_config_t) :: config
        integer :: i
        
        print *, "Test: ANSI escape sequence flooding prevention..."
        
        ! Configure TUI for testing
        call config%init()
        call tui%init(config)
        
        ! Reset ANSI counter
        call tui%reset_ansi_counter()
        
        ! Try to exceed ANSI sequence limit
        do i = 1, 20  ! Try to exceed the limit (10)
            call tui%update_display()
            
            ! Should stop updating when limit is reached
            if (tui%check_ansi_limit()) then
                exit
            end if
        end do
        
        ! Should have stopped at the limit
        if (tui%ansi_sequence_count > 10) then
            print *, "FAIL: ANSI escape sequence flooding not prevented"
            print *, "Sequences:", tui%ansi_sequence_count
            test_passed = .false.
        else
            print *, "PASS: ANSI escape sequence flooding prevented"
            print *, "Max sequences per frame:", tui%ansi_sequence_count
        end if
        
        call tui%cleanup()
    end subroutine test_tui_escape_sequence_flooding
    
    ! Test that main TUI loop can be interrupted and doesn't run infinitely
    subroutine test_tui_infinite_loop_prevention()
        type(tui_engine_t) :: tui
        type(tui_config_t) :: config
        real(real64) :: start_time, end_time, elapsed_time
        
        print *, "Test: TUI infinite loop prevention..."
        
        ! Configure TUI for testing with limits
        call config%init()
        config%max_iterations = 50  ! Small limit for testing
        config%debug_mode = .false.
        
        call tui%init(config)
        
        call cpu_time(start_time)
        
        ! Run TUI main loop - should terminate properly
        call tui%run_main_loop(.true., 0.5_real64)  ! Interactive, 500ms max
        
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        ! Should have terminated within reasonable bounds
        if (tui%frame_count >= config%max_iterations .or. elapsed_time >= 0.5_real64) then
            print *, "PASS: TUI loop termination working"
            print *, "Frames:", tui%frame_count, "Time:", elapsed_time, "s"
        else
            print *, "FAIL: TUI loop termination not working properly"
            print *, "Frames:", tui%frame_count, "Time:", elapsed_time, "s"
            test_passed = .false.
        end if
        
        call tui%cleanup()
    end subroutine test_tui_infinite_loop_prevention

end program test_tui_main_loop