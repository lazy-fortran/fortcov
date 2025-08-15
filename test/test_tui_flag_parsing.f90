program test_tui_flag_parsing
    use fortcov_config
    implicit none
    
    type(config_t) :: config
    character(len=:), allocatable :: args(:)
    character(len=256) :: error_message
    logical :: success
    
    print *, "Testing explicit TUI flag parsing..."
    
    ! Test 1: --tui flag
    allocate(character(len=10) :: args(1))
    args(1) = "--tui"
    
    call parse_config(args, config, success, error_message)
    
    if (success .and. config%tui_mode) then
        print *, "PASSED: --tui flag parsed correctly"
        print *, "  success:", success
        print *, "  tui_mode:", config%tui_mode
    else
        print *, "FAILED: --tui flag parsing"
        print *, "  success:", success
        print *, "  tui_mode:", config%tui_mode
        if (len_trim(error_message) > 0) then
            print *, "  error:", trim(error_message)
        end if
        call exit(1)
    end if
    
    ! Test 2: Multiple flags including --tui
    deallocate(args)
    allocate(character(len=15) :: args(3))
    args(1) = "--tui"
    args(2) = "--verbose"
    args(3) = "--quiet"
    
    call parse_config(args, config, success, error_message)
    
    if (success .and. config%tui_mode .and. config%verbose .and. config%quiet) then
        print *, "PASSED: Multiple flags including --tui parsed correctly"
    else
        print *, "FAILED: Multiple flags with --tui"
        print *, "  success:", success
        print *, "  tui_mode:", config%tui_mode
        print *, "  verbose:", config%verbose
        print *, "  quiet:", config%quiet
        call exit(1)
    end if
    
    print *, "All explicit TUI flag parsing tests PASSED"

end program test_tui_flag_parsing