program test_zero_configuration_issue_249
    !! Test that zero-configuration mode works when no arguments provided
    !! Issue #249: Running fortcov without arguments should auto-discover files
    
    use fortcov_config
    use zero_configuration_manager
    use error_handling
    use file_utils
    implicit none
    
    type(config_t) :: config
    character(len=:), allocatable :: args(:)
    character(len=256) :: error_message
    logical :: success
    integer :: exit_code
    character(len=256) :: test_dir
    character(len=256) :: gcov_file_path
    integer :: unit
    logical :: test_passed
    
    test_passed = .false.
    exit_code = 0
    
    ! Create test environment with sample .gcov files
    test_dir = "test_zero_config_249"
    call execute_command_line("rm -rf " // trim(test_dir), exitstat=exit_code)
    call execute_command_line("mkdir -p " // trim(test_dir), exitstat=exit_code)
    
    ! Create a sample .gcov file
    gcov_file_path = trim(test_dir) // "/sample.f90.gcov"
    open(newunit=unit, file=gcov_file_path, status='replace')
    write(unit, *) "        -:    0:Source:sample.f90"
    write(unit, *) "        -:    1:module sample"
    write(unit, *) "        5:    2:    implicit none"
    write(unit, *) "        -:    3:contains"
    write(unit, *) "        5:    4:    subroutine test()"
    write(unit, *) "        5:    5:        print *, 'test'"
    write(unit, *) "        5:    6:    end subroutine"
    write(unit, *) "        -:    7:end module"
    close(unit)
    
    ! Change to test directory
    call execute_command_line("cd " // trim(test_dir), exitstat=exit_code)
    
    ! Test 1: No arguments should trigger zero-configuration mode
    print *, "Test 1: Zero-configuration with no arguments"
    allocate(character(len=256) :: args(0))
    call parse_config(args, config, success, error_message)
    
    if (.not. success) then
        print *, "FAIL: parse_config returned failure"
        print *, "Error: ", trim(error_message)
        exit_code = 1
    else if (.not. config%zero_configuration_mode) then
        print *, "FAIL: Zero-configuration mode not activated"
        exit_code = 1
    else if (config%show_help) then
        print *, "FAIL: Help flag incorrectly set in zero-config mode"
        exit_code = 1
    else
        print *, "PASS: Zero-configuration mode activated correctly"
        test_passed = .true.
    end if
    
    ! Test 2: Verify default output path is set
    if (test_passed) then
        print *, "Test 2: Default output path in zero-config"
        if (.not. allocated(config%output_path)) then
            print *, "FAIL: Output path not set"
            exit_code = 1
        else if (trim(config%output_path) /= "build/coverage/coverage.md") then
            print *, "FAIL: Wrong output path: ", trim(config%output_path)
            exit_code = 1
        else
            print *, "PASS: Default output path correctly set"
        end if
    end if
    
    ! Test 3: Verify source paths are auto-discovered
    if (test_passed) then
        print *, "Test 3: Auto-discovery of source paths"
        if (.not. allocated(config%source_paths)) then
            print *, "FAIL: Source paths not set"
            exit_code = 1
        else if (size(config%source_paths) == 0) then
            print *, "FAIL: No source paths discovered"
            exit_code = 1
        else
            print *, "PASS: Source paths auto-discovered"
        end if
    end if
    
    ! Clean up
    call execute_command_line("cd .. && rm -rf " // trim(test_dir))
    
    if (exit_code == 0) then
        print *, "✓ All zero-configuration tests passed"
    else
        print *, "✗ Zero-configuration tests failed"
    end if
    
    call exit(exit_code)
    
end program test_zero_configuration_issue_249