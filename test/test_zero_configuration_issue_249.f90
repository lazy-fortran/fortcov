program test_zero_configuration_issue_249
    !! Test that zero-configuration mode works when no arguments provided
    !! Issue #249: Running fortcov without arguments should auto-discover files
    
    use fortcov_config
    use zero_configuration_manager
    use error_handling
    use file_utils
    use secure_command_executor, only: safe_mkdir
    implicit none
    
    type(config_t) :: config
    character(len=:), allocatable :: args(:)
    character(len=256) :: error_message
    logical :: success
    integer :: exit_code
    character(len=256) :: test_dir
    character(len=256) :: gcov_file_path
    character(len=256) :: current_dir
    integer :: unit
    logical :: test_passed
    type(error_context_t) :: error_ctx
    integer :: stat
    
    test_passed = .false.
    exit_code = 0
    
    ! Create test environment with sample .gcov files
    test_dir = "test_zero_config_249"
    
    ! Safe directory cleanup using Fortran intrinsics
    call safe_cleanup_test_directory(test_dir)
    
    ! Safe directory creation using secure module
    call safe_mkdir(test_dir, error_ctx)
    if (error_ctx%error_code /= ERROR_SUCCESS) then
        print *, "FAIL: Could not create test directory"
        call exit(1)
    end if
    
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
    
    ! Store current directory and change to test directory safely
    call getcwd(current_dir)
    call chdir(trim(test_dir), stat)
    if (stat /= 0) then
        print *, "FAIL: Could not change to test directory"
        call safe_cleanup_test_directory(test_dir)
        call exit(1)
    end if
    
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
    
    ! Return to original directory and clean up safely
    call chdir(trim(current_dir), stat)
    call safe_cleanup_test_directory(test_dir)
    
    if (exit_code == 0) then
        print *, "✓ All zero-configuration tests passed"
    else
        print *, "✗ Zero-configuration tests failed"
    end if
    
    call exit(exit_code)
    
contains

    subroutine safe_cleanup_test_directory(dir_name)
        !! Safely removes a test directory and its contents
        !! Uses Fortran intrinsics to avoid shell injection
        character(len=*), intent(in) :: dir_name
        logical :: dir_exists
        integer :: unit, iostat
        character(len=256) :: file_path
        
        ! Check if directory exists
        inquire(file=trim(dir_name), exist=dir_exists)
        if (.not. dir_exists) return
        
        ! Remove files in directory
        file_path = trim(dir_name) // "/sample.f90.gcov"
        inquire(file=file_path, exist=dir_exists)
        if (dir_exists) then
            open(newunit=unit, file=file_path, status='old', iostat=iostat)
            if (iostat == 0) then
                close(unit, status='delete')
            end if
        end if
        
        ! Remove the directory (only works if empty)
        ! For test purposes, we use a restricted approach
        ! In production, would use platform-specific safe API
        call rmdir(trim(dir_name))
    end subroutine safe_cleanup_test_directory
    
    subroutine rmdir(dir_name)
        !! Platform-safe directory removal for empty directories
        character(len=*), intent(in) :: dir_name
        character(len=512) :: command
        integer :: stat
        
        ! Use safe command construction with validation
        if (len_trim(dir_name) == 0) return
        if (index(dir_name, ";") > 0) return  ! Reject semicolons
        if (index(dir_name, "&") > 0) return  ! Reject ampersands
        if (index(dir_name, "|") > 0) return  ! Reject pipes
        if (index(dir_name, "`") > 0) return  ! Reject backticks
        if (index(dir_name, "$") > 0) return  ! Reject variables
        if (index(dir_name, ">") > 0) return  ! Reject redirects
        if (index(dir_name, "<") > 0) return  ! Reject redirects
        if (index(dir_name, "*") > 0) return  ! Reject wildcards
        if (index(dir_name, "?") > 0) return  ! Reject wildcards
        if (index(dir_name, "..") > 0) return ! Reject parent paths
        
        ! Only allow simple directory names
        command = "rmdir " // trim(dir_name) // " 2>/dev/null"
        call execute_command_line(command, exitstat=stat)
    end subroutine rmdir

end program test_zero_configuration_issue_249