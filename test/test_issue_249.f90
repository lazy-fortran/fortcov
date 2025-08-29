program test_issue_249
    !! Consolidated test suite for Issue #249: Zero-configuration mode
    !! Verifies that running fortcov without arguments:
    !! - Activates zero-configuration mode
    !! - Does NOT show help message
    !! - Sets correct default paths
    !! - Attempts coverage analysis with auto-discovery
    
    use config_core
    use zero_config_manager
    use error_handling_core
    use file_utils_core
    use file_ops_secure, only: safe_find_files, safe_mkdir
    use path_validation_core, only: validate_path_security, validate_executable_path
    use shell_utils_core, only: escape_shell_argument
    implicit none
    
    type(config_t) :: config
    character(len=:), allocatable :: args(:)
    character(len=256) :: error_message
    logical :: success
    integer :: exit_code, test_num
    logical :: all_pass
    type(error_context_t) :: error_ctx
    
    exit_code = 0
    test_num = 0
    all_pass = .true.
    
    print *, "==============================================================="
    print *, "Issue #249: Zero-configuration mode comprehensive test suite"
    print *, "==============================================================="
    print *, ""
    print *, "This test suite verifies that zero-configuration mode:"
    print *, "1. Activates when no arguments are provided"
    print *, "2. Does NOT show help message"
    print *, "3. Sets correct default paths"
    print *, "4. Handles edge cases correctly"
    print *, "5. Auto-discovers source files when appropriate"
    print *, ""
    
    ! Test 1: No arguments activates zero-config
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": No arguments -> zero-configuration mode"
    allocate(character(len=256) :: args(0))
    call parse_config(args, config, success, error_message)
    
    if (.not. success) then
        print *, "  ✗ FAIL: parse_config failed"
        print *, "    Error: ", trim(error_message)
        all_pass = .false.
    else if (config%show_help) then
        print *, "  ✗ FAIL: show_help is TRUE (should be FALSE)"
        all_pass = .false.
    else if (.not. config%zero_configuration_mode) then
        print *, "  ✗ FAIL: zero_configuration_mode is FALSE (should be TRUE)"
        all_pass = .false.
    else
        print *, "  ✓ PASS: Zero-config activated, help NOT shown"
    end if
    deallocate(args)
    
    ! Test 2: Default output path is set correctly
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Default output path in zero-config"
    allocate(character(len=256) :: args(0))
    call parse_config(args, config, success, error_message)
    
    if (.not. allocated(config%output_path)) then
        print *, "  ✗ FAIL: output_path not allocated"
        all_pass = .false.
    else if (trim(config%output_path) /= "build/coverage/coverage.md") then
        print *, "  ✗ FAIL: Wrong output path: ", trim(config%output_path)
        all_pass = .false.
    else
        print *, "  ✓ PASS: Default output path correct"
    end if
    deallocate(args)
    
    ! Test 3: Empty string arguments (edge case)
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Empty string arguments"
    allocate(character(len=256) :: args(2))
    args(1) = ""
    args(2) = "  "
    call parse_config(args, config, success, error_message)
    
    if (config%show_help) then
        print *, "  ✗ FAIL: Empty strings triggered help"
        all_pass = .false.
    else if (.not. config%zero_configuration_mode) then
        print *, "  ✗ FAIL: Empty strings disabled zero-config"
        all_pass = .false.
    else
        print *, "  ✓ PASS: Empty strings correctly ignored"
    end if
    deallocate(args)
    
    ! Test 4: Help flag explicitly requested
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Explicit --help flag"
    allocate(character(len=256) :: args(1))
    args(1) = "--help"
    call parse_config(args, config, success, error_message)
    
    if (.not. config%show_help) then
        print *, "  ✗ FAIL: --help flag didn't set show_help"
        all_pass = .false.
    else if (config%zero_configuration_mode) then
        print *, "  ✗ FAIL: --help shouldn't trigger zero-config"
        all_pass = .false.
    else
        print *, "  ✓ PASS: --help works correctly"
    end if
    deallocate(args)
    
    ! Test 5: Output flag with no source (should trigger zero-config)
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Output flag only (zero-config with override)"
    allocate(character(len=256) :: args(2))
    args(1) = "--output"
    args(2) = "custom.md"
    call parse_config(args, config, success, error_message)
    
    if (config%show_help) then
        print *, "  ✗ FAIL: Output flag triggered help"
        all_pass = .false.
    else if (.not. config%zero_configuration_mode) then
        print *, "  ✗ FAIL: Should use zero-config with output override"
        all_pass = .false.
    else if (trim(config%output_path) /= "custom.md") then
        print *, "  ✗ FAIL: Output override not applied"
        all_pass = .false.
    else
        print *, "  ✓ PASS: Zero-config with output override works"
    end if
    deallocate(args)
    
    ! Test 6: Coverage file argument (not zero-config)
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Coverage file argument"
    allocate(character(len=256) :: args(1))
    args(1) = "test.gcov"
    call parse_config(args, config, success, error_message)
    
    if (config%show_help) then
        print *, "  ✗ FAIL: Coverage file triggered help"
        all_pass = .false.
    else if (config%zero_configuration_mode) then
        print *, "  ✗ FAIL: Coverage file shouldn't trigger zero-config"
        all_pass = .false.
    else
        print *, "  ✓ PASS: Coverage file handled correctly"
    end if
    deallocate(args)
    
    ! Test 7: Auto-discovery with test environment
    test_num = test_num + 1
    print '(A,I2,A)', "Test ", test_num, ": Auto-discovery of source paths"
    call test_auto_discovery_with_environment(all_pass)
    
    ! Summary
    print *, ""
    print *, "==============================================================="
    if (all_pass) then
        print *, "✓ ALL TESTS PASSED"
        print *, ""
        print *, "Issue #249 Resolution Status: WORKING CORRECTLY"
        print *, ""
        print *, "Zero-configuration mode is functioning as documented:"
        print *, "- Running 'fortcov' without arguments activates zero-config"
        print *, "- It does NOT show the help message"
        print *, "- It sets correct default paths"
        print *, "- It attempts to analyze coverage with auto-discovery"
        print *, "- Default output: build/coverage/coverage.md"
        exit_code = 0
    else
        print *, "✗ SOME TESTS FAILED"
        print *, ""
        print *, "Issue #249 may need investigation"
        exit_code = 1
    end if
    print *, "==============================================================="
    
    call exit(exit_code)
    
contains

    subroutine test_auto_discovery_with_environment(test_passed)
        !! Test auto-discovery with a sample .gcov file environment
        logical, intent(inout) :: test_passed
        character(len=256) :: test_dir
        character(len=256) :: gcov_file_path
        integer :: unit
        type(config_t) :: local_config
        character(len=:), allocatable :: local_args(:)
        logical :: local_success
        character(len=256) :: local_error_message
        
        test_dir = "test_zero_config_249"
        
        ! Safe directory cleanup
        call safe_cleanup_test_directory(test_dir)
        
        ! Safe directory creation
        call safe_mkdir(test_dir, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            print *, "  ✗ FAIL: Could not create test directory"
            test_passed = .false.
            return
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
        
        ! Test with no arguments for auto-discovery
        allocate(character(len=256) :: local_args(0))
        call parse_config(local_args, local_config, local_success, local_error_message)
        
        if (.not. local_success) then
            print *, "  ✗ FAIL: parse_config failed in test environment"
            test_passed = .false.
        else if (.not. allocated(local_config%source_paths)) then
            print *, "  ✗ FAIL: Source paths not allocated"
            test_passed = .false.
        else if (size(local_config%source_paths) == 0) then
            print *, "  ✗ FAIL: No source paths discovered"
            test_passed = .false.
        else
            print *, "  ✓ PASS: Source paths auto-discovered"
        end if
        
        deallocate(local_args)
        
        ! Clean up test directory
        call safe_cleanup_test_directory(test_dir)
        
    end subroutine test_auto_discovery_with_environment

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
        
        ! Remove the directory
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
        command = "rmdir " // escape_shell_argument(trim(dir_name)) // " 2>/dev/null"
        call execute_command_line(command, exitstat=stat)
    end subroutine rmdir

end program test_issue_249