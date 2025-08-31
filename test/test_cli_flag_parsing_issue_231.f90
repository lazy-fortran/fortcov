program test_cli_flag_parsing_issue_231
    !! 
    !! Given-When-Then Test Documentation:
    !! 
    !! GIVEN: A fortcov installation with working coverage files
    !! WHEN: CLI flags are specified for output control and format
    !! THEN: The flags should be applied and affect the actual behavior
    !! 
    !! This test suite demonstrates Issue #231: ALL CLI flags are silently ignored
    !! Status: RESOLVED - All CLI flags now work correctly
    !! All tests should PASS showing flags work correctly
    !!
    !! DECOMPOSED: CLI flag parsing logic extracted to focused modules for 
    !! architecture size compliance (<500 lines per file)
    !!
    use test_framework_utilities
    use test_environment_utilities
    use test_file_utilities
    use test_cli_flag_parsing_core
    use file_ops_secure, only: safe_remove_file
    use error_handling_core, only: error_context_t
    implicit none
    
    type(test_counter_t) :: test_counter
    
    print *, "=========================================="
    print *, "CLI Flag Parsing Issue #231 Test Suite"
    print *, "=========================================="
    print *, ""
    print *, "Testing CLI flag functionality - all flags now work correctly."
    print *, "Issue #231 has been resolved - CLI flags are properly handled"
    print *, ""
    
    ! Initialize test framework
    call init_test_counter(test_counter)
    
    ! Setup test environment
    call setup_test_environment()
    
    ! Test individual flag functionality
    call test_output_path_flag(test_counter)
    call test_output_format_flag(test_counter)
    call test_verbose_flag(test_counter)
    call test_quiet_flag(test_counter)
    call test_threshold_flag(test_counter)
    call test_exclude_flag(test_counter)
    call test_source_flag(test_counter)
    call test_invalid_flag_handling(test_counter)
    
    ! Cleanup test environment
    call cleanup_test_environment()
    
    ! Report results
    call print_test_summary(test_counter, "CLI Flag Parsing Issue #231")
    
contains

    subroutine setup_test_environment()
        !! Setup test environment for CLI flag tests
        call setup_basic_test_environment("CLI flag parsing")
        
        ! Create necessary test files
        call create_test_gcov_file("test_sample.f90.gcov")
        call create_test_source_file("test_sample.f90")
        call create_test_config_file("test_config.cfg")
    end subroutine setup_test_environment
    
    subroutine cleanup_test_environment()
        !! Cleanup test environment
        call cleanup_basic_test_environment("CLI flag parsing")
        
        ! Clean up specific test files using secure file operations
        call safe_cleanup_test_files()
    end subroutine cleanup_test_environment

    subroutine safe_cleanup_test_files()
        !! Safely remove test files using Fortran intrinsics instead of shell commands
        character(len=*), parameter :: test_files(8) = [ &
            'test_sample.f90.gcov', &
            'test_sample.f90     ', &
            'test_config.cfg     ', &
            'test_output.md      ', &
            'test_output.json    ', &
            'test_output.xml     ', &
            'test_output.html    ', &
            'test_output.txt     ' &
        ]
        type(error_context_t) :: error_ctx
        integer :: i
        
        do i = 1, size(test_files)
            call safe_remove_file(trim(test_files(i)), error_ctx)
            ! Ignore errors - files may not exist, which is fine
        end do
    end subroutine safe_cleanup_test_files

end program test_cli_flag_parsing_issue_231
