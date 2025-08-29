program test_issue_436_example_config
    !! Test for issue #436: Example configuration file fails validation
    !! Bug: The official example config file fails with generic validation error
    
    use config_core, only: config_t, parse_config, validate_config_with_context
    use error_handling_core, only: error_context_t
    use iso_fortran_env, only: output_unit
    implicit none
    
    integer :: test_failures
    
    test_failures = 0
    
    print *, "========================================="
    print *, "Issue #436: Example Config Validation Test"
    print *, "========================================="
    print *, ""
    
    ! Test 1: Example config file can be loaded
    call test_example_config_loads(test_failures)
    
    ! Test 2: Example config validates successfully
    call test_example_config_validates(test_failures)
    
    ! Test 3: Example config has expected source paths
    call test_example_config_has_sources(test_failures)
    
    ! Report results
    print *, ""
    if (test_failures == 0) then
        print *, "All tests PASSED"
        stop 0
    else
        print '(A,I0)', "Tests FAILED: ", test_failures
        stop 1
    end if
    
contains

    !> Test that example config file can be loaded without errors
    subroutine test_example_config_loads(failures)
        integer, intent(inout) :: failures
        
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_msg
        character(len=256) :: args(1)
        
        print *, "Test 1: Example config file loading"
        
        ! Try to load the example config file
        args(1) = "--config=fortcov.nml.example"
        call parse_config(args, config, success, error_msg)
        
        if (.not. success) then
            print *, "  FAIL: Example config failed to load: ", trim(error_msg)
            failures = failures + 1
        else
            print *, "  PASS: Example config loaded successfully"
        end if
        print *, ""
        
    end subroutine test_example_config_loads
    
    !> Test that example config validates successfully
    subroutine test_example_config_validates(failures)
        integer, intent(inout) :: failures
        
        type(config_t) :: config
        type(error_context_t) :: error_ctx
        logical :: success
        character(len=512) :: load_error
        character(len=256) :: args(1)
        
        print *, "Test 2: Example config validation"
        
        ! Load the example config
        args(1) = "--config=fortcov.nml.example"
        call parse_config(args, config, success, load_error)
        
        if (.not. success) then
            print *, "  FAIL: Setup failed - could not load example config: ", trim(load_error)
            failures = failures + 1
            return
        end if
        
        ! Validate the loaded config
        call validate_config_with_context(config, error_ctx)
        
        ! Should validate successfully
        if (error_ctx%error_code /= 0) then
            print *, "  FAIL: Example config validation failed: ", trim(error_ctx%message)
            failures = failures + 1
        else
            print *, "  PASS: Example config validates successfully"
        end if
        print *, ""
        
    end subroutine test_example_config_validates
    
    !> Test that example config has expected source paths
    subroutine test_example_config_has_sources(failures)
        integer, intent(inout) :: failures
        
        type(config_t) :: config
        logical :: success
        character(len=512) :: error_msg
        character(len=256) :: args(1)
        
        print *, "Test 3: Example config source paths"
        
        ! Load the example config
        args(1) = "--config=fortcov.nml.example"
        call parse_config(args, config, success, error_msg)
        
        if (.not. success) then
            print *, "  FAIL: Setup failed - could not load example config: ", trim(error_msg)
            failures = failures + 1
            return
        end if
        
        ! Should have source_paths allocated and populated
        if (.not. allocated(config%source_paths)) then
            print *, "  FAIL: Expected source_paths to be allocated"
            failures = failures + 1
            return
        end if
        
        if (size(config%source_paths) == 0) then
            print *, "  FAIL: Expected source_paths to have content"
            failures = failures + 1
            return
        end if
        
        if (len_trim(config%source_paths(1)) == 0) then
            print *, "  FAIL: Expected first source path to be non-empty"
            failures = failures + 1
            return
        end if
        
        print *, "  PASS: Source paths configured correctly"
        print *, ""
        
    end subroutine test_example_config_has_sources

end program test_issue_436_example_config