program test_issue_131_exclude_patterns
    use coverage_engine
    use fortcov_config, only: config_t, initialize_config
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing Issue #131: --exclude patterns don't filter coverage files"
    print *
    
    all_tests_passed = .true.
    
    ! Test 1: Exclude patterns functionality with allocated patterns
    all_tests_passed = all_tests_passed .and. test_exclude_with_patterns()
    
    ! Test 2: Exclude patterns with unallocated patterns (should not crash)
    all_tests_passed = all_tests_passed .and. test_exclude_unallocated_patterns()
    
    ! Test 3: Exclude patterns with empty patterns array
    all_tests_passed = all_tests_passed .and. test_exclude_empty_patterns()
    
    print *
    if (all_tests_passed) then
        print *, "All Issue #131 tests PASSED"
    else
        print *, "Some Issue #131 tests FAILED"
        call exit(1)
    end if
    
contains

    function test_exclude_with_patterns() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: should_exclude
        
        print *, "  Test 1: Exclude patterns with allocated patterns"
        
        ! Initialize config with default values
        call initialize_config(config)
        
        ! Set up exclude patterns
        allocate(character(len=256) :: config%exclude_patterns(2))
        config%exclude_patterns(1) = "test*"
        config%exclude_patterns(2) = "*.mod"
        
        ! Test that test files are excluded
        should_exclude = check_exclude_patterns("test_example.f90", config)
        passed = should_exclude
        
        if (.not. passed) then
            print *, "    FAILED: 'test_example.f90' should be excluded by 'test*'"
            return
        end if
        
        ! Test that .mod files are excluded  
        should_exclude = check_exclude_patterns("module.mod", config)
        passed = passed .and. should_exclude
        
        if (.not. should_exclude) then
            print *, "    FAILED: 'module.mod' should be excluded by '*.mod'"
            passed = .false.
            return
        end if
        
        ! Test that other files are not excluded
        should_exclude = check_exclude_patterns("source.f90", config)
        passed = passed .and. (.not. should_exclude)
        
        if (should_exclude) then
            print *, "    FAILED: 'source.f90' should NOT be excluded"
            passed = .false.
            return
        end if
        
        if (passed) then
            print *, "    PASSED"
        end if
        
    end function test_exclude_with_patterns
    
    function test_exclude_unallocated_patterns() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: should_exclude
        
        print *, "  Test 2: Exclude patterns with unallocated patterns"
        
        ! Initialize config with default values
        call initialize_config(config)
        
        ! Make sure exclude_patterns is not allocated
        if (allocated(config%exclude_patterns)) then
            deallocate(config%exclude_patterns)
        end if
        
        ! This should not crash and should return false
        should_exclude = check_exclude_patterns("test_example.f90", config)
        passed = .not. should_exclude  ! Should not exclude anything
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Should not exclude when patterns unallocated"
        end if
        
    end function test_exclude_unallocated_patterns
    
    function test_exclude_empty_patterns() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: should_exclude
        
        print *, "  Test 3: Exclude patterns with empty patterns array"
        
        ! Initialize config with default values
        call initialize_config(config)
        
        ! Set up empty exclude patterns array
        allocate(character(len=256) :: config%exclude_patterns(0))
        
        ! This should not exclude anything
        should_exclude = check_exclude_patterns("test_example.f90", config)
        passed = .not. should_exclude  ! Should not exclude anything
        
        if (passed) then
            print *, "    PASSED"
        else
            print *, "    FAILED: Should not exclude when patterns array is empty"
        end if
        
    end function test_exclude_empty_patterns

end program test_issue_131_exclude_patterns