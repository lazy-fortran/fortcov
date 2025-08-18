program test_memory_safety_use_after_free
    !! Test for use-after-free vulnerability in coverage_engine.f90:255-280
    !! 
    !! Tests array extension patterns that can lead to use-after-free
    !! when array reallocations invalidate previous references
    use coverage_engine
    use fortcov_config
    use string_utils
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing Use-After-Free vulnerabilities in coverage_engine..."
    
    all_tests_passed = .true.
    
    ! Test 1: Find coverage files with many files (stress array allocation)
    all_tests_passed = all_tests_passed .and. test_find_coverage_files_stress()
    
    ! Test 2: Multiple check_exclude_patterns calls with long patterns
    all_tests_passed = all_tests_passed .and. test_exclude_patterns_stress()
    
    ! Test 3: String operations with very long filenames (buffer issues)
    all_tests_passed = all_tests_passed .and. test_long_filename_handling()
    
    ! Test 4: Repeated operations to stress memory allocation/deallocation
    all_tests_passed = all_tests_passed .and. test_repeated_operations()
    
    if (all_tests_passed) then
        print *, "All use-after-free tests PASSED"
        call exit(0)
    else
        print *, "Use-after-free vulnerabilities DETECTED"
        call exit(1)
    end if

contains

    function test_find_coverage_files_stress() result(passed)
        logical :: passed
        character(len=:), allocatable :: found_files(:)
        type(config_t) :: config
        integer :: i
        
        print *, "  Test 1: Find coverage files stress test"
        
        ! Setup config for file finding
        call initialize_config(config)
        
        ! Reallocate source_paths with proper size
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        passed = .true.
        
        ! Multiple calls to find_coverage_files to stress allocation/deallocation
        do i = 1, 10
            found_files = find_coverage_files(config)
            
            if (.not. allocated(found_files)) then
                print *, "    FAILED: find_coverage_files failed to allocate on iteration", i
                passed = .false.
                exit
            end if
            
            ! The function should always return an allocated array, even if empty
            if (size(found_files) < 0) then
                print *, "    FAILED: Invalid array size on iteration", i
                passed = .false.
                exit
            end if
        end do
        
        if (passed) then
            print *, "    PASSED: Find coverage files stress test handled safely"
        end if
    end function test_find_coverage_files_stress

    function test_exclude_patterns_stress() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: should_exclude
        integer :: i
        character(len=:), allocatable :: test_filename
        
        print *, "  Test 2: Exclude patterns stress test"
        
        ! Setup config with long exclude patterns
        call initialize_config(config)
        
        ! Reallocate exclude_patterns with proper size
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(3))
        config%exclude_patterns(1) = repeat("a", 200) // "*.f90"
        config%exclude_patterns(2) = repeat("b", 200) // "*.mod"
        config%exclude_patterns(3) = repeat("c", 200) // "*.o"
        
        passed = .true.
        
        ! Test with increasingly long filenames
        do i = 1, 10
            test_filename = repeat("x", i * 50) // "_test.f90"
            
            should_exclude = check_exclude_patterns(test_filename, config)
            
            ! The function should handle long filenames without crashing
            ! Result doesn't matter as much as not crashing
            if (i > 5 .and. len(test_filename) > 200) then
                ! For very long filenames, ensure function completes
                print *, "    INFO: Tested filename length:", len(test_filename)
            end if
        end do
        
        if (passed) then
            print *, "    PASSED: Exclude patterns stress test handled safely"
        end if
    end function test_exclude_patterns_stress

    function test_long_filename_handling() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: should_exclude
        character(len=:), allocatable :: extreme_filename
        
        print *, "  Test 3: Long filename handling"
        
        ! Setup basic config
        call initialize_config(config)
        
        ! Reallocate exclude_patterns with proper size before accessing
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(1))
        config%exclude_patterns(1) = "test_*.f90"
        
        passed = .true.
        
        ! Test with extremely long filename (>256 chars)
        extreme_filename = repeat("verylongfilename", 20) // "_test.f90"
        
        should_exclude = check_exclude_patterns(extreme_filename, config)
        
        ! If we get here without crashing, that's good
        if (len(extreme_filename) > 256) then
            print *, "    INFO: Tested extreme filename length:", len(extreme_filename)
            print *, "    INFO: Exclude result:", should_exclude
        end if
        
        ! Test multiple very long filenames
        extreme_filename = repeat("z", 1000) // ".f90"
        should_exclude = check_exclude_patterns(extreme_filename, config)
        
        ! Again, completing without crash is the main goal
        print *, "    INFO: Second extreme test completed"
        
        if (passed) then
            print *, "    PASSED: Long filename handling completed safely"
        end if
    end function test_long_filename_handling

    function test_repeated_operations() result(passed)
        logical :: passed
        character(len=:), allocatable :: files(:)
        type(config_t) :: config
        logical :: exclude_result
        integer :: i, j
        
        print *, "  Test 4: Repeated operations memory safety"
        
        call initialize_config(config)
        
        ! Reallocate source_paths with proper size before accessing
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        ! Reallocate exclude_patterns with proper size before accessing
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(2))
        config%exclude_patterns(1) = "*.mod"
        config%exclude_patterns(2) = "build/*"
        
        passed = .true.
        
        ! Repeatedly call both functions to stress memory management
        do i = 1, 5
            ! Find files
            files = find_coverage_files(config)
            
            if (.not. allocated(files)) then
                print *, "    FAILED: Files not allocated on iteration", i
                passed = .false.
                exit
            end if
            
            ! Test exclude patterns on each found file
            do j = 1, min(size(files), 10)  ! Limit to first 10 for efficiency
                exclude_result = check_exclude_patterns(files(j), config)
                ! Result doesn't matter, just that it doesn't crash
            end do
            
            ! Test with constructed filenames too
            exclude_result = check_exclude_patterns("test_file.f90", config)
            exclude_result = check_exclude_patterns("build/temp.o", config)
        end do
        
        if (passed) then
            print *, "    PASSED: Repeated operations handled safely"
        end if
    end function test_repeated_operations

end program test_memory_safety_use_after_free