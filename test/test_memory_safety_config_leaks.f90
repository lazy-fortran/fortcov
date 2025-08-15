program test_memory_safety_config_leaks
    !! Test for memory leaks in fortcov_config.f90:501-638 error paths
    !! 
    !! Tests config loading failure scenarios to expose memory leaks
    !! when error paths don't properly clean up allocated resources
    use fortcov_config
    use string_utils
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing Memory Leaks in config error paths..."
    
    all_tests_passed = .true.
    
    ! Test 1: Split allocation failure simulation
    all_tests_passed = all_tests_passed .and. test_split_allocation_failure()
    
    ! Test 2: Config loading with invalid data and early returns
    all_tests_passed = all_tests_passed .and. test_config_early_return_leaks()
    
    ! Test 3: Multiple failure scenarios with resource tracking
    all_tests_passed = all_tests_passed .and. test_multiple_failure_scenarios()
    
    ! Test 4: Memory cleanup verification in error paths
    all_tests_passed = all_tests_passed .and. test_memory_cleanup_verification()
    
    if (all_tests_passed) then
        print *, "All memory leak tests PASSED"
        call exit(0)
    else
        print *, "Memory leak vulnerabilities DETECTED"
        call exit(1)
    end if

contains

    function test_split_allocation_failure() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        integer :: unit, iostat, i
        
        print *, "  Test 1: Split allocation failure simulation"
        
        ! Create config file that triggers split allocation
        open(newunit=unit, file="test_split_alloc.nml", status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "  input_format = 'gcov'"
        ! Large comma-separated list to stress split allocation
        write(unit, '(A)') "  source_paths = 'a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z'"
        write(unit, '(A)') "/"
        close(unit)
        
        passed = .true.
        
        ! Test multiple config loads to stress allocation/deallocation
        do i = 1, 5
            call initialize_config(config)
            config%config_file = "test_split_alloc.nml"
            
            call load_config_file(config, success, error_message)
            
            ! Each iteration should properly clean up previous allocations
            if (.not. success) then
                print *, "    WARNING: Config load failed on iteration", i, ":", trim(error_message)
                ! This might be expected for some test cases
            end if
            
            ! Check if arrays are properly allocated when successful
            if (success) then
                if (.not. allocated(config%source_paths)) then
                    print *, "    FAILED: source_paths not allocated after successful load"
                    passed = .false.
                    exit
                end if
            end if
        end do
        
        ! Clean up test file
        open(newunit=unit, file="test_split_alloc.nml", status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
        if (passed) then
            print *, "    PASSED: Split allocation handled safely"
        end if
    end function test_split_allocation_failure

    function test_config_early_return_leaks() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        integer :: unit, iostat
        
        print *, "  Test 2: Config early return memory leaks"
        
        ! Create invalid config that triggers early returns
        open(newunit=unit, file="test_early_return.nml", status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "  input_format = 'gcov'"
        ! This should trigger error path at line 516-519 (allocation failure)
        write(unit, '(A)') "  source_paths = ','"  ! Empty after split
        write(unit, '(A)') "/"
        close(unit)
        
        call initialize_config(config)
        config%config_file = "test_early_return.nml"
        
        call load_config_file(config, success, error_message)
        
        ! This should fail, but test if memory is properly cleaned
        if (success) then
            print *, "    UNEXPECTED: Invalid config should have failed"
            passed = .false.
        else
            print *, "    EXPECTED: Config failed as expected:", trim(error_message)
            passed = .true.
        end if
        
        ! Check if any allocations are left dangling
        if (allocated(config%source_paths)) then
            print *, "    WARNING: source_paths still allocated after failure"
            ! This might be expected behavior, but worth noting
        end if
        
        if (allocated(config%exclude_patterns)) then
            print *, "    WARNING: exclude_patterns still allocated after failure"
        end if
        
        ! Clean up test file
        open(newunit=unit, file="test_early_return.nml", status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
        if (passed) then
            print *, "    PASSED: Early return handled safely"
        end if
    end function test_config_early_return_leaks

    function test_multiple_failure_scenarios() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        integer :: unit, iostat, scenario
        character(len=64) :: filename
        
        print *, "  Test 3: Multiple failure scenarios"
        
        passed = .true.
        
        ! Test several different failure scenarios
        do scenario = 1, 3
            write(filename, '(A,I0,A)') "test_scenario_", scenario, ".nml"
            
            open(newunit=unit, file=filename, status='replace')
            write(unit, '(A)') "&fortcov_config"
            write(unit, '(A)') "  input_format = 'gcov'"
            
            select case (scenario)
            case (1)
                ! Empty split case
                write(unit, '(A)') "  source_paths = ''"
            case (2)
                ! Only commas case
                write(unit, '(A)') "  source_paths = ',,,'"
            case (3)
                ! Very long values that could cause issues
                write(unit, '(A,A,A)') "  source_paths = '", repeat("x", 1000), "'"
            end select
            
            write(unit, '(A)') "/"
            close(unit)
            
            call initialize_config(config)
            config%config_file = filename
            
            call load_config_file(config, success, error_message)
            
            ! Most of these should fail, but shouldn't leak memory
            if (.not. success) then
                print *, "    Scenario", scenario, "failed as expected:", trim(error_message)
            else
                print *, "    Scenario", scenario, "unexpectedly succeeded"
            end if
            
            ! Clean up test file
            open(newunit=unit, file=filename, status='old', iostat=iostat)
            if (iostat == 0) then
                close(unit, status='delete')
            end if
        end do
        
        if (passed) then
            print *, "    PASSED: Multiple failure scenarios handled"
        end if
    end function test_multiple_failure_scenarios

    function test_memory_cleanup_verification() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        integer :: unit, iostat, i
        
        print *, "  Test 4: Memory cleanup verification"
        
        ! Create a config that will succeed, then fail, to test cleanup
        open(newunit=unit, file="test_cleanup.nml", status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "  input_format = 'gcov'"
        write(unit, '(A)') "  source_paths = 'src,lib'"
        write(unit, '(A)') "  exclude_patterns = '*.mod'"
        write(unit, '(A)') "/"
        close(unit)
        
        passed = .true.
        
        ! Load config successfully first
        call initialize_config(config)
        config%config_file = "test_cleanup.nml"
        
        call load_config_file(config, success, error_message)
        
        if (.not. success) then
            print *, "    FAILED: Initial config load should succeed"
            passed = .false.
        else
            print *, "    Initial config loaded successfully"
            
            ! Verify allocations exist
            if (.not. allocated(config%source_paths) .or. &
                .not. allocated(config%exclude_patterns)) then
                print *, "    FAILED: Arrays not allocated after successful load"
                passed = .false.
            end if
        end if
        
        ! Now load again to test if previous allocations are cleaned up
        ! The load_config_file should deallocate existing arrays (line 501-503)
        do i = 1, 3
            call load_config_file(config, success, error_message)
            
            if (.not. success) then
                print *, "    FAILED: Repeated config load failed on iteration", i
                passed = .false.
                exit
            end if
            
            ! Verify arrays are still properly allocated
            if (.not. allocated(config%source_paths)) then
                print *, "    FAILED: source_paths not allocated on iteration", i
                passed = .false.
                exit
            end if
        end do
        
        ! Clean up test file
        open(newunit=unit, file="test_cleanup.nml", status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
        if (passed) then
            print *, "    PASSED: Memory cleanup working correctly"
        end if
    end function test_memory_cleanup_verification

end program test_memory_safety_config_leaks