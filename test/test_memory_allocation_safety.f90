! Test for critical memory allocation safety in configuration parsing
! 
! This test exposes the memory allocation bugs identified by Patrick
! in PR #119 review - specifically around split() function usage 
! and variable reuse issues

program test_memory_allocation_safety
    use fortcov_config
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing memory allocation safety (PR #119 bugs)..."
    
    all_tests_passed = .true.
    
    ! Test 1: Multiple config file loads (exposes memory leaks)
    all_tests_passed = all_tests_passed .and. test_multiple_config_loads()
    
    ! Test 2: Split function memory handling
    all_tests_passed = all_tests_passed .and. test_split_memory_handling()
    
    ! Test 3: Variable reuse safety
    all_tests_passed = all_tests_passed .and. test_variable_reuse_safety()
    
    if (all_tests_passed) then
        print *, "All memory allocation safety tests PASSED"
        call exit(0)
    else
        print *, "Memory allocation safety tests FAILED"
        call exit(1)
    end if

contains

    ! Test multiple config file loads to expose memory leaks
    function test_multiple_config_loads() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        integer :: i, unit, iostat
        
        print *, "  Test 1: Multiple config file loads (memory leak detection)"
        
        ! Create a test config file with both formats
        open(newunit=unit, file="test_memory.nml", status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "  input_format = 'gcov'"
        write(unit, '(A)') "  source_paths = 'src,lib,app'"  ! Comma-separated (legacy)
        write(unit, '(A)') "  exclude_patterns = '*.mod,test/*'"  ! Comma-separated
        write(unit, '(A)') "/"
        close(unit)
        
        ! Load the same config multiple times - should not leak memory
        passed = .true.
        do i = 1, 5
            call initialize_config(config)
            config%config_file = "test_memory.nml"
            
            call load_config_file(config, success, error_message)
            
            if (.not. success) then
                print *, "    FAILED on iteration", i, ":", trim(error_message)
                passed = .false.
                exit
            end if
            
            ! Verify allocations are correct each time
            if (.not. allocated(config%source_paths) .or. &
                .not. allocated(config%exclude_patterns)) then
                print *, "    FAILED: Arrays not allocated on iteration", i
                passed = .false.
                exit
            end if
        end do
        
        ! Clean up
        open(newunit=unit, file="test_memory.nml", status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
        if (passed) then
            print *, "    PASSED: Multiple loads handled safely"
        end if
    end function test_multiple_config_loads

    ! Test split function memory handling
    function test_split_memory_handling() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        integer :: unit, iostat
        
        print *, "  Test 2: Split function memory handling"
        
        ! Create config file with many comma-separated values
        open(newunit=unit, file="test_split.nml", status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "  input_format = 'gcov'"
        ! Many values to stress memory allocation
        write(unit, '(A)') "  source_paths = 'a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p'"
        write(unit, '(A)') "  exclude_patterns = '1,2,3,4,5,6,7,8,9,10,11,12,13,14,15'"
        write(unit, '(A)') "/"
        close(unit)
        
        call initialize_config(config)
        config%config_file = "test_split.nml"
        
        ! This should expose the split() memory issues
        call load_config_file(config, success, error_message)
        
        passed = success
        
        if (success) then
            ! Verify correct number of elements were parsed
            if (allocated(config%source_paths)) then
                if (size(config%source_paths) /= 16) then
                    print *, "    FAILED: Expected 16 source paths, got", &
                            size(config%source_paths)
                    passed = .false.
                end if
            else
                print *, "    FAILED: source_paths not allocated"
                passed = .false.
            end if
            
            if (allocated(config%exclude_patterns)) then
                if (size(config%exclude_patterns) /= 15) then
                    print *, "    FAILED: Expected 15 exclude patterns, got", &
                            size(config%exclude_patterns)
                    passed = .false.
                end if
            else
                print *, "    FAILED: exclude_patterns not allocated"
                passed = .false.
            end if
        else
            print *, "    FAILED: Config loading failed:", trim(error_message)
        end if
        
        ! Clean up
        open(newunit=unit, file="test_split.nml", status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
        if (passed) then
            print *, "    PASSED: Split function memory handled correctly"
        end if
    end function test_split_memory_handling

    ! Test variable reuse safety (count variable reuse bug)
    function test_variable_reuse_safety() result(passed)
        logical :: passed
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message
        integer :: unit, iostat
        
        print *, "  Test 3: Variable reuse safety (count variable bug)"
        
        ! Create config with different numbers of source_paths vs exclude_patterns
        ! This exposes the count variable reuse bug
        open(newunit=unit, file="test_reuse.nml", status='replace')
        write(unit, '(A)') "&fortcov_config"
        write(unit, '(A)') "  input_format = 'gcov'"
        write(unit, '(A)') "  source_paths = 'one,two,three'"     ! 3 items
        write(unit, '(A)') "  exclude_patterns = 'a,b,c,d,e'"    ! 5 items  
        write(unit, '(A)') "/"
        close(unit)
        
        call initialize_config(config)
        config%config_file = "test_reuse.nml"
        
        call load_config_file(config, success, error_message)
        
        passed = success
        
        if (success) then
            ! Verify counts are independent (not affected by variable reuse)
            if (allocated(config%source_paths)) then
                if (size(config%source_paths) /= 3) then
                    print *, "    FAILED: source_paths count wrong, expected 3, got", &
                            size(config%source_paths)
                    passed = .false.
                end if
            end if
            
            if (allocated(config%exclude_patterns)) then
                if (size(config%exclude_patterns) /= 5) then
                    print *, "    FAILED: exclude_patterns count wrong, expected 5, got", &
                            size(config%exclude_patterns)
                    passed = .false.
                end if
            end if
        else
            print *, "    FAILED: Config loading failed:", trim(error_message)
        end if
        
        ! Clean up
        open(newunit=unit, file="test_reuse.nml", status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
        if (passed) then
            print *, "    PASSED: Variable reuse handled safely"
        end if
    end function test_variable_reuse_safety

end program test_memory_allocation_safety