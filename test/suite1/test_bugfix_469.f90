program test_bugfix_469
    !! Test for Issue #469: Auto-discovery fails to find gcov files in build/gcov/ directory
    !! 
    !! This test reproduces the zero-configuration bug where fortcov cannot find
    !! .gcov files in the build/gcov/ directory even when they exist.
    !! The root cause is that the discovery mechanism uses hardcoded filenames
    !! instead of proper file globbing to discover ALL .gcov files in the directory.
    use iso_fortran_env, only: error_unit
    use zero_config_manager, only: auto_discover_coverage_files_priority
    implicit none
    
    character(len=256), parameter :: test_dir = "build/gcov"
    character(len=256), parameter :: test_gcov_files(3) = [ &
        "build/gcov/actual_file.gcov   ", &
        "build/gcov/my_module.gcov     ", &
        "build/gcov/custom_name.gcov   "]
    character(len=:), allocatable :: discovered_files(:)
    logical :: all_tests_passed
    integer :: i, num_found
    
    all_tests_passed = .true.
    
    print *, "=== Test Issue #469: Auto-discovery bug in build/gcov/ ==="
    print *, ""
    
    ! Create test directory structure
    call create_test_environment()
    
    ! Test 1: Reproduce the bug - auto_discover should find actual files
    print *, "Test 1: Check if auto-discovery finds real .gcov files"
    discovered_files = auto_discover_coverage_files_priority()
    num_found = 0
    if (allocated(discovered_files)) num_found = size(discovered_files)
    
    print '(A,I0,A)', "  Found ", num_found, " files"
    
    ! Fixed: The auto-discovery should find all 3 test .gcov files using globbing
    if (num_found /= 3) then
        print *, "  ❌ FAIL: Auto-discovery did not find all 3 gcov files"
        print *, "     Expected 3 files but found", num_found
        all_tests_passed = .false.
    else
        print *, "  ✅ PASS: Found expected number of files - globbing works correctly"
    end if
    
    ! Clean up first set of files
    call cleanup_test_environment()
    
    ! Test 2: Verify it finds hardcoded names (current broken behavior)
    print *, ""
    print *, "Test 2: Check if it finds hardcoded filenames (broken behavior)"
    call create_hardcoded_test_files()
    discovered_files = auto_discover_coverage_files_priority()
    num_found = 0
    if (allocated(discovered_files)) num_found = size(discovered_files)
    
    print '(A,I0,A)', "  Found ", num_found, " hardcoded files"
    if (num_found > 0) then
        print *, "  ⚠️  Found hardcoded files - this shows the current broken implementation"
        print *, "     works only for specific filenames"
        do i = 1, min(num_found, 5)
            print '(A,A)', "     - ", trim(discovered_files(i))
        end do
    end if
    
    ! Clean up
    call cleanup_test_environment()
    
    print *, ""
    if (all_tests_passed) then
        print *, "=== Test completed - Bug #469 is FIXED ==="
        print *, "Auto-discovery properly uses .gcov file globbing"
        call exit(0)  ! Success - test passes
    else
        print *, "=== Test FAILED - Issue #469 regression detected ==="
        print *, "Auto-discovery is not finding all .gcov files correctly"
        call exit(1)  ! Failure - bug has reappeared
    end if
    
contains
    
    subroutine create_test_environment()
        !! Create test directory and realistic .gcov files
        character(len=256) :: cmd
        integer :: i, unit, iostat
        
        ! Create test directory
        cmd = "mkdir -p " // trim(test_dir)
        call execute_command_line(cmd)
        
        ! Create realistic .gcov files that should be found
        do i = 1, size(test_gcov_files)
            open(newunit=unit, file=trim(test_gcov_files(i)), status='replace', iostat=iostat)
            if (iostat == 0) then
                write(unit, '(A)') "        -:    0:Source:test.f90"
                write(unit, '(A)') "        -:    1:program test_program"
                write(unit, '(A)') "        1:    2:  print *, 'hello'"
                write(unit, '(A)') "        -:    3:end program"
                close(unit)
            end if
        end do
    end subroutine create_test_environment
    
    subroutine create_hardcoded_test_files()
        !! Create files with hardcoded names that the current implementation finds
        character(len=256) :: hardcoded_names(4)
        character(len=256) :: full_path, cmd
        integer :: i, unit, iostat
        
        ! Create test directory first
        cmd = "mkdir -p " // trim(test_dir)
        call execute_command_line(cmd)
        
        hardcoded_names = [ &
            "test.gcov     ", &
            "main.gcov     ", &
            "real_test.gcov", &
            "coverage.gcov "]
        
        do i = 1, size(hardcoded_names)
            full_path = trim(test_dir) // "/" // trim(hardcoded_names(i))
            open(newunit=unit, file=trim(full_path), status='replace', iostat=iostat)
            if (iostat == 0) then
                write(unit, '(A)') "        -:    0:Source:test.f90"
                write(unit, '(A)') "        1:    1:  print *, 'hardcoded'"
                close(unit)
            end if
        end do
    end subroutine create_hardcoded_test_files
    
    subroutine cleanup_test_environment()
        !! Remove test files and directory
        character(len=256) :: cmd
        cmd = "rm -rf build"
        call execute_command_line(cmd)
    end subroutine cleanup_test_environment
    
end program test_bugfix_469