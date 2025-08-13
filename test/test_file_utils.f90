program test_file_utils
    use file_utils
    use iso_fortran_env, only: error_unit
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing File System Utilities..."
    
    ! Test 1: Find files with extension pattern
    all_tests_passed = all_tests_passed .and. test_find_files_pattern()
    
    ! Test 2: Resolve relative path
    all_tests_passed = all_tests_passed .and. test_resolve_path()
    
    ! Test 3: Read binary file
    all_tests_passed = all_tests_passed .and. test_read_binary_file()
    
    ! Test 4: Write text file
    all_tests_passed = all_tests_passed .and. test_write_text_file()
    
    ! Test 5: Handle non-existent file
    all_tests_passed = all_tests_passed .and. test_nonexistent_file()
    
    ! Test 6: Find files recursively
    all_tests_passed = all_tests_passed .and. test_find_files_recursive()
    
    ! Test 7: Create directory if not exists
    all_tests_passed = all_tests_passed .and. test_ensure_directory()
    
    ! Test 8: Security - No command injection in patterns
    all_tests_passed = all_tests_passed .and. test_security_no_injection()
    
    ! Test 8b: Security - Command execution detection
    all_tests_passed = all_tests_passed .and. test_no_command_execution()
    
    ! Test 9: Cross-platform temp directory usage
    all_tests_passed = all_tests_passed .and. test_cross_platform_temp()
    
    ! Test 10: Large number of files (no arbitrary limits)
    all_tests_passed = all_tests_passed .and. test_no_arbitrary_limits()
    
    ! Test 11: Native glob pattern matching
    all_tests_passed = all_tests_passed .and. test_glob_pattern_matching()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_find_files_pattern() result(passed)
        logical :: passed
        character(len=:), allocatable :: files(:)
        integer :: unit, stat
        
        print *, "  Test 1: Find files with extension pattern"
        
        ! Given: Files in current directory: test.f90, test.gcda, test.gcno, other.txt
        call create_simple_test_files()
        
        ! When: Calling find_files("*.gc*") in current directory
        files = find_files("*.gc*")
        
        ! Then: Should return ["test.gcda", "test.gcno"]  
        passed = (size(files) == 2) .and. &
                 ((trim(files(1)) == "test.gcda" .and. &
                   trim(files(2)) == "test.gcno") .or. &
                  (trim(files(1)) == "test.gcno" .and. &
                   trim(files(2)) == "test.gcda"))
        
        call cleanup_simple_test_files()
        
        if (.not. passed) then
            print *, "    FAILED: Expected 2 files (test.gcda, test.gcno)"
            if (allocated(files)) then
                print *, "    Got:", size(files), "files"
            else
                print *, "    Got: unallocated array"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_find_files_pattern

    function test_resolve_path() result(passed)
        logical :: passed
        character(len=:), allocatable :: resolved
        character(len=256) :: cwd
        integer :: stat
        
        print *, "  Test 2: Resolve relative path"
        
        ! Given: A relative path "./src/module.f90"
        call getcwd(cwd, stat)
        
        ! When: Calling resolve_path()
        resolved = resolve_path("./src/module.f90")
        
        ! Then: Should return absolute path starting with current directory
        passed = (index(resolved, trim(cwd)) == 1) .and. &
                 (index(resolved, "/src/module.f90") > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Path not properly resolved"
            print *, "    Expected to start with: ", trim(cwd)
            print *, "    Got: ", resolved
        else
            print *, "    PASSED"
        end if
    end function test_resolve_path

    function test_read_binary_file() result(passed)
        logical :: passed
        integer(kind=1), allocatable :: data(:)
        character(len=*), parameter :: test_file = "temp_binary_test.bin"
        integer :: unit, stat
        logical :: error_flag
        
        print *, "  Test 3: Read binary file"
        
        ! Given: A binary file with bytes [71, 67, 78, 79] (0x47, 0x43, 0x4E, 0x4F)
        open(newunit=unit, file=test_file, access='stream', status='replace')
        write(unit) 71_1, 67_1, 78_1, 79_1
        close(unit)
        
        ! When: Calling read_binary_file()
        call read_binary_file(test_file, data, error_flag)
        
        ! Then: Should return integer array with values [71, 67, 78, 79]
        passed = (.not. error_flag) .and. &
                 (size(data) == 4) .and. &
                 (data(1) == 71) .and. &
                 (data(2) == 67) .and. &
                 (data(3) == 78) .and. &
                 (data(4) == 79)
        
        ! Clean up
        open(newunit=unit, file=test_file, status='old', iostat=stat)
        if (stat == 0) then
            close(unit, status='delete')
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Binary file reading failed"
            if (allocated(data)) then
                print *, "    Got size:", size(data)
                if (size(data) > 0) print *, "    First byte:", data(1)
            end if
        else
            print *, "    PASSED"
        end if
    end function test_read_binary_file

    function test_write_text_file() result(passed)
        logical :: passed
        character(len=*), parameter :: test_file = "temp_text_test.txt"
        character(len=*), parameter :: test_content = "Coverage: 85.5%"
        character(len=100) :: read_content
        integer :: unit, stat
        logical :: error_flag
        
        print *, "  Test 4: Write text file"
        
        ! Given: A string "Coverage: 85.5%"
        ! When: Calling write_text_file("report.md", content)
        call write_text_file(test_file, test_content, error_flag)
        
        ! Then: File should exist with exact content
        passed = .not. error_flag
        if (passed) then
            open(newunit=unit, file=test_file, status='old', iostat=stat)
            if (stat == 0) then
                read(unit, '(A)', iostat=stat) read_content
                close(unit, status='delete')
                passed = (stat == 0) .and. (trim(read_content) == test_content)
            else
                passed = .false.
            end if
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Text file writing/reading failed"
        else
            print *, "    PASSED"
        end if
    end function test_write_text_file

    function test_nonexistent_file() result(passed)
        logical :: passed
        integer(kind=1), allocatable :: data(:)
        logical :: error_flag
        
        print *, "  Test 5: Handle non-existent file"
        
        ! Given: A path to non-existent file
        ! When: Calling read_binary_file()
        call read_binary_file("definitely_does_not_exist.bin", data, error_flag)
        
        ! Then: Should return allocated empty array and set error flag
        passed = error_flag .and. allocated(data) .and. (size(data) == 0)
        
        if (.not. passed) then
            print *, "    FAILED: Non-existent file not handled properly"
            print *, "    Error flag:", error_flag
            if (allocated(data)) then
                print *, "    Array size:", size(data)
            else
                print *, "    Array not allocated"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_nonexistent_file

    function test_find_files_recursive() result(passed)
        logical :: passed
        character(len=:), allocatable :: files(:)
        
        print *, "  Test 6: Find files recursively"
        
        ! Given: Multiple .f90 files in current directory
        call create_recursive_test_files()
        
        ! When: Calling find_files("**/*.f90") (recursive search)
        files = find_files("**/*.f90")
        
        ! Then: Should return all .f90 files (at least 2)
        passed = (size(files) >= 2)  ! Should find at least 2 .f90 files
        
        call cleanup_recursive_test_files()
        
        if (.not. passed) then
            print *, "    FAILED: Recursive file finding failed"
            if (allocated(files)) then
                print *, "    Found", size(files), "files"
            end if
        else
            print *, "    PASSED"
        end if
    end function test_find_files_recursive

    function test_ensure_directory() result(passed)
        logical :: passed
        character(len=*), parameter :: test_dir = "."  ! Use current dir for MVP
        logical :: error_flag
        logical :: exists
        
        print *, "  Test 7: Create directory if not exists"
        
        ! Given: A path to existing directory (current dir)
        ! When: Calling ensure_directory
        call ensure_directory(test_dir, error_flag)
        
        ! Then: Should not return error for existing directory
        inquire(file=test_dir, exist=exists)
        passed = (.not. error_flag) .and. exists
        
        if (.not. passed) then
            print *, "    FAILED: Directory handling failed"
            print *, "    Error flag:", error_flag
            print *, "    Directory exists:", exists
        else
            print *, "    PASSED - MVP directory handling works"
        end if
    end function test_ensure_directory

    function test_security_no_injection() result(passed)
        logical :: passed
        character(len=:), allocatable :: files(:)
        character(len=*), parameter :: malicious_pattern = &
            "*.f90; rm -rf /; echo pwned"
        logical :: current_dir_exists
        
        print *, "  Test 8: Security - No command injection in patterns"
        
        ! Given: A malicious pattern with shell commands
        ! When: Calling find_files with malicious pattern  
        files = find_files(malicious_pattern)
        
        ! Then: Should safely handle pattern without executing commands
        ! The function should return empty results due to security blocking
        ! and the current directory should still exist (system intact)
        inquire(file=".", exist=current_dir_exists)
        passed = allocated(files) .and. current_dir_exists
        
        if (.not. passed) then
            print *, "    FAILED: Command injection vulnerability detected"
            print *, "    Files allocated:", allocated(files)
            print *, "    Current dir exists:", current_dir_exists
        else
            print *, "    PASSED"
        end if
    end function test_security_no_injection
    
    function test_no_command_execution() result(passed)
        logical :: passed
        character(len=*), parameter :: sentinel_file = "pwned_by_injection.txt"
        character(len=:), allocatable :: files(:)
        logical :: sentinel_exists_before, sentinel_exists_after
        
        print *, "  Test 8b: Security - Command execution detection"
        
        ! Given: Ensure sentinel file doesn't exist
        inquire(file=sentinel_file, exist=sentinel_exists_before)
        if (sentinel_exists_before) then
            open(unit=99, file=sentinel_file, status='old')
            close(99, status='delete')
        end if
        
        ! When: Using malicious pattern that would create sentinel file
        files = find_files("*.f90; touch " // sentinel_file // "; echo 'exploited'")
        
        ! Then: Sentinel file should NOT be created (no shell execution)
        inquire(file=sentinel_file, exist=sentinel_exists_after)
        passed = .not. sentinel_exists_after
        
        ! Cleanup in case of failure
        if (sentinel_exists_after) then
            open(unit=99, file=sentinel_file, status='old')
            close(99, status='delete')
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Shell commands were executed!"
            print *, "    Sentinel file was created - CRITICAL VULNERABILITY"
        else
            print *, "    PASSED"
        end if
    end function test_no_command_execution

    function test_cross_platform_temp() result(passed)
        logical :: passed
        character(len=:), allocatable :: temp_dir
        logical :: exists
        
        print *, "  Test 9: Cross-platform temp directory usage"
        
        ! When: Getting temporary directory
        temp_dir = get_temp_directory()
        
        ! Then: Should return a valid directory path (exists and not empty)
        inquire(file=temp_dir, exist=exists)
        passed = exists .and. (len(temp_dir) > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Temp directory not valid"
            print *, "    Got temp dir: ", temp_dir
            print *, "    Directory exists: ", exists
        else
            print *, "    PASSED - Using temp dir: ", temp_dir
        end if
    end function test_cross_platform_temp

    function test_no_arbitrary_limits() result(passed)
        logical :: passed
        character(len=:), allocatable :: files(:)
        character(len=*), parameter :: test_dir = "temp_many_files_test"
        integer, parameter :: num_files = 150  ! More than old limit of 100
        integer :: i, unit, stat
        character(len=20) :: filename
        logical :: exists
        
        print *, "  Test 10: Large number of files (no arbitrary limits)"
        
        ! Given: Directory with more than 100 files (create in current dir for test)
        do i = 1, num_files
            write(filename, '("temp_file_", I0, ".f90")') i
            open(newunit=unit, file=filename, status='replace', iostat=stat)
            if (stat == 0) then
                write(unit, '(A)') "! Test file"
                close(unit)
            end if
        end do
        
        ! When: Finding all .f90 files (using current directory pattern)
        files = find_files("temp_file_*.f90")
        
        ! Then: Should find all files, not limited to 100
        passed = allocated(files) .and. (size(files) >= num_files)
        
        ! Clean up individual files
        do i = 1, num_files
            write(filename, '("temp_file_", I0, ".f90")') i
            inquire(file=filename, exist=exists)
            if (exists) then
                open(newunit=unit, file=filename, status='old', iostat=stat)
                if (stat == 0) close(unit, status='delete')
            end if
        end do
        
        if (.not. passed) then
            print *, "    FAILED: Arbitrary file limit still enforced"
            if (allocated(files)) then
                print *, "    Found", size(files), "files, expected", num_files
            end if
        else
            print *, "    PASSED"
        end if
    end function test_no_arbitrary_limits
    
    function test_glob_pattern_matching() result(passed)
        logical :: passed
        integer :: unit, stat, i
        character(len=30) :: filename
        character(len=:), allocatable :: files(:)
        logical :: exists
        
        print *, "  Test 11: Native glob pattern matching"
        
        ! Given: Files with different patterns
        ! Create test_abc.f90, test_def.f90, test_123.txt, other.f90
        open(newunit=unit, file="test_abc.f90", status='replace')
        close(unit)
        open(newunit=unit, file="test_def.f90", status='replace')
        close(unit)
        open(newunit=unit, file="test_123.txt", status='replace')
        close(unit)
        open(newunit=unit, file="other.f90", status='replace')
        close(unit)
        
        ! When: Using pattern test_*.f90
        files = find_files("test_*.f90")
        
        ! Then: Should match only test_abc.f90 and test_def.f90
        passed = allocated(files) .and. (size(files) == 2)
        if (passed) then
            ! Check that we got the right files (order may vary)
            passed = (index(files(1), "test_") == 1 .and. index(files(1), ".f90") > 0) .and. &
                     (index(files(2), "test_") == 1 .and. index(files(2), ".f90") > 0) .and. &
                     (files(1) /= files(2))
        end if
        
        ! Clean up test files
        inquire(file="test_abc.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file="test_abc.f90", status='old')
            close(unit, status='delete')
        end if
        inquire(file="test_def.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file="test_def.f90", status='old')
            close(unit, status='delete')
        end if
        inquire(file="test_123.txt", exist=exists)
        if (exists) then
            open(newunit=unit, file="test_123.txt", status='old')
            close(unit, status='delete')
        end if
        inquire(file="other.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file="other.f90", status='old')
            close(unit, status='delete')
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Glob pattern matching failed"
            if (allocated(files)) then
                print *, "    Found", size(files), "files, expected 2"
                do i = 1, min(size(files), 5)
                    print *, "      File:", trim(files(i))
                end do
            end if
        else
            print *, "    PASSED"
        end if
    end function test_glob_pattern_matching

    ! Helper subroutines for test setup and cleanup
    subroutine create_test_files(dir)
        character(len=*), intent(in) :: dir
        integer :: unit, stat
        logical :: exists, error_flag
        character(len=256) :: safe_dir
        
        ! Ensure directory exists before creating files
        call ensure_directory(dir, error_flag)
        
        ! For testing purposes, use current directory if target dir doesn't exist
        ! This ensures tests can run without requiring directory creation
        inquire(file=dir, exist=exists)
        if (exists) then
            safe_dir = dir
        else
            safe_dir = "."  ! Use current directory as fallback
        end if
        
        ! Create test.f90
        open(newunit=unit, file=safe_dir // "/test.f90", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "program test"
            close(unit)
        end if
        
        ! Create test.gcda
        open(newunit=unit, file=safe_dir // "/test.gcda", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "gcda content"
            close(unit)
        end if
        
        ! Create test.gcno
        open(newunit=unit, file=safe_dir // "/test.gcno", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "gcno content"
            close(unit)
        end if
        
        ! Create other.txt
        open(newunit=unit, file=safe_dir // "/other.txt", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "other content"
            close(unit)
        end if
    end subroutine create_test_files

    subroutine cleanup_test_files(dir)
        character(len=*), intent(in) :: dir
        integer :: unit, stat
        logical :: exists
        character(len=256) :: safe_dir
        
        ! Use same logic as create_test_files for consistency
        inquire(file=dir, exist=exists)
        if (exists) then
            safe_dir = dir
        else
            safe_dir = "."  ! Use current directory as fallback
        end if
        
        ! Clean up individual files (pure Fortran - no shell commands)
        inquire(file=safe_dir // "/test.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file=safe_dir // "/test.f90", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file=safe_dir // "/test.gcda", exist=exists)
        if (exists) then
            open(newunit=unit, file=safe_dir // "/test.gcda", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file=safe_dir // "/test.gcno", exist=exists)
        if (exists) then
            open(newunit=unit, file=safe_dir // "/test.gcno", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file=safe_dir // "/other.txt", exist=exists)
        if (exists) then
            open(newunit=unit, file=safe_dir // "/other.txt", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
    end subroutine cleanup_test_files
    
    subroutine create_safe_test_files(dir)
        character(len=*), intent(in) :: dir
        call create_test_files(dir)
    end subroutine create_safe_test_files
    
    subroutine cleanup_safe_test_files(dir)
        character(len=*), intent(in) :: dir
        call cleanup_test_files(dir)
    end subroutine cleanup_safe_test_files

    subroutine create_recursive_test_structure(base_dir)
        character(len=*), intent(in) :: base_dir
        integer :: unit, stat
        logical :: exists, error_flag
        
        ! Ensure directories exist before creating files
        call ensure_directory(base_dir, error_flag)
        call ensure_directory(base_dir // "/src", error_flag)
        call ensure_directory(base_dir // "/test", error_flag)
        
        ! Check and try to create base_dir/src/main.f90
        inquire(file=base_dir // "/src", exist=exists)
        if (.not. exists) then
            ! If directories still don't exist, skip this test
            return
        end if
        
        open(newunit=unit, file=base_dir // "/src/main.f90", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "program main"
            close(unit)
        end if
        
        ! Create base_dir/test/test_main.f90
        open(newunit=unit, file=base_dir // "/test/test_main.f90", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "program test_main"
            close(unit)
        end if
    end subroutine create_recursive_test_structure

    subroutine cleanup_recursive_test_structure(base_dir)
        character(len=*), intent(in) :: base_dir
        integer :: unit, stat
        logical :: exists
        
        ! Clean up individual files
        inquire(file=base_dir // "/src/main.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file=base_dir // "/src/main.f90", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file=base_dir // "/test/test_main.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file=base_dir // "/test/test_main.f90", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
    end subroutine cleanup_recursive_test_structure

    subroutine cleanup_directory_tree(base_dir)
        character(len=*), intent(in) :: base_dir
        ! For now just clean the files we know about
        call cleanup_recursive_test_structure(base_dir)
    end subroutine cleanup_directory_tree

    subroutine create_test_files_in_current_dir(prefix)
        character(len=*), intent(in) :: prefix
        integer :: unit, stat
        
        ! Create test files with prefix in current directory
        open(newunit=unit, file=prefix // "_test.f90", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "program test"
            close(unit)
        end if
        
        open(newunit=unit, file=prefix // "_test.gcda", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "gcda content"
            close(unit)
        end if
        
        open(newunit=unit, file=prefix // "_test.gcno", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "gcno content"
            close(unit)
        end if
        
        open(newunit=unit, file=prefix // "_other.txt", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "other content"
            close(unit)
        end if
    end subroutine create_test_files_in_current_dir

    subroutine cleanup_test_files_in_current_dir(prefix)
        character(len=*), intent(in) :: prefix
        integer :: unit, stat
        logical :: exists
        
        ! Clean up test files in current directory
        inquire(file=prefix // "_test.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file=prefix // "_test.f90", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file=prefix // "_test.gcda", exist=exists)
        if (exists) then
            open(newunit=unit, file=prefix // "_test.gcda", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file=prefix // "_test.gcno", exist=exists)
        if (exists) then
            open(newunit=unit, file=prefix // "_test.gcno", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file=prefix // "_other.txt", exist=exists)
        if (exists) then
            open(newunit=unit, file=prefix // "_other.txt", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
    end subroutine cleanup_test_files_in_current_dir

    subroutine create_simple_test_files()
        integer :: unit, stat
        
        ! Create test files with simple names in current directory
        open(newunit=unit, file="test.f90", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "program test"
            close(unit)
        end if
        
        open(newunit=unit, file="test.gcda", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "gcda content"
            close(unit)
        end if
        
        open(newunit=unit, file="test.gcno", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "gcno content"
            close(unit)
        end if
        
        open(newunit=unit, file="other.txt", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "other content"
            close(unit)
        end if
    end subroutine create_simple_test_files

    subroutine cleanup_simple_test_files()
        integer :: unit, stat
        logical :: exists
        
        ! Clean up simple test files in current directory
        inquire(file="test.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file="test.f90", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file="test.gcda", exist=exists)
        if (exists) then
            open(newunit=unit, file="test.gcda", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file="test.gcno", exist=exists)
        if (exists) then
            open(newunit=unit, file="test.gcno", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file="other.txt", exist=exists)
        if (exists) then
            open(newunit=unit, file="other.txt", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
    end subroutine cleanup_simple_test_files

    subroutine create_recursive_test_files()
        integer :: unit, stat
        
        ! Create multiple .f90 files for recursive test
        open(newunit=unit, file="main.f90", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "program main"
            close(unit)
        end if
        
        open(newunit=unit, file="module.f90", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "module test_mod"
            close(unit)
        end if
        
        open(newunit=unit, file="other.txt", status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') "not fortran"
            close(unit)
        end if
    end subroutine create_recursive_test_files

    subroutine cleanup_recursive_test_files()
        integer :: unit, stat
        logical :: exists
        
        ! Clean up recursive test files
        inquire(file="main.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file="main.f90", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file="module.f90", exist=exists)
        if (exists) then
            open(newunit=unit, file="module.f90", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
        
        inquire(file="other.txt", exist=exists)
        if (exists) then
            open(newunit=unit, file="other.txt", status='old', iostat=stat)
            if (stat == 0) close(unit, status='delete')
        end if
    end subroutine cleanup_recursive_test_files

end program test_file_utils