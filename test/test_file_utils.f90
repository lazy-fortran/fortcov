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
        character(len=*), parameter :: test_dir = "temp_test_dir"
        integer :: unit, stat
        
        print *, "  Test 1: Find files with extension pattern"
        
        ! Given: A directory with files: test.f90, test.gcda, test.gcno, other.txt
        call create_test_files(test_dir)
        
        ! When: Calling find_files("*.gc*")
        files = find_files(test_dir // "/*.gc*")
        
        ! Then: Should return ["test.gcda", "test.gcno"]
        passed = (size(files) == 2) .and. &
                 ((trim(files(1)) == "test.gcda" .and. &
                   trim(files(2)) == "test.gcno") .or. &
                  (trim(files(1)) == "test.gcno" .and. &
                   trim(files(2)) == "test.gcda"))
        
        call cleanup_test_files(test_dir)
        
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
        character(len=*), parameter :: base_dir = "temp_recursive_test"
        
        print *, "  Test 6: Find files recursively"
        
        ! Given: Nested directories with .f90 files
        call create_recursive_test_structure(base_dir)
        
        ! When: Calling find_files("**/*.f90")
        files = find_files(base_dir // "/**/*.f90")
        
        ! Then: Should return all .f90 files in tree
        passed = (size(files) >= 2)  ! Should find at least 2 .f90 files
        
        call cleanup_recursive_test_structure(base_dir)
        
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
        character(len=*), parameter :: test_dir = "output/reports"
        logical :: error_flag
        logical :: exists
        
        print *, "  Test 7: Create directory if not exists"
        
        ! Given: A path to non-existent directory
        ! When: Calling ensure_directory("output/reports")
        call ensure_directory(test_dir, error_flag)
        
        ! Then: Directory should be created
        inquire(file=test_dir, exist=exists)
        passed = (.not. error_flag) .and. exists
        
        ! Clean up
        if (exists) call cleanup_directory_tree("output")
        
        if (.not. passed) then
            print *, "    FAILED: Directory creation failed"
            print *, "    Error flag:", error_flag
            print *, "    Directory exists:", exists
        else
            print *, "    PASSED"
        end if
    end function test_ensure_directory

    ! Helper subroutines for test setup and cleanup
    subroutine create_test_files(dir)
        character(len=*), intent(in) :: dir
        integer :: unit, stat
        
        call execute_command_line("mkdir -p " // dir, exitstat=stat)
        
        ! Create test.f90
        open(newunit=unit, file=dir // "/test.f90", status='replace')
        write(unit, '(A)') "program test"
        close(unit)
        
        ! Create test.gcda
        open(newunit=unit, file=dir // "/test.gcda", status='replace')
        write(unit, '(A)') "gcda content"
        close(unit)
        
        ! Create test.gcno
        open(newunit=unit, file=dir // "/test.gcno", status='replace')
        write(unit, '(A)') "gcno content"
        close(unit)
        
        ! Create other.txt
        open(newunit=unit, file=dir // "/other.txt", status='replace')
        write(unit, '(A)') "other content"
        close(unit)
    end subroutine create_test_files

    subroutine cleanup_test_files(dir)
        character(len=*), intent(in) :: dir
        call execute_command_line("rm -rf " // dir)
    end subroutine cleanup_test_files

    subroutine create_recursive_test_structure(base_dir)
        character(len=*), intent(in) :: base_dir
        integer :: unit, stat
        
        call execute_command_line("mkdir -p " // base_dir // "/src", exitstat=stat)
        call execute_command_line("mkdir -p " // base_dir // "/test", exitstat=stat)
        
        ! Create base_dir/src/main.f90
        open(newunit=unit, file=base_dir // "/src/main.f90", status='replace')
        write(unit, '(A)') "program main"
        close(unit)
        
        ! Create base_dir/test/test_main.f90
        open(newunit=unit, file=base_dir // "/test/test_main.f90", status='replace')
        write(unit, '(A)') "program test_main"
        close(unit)
    end subroutine create_recursive_test_structure

    subroutine cleanup_recursive_test_structure(base_dir)
        character(len=*), intent(in) :: base_dir
        call execute_command_line("rm -rf " // base_dir)
    end subroutine cleanup_recursive_test_structure

    subroutine cleanup_directory_tree(base_dir)
        character(len=*), intent(in) :: base_dir
        call execute_command_line("rm -rf " // base_dir)
    end subroutine cleanup_directory_tree

end program test_file_utils