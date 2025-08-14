program test_gcov_command_executor
    use gcov_command_executor
    use error_handling
    use file_utils
    use iso_fortran_env, only: error_unit
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing GCov Command Executor..."
    
    ! RED Phase - All tests should initially fail
    
    ! Test 1: Basic gcov command execution
    all_tests_passed = all_tests_passed .and. test_execute_gcov_basic()
    
    ! Test 2: Missing GCDA file handling
    all_tests_passed = all_tests_passed .and. test_missing_gcda_file()
    
    ! Test 3: Command failure scenarios
    all_tests_passed = all_tests_passed .and. test_command_failure()
    
    ! Test 4: Branch coverage support (-b flag)
    all_tests_passed = all_tests_passed .and. test_branch_coverage_flag()
    
    ! Test 5: Working directory changes
    all_tests_passed = all_tests_passed .and. test_working_directory()
    
    ! Test 6: File cleanup
    all_tests_passed = all_tests_passed .and. test_file_cleanup()
    
    ! Test 7: Multiple source file processing
    all_tests_passed = all_tests_passed .and. test_multiple_source_files()
    
    ! Test 8: .gcov file generation verification
    all_tests_passed = all_tests_passed .and. test_gcov_file_generation()
    
    ! Test 9: Error context handling
    all_tests_passed = all_tests_passed .and. test_error_context_handling()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_execute_gcov_basic() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=*), parameter :: test_source = "test_basic.f90"
        
        print *, "  Test 1: Basic gcov command execution"
        
        call setup_test_gcov_environment("test_basic")
        
        ! Given: A source file with corresponding .gcda and .gcno files
        ! When: Executing gcov command on source file
        call executor%execute_gcov(test_source, gcov_files, error_ctx)
        
        ! Then: Should succeed and generate .gcov file
        passed = (error_ctx%error_code == ERROR_SUCCESS) .and. &
                 (size(gcov_files) == 1)
        
        if (passed .and. size(gcov_files) > 0) then
            passed = passed .and. (index(gcov_files(1), ".gcov") > 0)
        end if
        
        call cleanup_test_environment("test_basic")
        
        if (.not. passed) then
            print *, "    FAILED: Basic gcov execution failed"
            print *, "    Error code:", error_ctx%error_code
            print *, "    Message:", trim(error_ctx%message)
            print *, "    Array size:", size(gcov_files)
        else
            print *, "    PASSED"
        end if
    end function test_execute_gcov_basic

    function test_missing_gcda_file() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=*), parameter :: test_source = "test_missing.f90"
        integer :: unit
        
        print *, "  Test 2: Missing GCDA file handling"
        
        ! Given: A source file without corresponding .gcda file
        open(newunit=unit, file=test_source, status='replace')
        write(unit, '(A)') "program test_missing"
        write(unit, '(A)') "  print *, 'Hello'"
        write(unit, '(A)') "end program"
        close(unit)
        
        ! When: Executing gcov command
        call executor%execute_gcov(test_source, gcov_files, error_ctx)
        
        ! Then: Should handle error gracefully
        passed = (error_ctx%error_code == ERROR_MISSING_SOURCE_FILE) .and. &
                 (size(gcov_files) == 0) .and. &
                 (error_ctx%recoverable .eqv. .true.)
        
        call cleanup_test_file(test_source)
        
        if (.not. passed) then
            print *, "    FAILED: Missing GCDA file not handled properly"
            print *, "    Error code:", error_ctx%error_code
            print *, "    Expected:", ERROR_MISSING_SOURCE_FILE
        else
            print *, "    PASSED"
        end if
    end function test_missing_gcda_file

    function test_command_failure() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=*), parameter :: test_source = "nonexistent.f90"
        
        print *, "  Test 3: Command failure scenarios"
        
        ! Given: A non-existent source file
        ! When: Executing gcov command
        call executor%execute_gcov(test_source, gcov_files, error_ctx)
        
        ! Then: Should report command failure with proper error context
        passed = (error_ctx%error_code /= ERROR_SUCCESS) .and. &
                 (len_trim(error_ctx%message) > 0) .and. &
                 (len_trim(error_ctx%suggestion) > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Command failure not handled properly"
            print *, "    Error code:", error_ctx%error_code
        else
            print *, "    PASSED"
        end if
    end function test_command_failure

    function test_branch_coverage_flag() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=*), parameter :: test_source = "test_branch.f90"
        
        print *, "  Test 4: Branch coverage support (-b flag)"
        
        call setup_test_gcov_environment("test_branch")
        
        ! Given: A source file with branch coverage data
        ! When: Executing gcov with branch coverage flag
        call executor%set_branch_coverage(.true.)
        call executor%execute_gcov(test_source, gcov_files, error_ctx)
        
        ! Then: Should execute gcov with -b flag and succeed
        passed = (error_ctx%error_code == ERROR_SUCCESS) .and. &
                 (size(gcov_files) >= 1)
        
        call cleanup_test_environment("test_branch")
        
        if (.not. passed) then
            print *, "    FAILED: Branch coverage flag not supported"
            print *, "    Error code:", error_ctx%error_code
        else
            print *, "    PASSED"
        end if
    end function test_branch_coverage_flag

    function test_working_directory() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=*), parameter :: test_dir = "test_workdir"
        character(len=*), parameter :: test_source = "test_wd.f90"
        character(len=256) :: original_cwd, current_cwd
        integer :: stat
        
        print *, "  Test 5: Working directory changes"
        
        ! Given: A test directory with coverage files
        call getcwd(original_cwd, stat)
        call setup_test_gcov_environment_in_dir(test_dir, "test_wd")
        
        ! When: Executing gcov with different working directory
        call executor%set_working_directory(test_dir)
        call executor%execute_gcov(test_source, gcov_files, error_ctx)
        
        ! Then: Should execute in correct directory and return to original
        call getcwd(current_cwd, stat)
        passed = (error_ctx%error_code == ERROR_SUCCESS) .and. &
                 (trim(current_cwd) == trim(original_cwd))
        
        call cleanup_test_environment(test_dir)
        
        if (.not. passed) then
            print *, "    FAILED: Working directory handling failed"
            print *, "    Original dir:", trim(original_cwd)
            print *, "    Current dir:", trim(current_cwd)
        else
            print *, "    PASSED"
        end if
    end function test_working_directory

    function test_file_cleanup() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=*), parameter :: test_source = "test_cleanup.f90"
        logical :: gcov_exists_before, gcov_exists_after
        
        print *, "  Test 6: File cleanup"
        
        call setup_test_gcov_environment("test_cleanup")
        
        ! Given: A successful gcov execution
        call executor%execute_gcov(test_source, gcov_files, error_ctx)
        
        ! Check if .gcov file was created
        if (size(gcov_files) > 0) then
            inquire(file=gcov_files(1), exist=gcov_exists_before)
        else
            gcov_exists_before = .false.
        end if
        
        ! When: Calling cleanup
        call executor%cleanup_gcov_files(gcov_files)
        
        ! Then: .gcov files should be removed
        if (size(gcov_files) > 0) then
            inquire(file=gcov_files(1), exist=gcov_exists_after)
        else
            gcov_exists_after = .false.
        end if
        
        passed = gcov_exists_before .and. (.not. gcov_exists_after)
        
        call cleanup_test_environment("test_cleanup")
        
        if (.not. passed) then
            print *, "    FAILED: File cleanup failed"
            print *, "    Before cleanup:", gcov_exists_before
            print *, "    After cleanup:", gcov_exists_after
        else
            print *, "    PASSED"
        end if
    end function test_file_cleanup

    function test_multiple_source_files() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=256), parameter :: source_files(3) = &
            ["test_multi1.f90", "test_multi2.f90", "test_multi3.f90"]
        integer :: i, total_gcov_files
        
        print *, "  Test 7: Multiple source file processing"
        
        ! Given: Multiple source files with coverage data
        do i = 1, size(source_files)
            call setup_test_gcov_environment(source_files(i)(1:11))
        end do
        
        total_gcov_files = 0
        
        ! When: Processing each source file
        do i = 1, size(source_files)
            call executor%execute_gcov(source_files(i), gcov_files, error_ctx)
            if (error_ctx%error_code == ERROR_SUCCESS) then
                total_gcov_files = total_gcov_files + size(gcov_files)
            end if
        end do
        
        ! Then: Should process all files successfully
        passed = (total_gcov_files == size(source_files))
        
        do i = 1, size(source_files)
            call cleanup_test_environment(source_files(i)(1:11))
        end do
        
        if (.not. passed) then
            print *, "    FAILED: Multiple file processing failed"
            print *, "    Expected files:", size(source_files)
            print *, "    Processed files:", total_gcov_files
        else
            print *, "    PASSED"
        end if
    end function test_multiple_source_files

    function test_gcov_file_generation() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=*), parameter :: test_source = "test_generation.f90"
        character(len=1024) :: line
        integer :: unit, stat, line_count
        logical :: has_coverage_data
        
        print *, "  Test 8: .gcov file generation verification"
        
        call setup_test_gcov_environment("test_generation")
        
        ! Given: A source file with coverage data
        ! When: Executing gcov command
        call executor%execute_gcov(test_source, gcov_files, error_ctx)
        
        ! Then: Generated .gcov file should contain valid coverage data
        passed = .false.
        if (error_ctx%error_code == ERROR_SUCCESS .and. size(gcov_files) > 0) then
            ! Read and verify .gcov file content
            open(newunit=unit, file=gcov_files(1), status='old', iostat=stat)
            if (stat == 0) then
                line_count = 0
                has_coverage_data = .false.
                do
                    read(unit, '(A)', iostat=stat) line
                    if (stat /= 0) exit
                    line_count = line_count + 1
                    ! Look for coverage data markers in .gcov format
                    if (index(line, ':') > 0 .and. line_count > 1) then
                        has_coverage_data = .true.
                    end if
                end do
                close(unit)
                passed = (line_count > 0) .and. has_coverage_data
            end if
        end if
        
        call cleanup_test_environment("test_generation")
        
        if (.not. passed) then
            print *, "    FAILED: .gcov file generation/verification failed"
            if (allocated(gcov_files) .and. size(gcov_files) > 0) then
                print *, "    Generated file:", trim(gcov_files(1))
            end if
        else
            print *, "    PASSED"
        end if
    end function test_gcov_file_generation

    function test_error_context_handling() result(passed)
        logical :: passed
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: gcov_files(:)
        character(len=*), parameter :: test_source = "invalid_source.f90"
        
        print *, "  Test 9: Error context handling"
        
        ! Given: An invalid scenario (missing files)
        ! When: Executing gcov command
        call executor%execute_gcov(test_source, gcov_files, error_ctx)
        
        ! Then: Should provide comprehensive error context
        passed = (error_ctx%error_code /= ERROR_SUCCESS) .and. &
                 (len_trim(error_ctx%message) > 0) .and. &
                 (len_trim(error_ctx%suggestion) > 0) .and. &
                 (len_trim(error_ctx%context) > 0)
        
        if (.not. passed) then
            print *, "    FAILED: Error context not properly populated"
            print *, "    Error code:", error_ctx%error_code
            print *, "    Message length:", len_trim(error_ctx%message)
            print *, "    Suggestion length:", len_trim(error_ctx%suggestion)
            print *, "    Context length:", len_trim(error_ctx%context)
        else
            print *, "    PASSED"
        end if
    end function test_error_context_handling

    ! Helper subroutines for test setup and cleanup
    subroutine setup_test_gcov_environment(test_name)
        character(len=*), intent(in) :: test_name
        integer :: unit, stat
        character(len=256) :: source_file, gcda_file, gcno_file, gcov_file
        
        source_file = trim(test_name) // ".f90"
        gcda_file = trim(test_name) // ".gcda"
        gcno_file = trim(test_name) // ".gcno"
        gcov_file = trim(source_file) // ".gcov"
        
        ! Create minimal source file
        open(newunit=unit, file=source_file, status='replace')
        write(unit, '(A)') "program " // trim(test_name)
        write(unit, '(A)') "  implicit none"
        write(unit, '(A)') "  print *, 'Test program'"
        write(unit, '(A)') "end program " // trim(test_name)
        close(unit)
        
        ! Create dummy .gcda file (would be generated by instrumented program)
        open(newunit=unit, file=gcda_file, access='stream', status='replace')
        write(unit) 1_1, 2_1, 3_1, 4_1  ! Minimal binary data
        close(unit)
        
        ! Create dummy .gcno file (would be generated by compiler)
        open(newunit=unit, file=gcno_file, access='stream', status='replace')
        write(unit) 5_1, 6_1, 7_1, 8_1  ! Minimal binary data
        close(unit)
        
        ! For testing purposes, pre-create a mock .gcov file
        ! In real usage, this would be generated by gcov command
        open(newunit=unit, file=gcov_file, status='replace')
        write(unit, '(A)') "        -:    0:Source:" // trim(source_file)
        write(unit, '(A)') "        -:    1:program " // trim(test_name)
        write(unit, '(A)') "        -:    2:  implicit none"
        write(unit, '(A)') "        1:    3:  print *, 'Test program'"
        write(unit, '(A)') "        -:    4:end program " // trim(test_name)
        close(unit)
    end subroutine setup_test_gcov_environment

    subroutine setup_test_gcov_environment_in_dir(dir, test_name)
        character(len=*), intent(in) :: dir, test_name
        integer :: stat
        
        call execute_command_line("mkdir -p " // dir, exitstat=stat)
        call execute_command_line("cd " // dir, exitstat=stat)
        call setup_test_gcov_environment(test_name)
        call execute_command_line("cd ..", exitstat=stat)
    end subroutine setup_test_gcov_environment_in_dir

    subroutine cleanup_test_environment(test_name)
        character(len=*), intent(in) :: test_name
        
        call cleanup_test_file(trim(test_name) // ".f90")
        call cleanup_test_file(trim(test_name) // ".gcda")
        call cleanup_test_file(trim(test_name) // ".gcno")
        call cleanup_test_file(trim(test_name) // ".f90.gcov")
        call execute_command_line("rm -rf " // test_name)
    end subroutine cleanup_test_environment

    subroutine cleanup_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit, stat
        logical :: exists
        
        inquire(file=filename, exist=exists)
        if (exists) then
            open(newunit=unit, file=filename, status='old', iostat=stat)
            if (stat == 0) then
                close(unit, status='delete')
            end if
        end if
    end subroutine cleanup_test_file

end program test_gcov_command_executor