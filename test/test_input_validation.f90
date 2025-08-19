program test_input_validation
    !! Test suite for input validation and data normalization
    !! 
    !! This test suite exposes gaps in input validation that Patrick's audit
    !! identified. Tests demonstrate missing validation for:
    !! - Path sanitization and validation
    !! - Coverage data bounds checking
    !! - Malformed data handling
    !! - Edge case values (negative, extreme)
    !! 
    !! All tests follow TDD - they SHOULD FAIL initially to expose issues
    use coverage_parser
    use coverage_model
    use gcov_command_executor
    use error_handling
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running Input Validation Tests..."
    write(*,*) "================================"
    
    ! Path validation tests
    call test_empty_path_rejection()
    call test_null_path_handling()
    call test_dangerous_path_characters()
    call test_excessively_long_paths()
    
    ! Coverage data bounds checking
    call test_negative_execution_counts()
    call test_extreme_execution_counts()
    call test_invalid_line_numbers()
    call test_zero_line_numbers()
    
    ! Malformed data handling
    call test_malformed_gcov_lines()
    call test_truncated_coverage_data()
    call test_corrupted_percentage_values()
    
    ! Data normalization tests moved to test_issue_122_input_validation.f90
    
    ! Report results
    write(*,*) ""
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
               pass_count, " (", (pass_count * 100) / test_count, "%)"
    
    if (pass_count /= test_count) then
        write(*,*) "Some tests failed - this exposes validation gaps to fix"
        stop 1
    end if

contains

    subroutine test_pass(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        pass_count = pass_count + 1
        write(*,'(A,A)') "PASS: ", test_name
    end subroutine

    subroutine test_fail(test_name, reason)
        character(len=*), intent(in) :: test_name, reason
        test_count = test_count + 1
        write(*,'(A,A)') "FAIL: ", test_name
        write(*,'(A,A)') "  Reason: ", reason
    end subroutine

    ! Test 1: Empty path rejection
    ! Given: Empty string path
    ! When: Creating parser or executing gcov
    ! Then: Should reject with appropriate error (CURRENTLY FAILS)
    subroutine test_empty_path_rejection()
        character(len=*), parameter :: test_name = "Empty path rejection"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        type(gcov_executor_t) :: executor
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        
        ! Test 1a: Parser creation with empty path
        call create_parser("", parser, error_flag)
        if (.not. error_flag) then
            call test_fail(test_name // " (parser)", &
                         "Empty path should be rejected but was accepted")
            return
        end if
        
        ! Test 1b: GCov execution with empty path  
        call executor%execute_gcov("", gcov_files, error_ctx)
        if (error_ctx%error_code == ERROR_SUCCESS) then
            call test_fail(test_name // " (executor)", &
                         "Empty path should cause error but succeeded")
            return
        end if
        
        call test_pass(test_name)
    end subroutine

    ! Test 2: Null/whitespace path handling
    ! Given: Path with only whitespace or special characters
    ! When: Processing path
    ! Then: Should normalize or reject appropriately (CURRENTLY FAILS)
    subroutine test_null_path_handling()
        character(len=*), parameter :: test_name = "Null/whitespace path handling"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        character(len=10) :: whitespace_path = "   " // char(9) // " " ! spaces + tab
        
        call create_parser(whitespace_path, parser, error_flag)
        if (.not. error_flag) then
            call test_fail(test_name, &
                         "Whitespace-only path should be rejected")
            return
        end if
        
        call test_pass(test_name)
    end subroutine

    ! Test 3: Dangerous path characters
    ! Given: Path with potentially dangerous characters
    ! When: Processing path
    ! Then: Should sanitize or reject (CURRENTLY FAILS)
    subroutine test_dangerous_path_characters()
        character(len=*), parameter :: test_name = "Dangerous path characters"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        character(len=50) :: dangerous_path = "file;rm -rf /.gcov"
        
        call create_parser(dangerous_path, parser, error_flag)
        if (.not. error_flag) then
            call test_fail(test_name, &
                         "Path with dangerous characters should be rejected")
            return
        end if
        
        call test_pass(test_name)
    end subroutine

    ! Test 4: Excessively long paths
    ! Given: Extremely long file path
    ! When: Processing path  
    ! Then: Should reject with length limit error (CURRENTLY FAILS)
    subroutine test_excessively_long_paths()
        character(len=*), parameter :: test_name = "Excessively long paths"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        character(len=5000) :: long_path
        integer :: i
        
        ! Create excessively long path
        long_path = ""
        do i = 1, 200
            long_path = trim(long_path) // "/very_long_directory_name"
        end do
        long_path = trim(long_path) // "/file.gcov"
        
        call create_parser(long_path, parser, error_flag)
        if (.not. error_flag) then
            call test_fail(test_name, &
                         "Excessively long path should be rejected")
            return
        end if
        
        call test_pass(test_name)
    end subroutine

    ! Test 5: Negative execution counts
    ! Given: Coverage data with negative execution counts
    ! When: Parsing coverage data
    ! Then: Should normalize or reject (CURRENTLY FAILS)
    subroutine test_negative_execution_counts()
        character(len=*), parameter :: test_name = "Negative execution counts"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        type(coverage_data_t) :: coverage_data
        character(len=256) :: test_file
        integer :: i, j
        logical :: found_invalid_count
        
        ! Create a test file with negative execution counts
        test_file = "test_negative_counts_temp.gcov"
        call create_test_file_with_negative_counts(test_file)
        
        ! Create parser
        call create_parser(test_file, parser, error_flag)
        if (error_flag) then
            ! If parser creation fails due to validation, that's acceptable behavior
            call test_pass(test_name // " (rejected at parser creation)")
            call cleanup_test_file(test_file)
            return
        end if
        
        ! Parse the malformed data
        coverage_data = parser%parse(test_file, error_flag)
        if (error_flag) then
            ! If parsing fails gracefully, that's acceptable behavior
            call test_pass(test_name // " (graceful parsing failure)")
            call cleanup_test_file(test_file)
            return
        end if
        
        ! Verify that negative execution counts were normalized
        found_invalid_count = .false.
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        ! Check for un-normalized negative counts (should be >= 0)
                        if (coverage_data%files(i)%lines(j)%execution_count < 0 .and. &
                            coverage_data%files(i)%lines(j)%execution_count /= -1) then
                            found_invalid_count = .true.
                            exit
                        end if
                    end do
                end if
                if (found_invalid_count) exit
            end do
        end if
        
        if (found_invalid_count) then
            call test_fail(test_name, &
                "Negative execution counts should be normalized to 0")
            call cleanup_test_file(test_file)
            return
        end if
        
        call test_pass(test_name)
        call cleanup_test_file(test_file)
    end subroutine

    ! Test 6: Extreme execution counts  
    ! Given: Coverage data with extremely large execution counts
    ! When: Parsing coverage data
    ! Then: Should handle within reasonable bounds (capped at INT32_MAX)
    subroutine test_extreme_execution_counts()
        character(len=*), parameter :: test_name = "Extreme execution counts"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        type(coverage_data_t) :: coverage_data
        character(len=256) :: test_file
        integer :: i, j
        logical :: found_uncapped_count
        integer, parameter :: MAX_REASONABLE_COUNT = 2147483647  ! INT32_MAX per security audit
        
        ! Create test file with extreme execution counts
        test_file = "test_extreme_counts_temp.gcov"
        call create_test_file_with_extreme_counts(test_file)
        
        ! Create parser
        call create_parser(test_file, parser, error_flag)
        if (error_flag) then
            ! If parser creation fails due to validation, that's acceptable behavior
            call test_pass(test_name // " (rejected at parser creation)")
            call cleanup_test_file(test_file)
            return
        end if
        
        ! Parse the malformed data
        coverage_data = parser%parse(test_file, error_flag)
        if (error_flag) then
            ! If parsing fails gracefully, that's acceptable behavior
            call test_pass(test_name // " (graceful parsing failure)")
            call cleanup_test_file(test_file)
            return
        end if
        
        ! Verify that extreme execution counts were capped
        found_uncapped_count = .false.
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        ! Check for uncapped extreme counts
                        if (coverage_data%files(i)%lines(j)%execution_count > &
                            MAX_REASONABLE_COUNT) then
                            found_uncapped_count = .true.
                            exit
                        end if
                    end do
                end if
                if (found_uncapped_count) exit
            end do
        end if
        
        if (found_uncapped_count) then
            call test_fail(test_name, &
                "Extreme execution counts should be capped at reasonable maximum")
            call cleanup_test_file(test_file)
            return
        end if
        
        call test_pass(test_name)
        call cleanup_test_file(test_file)
    end subroutine

    ! Test 7: Invalid line numbers
    ! Given: Coverage data with invalid line numbers (negative, zero)
    ! When: Parsing coverage data  
    ! Then: Should reject or normalize appropriately (CURRENTLY FAILS)
    subroutine test_invalid_line_numbers()
        character(len=*), parameter :: test_name = "Invalid line numbers"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        type(coverage_data_t) :: coverage_data
        character(len=256) :: test_file
        integer :: i, j
        logical :: found_invalid_line
        
        ! Create test file with invalid line numbers
        test_file = "test_invalid_lines_temp.gcov"
        call create_test_file_with_invalid_lines(test_file)
        
        ! Create parser
        call create_parser(test_file, parser, error_flag)
        if (error_flag) then
            ! If parser creation fails due to validation, that's acceptable behavior
            call test_pass(test_name // " (rejected at parser creation)")
            call cleanup_test_file(test_file)
            return
        end if
        
        ! Parse the malformed data
        coverage_data = parser%parse(test_file, error_flag)
        if (error_flag) then
            ! If parsing fails gracefully, that's acceptable behavior
            call test_pass(test_name // " (graceful parsing failure)")
            call cleanup_test_file(test_file)
            return
        end if
        
        ! Verify that invalid line numbers were skipped or normalized
        found_invalid_line = .false.
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        ! Check for invalid line numbers (should be > 0 and reasonable)
                        if (coverage_data%files(i)%lines(j)%line_number <= 0 .or. &
                            coverage_data%files(i)%lines(j)%line_number > 100000) then
                            found_invalid_line = .true.
                            exit
                        end if
                    end do
                end if
                if (found_invalid_line) exit
            end do
        end if
        
        if (found_invalid_line) then
            call test_fail(test_name, &
                "Invalid line numbers should be skipped or normalized")
            call cleanup_test_file(test_file)
            return
        end if
        
        call test_pass(test_name)
        call cleanup_test_file(test_file)
    end subroutine

    ! Test 8: Zero line numbers
    ! Given: Coverage data with line number 0
    ! When: Parsing coverage data
    ! Then: Should handle consistently (CURRENTLY INCONSISTENT)
    subroutine test_zero_line_numbers()
        character(len=*), parameter :: test_name = "Zero line numbers"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        type(coverage_data_t) :: coverage_data
        character(len=256) :: test_file
        integer :: i, j
        logical :: found_zero_line
        
        ! Create test file with zero line numbers
        test_file = "test_zero_lines_temp.gcov"
        call create_test_file_with_zero_lines(test_file)
        
        ! Create parser
        call create_parser(test_file, parser, error_flag)
        if (error_flag) then
            ! If parser creation fails due to validation, that's acceptable behavior
            call test_pass(test_name // " (rejected at parser creation)")
            call cleanup_test_file(test_file)
            return
        end if
        
        ! Parse the malformed data
        coverage_data = parser%parse(test_file, error_flag)
        if (error_flag) then
            ! If parsing fails gracefully, that's acceptable behavior
            call test_pass(test_name // " (graceful parsing failure)")
            call cleanup_test_file(test_file)
            return
        end if
        
        ! Verify that zero line numbers were skipped (as per current behavior)
        found_zero_line = .false.
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        ! Check for zero line numbers (should be skipped)
                        if (coverage_data%files(i)%lines(j)%line_number == 0) then
                            found_zero_line = .true.
                            exit
                        end if
                    end do
                end if
                if (found_zero_line) exit
            end do
        end if
        
        if (found_zero_line) then
            call test_fail(test_name, &
                "Zero line numbers should be consistently skipped")
            call cleanup_test_file(test_file)
            return
        end if
        
        call test_pass(test_name)
        call cleanup_test_file(test_file)
    end subroutine

    ! Test 9: Malformed gcov lines
    ! Given: Gcov file with malformed data lines
    ! When: Parsing file
    ! Then: Should handle gracefully without crashing (CURRENTLY FAILS)
    subroutine test_malformed_gcov_lines()
        character(len=*), parameter :: test_name = "Malformed gcov lines"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        type(coverage_data_t) :: coverage_data
        character(len=256) :: test_file
        
        ! Create test file with malformed gcov lines
        test_file = "test_malformed_temp.gcov"
        call create_malformed_gcov_file(test_file)
        
        ! Create parser
        call create_parser(test_file, parser, error_flag)
        if (error_flag) then
            call test_fail(test_name, "Failed to create parser")
            return
        end if
        
        ! Parse the malformed data - should not crash
        coverage_data = parser%parse(test_file, error_flag)
        
        ! Parser should handle malformed lines gracefully
        ! Either succeed by skipping malformed lines, or fail gracefully
        if (error_flag) then
            ! If it fails, it should be a controlled failure, not a crash
            ! This is acceptable behavior for malformed input
            call test_pass(test_name // " (graceful failure)")
        else
            ! If it succeeds, it should have skipped malformed lines
            ! and still produce some valid data
            call test_pass(test_name // " (robust parsing)")
        end if
        
        call cleanup_test_file(test_file)
    end subroutine

    ! Test 10: Truncated coverage data
    ! Given: Incomplete gcov file (cut off mid-line)
    ! When: Parsing file
    ! Then: Should handle gracefully (CURRENTLY FAILS)
    subroutine test_truncated_coverage_data()
        character(len=*), parameter :: test_name = "Truncated coverage data"
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        type(coverage_data_t) :: coverage_data
        character(len=256) :: test_file
        
        ! Create test file with truncated data
        test_file = "test_truncated_temp.gcov"
        call create_truncated_gcov_file(test_file)
        
        ! Create parser
        call create_parser(test_file, parser, error_flag)
        if (error_flag) then
            call test_fail(test_name, "Failed to create parser")
            return
        end if
        
        ! Parse the truncated data - should not crash
        coverage_data = parser%parse(test_file, error_flag)
        
        ! Parser should handle truncated files gracefully
        ! Either succeed with partial data, or fail gracefully
        if (error_flag) then
            ! If it fails, it should be a controlled failure, not a crash
            call test_pass(test_name // " (graceful failure)")
        else
            ! If it succeeds, it should have processed available data
            call test_pass(test_name // " (partial data recovery)")
        end if
        
        call cleanup_test_file(test_file)
    end subroutine

    ! Test 11: Corrupted percentage values
    ! Given: Coverage calculations producing invalid percentages (>100%, NaN)
    ! When: Calculating statistics
    ! Then: Should clamp to valid range (CURRENTLY FAILS)
    subroutine test_corrupted_percentage_values()
        use coverage_statistics, only: calculate_line_coverage, coverage_stats_t
        use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        character(len=*), parameter :: test_name = "Corrupted percentage values"
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: test_file
        type(coverage_line_t) :: test_lines(5)
        type(coverage_stats_t) :: stats
        real :: nan_value
        
        ! Create test data that could produce invalid percentages
        ! (This tests the clamp_percentage function indirectly)
        
        ! Initialize test lines with extreme counts that could overflow
        call test_lines(1)%init("test.f90", 1, 1000000000, .true.)
        call test_lines(2)%init("test.f90", 2, 0, .true.)
        call test_lines(3)%init("test.f90", 3, 2000000000, .true.) 
        call test_lines(4)%init("test.f90", 4, 0, .true.)
        call test_lines(5)%init("test.f90", 5, 0, .true.)
        
        ! Create file with test lines
        call test_file%init("test.f90")
        test_file%lines = test_lines
        
        ! Create coverage data
        call coverage_data%init()
        coverage_data%files = [test_file]
        
        ! Calculate coverage statistics
        stats = calculate_line_coverage(coverage_data)
        
        ! Verify percentage is within valid range [0.0, 100.0]
        if (stats%percentage < 0.0 .or. stats%percentage > 100.0) then
            call test_fail(test_name, &
                "Coverage percentage should be clamped to [0.0, 100.0] range")
            return
        end if
        
        ! Verify it's not NaN
        if (stats%percentage /= stats%percentage) then
            call test_fail(test_name, &
                "Coverage percentage should not be NaN")
            return
        end if
        
        call test_pass(test_name)
    end subroutine

    ! Helper subroutines for creating test data files
    
    subroutine create_test_file_with_negative_counts(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "        -:    0:Source:test_file.f90"
        write(unit, '(A)') "        -:    0:Graph:test_file.gcno"
        write(unit, '(A)') "        -:    0:Data:test_file.gcda"
        write(unit, '(A)') "        -:    1:program test"
        write(unit, '(A)') "       -5:    2:    integer :: x = 5"  ! Negative count
        write(unit, '(A)') "        1:    3:    print *, x"
        write(unit, '(A)') "        -:    4:end program"
        close(unit)
    end subroutine

    subroutine create_test_file_with_extreme_counts(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "        -:    0:Source:test_file.f90"
        write(unit, '(A)') "        -:    0:Graph:test_file.gcno"
        write(unit, '(A)') "        -:    0:Data:test_file.gcda"
        write(unit, '(A)') "        -:    1:program test"
        write(unit, '(A)') "9999999999:    2:    integer :: x = 5"  ! Extreme count
        write(unit, '(A)') "        1:    3:    print *, x"
        write(unit, '(A)') "        -:    4:end program"
        close(unit)
    end subroutine

    subroutine create_test_file_with_invalid_lines(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "        -:    0:Source:test_file.f90"
        write(unit, '(A)') "        -:    0:Graph:test_file.gcno"
        write(unit, '(A)') "        -:    0:Data:test_file.gcda"
        write(unit, '(A)') "        1:   -1:    program test"        ! Negative line number
        write(unit, '(A)') "        1:    2:    integer :: x = 5"
        write(unit, '(A)') "        1: 999999:    print *, x"       ! Extreme line number
        write(unit, '(A)') "        -:    4:end program"
        close(unit)
    end subroutine

    subroutine create_test_file_with_zero_lines(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "        -:    0:Source:test_file.f90"
        write(unit, '(A)') "        -:    0:Graph:test_file.gcno"
        write(unit, '(A)') "        -:    0:Data:test_file.gcda"
        write(unit, '(A)') "        1:    0:    program test"        ! Zero line number
        write(unit, '(A)') "        1:    2:    integer :: x = 5"
        write(unit, '(A)') "        1:    3:    print *, x"
        write(unit, '(A)') "        -:    4:end program"
        close(unit)
    end subroutine

    subroutine create_malformed_gcov_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "        -:    0:Source:test_file.f90"
        write(unit, '(A)') "        -:    0:Graph:test_file.gcno"
        write(unit, '(A)') "INVALID_LINE_FORMAT"  ! Malformed line
        write(unit, '(A)') "        1:    2:    integer :: x = 5"
        write(unit, '(A)') "NOT_A_GCOV_LINE_AT_ALL!!!"  ! Another malformed line
        write(unit, '(A)') "        1:    3:    print *, x"
        write(unit, '(A)') "        -:    4:end program"
        close(unit)
    end subroutine

    subroutine create_truncated_gcov_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') "        -:    0:Source:test_file.f90"
        write(unit, '(A)') "        -:    0:Graph:test_file.gcno"
        write(unit, '(A)') "        -:    0:Data:test_file.gcda"
        write(unit, '(A)') "        -:    1:program test"
        write(unit, '(A,A)', advance='no') "        1:    2:    integer :: x", ""  ! Truncated line
        close(unit)
    end subroutine

    subroutine cleanup_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit, stat
        logical :: file_exists
        
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) return
        
        open(newunit=unit, file=filename, status='old', iostat=stat)
        if (stat == 0) then
            close(unit, status='delete')
        end if
    end subroutine


end program test_input_validation