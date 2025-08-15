program test_coverage_statistics
    use coverage_model
    use coverage_statistics
    use string_utils, only: format_percentage, format_integer, compress_ranges
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running coverage_statistics tests..."
    
    ! Test 1: Calculate 100% line coverage
    call test_calculate_100_percent_line_coverage()
    
    ! Test 2: Calculate partial line coverage  
    call test_calculate_partial_line_coverage()
    
    ! Test 3: Calculate 0% coverage
    call test_calculate_zero_coverage()
    
    ! Test 4: Calculate branch coverage
    call test_calculate_branch_coverage()
    
    ! Test 5: Calculate function coverage
    call test_calculate_function_coverage()
    
    ! Test 6: Aggregate multiple files
    call test_aggregate_file_coverage()
    
    ! Test 7: Handle non-executable lines
    call test_handle_nonexecutable_lines()
    
    ! Test 8: Compress missing line ranges
    call test_compress_missing_ranges()
    
    ! Test 9: Module-level statistics
    call test_module_level_statistics()
    
    ! Test 10: Empty file handling
    call test_empty_file_handling()
    
    ! Test 11: Branch coverage lcov compliance
    call test_branch_coverage_lcov_compliance()
    
    ! Test 12: Zero division edge case - Issue #88
    call test_zero_division_edge_case()
    
    ! Test 13: No coverage data edge case - Issue #88
    call test_no_coverage_data_edge_case()
    
    ! Report results
    write(*,*) ""
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
               pass_count, " (", (pass_count * 100) / test_count, "%)"
    
    if (pass_count /= test_count) then
        stop 1  ! Exit with error code
    end if
    
contains

    subroutine assert(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: expected, actual
        
        test_count = test_count + 1
        if (condition) then
            write(*,'(A,A)') "PASS: ", test_name
            pass_count = pass_count + 1
        else
            write(*,'(A,A)') "FAIL: ", test_name
            write(*,'(A,A)') "  Expected: ", expected
            write(*,'(A,A)') "  Actual:   ", actual
        end if
    end subroutine assert

    ! Test 1: Calculate 100% line coverage
    ! Given: coverage_data_t with 10 lines, all executed
    ! When: Calling calculate_line_coverage()
    ! Then: Should return coverage_stats_t with percentage=100.0
    subroutine test_calculate_100_percent_line_coverage()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_line_t) :: lines(10)
        type(coverage_file_t) :: file_cov
        integer :: i
        
        ! Given: Create 10 lines, all executed
        do i = 1, 10
            lines(i) = coverage_line_t(execution_count=5, line_number=i, &
                                     filename="test.f90", is_executable=.true.)
        end do
        
        file_cov = coverage_file_t("test.f90", lines)
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate line coverage
        stats = calculate_line_coverage(coverage_data)
        
        ! Then: Should return 100.0%
        call assert(abs(stats%percentage - 100.0) < 0.001, &
                   "100% line coverage", "100.0", &
                   trim(format_percentage(real(stats%percentage), 1)))
    end subroutine test_calculate_100_percent_line_coverage

    ! Test 2: Calculate partial line coverage
    ! Given: coverage_data_t with 10 lines, 7 executed
    ! When: Calling calculate_line_coverage()
    ! Then: Should return percentage=70.0, missing_ranges="4-5, 8"
    subroutine test_calculate_partial_line_coverage()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_line_t) :: lines(10)
        type(coverage_file_t) :: file_cov
        integer :: i
        integer :: exec_counts(10) = [1, 1, 1, 0, 0, 1, 1, 0, 1, 1]
        
        ! Given: Create 10 lines, 7 executed (missing lines 4,5,8)
        do i = 1, 10
            lines(i) = coverage_line_t(execution_count=exec_counts(i), &
                                     line_number=i, filename="test.f90", &
                                     is_executable=.true.)
        end do
        
        file_cov = coverage_file_t("test.f90", lines)
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate line coverage
        stats = calculate_line_coverage(coverage_data)
        
        ! Then: Should return 70.0%
        call assert(abs(stats%percentage - 70.0) < 0.001, &
                   "70% line coverage", "70.0", &
                   trim(format_percentage(real(stats%percentage), 1)))
        
        ! And: Missing ranges should be "4-5, 8"
        call assert(trim(stats%missing_ranges) == "4-5, 8", &
                   "missing ranges", "4-5, 8", &
                   trim(stats%missing_ranges))
    end subroutine test_calculate_partial_line_coverage

    ! Test 3: Calculate 0% coverage
    ! Given: coverage_data_t with all execution_count=0
    ! When: Calling calculate_line_coverage()
    ! Then: Should return percentage=0.0
    subroutine test_calculate_zero_coverage()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_line_t) :: lines(5)
        type(coverage_file_t) :: file_cov
        integer :: i
        
        ! Given: Create 5 lines, none executed
        do i = 1, 5
            lines(i) = coverage_line_t(execution_count=0, line_number=i, &
                                     filename="test.f90", is_executable=.true.)
        end do
        
        file_cov = coverage_file_t("test.f90", lines)
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate line coverage
        stats = calculate_line_coverage(coverage_data)
        
        ! Then: Should return 0.0%
        call assert(abs(stats%percentage - 0.0) < 0.001, &
                   "0% line coverage", "0.0", &
                   trim(format_percentage(real(stats%percentage), 1)))
    end subroutine test_calculate_zero_coverage

    ! Test 4: Calculate branch coverage  
    ! Given: 4 branches with different coverage patterns
    ! When: Calling calculate_branch_coverage() with lcov standard
    ! Then: Should return percentage=75.0 (3/4 branches covered)
    subroutine test_calculate_branch_coverage()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_branch_t) :: branches(4)
        type(coverage_function_t) :: func
        type(coverage_file_t) :: file_cov
        
        ! Given: Create branches with different coverage patterns
        branches(1) = coverage_branch_t(taken_count=5, not_taken_count=3, &
                                       branch_id=1, line_number=10, &
                                       filename="test.f90")  ! Covered (lcov)
        branches(2) = coverage_branch_t(taken_count=2, not_taken_count=4, &
                                       branch_id=2, line_number=15, &
                                       filename="test.f90")  ! Covered (lcov)
        branches(3) = coverage_branch_t(taken_count=1, not_taken_count=0, &
                                       branch_id=3, line_number=20, &
                                       filename="test.f90")  ! Covered (lcov)
        branches(4) = coverage_branch_t(taken_count=0, not_taken_count=0, &
                                       branch_id=4, line_number=25, &
                                       filename="test.f90")  ! Not covered
        
        func = coverage_function_t("test_func", "test_mod", .false., 1, &
                                  5, "test.f90")
        func%branches = branches
        file_cov = coverage_file_t("test.f90", [coverage_line_t(1, 1, &
                                  "test.f90", .true.)])
        file_cov%functions = [func]
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate branch coverage
        stats = calculate_branch_coverage(coverage_data)
        
        ! Then: Should return 75.0% (3 out of 4 branches covered per lcov)
        call assert(abs(stats%percentage - 75.0) < 0.001, &
                   "75% branch coverage (lcov)", "75.0", &
                   trim(format_percentage(real(stats%percentage), 1)))
    end subroutine test_calculate_branch_coverage

    ! Test 5: Calculate function coverage
    ! Given: 5 functions: 3 called, 2 never called
    ! When: Calling calculate_function_coverage()
    ! Then: Should return percentage=60.0
    subroutine test_calculate_function_coverage()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_function_t) :: functions(5)
        type(coverage_file_t) :: file_cov
        integer :: i
        integer :: exec_counts(5) = [1, 0, 5, 0, 2]  ! 3 called, 2 not
        
        ! Given: Create 5 functions, 3 called
        do i = 1, 5
            functions(i) = coverage_function_t(name="func"//char(48+i), &
                                              parent_module="test_mod", &
                                              is_module_procedure=.false., &
                                              execution_count=exec_counts(i), &
                                              line_number=i*10, &
                                              filename="test.f90")
        end do
        
        file_cov = coverage_file_t("test.f90", [coverage_line_t(1, 1, &
                                  "test.f90", .true.)])
        file_cov%functions = functions
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate function coverage
        stats = calculate_function_coverage(coverage_data)
        
        ! Then: Should return 60.0%
        call assert(abs(stats%percentage - 60.0) < 0.001, &
                   "60% function coverage", "60.0", &
                   trim(format_percentage(real(stats%percentage), 1)))
    end subroutine test_calculate_function_coverage

    ! Test 6: Aggregate multiple files
    ! Given: File A (80% coverage), File B (60% coverage)
    ! When: Calling aggregate_file_coverage()
    ! Then: Should return weighted average based on line counts
    subroutine test_aggregate_file_coverage()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_line_t) :: lines_a(10), lines_b(5)
        type(coverage_file_t) :: files(2)
        integer :: i
        
        ! Given: File A with 10 lines, 8 covered (80%)
        do i = 1, 10
            if (i <= 8) then
                lines_a(i) = coverage_line_t(execution_count=1, &
                           line_number=i, filename="a.f90", &
                           is_executable=.true.)
            else
                lines_a(i) = coverage_line_t(execution_count=0, &
                           line_number=i, filename="a.f90", &
                           is_executable=.true.)
            end if
        end do
        
        ! File B with 5 lines, 3 covered (60%)
        do i = 1, 5
            if (i <= 3) then
                lines_b(i) = coverage_line_t(execution_count=1, &
                           line_number=i, filename="b.f90", &
                           is_executable=.true.)
            else
                lines_b(i) = coverage_line_t(execution_count=0, &
                           line_number=i, filename="b.f90", &
                           is_executable=.true.)
            end if
        end do
        
        files(1) = coverage_file_t("a.f90", lines_a)
        files(2) = coverage_file_t("b.f90", lines_b)
        coverage_data = coverage_data_t(files)
        
        ! When: Calculate aggregate coverage
        stats = calculate_line_coverage(coverage_data)
        
        ! Then: Should return weighted average: (8+3)/(10+5) = 11/15 = 73.33%
        call assert(abs(stats%percentage - 73.33) < 0.1, &
                   "aggregate file coverage", "73.33", &
                   trim(format_percentage(real(stats%percentage), 2)))
    end subroutine test_aggregate_file_coverage

    ! Test 7: Handle non-executable lines
    ! Given: File with 20 lines, 10 executable, 8 covered
    ! When: Calculating coverage
    ! Then: Should return 80.0% (8/10, not 8/20)
    subroutine test_handle_nonexecutable_lines()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_line_t) :: lines(20)
        type(coverage_file_t) :: file_cov
        integer :: i
        
        ! Given: 20 lines, only even numbers executable, 8 of 10 covered
        do i = 1, 20
            if (mod(i, 2) == 0) then  ! Even lines executable
                if (i <= 16) then     ! First 8 executable lines covered
                    lines(i) = coverage_line_t(execution_count=1, &
                             line_number=i, filename="test.f90", &
                             is_executable=.true.)
                else                  ! Last 2 executable lines not covered
                    lines(i) = coverage_line_t(execution_count=0, &
                             line_number=i, filename="test.f90", &
                             is_executable=.true.)
                end if
            else  ! Odd lines not executable
                lines(i) = coverage_line_t(execution_count=0, &
                         line_number=i, filename="test.f90", &
                         is_executable=.false.)
            end if
        end do
        
        file_cov = coverage_file_t("test.f90", lines)
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate coverage
        stats = calculate_line_coverage(coverage_data)
        
        ! Then: Should return 80.0% (8 covered / 10 executable)
        call assert(abs(stats%percentage - 80.0) < 0.001, &
                   "non-executable lines", "80.0", &
                   trim(format_percentage(real(stats%percentage), 1)))
    end subroutine test_handle_nonexecutable_lines

    ! Test 8: Compress missing line ranges
    ! Given: Uncovered lines [3,4,5,10,11,20]
    ! When: Generating missing_ranges string
    ! Then: Should return "3-5, 10-11, 20"
    subroutine test_compress_missing_ranges()
        integer :: missing_lines(6) = [3, 4, 5, 10, 11, 20]
        character(len=:), allocatable :: result
        
        ! When: Compressing ranges
        result = compress_ranges(missing_lines)
        
        ! Then: Should return compressed format
        call assert(trim(result) == "3-5, 10-11, 20", &
                   "compress missing ranges", "3-5, 10-11, 20", &
                   trim(result))
    end subroutine test_compress_missing_ranges

    ! Test 9: Module-level statistics
    ! Given: Module with multiple procedures
    ! When: Calculating module coverage
    ! Then: Should aggregate all procedures within module
    subroutine test_module_level_statistics()
        type(coverage_data_t) :: coverage_data
        type(module_stats_t) :: stats
        type(coverage_function_t) :: functions(3)
        type(coverage_file_t) :: file_cov
        
        ! Given: Module with 3 procedures, 2 covered
        functions(1) = coverage_function_t("proc1", "my_module", .true., &
                                          5, 10, "test.f90")
        functions(2) = coverage_function_t("proc2", "my_module", .true., &
                                          0, 20, "test.f90")
        functions(3) = coverage_function_t("proc3", "my_module", .true., &
                                          3, 30, "test.f90")
        
        file_cov = coverage_file_t("test.f90", [coverage_line_t(1, 1, &
                                  "test.f90", .true.)])
        file_cov%functions = functions
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate module statistics
        stats = calculate_module_coverage(coverage_data, "my_module")
        
        ! Then: Should return module-level stats
        call assert(abs(stats%function_percentage - 66.67) < 0.1, &
                   "module function coverage", "66.67", &
                   trim(format_percentage(real(stats%function_percentage), 2)))
    end subroutine test_module_level_statistics

    ! Test 10: Empty file handling  
    ! Given: coverage_data_t with no executable lines
    ! When: Calculating statistics
    ! Then: Should return 0.0% (0/0 should be 0% per Issue #88)
    subroutine test_empty_file_handling()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_file_t) :: file_cov
        
        ! Given: Empty file with no lines
        file_cov = coverage_file_t("empty.f90", [coverage_line_t(0, 1, &
                                  "empty.f90", .false.)])
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate coverage
        stats = calculate_line_coverage(coverage_data)
        
        ! Then: Should return 0.0% (Issue #88 fix: 0/0 = 0% not 100%)
        call assert(abs(stats%percentage - 0.0) < 0.001, &
                   "empty file handling", "0.0", &
                   trim(format_percentage(real(stats%percentage), 1)))
    end subroutine test_empty_file_handling

    ! Test 11: Branch coverage lcov compliance  
    ! Given: Branches with only taken_count > 0 (no not_taken_count)
    ! When: Calculating branch coverage with lcov standard
    ! Then: Should count branch as covered (current implementation fails)
    subroutine test_branch_coverage_lcov_compliance()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_branch_t) :: branches(3)
        type(coverage_function_t) :: func
        type(coverage_file_t) :: file_cov
        
        ! Given: Create branches following lcov standard:
        ! Branch 1: taken=5, not_taken=0 (should be covered per lcov)
        ! Branch 2: taken=3, not_taken=2 (fully covered)  
        ! Branch 3: taken=0, not_taken=0 (not covered)
        call branches(1)%init(taken_count=5, not_taken_count=0, branch_id=1, &
                             line_number=10, filename="test.f90")
        call branches(2)%init(taken_count=3, not_taken_count=2, branch_id=2, &
                             line_number=15, filename="test.f90")
        call branches(3)%init(taken_count=0, not_taken_count=0, branch_id=3, &
                             line_number=20, filename="test.f90")
        
        call func%init(name="test_func", parent_module="test_mod", &
                       is_module_procedure=.false., execution_count=1, &
                       line_number=10, filename="test.f90")
        func%branches = branches
        call file_cov%init("test.f90", [coverage_line_t(1, 10, &
                                  "test.f90", .true.)])
        file_cov%functions = [func]
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate branch coverage
        stats = calculate_branch_coverage(coverage_data)
        
        ! Then: Should return 66.67% (2/3 branches covered per lcov standard)
        ! Current implementation incorrectly returns 33.33% (1/3)
        call assert(abs(stats%percentage - 66.67) < 0.01, &
                   "lcov compliant branch coverage", "66.67", &
                   trim(format_percentage(real(stats%percentage), 2)))
                   
        call assert(stats%covered_count == 2, &
                   "lcov covered branch count", "2", &
                   trim(format_integer(stats%covered_count)))
    end subroutine test_branch_coverage_lcov_compliance

    ! Test 12: Zero division edge case - Issue #88
    ! Given: No executable lines (total_lines = 0)
    ! When: Calculating line coverage
    ! Then: Should return 0.0% not 100.0% (0/0 should be 0%)
    subroutine test_zero_division_edge_case()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: stats
        type(coverage_line_t) :: lines(3)
        type(coverage_file_t) :: file_cov
        integer :: i
        
        ! Given: Create file with no executable lines (all comments/blanks)
        do i = 1, 3
            lines(i) = coverage_line_t(execution_count=0, line_number=i, &
                                     filename="comments.f90", &
                                     is_executable=.false.)
        end do
        
        file_cov = coverage_file_t("comments.f90", lines)
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate line coverage (0 covered / 0 total)
        stats = calculate_line_coverage(coverage_data)
        
        ! Then: Should return 0.0% not 100.0% (Issue #88 fix)
        call assert(abs(stats%percentage - 0.0) < 0.001, &
                   "zero division edge case", "0.0", &
                   trim(format_percentage(real(stats%percentage), 1)))
        
        call assert(stats%total_count == 0, &
                   "zero total count", "0", &
                   trim(format_integer(stats%total_count)))
        
        call assert(stats%covered_count == 0, &
                   "zero covered count", "0", &
                   trim(format_integer(stats%covered_count)))
    end subroutine test_zero_division_edge_case

    ! Test 13: No coverage data edge case - Issue #88  
    ! Given: No branches and no functions
    ! When: Calculating branch/function coverage
    ! Then: Should return 0.0% not 100.0%
    subroutine test_no_coverage_data_edge_case()
        type(coverage_data_t) :: coverage_data
        type(coverage_stats_t) :: branch_stats, func_stats
        type(coverage_file_t) :: file_cov
        
        ! Given: File with no functions or branches
        file_cov = coverage_file_t("empty.f90", [coverage_line_t(0, 1, &
                                  "empty.f90", .false.)])
        ! No functions allocated (default)
        coverage_data = coverage_data_t([file_cov])
        
        ! When: Calculate branch coverage (0 covered / 0 total)
        branch_stats = calculate_branch_coverage(coverage_data)
        
        ! Then: Should return 0.0% not 100.0% (Issue #88 fix)
        call assert(abs(branch_stats%percentage - 0.0) < 0.001, &
                   "no branches edge case", "0.0", &
                   trim(format_percentage(real(branch_stats%percentage), 1)))
        
        ! When: Calculate function coverage (0 covered / 0 total)
        func_stats = calculate_function_coverage(coverage_data)
        
        ! Then: Should return 0.0% not 100.0% (Issue #88 fix)
        call assert(abs(func_stats%percentage - 0.0) < 0.001, &
                   "no functions edge case", "0.0", &
                   trim(format_percentage(real(func_stats%percentage), 1)))
    end subroutine test_no_coverage_data_edge_case

end program test_coverage_statistics