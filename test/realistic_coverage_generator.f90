module realistic_coverage_generator
    !! Realistic Coverage Data Generator for Testing
    !! 
    !! This module provides utilities for generating realistic gcov output
    !! patterns that mirror real-world coverage scenarios for comprehensive
    !! testing of fortcov's parsing and calculation capabilities.
    
    implicit none
    private
    
    public :: generate_realistic_gcov_file
    public :: generate_branch_coverage_gcov
    public :: generate_function_coverage_gcov
    public :: generate_complex_control_flow_gcov
    public :: generate_edge_case_gcov
    public :: generate_precision_test_gcov
    public :: gcov_pattern_t
    
    ! Test pattern configuration
    type :: gcov_pattern_t
        logical :: include_headers = .true.
        logical :: include_branches = .false.
        logical :: include_functions = .false.
        logical :: include_unexecuted_lines = .false.
        logical :: include_large_counts = .false.
        integer :: max_execution_count = 100
        real :: coverage_percentage = 75.0
        character(len=:), allocatable :: source_filename
        character(len=:), allocatable :: program_type  ! 'simple', 'complex', 'loops', 'conditionals'
    end type gcov_pattern_t
    
contains

    subroutine generate_realistic_gcov_file(filename, pattern)
        !! Generate a realistic .gcov file with specified patterns
        character(len=*), intent(in) :: filename
        type(gcov_pattern_t), intent(in) :: pattern
        
        integer :: unit
        character(len=256) :: source_name
        
        if (allocated(pattern%source_filename)) then
            source_name = pattern%source_filename
        else
            source_name = "test_source.f90"
        end if
        
        open(newunit=unit, file=filename, status='replace')
        
        ! Generate headers if requested
        if (pattern%include_headers) then
            call write_gcov_headers(unit, source_name)
        end if
        
        ! Generate coverage data based on program type
        if (allocated(pattern%program_type)) then
            select case (trim(pattern%program_type))
            case ('simple')
                call write_simple_program_coverage(unit, pattern)
            case ('complex')
                call write_complex_program_coverage(unit, pattern)
            case ('loops')
                call write_loop_intensive_coverage(unit, pattern)
            case ('conditionals')
                call write_conditional_heavy_coverage(unit, pattern)
            case default
                call write_simple_program_coverage(unit, pattern)
            end select
        else
            call write_simple_program_coverage(unit, pattern)
        end if
        
        close(unit)
    end subroutine generate_realistic_gcov_file
    
    subroutine generate_branch_coverage_gcov(filename, branch_patterns)
        !! Generate gcov with realistic branch coverage patterns
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: branch_patterns  ! 'partial', 'complete', 'mixed'
        
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        
        call write_gcov_headers(unit, "branch_test.f90")
        
        ! Branch coverage scenarios
        write(unit, '(A)') "        -:    1:program branch_test"
        write(unit, '(A)') "        1:    2:    integer :: x = 5"
        write(unit, '(A)') "        1:    3:    if (x > 3) then"
        
        select case (trim(branch_patterns))
        case ('partial')
            write(unit, '(A)') "branch  0 taken 1 (fallthrough)"
            write(unit, '(A)') "branch  1 never executed"
            write(unit, '(A)') "        1:    4:        print *, 'x is large'"
            write(unit, '(A)') "    #####:    5:    else"
            write(unit, '(A)') "    #####:    6:        print *, 'x is small'"
        case ('complete')
            write(unit, '(A)') "branch  0 taken 3 (fallthrough)"
            write(unit, '(A)') "branch  1 taken 2"
            write(unit, '(A)') "        3:    4:        print *, 'x is large'"
            write(unit, '(A)') "        2:    5:    else"
            write(unit, '(A)') "        2:    6:        print *, 'x is small'"
        case ('mixed')
            write(unit, '(A)') "branch  0 taken 5 (fallthrough)"
            write(unit, '(A)') "branch  1 taken 3"
            write(unit, '(A)') "branch  2 never executed"
            write(unit, '(A)') "        5:    4:        print *, 'x is large'"
            write(unit, '(A)') "        3:    5:    else if (x < 0) then"
            write(unit, '(A)') "    #####:    6:        print *, 'x is negative'"
            write(unit, '(A)') "        3:    7:    else"
            write(unit, '(A)') "        3:    8:        print *, 'x is small positive'"
        end select
        
        write(unit, '(A)') "        -:    9:    end if"
        write(unit, '(A)') "        1:   10:end program"
        
        close(unit)
    end subroutine generate_branch_coverage_gcov
    
    subroutine generate_function_coverage_gcov(filename)
        !! Generate gcov with realistic function coverage patterns
        character(len=*), intent(in) :: filename
        
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        
        call write_gcov_headers(unit, "function_test.f90")
        
        ! Function coverage with call statistics
        write(unit, '(A)') "        -:    1:module function_test"
        write(unit, '(A)') "        -:    2:    implicit none"
        write(unit, '(A)') "        -:    3:contains"
        write(unit, '(A)') "        -:    4:"
        
        ! Frequently called function
        write(unit, '(A)') "function frequently_called called 150 returned 100%"
        write(unit, '(A)') "      150:    5:    integer function frequently_called(x)"
        write(unit, '(A)') "        -:    6:        integer, intent(in) :: x"
        write(unit, '(A)') "      150:    7:        frequently_called = x * 2"
        write(unit, '(A)') "      150:    8:    end function"
        write(unit, '(A)') "        -:    9:"
        
        ! Occasionally called function
        write(unit, '(A)') "function occasionally_called called 12 returned 100%"
        write(unit, '(A)') "       12:   10:    subroutine occasionally_called()"
        write(unit, '(A)') "       12:   11:        print *, 'Occasional call'"
        write(unit, '(A)') "       12:   12:    end subroutine"
        write(unit, '(A)') "        -:   13:"
        
        ! Never called function
        write(unit, '(A)') "function never_called called 0 returned 0%"
        write(unit, '(A)') "    #####:   14:    subroutine never_called()"
        write(unit, '(A)') "    #####:   15:        print *, 'This never executes'"
        write(unit, '(A)') "    #####:   16:    end subroutine"
        write(unit, '(A)') "        -:   17:"
        
        ! Function with early returns
        write(unit, '(A)') "function early_return called 25 returned 80%"
        write(unit, '(A)') "       25:   18:    logical function early_return(flag)"
        write(unit, '(A)') "        -:   19:        logical, intent(in) :: flag"
        write(unit, '(A)') "       25:   20:        if (flag) then"
        write(unit, '(A)') "       20:   21:            early_return = .true."
        write(unit, '(A)') "       20:   22:            return"
        write(unit, '(A)') "        -:   23:        end if"
        write(unit, '(A)') "        5:   24:        early_return = .false."
        write(unit, '(A)') "        5:   25:    end function"
        write(unit, '(A)') "        -:   26:"
        write(unit, '(A)') "        -:   27:end module"
        
        close(unit)
    end subroutine generate_function_coverage_gcov
    
    subroutine generate_complex_control_flow_gcov(filename)
        !! Generate gcov with complex nested control structures
        character(len=*), intent(in) :: filename
        
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        
        call write_gcov_headers(unit, "complex_flow.f90")
        
        ! Complex nested structure with realistic execution patterns
        write(unit, '(A)') "        -:    1:program complex_flow"
        write(unit, '(A)') "        1:    2:    integer :: i, j, sum = 0"
        write(unit, '(A)') "        1:    3:    logical :: debug = .false."
        write(unit, '(A)') "        -:    4:"
        write(unit, '(A)') "        5:    5:    do i = 1, 5"
        write(unit, '(A)') "branch  0 taken 5 (fallthrough)"
        write(unit, '(A)') "branch  1 taken 1 (exit)"
        write(unit, '(A)') "       20:    6:        do j = 1, 4"
        write(unit, '(A)') "branch  0 taken 20 (fallthrough)"  
        write(unit, '(A)') "branch  1 taken 5 (exit)"
        write(unit, '(A)') "       20:    7:            if (i * j > 10) then"
        write(unit, '(A)') "branch  0 taken 8 (fallthrough)"
        write(unit, '(A)') "branch  1 taken 12"
        write(unit, '(A)') "        8:    8:                sum = sum + i * j"
        write(unit, '(A)') "        8:    9:                if (debug) then"
        write(unit, '(A)') "branch  0 never executed"
        write(unit, '(A)') "branch  1 taken 8 (fallthrough)"
        write(unit, '(A)') "    #####:   10:                    print *, 'Debug:', i, j"
        write(unit, '(A)') "        -:   11:                end if"
        write(unit, '(A)') "       12:   12:            else"
        write(unit, '(A)') "       12:   13:                sum = sum + 1"
        write(unit, '(A)') "        -:   14:            end if"
        write(unit, '(A)') "        -:   15:        end do"
        write(unit, '(A)') "        -:   16:    end do"
        write(unit, '(A)') "        -:   17:"
        write(unit, '(A)') "        1:   18:    print *, 'Final sum:', sum"
        write(unit, '(A)') "        1:   19:end program"
        
        close(unit)
    end subroutine generate_complex_control_flow_gcov
    
    subroutine generate_edge_case_gcov(filename, edge_case_type)
        !! Generate gcov files for edge case scenarios
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: edge_case_type  ! 'zero_coverage', 'perfect_coverage', 'large_counts'
        
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        
        call write_gcov_headers(unit, "edge_case.f90")
        
        select case (trim(edge_case_type))
        case ('zero_coverage')
            ! File with no execution at all
            write(unit, '(A)') "        -:    1:program dead_code"
            write(unit, '(A)') "    #####:    2:    print *, 'This never runs'"
            write(unit, '(A)') "    #####:    3:    call unreachable_subroutine()"
            write(unit, '(A)') "    #####:    4:end program"
            write(unit, '(A)') "        -:    5:"
            write(unit, '(A)') "function unreachable_subroutine called 0 returned 0%"
            write(unit, '(A)') "    #####:    6:subroutine unreachable_subroutine()"
            write(unit, '(A)') "    #####:    7:    print *, 'Never called'"
            write(unit, '(A)') "    #####:    8:end subroutine"
            
        case ('perfect_coverage')
            ! File with 100% line and branch coverage
            write(unit, '(A)') "        -:    1:program perfect"
            write(unit, '(A)') "       10:    2:    integer :: i"
            write(unit, '(A)') "       10:    3:    do i = 1, 10"
            write(unit, '(A)') "branch  0 taken 10 (fallthrough)"
            write(unit, '(A)') "branch  1 taken 1 (exit)"
            write(unit, '(A)') "      100:    4:        if (mod(i, 2) == 0) then"
            write(unit, '(A)') "branch  0 taken 50 (fallthrough)"
            write(unit, '(A)') "branch  1 taken 50"
            write(unit, '(A)') "       50:    5:            print *, 'Even:', i"
            write(unit, '(A)') "       50:    6:        else"
            write(unit, '(A)') "       50:    7:            print *, 'Odd:', i"
            write(unit, '(A)') "        -:    8:        end if"
            write(unit, '(A)') "        -:    9:    end do"
            write(unit, '(A)') "       10:   10:end program"
            
        case ('large_counts')
            ! File with very large execution counts
            write(unit, '(A)') "        -:    1:program large_counts"
            write(unit, '(A)') "        1:    2:    integer :: i, j"
            write(unit, '(A)') " 10000000:    3:    do i = 1, 10000000"
            write(unit, '(A)') "branch  0 taken 10000000 (fallthrough)"
            write(unit, '(A)') "branch  1 taken 1 (exit)"
            write(unit, '(A)') "100000000:    4:        do j = 1, 10"
            write(unit, '(A)') "branch  0 taken 100000000 (fallthrough)"
            write(unit, '(A)') "branch  1 taken 10000000 (exit)"
            write(unit, '(A)') "100000000:    5:            ! Hot path computation"
            write(unit, '(A)') "        -:    6:        end do"
            write(unit, '(A)') "        -:    7:    end do"
            write(unit, '(A)') "        1:    8:end program"
            
        end select
        
        close(unit)
    end subroutine generate_edge_case_gcov
    
    subroutine generate_precision_test_gcov(filename)
        !! Generate gcov for floating point precision testing
        character(len=*), intent(in) :: filename
        
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace')
        
        call write_gcov_headers(unit, "precision_test.f90")
        
        ! Create scenario where coverage percentages require precision
        ! Example: 2 out of 3 executable lines = 66.66666...%
        write(unit, '(A)') "        -:    1:program precision_test"
        write(unit, '(A)') "        -:    2:    ! Testing precision in coverage calculations"
        write(unit, '(A)') "        1:    3:    logical :: flag = .true."
        write(unit, '(A)') "        1:    4:    if (flag) then"
        write(unit, '(A)') "branch  0 taken 1 (fallthrough)"
        write(unit, '(A)') "branch  1 never executed"
        write(unit, '(A)') "        1:    5:        print *, 'Executed line'"
        write(unit, '(A)') "    #####:    6:    else"
        write(unit, '(A)') "    #####:    7:        print *, 'Unexecuted line'"
        write(unit, '(A)') "        -:    8:    end if"
        write(unit, '(A)') "        -:    9:    ! Result: 2/3 executable = 66.666...%"
        write(unit, '(A)') "        1:   10:end program"
        
        close(unit)
    end subroutine generate_precision_test_gcov
    
    ! Helper subroutines
    subroutine write_gcov_headers(unit, source_name)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: source_name
        
        write(unit, '(A)') "        -:    0:Source:" // trim(source_name)
        write(unit, '(A)') "        -:    0:Graph:" // trim(source_name) // ".gcno"
        write(unit, '(A)') "        -:    0:Data:" // trim(source_name) // ".gcda"  
        write(unit, '(A)') "        -:    0:Runs:1"
        write(unit, '(A)') "        -:    0:Programs:1"
    end subroutine write_gcov_headers
    
    subroutine write_simple_program_coverage(unit, pattern)
        integer, intent(in) :: unit
        type(gcov_pattern_t), intent(in) :: pattern
        
        ! Basic program structure with configurable coverage
        write(unit, '(A)') "        -:    1:program simple"
        write(unit, '(A)') "        1:    2:    integer :: result"
        
        if (pattern%include_unexecuted_lines) then
            write(unit, '(A)') "        1:    3:    result = calculate(5)"
            write(unit, '(A)') "    #####:    4:    result = calculate(0)  ! Dead code"
        else
            write(unit, '(A)') "        2:    3:    result = calculate(5)"
            write(unit, '(A)') "        1:    4:    result = calculate(3)"
        end if
        
        write(unit, '(A)') "        1:    5:    print *, result"
        write(unit, '(A)') "        1:    6:end program"
        
        if (pattern%include_functions) then
            write(unit, '(A)') "        -:    7:"
            write(unit, '(A)') "function calculate called 2 returned 100%"
            write(unit, '(A)') "        2:    8:integer function calculate(x)"
            write(unit, '(A)') "        -:    9:    integer, intent(in) :: x"
            write(unit, '(A)') "        2:   10:    calculate = x * x"
            write(unit, '(A)') "        2:   11:end function"
        end if
    end subroutine write_simple_program_coverage
    
    subroutine write_complex_program_coverage(unit, pattern)
        integer, intent(in) :: unit
        type(gcov_pattern_t), intent(in) :: pattern
        
        ! More complex program with multiple control structures
        write(unit, '(A)') "        -:    1:program complex"
        write(unit, '(A)') "        1:    2:    integer :: i, sum = 0"
        write(unit, '(A)') "       10:    3:    do i = 1, 10"
        
        if (pattern%include_branches) then
            write(unit, '(A)') "branch  0 taken 10 (fallthrough)"
            write(unit, '(A)') "branch  1 taken 1 (exit)"
        end if
        
        write(unit, '(A)') "       10:    4:        if (mod(i, 2) == 0) then"
        
        if (pattern%include_branches) then
            write(unit, '(A)') "branch  0 taken 5 (fallthrough)"
            write(unit, '(A)') "branch  1 taken 5"
        end if
        
        write(unit, '(A)') "        5:    5:            sum = sum + i"
        write(unit, '(A)') "        -:    6:        end if"
        write(unit, '(A)') "        -:    7:    end do"
        write(unit, '(A)') "        1:    8:    print *, sum"
        write(unit, '(A)') "        1:    9:end program"
    end subroutine write_complex_program_coverage
    
    subroutine write_loop_intensive_coverage(unit, pattern)
        integer, intent(in) :: unit
        type(gcov_pattern_t), intent(in) :: pattern
        
        ! Loop-heavy program
        write(unit, '(A)') "        -:    1:program loops"
        write(unit, '(A)') "        1:    2:    integer :: i, j, k, count = 0"
        write(unit, '(A)') "       10:    3:    do i = 1, 10"
        write(unit, '(A)') "       50:    4:        do j = 1, 5"
        write(unit, '(A)') "      100:    5:            do k = 1, 2"
        write(unit, '(A)') "      100:    6:                count = count + 1"
        write(unit, '(A)') "        -:    7:            end do"
        write(unit, '(A)') "        -:    8:        end do"
        write(unit, '(A)') "        -:    9:    end do"
        write(unit, '(A)') "        1:   10:    print *, count"
        write(unit, '(A)') "        1:   11:end program"
    end subroutine write_loop_intensive_coverage
    
    subroutine write_conditional_heavy_coverage(unit, pattern)
        integer, intent(in) :: unit
        type(gcov_pattern_t), intent(in) :: pattern
        
        ! Conditional-heavy program
        write(unit, '(A)') "        -:    1:program conditionals"
        write(unit, '(A)') "        1:    2:    integer :: x = 5, result = 0"
        write(unit, '(A)') "        1:    3:    if (x > 10) then"
        write(unit, '(A)') "    #####:    4:        result = 1"
        write(unit, '(A)') "        1:    5:    else if (x > 5) then"
        write(unit, '(A)') "    #####:    6:        result = 2"
        write(unit, '(A)') "        1:    7:    else if (x > 0) then"
        write(unit, '(A)') "        1:    8:        result = 3"
        write(unit, '(A)') "    #####:    9:    else"
        write(unit, '(A)') "    #####:   10:        result = 4"
        write(unit, '(A)') "        -:   11:    end if"
        write(unit, '(A)') "        1:   12:    print *, result"
        write(unit, '(A)') "        1:   13:end program"
    end subroutine write_conditional_heavy_coverage

end module realistic_coverage_generator
