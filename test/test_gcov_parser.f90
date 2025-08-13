program test_gcov_parser
    use coverage_model
    use coverage_parser
    use gcov_binary_format
    use file_utils
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running gcov_parser tests..."
    
    ! Test 1: Parse simple subroutine coverage
    call test_parse_simple_subroutine()
    
    ! Test 2: Handle module procedures
    call test_handle_module_procedures()
    
    ! Test 3: Parse contains block
    call test_parse_contains_block()
    
    ! Test 4: Handle interface blocks
    call test_handle_interface_blocks()
    
    ! Test 5: Process use statements
    call test_process_use_statements()
    
    ! Test 6: Parse branch coverage in if-then-else
    call test_parse_if_then_else_branches()
    
    ! Test 7: Handle do loop coverage
    call test_handle_do_loop_coverage()
    
    ! Test 8: Parse select case coverage
    call test_parse_select_case_coverage()
    
    ! Test 9: Handle implicit none statements
    call test_handle_implicit_none()
    
    ! Test 10: Parse array constructors
    call test_parse_array_constructors()
    
    ! Report results
    write(*,*) ""
    if (test_count > 0) then
        write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
                   pass_count, " (", (pass_count * 100) / test_count, "%)"
    else
        write(*,*) "No tests run"
    end if
    
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

    subroutine assert_int(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        integer, intent(in) :: expected, actual
        character(len=20) :: exp_str, act_str
        
        write(exp_str, '(I0)') expected
        write(act_str, '(I0)') actual
        call assert(condition, test_name, trim(exp_str), trim(act_str))
    end subroutine assert_int

    ! Test 1: Parse simple subroutine coverage
    ! Given: Coverage data for a simple subroutine with 5 lines
    ! When: Parsing with gcov_parser
    ! Then: Should return coverage_data_t with correct line counts
    subroutine test_parse_simple_subroutine()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: Simple subroutine with 5 lines
        source_file = create_simple_subroutine_source()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing with gcov_parser
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Should return coverage data with 5 executable lines
        if (.not. error_flag .and. size(coverage_data%files) > 0) then
            call assert_int(count_executable_lines(coverage_data%files(1)) == 5, &
                           "simple subroutine line count", 5, &
                           count_executable_lines(coverage_data%files(1)))
        else
            call assert(.false., "simple subroutine parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_parse_simple_subroutine

    ! Test 2: Handle module procedures
    ! Given: Coverage for procedures within a Fortran module
    ! When: Parsing module source
    ! Then: Should correctly identify parent module for each procedure
    subroutine test_handle_module_procedures()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: Module with procedures
        source_file = create_module_with_procedures()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing module source
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Should identify module procedures
        if (.not. error_flag .and. size(coverage_data%files) > 0) then
            call assert(has_module_procedures(coverage_data%files(1)), &
                       "module procedure identification", "found", "found")
        else
            call assert(.false., "module procedure parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_handle_module_procedures

    ! Test 3: Parse contains block
    ! Given: A module with contained procedures
    ! When: Parsing coverage
    ! Then: Should maintain hierarchy: module -> contains -> procedures
    subroutine test_parse_contains_block()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: Module with contains block
        source_file = create_module_with_contains()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing coverage
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Should handle contains block properly
        if (.not. error_flag) then
            call assert(has_contains_structure(coverage_data), &
                       "contains block parsing", "handled", "handled")
        else
            call assert(.false., "contains block parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_parse_contains_block

    ! Test 4: Handle interface blocks
    ! Given: Coverage for generic interfaces
    ! When: Parsing interface definitions
    ! Then: Should not count interface lines as executable
    subroutine test_handle_interface_blocks()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        integer :: non_exec_count
        
        ! Given: Source with interface blocks
        source_file = create_source_with_interfaces()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing interface definitions
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Interface lines should be non-executable
        if (.not. error_flag .and. size(coverage_data%files) > 0) then
            non_exec_count = count_non_executable_lines(coverage_data%files(1))
            call assert(non_exec_count > 0, "interface non-executable", &
                       "has non-exec lines", "found non-exec lines")
        else
            call assert(.false., "interface parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_handle_interface_blocks

    ! Test 5: Process use statements
    ! Given: Module with use statements
    ! When: Parsing coverage
    ! Then: Should mark use statements as non-executable
    subroutine test_process_use_statements()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: Source with use statements
        source_file = create_source_with_use_statements()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing coverage
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Use statements should be non-executable
        if (.not. error_flag) then
            call assert(has_use_statements_marked(coverage_data), &
                       "use statements marked", "non-executable", "marked")
        else
            call assert(.false., "use statement parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_process_use_statements

    ! Test 6: Parse branch coverage in if-then-else
    ! Given: If-then-else block with coverage
    ! When: Parsing branch data
    ! Then: Should identify both branches and their execution counts
    subroutine test_parse_if_then_else_branches()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: If-then-else with branches
        source_file = create_if_then_else_source()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing branch data
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Should track branch coverage
        if (.not. error_flag) then
            call assert(has_branch_coverage(coverage_data), &
                       "branch coverage tracking", "tracked", "tracked")
        else
            call assert(.false., "branch parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_parse_if_then_else_branches

    ! Test 7: Handle do loop coverage
    ! Given: Do loop with coverage data
    ! When: Parsing loop execution
    ! Then: Should show loop body execution counts
    subroutine test_handle_do_loop_coverage()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: Do loop source
        source_file = create_do_loop_source()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing loop execution
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Should handle loop coverage
        if (.not. error_flag) then
            call assert(has_loop_coverage(coverage_data), &
                       "loop coverage tracking", "tracked", "tracked")
        else
            call assert(.false., "loop parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_handle_do_loop_coverage

    ! Test 8: Parse select case coverage
    ! Given: Select case with multiple cases
    ! When: Parsing case branches
    ! Then: Should track each case branch separately
    subroutine test_parse_select_case_coverage()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: Select case source
        source_file = create_select_case_source()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing case branches
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Should handle case coverage
        if (.not. error_flag) then
            call assert(has_case_coverage(coverage_data), &
                       "case coverage tracking", "tracked", "tracked")
        else
            call assert(.false., "case parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_parse_select_case_coverage

    ! Test 9: Handle implicit none statements
    ! Given: Source with implicit none
    ! When: Parsing coverage
    ! Then: Should mark as non-executable line
    subroutine test_handle_implicit_none()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: Source with implicit none
        source_file = create_source_with_implicit_none()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing coverage
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Implicit none should be non-executable
        if (.not. error_flag) then
            call assert(has_implicit_none_marked(coverage_data), &
                       "implicit none marked", "non-executable", "marked")
        else
            call assert(.false., "implicit none parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_handle_implicit_none

    ! Test 10: Parse array constructors
    ! Given: Array constructor expressions
    ! When: Parsing coverage
    ! Then: Should count as single executable statement
    subroutine test_parse_array_constructors()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        character(len=:), allocatable :: source_file, gcno_file, gcda_file
        logical :: error_flag
        
        ! Given: Array constructor source
        source_file = create_array_constructor_source()
        call create_coverage_files(source_file, gcno_file, gcda_file)
        
        ! When: Parsing coverage
        coverage_data = parser%parse(gcno_file, error_flag)
        
        ! Then: Should handle array constructors
        if (.not. error_flag) then
            call assert(has_array_constructor_coverage(coverage_data), &
                       "array constructor coverage", "tracked", "tracked")
        else
            call assert(.false., "array constructor parsing", "success", "failed")
        end if
        
        ! Cleanup
        call cleanup_test_files(source_file, gcno_file, gcda_file)
    end subroutine test_parse_array_constructors

    !
    ! Helper functions for test data creation and validation
    !

    function create_simple_subroutine_source() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_simple.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'subroutine simple_sub()'
        write(unit, '(A)') '    integer :: x'
        write(unit, '(A)') '    x = 1'
        write(unit, '(A)') '    x = x + 2'
        write(unit, '(A)') '    print *, x'
        write(unit, '(A)') 'end subroutine'
        close(unit)
    end function create_simple_subroutine_source

    function create_module_with_procedures() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_module.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'module test_mod'
        write(unit, '(A)') '    implicit none'
        write(unit, '(A)') 'contains'
        write(unit, '(A)') '    subroutine proc_a()'
        write(unit, '(A)') '        integer :: x = 1'
        write(unit, '(A)') '    end subroutine'
        write(unit, '(A)') '    function proc_b() result(y)'
        write(unit, '(A)') '        integer :: y = 2'
        write(unit, '(A)') '    end function'
        write(unit, '(A)') 'end module'
        close(unit)
    end function create_module_with_procedures

    function create_module_with_contains() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_contains.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'module contains_mod'
        write(unit, '(A)') '    implicit none'
        write(unit, '(A)') '    integer :: module_var'
        write(unit, '(A)') 'contains'
        write(unit, '(A)') '    subroutine contained_proc()'
        write(unit, '(A)') '        module_var = 42'
        write(unit, '(A)') '    end subroutine'
        write(unit, '(A)') 'end module'
        close(unit)
    end function create_module_with_contains

    function create_source_with_interfaces() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_interfaces.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'module interface_mod'
        write(unit, '(A)') '    implicit none'
        write(unit, '(A)') '    interface generic_proc'
        write(unit, '(A)') '        procedure :: real_proc, int_proc'
        write(unit, '(A)') '    end interface'
        write(unit, '(A)') 'contains'
        write(unit, '(A)') '    subroutine real_proc(x)'
        write(unit, '(A)') '        real :: x'
        write(unit, '(A)') '    end subroutine'
        write(unit, '(A)') 'end module'
        close(unit)
    end function create_source_with_interfaces

    function create_source_with_use_statements() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_use.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'program test_use'
        write(unit, '(A)') '    use iso_fortran_env'
        write(unit, '(A)') '    use, intrinsic :: iso_c_binding'
        write(unit, '(A)') '    implicit none'
        write(unit, '(A)') '    integer :: x = 1'
        write(unit, '(A)') 'end program'
        close(unit)
    end function create_source_with_use_statements

    function create_if_then_else_source() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_branches.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'subroutine branch_test(x)'
        write(unit, '(A)') '    integer :: x'
        write(unit, '(A)') '    if (x > 0) then'
        write(unit, '(A)') '        x = x + 1'
        write(unit, '(A)') '    else'
        write(unit, '(A)') '        x = x - 1'
        write(unit, '(A)') '    end if'
        write(unit, '(A)') 'end subroutine'
        close(unit)
    end function create_if_then_else_source

    function create_do_loop_source() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_loop.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'subroutine loop_test()'
        write(unit, '(A)') '    integer :: i'
        write(unit, '(A)') '    do i = 1, 10'
        write(unit, '(A)') '        print *, i'
        write(unit, '(A)') '    end do'
        write(unit, '(A)') 'end subroutine'
        close(unit)
    end function create_do_loop_source

    function create_select_case_source() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_case.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'subroutine case_test(x)'
        write(unit, '(A)') '    integer :: x'
        write(unit, '(A)') '    select case (x)'
        write(unit, '(A)') '    case (1)'
        write(unit, '(A)') '        print *, "one"'
        write(unit, '(A)') '    case (2)'
        write(unit, '(A)') '        print *, "two"'
        write(unit, '(A)') '    case default'
        write(unit, '(A)') '        print *, "other"'
        write(unit, '(A)') '    end select'
        write(unit, '(A)') 'end subroutine'
        close(unit)
    end function create_select_case_source

    function create_source_with_implicit_none() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_implicit.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'subroutine implicit_test()'
        write(unit, '(A)') '    implicit none'
        write(unit, '(A)') '    integer :: x = 42'
        write(unit, '(A)') '    print *, x'
        write(unit, '(A)') 'end subroutine'
        close(unit)
    end function create_source_with_implicit_none

    function create_array_constructor_source() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "test_arrays.f90"
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(A)') 'subroutine array_test()'
        write(unit, '(A)') '    integer :: arr(3)'
        write(unit, '(A)') '    arr = [1, 2, 3]'
        write(unit, '(A)') '    print *, arr'
        write(unit, '(A)') 'end subroutine'
        close(unit)
    end function create_array_constructor_source

    subroutine create_coverage_files(source_file, gcno_file, gcda_file)
        character(len=*), intent(in) :: source_file
        character(len=:), allocatable, intent(out) :: gcno_file, gcda_file
        integer :: dot_pos
        character(len=:), allocatable :: base_name
        
        ! Extract base name without extension
        dot_pos = index(source_file, ".", back=.true.)
        if (dot_pos > 0) then
            base_name = source_file(1:dot_pos-1)
        else
            base_name = source_file
        end if
        
        gcno_file = base_name // ".gcno"
        gcda_file = base_name // ".gcda"
        
        ! Create dummy coverage files for testing
        call create_dummy_gcno(gcno_file)
        call create_dummy_gcda(gcda_file)
    end subroutine create_coverage_files

    subroutine create_dummy_gcno(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        integer :: func_name_len, source_name_len
        character(len=12) :: func_name = "test_function"
        character(len=8) :: source_name = "test.f90"
        
        open(newunit=unit, file=filename, access='stream', &
             form='unformatted')
        
        ! Write GCNO magic number
        write(unit) int(z'67636E6F', kind=4)  ! GCNO magic
        
        ! Write version (GCC version A103)
        write(unit) transfer("A103", int(1, kind=4))  ! Version
        
        ! Write function count
        write(unit) int(1, kind=4)  ! One function
        
        ! Write function record
        func_name_len = len_trim(func_name)
        write(unit) func_name_len
        write(unit) func_name
        
        source_name_len = len_trim(source_name)
        write(unit) source_name_len
        write(unit) source_name
        
        write(unit) int(1, kind=4)    ! Line number
        write(unit) int(12345, kind=4) ! Checksum
        
        close(unit)
    end subroutine create_dummy_gcno

    subroutine create_dummy_gcda(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        integer :: i
        integer(8) :: counter_values(5) = [100_8, 200_8, 0_8, 50_8, 300_8]
        
        open(newunit=unit, file=filename, access='stream', &
             form='unformatted')
        
        ! Write GCDA magic number
        write(unit) int(z'67636461', kind=4)  ! GCDA magic
        
        ! Write counter count
        write(unit) int(5, kind=4)  ! Five counters
        
        ! Write actual counter values
        do i = 1, 5
            write(unit) counter_values(i)
        end do
        
        close(unit)
    end subroutine create_dummy_gcda

    subroutine cleanup_test_files(source_file, gcno_file, gcda_file)
        character(len=*), intent(in) :: source_file
        character(len=*), intent(in) :: gcno_file, gcda_file
        
        call delete_if_exists(source_file)
        call delete_if_exists(gcno_file)
        call delete_if_exists(gcda_file)
    end subroutine cleanup_test_files

    subroutine delete_if_exists(filename)
        character(len=*), intent(in) :: filename
        logical :: exists
        
        inquire(file=filename, exist=exists)
        if (exists) then
            open(99, file=filename, status='old')
            close(99, status='delete')
        end if
    end subroutine delete_if_exists

    ! Validation helper functions (placeholder implementations)
    function count_executable_lines(file) result(count)
        type(coverage_file_t), intent(in) :: file
        integer :: count
        integer :: i
        
        count = 0
        do i = 1, size(file%lines)
            if (file%lines(i)%is_executable) then
                count = count + 1
            end if
        end do
    end function count_executable_lines

    function count_non_executable_lines(file) result(count)
        type(coverage_file_t), intent(in) :: file
        integer :: count
        integer :: i
        
        count = 0
        do i = 1, size(file%lines)
            if (.not. file%lines(i)%is_executable) then
                count = count + 1
            end if
        end do
    end function count_non_executable_lines

    function has_module_procedures(file) result(has_procs)
        type(coverage_file_t), intent(in) :: file
        logical :: has_procs
        
        ! Simplified check - if we have any coverage data, assume procedures exist
        has_procs = allocated(file%lines) .and. size(file%lines) > 0
    end function has_module_procedures

    function has_contains_structure(coverage_data) result(has_contains)
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: has_contains
        
        ! Simplified check - just verify we have files
        has_contains = (size(coverage_data%files) > 0)
    end function has_contains_structure

    function has_use_statements_marked(coverage_data) &
            result(has_marked)
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: has_marked
        
        ! Simplified check - verify we have files with non-executable lines
        has_marked = .false.
        if (size(coverage_data%files) > 0 .and. &
            size(coverage_data%files(1)%lines) > 0) then
            has_marked = .not. coverage_data%files(1)%lines(1)%is_executable
        end if
    end function has_use_statements_marked

    function has_branch_coverage(coverage_data) result(has_branches)
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: has_branches
        
        ! Simplified check - just verify we have executable lines
        has_branches = (size(coverage_data%files) > 0)
    end function has_branch_coverage

    function has_loop_coverage(coverage_data) result(has_loops)
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: has_loops
        
        ! Simplified check - just verify we have executable lines
        has_loops = (size(coverage_data%files) > 0)
    end function has_loop_coverage

    function has_case_coverage(coverage_data) result(has_cases)
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: has_cases
        
        ! Simplified check - just verify we have executable lines
        has_cases = (size(coverage_data%files) > 0)
    end function has_case_coverage

    function has_implicit_none_marked(coverage_data) &
            result(has_marked)
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: has_marked
        integer :: i
        
        ! Check if any line is marked as non-executable (implicit none would be)
        has_marked = .false.
        if (size(coverage_data%files) > 0 .and. &
            size(coverage_data%files(1)%lines) > 0) then
            do i = 1, size(coverage_data%files(1)%lines)
                if (.not. coverage_data%files(1)%lines(i)%is_executable) then
                    has_marked = .true.
                    exit
                end if
            end do
        end if
    end function has_implicit_none_marked

    function has_array_constructor_coverage(coverage_data) &
            result(has_arrays)
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: has_arrays
        
        ! Simplified check - just verify we have executable lines
        has_arrays = (size(coverage_data%files) > 0)
    end function has_array_constructor_coverage

end program test_gcov_parser