program test_gcov_parser
    use coverage_model
    use coverage_parser
    use file_utils
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running gcov_parser tests..."
    
    ! Test 1: Verify parser factory for gcov files
    call test_parser_factory_gcov()
    
    ! Test 2: Verify gcov parser can_parse method
    call test_gcov_can_parse()
    
    ! Test 3: Verify gcov parser required files
    call test_gcov_required_files()
    
    ! Test 4: Verify gcov parser parse method returns error (not implemented)
    call test_gcov_parse_not_implemented()
    
    ! NEW COMPREHENSIVE TESTS FOR ISSUE #23 - TDD RED PHASE
    ! Test 5: Parse basic gcov line format
    call test_parse_basic_gcov_line()
    
    ! Test 6: Handle non-executable lines
    call test_parse_non_executable_lines()
    
    ! Test 7: Handle unexecuted lines
    call test_parse_unexecuted_lines()
    
    ! Test 8: Parse header information
    call test_parse_header_information()
    
    ! Test 9: Parse complete gcov file
    call test_parse_complete_gcov_file()
    
    ! Test 10: Handle malformed gcov data
    call test_parse_malformed_gcov_data()
    
    ! Test 11: Parse module procedures
    call test_parse_module_procedures()
    
    ! Test 12: Handle function boundaries
    call test_parse_function_boundaries()
    
    ! Test 13: Parse multiple source files
    call test_parse_multiple_source_files()
    
    ! Test 14: Handle missing coverage data
    call test_parse_missing_coverage_data()
    
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

    ! Test 1: Verify parser factory for gcov files
    ! Given: A .gcov file path
    ! When: Creating parser from factory
    ! Then: Should create gcov_parser_t instance
    subroutine test_parser_factory_gcov()
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        
        ! When: Creating parser for .gcov file
        call create_parser("test.gcov", parser, error_flag)
        
        ! Then: Should succeed and create gcov parser
        call assert(.not. error_flag, "gcov parser factory", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        ! Verify it's the right type
        select type(parser)
        type is (gcov_parser_t)
            call assert(.true., "gcov parser type", "gcov_parser_t", "gcov_parser_t")
        class default
            call assert(.false., "gcov parser type", "gcov_parser_t", "wrong type")
        end select
    end subroutine test_parser_factory_gcov

    ! Test 2: Verify gcov parser can_parse method
    ! Given: Various file extensions
    ! When: Checking if parser can parse them
    ! Then: Should only support .gcov files
    subroutine test_gcov_can_parse()
        type(gcov_parser_t) :: parser
        
        ! Should support .gcov files
        call assert(parser%can_parse("test.gcov"), "can parse gcov", "true", "true")
        
        ! Should NOT support .gcda files (binary format not supported)
        call assert(.not. parser%can_parse("test.gcda"), "cannot parse gcda", "false", &
                   merge("false", "true ", .not. parser%can_parse("test.gcda")))
        
        ! Should NOT support .gcno files (binary format not supported)
        call assert(.not. parser%can_parse("test.gcno"), "cannot parse gcno", "false", &
                   merge("false", "true ", .not. parser%can_parse("test.gcno")))
        
        ! Should NOT support other extensions
        call assert(.not. parser%can_parse("test.txt"), "cannot parse txt", "false", &
                   merge("false", "true ", .not. parser%can_parse("test.txt")))
    end subroutine test_gcov_can_parse

    ! Test 3: Verify gcov parser required files
    ! Given: A gcov parser instance
    ! When: Getting required file extensions
    ! Then: Should return .gcov extension
    subroutine test_gcov_required_files()
        type(gcov_parser_t) :: parser
        character(len=:), allocatable :: extensions(:)
        
        ! When: Getting required extensions
        extensions = parser%get_required_files()
        
        ! Then: Should return .gcov extension
        call assert(size(extensions) == 1, "required files count", "1", &
                   merge("1       ", "multiple", size(extensions) == 1))
        
        if (size(extensions) >= 1) then
            call assert(trim(extensions(1)) == ".gcov", "required extension", ".gcov", &
                       trim(extensions(1)))
        end if
    end subroutine test_gcov_required_files

    ! Test 4: Verify gcov parser parse method returns error (not implemented)
    ! Given: A gcov parser and test file
    ! When: Calling parse method
    ! Then: Should return error flag (parsing not implemented)
    subroutine test_gcov_parse_not_implemented()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! When: Calling parse method
        coverage_data = parser%parse("test.gcov", error_flag)
        
        ! Then: Should return error (not implemented)
        call assert(error_flag, "parse not implemented", "error", &
                   merge("error  ", "success", error_flag))
    end subroutine test_gcov_parse_not_implemented
    
    ! COMPREHENSIVE TDD TESTS FOR ISSUE #23 - RED PHASE
    
    ! Test 5: Parse basic gcov line format
    ! Given: Gcov line "    2:    5:    x = y + 1"
    ! When: Parsing line
    ! Then: Should extract execution_count=2, line_number=5, content="    x = y + 1"
    subroutine test_parse_basic_gcov_line()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create test gcov file with basic line format
        call create_test_gcov_basic_line()
        
        ! When: Parsing basic gcov file
        coverage_data = parser%parse("test_basic.gcov", error_flag)
        
        ! Then: Should parse successfully
        call assert(.not. error_flag, "basic line parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        ! Should have one file with coverage data
        call assert_int(size(coverage_data%files) > 0, "has coverage files", 1, &
                       size(coverage_data%files))
        
        ! Should extract line with execution count 2
        if (size(coverage_data%files) > 0) then
            call assert_int(size(coverage_data%files(1)%lines) > 0, "has lines", 1, &
                           size(coverage_data%files(1)%lines))
            
            if (size(coverage_data%files(1)%lines) > 0) then
                call assert_int(coverage_data%files(1)%lines(1)%execution_count == 2, &
                               "execution count", 2, &
                               coverage_data%files(1)%lines(1)%execution_count)
                call assert_int(coverage_data%files(1)%lines(1)%line_number == 5, &
                               "line number", 5, &
                               coverage_data%files(1)%lines(1)%line_number)
            end if
        end if
        
        ! Clean up test file
        call cleanup_test_files()
    end subroutine test_parse_basic_gcov_line
    
    ! Test 6: Handle non-executable lines
    ! Given: Gcov line "    -:    1:! This is a comment"
    ! When: Parsing line
    ! Then: Should mark as non-executable with execution_count=-1
    subroutine test_parse_non_executable_lines()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create test gcov file with non-executable lines
        call create_test_gcov_non_executable()
        
        ! When: Parsing non-executable gcov file
        coverage_data = parser%parse("test_non_exec.gcov", error_flag)
        
        ! Then: Should parse successfully
        call assert(.not. error_flag, "non-executable parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        ! Should mark non-executable lines appropriately
        if (size(coverage_data%files) > 0 .and. &
           size(coverage_data%files(1)%lines) > 0) then
            call assert(.not. coverage_data%files(1)%lines(1)%is_executable, &
                       "non-executable line", "false", &
                       merge("false", "true ", .not. coverage_data%files(1)%lines(1)%is_executable))
        end if
        
        call cleanup_test_files()
    end subroutine test_parse_non_executable_lines
    
    ! Test 7: Handle unexecuted lines
    ! Given: Gcov line "#####:   10:    never_reached = 1"
    ! When: Parsing line
    ! Then: Should mark as executable=true, execution_count=0
    subroutine test_parse_unexecuted_lines()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create test gcov file with unexecuted lines
        call create_test_gcov_unexecuted()
        
        ! When: Parsing unexecuted gcov file
        coverage_data = parser%parse("test_unexec.gcov", error_flag)
        
        ! Then: Should parse successfully
        call assert(.not. error_flag, "unexecuted parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        ! Should mark unexecuted lines as executable but not executed
        if (size(coverage_data%files) > 0 .and. &
           size(coverage_data%files(1)%lines) > 0) then
            call assert(coverage_data%files(1)%lines(1)%is_executable, &
                       "unexecuted line executable", "true", "true")
            call assert_int(coverage_data%files(1)%lines(1)%execution_count == 0, &
                           "unexecuted count", 0, &
                           coverage_data%files(1)%lines(1)%execution_count)
        end if
        
        call cleanup_test_files()
    end subroutine test_parse_unexecuted_lines
    
    ! Test 8: Parse header information
    ! Given: Gcov header lines (Source:, Graph:, Data:)
    ! When: Parsing file header
    ! Then: Should extract source filename and coverage data paths
    subroutine test_parse_header_information()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create test gcov file with headers
        call create_test_gcov_headers()
        
        ! When: Parsing gcov file with headers
        coverage_data = parser%parse("test_headers.gcov", error_flag)
        
        ! Then: Should parse successfully and extract source filename
        call assert(.not. error_flag, "header parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        ! Should extract source filename from header
        if (size(coverage_data%files) > 0) then
            call assert(index(coverage_data%files(1)%filename, "test.f90") > 0, &
                       "source filename extracted", "contains test.f90", &
                       coverage_data%files(1)%filename)
        end if
        
        call cleanup_test_files()
    end subroutine test_parse_header_information
    
    ! Test 9: Parse complete gcov file
    ! Given: Complete gcov file with headers and coverage data
    ! When: Parsing entire file
    ! Then: Should build complete coverage_data_t structure
    subroutine test_parse_complete_gcov_file()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create complete test gcov file
        call create_test_gcov_complete()
        
        ! When: Parsing complete gcov file
        coverage_data = parser%parse("test_complete.gcov", error_flag)
        
        ! Then: Should parse successfully
        call assert(.not. error_flag, "complete file parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        ! Should have proper structure with files and lines
        call assert_int(size(coverage_data%files) >= 1, "has files", 1, &
                       size(coverage_data%files))
        
        if (size(coverage_data%files) > 0) then
            call assert_int(size(coverage_data%files(1)%lines) >= 3, "has lines", 3, &
                           size(coverage_data%files(1)%lines))
        end if
        
        call cleanup_test_files()
    end subroutine test_parse_complete_gcov_file
    
    ! Test 10: Handle malformed gcov data
    ! Given: Malformed gcov file with invalid format
    ! When: Parsing malformed file
    ! Then: Should return error_flag=true
    subroutine test_parse_malformed_gcov_data()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create malformed test gcov file
        call create_test_gcov_malformed()
        
        ! When: Parsing malformed gcov file
        coverage_data = parser%parse("test_malformed.gcov", error_flag)
        
        ! Then: Should return error for malformed data
        call assert(error_flag, "malformed parse error", "error", &
                   merge("error  ", "success", error_flag))
        
        call cleanup_test_files()
    end subroutine test_parse_malformed_gcov_data
    
    ! Test 11: Parse module procedures
    ! Given: Gcov output with Fortran module structure
    ! When: Parsing module source
    ! Then: Should correctly identify parent module for each procedure
    subroutine test_parse_module_procedures()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create test gcov file with module procedures
        call create_test_gcov_module_procedures()
        
        ! When: Parsing module gcov file
        coverage_data = parser%parse("test_module.gcov", error_flag)
        
        ! Then: Should parse successfully
        call assert(.not. error_flag, "module parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        ! Should handle module structure appropriately
        call assert_int(size(coverage_data%files) >= 1, "has module file", 1, &
                       size(coverage_data%files))
        
        call cleanup_test_files()
    end subroutine test_parse_module_procedures
    
    ! Test 12: Handle function boundaries
    ! Given: Gcov output with function separation (------ lines)
    ! When: Parsing function boundaries
    ! Then: Should track function names and execution counts
    subroutine test_parse_function_boundaries()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create test gcov file with function boundaries
        call create_test_gcov_function_boundaries()
        
        ! When: Parsing function boundaries gcov file
        coverage_data = parser%parse("test_functions.gcov", error_flag)
        
        ! Then: Should parse successfully
        call assert(.not. error_flag, "function boundaries parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        call cleanup_test_files()
    end subroutine test_parse_function_boundaries
    
    ! Test 13: Parse multiple source files
    ! Given: Gcov output covering multiple source files
    ! When: Parsing multi-file output
    ! Then: Should separate coverage by source file correctly
    subroutine test_parse_multiple_source_files()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create test gcov file with multiple sources
        call create_test_gcov_multiple_sources()
        
        ! When: Parsing multi-source gcov file
        coverage_data = parser%parse("test_multi.gcov", error_flag)
        
        ! Then: Should parse successfully
        call assert(.not. error_flag, "multi-source parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        ! Should handle multiple source files
        call assert_int(size(coverage_data%files) >= 2, "has multiple files", 2, &
                       size(coverage_data%files))
        
        call cleanup_test_files()
    end subroutine test_parse_multiple_source_files
    
    ! Test 14: Handle missing coverage data
    ! Given: Gcov file with no Data: line (no .gcda)
    ! When: Parsing coverage
    ! Then: Should mark all lines as execution_count=0
    subroutine test_parse_missing_coverage_data()
        type(gcov_parser_t) :: parser
        type(coverage_data_t) :: coverage_data
        logical :: error_flag
        
        ! Create test gcov file without coverage data
        call create_test_gcov_missing_data()
        
        ! When: Parsing gcov file without coverage data
        coverage_data = parser%parse("test_missing.gcov", error_flag)
        
        ! Then: Should parse successfully but with zero execution counts
        call assert(.not. error_flag, "missing data parse success", "no error", &
                   merge("no error", "error   ", .not. error_flag))
        
        call cleanup_test_files()
    end subroutine test_parse_missing_coverage_data
    
    ! HELPER SUBROUTINES FOR TEST DATA CREATION
    
    subroutine create_test_gcov_basic_line()
        integer, parameter :: unit = 10
        open(unit, file="test_basic.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:test.f90"
        write(unit, '(A)') "        -:    0:Graph:test.gcno"
        write(unit, '(A)') "        -:    0:Data:test.gcda"
        write(unit, '(A)') "        2:    5:    x = y + 1"
        close(unit)
    end subroutine create_test_gcov_basic_line
    
    subroutine create_test_gcov_non_executable()
        integer, parameter :: unit = 10
        open(unit, file="test_non_exec.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:test.f90"
        write(unit, '(A)') "        -:    0:Graph:test.gcno"
        write(unit, '(A)') "        -:    0:Data:test.gcda"
        write(unit, '(A)') "        -:    1:! This is a comment"
        write(unit, '(A)') "        -:    2:module test_module"
        close(unit)
    end subroutine create_test_gcov_non_executable
    
    subroutine create_test_gcov_unexecuted()
        integer, parameter :: unit = 10
        open(unit, file="test_unexec.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:test.f90"
        write(unit, '(A)') "        -:    0:Graph:test.gcno"
        write(unit, '(A)') "        -:    0:Data:test.gcda"
        write(unit, '(A)') "    #####:   10:    never_reached = 1"
        close(unit)
    end subroutine create_test_gcov_unexecuted
    
    subroutine create_test_gcov_headers()
        integer, parameter :: unit = 10
        open(unit, file="test_headers.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:test.f90"
        write(unit, '(A)') "        -:    0:Graph:test.gcno"
        write(unit, '(A)') "        -:    0:Data:test.gcda"
        write(unit, '(A)') "        1:    1:program test"
        close(unit)
    end subroutine create_test_gcov_headers
    
    subroutine create_test_gcov_complete()
        integer, parameter :: unit = 10
        open(unit, file="test_complete.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:test.f90"
        write(unit, '(A)') "        -:    0:Graph:test.gcno"
        write(unit, '(A)') "        -:    0:Data:test.gcda"
        write(unit, '(A)') "        1:    1:program test"
        write(unit, '(A)') "        2:    2:    print *, 'hello'"
        write(unit, '(A)') "    #####:    3:    if (.false.) print *, 'never'"
        write(unit, '(A)') "        -:    4:end program"
        close(unit)
    end subroutine create_test_gcov_complete
    
    subroutine create_test_gcov_malformed()
        integer, parameter :: unit = 10
        open(unit, file="test_malformed.gcov", status="replace")
        write(unit, '(A)') "INVALID:HEADER:FORMAT"
        write(unit, '(A)') "not-a-number:invalid:line"
        close(unit)
    end subroutine create_test_gcov_malformed
    
    subroutine create_test_gcov_module_procedures()
        integer, parameter :: unit = 10
        open(unit, file="test_module.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:module.f90"
        write(unit, '(A)') "        -:    0:Graph:module.gcno"
        write(unit, '(A)') "        -:    0:Data:module.gcda"
        write(unit, '(A)') "        -:    1:module test_module"
        write(unit, '(A)') "        1:    2:contains"
        write(unit, '(A)') "        2:    3:    subroutine test_sub()"
        write(unit, '(A)') "        2:    4:        print *, 'test'"
        write(unit, '(A)') "        2:    5:    end subroutine"
        write(unit, '(A)') "        -:    6:end module"
        close(unit)
    end subroutine create_test_gcov_module_procedures
    
    subroutine create_test_gcov_function_boundaries()
        integer, parameter :: unit = 10
        open(unit, file="test_functions.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:test.f90"
        write(unit, '(A)') "        -:    0:Graph:test.gcno"
        write(unit, '(A)') "        -:    0:Data:test.gcda"
        write(unit, '(A)') "function test_function called 2 returned 100%"
        write(unit, '(A)') "        2:    1:function test_function() result(res)"
        write(unit, '(A)') "        2:    2:    res = 42"
        write(unit, '(A)') "        2:    3:end function"
        close(unit)
    end subroutine create_test_gcov_function_boundaries
    
    subroutine create_test_gcov_multiple_sources()
        integer, parameter :: unit = 10
        open(unit, file="test_multi.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:test1.f90"
        write(unit, '(A)') "        -:    0:Graph:test1.gcno"
        write(unit, '(A)') "        -:    0:Data:test1.gcda"
        write(unit, '(A)') "        1:    1:program test1"
        write(unit, '(A)') "        -:    0:Source:test2.f90"
        write(unit, '(A)') "        -:    0:Graph:test2.gcno"
        write(unit, '(A)') "        -:    0:Data:test2.gcda"
        write(unit, '(A)') "        2:    1:program test2"
        close(unit)
    end subroutine create_test_gcov_multiple_sources
    
    subroutine create_test_gcov_missing_data()
        integer, parameter :: unit = 10
        open(unit, file="test_missing.gcov", status="replace")
        write(unit, '(A)') "        -:    0:Source:test.f90"
        write(unit, '(A)') "        -:    0:Graph:test.gcno"
        write(unit, '(A)') "        -:    1:program test"
        write(unit, '(A)') "        -:    2:end program"
        close(unit)
    end subroutine create_test_gcov_missing_data
    
    subroutine cleanup_test_files()
        integer, parameter :: files_to_clean = 10
        character(len=50), parameter :: test_files(files_to_clean) = [ &
            "test_basic.gcov      ", "test_non_exec.gcov   ", "test_unexec.gcov     ", &
            "test_headers.gcov    ", "test_complete.gcov   ", "test_malformed.gcov  ", &
            "test_module.gcov     ", "test_functions.gcov  ", "test_multi.gcov      ", &
            "test_missing.gcov    " ]
        integer :: i, iostat_val
        
        do i = 1, files_to_clean
            open(99, file=trim(test_files(i)), status="old", iostat=iostat_val)
            if (iostat_val == 0) then
                close(99, status="delete")
            end if
        end do
    end subroutine cleanup_test_files


end program test_gcov_parser