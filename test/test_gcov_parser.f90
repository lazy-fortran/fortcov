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
        
        ! Should NOT support .gcda files (binary parsing removed)
        call assert(.not. parser%can_parse("test.gcda"), "cannot parse gcda", "false", &
                   merge("false", "true ", .not. parser%can_parse("test.gcda")))
        
        ! Should NOT support .gcno files (binary parsing removed)
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


end program test_gcov_parser