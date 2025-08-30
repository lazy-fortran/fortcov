program test_epic_1_cli_fixes
    !! Test program for EPIC 1 CLI parsing fixes
    !! Tests for Issues #938, #939, #940
    
    use config_core, only: parse_config
    use config_types, only: config_t
    implicit none
    
    character(len=256) :: error_message
    type(config_t) :: config
    logical :: success
    integer :: test_count = 0, passed_count = 0
    
    print *, "========================================"
    print *, "EPIC 1: CLI Parsing Reliability Tests"
    print *, "========================================"
    print *, ""
    
    ! Test Issue #938: --threads flag parsing
    call test_threads_flag()
    
    ! Test Issue #939: --architecture-format equals syntax
    call test_architecture_format_equals()
    
    ! Test Issue #940: --verbose flag warnings
    call test_verbose_flag_warnings()
    
    ! Final summary
    print *, "========================================"
    print '(A,I0,A,I0,A)', "EPIC 1 Summary: ", passed_count, "/", test_count, " tests passed"
    if (passed_count == test_count) then
        print *, "✅ ALL CLI PARSING FIXES WORKING"
    else
        print *, "❌ SOME CLI PARSING ISSUES REMAIN"
        call exit(1)
    end if
    print *, "========================================"

contains

    subroutine test_threads_flag()
        print *, "=== Testing Issue #938: --threads flag ==="
        
        ! Test --threads with space syntax
        call run_test("--threads 4", test_threads_space_syntax)
        
        ! Test -t short form with space syntax  
        call run_test("-t 8", test_threads_short_form)
        
        ! Test --threads with equals syntax
        call run_test("--threads=12", test_threads_equals_syntax)
        
        ! Test -t with equals syntax
        call run_test("-t=16", test_threads_short_equals_syntax)
        
        print *, ""
    end subroutine test_threads_flag
    
    subroutine test_architecture_format_equals()
        print *, "=== Testing Issue #939: --architecture-format equals ==="
        
        ! Test --architecture-format with space syntax
        call run_test("--architecture-format json", test_arch_format_space)
        
        ! Test --architecture-format with equals syntax
        call run_test("--architecture-format=json", test_arch_format_equals)
        
        print *, ""
    end subroutine test_architecture_format_equals
    
    subroutine test_verbose_flag_warnings()
        print *, "=== Testing Issue #940: --verbose flag warnings ==="
        
        ! Test --verbose flag
        call run_test("--verbose --help", test_verbose_flag)
        
        ! Test -v short form
        call run_test("-v --help", test_verbose_short_form)
        
        print *, ""
    end subroutine test_verbose_flag_warnings
    
    subroutine run_test(args_str, test_proc)
        character(len=*), intent(in) :: args_str
        interface
            subroutine test_proc(args_array, config, success, error_message)
                import :: config_t
                character(len=*), intent(in) :: args_array(:)
                type(config_t), intent(out) :: config
                logical, intent(out) :: success
                character(len=*), intent(out) :: error_message
            end subroutine test_proc
        end interface
        
        character(len=256), allocatable :: args(:)
        integer :: num_args, i, start_pos, end_pos
        
        test_count = test_count + 1
        
        ! Parse args_str into args array (simple space-based splitting)
        num_args = count_words(args_str)
        allocate(character(len=256) :: args(num_args))
        
        start_pos = 1
        do i = 1, num_args
            end_pos = index(args_str(start_pos:), " ")
            if (end_pos == 0) end_pos = len_trim(args_str(start_pos:)) + 1
            end_pos = start_pos + end_pos - 2
            args(i) = trim(args_str(start_pos:end_pos))
            start_pos = end_pos + 2
        end do
        
        call test_proc(args, config, success, error_message)
        
        if (success) then
            passed_count = passed_count + 1
            print '(A,A,A)', "  ✅ PASS: ", trim(args_str)
        else
            print '(A,A,A)', "  ❌ FAIL: ", trim(args_str)
            print '(A,A)', "     Error: ", trim(error_message)
        end if
        
        deallocate(args)
    end subroutine run_test
    
    function count_words(str) result(count)
        character(len=*), intent(in) :: str
        integer :: count, i
        logical :: in_word
        
        count = 0
        in_word = .false.
        
        do i = 1, len_trim(str)
            if (str(i:i) /= ' ') then
                if (.not. in_word) then
                    count = count + 1
                    in_word = .true.
                end if
            else
                in_word = .false.
            end if
        end do
    end function count_words
    
    ! Test procedure implementations
    subroutine test_threads_space_syntax(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config(args, config, success, error_message)
        if (success) then
            if (config%threads /= 4) then
                success = .false.
                error_message = "Expected threads=4, got threads=" // trim(int_to_str(config%threads))
            end if
        end if
    end subroutine test_threads_space_syntax
    
    subroutine test_threads_short_form(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config(args, config, success, error_message)
        if (success) then
            if (config%threads /= 8) then
                success = .false.
                error_message = "Expected threads=8, got threads=" // trim(int_to_str(config%threads))
            end if
        end if
    end subroutine test_threads_short_form
    
    subroutine test_threads_equals_syntax(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config(args, config, success, error_message)
        if (success) then
            if (config%threads /= 12) then
                success = .false.
                error_message = "Expected threads=12, got threads=" // trim(int_to_str(config%threads))
            end if
        end if
    end subroutine test_threads_equals_syntax
    
    subroutine test_threads_short_equals_syntax(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config(args, config, success, error_message)
        if (success) then
            if (config%threads /= 16) then
                success = .false.
                error_message = "Expected threads=16, got threads=" // trim(int_to_str(config%threads))
            end if
        end if
    end subroutine test_threads_short_equals_syntax
    
    subroutine test_arch_format_space(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config(args, config, success, error_message)
        if (success) then
            if (trim(config%architecture_output_format) /= "json") then
                success = .false.
                error_message = "Expected architecture_output_format=json, got " // &
                    trim(config%architecture_output_format)
            end if
        end if
    end subroutine test_arch_format_space
    
    subroutine test_arch_format_equals(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config(args, config, success, error_message)
        if (success) then
            if (trim(config%architecture_output_format) /= "json") then
                success = .false.
                error_message = "Expected architecture_output_format=json, got " // &
                    trim(config%architecture_output_format)
            end if
        end if
    end subroutine test_arch_format_equals
    
    subroutine test_verbose_flag(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config(args, config, success, error_message)
        if (success) then
            if (.not. config%verbose) then
                success = .false.
                error_message = "Expected verbose=.true., got verbose=.false."
            end if
        end if
    end subroutine test_verbose_flag
    
    subroutine test_verbose_short_form(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_config(args, config, success, error_message)
        if (success) then
            if (.not. config%verbose) then
                success = .false.
                error_message = "Expected verbose=.true., got verbose=.false."
            end if
        end if
    end subroutine test_verbose_short_form
    
    function int_to_str(val) result(str)
        integer, intent(in) :: val
        character(len=16) :: str
        write(str, '(I0)') val
    end function int_to_str

end program test_epic_1_cli_fixes