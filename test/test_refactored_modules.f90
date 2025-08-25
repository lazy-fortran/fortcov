program test_refactored_modules
    !! Simple Test for Phase 1 Architecture Refactoring
    !!
    !! Tests the basic functionality of extracted modules.
    use report_configuration
    use coverage_metrics_calculator
    use source_location_types
    use coverage_line_types
    use platform_detection
    implicit none

    integer :: tests_run = 0, tests_passed = 0

    print *, 'Testing Phase 1 Architecture Refactoring'
    print *, '========================================'

    call test_report_configuration()
    call test_metrics_calculator()
    call test_source_location_types()
    call test_coverage_line_types()
    call test_platform_detection_module()

    print *, ''
    if (tests_passed == tests_run) then
        print '(A, I0, A)', 'SUCCESS: All ', tests_run, ' tests passed!'
        stop 0
    else
        print '(A, I0, A, I0, A)', 'FAILED: ', tests_run - tests_passed, &
              ' out of ', tests_run, ' tests failed!'
        stop 1
    end if

contains

    subroutine test_report_configuration()
        type(report_config_t) :: config
        type(terminal_session_t) :: session
        type(filter_criteria_t) :: filter

        call run_simple_test('report_config_t initialization')
        call config%init()
        if (allocated(config%output_format) .and. &
            config%enable_syntax_highlighting) then
            call pass_test()
        else
            call fail_test()
        end if

        call run_simple_test('terminal_session_t initialization')
        call session%init()
        if (session%terminal_width == 80 .and. &
            session%terminal_height == 24) then
            call pass_test()
        else
            call fail_test()
        end if

        call run_simple_test('filter_criteria_t initialization')
        call filter%init()
        if (allocated(filter%include_patterns) .and. &
            allocated(filter%exclude_patterns)) then
            call pass_test()
        else
            call fail_test()
        end if
    end subroutine test_report_configuration

    subroutine test_metrics_calculator()
        type(coverage_metrics_t) :: metrics

        call run_simple_test('coverage_metrics_t initialization')
        call metrics%init()
        if (metrics%total_lines == 0 .and. &
            metrics%covered_lines == 0) then
            call pass_test()
        else
            call fail_test()
        end if
    end subroutine test_metrics_calculator

    subroutine test_source_location_types()
        type(source_location_t) :: location

        call run_simple_test('source_location_t initialization')
        call location%init('test.f90', 42, 1, 10)
        if (allocated(location%filename) .and. &
            location%filename == 'test.f90' .and. &
            location%line_number == 42) then
            call pass_test()
        else
            call fail_test()
        end if
    end subroutine test_source_location_types

    subroutine test_coverage_line_types()
        type(coverage_line_t) :: line
        type(line_coverage_t) :: simple_line

        call run_simple_test('coverage_line_t initialization')
        call line%init('test.f90', 42, .true., 5)
        if (allocated(line%filename) .and. &
            line%line_number == 42 .and. &
            line%is_covered()) then
            call pass_test()
        else
            call fail_test()
        end if

        call run_simple_test('line_coverage_t initialization')
        call simple_line%init(42, .true., 5)
        if (simple_line%line_number == 42 .and. &
            simple_line%is_covered) then
            call pass_test()
        else
            call fail_test()
        end if
    end subroutine test_coverage_line_types

    subroutine test_platform_detection_module()
        logical :: unix_result

        call run_simple_test('platform detection functionality')
        unix_result = is_unix()
        ! Test passes if function runs without error
        call pass_test()
    end subroutine test_platform_detection_module

    subroutine run_simple_test(test_name)
        character(len=*), intent(in) :: test_name
        tests_run = tests_run + 1
        print '(A, A, A)', 'Running: ', test_name, '...'
    end subroutine run_simple_test

    subroutine pass_test()
        tests_passed = tests_passed + 1
        print '(A)', '  PASS'
    end subroutine pass_test

    subroutine fail_test()
        print '(A)', '  FAIL'
    end subroutine fail_test

end program test_refactored_modules