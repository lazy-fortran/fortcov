program test_cli_integration
    !! CLI Integration Test Module
    !!
    !! Tests complex CLI scenarios including flag ordering
    !! independence and integration workflows.
    !!
    !! Extracted from test_cli_consistency_validation.f90 for 
    !! SRP compliance and improved maintainability.

    use iso_fortran_env, only: output_unit, error_unit
    use config_core, only: config_t, parse_config
    use test_utils_core, only: assert_test, reset_test_counters, &
                          print_test_header, print_test_summary
    implicit none

    call reset_test_counters()
    call print_test_header("CLI Integration")

    ! Test integration scenarios
    call test_flag_ordering_independence()

    call print_test_summary("CLI INTEGRATION")

contains


    subroutine test_flag_ordering_independence()
        !! Tests that flag ordering doesn't matter
        
        type(config_t) :: config1, config2
        character(len=64), allocatable :: args1(:), args2(:)
        logical :: success1, success2
        character(len=256) :: error_message1, error_message2
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== FLAG ORDERING INDEPENDENCE ==="
        
        ! Test order 1: source, output, verbose
        allocate(character(len=64) :: args1(3))
        args1(1) = "--source=src"
        args1(2) = "--output=test.md"
        args1(3) = "--verbose"
        
        call parse_config(args1, config1, success1, error_message1)
        
        ! Test order 2: verbose, output, source
        allocate(character(len=64) :: args2(3))
        args2(1) = "--verbose"
        args2(2) = "--output=test.md"
        args2(3) = "--source=src"
        
        call parse_config(args2, config2, success2, error_message2)
        
        call assert_test(success1 .and. success2, "Both orderings parse", &
                        "Both should succeed")
        
        if (success1 .and. success2) then
            call assert_test(config1%verbose .eqv. config2%verbose .and. &
                           size(config1%source_paths) > 0 .and. &
                           size(config2%source_paths) > 0 .and. &
                           trim(config1%source_paths(1)) == &
                           trim(config2%source_paths(1)) .and. &
                           trim(config1%output_path) == trim(config2%output_path), &
                           "Flag ordering independence", &
                           "Results should be identical regardless of order")
        end if
        
    end subroutine test_flag_ordering_independence

end program test_cli_integration