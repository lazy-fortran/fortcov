program test_infrastructure_stability_validation
    !! Test Infrastructure Stability Validation (Issue #509)
    !!
    !! Validates Sprint 2 success criteria #4: "Test infrastructure stable
    !! (all tests pass)"
    !! This test checks that the testing infrastructure is reliable and can be used
    !! to validate all other functionality without introducing instability.
    !!
    !! This main program coordinates modular infrastructure tests to meet
    !! size requirements.
    
    use iso_fortran_env, only: output_unit
    use test_infrastructure_core_validation, only: run_core_infrastructure_tests
    use test_infrastructure_extended_validation, only: run_extended_infrastructure_tests
    implicit none
    
    integer :: test_count = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.
    
    write(output_unit, '(A)') "======================================================="
    write(output_unit, '(A)') "      Test Infrastructure Stability Validation        "
    write(output_unit, '(A)') "======================================================="
    write(output_unit, '(A)') ""
    
    ! Run core infrastructure tests
    call run_core_infrastructure_tests(test_count, passed_tests, all_tests_passed)
    
    ! Run extended infrastructure tests  
    call run_extended_infrastructure_tests(test_count, passed_tests, all_tests_passed)
    
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "======================================================="
    write(*, '(A,I0,A,I0,A)') "INFRASTRUCTURE STABILITY: ", passed_tests, "/", &
                              test_count, " tests passed"
    
    if (all_tests_passed) then
        write(output_unit, '(A)') "✅ TEST INFRASTRUCTURE FULLY STABLE"
        write(output_unit, '(A)') "   All tests can rely on stable foundation"
        call exit(0)
    else
        write(output_unit, '(A)') "❌ INFRASTRUCTURE INSTABILITY DETECTED"
        write(output_unit, '(A)') "   Test infrastructure needs fixes before validation"
        call exit(1)
    end if

end program test_infrastructure_stability_validation