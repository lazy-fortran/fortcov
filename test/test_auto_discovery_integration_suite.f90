program test_auto_discovery_integration_suite
    !! Auto-Discovery Integration Test Suite
    !!
    !! This program coordinates the complete auto-discovery validation by
    !! running all decomposed test modules in sequence and providing
    !! comprehensive reporting across the entire test suite.
    !!
    !! Test Modules Coordinated:
    !! - Core validation tests (build detection, workflows, integration)
    !! - Project scenario tests (FPM, CMake, Make scenarios)
    !! - Error handling tests (missing systems, failures, corrupted data)
    !! - Shared utilities (workspace management, project creation)
    !! 
    !! This integration approach ensures full coverage while maintaining
    !! the SRP-driven modular decomposition for maintainability.
    
    use iso_fortran_env, only: output_unit
    use test_auto_discovery_shared_utilities
    implicit none
    
    integer :: total_modules = 3
    integer :: passed_modules = 0
    logical :: all_modules_passed = .true.
    
    write(output_unit, '(A)') "============================================="
    write(output_unit, '(A)') "  Auto-Discovery Integration Test Suite   "
    write(output_unit, '(A)') "============================================="
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "Running comprehensive auto-discovery validation..."
    write(output_unit, '(A)') ""
    
    ! Execute all test modules in sequence
    call run_test_module("Core Validation", "test_auto_discovery_core_validation")
    call run_test_module("Project Scenarios", "test_auto_discovery_project_scenarios")
    call run_test_module("Error Handling", "test_auto_discovery_error_handling")
    
    ! Print comprehensive integration summary
    call print_integration_summary()
    
contains

    subroutine run_test_module(module_name, executable_name)
        !! Execute a test module and track results
        character(len=*), intent(in) :: module_name, executable_name
        integer :: exit_code
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "--- Running " // trim(module_name) // " ---"
        
        ! Reset shared test statistics for each module
        shared_test_count = 0
        shared_passed_tests = 0
        shared_all_tests_passed = .true.
        
        ! Execute the test module
        call execute_command_line('./' // trim(executable_name), &
                                 exitstat=exit_code)
        
        if (exit_code == 0) then
            passed_modules = passed_modules + 1
            write(output_unit, '(A)') "✅ " // trim(module_name) // " PASSED"
        else
            all_modules_passed = .false.
            write(output_unit, '(A)') "❌ " // trim(module_name) // " FAILED"
        end if
        
    end subroutine run_test_module

    subroutine print_integration_summary()
        !! Print comprehensive integration test summary
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "============================================="
        write(output_unit, '(A)') "  Integration Test Suite Summary           "
        write(output_unit, '(A)') "============================================="
        write(output_unit, '(A)') ""
        
        write(*, '(A,I0,A,I0,A)') "MODULES: ", passed_modules, "/", &
                                  total_modules, " passed"
        
        if (all_modules_passed) then
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "🎉 AUTO-DISCOVERY INTEGRATION SUITE PASSED"
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "All decomposed modules validated successfully:"
            write(output_unit, '(A)') "  ✅ Core validation (build detection, workflows)"
            write(output_unit, '(A)') "  ✅ Project scenarios (FPM, CMake, Make)"
            write(output_unit, '(A)') "  ✅ Error handling (failures, corrupted data)"
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "SRP-driven decomposition maintained test quality"
            write(output_unit, '(A)') "while achieving <500 line target per module."
            call exit(0)
        else
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "❌ AUTO-DISCOVERY INTEGRATION SUITE FAILED"
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "One or more test modules failed validation."
            write(output_unit, '(A)') "Check individual module output for details."
            call exit(1)
        end if
        
    end subroutine print_integration_summary

end program test_auto_discovery_integration_suite