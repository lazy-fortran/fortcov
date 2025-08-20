program test_documentation_example_validation
    !! Documentation Example Functional Validation Test Suite (Issue #193)
    !! 
    !! This test validates that all code examples in documentation are
    !! copy-paste ready and actually work as documented.
    !!
    !! Given: Example-first documentation requirement per Issue #193
    !! When: Testing all documented code examples and command sequences
    !! Then: All examples should execute successfully and produce expected results
    
    use file_utils
    use string_utils
    implicit none
    
    ! Test execution tracking
    integer :: total_tests = 0
    integer :: passed_tests = 0
    integer :: failed_tests = 0
    
    print *, "================================================================="
    print *, "DOCUMENTATION EXAMPLE VALIDATION (Issue #193 - RED Phase)"
    print *, "================================================================="
    print *, ""
    print *, "EXAMPLE VALIDATION SCOPE:"
    print *, "  ✓ All command-line examples must execute successfully"
    print *, "  ✓ All code examples must be copy-paste ready"
    print *, "  ✓ All installation procedures must work in clean environments" 
    print *, "  ✓ All configuration examples must be syntactically valid"
    print *, "  ✓ All troubleshooting examples must resolve stated problems"
    print *, ""
    
    ! === BASIC EXAMPLE VALIDATION ===
    call test_readme_quick_start_examples()
    call test_installation_command_examples()
    call test_usage_guide_examples()
    call test_configuration_examples()
    
    ! === ADVANCED EXAMPLE VALIDATION ===
    call test_build_system_integration_examples()
    call test_ci_cd_workflow_examples()
    call test_troubleshooting_resolution_examples()
    call test_api_usage_examples()
    
    ! === EXAMPLE QUALITY VALIDATION ===
    call test_example_output_accuracy()
    call test_example_error_handling()
    call test_example_performance_claims()
    
    ! Final results
    print *, ""
    print *, "================================================================="
    print *, "EXAMPLE VALIDATION TEST RESULTS"
    print *, "================================================================="
    write(*,'(A,I0)') "Total tests: ", total_tests
    write(*,'(A,I0)') "Passed: ", passed_tests
    write(*,'(A,I0)') "Failed: ", failed_tests
    
    if (failed_tests > 0) then
        print *, ""
        print *, "❌ EXAMPLE VALIDATION FAILED"
        print *, "   Documentation contains non-functional examples."
        print *, "   This is expected in RED phase - tests guide example improvement."
        call exit(1)
    else
        print *, ""
        print *, "✅ EXAMPLE VALIDATION PASSED"
        print *, "   All documentation examples are functional and copy-paste ready."
        call exit(0)
    end if

contains

    subroutine test_readme_quick_start_examples()
        !! Given: README.md contains quick start examples
        !! When: Testing each command in quick start sequence  
        !! Then: All commands should execute successfully in sequence
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "README Quick Start Example Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("README.md")) then
            ! Test basic fortcov command examples from README
            if (.not. test_command_example("fpm build --flag "//'"'//"-fprofile-arcs -ftest-coverage"//'"')) then
                call report_failure("README fpm build example fails")
                test_passed = .false.
            end if
            
            if (.not. test_command_example("fpm test --flag "//'"'//"-fprofile-arcs -ftest-coverage"//'"')) then
                call report_failure("README fpm test example fails")
                test_passed = .false.
            end if
            
            if (.not. test_command_example("fortcov --source=. --exclude=build/*,test/* --output=coverage.md")) then
                call report_failure("README fortcov command example fails")
                test_passed = .false.
            end if
            
        else
            call report_failure("README.md not found")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_readme_quick_start_examples

    subroutine test_installation_command_examples()
        !! Given: Installation documentation contains command examples
        !! When: Testing installation command sequences
        !! Then: All installation commands should work in clean environments
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Installation Command Example Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test current installation documentation
        if (file_exists("INSTALLATION.md")) then
            if (.not. validate_installation_examples("INSTALLATION.md")) then
                call report_failure("INSTALLATION.md contains non-functional examples")
                test_passed = .false.
            end if
        end if
        
        ! Test target consolidated installation documentation
        if (file_exists("doc/user/installation.md")) then
            if (.not. validate_installation_examples("doc/user/installation.md")) then
                call report_failure("doc/user/installation.md contains non-functional examples")
                test_passed = .false.
            end if
        else
            ! This should fail in RED phase as file doesn't exist yet
            call report_failure("doc/user/installation.md does not exist yet - RED phase expected")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_installation_command_examples

    subroutine test_usage_guide_examples()
        !! Given: Usage guide contains command examples
        !! When: Testing all usage examples from documentation
        !! Then: All usage examples should execute successfully
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Usage Guide Example Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("USER_GUIDE.md")) then
            if (.not. validate_usage_examples("USER_GUIDE.md")) then
                call report_failure("USER_GUIDE.md contains non-functional examples")
                test_passed = .false.
            end if
        end if
        
        if (file_exists("ENHANCED_CLI_GUIDE.md")) then
            if (.not. validate_cli_examples("ENHANCED_CLI_GUIDE.md")) then
                call report_failure("ENHANCED_CLI_GUIDE.md contains non-functional examples")
                test_passed = .false.
            end if
        end if
        
        ! Test target consolidated usage guide
        if (file_exists("doc/user/usage-guide.md")) then
            if (.not. validate_usage_examples("doc/user/usage-guide.md")) then
                call report_failure("doc/user/usage-guide.md contains non-functional examples")
                test_passed = .false.
            end if
        else
            call report_failure("doc/user/usage-guide.md does not exist yet - RED phase expected")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_usage_guide_examples

    subroutine test_configuration_examples()
        !! Given: Configuration documentation contains config file examples
        !! When: Testing all configuration examples for syntax validity
        !! Then: All config examples should be syntactically valid
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Configuration Example Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("CONFIGURATION.md")) then
            if (.not. validate_configuration_examples("CONFIGURATION.md")) then
                call report_failure("CONFIGURATION.md contains invalid configuration examples")
                test_passed = .false.
            end if
        end if
        
        ! Test target consolidated configuration guide
        if (file_exists("doc/user/configuration.md")) then
            if (.not. validate_configuration_examples("doc/user/configuration.md")) then
                call report_failure("doc/user/configuration.md contains invalid configuration examples")
                test_passed = .false.
            end if
        else
            call report_failure("doc/user/configuration.md does not exist yet - RED phase expected")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_configuration_examples

    subroutine test_build_system_integration_examples()
        !! Given: Build system integration documentation contains complex examples
        !! When: Testing build system integration examples
        !! Then: All build system examples should work with their respective tools
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Build System Integration Example Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("BUILD_SYSTEM_INTEGRATION_COMPLETE.md")) then
            if (.not. validate_build_system_examples("BUILD_SYSTEM_INTEGRATION_COMPLETE.md")) then
                call report_failure("BUILD_SYSTEM_INTEGRATION_COMPLETE.md contains non-functional build examples")
                test_passed = .false.
            end if
        end if
        
        if (file_exists("doc/developer/build-integration.md")) then
            if (.not. validate_build_system_examples("doc/developer/build-integration.md")) then
                call report_failure("doc/developer/build-integration.md contains non-functional build examples")
                test_passed = .false.
            end if
        else
            call report_failure("doc/developer/build-integration.md does not exist yet - RED phase expected")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_build_system_integration_examples

    subroutine test_ci_cd_workflow_examples()
        !! Given: CI/CD workflow documentation contains YAML examples
        !! When: Testing all CI/CD workflow examples for syntax validity
        !! Then: All YAML examples should be syntactically valid
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "CI/CD Workflow Example Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("CI_CD_MATRIX_GUIDE.md")) then
            if (.not. validate_ci_cd_examples("CI_CD_MATRIX_GUIDE.md")) then
                call report_failure("CI_CD_MATRIX_GUIDE.md contains invalid YAML examples")
                test_passed = .false.
            end if
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_ci_cd_workflow_examples

    subroutine test_troubleshooting_resolution_examples()
        !! Given: Troubleshooting documentation contains problem-resolution examples
        !! When: Testing troubleshooting examples resolve stated problems
        !! Then: All troubleshooting examples should provide working solutions
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Troubleshooting Resolution Example Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("TROUBLESHOOTING.md")) then
            if (.not. validate_troubleshooting_examples("TROUBLESHOOTING.md")) then
                call report_failure("TROUBLESHOOTING.md contains non-working solutions")
                test_passed = .false.
            end if
        end if
        
        if (file_exists("doc/user/troubleshooting.md")) then
            if (.not. validate_troubleshooting_examples("doc/user/troubleshooting.md")) then
                call report_failure("doc/user/troubleshooting.md contains non-working solutions")
                test_passed = .false.
            end if
        else
            call report_failure("doc/user/troubleshooting.md does not exist yet - RED phase expected")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_troubleshooting_resolution_examples

    subroutine test_api_usage_examples()
        !! Given: API documentation contains code usage examples
        !! When: Testing all API usage examples compile and run
        !! Then: All API examples should be compilable and functional
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "API Usage Example Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("API_REFERENCE.md")) then
            if (.not. validate_api_examples("API_REFERENCE.md")) then
                call report_failure("API_REFERENCE.md contains non-compilable examples")
                test_passed = .false.
            end if
        end if
        
        if (file_exists("doc/developer/api-reference.md")) then
            if (.not. validate_api_examples("doc/developer/api-reference.md")) then
                call report_failure("doc/developer/api-reference.md contains non-compilable examples")
                test_passed = .false.
            end if
        else
            call report_failure("doc/developer/api-reference.md does not exist yet - RED phase expected")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_api_usage_examples

    subroutine test_example_output_accuracy()
        !! Given: Documentation shows expected output for examples
        !! When: Running examples and comparing actual vs documented output
        !! Then: Actual output should match documented output
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Example Output Accuracy Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would run documented examples and verify output matches
        ! For RED phase, we expect this to fail as examples may not be accurate
        
        call report_failure("Output accuracy validation not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_example_output_accuracy

    subroutine test_example_error_handling()
        !! Given: Documentation contains examples with error scenarios
        !! When: Testing error handling examples produce expected errors
        !! Then: Error examples should demonstrate proper error handling
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Example Error Handling Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would validate that error scenario examples actually produce
        ! the documented errors and recovery procedures work
        
        call report_failure("Error handling validation not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_example_error_handling

    subroutine test_example_performance_claims()
        !! Given: Documentation contains performance claims in examples
        !! When: Testing examples meet documented performance expectations
        !! Then: Examples should achieve claimed performance characteristics
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Example Performance Claims Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would validate performance claims in examples
        call report_failure("Performance claims validation not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_example_performance_claims

    ! === HELPER FUNCTIONS ===

    function test_command_example(command) result(success)
        character(len=*), intent(in) :: command
        logical :: success
        
        ! Simulate command testing - in real implementation would execute command
        ! For RED phase, we simulate potential command failures
        success = .false.  ! Expected to fail in RED phase as examples may not work
        
        write(*,'(A,A)') "    Testing command: ", trim(command)
        write(*,'(A)') "    Command validation not fully implemented - RED phase simulation"
    end function test_command_example

    function validate_installation_examples(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        ! Validate installation examples in file
        valid = .false.  ! Expected to fail in RED phase
        
        write(*,'(A,A)') "    Validating installation examples in: ", trim(filename)
    end function validate_installation_examples

    function validate_usage_examples(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = .false.  ! Expected to fail in RED phase
        write(*,'(A,A)') "    Validating usage examples in: ", trim(filename)
    end function validate_usage_examples

    function validate_cli_examples(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = .false.  ! Expected to fail in RED phase
        write(*,'(A,A)') "    Validating CLI examples in: ", trim(filename)
    end function validate_cli_examples

    function validate_configuration_examples(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = .false.  ! Expected to fail in RED phase
        write(*,'(A,A)') "    Validating configuration examples in: ", trim(filename)
    end function validate_configuration_examples

    function validate_build_system_examples(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = .false.  ! Expected to fail in RED phase
        write(*,'(A,A)') "    Validating build system examples in: ", trim(filename)
    end function validate_build_system_examples

    function validate_ci_cd_examples(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = .false.  ! Expected to fail in RED phase
        write(*,'(A,A)') "    Validating CI/CD examples in: ", trim(filename)
    end function validate_ci_cd_examples

    function validate_troubleshooting_examples(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = .false.  ! Expected to fail in RED phase
        write(*,'(A,A)') "    Validating troubleshooting examples in: ", trim(filename)
    end function validate_troubleshooting_examples

    function validate_api_examples(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        
        valid = .false.  ! Expected to fail in RED phase
        write(*,'(A,A)') "    Validating API examples in: ", trim(filename)
    end function validate_api_examples


    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        
        total_tests = total_tests + 1
        write(*,'(A,I0,A,A)') "Test ", total_tests, ": ", trim(test_name)
    end subroutine start_test

    subroutine end_test(test_name, test_passed)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: test_passed
        
        if (test_passed) then
            passed_tests = passed_tests + 1
            print *, "  ✅ PASSED"
        else
            failed_tests = failed_tests + 1
            print *, "  ❌ FAILED"
        end if
        print *, ""
    end subroutine end_test

    subroutine report_failure(message)
        character(len=*), intent(in) :: message
        
        write(*,'(A,A)') "    FAILURE: ", trim(message)
    end subroutine report_failure

end program test_documentation_example_validation