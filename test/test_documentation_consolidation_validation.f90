program test_documentation_consolidation_validation
    !! Documentation Consolidation Architecture Validation Test Suite (Issue #193)
    !! 
    !! This test suite implements RED phase validation for documentation consolidation,
    !! testing the target documentation architecture described in DESIGN.md.
    !!
    !! Given: Current scattered documentation across 52+ files
    !! When: Documentation consolidation is implemented per Issue #193 requirements
    !! Then: All documentation should be properly organized in doc/ structure with no duplication
    
    use file_utils
    use string_utils
    implicit none
    
    ! Test execution tracking
    integer :: total_tests = 0
    integer :: passed_tests = 0
    integer :: failed_tests = 0
    
    print *, "================================================================="
    print *, "DOCUMENTATION CONSOLIDATION VALIDATION (Issue #193 - RED Phase)"
    print *, "================================================================="
    print *, ""
    print *, "TESTING TARGET ARCHITECTURE:"
    print *, "  ✓ All user docs in doc/user/ with proper structure"
    print *, "  ✓ All developer docs in doc/developer/ with clear separation"
    print *, "  ✓ Implementation docs in doc/implementation/ without obsolete reports"
    print *, "  ✓ Zero content duplication across all documents"
    print *, "  ✓ All examples copy-paste ready and functional"
    print *, "  ✓ All internal links point to correct locations"
    print *, "  ✓ Clean root directory with only README.md for documentation"
    print *, ""
    
    ! === DOCUMENTATION FILE ORGANIZATION TESTS ===
    call test_user_documentation_structure()
    call test_developer_documentation_structure()
    call test_implementation_documentation_cleanup()
    call test_root_directory_documentation_cleanup()
    
    ! === CONTENT QUALITY AND DUPLICATION TESTS ===
    call test_content_duplication_elimination()
    call test_cross_reference_integrity()
    call test_audience_targeting_validation()
    
    ! === EXAMPLE VALIDATION TESTS ===
    call test_documentation_examples_functional()
    call test_installation_procedures_work()
    call test_getting_started_workflow_success()
    
    ! === NAVIGATION AND DISCOVERABILITY TESTS ===
    call test_documentation_navigation_structure()
    call test_information_findability()
    call test_user_journey_completeness()
    
    ! Final results
    print *, ""
    print *, "================================================================="
    print *, "DOCUMENTATION CONSOLIDATION TEST RESULTS"
    print *, "================================================================="
    write(*,'(A,I0)') "Total tests: ", total_tests
    write(*,'(A,I0)') "Passed: ", passed_tests
    write(*,'(A,I0)') "Failed: ", failed_tests
    
    if (failed_tests > 0) then
        print *, ""
        print *, "❌ DOCUMENTATION CONSOLIDATION VALIDATION FAILED"
        print *, "   Documentation architecture does not meet target state."
        print *, "   This is expected in RED phase - tests should guide implementation."
        call exit(1)
    else
        print *, ""
        print *, "✅ DOCUMENTATION CONSOLIDATION VALIDATION PASSED"
        print *, "   Target documentation architecture successfully achieved."
        call exit(0)
    end if

contains

    subroutine test_user_documentation_structure()
        !! Given: Target user documentation structure per Issue #193
        !! When: Checking for required user documentation files and organization
        !! Then: All user docs should be in doc/user/ with correct naming and content
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "User Documentation Structure Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test required user documentation files exist
        if (.not. file_exists("doc/user/installation.md")) then
            call report_failure("Missing doc/user/installation.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/user/getting-started.md")) then
            call report_failure("Missing doc/user/getting-started.md") 
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/user/usage-guide.md")) then
            call report_failure("Missing doc/user/usage-guide.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/user/examples.md")) then
            call report_failure("Missing doc/user/examples.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/user/troubleshooting.md")) then
            call report_failure("Missing doc/user/troubleshooting.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/user/configuration.md")) then
            call report_failure("Missing doc/user/configuration.md")
            test_passed = .false.
        end if
        
        ! Test that old scattered user docs are gone from root
        if (file_exists("USER_GUIDE.md")) then
            call report_failure("USER_GUIDE.md still exists in root - should be moved to doc/user/")
            test_passed = .false.
        end if
        
        if (file_exists("INSTALLATION.md")) then
            call report_failure("INSTALLATION.md still exists in root - should be moved to doc/user/")
            test_passed = .false.
        end if
        
        if (file_exists("TROUBLESHOOTING.md")) then
            call report_failure("TROUBLESHOOTING.md still exists in root - should be moved to doc/user/")
            test_passed = .false.
        end if
        
        if (file_exists("EXAMPLES.md")) then
            call report_failure("EXAMPLES.md still exists in root - should be moved to doc/user/")
            test_passed = .false.
        end if
        
        if (file_exists("CONFIGURATION.md")) then
            call report_failure("CONFIGURATION.md still exists in root - should be moved to doc/user/")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_user_documentation_structure

    subroutine test_developer_documentation_structure()
        !! Given: Target developer documentation structure per Issue #193
        !! When: Checking for required developer documentation files and organization
        !! Then: All developer docs should be in doc/developer/ with consolidated architecture
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Developer Documentation Structure Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test required developer documentation files exist
        if (.not. file_exists("doc/developer/architecture.md")) then
            call report_failure("Missing doc/developer/architecture.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/developer/api-reference.md")) then
            call report_failure("Missing doc/developer/api-reference.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/developer/build-integration.md")) then
            call report_failure("Missing doc/developer/build-integration.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/developer/development-guide.md")) then
            call report_failure("Missing doc/developer/development-guide.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/developer/testing.md")) then
            call report_failure("Missing doc/developer/testing.md")
            test_passed = .false.
        end if
        
        ! Test that scattered architecture docs are consolidated
        if (file_exists("ARCHITECTURAL_DECOMPOSITION.md")) then
            call report_failure("ARCHITECTURAL_DECOMPOSITION.md still in root - should be consolidated into doc/developer/architecture.md")
            test_passed = .false.
        end if
        
        if (file_exists("MODULAR_ARCHITECTURE_GUIDE.md")) then
            call report_failure("MODULAR_ARCHITECTURE_GUIDE.md still in root - should be consolidated into doc/developer/architecture.md")
            test_passed = .false.
        end if
        
        if (file_exists("FOUNDATION_LAYER_GUIDE.md")) then
            call report_failure("FOUNDATION_LAYER_GUIDE.md still in root - should be consolidated into doc/developer/architecture.md")
            test_passed = .false.
        end if
        
        if (file_exists("COVERAGE_REPORTING_ARCHITECTURE.md")) then
            call report_failure("COVERAGE_REPORTING_ARCHITECTURE.md still in root - should be consolidated into doc/developer/architecture.md")
            test_passed = .false.
        end if
        
        if (file_exists("API_REFERENCE.md")) then
            call report_failure("API_REFERENCE.md still in root - should be moved to doc/developer/")
            test_passed = .false.
        end if
        
        if (file_exists("BUILD_SYSTEM_INTEGRATION_COMPLETE.md")) then
            call report_failure("BUILD_SYSTEM_INTEGRATION_COMPLETE.md still in root - should be moved to doc/developer/build-integration.md")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_developer_documentation_structure

    subroutine test_implementation_documentation_cleanup()
        !! Given: Implementation documentation cleanup requirements per Issue #193
        !! When: Checking for proper cleanup of obsolete implementation reports
        !! Then: Only relevant implementation docs should remain, obsolete reports deleted
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Implementation Documentation Cleanup Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test required implementation documentation files exist
        if (.not. file_exists("doc/implementation/design-decisions.md")) then
            call report_failure("Missing doc/implementation/design-decisions.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/implementation/performance.md")) then
            call report_failure("Missing doc/implementation/performance.md")
            test_passed = .false.
        end if
        
        if (.not. file_exists("doc/implementation/security.md")) then
            call report_failure("Missing doc/implementation/security.md")
            test_passed = .false.
        end if
        
        ! Test obsolete implementation reports are deleted
        if (file_exists("CLI_VALIDATION_REPORT.md")) then
            call report_failure("CLI_VALIDATION_REPORT.md is obsolete - should be deleted")
            test_passed = .false.
        end if
        
        if (file_exists("VALIDATION_INTEGRATION.md")) then
            call report_failure("VALIDATION_INTEGRATION.md is obsolete - should be deleted")
            test_passed = .false.
        end if
        
        if (file_exists("ATOMIC_TEMP_FILE_GUIDE.md")) then
            call report_failure("ATOMIC_TEMP_FILE_GUIDE.md is obsolete - should be deleted or merged")
            test_passed = .false.
        end if
        
        if (file_exists("PERFORMANCE_PROFILE.md")) then
            call report_failure("PERFORMANCE_PROFILE.md should be moved to doc/implementation/performance.md")
            test_passed = .false.
        end if
        
        if (file_exists("TIMEOUT_PROTECTION_ARCHITECTURE.md")) then
            call report_failure("TIMEOUT_PROTECTION_ARCHITECTURE.md should be merged into doc/implementation/security.md")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_implementation_documentation_cleanup

    subroutine test_root_directory_documentation_cleanup()
        !! Given: Root directory cleanup requirements per Issue #193
        !! When: Checking root directory for documentation pollution
        !! Then: Only README.md should remain for documentation in root
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Root Directory Documentation Cleanup Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! README.md should still exist as gateway document
        if (.not. file_exists("README.md")) then
            call report_failure("README.md missing - should remain as gateway document")
            test_passed = .false.
        end if
        
        ! Test for scattered documentation files that should be gone
        if (file_exists("ENHANCED_CLI_GUIDE.md")) then
            call report_failure("ENHANCED_CLI_GUIDE.md still in root - should be merged into doc/user/usage-guide.md")
            test_passed = .false.
        end if
        
        if (file_exists("CI_CD_MATRIX_GUIDE.md")) then
            call report_failure("CI_CD_MATRIX_GUIDE.md still in root - should be merged into doc/user/installation.md")
            test_passed = .false.
        end if
        
        ! Generated files that can remain in root
        ! coverage.md is generated output and can stay in root
        
        call end_test(test_name, test_passed)
    end subroutine test_root_directory_documentation_cleanup

    subroutine test_content_duplication_elimination()
        !! Given: Content duplication elimination requirement per Issue #193
        !! When: Scanning all documentation for duplicate content
        !! Then: No significant content should appear in multiple documents
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Content Duplication Elimination Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would require sophisticated content analysis
        ! For RED phase, we assume this will initially fail and be implemented later
        ! In real implementation, this would scan all .md files for duplicate paragraphs/sections
        
        call report_failure("Content duplication detection not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_content_duplication_elimination

    subroutine test_cross_reference_integrity()
        !! Given: Cross-reference integrity requirement per Issue #193
        !! When: Validating all internal links and references
        !! Then: All internal references should point to correct locations
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Cross-Reference Integrity Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would scan all markdown files for internal links
        ! and verify they point to existing files and sections
        ! For RED phase, we expect this to fail initially
        
        call report_failure("Cross-reference integrity validation not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_cross_reference_integrity

    subroutine test_audience_targeting_validation()
        !! Given: Audience-driven information architecture requirement per Issue #193  
        !! When: Validating content appears in correct audience sections
        !! Then: User content in doc/user/, developer content in doc/developer/
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Audience Targeting Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would analyze content to ensure user-facing content
        ! (installation, usage, troubleshooting) is in doc/user/
        ! and technical content (API, architecture) is in doc/developer/
        
        call report_failure("Audience targeting validation not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_audience_targeting_validation

    subroutine test_documentation_examples_functional()
        !! Given: Example-first documentation requirement per Issue #193
        !! When: Testing all documented code examples
        !! Then: All examples should be copy-paste ready and functional
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Documentation Examples Functionality Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would extract all code examples from documentation
        ! and verify they execute successfully
        
        call report_failure("Documentation example validation not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_documentation_examples_functional

    subroutine test_installation_procedures_work()
        !! Given: Installation documentation consolidation in doc/user/installation.md
        !! When: Testing all documented installation procedures
        !! Then: All installation workflows should succeed as documented
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Installation Procedures Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would execute documented installation procedures
        ! in clean environments to verify they work as promised
        
        if (file_exists("doc/user/installation.md")) then
            ! Test would extract and execute installation commands
            call report_failure("Installation procedure validation not yet implemented")
            test_passed = .false.
        else
            call report_failure("doc/user/installation.md does not exist yet")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_installation_procedures_work

    subroutine test_getting_started_workflow_success()
        !! Given: Getting started documentation consolidation in doc/user/getting-started.md
        !! When: Following documented getting started workflows
        !! Then: New users should achieve 95% success rate following docs
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Getting Started Workflow Success Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("doc/user/getting-started.md")) then
            ! Test would simulate new user following getting started guide
            call report_failure("Getting started workflow validation not yet implemented")
            test_passed = .false.
        else
            call report_failure("doc/user/getting-started.md does not exist yet")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_getting_started_workflow_success

    subroutine test_documentation_navigation_structure()
        !! Given: Gateway document navigation requirement per Issue #193
        !! When: Validating README.md provides clear navigation to all docs
        !! Then: Users should find any information within 2 minutes
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Documentation Navigation Structure Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("README.md")) then
            ! Test would analyze README.md for comprehensive navigation links
            call report_failure("Navigation structure validation not yet implemented")
            test_passed = .false.
        else
            call report_failure("README.md missing")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_documentation_navigation_structure

    subroutine test_information_findability()
        !! Given: Information architecture improvement requirement per Issue #193
        !! When: Testing information discovery patterns
        !! Then: All common user questions should have clear answers within 2 minutes
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Information Findability Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would simulate common user information-seeking patterns
        call report_failure("Information findability validation not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_information_findability

    subroutine test_user_journey_completeness()
        !! Given: User journey optimization requirement per Issue #193
        !! When: Testing complete user workflows from start to finish
        !! Then: All user journeys should be documented and functional
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "User Journey Completeness Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! This test would validate complete user journeys:
        ! - New user: installation → getting started → first coverage report
        ! - Advanced user: configuration → complex workflows → troubleshooting
        ! - Developer: architecture → API → contribution
        
        call report_failure("User journey completeness validation not yet implemented - RED phase expected failure")
        test_passed = .false.
        
        call end_test(test_name, test_passed)
    end subroutine test_user_journey_completeness

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


end program test_documentation_consolidation_validation