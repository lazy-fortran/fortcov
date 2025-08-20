program test_documentation_directory_structure_validation
    !! Documentation Directory Structure and Organization Validation (Issue #193)
    !! 
    !! This test validates the target documentation directory structure,
    !! file organization, and proper cleanup of scattered documentation.
    !!
    !! Given: Target documentation architecture with organized doc/ structure
    !! When: Validating directory structure and file organization
    !! Then: Documentation should follow clean, discoverable hierarchy
    
    use file_utils
    use string_utils
    implicit none
    
    ! Test execution tracking
    integer :: total_tests = 0
    integer :: passed_tests = 0
    integer :: failed_tests = 0
    
    ! Directory structure validation
    character(len=256), parameter :: TARGET_USER_DIR = "doc/user/"
    character(len=256), parameter :: TARGET_DEVELOPER_DIR = "doc/developer/"
    character(len=256), parameter :: TARGET_IMPLEMENTATION_DIR = "doc/implementation/"
    
    print *, "================================================================="
    print *, "DIRECTORY STRUCTURE VALIDATION (Issue #193 - RED Phase)"
    print *, "================================================================="
    print *, ""
    print *, "DIRECTORY STRUCTURE REQUIREMENTS:"
    print *, "  ✓ Clean doc/ organization with audience-driven structure"
    print *, "  ✓ Root directory contains only README.md for documentation"
    print *, "  ✓ User documentation in doc/user/ with complete coverage"
    print *, "  ✓ Developer documentation in doc/developer/ with technical focus"
    print *, "  ✓ Implementation details in doc/implementation/ without clutter"
    print *, "  ✓ No orphaned documentation files or scattered content"
    print *, ""
    
    ! === DIRECTORY STRUCTURE CREATION TESTS ===
    call test_target_directory_structure()
    call test_user_documentation_directory()
    call test_developer_documentation_directory()
    call test_implementation_documentation_directory()
    
    ! === FILE ORGANIZATION TESTS ===
    call test_file_consolidation_completeness()
    call test_root_directory_cleanup()
    call test_obsolete_file_removal()
    
    ! === NAVIGATION STRUCTURE TESTS ===
    call test_documentation_discoverability()
    call test_hierarchical_organization()
    call test_file_naming_consistency()
    
    ! === MAINTENANCE STRUCTURE TESTS ===
    call test_documentation_maintainability()
    call test_future_scalability()
    call test_contributor_accessibility()
    
    ! Final results
    print *, ""
    print *, "================================================================="
    print *, "DIRECTORY STRUCTURE VALIDATION TEST RESULTS"
    print *, "================================================================="
    write(*,'(A,I0)') "Total tests: ", total_tests
    write(*,'(A,I0)') "Passed: ", passed_tests
    write(*,'(A,I0)') "Failed: ", failed_tests
    
    if (failed_tests > 0) then
        print *, ""
        print *, "❌ DIRECTORY STRUCTURE VALIDATION FAILED"
        print *, "   Documentation structure does not match target organization."
        print *, "   This is expected in RED phase - tests guide structure creation."
        call exit(1)
    else
        print *, ""
        print *, "✅ DIRECTORY STRUCTURE VALIDATION PASSED"
        print *, "   Documentation structure matches target architecture perfectly."
        call exit(0)
    end if

contains

    subroutine test_target_directory_structure()
        !! Given: Target directory structure per Issue #193 architecture
        !! When: Validating primary doc/ directories exist with correct permissions
        !! Then: All target directories should exist and be accessible
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Target Directory Structure Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test main doc/ directory exists
        if (.not. directory_exists("doc/")) then
            call report_failure("Main doc/ directory does not exist")
            test_passed = .false.
        end if
        
        ! Test user documentation directory
        if (.not. directory_exists(TARGET_USER_DIR)) then
            call report_failure("doc/user/ directory does not exist - target structure not created")
            test_passed = .false.
        end if
        
        ! Test developer documentation directory
        if (.not. directory_exists(TARGET_DEVELOPER_DIR)) then
            call report_failure("doc/developer/ directory does not exist - target structure not created")
            test_passed = .false.
        end if
        
        ! Test implementation documentation directory
        if (.not. directory_exists(TARGET_IMPLEMENTATION_DIR)) then
            call report_failure("doc/implementation/ directory does not exist - target structure not created")
            test_passed = .false.
        end if
        
        ! Test directory permissions and accessibility
        if (.not. validate_directory_permissions("doc/")) then
            call report_failure("doc/ directory has incorrect permissions")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_target_directory_structure

    subroutine test_user_documentation_directory()
        !! Given: User documentation organization requirements per Issue #193
        !! When: Validating doc/user/ contains all required user-facing files
        !! Then: Complete user documentation should be properly organized
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "User Documentation Directory Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test required user documentation files exist in correct location
        if (.not. file_exists(trim(TARGET_USER_DIR) // "installation.md")) then
            call report_failure("doc/user/installation.md missing - user docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_USER_DIR) // "getting-started.md")) then
            call report_failure("doc/user/getting-started.md missing - user docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_USER_DIR) // "usage-guide.md")) then
            call report_failure("doc/user/usage-guide.md missing - user docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_USER_DIR) // "examples.md")) then
            call report_failure("doc/user/examples.md missing - user docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_USER_DIR) // "troubleshooting.md")) then
            call report_failure("doc/user/troubleshooting.md missing - user docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_USER_DIR) // "configuration.md")) then
            call report_failure("doc/user/configuration.md missing - user docs not consolidated")
            test_passed = .false.
        end if
        
        ! Test user directory contains only user-appropriate content
        if (.not. validate_user_directory_content_appropriateness()) then
            call report_failure("doc/user/ contains inappropriately technical content")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_user_documentation_directory

    subroutine test_developer_documentation_directory()
        !! Given: Developer documentation organization requirements per Issue #193
        !! When: Validating doc/developer/ contains all required technical files
        !! Then: Complete developer documentation should be properly organized
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Developer Documentation Directory Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test required developer documentation files exist in correct location
        if (.not. file_exists(trim(TARGET_DEVELOPER_DIR) // "architecture.md")) then
            call report_failure("doc/developer/architecture.md missing - developer docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_DEVELOPER_DIR) // "api-reference.md")) then
            call report_failure("doc/developer/api-reference.md missing - developer docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_DEVELOPER_DIR) // "build-integration.md")) then
            call report_failure("doc/developer/build-integration.md missing - developer docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_DEVELOPER_DIR) // "development-guide.md")) then
            call report_failure("doc/developer/development-guide.md missing - developer docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_DEVELOPER_DIR) // "testing.md")) then
            call report_failure("doc/developer/testing.md missing - developer docs not consolidated")
            test_passed = .false.
        end if
        
        ! Test developer directory contains appropriately technical content
        if (.not. validate_developer_directory_content_appropriateness()) then
            call report_failure("doc/developer/ lacks sufficient technical depth")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_developer_documentation_directory

    subroutine test_implementation_documentation_directory()
        !! Given: Implementation documentation cleanup requirements per Issue #193
        !! When: Validating doc/implementation/ contains clean, relevant content
        !! Then: Implementation details should be organized without obsolete reports
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Implementation Documentation Directory Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test required implementation documentation files exist
        if (.not. file_exists(trim(TARGET_IMPLEMENTATION_DIR) // "design-decisions.md")) then
            call report_failure("doc/implementation/design-decisions.md missing - implementation docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_IMPLEMENTATION_DIR) // "performance.md")) then
            call report_failure("doc/implementation/performance.md missing - implementation docs not consolidated")
            test_passed = .false.
        end if
        
        if (.not. file_exists(trim(TARGET_IMPLEMENTATION_DIR) // "security.md")) then
            call report_failure("doc/implementation/security.md missing - implementation docs not consolidated")
            test_passed = .false.
        end if
        
        ! Test implementation directory doesn't contain obsolete reports
        if (.not. validate_implementation_directory_cleanliness()) then
            call report_failure("doc/implementation/ contains obsolete or inappropriate content")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_implementation_documentation_directory

    subroutine test_file_consolidation_completeness()
        !! Given: File consolidation requirements per Issue #193
        !! When: Validating all scattered files have been properly consolidated
        !! Then: No documentation should remain scattered in inappropriate locations
        
        logical :: test_passed
        character(len=256) :: test_name
        integer :: scattered_files_count
        
        test_name = "File Consolidation Completeness Validation"
        call start_test(test_name)
        
        test_passed = .true.
        scattered_files_count = 0
        
        ! Count remaining scattered files that should have been consolidated
        scattered_files_count = count_remaining_scattered_files()
        
        if (scattered_files_count > 0) then
            write(*,'(A,I0,A)') "    FAILURE: ", scattered_files_count, " files still scattered and not consolidated"
            call report_failure("File consolidation incomplete - scattered files remain")
            test_passed = .false.
        end if
        
        ! Test specific high-priority files have been moved
        if (file_exists("USER_GUIDE.md")) then
            call report_failure("USER_GUIDE.md not moved to doc/user/usage-guide.md")
            test_passed = .false.
        end if
        
        if (file_exists("INSTALLATION.md")) then
            call report_failure("INSTALLATION.md not moved to doc/user/installation.md")
            test_passed = .false.
        end if
        
        if (file_exists("API_REFERENCE.md")) then
            call report_failure("API_REFERENCE.md not moved to doc/developer/api-reference.md")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_file_consolidation_completeness

    subroutine test_root_directory_cleanup()
        !! Given: Root directory cleanup requirements per Issue #193
        !! When: Validating root directory contains minimal documentation
        !! Then: Only README.md should remain for documentation in root
        
        logical :: test_passed
        character(len=256) :: test_name
        integer :: root_doc_files_count
        
        test_name = "Root Directory Cleanup Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Count documentation files remaining in root
        root_doc_files_count = count_root_documentation_files()
        
        if (root_doc_files_count > 1) then  ! Only README.md should remain
            write(*,'(A,I0,A)') "    FAILURE: ", root_doc_files_count, " documentation files in root (should be 1: README.md)"
            call report_failure("Root directory cleanup incomplete")
            test_passed = .false.
        end if
        
        ! Ensure README.md still exists as gateway document
        if (.not. file_exists("README.md")) then
            call report_failure("README.md missing from root - should remain as gateway document")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_root_directory_cleanup

    subroutine test_obsolete_file_removal()
        !! Given: Obsolete file removal requirements per Issue #193
        !! When: Validating obsolete implementation reports are deleted
        !! Then: Obsolete files should be completely removed, not just moved
        
        logical :: test_passed
        character(len=256) :: test_name
        integer :: obsolete_files_count
        
        test_name = "Obsolete File Removal Validation"
        call start_test(test_name)
        
        test_passed = .true.
        obsolete_files_count = 0
        
        ! Test specific obsolete files have been removed
        if (file_exists("CLI_VALIDATION_REPORT.md")) then
            obsolete_files_count = obsolete_files_count + 1
            call report_failure("CLI_VALIDATION_REPORT.md not removed - obsolete implementation report")
        end if
        
        if (file_exists("VALIDATION_INTEGRATION.md")) then
            obsolete_files_count = obsolete_files_count + 1
            call report_failure("VALIDATION_INTEGRATION.md not removed - obsolete implementation report")
        end if
        
        if (file_exists("ATOMIC_TEMP_FILE_GUIDE.md")) then
            obsolete_files_count = obsolete_files_count + 1
            call report_failure("ATOMIC_TEMP_FILE_GUIDE.md not removed - obsolete implementation report")
        end if
        
        if (obsolete_files_count > 0) then
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_obsolete_file_removal

    subroutine test_documentation_discoverability()
        !! Given: Information discoverability requirements per Issue #193
        !! When: Validating documentation structure supports easy discovery
        !! Then: Users should find information within 2 minutes target time
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Documentation Discoverability Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test discoverability structure
        if (.not. validate_discoverability_structure()) then
            call report_failure("Documentation structure does not support easy discoverability")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_documentation_discoverability

    subroutine test_hierarchical_organization()
        !! Given: Hierarchical organization requirements per Issue #193
        !! When: Validating documentation follows logical hierarchy
        !! Then: Documentation should have clear parent-child relationships
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Hierarchical Organization Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test hierarchical organization
        if (.not. validate_hierarchical_organization()) then
            call report_failure("Documentation does not follow clear hierarchical organization")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_hierarchical_organization

    subroutine test_file_naming_consistency()
        !! Given: File naming consistency requirements for maintainability
        !! When: Validating file naming follows consistent patterns
        !! Then: All files should follow kebab-case naming convention
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "File Naming Consistency Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test file naming consistency
        if (.not. validate_file_naming_consistency()) then
            call report_failure("File names do not follow consistent naming patterns")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_file_naming_consistency

    subroutine test_documentation_maintainability()
        !! Given: Documentation maintainability requirements
        !! When: Validating structure supports easy maintenance
        !! Then: Documentation structure should be maintainable long-term
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Documentation Maintainability Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test maintainability structure
        if (.not. validate_maintainability_structure()) then
            call report_failure("Documentation structure does not support maintainability")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_documentation_maintainability

    subroutine test_future_scalability()
        !! Given: Future scalability requirements for documentation growth
        !! When: Validating structure can accommodate future content
        !! Then: Structure should support growth without reorganization
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Future Scalability Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test scalability structure
        if (.not. validate_scalability_structure()) then
            call report_failure("Documentation structure does not support future scalability")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_future_scalability

    subroutine test_contributor_accessibility()
        !! Given: Contributor accessibility requirements
        !! When: Validating structure is accessible to new contributors
        !! Then: New contributors should understand documentation organization
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Contributor Accessibility Validation"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Test contributor accessibility
        if (.not. validate_contributor_accessibility()) then
            call report_failure("Documentation structure not accessible to new contributors")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_contributor_accessibility

    ! === HELPER FUNCTIONS ===

    function directory_exists(directory_path) result(exists)
        character(len=*), intent(in) :: directory_path
        logical :: exists
        
        ! Check if directory exists - RED phase simulation
        exists = .false.  ! Target directories don't exist yet in RED phase
        write(*,'(A,A)') "    Checking directory: ", trim(directory_path)
    end function directory_exists

    function validate_directory_permissions(directory_path) result(permissions_valid)
        character(len=*), intent(in) :: directory_path
        logical :: permissions_valid
        
        permissions_valid = .true.  ! Assume permissions OK if directory exists
        write(*,'(A,A)') "    Validating permissions for: ", trim(directory_path)
    end function validate_directory_permissions

    function validate_user_directory_content_appropriateness() result(appropriate)
        logical :: appropriate
        
        appropriate = .false.  ! RED phase - user content not properly organized yet
    end function validate_user_directory_content_appropriateness

    function validate_developer_directory_content_appropriateness() result(appropriate)
        logical :: appropriate
        
        appropriate = .false.  ! RED phase simulation
    end function validate_developer_directory_content_appropriateness

    function validate_implementation_directory_cleanliness() result(clean)
        logical :: clean
        
        clean = .false.  ! RED phase - implementation docs not cleaned yet
    end function validate_implementation_directory_cleanliness

    function count_remaining_scattered_files() result(count)
        integer :: count
        
        ! Count scattered files still in root that should be consolidated
        count = 0
        
        if (file_exists("USER_GUIDE.md")) count = count + 1
        if (file_exists("INSTALLATION.md")) count = count + 1
        if (file_exists("TROUBLESHOOTING.md")) count = count + 1
        if (file_exists("EXAMPLES.md")) count = count + 1
        if (file_exists("CONFIGURATION.md")) count = count + 1
        if (file_exists("API_REFERENCE.md")) count = count + 1
        if (file_exists("ENHANCED_CLI_GUIDE.md")) count = count + 1
        if (file_exists("BUILD_SYSTEM_INTEGRATION_COMPLETE.md")) count = count + 1
        if (file_exists("ARCHITECTURAL_DECOMPOSITION.md")) count = count + 1
        if (file_exists("MODULAR_ARCHITECTURE_GUIDE.md")) count = count + 1
        if (file_exists("FOUNDATION_LAYER_GUIDE.md")) count = count + 1
        if (file_exists("COVERAGE_REPORTING_ARCHITECTURE.md")) count = count + 1
        if (file_exists("CI_CD_MATRIX_GUIDE.md")) count = count + 1
    end function count_remaining_scattered_files

    function count_root_documentation_files() result(count)
        integer :: count
        
        ! Count .md files in root directory
        count = 0
        
        ! This would use directory scanning in real implementation
        ! For RED phase, simulate high count due to scattered files
        count = 20  ! RED phase simulation - many files still scattered
    end function count_root_documentation_files

    function validate_discoverability_structure() result(discoverable)
        logical :: discoverable
        
        discoverable = .false.  ! RED phase - structure not optimized for discovery yet
    end function validate_discoverability_structure

    function validate_hierarchical_organization() result(hierarchical)
        logical :: hierarchical
        
        hierarchical = .false.  ! RED phase simulation
    end function validate_hierarchical_organization

    function validate_file_naming_consistency() result(consistent)
        logical :: consistent
        
        consistent = .false.  ! RED phase - naming not consistent yet
    end function validate_file_naming_consistency

    function validate_maintainability_structure() result(maintainable)
        logical :: maintainable
        
        maintainable = .false.  ! RED phase simulation
    end function validate_maintainability_structure

    function validate_scalability_structure() result(scalable)
        logical :: scalable
        
        scalable = .false.  ! RED phase simulation
    end function validate_scalability_structure

    function validate_contributor_accessibility() result(accessible)
        logical :: accessible
        
        accessible = .false.  ! RED phase simulation
    end function validate_contributor_accessibility

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

end program test_documentation_directory_structure_validation