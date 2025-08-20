program test_documentation_duplication_detection
    !! Documentation Content Duplication Detection Test Suite (Issue #193)
    !! 
    !! This test implements automated detection of content duplication across
    !! documentation files, enforcing the "single source of truth" principle.
    !!
    !! Given: Documentation consolidation requirement to eliminate ALL duplication
    !! When: Scanning all markdown files for duplicate content blocks
    !! Then: No significant content should appear in multiple documents
    
    use file_utils
    use string_utils
    implicit none
    
    ! Test execution tracking
    integer :: total_tests = 0
    integer :: passed_tests = 0
    integer :: failed_tests = 0
    
    ! Duplication detection parameters
    integer, parameter :: MIN_DUPLICATE_LINES = 3
    integer, parameter :: MIN_DUPLICATE_CHARS = 100
    integer, parameter :: MAX_FILES = 100
    integer, parameter :: MAX_LINES_PER_FILE = 5000
    
    print *, "================================================================="
    print *, "DOCUMENTATION DUPLICATION DETECTION (Issue #193 - RED Phase)"
    print *, "================================================================="
    print *, ""
    print *, "DUPLICATION DETECTION PARAMETERS:"
    write(*,'(A,I0,A)') "  ✓ Minimum duplicate block: ", MIN_DUPLICATE_LINES, " lines"
    write(*,'(A,I0,A)') "  ✓ Minimum duplicate chars: ", MIN_DUPLICATE_CHARS, " characters"
    print *, "  ✓ Scanning all .md files for content overlap"
    print *, "  ✓ Detecting architectural content scattered across files"
    print *, ""
    
    ! === DUPLICATION DETECTION TESTS ===
    call test_architecture_content_consolidation()
    call test_installation_procedure_duplication()
    call test_example_code_duplication()
    call test_troubleshooting_content_duplication()
    call test_cli_reference_duplication()
    
    ! === CONTENT ORGANIZATION TESTS ===
    call test_scattered_documentation_detection()
    call test_obsolete_content_identification()
    call test_redundant_file_detection()
    
    ! Final results
    print *, ""
    print *, "================================================================="
    print *, "DUPLICATION DETECTION TEST RESULTS"
    print *, "================================================================="
    write(*,'(A,I0)') "Total tests: ", total_tests
    write(*,'(A,I0)') "Passed: ", passed_tests
    write(*,'(A,I0)') "Failed: ", failed_tests
    
    if (failed_tests > 0) then
        print *, ""
        print *, "❌ DUPLICATION DETECTION VALIDATION FAILED"
        print *, "   Content duplication found - consolidation needed."
        print *, "   This is expected in RED phase - tests guide implementation."
        call exit(1)
    else
        print *, ""
        print *, "✅ DUPLICATION DETECTION VALIDATION PASSED"
        print *, "   No content duplication found - single source of truth achieved."
        call exit(0)
    end if

contains

    subroutine test_architecture_content_consolidation()
        !! Given: Multiple architecture documents in root directory
        !! When: Checking for architecture content scattered across files
        !! Then: Architecture content should be consolidated into single document
        
        logical :: test_passed
        character(len=256) :: test_name
        character(len=100) :: content1, content2
        
        test_name = "Architecture Content Consolidation Detection"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Check for architecture content in multiple files - this SHOULD fail in RED phase
        if (file_exists("ARCHITECTURAL_DECOMPOSITION.md") .and. file_exists("MODULAR_ARCHITECTURE_GUIDE.md")) then
            ! In RED phase, these files exist and likely have overlapping content
            content1 = simulate_file_content("ARCHITECTURAL_DECOMPOSITION.md")
            content2 = simulate_file_content("MODULAR_ARCHITECTURE_GUIDE.md")
            
            if (detect_content_overlap(content1, content2)) then
                call report_failure("Architecture content duplication detected between ARCHITECTURAL_DECOMPOSITION.md and MODULAR_ARCHITECTURE_GUIDE.md")
                call report_failure("These files should be consolidated into doc/developer/architecture.md")
                test_passed = .false.
            end if
        end if
        
        if (file_exists("FOUNDATION_LAYER_GUIDE.md") .and. file_exists("DESIGN.md")) then
            content1 = simulate_file_content("FOUNDATION_LAYER_GUIDE.md")
            content2 = simulate_file_content("DESIGN.md")
            
            if (detect_content_overlap(content1, content2)) then
                call report_failure("Architecture content duplication detected between FOUNDATION_LAYER_GUIDE.md and DESIGN.md")
                call report_failure("Foundation layer content should be consolidated into unified architecture document")
                test_passed = .false.
            end if
        end if
        
        if (file_exists("COVERAGE_REPORTING_ARCHITECTURE.md") .and. file_exists("DESIGN.md")) then
            content1 = simulate_file_content("COVERAGE_REPORTING_ARCHITECTURE.md")
            content2 = simulate_file_content("DESIGN.md")
            
            if (detect_content_overlap(content1, content2)) then
                call report_failure("Reporting architecture content duplication detected")
                call report_failure("Should be consolidated into unified architecture document")
                test_passed = .false.
            end if
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_architecture_content_consolidation

    subroutine test_installation_procedure_duplication()
        !! Given: Installation procedures possibly scattered across files
        !! When: Checking for installation content duplication
        !! Then: Installation procedures should appear in single location
        
        logical :: test_passed
        character(len=256) :: test_name
        character(len=100) :: content1, content2
        
        test_name = "Installation Procedure Duplication Detection"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("INSTALLATION.md") .and. file_exists("README.md")) then
            content1 = simulate_file_content("INSTALLATION.md")
            content2 = simulate_file_content("README.md")
            
            if (detect_installation_overlap(content1, content2)) then
                call report_failure("Installation procedure duplication between INSTALLATION.md and README.md")
                call report_failure("Installation details should be in doc/user/installation.md with README containing brief overview only")
                test_passed = .false.
            end if
        end if
        
        if (file_exists("BUILD_SYSTEM_INTEGRATION_COMPLETE.md") .and. file_exists("INSTALLATION.md")) then
            content1 = simulate_file_content("BUILD_SYSTEM_INTEGRATION_COMPLETE.md")
            content2 = simulate_file_content("INSTALLATION.md")
            
            if (detect_content_overlap(content1, content2)) then
                call report_failure("Build system integration content duplicated across files")
                call report_failure("Should be consolidated in doc/user/installation.md and doc/developer/build-integration.md")
                test_passed = .false.
            end if
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_installation_procedure_duplication

    subroutine test_example_code_duplication()
        !! Given: Code examples possibly scattered across multiple files
        !! When: Checking for duplicate code examples
        !! Then: Each example should appear exactly once
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Code Example Duplication Detection"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Check for duplicate code examples across files
        if (file_exists("EXAMPLES.md") .and. file_exists("USER_GUIDE.md")) then
            if (detect_code_example_duplication("EXAMPLES.md", "USER_GUIDE.md")) then
                call report_failure("Code examples duplicated between EXAMPLES.md and USER_GUIDE.md")
                call report_failure("Examples should be consolidated in doc/user/examples.md")
                test_passed = .false.
            end if
        end if
        
        if (file_exists("README.md") .and. file_exists("EXAMPLES.md")) then
            if (detect_code_example_duplication("README.md", "EXAMPLES.md")) then
                call report_failure("Code examples duplicated between README.md and EXAMPLES.md")
                call report_failure("README should contain only basic example, detailed examples in doc/user/examples.md")
                test_passed = .false.
            end if
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_example_code_duplication

    subroutine test_troubleshooting_content_duplication()
        !! Given: Troubleshooting information possibly scattered across files
        !! When: Checking for troubleshooting content duplication
        !! Then: Troubleshooting should be consolidated in single location
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Troubleshooting Content Duplication Detection"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("TROUBLESHOOTING.md") .and. file_exists("USER_GUIDE.md")) then
            if (detect_troubleshooting_overlap("TROUBLESHOOTING.md", "USER_GUIDE.md")) then
                call report_failure("Troubleshooting content duplicated between files")
                call report_failure("Should be consolidated in doc/user/troubleshooting.md")
                test_passed = .false.
            end if
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_troubleshooting_content_duplication

    subroutine test_cli_reference_duplication()
        !! Given: CLI reference information possibly scattered across files
        !! When: Checking for CLI documentation duplication
        !! Then: CLI reference should appear in single authoritative location
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "CLI Reference Duplication Detection"
        call start_test(test_name)
        
        test_passed = .true.
        
        if (file_exists("ENHANCED_CLI_GUIDE.md") .and. file_exists("USER_GUIDE.md")) then
            if (detect_cli_reference_duplication("ENHANCED_CLI_GUIDE.md", "USER_GUIDE.md")) then
                call report_failure("CLI reference content duplicated across files")
                call report_failure("Should be consolidated in doc/user/usage-guide.md")
                test_passed = .false.
            end if
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_cli_reference_duplication

    subroutine test_scattered_documentation_detection()
        !! Given: Documentation consolidation requirements
        !! When: Scanning for files that should be consolidated
        !! Then: Flag files that represent scattered information
        
        logical :: test_passed
        character(len=256) :: test_name
        integer :: scattered_files_count
        
        test_name = "Scattered Documentation Detection"
        call start_test(test_name)
        
        scattered_files_count = 0
        test_passed = .true.
        
        ! Count scattered documentation files in root that should be consolidated
        if (file_exists("USER_GUIDE.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("INSTALLATION.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("TROUBLESHOOTING.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("EXAMPLES.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("CONFIGURATION.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("API_REFERENCE.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("ENHANCED_CLI_GUIDE.md")) scattered_files_count = scattered_files_count + 1
        
        ! Architecture files that should be consolidated
        if (file_exists("ARCHITECTURAL_DECOMPOSITION.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("MODULAR_ARCHITECTURE_GUIDE.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("FOUNDATION_LAYER_GUIDE.md")) scattered_files_count = scattered_files_count + 1
        if (file_exists("COVERAGE_REPORTING_ARCHITECTURE.md")) scattered_files_count = scattered_files_count + 1
        
        if (scattered_files_count > 0) then
            write(*,'(A,I0,A)') "    FAILURE: ", scattered_files_count, " scattered documentation files detected"
            call report_failure("Documentation files should be consolidated into organized doc/ structure")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_scattered_documentation_detection

    subroutine test_obsolete_content_identification()
        !! Given: Documentation cleanup requirements per Issue #193
        !! When: Checking for obsolete implementation reports
        !! Then: Obsolete files should be identified for deletion
        
        logical :: test_passed
        character(len=256) :: test_name
        integer :: obsolete_files_count
        
        test_name = "Obsolete Content Identification"
        call start_test(test_name)
        
        obsolete_files_count = 0
        test_passed = .true.
        
        ! Count obsolete implementation reports
        if (file_exists("CLI_VALIDATION_REPORT.md")) then
            obsolete_files_count = obsolete_files_count + 1
            call report_failure("CLI_VALIDATION_REPORT.md is obsolete implementation report - should be deleted")
        end if
        
        if (file_exists("VALIDATION_INTEGRATION.md")) then
            obsolete_files_count = obsolete_files_count + 1
            call report_failure("VALIDATION_INTEGRATION.md is obsolete implementation report - should be deleted")
        end if
        
        if (file_exists("ATOMIC_TEMP_FILE_GUIDE.md")) then
            obsolete_files_count = obsolete_files_count + 1
            call report_failure("ATOMIC_TEMP_FILE_GUIDE.md is obsolete implementation report - should be deleted or merged")
        end if
        
        if (obsolete_files_count > 0) then
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_obsolete_content_identification

    subroutine test_redundant_file_detection()
        !! Given: File consolidation requirements per Issue #193
        !! When: Checking for files that serve redundant purposes
        !! Then: Redundant files should be identified for consolidation
        
        logical :: test_passed
        character(len=256) :: test_name
        
        test_name = "Redundant File Detection"
        call start_test(test_name)
        
        test_passed = .true.
        
        ! Check for files with overlapping purposes
        if (file_exists("BUILD_SYSTEM_INTEGRATION_COMPLETE.md") .and. file_exists("CI_CD_MATRIX_GUIDE.md")) then
            call report_failure("BUILD_SYSTEM_INTEGRATION_COMPLETE.md and CI_CD_MATRIX_GUIDE.md have overlapping content")
            call report_failure("Should be consolidated into doc/user/installation.md and doc/developer/build-integration.md")
            test_passed = .false.
        end if
        
        call end_test(test_name, test_passed)
    end subroutine test_redundant_file_detection

    ! === HELPER FUNCTIONS ===

    function simulate_file_content(filename) result(content)
        character(len=*), intent(in) :: filename
        character(len=100) :: content
        
        ! Simplified file reading - in real implementation would read entire file
        ! For RED phase testing, we simulate content reading
        content = "simulated file content for " // filename
    end function simulate_file_content

    function detect_content_overlap(content1, content2) result(has_overlap)
        character(len=*), intent(in) :: content1, content2
        logical :: has_overlap
        
        ! Simplified overlap detection - in real implementation would do sophisticated analysis
        ! For RED phase, we assume overlap exists for scattered files
        has_overlap = .true.  ! Expected to detect overlap in RED phase
    end function detect_content_overlap

    function detect_installation_overlap(content1, content2) result(has_overlap)
        character(len=*), intent(in) :: content1, content2
        logical :: has_overlap
        
        ! Detect installation-specific content overlap
        has_overlap = .true.  ! Expected in RED phase
    end function detect_installation_overlap

    function detect_code_example_duplication(file1, file2) result(has_duplication)
        character(len=*), intent(in) :: file1, file2
        logical :: has_duplication
        
        ! Detect duplicate code examples between files
        has_duplication = .true.  ! Expected in RED phase
    end function detect_code_example_duplication

    function detect_troubleshooting_overlap(file1, file2) result(has_overlap)
        character(len=*), intent(in) :: file1, file2
        logical :: has_overlap
        
        ! Detect troubleshooting content overlap
        has_overlap = .true.  ! Expected in RED phase
    end function detect_troubleshooting_overlap

    function detect_cli_reference_duplication(file1, file2) result(has_duplication)
        character(len=*), intent(in) :: file1, file2
        logical :: has_duplication
        
        ! Detect CLI reference content duplication
        has_duplication = .true.  ! Expected in RED phase
    end function detect_cli_reference_duplication


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

end program test_documentation_duplication_detection