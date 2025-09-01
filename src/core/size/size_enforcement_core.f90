module size_enforcement_core
    !! CI Integration and Enforcement for Proactive Size Management
    !! 
    !! Provides CI-ready enforcement mechanisms for architectural size limits.
    !! Integrates with architectural_size_validator for comprehensive enforcement.
    !! Addresses Issue #718 CI integration requirements.
    !! 
    !! CI INTEGRATION FEATURES:
    !! - Pre-commit hook integration for size validation
    !! - CI pipeline step with proper exit codes
    !! - GitHub Actions compatible output formatting
    !! - Automated violation blocking with clear remediation guidance
    use architectural_size_validator
    use size_report_generator, only: count_file_violations_by_severity, &
                                     count_directory_violations_by_severity
    use error_handling_core
    use string_utils, only: int_to_string
    implicit none
    private
    
    ! Public interface for CI enforcement
    public :: enforce_size_limits_for_ci
    public :: generate_pre_commit_hook
    public :: validate_pr_changes
    public :: size_enforcement_config_t
    public :: ci_enforcement_result_t
    
    ! Exit codes for CI integration
    integer, parameter :: CI_SUCCESS = 0
    integer, parameter :: CI_SUCCESS_WITH_WARNINGS = 1  
    integer, parameter :: CI_FAILURE_VIOLATIONS = 2
    integer, parameter :: CI_FAILURE_ERROR = 3
    
    ! Configuration for size enforcement
    type :: size_enforcement_config_t
        logical :: fail_on_warnings  ! Should warnings cause CI failure?
        logical :: fail_on_violations  ! Should violations cause CI failure?
        logical :: generate_github_annotations  ! Generate GitHub Actions annotations?
        character(len=:), allocatable :: base_directory
        character(len=:), allocatable :: output_format  ! "ci", "human", "json"
        logical :: verbose_output  ! Include detailed violation information?
    end type size_enforcement_config_t
    
    ! Result of CI enforcement check
    type :: ci_enforcement_result_t
        integer :: exit_code
        logical :: should_block_merge
        character(len=:), allocatable :: summary_message
        character(len=:), allocatable :: detailed_report
        character(len=:), allocatable :: remediation_actions
        integer :: violations_count
        integer :: warnings_count
    end type ci_enforcement_result_t

contains

    subroutine enforce_size_limits_for_ci(config, result)
        !! Main CI enforcement entry point - validates codebase and sets exit codes
        !! This is the primary function called by CI pipelines
        type(size_enforcement_config_t), intent(in) :: config
        type(ci_enforcement_result_t), intent(out) :: result
        
        type(architectural_size_report_t) :: size_report
        type(error_context_t) :: error_ctx
        
        ! Initialize result
        result%exit_code = CI_SUCCESS
        result%should_block_merge = .false.
        result%violations_count = 0
        result%warnings_count = 0
        
        ! Validate codebase architecture
        call validate_codebase_architecture(config%base_directory, size_report, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call handle_enforcement_error(error_ctx, result)
            return
        end if
        
        ! Analyze results and set CI exit codes
        call determine_ci_exit_code(size_report, config, result)
        
        ! Generate appropriate reports
        call generate_enforcement_reports(size_report, config, result)
        
    end subroutine enforce_size_limits_for_ci
    
    subroutine generate_pre_commit_hook(hook_content)
        !! Generates pre-commit hook script for proactive size validation
        !! Prevents commits that would introduce size violations
        character(len=:), allocatable, intent(out) :: hook_content
        
        hook_content = '#!/bin/bash' // new_line('') // &
                      '# Proactive Size Management Pre-commit Hook' // new_line('') // &
                      '# Generated for Issue #718 - Architectural Compliance' // new_line('') // &
                      '' // new_line('') // &
                      'set -e' // new_line('') // &
                      '' // new_line('') // &
                      'echo "🔍 Checking architectural size compliance..."' // new_line('') // &
                      '' // new_line('') // &
                      '# Run size validation' // new_line('') // &
                      'fpm run fortcov -- --validate-architecture --ci-format' // new_line('') // &
                      '' // new_line('') // &
                      'exit_code=$?' // new_line('') // &
                      '' // new_line('') // &
                      'case $exit_code in' // new_line('') // &
                      '    0)' // new_line('') // &
                      '        echo "✅ All architectural size checks passed"' // new_line('') // &
                      '        exit 0' // new_line('') // &
                      '        ;;' // new_line('') // &
                      '    1)' // new_line('') // &
                      '        echo "⚠️  Size warnings detected - commit allowed"' // new_line('') // &
                      '        echo "Consider refactoring before files reach limits"' // new_line('') // &
                      '        exit 0' // new_line('') // &
                      '        ;;' // new_line('') // &
                      '    2)' // new_line('') // &
                      '        echo "❌ ARCHITECTURAL VIOLATIONS DETECTED"' // new_line('') // &
                      '        echo "Commit blocked - fix violations before committing"' // new_line('') // &
                      '        echo "Run: fpm run fortcov -- --validate-architecture --human-format"' // &
                      ' for details' // new_line('') // &
                      '        exit 1' // new_line('') // &
                      '        ;;' // new_line('') // &
                      '    *)' // new_line('') // &
                      '        echo "❌ Size validation failed with error"' // new_line('') // &
                      '        exit 1' // new_line('') // &
                      '        ;;' // new_line('') // &
                      'esac'
                      
    end subroutine generate_pre_commit_hook
    
    subroutine validate_pr_changes(base_branch, pr_branch, result)
        !! Validates size impact of PR changes vs base branch
        !! Provides targeted analysis of architectural impact
        character(len=*), intent(in) :: base_branch, pr_branch
        type(ci_enforcement_result_t), intent(out) :: result
        
        type(architectural_size_report_t) :: base_report, pr_report
        type(error_context_t) :: error_ctx
        type(size_enforcement_config_t) :: config
        
        ! Configure for PR validation
        config%base_directory = "."
        config%fail_on_violations = .true.
        config%fail_on_warnings = .false.
        config%generate_github_annotations = .true.
        config%output_format = "ci"
        config%verbose_output = .true.
        
        ! Get current PR state
        call validate_codebase_architecture(config%base_directory, pr_report, error_ctx)
        
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call handle_enforcement_error(error_ctx, result)
            return
        end if
        
        ! For now, use PR report as baseline (full implementation would diff branches)
        call determine_ci_exit_code(pr_report, config, result)
        call generate_enforcement_reports(pr_report, config, result)
        
        ! Add PR-specific messaging
        result%summary_message = "PR ARCHITECTURAL ANALYSIS: " // result%summary_message
        
    end subroutine validate_pr_changes

    ! ================ INTERNAL IMPLEMENTATION SUBROUTINES ================
    
    subroutine determine_ci_exit_code(size_report, config, result)
        !! Analyzes size report and determines appropriate CI exit code
        type(architectural_size_report_t), intent(in) :: size_report
        type(size_enforcement_config_t), intent(in) :: config
        type(ci_enforcement_result_t), intent(inout) :: result
        
        ! Count violations and warnings
        if (allocated(size_report%file_violations)) then
            result%violations_count = count_file_violations_by_severity( &
                size_report%file_violations, "VIOLATION")
            result%warnings_count = count_file_violations_by_severity( &
                size_report%file_violations, "WARNING") + &
                count_file_violations_by_severity(size_report%file_violations, &
                "CRITICAL")
        end if
        
        if (allocated(size_report%directory_violations)) then
            result%violations_count = result%violations_count + &
                count_directory_violations_by_severity( &
                    size_report%directory_violations, "VIOLATION")
            result%warnings_count = result%warnings_count + &
                count_directory_violations_by_severity( &
                    size_report%directory_violations, "WARNING")
        end if
        
        ! Determine exit code and blocking behavior
        if (result%violations_count > 0 .and. config%fail_on_violations) then
            result%exit_code = CI_FAILURE_VIOLATIONS
            result%should_block_merge = .true.
            result%summary_message = "BLOCKING: " // int_to_string(result%violations_count) // &
                                    " architectural violations must be fixed"
        else if (result%warnings_count > 0 .and. config%fail_on_warnings) then
            result%exit_code = CI_SUCCESS_WITH_WARNINGS  
            result%should_block_merge = .false.
            result%summary_message = "WARNING: " // int_to_string(result%warnings_count) // &
                                    " files approaching size limits"
        else if (result%warnings_count > 0) then
            ! CRITICAL FIX: When warnings exist but fail_on_warnings is false, return success
            result%exit_code = CI_SUCCESS
            result%should_block_merge = .false.
            result%summary_message = "PASSED WITH WARNINGS: " // &
                                    int_to_string(result%warnings_count) // " size warnings"
        else
            result%exit_code = CI_SUCCESS
            result%should_block_merge = .false.
            result%summary_message = "PASSED: All architectural size checks successful"
        end if
        
    end subroutine determine_ci_exit_code
    
    subroutine generate_enforcement_reports(size_report, config, result)
        !! Generates detailed reports for CI consumption
        type(architectural_size_report_t), intent(in) :: size_report
        type(size_enforcement_config_t), intent(in) :: config
        type(ci_enforcement_result_t), intent(inout) :: result
        
        character(len=:), allocatable :: report_text
        
        ! Generate main report
        call generate_size_report(size_report, config%output_format, report_text)
        result%detailed_report = report_text
        
        ! Generate remediation actions
        call generate_remediation_actions(size_report, result%remediation_actions)
        
        ! Add GitHub Actions annotations if requested
        if (config%generate_github_annotations) then
            call add_github_annotations(size_report, result%detailed_report)
        end if
        
    end subroutine generate_enforcement_reports
    
    subroutine generate_remediation_actions(size_report, remediation_text)
        !! Generates specific, actionable remediation guidance
        type(architectural_size_report_t), intent(in) :: size_report
        character(len=:), allocatable, intent(out) :: remediation_text
        
        integer :: i, violation_count
        character(len=:), allocatable :: actions_list
        
        remediation_text = "RECOMMENDED ACTIONS:" // new_line('')
        actions_list = ""
        violation_count = 0
        
        if (allocated(size_report%file_violations)) then
            do i = 1, size(size_report%file_violations)
                if (trim(size_report%file_violations(i)%severity_level) == "VIOLATION") then
                    violation_count = violation_count + 1
                    actions_list = actions_list // &
                        int_to_string(violation_count) // ". DECOMPOSE: " // &
                        size_report%file_violations(i)%filename // " (" // &
                        int_to_string(size_report%file_violations(i)%current_lines) // &
                        " lines → split into <500 line modules)" // new_line('')
                end if
            end do
        end if
        
        if (violation_count > 0) then
            remediation_text = remediation_text // actions_list // new_line('') // &
                              "ARCHITECTURAL GUIDANCE:" // new_line('') // &
                              "- Follow Single Responsibility Principle (SRP)" // new_line('') // &
                              "- Extract related functions into dedicated modules" // new_line('') // &
                              "- Use composition over large monolithic modules" // new_line('') // &
                              "- See DESIGN.md for decomposition patterns"
        else
            remediation_text = remediation_text // "No immediate actions required - " // &
                              "maintain current architectural discipline"
        end if
        
    end subroutine generate_remediation_actions
    
    subroutine add_github_annotations(size_report, report_text)
        !! Adds GitHub Actions annotations for in-PR display
        type(architectural_size_report_t), intent(in) :: size_report
        character(len=:), allocatable, intent(inout) :: report_text
        
        integer :: i
        character(len=:), allocatable :: annotations
        
        annotations = ""
        
        if (allocated(size_report%file_violations)) then
            do i = 1, size(size_report%file_violations)
                if (trim(size_report%file_violations(i)%severity_level) /= "") then
                    annotations = annotations // &
                        "::warning file=" // size_report%file_violations(i)%filename // &
                        ",line=1::SIZE " // size_report%file_violations(i)%severity_level // &
                        " - " // int_to_string(size_report%file_violations(i)%current_lines) // &
                        "/" // int_to_string(size_report%file_violations(i)%target_limit) // &
                        " lines - " // size_report%file_violations(i)%remediation_hint // &
                        new_line('')
                end if
            end do
        end if
        
        if (len(annotations) > 0) then
            report_text = annotations // new_line('') // report_text
        end if
        
    end subroutine add_github_annotations
    
    subroutine handle_enforcement_error(error_ctx, result)
        !! Handles errors during enforcement and sets appropriate CI codes
        type(error_context_t), intent(in) :: error_ctx
        type(ci_enforcement_result_t), intent(inout) :: result
        
        result%exit_code = CI_FAILURE_ERROR
        result%should_block_merge = .true.
        result%summary_message = "SIZE VALIDATION ERROR: " // trim(error_ctx%message)
        result%detailed_report = "Architectural size validation failed due to system error. " // &
                                "Check CI logs for details."
        result%remediation_actions = "Fix system/environment issue preventing size validation"
        
    end subroutine handle_enforcement_error
    
    function count_severity_level(violations, severity) result(count)
        !! Counts file violations of specific severity level
        type(file_size_violation_t), intent(in) :: violations(:)
        character(len=*), intent(in) :: severity
        integer :: count
        
        integer :: i
        count = 0
        do i = 1, size(violations)
            if (trim(violations(i)%severity_level) == trim(severity)) then
                count = count + 1
            end if
        end do
        
    end function count_severity_level
    
    function count_directory_severity_level(violations, severity) result(count)
        !! Counts directory violations of specific severity level
        type(directory_size_violation_t), intent(in) :: violations(:)
        character(len=*), intent(in) :: severity
        integer :: count
        
        integer :: i
        count = 0
        do i = 1, size(violations)
            if (trim(violations(i)%severity_level) == trim(severity)) then
                count = count + 1
            end if
        end do
        
    end function count_directory_severity_level

end module size_enforcement_core