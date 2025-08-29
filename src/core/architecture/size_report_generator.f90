module size_report_generator
    !! Size Violation Report Generation
    !! 
    !! Generates comprehensive reports in various formats (human, CI, JSON).
    !! Single responsibility: report formatting and generation.
    use size_violation_analyzer, only: file_size_violation_t, directory_size_violation_t
    use string_utils, only: int_to_string
    implicit none
    private
    
    ! Public interface for report generation
    public :: generate_comprehensive_size_report
    public :: generate_report_in_format
    public :: architectural_size_report_t
    
    ! Comprehensive architectural size report
    type :: architectural_size_report_t
        logical :: has_violations
        logical :: has_warnings
        integer :: total_files_scanned
        integer :: total_directories_scanned
        type(file_size_violation_t), allocatable :: file_violations(:)
        type(directory_size_violation_t), allocatable :: directory_violations(:)
        character(len=:), allocatable :: summary_message
        character(len=:), allocatable :: ci_exit_recommendation
    end type architectural_size_report_t

contains

    subroutine generate_comprehensive_size_report(file_violations, directory_violations, &
                                                files_scanned, directories_scanned, report)
        !! Generates comprehensive report from violation data
        type(file_size_violation_t), intent(in) :: file_violations(:)
        type(directory_size_violation_t), intent(in) :: directory_violations(:)
        integer, intent(in) :: files_scanned, directories_scanned
        type(architectural_size_report_t), intent(out) :: report
        
        ! Initialize report
        report%total_files_scanned = files_scanned
        report%total_directories_scanned = directories_scanned
        
        ! Copy violation data
        if (size(file_violations) > 0) then
            allocate(report%file_violations(size(file_violations)))
            report%file_violations = file_violations
        else
            allocate(report%file_violations(0))
        end if
        
        if (size(directory_violations) > 0) then
            allocate(report%directory_violations(size(directory_violations)))
            report%directory_violations = directory_violations
        else
            allocate(report%directory_violations(0))
        end if
        
        ! Analyze violations and generate summary
        call analyze_violations_and_generate_summary(report)
        
    end subroutine generate_comprehensive_size_report
    
    subroutine generate_report_in_format(report, output_format, report_text)
        !! Generates report in specified format
        type(architectural_size_report_t), intent(in) :: report
        character(len=*), intent(in) :: output_format  ! "human", "ci", "json"
        character(len=:), allocatable, intent(out) :: report_text
        
        if (trim(output_format) == "ci") then
            call generate_ci_report(report, report_text)
        else if (trim(output_format) == "json") then
            call generate_json_report(report, report_text)
        else if (trim(output_format) == "human") then
            call generate_human_report(report, report_text)
        else
            call generate_human_report(report, report_text)  ! Default
        end if
        
    end subroutine generate_report_in_format

    ! ================ INTERNAL REPORT GENERATION ================
    
    subroutine analyze_violations_and_generate_summary(report)
        !! Analyzes all violations and generates executive summary
        type(architectural_size_report_t), intent(inout) :: report
        
        integer :: file_violations, dir_violations, warnings
        character(len=:), allocatable :: summary
        
        ! Count violations by severity
        file_violations = count_file_violations_by_severity(report%file_violations, "VIOLATION")
        dir_violations = count_directory_violations_by_severity( &
                           report%directory_violations, "VIOLATION")
        warnings = count_file_violations_by_severity(report%file_violations, "WARNING") + &
                  count_file_violations_by_severity(report%file_violations, "CRITICAL")
        
        report%has_violations = (file_violations > 0 .or. dir_violations > 0)
        report%has_warnings = (warnings > 0)
        
        ! Generate summary
        summary = "ARCHITECTURAL SIZE ANALYSIS: " // &
                 int_to_string(report%total_files_scanned) // " files, " // &
                 int_to_string(report%total_directories_scanned) // " directories scanned. "
        
        if (report%has_violations) then
            summary = summary // "VIOLATIONS FOUND: " // &
                     int_to_string(file_violations + dir_violations) // " critical. "
            report%ci_exit_recommendation = "EXIT_FAILURE"
        else if (report%has_warnings) then
            summary = summary // "WARNINGS FOUND: " // &
                     int_to_string(warnings) // " approaching limits. "
            report%ci_exit_recommendation = "EXIT_SUCCESS_WITH_WARNINGS"
        else
            summary = summary // "ALL CHECKS PASSED - Architectural compliance maintained."
            report%ci_exit_recommendation = "EXIT_SUCCESS"
        end if
        
        report%summary_message = summary
        
    end subroutine analyze_violations_and_generate_summary
    
    subroutine generate_ci_report(report, report_text)
        !! Generates CI-friendly report format for automated processing
        type(architectural_size_report_t), intent(in) :: report
        character(len=:), allocatable, intent(out) :: report_text
        
        character(len=:), allocatable :: violations_section
        integer :: i
        
        report_text = "::group::Architectural Size Analysis" // new_line('') // &
                     report%summary_message // new_line('') // new_line('')
        
        ! File violations section
        if (allocated(report%file_violations) .and. size(report%file_violations) > 0) then
            violations_section = "FILE SIZE VIOLATIONS:" // new_line('')
            do i = 1, size(report%file_violations)
                if (trim(report%file_violations(i)%severity_level) /= "") then
                    violations_section = violations_section // &
                        "::error file=" // report%file_violations(i)%filename // &
                        "::" // report%file_violations(i)%severity_level // &
                        " - " // int_to_string(report%file_violations(i)%current_lines) // &
                        "/" // int_to_string(report%file_violations(i)%target_limit) // &
                        " lines (" // report%file_violations(i)%remediation_hint // &
                        ")" // new_line('')
                end if
            end do
            report_text = report_text // violations_section // new_line('')
        end if
        
        report_text = report_text // "::endgroup::"
        
    end subroutine generate_ci_report
    
    subroutine generate_human_report(report, report_text)
        !! Generates human-readable report format for manual review  
        type(architectural_size_report_t), intent(in) :: report
        character(len=:), allocatable, intent(out) :: report_text
        
        report_text = "=== ARCHITECTURAL SIZE ANALYSIS ===" // new_line('') // &
                     report%summary_message // new_line('') // new_line('')
        
        ! Add detailed violations if present
        if (report%has_violations .or. report%has_warnings) then
            report_text = report_text // "DETAILED FINDINGS:" // new_line('')
            call append_detailed_violations(report, report_text)
        end if
        
    end subroutine generate_human_report
    
    subroutine append_detailed_violations(report, report_text)
        !! Appends detailed violation information to report
        type(architectural_size_report_t), intent(in) :: report
        character(len=:), allocatable, intent(inout) :: report_text
        
        integer :: i
        
        if (allocated(report%file_violations)) then
            do i = 1, size(report%file_violations) 
                if (trim(report%file_violations(i)%severity_level) /= "") then
                    report_text = report_text // &
                        "- " // report%file_violations(i)%filename // ": " // &
                        int_to_string(report%file_violations(i)%current_lines) // &
                        " lines [" // report%file_violations(i)%severity_level // "] " // &
                        report%file_violations(i)%remediation_hint // new_line('')
                end if
            end do
        end if
        
    end subroutine append_detailed_violations
    
    subroutine generate_json_report(report, report_text)
        !! Generates JSON format report for machine consumption
        type(architectural_size_report_t), intent(in) :: report
        character(len=:), allocatable, intent(out) :: report_text
        
        character(len=:), allocatable :: violations_json
        integer :: i
        logical :: first_violation
        
        ! Start JSON object
        report_text = '{' // new_line('') // &
                     '  "summary": "' // escape_json_string(report%summary_message) // '",' // new_line('') // &
                     '  "has_violations": ' // logical_to_json(report%has_violations) // ',' // new_line('') // &
                     '  "has_warnings": ' // logical_to_json(report%has_warnings) // ',' // new_line('') // &
                     '  "total_files_scanned": ' // int_to_string(report%total_files_scanned) // ',' // new_line('') // &
                     '  "total_directories_scanned": ' // &
                     int_to_string(report%total_directories_scanned) // ',' // new_line('') // &
                     '  "ci_exit_recommendation": "' // report%ci_exit_recommendation // '"'
        
        ! Add file violations array
        if (allocated(report%file_violations) .and. size(report%file_violations) > 0) then
            report_text = report_text // ',' // new_line('') // '  "file_violations": ['
            first_violation = .true.
            do i = 1, size(report%file_violations)
                if (trim(report%file_violations(i)%severity_level) /= "") then
                    if (.not. first_violation) then
                        report_text = report_text // ','
                    end if
                    report_text = report_text // new_line('') // '    {' // new_line('') // &
                        '      "filename": "' // escape_json_string(report%file_violations(i)%filename) // '",' // new_line('') // &
                        '      "current_lines": ' // int_to_string(report%file_violations(i)%current_lines) // ',' // new_line('') // &
                        '      "target_limit": ' // int_to_string(report%file_violations(i)%target_limit) // ',' // new_line('') // &
                        '      "severity_level": "' // trim(report%file_violations(i)%severity_level) // '",' // new_line('') // &
                        '      "remediation_hint": "' // escape_json_string(report%file_violations(i)%remediation_hint) // '"' // new_line('') // &
                        '    }'
                    first_violation = .false.
                end if
            end do
            report_text = report_text // new_line('') // '  ]'
        else
            report_text = report_text // ',' // new_line('') // '  "file_violations": []'
        end if
        
        ! Add directory violations array
        if (allocated(report%directory_violations) .and. size(report%directory_violations) > 0) then
            report_text = report_text // ',' // new_line('') // '  "directory_violations": ['
            first_violation = .true.
            do i = 1, size(report%directory_violations)
                if (trim(report%directory_violations(i)%severity_level) /= "") then
                    if (.not. first_violation) then
                        report_text = report_text // ','
                    end if
                    report_text = report_text // new_line('') // '    {' // new_line('') // &
                        '      "directory": "' // escape_json_string(report%directory_violations(i)%directory_path) // '",' // new_line('') // &
                        '      "current_items": ' // int_to_string(report%directory_violations(i)%current_items) // ',' // new_line('') // &
                        '      "soft_limit": ' // int_to_string(report%directory_violations(i)%soft_limit) // ',' // new_line('') // &
                        '      "hard_limit": ' // int_to_string(report%directory_violations(i)%hard_limit) // ',' // new_line('') // &
                        '      "severity_level": "' // trim(report%directory_violations(i)%severity_level) // '",' // new_line('') // &
                        '      "remediation_hint": "' // escape_json_string(report%directory_violations(i)%remediation_hint) // '"' // new_line('') // &
                        '    }'
                    first_violation = .false.
                end if
            end do
            report_text = report_text // new_line('') // '  ]'
        else
            report_text = report_text // ',' // new_line('') // '  "directory_violations": []'
        end if
        
        ! Close JSON object
        report_text = report_text // new_line('') // '}' // new_line('')
        
    end subroutine generate_json_report
    
    function escape_json_string(input_str) result(escaped)
        !! Escapes special characters in JSON strings
        character(len=*), intent(in) :: input_str
        character(len=:), allocatable :: escaped
        
        integer :: i, j
        character(len=1) :: ch
        character(len=:), allocatable :: temp
        
        ! Allocate maximum possible size (each char could become 6 chars \uXXXX)
        allocate(character(len=len(input_str)*6) :: temp)
        
        j = 0
        do i = 1, len_trim(input_str)
            ch = input_str(i:i)
            select case (ch)
            case ('"')
                j = j + 1
                temp(j:j+1) = '\"'
                j = j + 1
            case ('\\')
                j = j + 1  
                temp(j:j+1) = '\\\\'
                j = j + 1
            case (achar(8))  ! Backspace
                j = j + 1
                temp(j:j+1) = '\\b'
                j = j + 1
            case (achar(12))  ! Form feed
                j = j + 1
                temp(j:j+1) = '\\f'
                j = j + 1
            case (achar(10))  ! Newline
                j = j + 1
                temp(j:j+1) = '\\n'
                j = j + 1
            case (achar(13))  ! Carriage return
                j = j + 1
                temp(j:j+1) = '\\r'
                j = j + 1
            case (achar(9))  ! Tab
                j = j + 1
                temp(j:j+1) = '\\t'
                j = j + 1
            case default
                j = j + 1
                temp(j:j) = ch
            end select
        end do
        
        escaped = temp(1:j)
        
    end function escape_json_string
    
    function logical_to_json(val) result(json_str)
        !! Converts logical value to JSON boolean string
        logical, intent(in) :: val
        character(len=:), allocatable :: json_str
        
        if (val) then
            json_str = "true"
        else
            json_str = "false"
        end if
        
    end function logical_to_json
    
    function count_file_violations_by_severity(violations, severity) result(count)
        !! Counts file violations matching specific severity level
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
        
    end function count_file_violations_by_severity
    
    function count_directory_violations_by_severity(violations, severity) result(count)
        !! Counts directory violations matching specific severity level  
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
        
    end function count_directory_violations_by_severity

end module size_report_generator