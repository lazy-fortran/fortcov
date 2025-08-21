module json_parser_utilities
    use string_utils
    implicit none
    private
    
    ! Public interface for JSON parsing utilities
    public :: extract_coverage_rates_from_json
    public :: generate_packages_from_json
    public :: generate_classes_from_json_files
    public :: generate_lines_from_json_file
    public :: extract_json_real_value
    public :: extract_json_int_value
    public :: extract_json_string_value
    
contains

    ! Simplified JSON parsing functions for direct conversion
    
    subroutine extract_coverage_rates_from_json(json_content, line_rate, branch_rate, &
                                               covered_lines, total_lines, success)
        character(len=*), intent(in) :: json_content
        real, intent(out) :: line_rate, branch_rate
        integer, intent(out) :: covered_lines, total_lines
        logical, intent(out) :: success
        
        integer :: summary_start, summary_end
        character(len=:), allocatable :: summary_section
        
        success = .false.
        line_rate = 0.0
        branch_rate = 0.0
        covered_lines = 0
        total_lines = 0
        
        ! Find summary section
        summary_start = index(json_content, '"summary":')
        if (summary_start == 0) return
        
        summary_start = index(json_content(summary_start:), '{')
        if (summary_start == 0) return
        summary_start = summary_start + summary_start - 1
        
        summary_end = index(json_content(summary_start:), '}')
        if (summary_end == 0) return
        summary_end = summary_start + summary_end - 1
        
        summary_section = json_content(summary_start:summary_end)
        
        ! Extract values from summary
        call extract_json_real_value(summary_section, 'line_coverage', line_rate, success)
        if (.not. success) return
        
        call extract_json_int_value(summary_section, 'covered_lines', covered_lines, success)
        if (.not. success) return
        
        call extract_json_int_value(summary_section, 'total_lines', total_lines, success)
        if (.not. success) return
        
        ! Branch rate same as line rate for now
        branch_rate = line_rate
        
        ! Convert percentage to rate
        line_rate = line_rate / 100.0
        branch_rate = branch_rate / 100.0
        
        success = .true.
        
    end subroutine extract_coverage_rates_from_json
    
    function generate_packages_from_json(json_content) result(packages_xml)
        character(len=*), intent(in) :: json_content
        character(len=:), allocatable :: packages_xml
        
        integer :: files_start, files_end
        character(len=:), allocatable :: files_section
        
        packages_xml = '<packages>' // new_line('') // &
                      '  <package name="fortcov-coverage">' // new_line('') // &
                      '    <classes>' // new_line('')
        
        ! Find files array
        files_start = index(json_content, '"files":')
        if (files_start > 0) then
            files_start = index(json_content(files_start:), '[')
            if (files_start > 0) then
                files_start = files_start + files_start - 1
                files_end = index(json_content(files_start:), ']')
                if (files_end > 0) then
                    files_end = files_start + files_end - 1
                    files_section = json_content(files_start:files_end)
                    
                    ! Generate classes from files (simplified)
                    packages_xml = packages_xml // generate_classes_from_json_files(files_section)
                end if
            end if
        end if
        
        packages_xml = packages_xml // &
                      '    </classes>' // new_line('') // &
                      '  </package>' // new_line('') // &
                      '</packages>'
        
    end function generate_packages_from_json
    
    function generate_classes_from_json_files(files_json) result(classes_xml)
        character(len=*), intent(in) :: files_json
        character(len=:), allocatable :: classes_xml
        
        integer :: pos, file_start, file_end
        character(len=:), allocatable :: filename, lines_xml
        
        classes_xml = ''
        pos = 1
        
        ! Find each file object
        do
            file_start = index(files_json(pos:), '{"filename":')
            if (file_start == 0) exit
            file_start = file_start + pos - 1
            
            file_end = index(files_json(file_start:), '}')
            if (file_end == 0) exit
            file_end = file_start + file_end - 1
            
            ! Extract filename
            call extract_json_string_value(files_json(file_start:file_end), 'filename', filename)
            
            ! Generate lines XML (simplified)
            call generate_lines_from_json_file(files_json(file_start:file_end), lines_xml)
            
            ! Add class
            classes_xml = classes_xml // &
                         '      <class filename="' // trim(filename) // &
                         '" name="' // get_base_name(filename) // &
                         '" line-rate="1.0" branch-rate="1.0" complexity="0.0">' // &
                         new_line('') // &
                         '        <lines>' // new_line('') // &
                         lines_xml // &
                         '        </lines>' // new_line('') // &
                         '      </class>' // new_line('')
            
            pos = file_end + 1
            if (pos > len(files_json)) exit
        end do
        
    end function generate_classes_from_json_files
    
    subroutine generate_lines_from_json_file(file_json, lines_xml)
        character(len=*), intent(in) :: file_json
        character(len=:), allocatable, intent(out) :: lines_xml
        
        integer :: lines_start, lines_end, pos, line_start, line_end
        character(len=:), allocatable :: lines_section
        integer :: line_number, execution_count
        logical :: success
        
        lines_xml = ''
        
        ! Find lines array
        lines_start = index(file_json, '"lines":')
        if (lines_start == 0) return
        
        lines_start = index(file_json(lines_start:), '[')
        if (lines_start == 0) return
        lines_start = lines_start + lines_start - 1
        
        lines_end = index(file_json(lines_start:), ']')
        if (lines_end == 0) return
        lines_end = lines_start + lines_end - 1
        
        lines_section = file_json(lines_start:lines_end)
        pos = 1
        
        ! Parse each line object
        do
            line_start = index(lines_section(pos:), '{"line_number":')
            if (line_start == 0) exit
            line_start = line_start + pos - 1
            
            line_end = index(lines_section(line_start:), '}')
            if (line_end == 0) exit
            line_end = line_start + line_end - 1
            
            ! Extract line data
            call extract_json_int_value(lines_section(line_start:line_end), 'line_number', &
                                       line_number, success)
            if (.not. success) then
                pos = line_end + 1
                cycle
            end if
            
            call extract_json_int_value(lines_section(line_start:line_end), 'execution_count', &
                                       execution_count, success)
            if (.not. success) then
                pos = line_end + 1
                cycle
            end if
            
            ! Add line XML
            lines_xml = lines_xml // &
                       '          <line number="' // int_to_string(line_number) // &
                       '" hits="' // int_to_string(execution_count) // &
                       '" branch="false"/>' // new_line('')
            
            pos = line_end + 1
            if (pos > len(lines_section)) exit
        end do
        
    end subroutine generate_lines_from_json_file
    
    ! JSON parsing helper functions
    
    subroutine extract_json_real_value(json_text, key, value, success)
        character(len=*), intent(in) :: json_text, key
        real, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: key_start, value_start, value_end, iostat_var
        character(len=20) :: value_str
        
        success = .false.
        value = 0.0
        
        key_start = index(json_text, '"' // trim(key) // '":')
        if (key_start == 0) return
        
        value_start = key_start + len(trim(key)) + 3
        value_end = index(json_text(value_start:), ',')
        if (value_end == 0) then
            value_end = index(json_text(value_start:), '}')
            if (value_end == 0) return
        end if
        value_end = value_start + value_end - 2
        
        if (value_end <= value_start) return
        
        value_str = adjustl(json_text(value_start:value_end))
        read(value_str, *, iostat=iostat_var) value
        
        success = (iostat_var == 0)
        
    end subroutine extract_json_real_value
    
    subroutine extract_json_int_value(json_text, key, value, success)
        character(len=*), intent(in) :: json_text, key
        integer, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: key_start, value_start, value_end, iostat_var
        character(len=20) :: value_str
        
        success = .false.
        value = 0
        
        key_start = index(json_text, '"' // trim(key) // '":')
        if (key_start == 0) return
        
        value_start = key_start + len(trim(key)) + 3
        value_end = index(json_text(value_start:), ',')
        if (value_end == 0) then
            value_end = index(json_text(value_start:), '}')
            if (value_end == 0) return
        end if
        value_end = value_start + value_end - 2
        
        if (value_end <= value_start) return
        
        value_str = adjustl(json_text(value_start:value_end))
        read(value_str, *, iostat=iostat_var) value
        
        success = (iostat_var == 0)
        
    end subroutine extract_json_int_value
    
    subroutine extract_json_string_value(json_text, key, value)
        character(len=*), intent(in) :: json_text, key
        character(len=:), allocatable, intent(out) :: value
        
        integer :: key_start, value_start, value_end
        
        value = ''
        
        key_start = index(json_text, '"' // trim(key) // '":')
        if (key_start == 0) return
        
        value_start = index(json_text(key_start:), '"')
        if (value_start == 0) return
        value_start = key_start + value_start
        
        value_end = index(json_text(value_start:), '"')
        if (value_end == 0) return
        value_end = value_start + value_end - 2
        
        if (value_end < value_start) return
        
        value = json_text(value_start:value_end)
        
    end subroutine extract_json_string_value

    ! Utility helper functions needed by JSON parsing
    function get_base_name(filename) result(base_name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: base_name
        integer :: last_slash, last_dot
        
        last_slash = index(filename, '/', back=.true.)
        last_dot = index(filename, '.', back=.true.)
        
        if (last_slash > 0 .and. last_dot > last_slash) then
            base_name = filename(last_slash+1:last_dot-1)
        else if (last_slash > 0) then
            base_name = filename(last_slash+1:)
        else if (last_dot > 0) then
            base_name = filename(1:last_dot-1)
        else
            base_name = filename
        end if
        
    end function get_base_name
    
    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=20) :: temp_str
        character(len=:), allocatable :: str
        
        write(temp_str, '(I0)') value
        str = trim(adjustl(temp_str))
        
    end function int_to_string

end module json_parser_utilities