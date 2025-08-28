module coverage_reporter_impl
    !! Consolidated Coverage Reporter Implementations
    !!
    !! This module consolidates all concrete reporter implementations
    !! Combines: coverage_reporter_text, coverage_reporter_md, coverage_reporter_json,
    !!           coverage_reporter_html, coverage_reporter_xml, coverage_reporter_utils
    
    use coverage_types
    use coverage_reporter
    use coverage_stats_core, only: stats_t => coverage_stats_t
    use string_utils, only: int_to_string, format_percentage
    implicit none
    private
    
    ! Public exports - all reporter types
    public :: text_reporter_t
    public :: markdown_reporter_t
    public :: json_reporter_t
    public :: html_reporter_t
    public :: xml_reporter_t
    
    ! ============================================================================
    ! Text Reporter Implementation
    ! ============================================================================
    
    type, extends(coverage_reporter_t) :: text_reporter_t
    contains
        procedure :: generate_report => text_generate_report
        procedure :: get_format_name => text_get_format_name
        procedure :: supports_diff => text_supports_diff
    end type text_reporter_t
    
    ! ============================================================================
    ! Markdown Reporter Implementation
    ! ============================================================================
    
    type, extends(coverage_reporter_t) :: markdown_reporter_t
    contains
        procedure :: generate_report => markdown_generate_report
        procedure :: get_format_name => markdown_get_format_name
        procedure :: supports_diff => markdown_supports_diff
    end type markdown_reporter_t
    
    ! ============================================================================
    ! JSON Reporter Implementation
    ! ============================================================================
    
    type, extends(coverage_reporter_t) :: json_reporter_t
    contains
        procedure :: generate_report => json_generate_report
        procedure :: get_format_name => json_get_format_name
        procedure :: supports_diff => json_supports_diff
    end type json_reporter_t
    
    ! ============================================================================
    ! HTML Reporter Implementation
    ! ============================================================================
    
    type, extends(coverage_reporter_t) :: html_reporter_t
    contains
        procedure :: generate_report => html_generate_report
        procedure :: get_format_name => html_get_format_name
        procedure :: supports_diff => html_supports_diff
    end type html_reporter_t
    
    ! ============================================================================
    ! XML Reporter Implementation
    ! ============================================================================
    
    type, extends(coverage_reporter_t) :: xml_reporter_t
    contains
        procedure :: generate_report => xml_generate_report
        procedure :: get_format_name => xml_get_format_name
        procedure :: supports_diff => xml_supports_diff
    end type xml_reporter_t
    
contains
    
    ! Simplified stub implementations for now to get building
    
    subroutine text_generate_report(this, coverage_data, output_path, &
                                  success, error_message, &
                                  diff_data, threshold)
        use file_utils_core, only: write_text_file
        use coverage_stats_core, only: calculate_line_coverage, coverage_stats_t
        class(text_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: text_content
        type(coverage_stats_t) :: stats
        logical :: write_error
        integer :: i
        
        ! Calculate statistics
        stats = calculate_line_coverage(coverage_data)
        
        ! Generate text report content
        text_content = "Coverage Report" // new_line('a') // &
                      "===============" // new_line('a') // new_line('a')
        
        ! Add summary
        text_content = text_content // "Overall Coverage: " // format_percentage(stats%percentage, 2) // new_line('a')
        text_content = text_content // "Lines Covered: " // int_to_string(stats%covered_count) // &
                      " of " // int_to_string(stats%total_count) // new_line('a')
        if (allocated(coverage_data%files)) then
            text_content = text_content // "Files Analyzed: " // int_to_string(size(coverage_data%files)) // &
                          new_line('a') // new_line('a')
        else
            text_content = text_content // "Files Analyzed: 0" // new_line('a') // new_line('a')
        end if
        
        ! Add file details
        text_content = text_content // "File Details:" // new_line('a')
        text_content = text_content // "--------------" // new_line('a')
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                text_content = text_content // trim(coverage_data%files(i)%filename) // ": " // &
                              format_percentage(coverage_data%files(i)%line_coverage, 2) // new_line('a')
            end do
        end if
        
        ! Write to file
        call write_text_file(output_path, text_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write text file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
        
        ! Suppress unused warnings
        if (present(diff_data)) continue
        if (present(threshold)) continue
    end subroutine text_generate_report
    
    function text_get_format_name(this) result(format_name)
        class(text_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "text"
    end function text_get_format_name
    
    function text_supports_diff(this) result(supported)
        class(text_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function text_supports_diff
    
    ! Markdown reporter stubs
    subroutine markdown_generate_report(this, coverage_data, output_path, &
                                      success, error_message, &
                                      diff_data, threshold)
        use markdown_reporter, only: generate_markdown_report, markdown_report_options_t
        use file_utils_core, only: write_text_file
        class(markdown_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: markdown_content
        type(markdown_report_options_t) :: options
        logical :: write_error
        
        ! Initialize markdown options with defaults
        call options%init()
        
        ! Generate markdown content
        markdown_content = generate_markdown_report(coverage_data, options)
        
        if (.not. allocated(markdown_content)) then
            success = .false.
            error_message = "Failed to generate markdown content"
            return
        end if
        
        ! Write markdown content to file
        call write_text_file(output_path, markdown_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write markdown file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
    end subroutine markdown_generate_report
    
    function markdown_get_format_name(this) result(format_name)
        class(markdown_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "markdown"
    end function markdown_get_format_name
    
    function markdown_supports_diff(this) result(supported)
        class(markdown_reporter_t), intent(in) :: this
        logical :: supported
        supported = .true.
    end function markdown_supports_diff
    
    ! JSON reporter implementation
    subroutine json_generate_report(this, coverage_data, output_path, &
                                  success, error_message, &
                                  diff_data, threshold)
        use json_io, only: export_coverage_to_json
        use file_utils_core, only: write_text_file
        class(json_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: json_content
        logical :: write_error
        
        ! Convert coverage data to JSON
        call export_coverage_to_json(coverage_data, json_content)
        
        if (.not. allocated(json_content)) then
            success = .false.
            error_message = "Failed to generate JSON content"
            return
        end if
        
        ! Write JSON content to file
        call write_text_file(output_path, json_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write JSON file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
        
        ! Suppress unused parameter warnings
        if (present(diff_data)) continue
        if (present(threshold)) continue
        
    end subroutine json_generate_report
    
    function json_get_format_name(this) result(format_name)
        class(json_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "json"
    end function json_get_format_name
    
    function json_supports_diff(this) result(supported)
        class(json_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function json_supports_diff
    
    ! HTML reporter implementation
    subroutine html_generate_report(this, coverage_data, output_path, &
                                  success, error_message, &
                                  diff_data, threshold)
        use file_utils_core, only: write_text_file
        use coverage_stats_core, only: calculate_line_coverage, coverage_stats_t
        class(html_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: html_content
        type(coverage_stats_t) :: stats
        logical :: write_error
        integer :: i
        
        ! Calculate statistics
        stats = calculate_line_coverage(coverage_data)
        
        ! Generate HTML content
        html_content = "<!DOCTYPE html>" // new_line('a') // &
            "<html><head><title>Coverage Report</title>" // new_line('a') // &
            "<style>" // new_line('a') // &
            "body { font-family: Arial, sans-serif; margin: 20px; }" // new_line('a') // &
            ".summary { background: #f0f0f0; padding: 10px; border-radius: 5px; }" // new_line('a') // &
            ".file { margin: 10px 0; padding: 5px; border-left: 3px solid #007acc; }" // new_line('a') // &
            "</style></head><body>" // new_line('a') // &
            "<h1>Coverage Report</h1>" // new_line('a') // &
            "<div class='summary'>" // new_line('a')
        
        html_content = html_content // "<p><strong>Overall Coverage:</strong> " // &
                      format_percentage(stats%percentage, 2) // "</p>" // new_line('a')
        html_content = html_content // "<p><strong>Lines Covered:</strong> " // &
                      int_to_string(stats%covered_count) // " of " // &
                      int_to_string(stats%total_count) // "</p>" // new_line('a')
        if (allocated(coverage_data%files)) then
            html_content = html_content // "<p><strong>Files Analyzed:</strong> " // &
                          int_to_string(size(coverage_data%files)) // "</p>" // new_line('a')
        else
            html_content = html_content // "<p><strong>Files Analyzed:</strong> 0</p>" // new_line('a')
        end if
        
        html_content = html_content // "</div>" // new_line('a') // &
            "<h2>File Details</h2>" // new_line('a')
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                html_content = html_content // "<div class='file'><strong>" // &
                              trim(coverage_data%files(i)%filename) // ":</strong> " // &
                              format_percentage(coverage_data%files(i)%line_coverage, 2) // &
                              "</div>" // new_line('a')
            end do
        end if
        
        html_content = html_content // "</body></html>"
        
        ! Write to file
        call write_text_file(output_path, html_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write HTML file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
        
        ! Suppress unused warnings
        if (present(diff_data)) continue
        if (present(threshold)) continue
    end subroutine html_generate_report
    
    function html_get_format_name(this) result(format_name)
        class(html_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "html"
    end function html_get_format_name
    
    function html_supports_diff(this) result(supported)
        class(html_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function html_supports_diff
    
    ! XML reporter implementation
    subroutine xml_generate_report(this, coverage_data, output_path, &
                                 success, error_message, &
                                 diff_data, threshold)
        use file_utils_core, only: write_text_file
        use coverage_stats_core, only: calculate_line_coverage, coverage_stats_t
        class(xml_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: xml_content
        type(coverage_stats_t) :: stats
        logical :: write_error
        integer :: i
        
        ! Calculate statistics
        stats = calculate_line_coverage(coverage_data)
        
        ! Generate XML content
        xml_content = "<?xml version='1.0' encoding='UTF-8'?>" // new_line('a') // &
            "<coverage version='1.0'>" // new_line('a') // &
            "  <summary>" // new_line('a')
        
        xml_content = xml_content // "    <overall_coverage>" // &
                     format_percentage(stats%percentage, 2) // "</overall_coverage>" // new_line('a')
        xml_content = xml_content // "    <lines_covered>" // &
                     int_to_string(stats%covered_count) // "</lines_covered>" // new_line('a')
        xml_content = xml_content // "    <total_lines>" // &
                     int_to_string(stats%total_count) // "</total_lines>" // new_line('a')
        if (allocated(coverage_data%files)) then
            xml_content = xml_content // "    <files_analyzed>" // &
                         int_to_string(size(coverage_data%files)) // "</files_analyzed>" // new_line('a')
        else
            xml_content = xml_content // "    <files_analyzed>0</files_analyzed>" // new_line('a')
        end if
        
        xml_content = xml_content // "  </summary>" // new_line('a') // &
            "  <files>" // new_line('a')
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                xml_content = xml_content // "    <file>" // new_line('a') // &
                             "      <name>" // trim(coverage_data%files(i)%filename) // "</name>" // new_line('a') // &
                             "      <coverage>" // format_percentage(coverage_data%files(i)%line_coverage, 2) // &
                             "</coverage>" // new_line('a') // &
                             "      <covered_lines>" // int_to_string(coverage_data%files(i)%covered_lines) // &
                             "</covered_lines>" // new_line('a') // &
                             "      <total_lines>" // int_to_string(coverage_data%files(i)%total_lines) // &
                             "</total_lines>" // new_line('a') // &
                             "    </file>" // new_line('a')
            end do
        end if
        
        xml_content = xml_content // "  </files>" // new_line('a') // &
            "</coverage>"
        
        ! Write to file
        call write_text_file(output_path, xml_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write XML file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
        
        ! Suppress unused warnings
        if (present(diff_data)) continue
        if (present(threshold)) continue
    end subroutine xml_generate_report
    
    function xml_get_format_name(this) result(format_name)
        class(xml_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "xml"
    end function xml_get_format_name
    
    function xml_supports_diff(this) result(supported)
        class(xml_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function xml_supports_diff
    
end module coverage_reporter_impl