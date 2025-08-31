module xml_reporter
    !! XML reporter implementation extracted from coverage_reporter_impl
    !! 
    !! Focused solely on XML format report generation.
    !! Provides clean separation of XML formatting logic.
    use coverage_types
    use coverage_reporter
    use coverage_stats_core, only: stats_t => coverage_stats_t
    use string_utils, only: int_to_string, format_percentage
    implicit none
    private
    
    public :: xml_reporter_t
    
    type, extends(coverage_reporter_t) :: xml_reporter_t
    contains
        procedure :: generate_report => xml_generate_report
        procedure :: get_format_name => xml_get_format_name
        procedure :: supports_diff => xml_supports_diff
    end type xml_reporter_t
    
contains
    
    subroutine xml_generate_report(this, coverage_data, output_path, &
                                 success, error_message, &
                                 diff_data, threshold)
        use file_utilities, only: write_text_file
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
    
end module xml_reporter