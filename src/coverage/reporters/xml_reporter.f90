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
        ! Optimized buffered builder variables (avoid O(n^2) concatenation)
        integer :: total_len, pos
        character(len=:), allocatable :: buffer
        character(len=:), allocatable :: pct_str, files_analyzed_str
        character(len=:), allocatable :: fname, cov_str, covered_str, total_str
        
        ! Calculate statistics
        stats = calculate_line_coverage(coverage_data)
        
        ! Pre-compute dynamic strings
        pct_str = format_percentage(stats%percentage, 2)
        covered_str = int_to_string(stats%covered_count)
        total_str = int_to_string(stats%total_count)
        if (allocated(coverage_data%files)) then
            files_analyzed_str = int_to_string(size(coverage_data%files))
        else
            files_analyzed_str = '0'
        end if

        ! First pass: estimate total length for single allocation
        total_len = 0
        total_len = total_len + len("<?xml version='1.0' encoding='UTF-8'?>") + 1
        total_len = total_len + len("<coverage version='1.0'>") + 1
        total_len = total_len + len("  <summary>") + 1
        total_len = total_len + len("    <overall_coverage>") + len_trim(pct_str) + len("</overall_coverage>") + 1
        total_len = total_len + len("    <lines_covered>") + len_trim(covered_str) + len("</lines_covered>") + 1
        total_len = total_len + len("    <total_lines>") + len_trim(total_str) + len("</total_lines>") + 1
        total_len = total_len + len("    <files_analyzed>") + len_trim(files_analyzed_str) + len("</files_analyzed>") + 1
        total_len = total_len + len("  </summary>") + 1
        total_len = total_len + len("  <files>") + 1

        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                fname = trim(coverage_data%files(i)%filename)
                cov_str = format_percentage(coverage_data%files(i)%line_coverage, 2)
                covered_str = int_to_string(coverage_data%files(i)%covered_lines)
                total_str = int_to_string(coverage_data%files(i)%total_lines)
                total_len = total_len + len("    <file>") + 1
                total_len = total_len + len("      <name>") + len_trim(fname) + len("</name>") + 1
                total_len = total_len + len("      <coverage>") + len_trim(cov_str) + len("</coverage>") + 1
                total_len = total_len + len("      <covered_lines>") + len_trim(covered_str) + len("</covered_lines>") + 1
                total_len = total_len + len("      <total_lines>") + len_trim(total_str) + len("</total_lines>") + 1
                total_len = total_len + len("    </file>") + 1
            end do
        end if

        total_len = total_len + len("  </files>") + 1
        total_len = total_len + len("</coverage>")

        allocate(character(len=total_len) :: buffer)
        pos = 1

        call append(buffer, pos, "<?xml version='1.0' encoding='UTF-8'?>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1
        call append(buffer, pos, "<coverage version='1.0'>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1
        call append(buffer, pos, "  <summary>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1

        call append(buffer, pos, "    <overall_coverage>")
        call append(buffer, pos, trim(pct_str))
        call append(buffer, pos, "</overall_coverage>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1

        call append(buffer, pos, "    <lines_covered>")
        call append(buffer, pos, trim(covered_str))
        call append(buffer, pos, "</lines_covered>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1

        call append(buffer, pos, "    <total_lines>")
        call append(buffer, pos, trim(total_str))
        call append(buffer, pos, "</total_lines>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1

        call append(buffer, pos, "    <files_analyzed>")
        call append(buffer, pos, trim(files_analyzed_str))
        call append(buffer, pos, "</files_analyzed>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1

        call append(buffer, pos, "  </summary>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1
        call append(buffer, pos, "  <files>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1

        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                fname = trim(coverage_data%files(i)%filename)
                cov_str = format_percentage(coverage_data%files(i)%line_coverage, 2)
                covered_str = int_to_string(coverage_data%files(i)%covered_lines)
                total_str = int_to_string(coverage_data%files(i)%total_lines)

                call append(buffer, pos, "    <file>")
                buffer(pos:pos) = new_line('a'); pos = pos + 1
                call append(buffer, pos, "      <name>")
                call append(buffer, pos, fname)
                call append(buffer, pos, "</name>")
                buffer(pos:pos) = new_line('a'); pos = pos + 1
                call append(buffer, pos, "      <coverage>")
                call append(buffer, pos, trim(cov_str))
                call append(buffer, pos, "</coverage>")
                buffer(pos:pos) = new_line('a'); pos = pos + 1
                call append(buffer, pos, "      <covered_lines>")
                call append(buffer, pos, trim(covered_str))
                call append(buffer, pos, "</covered_lines>")
                buffer(pos:pos) = new_line('a'); pos = pos + 1
                call append(buffer, pos, "      <total_lines>")
                call append(buffer, pos, trim(total_str))
                call append(buffer, pos, "</total_lines>")
                buffer(pos:pos) = new_line('a'); pos = pos + 1
                call append(buffer, pos, "    </file>")
                buffer(pos:pos) = new_line('a'); pos = pos + 1
            end do
        end if

        call append(buffer, pos, "  </files>")
        buffer(pos:pos) = new_line('a'); pos = pos + 1
        call append(buffer, pos, "</coverage>")

        xml_content = buffer(1:pos-1)
        
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
    contains
        pure subroutine append(buf, pos, txt)
            character(len=*), intent(inout) :: buf
            integer,           intent(inout) :: pos
            character(len=*),  intent(in)    :: txt
            integer :: L
            L = len(txt)
            if (L > 0) then
                buf(pos:pos+L-1) = txt
                pos = pos + L
            end if
        end subroutine append
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
