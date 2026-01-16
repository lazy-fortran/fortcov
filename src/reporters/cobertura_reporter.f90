module cobertura_reporter
    !! Cobertura XML report generation
    !!
    !! Generates Cobertura-compatible XML output from coverage data.
    use coverage_model_core, only: coverage_data_t
    use string_utils, only: int_to_string
    use xml_utils, only: generate_packages_section, generate_sources_section, &
                         get_current_timestamp
    implicit none
    private

    public :: generate_cobertura_xml_report

contains

    function generate_cobertura_xml_report(coverage_data) result(xml_report)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: xml_report

        character(len=:), allocatable :: sources_xml
        character(len=:), allocatable :: packages_xml
        character(len=:), allocatable :: timestamp
        character(len=:), allocatable :: line_rate_str
        character(len=:), allocatable :: branch_rate_str
        character(len=:), allocatable :: lines_valid_str
        character(len=:), allocatable :: lines_covered_str
        character(len=:), allocatable :: branches_valid_str
        character(len=:), allocatable :: branches_covered_str
        character(len=:), allocatable :: buffer
        integer :: total_len, pos
        integer :: lines_valid, lines_covered
        integer :: branches_valid, branches_covered
        real :: line_rate, branch_rate

        call calculate_line_totals(coverage_data, lines_valid, lines_covered)

        branches_valid = 0
        branches_covered = 0

        if (lines_valid > 0) then
            line_rate = real(lines_covered) / real(lines_valid)
        else
            line_rate = 0.0
        end if

        branch_rate = 0.0

        sources_xml = generate_sources_section(coverage_data)
        packages_xml = generate_packages_section(coverage_data)
        timestamp = trim(get_current_timestamp())

        line_rate_str = rate_to_string(line_rate)
        branch_rate_str = rate_to_string(branch_rate)
        lines_valid_str = int_to_string(lines_valid)
        lines_covered_str = int_to_string(lines_covered)
        branches_valid_str = int_to_string(branches_valid)
        branches_covered_str = int_to_string(branches_covered)

        total_len = 1024 + len_trim(sources_xml) + len_trim(packages_xml) + &
                    len_trim(timestamp) + len_trim(line_rate_str) + &
                    len_trim(branch_rate_str) + len_trim(lines_valid_str) + &
                    len_trim(lines_covered_str) + len_trim(branches_valid_str) + &
                    len_trim(branches_covered_str)

        allocate(character(len=total_len) :: buffer)
        pos = 1

        call append_text(buffer, pos, '<?xml version="1.0"?>')
        call append_nl(buffer, pos)
        call append_text(buffer, pos, '<!DOCTYPE coverage SYSTEM ')
        call append_text(buffer, pos, &
                         '"http://cobertura.sourceforge.net/xml/coverage-04.dtd">')
        call append_nl(buffer, pos)

        call append_text(buffer, pos, '<coverage lines-valid="')
        call append_text(buffer, pos, trim(lines_valid_str))
        call append_text(buffer, pos, '" lines-covered="')
        call append_text(buffer, pos, trim(lines_covered_str))
        call append_text(buffer, pos, '" line-rate="')
        call append_text(buffer, pos, trim(line_rate_str))
        call append_text(buffer, pos, '" branches-valid="')
        call append_text(buffer, pos, trim(branches_valid_str))
        call append_text(buffer, pos, '" branches-covered="')
        call append_text(buffer, pos, trim(branches_covered_str))
        call append_text(buffer, pos, '" branch-rate="')
        call append_text(buffer, pos, trim(branch_rate_str))
        call append_text(buffer, pos, &
                         '" complexity="0.0" version="fortcov" timestamp="')
        call append_text(buffer, pos, trim(timestamp))
        call append_text(buffer, pos, '">')
        call append_nl(buffer, pos)

        call append_text(buffer, pos, trim(sources_xml))
        call append_nl(buffer, pos)
        call append_text(buffer, pos, trim(packages_xml))
        call append_nl(buffer, pos)
        call append_text(buffer, pos, '</coverage>')

        xml_report = buffer(1:pos-1)

    contains

        subroutine calculate_line_totals(data, total_lines, covered_lines)
            type(coverage_data_t), intent(in) :: data
            integer, intent(out) :: total_lines
            integer, intent(out) :: covered_lines
            integer :: file_idx

            total_lines = 0
            covered_lines = 0

            if (.not. allocated(data%files)) return

            do file_idx = 1, size(data%files)
                total_lines = total_lines + data%files(file_idx)%total_lines
                covered_lines = covered_lines + data%files(file_idx)%covered_lines
            end do
        end subroutine calculate_line_totals

        pure subroutine append_text(buf, pos, txt)
            character(len=*), intent(inout) :: buf
            integer, intent(inout) :: pos
            character(len=*), intent(in) :: txt
            integer :: l

            l = len(txt)
            if (l == 0) return
            buf(pos:pos+l-1) = txt
            pos = pos + l
        end subroutine append_text

        pure subroutine append_nl(buf, pos)
            character(len=*), intent(inout) :: buf
            integer, intent(inout) :: pos

            buf(pos:pos) = new_line('a')
            pos = pos + 1
        end subroutine append_nl

        function rate_to_string(value) result(str)
            real, intent(in) :: value
            character(len=:), allocatable :: str
            character(len=32) :: tmp

            write(tmp, '(F12.6)') value
            str = trim(adjustl(tmp))
            if (len(str) >= 1) then
                if (str(1:1) == ".") then
                    str = "0" // str
                else if (len(str) >= 2) then
                    if (str(1:2) == "-.") str = "-0" // str(2:)
                end if
            end if
        end function rate_to_string

    end function generate_cobertura_xml_report

end module cobertura_reporter
