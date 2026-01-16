module xml_generator_core
    !! XML Generation Functions
    !! 
    !! Handles generation of XML sections for coverage reports.
    !! Extracted from xml_utils.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use string_utils, only: int_to_string
    use xml_utility_helpers, only: get_directory_path, get_base_name
    implicit none
    private
    
    public :: generate_sources_section, generate_packages_section
    public :: calculate_file_line_rate

contains

    ! Generate sources section of XML (optimized to avoid O(n^2) concatenation)
    function generate_sources_section(coverage_data) result(sources_xml)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: sources_xml
        integer :: i
        character(len=:), allocatable :: source_path
        character(len=:), allocatable :: source_path_xml
        integer :: total_len, pos, path_len
        character(len=:), allocatable :: buffer

        

        ! First pass: estimate total length for single allocation
        total_len = len('<sources>') + 1 + len('</sources>')
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                source_path = get_directory_path(coverage_data%files(i)%filename)
                path_len = xml_escape_length(trim(source_path), .false.)
                total_len = total_len + len('  <source>') + path_len + &
                            len('</source>') + 1
            end do
        end if

        allocate(character(len=total_len) :: buffer)
        pos = 1

        call append_text(buffer, pos, '<sources>')
        buffer(pos:pos) = new_line('a')
        pos = pos + 1

        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                source_path = get_directory_path(coverage_data%files(i)%filename)
                source_path_xml = xml_escape(trim(source_path), .false.)
                call append_text(buffer, pos, '  <source>')
                call append_text(buffer, pos, trim(source_path_xml))
                call append_text(buffer, pos, '</source>')
                buffer(pos:pos) = new_line('a')
                pos = pos + 1
            end do
        end if

        call append_text(buffer, pos, '</sources>')

        sources_xml = buffer(1:pos-1)
    contains
        pure subroutine append_text(buf, pos, txt)
            character(len=*), intent(inout) :: buf
            integer,           intent(inout) :: pos
            character(len=*),  intent(in)    :: txt
            integer :: L
            L = len(txt)
            if (L > 0) then
                buf(pos:pos+L-1) = txt
                pos = pos + L
            end if
        end subroutine append_text
    end function generate_sources_section
    
    ! Generate packages section of XML (optimized to avoid O(n^2) concatenation)
    function generate_packages_section(coverage_data) result(packages_xml)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: packages_xml
        integer :: i, j
        real :: file_line_rate
        integer :: total_len, pos
        character(len=:), allocatable :: buffer
        character(len=:), allocatable :: fname, bname
        character(len=:), allocatable :: fname_xml, bname_xml
        character(len=:), allocatable :: rate_str
        character(len=:), allocatable :: branch_rate_str
        character(len=:), allocatable :: line_no_str, hits_str
        integer :: fname_len, bname_len

        

        ! Header/footer static parts
        total_len = 0
        total_len = total_len + len('<packages>') + 1
        total_len = total_len + len('  <package name="fortcov-coverage">') + 1
        total_len = total_len + len('    <classes>') + 1
        total_len = total_len + len('    </classes>') + 1
        total_len = total_len + len('  </package>') + 1
        total_len = total_len + len('</packages>')

        branch_rate_str = "0.0"

        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                call calculate_file_line_rate(coverage_data%files(i), file_line_rate)
                fname = trim(coverage_data%files(i)%filename)
                bname = get_base_name(coverage_data%files(i)%filename)
                rate_str = rate_to_string(file_line_rate)
                fname_len = xml_escape_length(fname, .true.)
                bname_len = xml_escape_length(bname, .true.)

                ! Opening class tag + lines start
                total_len = total_len + len('      <class filename="') + fname_len + &
                             len('" name="') + bname_len + &
                             len('" line-rate="') + len_trim(rate_str) + &
                             len('" branch-rate="') + len_trim(branch_rate_str) + &
                             len('" complexity="0.0">') + 1
                total_len = total_len + len('        <lines>') + 1

                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        if (coverage_data%files(i)%lines(j)%is_executable) then
                            line_no_str = int_to_string( &
                                coverage_data%files(i)%lines(j)%line_number)
                            hits_str = int_to_string( &
                                coverage_data%files(i)%lines(j)%execution_count)
                            total_len = total_len + len('          <line number="') + &
                                         len_trim(line_no_str) + len('" hits="') + &
                                         len_trim(hits_str) + &
                                         len('" branch="false"/>') + 1
                        end if
                    end do
                end if

                ! Closing tags for this class
                total_len = total_len + len('        </lines>') + 1
                total_len = total_len + len('      </class>') + 1
            end do
        end if

        allocate(character(len=total_len) :: buffer)
        pos = 1

        call append_text(buffer, pos, '<packages>')
        buffer(pos:pos) = new_line('a')
        pos = pos + 1
        call append_text(buffer, pos, '  <package name="fortcov-coverage">')
        buffer(pos:pos) = new_line('a')
        pos = pos + 1
        call append_text(buffer, pos, '    <classes>')
        buffer(pos:pos) = new_line('a')
        pos = pos + 1

        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                call calculate_file_line_rate(coverage_data%files(i), file_line_rate)
                fname = trim(coverage_data%files(i)%filename)
                bname = get_base_name(coverage_data%files(i)%filename)
                rate_str = rate_to_string(file_line_rate)
                fname_xml = xml_escape(fname, .true.)
                bname_xml = xml_escape(bname, .true.)

                call append_text(buffer, pos, '      <class filename="')
                call append_text(buffer, pos, trim(fname_xml))
                call append_text(buffer, pos, '" name="')
                call append_text(buffer, pos, trim(bname_xml))
                call append_text(buffer, pos, '" line-rate="')
                call append_text(buffer, pos, trim(rate_str))
                call append_text(buffer, pos, '" branch-rate="')
                call append_text(buffer, pos, trim(branch_rate_str))
                call append_text(buffer, pos, '" complexity="0.0">')
                buffer(pos:pos) = new_line('a')
                pos = pos + 1

                call append_text(buffer, pos, '        <lines>')
                buffer(pos:pos) = new_line('a')
                pos = pos + 1

                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        if (coverage_data%files(i)%lines(j)%is_executable) then
                            line_no_str = int_to_string( &
                                coverage_data%files(i)%lines(j)%line_number)
                            hits_str = int_to_string( &
                                coverage_data%files(i)%lines(j)%execution_count)
                            call append_text(buffer, pos, '          <line number="')
                            call append_text(buffer, pos, trim(line_no_str))
                            call append_text(buffer, pos, '" hits="')
                            call append_text(buffer, pos, trim(hits_str))
                            call append_text(buffer, pos, '" branch="false"/>')
                            buffer(pos:pos) = new_line('a')
                            pos = pos + 1
                        end if
                    end do
                end if

                call append_text(buffer, pos, '        </lines>')
                buffer(pos:pos) = new_line('a')
                pos = pos + 1
                call append_text(buffer, pos, '      </class>')
                buffer(pos:pos) = new_line('a')
                pos = pos + 1
            end do
        end if

        call append_text(buffer, pos, '    </classes>')
        buffer(pos:pos) = new_line('a')
        pos = pos + 1
        call append_text(buffer, pos, '  </package>')
        buffer(pos:pos) = new_line('a')
        pos = pos + 1
        call append_text(buffer, pos, '</packages>')

        packages_xml = buffer(1:pos-1)
    contains
        pure subroutine append_text(buf, pos, txt)
            character(len=*), intent(inout) :: buf
            integer,           intent(inout) :: pos
            character(len=*),  intent(in)    :: txt
            integer :: L
            L = len(txt)
            if (L > 0) then
                buf(pos:pos+L-1) = txt
                pos = pos + L
            end if
        end subroutine append_text
    end function generate_packages_section
    
    ! Calculate line rate for a single file
    subroutine calculate_file_line_rate(file_data, line_rate)
        type(coverage_file_t), intent(in) :: file_data
        real, intent(out) :: line_rate
        
        integer :: total_lines, covered_lines, i
        
        total_lines = 0
        covered_lines = 0
        
        ! Memory safety: Check if lines array is allocated
        if (allocated(file_data%lines)) then
            do i = 1, size(file_data%lines)
                if (file_data%lines(i)%is_executable) then
                    total_lines = total_lines + 1
                    if (file_data%lines(i)%execution_count > 0) then
                        covered_lines = covered_lines + 1
                    end if
                end if
            end do
        end if
        
        if (total_lines > 0) then
            line_rate = real(covered_lines) / real(total_lines)
        else
            line_rate = 0.0
        end if
        
    end subroutine calculate_file_line_rate

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

    pure integer function xml_escape_length(value, escape_quotes) result(out_len)
        character(len=*), intent(in) :: value
        logical, intent(in) :: escape_quotes
        integer :: i
        character(len=1) :: c

        out_len = len_trim(value)
        do i = 1, len_trim(value)
            c = value(i:i)
            select case (c)
            case ('&')
                out_len = out_len + 4
            case ('<', '>')
                out_len = out_len + 3
            case ('"', "'")
                if (escape_quotes) out_len = out_len + 5
            end select
        end do
    end function xml_escape_length

    pure function xml_escape(value, escape_quotes) result(escaped)
        character(len=*), intent(in) :: value
        logical, intent(in) :: escape_quotes
        character(len=:), allocatable :: escaped

        integer :: i, pos, out_len
        character(len=1) :: c

        out_len = xml_escape_length(value, escape_quotes)
        allocate(character(len=out_len) :: escaped)

        pos = 1
        do i = 1, len_trim(value)
            c = value(i:i)
            select case (c)
            case ('&')
                escaped(pos:pos+4) = '&amp;'
                pos = pos + 5
            case ('<')
                escaped(pos:pos+3) = '&lt;'
                pos = pos + 4
            case ('>')
                escaped(pos:pos+3) = '&gt;'
                pos = pos + 4
            case ('"')
                if (escape_quotes) then
                    escaped(pos:pos+5) = '&quot;'
                    pos = pos + 6
                else
                    escaped(pos:pos) = '"'
                    pos = pos + 1
                end if
            case ("'")
                if (escape_quotes) then
                    escaped(pos:pos+5) = '&apos;'
                    pos = pos + 6
                else
                    escaped(pos:pos) = "'"
                    pos = pos + 1
                end if
            case default
                escaped(pos:pos) = c
                pos = pos + 1
            end select
        end do
    end function xml_escape
    
end module xml_generator_core
