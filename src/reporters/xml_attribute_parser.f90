module xml_attribute_parser
    !! XML Attribute Extraction and Line Parsing
    !!
    !! Handles extraction of attributes from XML elements and line parsing.
    !! Extracted from xml_utils.f90 for SRP compliance (Issue #718).
    use coverage_model_core, only: coverage_line_t
    use xml_parser_core, only: count_xml_elements
    implicit none
    private

    public :: extract_filename_from_class, parse_lines_from_class
    public :: extract_line_attributes

contains

    subroutine extract_filename_from_class(class_xml, filename, success)
        character(len=*), intent(in) :: class_xml
        character(len=:), allocatable, intent(out) :: filename
        logical, intent(out) :: success

        integer :: start_pos, end_pos

        success = .false.

        ! Find filename attribute start
        start_pos = index(class_xml, 'filename="')
        if (start_pos == 0) return
        start_pos = start_pos + 10  ! length of filename attribute marker

        ! Bounds check
        if (start_pos > len(class_xml)) return

        ! Find closing quote
        end_pos = index(class_xml(start_pos:), '"')
        if (end_pos == 0) return
        end_pos = start_pos + end_pos - 2

        ! Final bounds check
        if (end_pos < start_pos .or. end_pos > len(class_xml)) return

        filename = class_xml(start_pos:end_pos)
        success = .true.

    end subroutine extract_filename_from_class

    subroutine parse_lines_from_class(class_xml, lines, success)
        character(len=*), intent(in) :: class_xml
        type(coverage_line_t), allocatable, intent(out) :: lines(:)
        logical, intent(out) :: success

        integer :: line_count, i, pos, line_start, stat
        integer :: line_number, hits

        success = .false.

        ! Count line elements
        line_count = count_xml_elements(class_xml, '<line number=')
        if (line_count == 0) then
            allocate (lines(0), stat=stat)
            if (stat /= 0) return
            success = .true.
            return
        end if

        allocate (lines(line_count), stat=stat)
        if (stat /= 0) return
        pos = 1

        ! Parse each line
        do i = 1, line_count
            line_start = index(class_xml(pos:), '<line number="')
            if (line_start == 0) return
            line_start = line_start + pos - 1

            ! Bounds check
            if (line_start > len(class_xml)) return

            ! Extract line number and hits
            call extract_line_attributes(class_xml(line_start:), line_number, hits, &
                                         success)
            if (.not. success) return

            ! Create line object using dummy filename from parent context
            call lines(i)%init('dummy.f90', line_number, hits, .true.)

            ! Move position for next line - find actual end of current line element
            pos = index(class_xml(line_start:), '/>')
            if (pos == 0) then
                pos = index(class_xml(line_start:), '</line>')
                if (pos == 0) return
                pos = line_start + pos + 6  ! length of line end tag
            else
                pos = line_start + pos + 1  ! length of line empty tag
            end if
        end do

        success = .true.

    end subroutine parse_lines_from_class

    subroutine extract_line_attributes(line_xml, line_number, hits, success)
        character(len=*), intent(in) :: line_xml
        integer, intent(out) :: line_number, hits
        logical, intent(out) :: success

        integer :: num_start, num_end, hits_start, hits_end, iostat_var
        character(len=256) :: temp_str

        success = .false.
        line_number = 0
        hits = 0

        ! Extract line number
        num_start = index(line_xml, 'number="')
        if (num_start == 0) return
        num_start = num_start + 8  ! length of number attribute marker

        ! Bounds check
        if (num_start > len(line_xml)) return

        num_end = index(line_xml(num_start:), '"')
        if (num_end == 0) return
        num_end = num_start + num_end - 2

        ! Final bounds check
        if (num_end < num_start .or. num_end > len(line_xml)) return

        temp_str = adjustl(line_xml(num_start:num_end))
        read (temp_str, *, iostat=iostat_var) line_number
        if (iostat_var /= 0) return

        ! Extract hits
        hits_start = index(line_xml, 'hits="')
        if (hits_start == 0) return
        hits_start = hits_start + 6  ! length of hits attribute marker

        ! Bounds check
        if (hits_start > len(line_xml)) return

        hits_end = index(line_xml(hits_start:), '"')
        if (hits_end == 0) return
        hits_end = hits_start + hits_end - 2

        ! Final bounds check
        if (hits_end < hits_start .or. hits_end > len(line_xml)) return

        temp_str = adjustl(line_xml(hits_start:hits_end))
        read (temp_str, *, iostat=iostat_var) hits
        if (iostat_var /= 0) return

        success = .true.

    end subroutine extract_line_attributes

end module xml_attribute_parser
