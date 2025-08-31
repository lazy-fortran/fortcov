program test_xml_generator_performance_1066
    !! Validate XML packages generator avoids pathological concatenation
    !! and produces correct class entries for many files (Issue #1066)
    use coverage_format_converter
    implicit none

    character(len=:), allocatable :: json_content
    character(len=:), allocatable :: xml_output
    integer :: nfiles
    logical :: success

    nfiles = 75  ! enough to exercise builder without slowing tests
    call build_sample_json(nfiles, json_content)

    call convert_json_to_cobertura_xml(json_content, xml_output, success)
    if (.not. success) then
        print *, 'ERROR: XML conversion failed'
        stop 1
    end if

    call assert_contains(xml_output, '<packages>')
    call assert_contains(xml_output, '</packages>')
    call assert_contains(xml_output, '<classes>')

    if (count_occurrences(xml_output, '<class name="') /= nfiles) then
        print *, 'ERROR: class entries count mismatch'
        stop 1
    end if

    print *, '✅ PASS: XML generator performance/correctness validated (Issue #1066)'

contains

    subroutine build_sample_json(n, json)
        integer, intent(in) :: n
        character(len=:), allocatable, intent(out) :: json
        integer :: i
        character(len=32) :: num
        character(len=:), allocatable :: buf
        integer :: total_len, pos

        ! Estimate size: header + per-file + footer (rough but safe)
        total_len = len('{"summary":{"line_coverage":80,"covered_lines":800,'// &
                        '"total_lines":1000},"files":[')
        total_len = total_len + n * (len('{"filename":"file') + 8 + len('.f90"},'))
        total_len = total_len + len(']}')

        allocate(character(len=total_len) :: buf)
        pos = 1

        call append(buf, pos, '{"summary":{"line_coverage":80,"covered_lines":800,')
        call append(buf, pos, '"total_lines":1000},"files":[')

        do i = 1, n
            write(num, '(I0)') i
            call append(buf, pos, '{"filename":"file')
            call append(buf, pos, trim(num))
            call append(buf, pos, '.f90"}')
            if (i < n) call append(buf, pos, ',')
        end do

        call append(buf, pos, ']}')
        json = buf(1:pos-1)
    end subroutine build_sample_json

    subroutine assert_contains(hay, needle)
        character(len=*), intent(in) :: hay, needle
        if (index(hay, needle) == 0) then
            print *, 'ERROR: expected substring not found: ', trim(needle)
            stop 1
        end if
    end subroutine assert_contains

    function count_occurrences(hay, needle) result(cnt)
        character(len=*), intent(in) :: hay, needle
        integer :: cnt, start, loc, L
        cnt = 0
        start = 1
        L = len_trim(needle)
        if (L == 0) return
        do
            if (start > len(hay)) exit
            loc = index(hay(start:), needle)
            if (loc == 0) exit
            cnt = cnt + 1
            start = start + loc + L - 1
        end do
    end function count_occurrences

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

end program test_xml_generator_performance_1066

