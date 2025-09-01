program test_xml_attribute_parser_1115
    use coverage_model_core, only: coverage_line_t
    use xml_attribute_parser, only: extract_filename_from_class, parse_lines_from_class
    implicit none

    character(len=:), allocatable :: filename
    character(len=:), allocatable :: class_xml
    type(coverage_line_t), allocatable :: lines(:)
    logical :: ok

    ! Build a minimal class XML snippet
    class_xml = '<class filename="myfile.f90">' // &
                '<line number="10" hits="3"/>' // &
                '<line number="20" hits="0"/>' // &
                '</class>'

    call extract_filename_from_class(class_xml, filename, ok)
    if (.not. ok) then
        print *, 'FAIL: extract_filename_from_class returned .false.'
        stop 1
    end if
    if (filename /= 'myfile.f90') then
        print *, 'FAIL: filename mismatch -> ', trim(filename)
        stop 1
    end if

    call parse_lines_from_class(class_xml, lines, ok)
    if (.not. ok) then
        print *, 'FAIL: parse_lines_from_class returned .false.'
        stop 1
    end if

    if (.not. allocated(lines)) then
        print *, 'FAIL: lines not allocated'
        stop 1
    end if

    if (size(lines) /= 2) then
        print *, 'FAIL: expected 2 lines, got ', size(lines)
        stop 1
    end if

    if (lines(1)%line_number /= 10 .or. lines(1)%execution_count /= 3) then
        print *, 'FAIL: line(1) values incorrect:', lines(1)%line_number, lines(1)%execution_count
        stop 1
    end if
    if (lines(2)%line_number /= 20 .or. lines(2)%execution_count /= 0) then
        print *, 'FAIL: line(2) values incorrect:', lines(2)%line_number, lines(2)%execution_count
        stop 1
    end if

    print *, 'PASS: xml_attribute_parser basic extraction'
    stop 0
end program test_xml_attribute_parser_1115
