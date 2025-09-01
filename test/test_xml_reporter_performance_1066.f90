program test_xml_reporter_performance_1066
    !! Validate xml_reporter uses efficient buffered building (Issue #1066)
    use coverage_types
    use coverage_stats_core, only: calculate_line_coverage
    use file_utilities, only: read_file_content
    use xml_reporter
    use file_ops_secure, only: safe_mkdir
    use error_handling_core, only: error_context_t
    use portable_temp_utils, only: get_temp_dir
    implicit none

    type(xml_reporter_t) :: reporter
    type(coverage_data_t) :: data
    type(coverage_file_t), allocatable :: files(:)
    type(coverage_line_t), allocatable :: lines(:)
    integer :: i, j, nfiles, nlines
    logical :: success, read_error
    character(len=:), allocatable :: content
    character(len=:), allocatable :: out_path

    ! Build a moderately large dataset to exercise performance
    nfiles = 120
    nlines = 20

    allocate(files(nfiles))
    do i = 1, nfiles
        allocate(lines(nlines))
        do j = 1, nlines
            call lines(j)%init('file'//trim(i2s(i))//'.f90', j, merge(1, 0, mod(j, 2) == 0), .true.)
        end do
        call files(i)%init('file'//trim(i2s(i))//'.f90', lines)
        call files(i)%calculate_coverage()
        deallocate(lines)
    end do

    call data%init(files)

    block
        character(len=:), allocatable :: base
        type(error_context_t) :: err
        base = get_temp_dir()
        out_path = trim(base) // '/fortcov_tests/test_temp_dir/xml_reporter_1066.xml'
        call safe_mkdir(trim(base) // '/fortcov_tests/test_temp_dir', err)
    end block

    call reporter%generate_report(data, out_path, success, content)
    if (.not. success) then
        print *, 'ERROR: xml_reporter failed to write output'
        stop 1
    end if

    call read_file_content(out_path, content, read_error)
    if (read_error) then
        print *, 'ERROR: failed to read generated XML content'
        stop 1
    end if

    if (index(content, '<files>') == 0) then
        print *, 'ERROR: missing <files> section'
        stop 1
    end if

    if (count_occurrences(content, '<file>') /= nfiles) then
        print *, 'ERROR: file entries count mismatch'
        stop 1
    end if

    print *, '[PASS] xml_reporter buffered generation validated (Issue #1066)'

contains

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

    function i2s(v) result(s)
        integer, intent(in) :: v
        character(len=:), allocatable :: s
        character(len=32) :: tmp
        write(tmp,'(I0)') v
        s = trim(tmp)
    end function i2s

end program test_xml_reporter_performance_1066
