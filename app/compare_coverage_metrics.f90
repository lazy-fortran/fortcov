program compare_coverage_metrics
    !! Compare coverage totals between Cobertura XML and FortCov Markdown.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, int32, &
                                                                                real64
    implicit none

    character(len=1024) :: xml_path
    character(len=1024) :: md_path
    character(len=64) :: tol_env
    integer :: argc
    integer :: status
    integer(int32) :: lines_valid
    integer(int32) :: lines_covered
    integer(int32) :: stmts
    integer(int32) :: covered
    real(real64) :: cob_rate
    real(real64) :: fort_rate
    real(real64) :: tol
    real(real64) :: tol_value

    argc = command_argument_count()
    if (argc /= 2) then
        write (error_unit, '(A)') &
            "Usage: compare_coverage_metrics coverage.xml coverage.md"
        error stop 2
    end if

    call get_command_argument(1, xml_path)
    call get_command_argument(2, md_path)

    call parse_cobertura_totals(trim(xml_path), lines_valid, lines_covered, &
                                cob_rate)
    call parse_fortcov_totals(trim(md_path), stmts, covered, fort_rate)

    write (output_unit, '(A,I0,A,I0,A,F0.6)') "Cobertura: lines-valid=", &
        lines_valid, " lines-covered=", lines_covered, " line-rate=", cob_rate
    write (output_unit, '(A,I0,A,I0,A,F0.6)') "FortCov : stmts=", stmts, &
        " covered=", covered, " line-rate=", fort_rate

    tol = 0.30_real64
    tol_env = ""
    call get_environment_variable("FORTCOV_COMPARE_TOL", tol_env, status=status)
    if (status == 0 .and. len_trim(tol_env) > 0) then
        call parse_real_value(trim(tol_env), tol_value, status)
        if (status == 0) then
            tol = tol_value
        end if
    end if

    if (abs(cob_rate - fort_rate) > tol) then
        write (error_unit, '(A,F0.6,A,F0.6,A,F0.6,A)') &
            "Mismatch: line-rate: ", cob_rate, " != ", fort_rate, &
            " (tol=", tol, ")"
        error stop 1
    end if

contains

    subroutine parse_cobertura_totals(path, lines_valid, lines_covered, line_rate)
        character(len=*), intent(in) :: path
        integer(int32), intent(out) :: lines_valid
        integer(int32), intent(out) :: lines_covered
        real(real64), intent(out) :: line_rate
        character(len=64) :: lv_str
        character(len=64) :: lc_str
        character(len=64) :: lr_str
        logical :: have_attrs
        integer :: status

        call read_cobertura_attrs(path, lv_str, lc_str, lr_str, have_attrs)
        if (have_attrs) then
            call parse_int_value(lv_str, lines_valid, status)
            if (status /= 0) then
                call abort_with_message("Invalid lines-valid in Cobertura XML")
            end if
            call parse_int_value(lc_str, lines_covered, status)
            if (status /= 0) then
                call abort_with_message("Invalid lines-covered in Cobertura XML")
            end if
            call parse_real_value(lr_str, line_rate, status)
            if (status /= 0) then
                call abort_with_message("Invalid line-rate in Cobertura XML")
            end if
        else
            call compute_cobertura_line_rate(path, lines_valid, lines_covered, &
                                             line_rate)
        end if
    end subroutine parse_cobertura_totals

    subroutine read_cobertura_attrs(path, lv_str, lc_str, lr_str, have_attrs)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: lv_str
        character(len=*), intent(out) :: lc_str
        character(len=*), intent(out) :: lr_str
        logical, intent(out) :: have_attrs
        character(len=4096) :: line
        logical :: found_lv
        logical :: found_lc
        logical :: found_lr
        integer :: unit
        integer :: stat

        lv_str = ""
        lc_str = ""
        lr_str = ""
        found_lv = .false.
        found_lc = .false.
        found_lr = .false.

        open (newunit=unit, file=trim(path), status="old", action="read", &
              iostat=stat)
        if (stat /= 0) then
            call abort_with_message("Unable to read Cobertura XML")
        end if

        do
            read (unit, '(A)', iostat=stat) line
            if (stat /= 0) then
                exit
            end if
            if (.not. found_lv) then
                call extract_attr_value(line, 'lines-valid="', lv_str, found_lv)
            end if
            if (.not. found_lc) then
                call extract_attr_value(line, 'lines-covered="', lc_str, found_lc)
            end if
            if (.not. found_lr) then
                call extract_attr_value(line, 'line-rate="', lr_str, found_lr)
            end if
            if (found_lv .and. found_lc .and. found_lr) then
                exit
            end if
        end do

        close (unit)
        have_attrs = found_lv .and. found_lc .and. found_lr
    end subroutine read_cobertura_attrs

    subroutine compute_cobertura_line_rate(path, lines_valid, lines_covered, &
                                           line_rate)
        character(len=*), intent(in) :: path
        integer(int32), intent(out) :: lines_valid
        integer(int32), intent(out) :: lines_covered
        real(real64), intent(out) :: line_rate
        character(len=4096) :: line
        character(len=64) :: hits_str
        integer :: unit
        integer :: stat
        integer :: hits
        logical :: found_hits

        lines_valid = 0
        lines_covered = 0
        line_rate = 0.0_real64

        open (newunit=unit, file=trim(path), status="old", action="read", &
              iostat=stat)
        if (stat /= 0) then
            call abort_with_message("Unable to read Cobertura XML")
        end if

        do
            read (unit, '(A)', iostat=stat) line
            if (stat /= 0) then
                exit
            end if
            if (index(line, "<line") > 0) then
                lines_valid = lines_valid + 1
                hits_str = ""
                found_hits = .false.
                call extract_attr_value(line, 'hits="', hits_str, found_hits)
                if (found_hits) then
                    call parse_int_value(hits_str, hits, stat)
                    if (stat == 0 .and. hits > 0) then
                        lines_covered = lines_covered + 1
                    end if
                end if
            end if
        end do

        close (unit)
        if (lines_valid > 0) then
            line_rate = real(lines_covered, real64)/real(lines_valid, real64)
        end if
    end subroutine compute_cobertura_line_rate

    subroutine parse_fortcov_totals(path, stmts, covered, line_rate)
        character(len=*), intent(in) :: path
        integer(int32), intent(out) :: stmts
        integer(int32), intent(out) :: covered
        real(real64), intent(out) :: line_rate
        character(len=4096) :: line
        integer :: unit
        integer :: stat
        logical :: found

        found = .false.
        open (newunit=unit, file=trim(path), status="old", action="read", &
              iostat=stat)
        if (stat /= 0) then
            call abort_with_message("Unable to read FortCov Markdown")
        end if

        do
            read (unit, '(A)', iostat=stat) line
            if (stat /= 0) then
                exit
            end if
            if (index(adjustl(line), "| TOTAL") == 1) then
                call parse_total_row(line, stmts, covered, line_rate)
                found = .true.
                exit
            end if
        end do

        close (unit)
        if (.not. found) then
            call abort_with_message("TOTAL row missing in FortCov Markdown")
        end if
    end subroutine parse_fortcov_totals

    subroutine parse_total_row(line, stmts, covered, line_rate)
        character(len=*), intent(in) :: line
        integer(int32), intent(out) :: stmts
        integer(int32), intent(out) :: covered
        real(real64), intent(out) :: line_rate
        character(len=64) :: field
        character(len=64) :: rate_str
        integer :: status
        real(real64) :: rate_value

        call get_pipe_field(line, 2, field, status)
        if (status /= 0) then
            call abort_with_message("Malformed TOTAL row in FortCov Markdown")
        end if
        call parse_int_value(field, stmts, status)
        if (status /= 0) then
            call abort_with_message("Invalid TOTAL statements in FortCov Markdown")
        end if

        call get_pipe_field(line, 3, field, status)
        if (status /= 0) then
            call abort_with_message("Malformed TOTAL row in FortCov Markdown")
        end if
        call parse_int_value(field, covered, status)
        if (status /= 0) then
            call abort_with_message("Invalid TOTAL covered in FortCov Markdown")
        end if

        call get_pipe_field(line, 4, field, status)
        if (status /= 0) then
            call abort_with_message("Malformed TOTAL row in FortCov Markdown")
        end if
        call strip_percent(field, rate_str)
        call parse_real_value(rate_str, rate_value, status)
        if (status /= 0) then
            call abort_with_message("Invalid TOTAL percent in FortCov Markdown")
        end if
        line_rate = rate_value/100.0_real64
    end subroutine parse_total_row

    subroutine get_pipe_field(line, field_index, field, status)
        character(len=*), intent(in) :: line
        integer, intent(in) :: field_index
        character(len=*), intent(out) :: field
        integer, intent(out) :: status
        integer :: i
        integer :: start_pos
        integer :: end_pos
        integer :: pipe_count
        integer :: line_len

        status = 1
        field = ""
        start_pos = 0
        end_pos = 0
        pipe_count = 0
        line_len = len_trim(line)

        do i = 1, line_len
            if (line(i:i) == "|") then
                pipe_count = pipe_count + 1
                if (pipe_count == field_index) then
                    start_pos = i + 1
                else if (pipe_count == field_index + 1) then
                    end_pos = i - 1
                    exit
                end if
            end if
        end do

        if (start_pos > 0) then
            if (end_pos == 0) then
                end_pos = line_len
            end if
            field = adjustl(line(start_pos:end_pos))
            status = 0
        end if
    end subroutine get_pipe_field

    subroutine strip_percent(text, stripped)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: stripped
        integer :: percent_pos

        stripped = adjustl(text)
        percent_pos = index(stripped, "%")
        if (percent_pos > 0) then
            stripped = stripped(1:percent_pos - 1)
        end if
        stripped = adjustl(stripped)
    end subroutine strip_percent

    subroutine extract_attr_value(line, key, value, found)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: key
        character(len=*), intent(out) :: value
        logical, intent(out) :: found
        integer :: start_pos
        integer :: end_pos
        integer :: key_len

        value = ""
        found = .false.
        key_len = len_trim(key)
        start_pos = index(line, key)
        if (start_pos == 0) then
            return
        end if
        start_pos = start_pos + key_len
        end_pos = index(line(start_pos:), '"')
        if (end_pos == 0) then
            return
        end if
        value = line(start_pos:start_pos + end_pos - 2)
        found = .true.
    end subroutine extract_attr_value

    subroutine parse_int_value(text, value, status)
        character(len=*), intent(in) :: text
        integer(int32), intent(out) :: value
        integer, intent(out) :: status
        read (text, *, iostat=status) value
    end subroutine parse_int_value

    subroutine parse_real_value(text, value, status)
        character(len=*), intent(in) :: text
        real(real64), intent(out) :: value
        integer, intent(out) :: status
        read (text, *, iostat=status) value
    end subroutine parse_real_value

    subroutine abort_with_message(message)
        character(len=*), intent(in) :: message
        write (error_unit, '(A)') trim(message)
        error stop 1
    end subroutine abort_with_message

end program compare_coverage_metrics
