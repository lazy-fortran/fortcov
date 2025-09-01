program test_line_counts_are_correct_issue_1118
    !! Regression test for Issue #1118
    !! Ensures coverage counts lines (not files) by validating
    !! executable and covered line totals from a gcov sample.

    use iso_fortran_env, only: output_unit
    use coverage_parser_factory, only: gcov_parser_t
    use coverage_model_core, only: coverage_data_t
    implicit none

    character(len=*), parameter :: test_file = 'issue_1118_sample.gcov'
    type(gcov_parser_t) :: parser
    type(coverage_data_t) :: data
    logical :: error_flag
    integer :: unit
    logical :: ok

    ok = .true.

    ! Create a minimal gcov with mixed executable/non-executable lines
    open(newunit=unit, file=test_file, status='replace')
    write(unit, '(A)') '        -:    0:Source:src/example_1118.f90'
    write(unit, '(A)') '        -:    0:Graph:example_1118.gcno'
    write(unit, '(A)') '        -:    0:Data:example_1118.gcda'
    write(unit, '(A)') '        -:    1:program example_1118'        ! non-executable
    write(unit, '(A)') '    #####:    2:print *, "x"'               ! exec=0 (executable)
    write(unit, '(A)') '       12:    3:call foo()'                  ! exec>0 (executable)
    write(unit, '(A)') '        -:    4:! comment'                   ! non-executable
    write(unit, '(A)') '        0:    5:if (1==2) then'              ! exec=0 (executable)
    write(unit, '(A)') '        1:    6:end if'                      ! exec>0 (executable)
    close(unit)

    data = parser%parse(test_file, error_flag)

    if (error_flag) then
        ok = .false.
    else if (.not. allocated(data%files)) then
        ok = .false.
    else if (size(data%files) /= 1) then
        ok = .false.
    else
        ! Expect executable lines: lines 2,3,5,6 => 4 total
        if (data%files(1)%total_lines /= 4) then
            ok = .false.
        end if
        ! Covered lines: lines with exec>0 => lines 3 and 6 => 2 covered
        if (data%files(1)%covered_lines /= 2) then
            ok = .false.
        end if
        ! Also validate aggregated totals on coverage_data
        if (data%total_lines /= 4) then
            ok = .false.
        end if
        if (data%covered_lines /= 2) then
            ok = .false.
        end if
    end if

    if (ok) then
        write(output_unit, '(A)') 'ISSUE #1118 line counts: PASS'
        call cleanup()
        stop 0
    else
        write(output_unit, '(A)') 'ISSUE #1118 line counts: FAIL'
        call cleanup()
        stop 1
    end if

contains

    subroutine cleanup()
        integer :: ios
        open(newunit=unit, file=test_file, status='old', action='read', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
        end if
    end subroutine cleanup

end program test_line_counts_are_correct_issue_1118
