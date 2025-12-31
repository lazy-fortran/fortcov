program test_gcov_function_block_dedup_issue_1222
    !! Test for issue #1222: Duplicate missing lines in report when gcov file
    !! has multiple function blocks
    !!
    !! When a .gcov file contains multiple function blocks (separated by
    !! ------------------), FortCov should only parse the main block and skip
    !! function-level duplicates.

    use gcov_file_processor, only: process_gcov_file
    use coverage_data_core, only: coverage_data_t
    implicit none

    integer :: passed = 0, failed = 0

    call test_function_block_separator_skipped()
    call test_multiple_function_blocks_deduplicated()
    call test_source_header_resets_function_block_state()

    print '(A)', ''
    print '(A,I0,A,I0,A)', 'Results: ', passed, ' passed, ', failed, ' failed'
    if (failed > 0) stop 1

contains

    subroutine test_function_block_separator_skipped()
        !! Given a gcov file with a function block separator
        !! When processed
        !! Then lines after the separator should be skipped
        character(len=*), parameter :: path = '/tmp/test_func_block.gcov'
        type(coverage_data_t) :: data
        logical :: err
        integer :: unit

        print '(A)', 'Test: Function block separator skipped'

        open(newunit=unit, file=path, status='replace')
        write(unit,'(A)') '        -:    0:Source:src/sample.f90'
        write(unit,'(A)') '        1:    1:program sample'
        write(unit,'(A)') '    #####:    2:  print *, "fail"'
        write(unit,'(A)') '        1:    3:end program'
        write(unit,'(A)') '------------------'
        write(unit,'(A)') 'MAIN__:'
        write(unit,'(A)') '        1:    1:program sample'
        write(unit,'(A)') '    #####:    2:  print *, "fail"'
        write(unit,'(A)') '        1:    3:end program'
        close(unit)

        call process_gcov_file(path, data, err)

        if (err) then
            print '(A)', '  FAIL: Processing returned error'
            failed = failed + 1
            call cleanup(path)
            return
        end if

        ! Should have 3 executable lines, not 6 (deduplicated)
        if (data%total_lines /= 3) then
            print '(A,I0,A)', '  FAIL: Expected 3 lines, got ', data%total_lines, &
                ' (duplicates not filtered)'
            failed = failed + 1
        else
            print '(A)', '  PASS: 3 lines parsed (duplicates filtered)'
            passed = passed + 1
        end if

        call cleanup(path)
    end subroutine test_function_block_separator_skipped

    subroutine test_multiple_function_blocks_deduplicated()
        !! Given a gcov file with multiple function blocks
        !! When processed
        !! Then each line number should only appear once
        character(len=*), parameter :: path = '/tmp/test_multi_func.gcov'
        type(coverage_data_t) :: data
        logical :: err
        integer :: unit

        print '(A)', 'Test: Multiple function blocks deduplicated'

        open(newunit=unit, file=path, status='replace')
        write(unit,'(A)') '        -:    0:Source:src/demo.f90'
        write(unit,'(A)') '        2:    1:program demo'
        write(unit,'(A)') '        1:    2:  x = 1'
        write(unit,'(A)') '        1:    3:  y = 2'
        write(unit,'(A)') '    #####:    4:  print *, "a"'
        write(unit,'(A)') '    #####:    5:  print *, "b"'
        write(unit,'(A)') '        1:    6:end program'
        write(unit,'(A)') '------------------'
        write(unit,'(A)') 'MAIN__:'
        write(unit,'(A)') '        1:    1:program demo'
        write(unit,'(A)') '    #####:    4:  print *, "a"'
        write(unit,'(A)') '    #####:    5:  print *, "b"'
        write(unit,'(A)') '------------------'
        write(unit,'(A)') '__demo_MOD_sub:'
        write(unit,'(A)') '        1:    2:  x = 1'
        write(unit,'(A)') '    #####:    4:  print *, "a"'
        close(unit)

        call process_gcov_file(path, data, err)

        if (err) then
            print '(A)', '  FAIL: Processing returned error'
            failed = failed + 1
            call cleanup(path)
            return
        end if

        ! Should have 6 executable lines (lines 1-6), not 13
        if (data%total_lines /= 6) then
            print '(A,I0,A)', '  FAIL: Expected 6 lines, got ', data%total_lines, &
                ' (function blocks not filtered)'
            failed = failed + 1
        else
            print '(A)', '  PASS: 6 lines parsed (function blocks filtered)'
            passed = passed + 1
        end if

        ! Should have 4 covered lines (1,2,3,6)
        if (data%covered_lines /= 4) then
            print '(A,I0)', '  FAIL: Expected 4 covered lines, got ', data%covered_lines
            failed = failed + 1
        else
            print '(A)', '  PASS: 4 covered lines'
            passed = passed + 1
        end if

        call cleanup(path)
    end subroutine test_multiple_function_blocks_deduplicated

    subroutine test_source_header_resets_function_block_state()
        !! Given a gcov file where function blocks are followed by new Source
        !! When processed
        !! Then the new Source section should be parsed normally
        character(len=*), parameter :: path = '/tmp/test_source_reset.gcov'
        type(coverage_data_t) :: data
        logical :: err
        integer :: unit

        print '(A)', 'Test: Source header resets function block state'

        open(newunit=unit, file=path, status='replace')
        ! First file with function block
        write(unit,'(A)') '        -:    0:Source:src/first.f90'
        write(unit,'(A)') '        1:    1:program first'
        write(unit,'(A)') '        1:    2:end program'
        write(unit,'(A)') '------------------'
        write(unit,'(A)') 'MAIN__:'
        write(unit,'(A)') '        1:    1:program first'
        write(unit,'(A)') '        1:    2:end program'
        ! Second file (should be parsed after function block)
        write(unit,'(A)') '        -:    0:Source:src/second.f90'
        write(unit,'(A)') '        1:    1:program second'
        write(unit,'(A)') '    #####:    2:  unreached = 1'
        write(unit,'(A)') '        1:    3:end program'
        close(unit)

        call process_gcov_file(path, data, err)

        if (err) then
            print '(A)', '  FAIL: Processing returned error'
            failed = failed + 1
            call cleanup(path)
            return
        end if

        ! Should have 2 files
        if (data%total_files /= 2) then
            print '(A,I0)', '  FAIL: Expected 2 files, got ', data%total_files
            failed = failed + 1
        else
            print '(A)', '  PASS: 2 files parsed'
            passed = passed + 1
        end if

        ! Total lines: 2 from first + 3 from second = 5
        if (data%total_lines /= 5) then
            print '(A,I0)', '  FAIL: Expected 5 total lines, got ', data%total_lines
            failed = failed + 1
        else
            print '(A)', '  PASS: 5 total lines'
            passed = passed + 1
        end if

        call cleanup(path)
    end subroutine test_source_header_resets_function_block_state

    subroutine cleanup(path)
        character(len=*), intent(in) :: path
        integer :: unit
        logical :: exists

        inquire(file=path, exist=exists)
        if (exists) then
            open(newunit=unit, file=path, status='old')
            close(unit, status='delete')
        end if
    end subroutine cleanup

end program test_gcov_function_block_dedup_issue_1222
