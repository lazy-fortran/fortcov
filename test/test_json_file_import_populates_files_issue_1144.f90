program test_json_file_import_populates_files_issue_1144
    !! Verifies JSON file import populates coverage_data%files (Issue #1144)
    use json_io
    use coverage_model_core
    implicit none

    call test_json_file_import_single_file_single_line()

    print *, "=== Issue #1144: JSON file import populates files ==="

contains

    subroutine test_json_file_import_single_file_single_line()
        type(coverage_data_t) :: data
        logical :: error_caught
        character(len=*), parameter :: filename = 'issue_1144_test.json'
        integer :: unit
        character(len=*), parameter :: json_content = &
            '{"version":"1.0","tool":"fortcov","files":[' // &
            '{"filename":"example.f90","lines":[{"line_number":10,"execution_count":3}]}' // &
            ']}'

        ! Write JSON content to file
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') json_content
        close(unit)

        ! Import coverage from JSON file
        call import_coverage_from_json_file(filename, data, error_caught)
        if (error_caught) then
            print *, 'ERROR: import_coverage_from_json_file reported error'
            stop 1
        end if

        ! Validate files populated
        if (.not. allocated(data%files)) then
            print *, 'ERROR: files array not allocated after import'
            stop 1
        end if

        if (size(data%files) /= 1) then
            print *, 'ERROR: expected 1 file, got', size(data%files)
            stop 1
        end if

        if (trim(data%files(1)%filename) /= 'example.f90') then
            print *, 'ERROR: filename mismatch: ', trim(data%files(1)%filename)
            stop 1
        end if

        if (.not. allocated(data%files(1)%lines)) then
            print *, 'ERROR: lines not allocated for imported file'
            stop 1
        end if

        if (size(data%files(1)%lines) /= 1) then
            print *, 'ERROR: expected 1 line, got', size(data%files(1)%lines)
            stop 1
        end if

        if (data%files(1)%lines(1)%line_number /= 10) then
            print *, 'ERROR: line_number mismatch: ', data%files(1)%lines(1)%line_number
            stop 1
        end if

        if (data%files(1)%lines(1)%execution_count /= 3) then
            print *, 'ERROR: execution_count mismatch: ', data%files(1)%lines(1)%execution_count
            stop 1
        end if

        ! Cleanup test file
        open(newunit=unit, file=filename, status='old')
        close(unit, status='delete')
    end subroutine test_json_file_import_single_file_single_line

end program test_json_file_import_populates_files_issue_1144

