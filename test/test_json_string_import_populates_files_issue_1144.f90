program test_json_string_import_populates_files_issue_1144
    !! Verifies in-memory JSON import populates coverage_data%files (Issue #1144)
    use json_io
    use coverage_model_core
    implicit none

    call test_json_string_import_single_file_single_line()

contains

    subroutine test_json_string_import_single_file_single_line()
        type(coverage_data_t) :: data
        character(len=*), parameter :: json_content = &
            '{"version":"1.0","tool":"fortcov","files":[' // &
            '{"filename":"example.f90","lines":[{"line_number":42,"execution_count":7}]}' // &
            ']}'

        call import_coverage_from_json(json_content, data)

        if (.not. allocated(data%files)) then
            print *, 'ERROR: files array not allocated after string import'
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
            print *, 'ERROR: lines not allocated for imported file (string)'
            stop 1
        end if

        if (size(data%files(1)%lines) /= 1) then
            print *, 'ERROR: expected 1 line, got', size(data%files(1)%lines)
            stop 1
        end if

        if (data%files(1)%lines(1)%line_number /= 42) then
            print *, 'ERROR: line_number mismatch: ', data%files(1)%lines(1)%line_number
            stop 1
        end if

        if (data%files(1)%lines(1)%execution_count /= 7) then
            print *, 'ERROR: execution_count mismatch: ', data%files(1)%lines(1)%execution_count
            stop 1
        end if
    end subroutine test_json_string_import_single_file_single_line

end program test_json_string_import_populates_files_issue_1144

