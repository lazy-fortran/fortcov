program test_json_file_import_empty_files_array_issue_1144
    !! Verifies JSON file import handles empty files array (Issue #1144)
    use json_io
    use coverage_model_core
    implicit none

    call test_json_file_import_empty_files()

contains

    subroutine test_json_file_import_empty_files()
        type(coverage_data_t) :: data
        logical :: error_caught
        integer :: unit
        character(len=*), parameter :: filename = 'issue_1144_empty_files.json'
        character(len=*), parameter :: json_content = &
            '{"version":"1.0","tool":"fortcov","files":[]}'

        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') json_content
        close(unit)

        call import_coverage_from_json_file(filename, data, error_caught)
        if (error_caught) then
            print *, 'ERROR: error_caught for empty files array (file)'
            stop 1
        end if

        if (.not. allocated(data%files)) then
            print *, 'ERROR: files array not allocated for empty array (file)'
            stop 1
        end if

        if (size(data%files) /= 0) then
            print *, 'ERROR: expected 0 files, got', size(data%files)
            stop 1
        end if

        open(newunit=unit, file=filename, status='old')
        close(unit, status='delete')
    end subroutine test_json_file_import_empty_files

end program test_json_file_import_empty_files_array_issue_1144

