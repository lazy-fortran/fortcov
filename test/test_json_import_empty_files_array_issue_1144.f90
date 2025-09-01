program test_json_import_empty_files_array_issue_1144
    !! Verifies JSON string import handles empty files array (Issue #1144)
    use json_io
    use coverage_model_core
    implicit none

    call test_json_string_import_empty_files()

contains

    subroutine test_json_string_import_empty_files()
        type(coverage_data_t) :: data
        character(len=*), parameter :: json_content = &
            '{"version":"1.0","tool":"fortcov","files":[]}'

        call import_coverage_from_json(json_content, data)

        if (.not. allocated(data%files)) then
            print *, 'ERROR: files array not allocated for empty array (string)'
            stop 1
        end if

        if (size(data%files) /= 0) then
            print *, 'ERROR: expected 0 files, got', size(data%files)
            stop 1
        end if
    end subroutine test_json_string_import_empty_files

end program test_json_import_empty_files_array_issue_1144

