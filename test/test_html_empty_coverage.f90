program test_html_empty_coverage
    use coverage_model_core
    use data_transformer_core
    use report_engine_html
    use theme_manager_core
    implicit none

    type(coverage_data_t) :: coverage
    type(data_transformer_t) :: transformer
    type(theme_manager_t) :: theme_mgr
    logical :: success
    logical :: exists
    character(len=:), allocatable :: err
    character(len=*), parameter :: out_path = 'test_empty_coverage.html'

    call coverage%init()         ! creates empty files array
    call transformer%init()
    call theme_mgr%init()

    call generate_html_report_content(coverage, transformer, theme_mgr, &
                                      out_path, success, err)

    if (.not. success) then
        print *, 'HTML generation should succeed on empty coverage. Error: ', trim(err)
        stop 1
    else
        inquire(file=out_path, exist=exists)
        if (.not. exists) then
            print *, 'HTML file was not created at: ', trim(out_path)
            stop 1
        end if
        print *, 'OK: HTML generation with empty coverage succeeded'
    end if

end program test_html_empty_coverage
