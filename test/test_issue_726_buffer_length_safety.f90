program test_issue_726_buffer_length_safety
    !! Validate empty result buffer length is minimal (Issue #726)
    use config_core, only: config_t, initialize_config
    use coverage_processor_gcov, only: discover_gcov_files
    use iso_fortran_env, only: output_unit
    implicit none

    type(config_t) :: config
    character(len=:), allocatable :: files(:)

    call initialize_config(config)
    config%quiet = .true.

    call discover_gcov_files(config, files)

    if (.not. allocated(files)) then
        write(output_unit,'(A)') '✗ files not allocated'
        stop 1
    end if

    if (size(files) /= 0) then
        write(output_unit,'(A,I0)') '✗ expected empty files, size=', size(files)
        stop 1
    end if

    if (len(files) /= 1) then
        write(output_unit,'(A,I0)') '✗ expected element length 1, got ', len(files)
        stop 1
    end if

    write(output_unit,'(A)') '✓ Issue #726 buffer length safety validated'

end program test_issue_726_buffer_length_safety

