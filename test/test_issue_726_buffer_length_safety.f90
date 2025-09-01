program test_issue_726_buffer_length_safety
    !! Validate empty result buffer length is minimal (Issue #726)
    !! Isolation fix: constrain discovery to an empty temp directory to avoid
    !! interference from concurrent/previous test artifacts in the repo root.
    use config_core,          only: config_t, initialize_config
    use coverage_processor_gcov, only: discover_gcov_files
    use directory_operations, only: ensure_directory
    use iso_fortran_env,      only: output_unit
    implicit none

    type(config_t) :: config
    character(len=:), allocatable :: files(:)
    character(len=256) :: tmpdir
    logical :: dir_err

    call initialize_config(config)
    config%quiet = .true.

    ! Use an isolated empty directory for discovery to ensure deterministic result
    tmpdir = 'build/test_tmp_issue_726'
    call ensure_directory(trim(tmpdir), dir_err)
    if (dir_err) then
        write(output_unit,'(A)') '✗ failed to create temp directory'
        stop 1
    end if

    ! Constrain gcov discovery to the isolated directory
    if (allocated(config%source_paths)) deallocate(config%source_paths)
    allocate(character(len=256) :: config%source_paths(1))
    config%source_paths(1) = trim(tmpdir)

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
