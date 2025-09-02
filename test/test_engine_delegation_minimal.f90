program test_engine_delegation_minimal
    !! Minimal tests for coverage_engine_core delegation after collapse
    use config_core,          only: config_t, initialize_config
    use coverage_engine_core, only: find_coverage_files, check_exclude_patterns
    use file_ops_secure,      only: safe_mkdir, safe_remove_file, safe_remove_directory
    use error_handling_core,  only: error_context_t
    implicit none

    type(config_t)        :: cfg
    character(len=:), allocatable :: files(:)
    character(len=:), allocatable :: tmpdir, a_path, b_path
    type(error_context_t) :: err
    integer :: unit
    logical :: exists

    print *, 'OK: engine delegation minimal'

    ! Setup: temp directory with two gcov files
    tmpdir = 'engine_test_tmp'
    call safe_mkdir(tmpdir, err)

    a_path = tmpdir // '/a.gcov'
    b_path = tmpdir // '/b.gcov'

    open(newunit=unit, file=a_path, status='replace')
    write(unit, '(A)') 'dummy'
    close(unit)

    open(newunit=unit, file=b_path, status='replace')
    write(unit, '(A)') 'dummy'
    close(unit)

    ! Initialize config and point to explicit coverage files
    call initialize_config(cfg)
    cfg%quiet = .true.
    cfg%max_files = 10

    allocate(character(len=len(a_path)) :: cfg%coverage_files(2))
    cfg%coverage_files(1) = a_path
    cfg%coverage_files(2) = b_path

    ! WHEN: find_coverage_files is called
    files = find_coverage_files(cfg)

    ! THEN: both files are discovered (no filtering at this stage)
    if (.not. allocated(files)) then
        print *, 'FAIL: files not discovered'
        call cleanup()
        stop 1
    end if

    if (size(files) /= 2) then
        print *, 'FAIL: expected 2 files, got ', size(files)
        call cleanup()
        stop 1
    end if

    ! AND: exclude pattern evaluation delegates correctly
    allocate(character(len=8) :: cfg%exclude_patterns(1))
    cfg%exclude_patterns(1) = '*.gcov'

    if (.not. check_exclude_patterns(a_path, cfg)) then
        print *, 'FAIL: expected a.gcov to be excluded by pattern'
        call cleanup()
        stop 1
    end if

    ! Cleanup and exit success
    call cleanup()
    print *, 'OK: engine delegation minimal'
    stop 0

contains

    subroutine cleanup()
        type(error_context_t) :: e
        inquire(file=a_path, exist=exists)
        if (exists) call safe_remove_file(a_path, e)
        inquire(file=b_path, exist=exists)
        if (exists) call safe_remove_file(b_path, e)
        inquire(file=tmpdir, exist=exists)
        if (exists) call safe_remove_directory(tmpdir, e)
    end subroutine cleanup

end program test_engine_delegation_minimal

