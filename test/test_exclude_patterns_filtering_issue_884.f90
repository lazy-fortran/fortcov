program test_exclude_patterns_filtering_issue_884
    !! Ensure exclude patterns do not remove valid gcov files (Issue #884)
    use iso_fortran_env, only: output_unit
    use config_core, only: config_t, parse_config
    use coverage_workflows_discovery, only: filter_coverage_files_by_patterns
    implicit none

    type(config_t) :: config
    character(len=:), allocatable :: filtered(:)
    character(len=256) :: files(3)
    character(len=:), allocatable :: args(:)
    logical :: success
    character(len=256) :: error_message
    integer :: i, keep_count
    logical :: has_main

    write(output_unit, '(A)') 'Running Issue #884: exclude pattern filtering correctness'

    ! Build CLI-like args with exclude patterns only
    allocate(character(len=64) :: args(2))
    args(1) = '--exclude=build/*'
    args(2) = '--exclude=test/*'

    call parse_config(args, config, success, error_message)
    if (.not. success) then
        write(output_unit, '(A)') 'Config parsing failed: ' // trim(error_message)
        stop 1
    end if

    ! Simulated discovered coverage files
    files(1) = 'main.f90.gcov'      ! Should be kept
    files(2) = 'build/foo.gcov'     ! Should be excluded by build/*
    files(3) = 'test/bar.gcov'      ! Should be excluded by test/*

    filtered = filter_coverage_files_by_patterns(files, config)

    if (.not. allocated(filtered)) then
        write(output_unit, '(A)') 'FAIL: Filtering returned unallocated array'
        stop 1
    end if

    keep_count = size(filtered)
    has_main = .false.
    do i = 1, keep_count
        if (trim(filtered(i)) == 'main.f90.gcov') has_main = .true.
        if (index(filtered(i), 'build/') == 1 .or. index(filtered(i), 'test/') == 1) then
            write(output_unit, '(A)') 'FAIL: Excluded path leaked through filtering: ' // trim(filtered(i))
            stop 1
        end if
    end do

    if (.not. has_main) then
        write(output_unit, '(A)') 'FAIL: main.f90.gcov was incorrectly excluded'
        stop 1
    end if

    write(output_unit, '(A)') 'PASS: Exclude pattern filtering keeps valid gcov files'

end program test_exclude_patterns_filtering_issue_884

