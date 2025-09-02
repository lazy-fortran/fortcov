program test_threshold_minimal
    use coverage_data_core, only: coverage_file_t, coverage_data_t, &
                                   file_init_with_lines, data_init_with_files
    use coverage_types, only: coverage_line_t
    use coverage_stats_reporter, only: calculate_coverage_statistics, &
                                       apply_threshold_validation, line_coverage_stats_t
    use config_types, only: config_t
    implicit none

    type(coverage_line_t), allocatable :: lines(:)
    type(coverage_file_t) :: file
    type(coverage_file_t), allocatable :: files(:)
    type(coverage_data_t) :: data
    type(line_coverage_stats_t) :: stats
    type(config_t) :: cfg
    integer :: rc, i

    allocate(lines(10))
    do i = 1, 10
        call lines(i)%init('src/mod.f90', i, merge(1, 0, i <= 6), .true.)
    end do
    call file_init_with_lines(file, '0:Source:src/mod.f90', lines)
    allocate(files(1)); files(1) = file
    call data_init_with_files(data, files)

    call calculate_coverage_statistics(data, stats)

    cfg%quiet = .true.
    cfg%fail_under_threshold = 70.0  ! 60% should fail under 70%
    rc = apply_threshold_validation(stats, cfg)
    if (rc == 0) then
        print *, 'Expected threshold failure, got success'
        stop 1
    end if

    cfg%quiet = .true.
    cfg%fail_under_threshold = 50.0  ! 60% should pass at 50%
    rc = apply_threshold_validation(stats, cfg)
    if (rc /= 0) then
        print *, 'Expected threshold pass, got failure code=', rc
        stop 2
    end if

    print *, 'OK: threshold minimal'
end program test_threshold_minimal
