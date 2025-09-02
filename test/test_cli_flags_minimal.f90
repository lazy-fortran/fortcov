program test_cli_flags_minimal
    use config_core, only: config_t, parse_config
    implicit none

    type(config_t) :: cfg
    logical :: ok
    character(len=256) :: err
    character(len=:), allocatable :: args(:)

    allocate(character(len=64) :: args(6))
    args = [ character(len=64) :: &
        '--source=src', &
        '--output=coverage.md', &
        '--fail-under=75', &
        '--discover-and-gcov', &
        '*.gcov', &
        'more.gcov' ]

    call parse_config(args, cfg, ok, err)
    if (.not. ok) then
        print *, 'parse_config failed: ', trim(err)
        stop 1
    end if

    if (.not. allocated(cfg%source_paths)) stop 2
    if (size(cfg%source_paths) < 1) stop 3
    if (trim(cfg%source_paths(1)) /= 'src') stop 4

    if (.not. allocated(cfg%output_path)) stop 5
    if (trim(cfg%output_path) /= 'coverage.md') stop 6

    if (abs(cfg%fail_under_threshold - 75.0) > 1.0e-6) stop 7

    ! --discover-and-gcov disables generic auto-discovery to route to gcov path
    if (cfg%auto_discovery) stop 8

    print *, 'OK: cli flags minimal'
end program test_cli_flags_minimal
