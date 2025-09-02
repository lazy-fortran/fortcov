program test_format_rejection_minimal
    !! Minimal negative tests for unsupported output formats and file imports
    use config_types, only: config_t
    use config_validators, only: validate_output_settings, validate_import_file
    use coverage_reporter_factory, only: create_reporter
    use coverage_reporter, only: coverage_reporter_t

    implicit none

    type(config_t) :: cfg
    logical :: ok
    character(len=256) :: msg
    integer :: failures

    class(coverage_reporter_t), allocatable :: dummy
    logical :: factory_error

    failures = 0

    ! GIVEN: default config
    call initialize_config(cfg)

    ! WHEN: an unsupported format is specified
    cfg%output_format = 'json'
    call validate_output_settings(cfg, ok, msg)
    if (ok) then
        print *, 'FAIL: validate_output_settings accepted unsupported format json'
        failures = failures + 1
    else
        print *, 'OK: unsupported format rejected: ', trim(msg)
    end if

    ! AND: the reporter factory is asked for unsupported format
    call create_reporter('html', dummy, factory_error)
    if (.not. factory_error) then
        print *, 'FAIL: factory did not flag unsupported format html'
        failures = failures + 1
    else
        print *, 'OK: factory rejected unsupported format html'
    end if

    ! AND: import of JSON is rejected
    call validate_import_file('sample.json', ok, msg)
    if (ok) then
        print *, 'FAIL: validate_import_file accepted JSON import'
        failures = failures + 1
    else
        print *, 'OK: JSON import rejected: ', trim(msg)
    end if

    if (failures == 0) then
        print *, 'OK: format rejection minimal'
    else
        error stop 1
    end if

contains

    subroutine initialize_config(config)
        type(config_t), intent(out) :: config
        ! Minimal initializer mirroring defaults used in app
        config%input_format = 'auto'
        config%output_format = 'markdown'
        config%output_path = 'coverage.md'
        config%minimum_coverage = 0.0
        config%fail_under_threshold = 0.0
        config%threads = 1
        config%verbose = .false.
        config%quiet = .true.
        config%show_help = .false.
        config%show_version = .false.
        config%validate_config_only = .false.
        config%config_file = ''
        config%enable_diff = .false.
        config%diff_baseline_file = ''
        config%diff_current_file = ''
        config%include_unchanged = .false.
        config%diff_threshold = 0.0
        config%import_file = ''
        config%keep_gcov_files = .false.
        config%gcov_args = ''
        config%strict_mode = .false.
        config%zero_configuration_mode = .false.
        config%auto_discovery = .true.
        config%auto_test_execution = .false.
        config%test_timeout_seconds = 300
        config%max_files = 1000
    end subroutine initialize_config

end program test_format_rejection_minimal
