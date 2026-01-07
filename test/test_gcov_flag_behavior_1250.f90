program test_gcov_flag_behavior_1250
    !! Test for Issue #1250: --gcov controls gcov generation attempts.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use config_defaults_core, only: initialize_default_config
    use config_types, only: config_t
    use coverage_processor_gcov, only: should_attempt_gcov_generation
    implicit none

    type(config_t) :: config
    character(len=:), allocatable :: found_files(:)
    logical :: should_attempt

    call initialize_default_config(config)

    should_attempt = should_attempt_gcov_generation(config, found_files)
    if (.not. should_attempt) then
        write (output_unit, '(A)') '  [PASS] default auto-discovery skips gcov'
    else
        write (output_unit, '(A)') '  [FAIL] default auto-discovery triggered gcov'
    end if

    config%auto_discovery = .false.
    should_attempt = should_attempt_gcov_generation(config, found_files)
    if (should_attempt) then
        write (output_unit, '(A)') '  [PASS] --gcov requests generation'
    else
        write (output_unit, '(A)') '  [FAIL] --gcov did not request generation'
    end if

    allocate (character(len=1) :: found_files(1))
    found_files(1) = 'x'
    should_attempt = should_attempt_gcov_generation(config, found_files)
    if (.not. should_attempt) then
        write (output_unit, '(A)') '  [PASS] existing gcov skips generation'
    else
        write (output_unit, '(A)') '  [FAIL] existing gcov retriggered generation'
    end if
end program test_gcov_flag_behavior_1250
