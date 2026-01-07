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
    integer :: failures

    call initialize_default_config(config)

    failures = 0

    should_attempt = should_attempt_gcov_generation(config, found_files)
    if (.not. should_attempt) then
        write (output_unit, '(A)') '  [PASS] default auto-discovery skips gcov'
    else
        write (output_unit, '(A)') '  [FAIL] default auto-discovery triggered gcov'
        failures = failures + 1
    end if

    config%auto_discovery = .false.
    should_attempt = should_attempt_gcov_generation(config, found_files)
    if (should_attempt) then
        write (output_unit, '(A)') '  [PASS] --gcov requests generation'
    else
        write (output_unit, '(A)') '  [FAIL] --gcov did not request generation'
        failures = failures + 1
    end if

    allocate (character(len=1) :: found_files(1))
    found_files(1) = 'x'
    should_attempt = should_attempt_gcov_generation(config, found_files)
    if (.not. should_attempt) then
        write (output_unit, '(A)') '  [PASS] existing gcov skips generation'
    else
        write (output_unit, '(A)') '  [FAIL] existing gcov retriggered generation'
        failures = failures + 1
    end if

    if (failures > 0) then
        error stop 1
    end if
end program test_gcov_flag_behavior_1250
