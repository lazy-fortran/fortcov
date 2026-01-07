program test_cmake_coverage_command_1252
    !! Test for Issue #1252: CMake coverage build command is configured correctly.

    use, intrinsic :: iso_fortran_env, only: output_unit
    use build_detector_core, only: build_system_info_t
    use config_defaults_core, only: initialize_default_config
    use coverage_build, only: get_coverage_build_command
    use config_types, only: config_t
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_cmake_coverage_command()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_cmake_coverage_command()
        type(config_t) :: config
        type(build_system_info_t) :: build_info
        character(len=:), allocatable :: command
        character(len=2), parameter :: shell_and = achar(38)//achar(38)
        character(len=*), parameter :: expected = &
                                       'cmake -S . -B build '// &
                                       '-DCMAKE_BUILD_TYPE=Debug '// &
                                       '-DENABLE_COVERAGE=ON '// &
                                       shell_and//' cmake --build build '// &
                                       '--parallel'

        tests = tests + 1

        call initialize_default_config(config)
        build_info%system_type = 'cmake'

        command = get_coverage_build_command(build_info, config)

        if (trim(command) == expected) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] cmake coverage build command'
        else
            write (output_unit, '(A)') '  [FAIL] cmake coverage build command'
            write (output_unit, '(A)') '    Expected: '//expected
            write (output_unit, '(A)') '    Actual:   '//trim(command)
        end if
    end subroutine test_cmake_coverage_command

end program test_cmake_coverage_command_1252
