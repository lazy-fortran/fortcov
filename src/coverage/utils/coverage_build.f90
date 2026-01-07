module coverage_build
    !! Build system coordination extracted from coverage_workflows
    !!
    !! Focused on build system detection and integration for coverage
    !! workflows. Provides specialized build system operations separated
    !! from test execution and other workflow functionality.
    use constants_core, only: EXIT_SUCCESS
    use config_core, only: config_t
    use build_detector_core, only: build_system_info_t
    use build_system_validation, only: detect_and_validate_build_system, &
                                       report_unknown_build_system, &
                                       report_build_tool_unavailable
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, &
                                   ERROR_INVALID_CONFIG, clear_error_context
    implicit none
    private

    public :: detect_and_validate_build_system_for_coverage
    public :: get_coverage_build_command

contains

    function detect_and_validate_build_system_for_coverage(config, build_info, &
                                                           error_ctx) result(exit_code)
        !! Detect build system specifically for coverage analysis
        !!
        !! Extended version of build system detection that includes coverage-specific
        !! validation and configuration setup for coverage workflows.
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        integer :: exit_code

        exit_code = EXIT_SUCCESS

        ! Unified detection/validation and reporting
        exit_code = detect_and_validate_build_system(config, build_info, error_ctx, '.')
        if (exit_code /= EXIT_SUCCESS) return
        if (build_info%system_type == 'unknown') then
            call report_unknown_build_system(config)
            return
        end if
        if (.not. build_info%tool_available) then
            call report_build_tool_unavailable(config, build_info)
            return
        end if

        ! Validate coverage support for detected build system
        call validate_coverage_support(build_info, config, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            exit_code = 3  ! Coverage support validation failed
            return
        end if
        ! Successful detection already reported by detect_and_validate_build_system
        ! Avoid duplicate reporting here

    end function detect_and_validate_build_system_for_coverage

    function get_coverage_build_command(build_info, config) result(command)
        !! Generate coverage-enabled build command for detected build system
        !!
        !! Creates appropriate build command with coverage flags based on
        !! the detected build system type and configuration.
        type(build_system_info_t), intent(in) :: build_info
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: command
        select case (trim(build_info%system_type))
        case ('fpm')
            command = get_fpm_coverage_command(config)
        case ('cmake')
            command = get_cmake_coverage_command(config)
        case ('make')
            command = get_make_coverage_command(config)
        case ('meson')
            command = get_meson_coverage_command(config)
        case default
            ! Unknown system, return test command as fallback
            command = build_info%test_command
        end select

    end function get_coverage_build_command

    function get_fpm_coverage_command(config) result(command)
        !! Generate FPM build command with coverage flags
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: command

        ! Base FPM build command with coverage flags
        command = 'fpm build --flag "-fprofile-arcs -ftest-coverage"'

    end function get_fpm_coverage_command

    function get_cmake_coverage_command(config) result(command)
        !! Generate CMake build command with coverage flags
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: command
        character(len=2), parameter :: shell_and = achar(38)//achar(38)

        ! Configure then build with coverage enabled (out-of-source default).
        command = 'cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug '// &
                  '-DENABLE_COVERAGE=ON '//shell_and//' cmake --build build'

        ! Add parallel build if supported
        command = command//' --parallel'

    end function get_cmake_coverage_command

    function get_make_coverage_command(config) result(command)
        !! Generate Make build command with coverage flags
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: command

        ! Base Make command with coverage flags
        command = 'make COVERAGE=1 CFLAGS="-fprofile-arcs -ftest-coverage" '// &
                  'FFLAGS="-fprofile-arcs -ftest-coverage"'

    end function get_make_coverage_command

    function get_meson_coverage_command(config) result(command)
        !! Generate Meson build command with coverage flags
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: command

        ! Base Meson build command with coverage
        command = 'meson compile -C builddir --werror'

        ! Coverage setup should be done during meson setup phase
        ! This assumes coverage was enabled during setup

    end function get_meson_coverage_command

    subroutine validate_coverage_support(build_info, config, error_ctx)
        !! Validate that the detected build system supports coverage analysis
        type(build_system_info_t), intent(in) :: build_info
        type(config_t), intent(in) :: config
        type(error_context_t), intent(out) :: error_ctx

        call clear_error_context(error_ctx)

        ! All currently supported build systems have coverage support
        select case (trim(build_info%system_type))
        case ('fpm', 'cmake', 'make', 'meson')
            ! Supported systems - no error
            return
        case default
            ! Unknown or unsupported system
            error_ctx%error_code = ERROR_INVALID_CONFIG
            error_ctx%message = 'Build system does not support coverage analysis: '// &
                                trim(build_info%system_type)
            return
        end select

    end subroutine validate_coverage_support

    ! Note: reporting now standardized via build_system_validation

end module coverage_build
