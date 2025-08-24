module test_build_auto_discovery
    !! Test Build Auto-Discovery Module (Issue #277 - Part 1)
    !!
    !! Handles auto-discovery of test build capabilities using build system
    !! detection. This is the main entry point for test build discovery.
    !!
    !! Key Features:
    !! - Auto-discovery of test build capabilities using build system detection
    !! - Build system validation and tool availability checking
    !! - Test command configuration with coverage flags
    !! - Graceful error handling and user guidance

    use config_types, only: config_t
    use build_system_detector, only: build_system_info_t, detect_build_system, &
                                     get_coverage_test_command, &
                                     validate_build_tool_available
    use error_handling, only: error_context_t, ERROR_SUCCESS, &
                              ERROR_INVALID_CONFIG, clear_error_context
    use file_utils, only: file_exists
    use string_utils, only: trim_string
    use foundation_constants, only: EXIT_SUCCESS, EXIT_FAILURE
    implicit none
    private

    ! Public interface
    public :: auto_discover_test_build
    public :: test_build_result_t

    ! Result type
    type :: test_build_result_t
        logical :: success = .false.
        character(len=20) :: build_system = 'unknown'
        character(len=512) :: test_command = ''
        logical :: tool_available = .false.
        character(len=256) :: error_message = ''
        character(len=512) :: guidance_message = ''
    end type test_build_result_t

contains

    subroutine auto_discover_test_build(project_path, config, result)
        !! Auto-discover test build capabilities
        !!
        !! Detects build system and determines appropriate test command
        !! with coverage flags. Validates that build tools are available
        !! and provides guidance when tools are missing or unavailable.
        !!
        !! Args:
        !!   project_path: Project root directory to analyze
        !!   config: Configuration for auto-discovery behavior
        !!   result: Populated with discovery results and guidance
        
        character(len=*), intent(in) :: project_path
        type(config_t), intent(in) :: config  
        type(test_build_result_t), intent(out) :: result

        ! Initialize result structure
        call initialize_test_build_result(result)

        ! Skip if auto-discovery disabled
        if (.not. config%auto_discovery) then
            result%guidance_message = 'Auto-discovery disabled in configuration'
            return
        end if

        ! Execute main discovery workflow
        call execute_build_discovery_workflow(project_path, result)

    end subroutine auto_discover_test_build

    subroutine initialize_test_build_result(result)
        !! Initialize test build result structure
        type(test_build_result_t), intent(out) :: result
        
        result%success = .false.
        result%build_system = 'unknown'
        result%test_command = ''
        result%tool_available = .false.
        result%error_message = ''
        result%guidance_message = ''
    end subroutine initialize_test_build_result

    subroutine execute_build_discovery_workflow(project_path, result)
        !! Execute the main build system discovery workflow
        character(len=*), intent(in) :: project_path
        type(test_build_result_t), intent(inout) :: result

        type(build_system_info_t) :: build_info
        logical :: path_valid

        ! Validate project path
        call validate_project_path(project_path, path_valid, result%error_message)
        if (.not. path_valid) return

        ! Detect and process build system
        call detect_build_system(project_path, build_info)
        result%build_system = trim(build_info%system_type)

        if (trim(build_info%system_type) /= 'unknown') then
            call process_detected_build_system(build_info, result)
        else
            call handle_unknown_build_system(result)
        end if
    end subroutine execute_build_discovery_workflow

    subroutine process_detected_build_system(build_info, result)
        !! Process a detected build system
        type(build_system_info_t), intent(in) :: build_info
        type(test_build_result_t), intent(inout) :: result

        ! Validate build tool is available
        call validate_build_tool_available(build_info, result%tool_available)
        
        if (result%tool_available) then
            call configure_test_command(build_info, result)
        else
            call handle_unavailable_build_tool(build_info, result)
        end if
    end subroutine process_detected_build_system

    subroutine validate_project_path(project_path, valid, error_message)
        !! Validate that project path exists and is accessible
        character(len=*), intent(in) :: project_path
        logical, intent(out) :: valid
        character(len=*), intent(out) :: error_message

        character(len=256) :: resolved_path

        valid = .false.
        error_message = ''

        ! Resolve and validate path
        resolved_path = trim(project_path)
        if (len_trim(resolved_path) == 0) then
            resolved_path = '.'
        end if

        ! Check if path exists
        inquire(file=trim(resolved_path), exist=valid)
        if (.not. valid) then
            write(error_message, '(A,A,A)') &
                'Project path does not exist: ', trim(resolved_path), &
                '. Please verify the path is correct and accessible.'
            return
        end if

        ! Additional validation could be added here
        valid = .true.

    end subroutine validate_project_path

    subroutine provide_build_system_guidance(result)
        !! Provide guidance for setting up build systems
        type(test_build_result_t), intent(inout) :: result
        
        result%guidance_message = 'Create build configuration: fpm.toml (FPM), ' // &
                                 'CMakeLists.txt (CMake), Makefile (Make), or meson.build (Meson)'
    end subroutine provide_build_system_guidance

    subroutine handle_unknown_build_system(result)
        !! Handle case when no build system is detected
        type(test_build_result_t), intent(inout) :: result
        
        result%success = .false.
        result%error_message = 'No build system detected in project directory'
        call provide_build_system_guidance(result)
        
    end subroutine handle_unknown_build_system

    subroutine handle_unavailable_build_tool(build_info, result)
        !! Handle case when build tool is not available
        type(build_system_info_t), intent(in) :: build_info
        type(test_build_result_t), intent(inout) :: result
        
        result%success = .false.
        write(result%error_message, '(A,A,A)') &
            'Build tool not available: ', trim(build_info%system_type), ' not found in PATH'
        result%guidance_message = 'Install ' // trim(build_info%system_type) // &
            ' build tool and ensure it is available in your PATH environment variable'
    end subroutine handle_unavailable_build_tool

    subroutine configure_test_command(build_info, result)
        !! Configure test command with coverage flags
        type(build_system_info_t), intent(in) :: build_info
        type(test_build_result_t), intent(inout) :: result
        
        type(error_context_t) :: error_ctx
        
        call get_coverage_test_command(build_info%system_type, result%test_command, error_ctx)
        
        if (error_ctx%error_code == ERROR_SUCCESS) then
            result%success = .true.
        else
            result%success = .false.
            result%error_message = trim(error_ctx%message)
            result%guidance_message = &
                'Failed to configure test command for ' // trim(build_info%system_type)
        end if
    end subroutine configure_test_command

end module test_build_auto_discovery