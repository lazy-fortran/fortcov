module build_system_validation
    !! Build System Detection and Validation
    !! 
    !! Handles build system detection and validation for test execution.
    !! Extracted from coverage_test_executor.f90 for SRP compliance (Issue #718).
    use constants_core, only: EXIT_SUCCESS
    use config_core, only: config_t
    use build_detector_core, only: build_system_info_t, detect_build_system
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use path_security, only: validate_path_security
    implicit none
    private
    
    public :: detect_and_validate_build_system
    public :: report_build_system_detected, report_unknown_build_system
    public :: report_build_tool_unavailable, report_build_detection_failed

contains

    function detect_and_validate_build_system(config, build_info, error_ctx, project_path) result(exit_code)
        !! Detect build system for a project path and validate it's usable
        !!
        !! This unified helper centralizes path validation, detection,
        !! and standardized reporting to avoid duplication across callers.
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        character(len=*), intent(in), optional :: project_path
        integer :: exit_code
        
        character(len=:), allocatable :: safe_dir
        logical :: dir_exists
        
        exit_code = EXIT_SUCCESS
        
        ! Secure and validate the input directory (default to '.')
        if (present(project_path)) then
            call validate_path_security(project_path, safe_dir, error_ctx)
        else
            call validate_path_security('.', safe_dir, error_ctx)
        end if
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call report_build_detection_failed(config, error_ctx)
            exit_code = 2
            return
        end if
        
        inquire(file=safe_dir, exist=dir_exists)
        if (.not. dir_exists) then
            error_ctx%error_code = 2
            error_ctx%message = 'Project directory not found: ' // safe_dir
            call report_build_detection_failed(config, error_ctx)
            exit_code = 2
            return
        end if
        
        ! Detect build system
        call detect_build_system(safe_dir, build_info, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            call report_build_detection_failed(config, error_ctx)
            exit_code = 2  ! Build system detection failed
            return
        end if
        
        ! Handle unknown build system
        if (build_info%system_type == 'unknown') then
            call report_unknown_build_system(config)
            return  ! Skip gracefully
        end if
        
        ! Check if build tool is available
        if (.not. build_info%tool_available) then
            call report_build_tool_unavailable(config, build_info)
            return  ! Skip gracefully
        end if
        
        call report_build_system_detected(config, build_info)
        
    end function detect_and_validate_build_system
    
    subroutine report_build_detection_failed(config, error_ctx)
        !! Report build system detection failure
        type(config_t), intent(in) :: config
        type(error_context_t), intent(in) :: error_ctx
        
        if (.not. config%quiet) then
            print *, "Error: Build system detection failed: " // &
                     trim(error_ctx%message)
        end if
    end subroutine report_build_detection_failed
    
    subroutine report_unknown_build_system(config)
        !! Report unknown build system detected
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "Warning: No known build system detected, skipping tests"
            print *, "   Supported: FPM, CMake, Make, Meson"
        end if
    end subroutine report_unknown_build_system
    
    subroutine report_build_tool_unavailable(config, build_info)
        !! Report build tool not available
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "Warning: Build tool not available for " // &
                     trim(build_info%system_type) // ", skipping tests"
        end if
    end subroutine report_build_tool_unavailable
    
    subroutine report_build_system_detected(config, build_info)
        !! Report successful build system detection
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "Build system detected: " // trim(build_info%system_type)
        end if
    end subroutine report_build_system_detected

end module build_system_validation
