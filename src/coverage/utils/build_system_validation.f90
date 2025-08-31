module build_system_validation
    !! Build System Detection and Validation
    !! 
    !! Handles build system detection and validation for test execution.
    !! Extracted from coverage_test_executor.f90 for SRP compliance (Issue #718).
    use constants_core, only: EXIT_SUCCESS
    use config_core, only: config_t
    use build_detector_core, only: build_system_info_t, detect_build_system
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    implicit none
    private
    
    public :: detect_and_validate_build_system
    public :: report_build_system_detected, report_unknown_build_system
    public :: report_build_tool_unavailable, report_build_detection_failed

contains

    function detect_and_validate_build_system(config, build_info, error_ctx) result(exit_code)
        !! Detect build system and validate it's usable
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(out) :: build_info
        type(error_context_t), intent(out) :: error_ctx
        integer :: exit_code
        
        exit_code = EXIT_SUCCESS
        
        ! Detect build system
        call detect_build_system('.', build_info, error_ctx)
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
            print *, "❌ Build system detection failed: " // &
                     trim(error_ctx%message)
        end if
    end subroutine report_build_detection_failed
    
    subroutine report_unknown_build_system(config)
        !! Report unknown build system detected
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "⚠️  No known build system detected, skipping tests"
            print *, "   Supported: FPM, CMake, Make, Meson"
        end if
    end subroutine report_unknown_build_system
    
    subroutine report_build_tool_unavailable(config, build_info)
        !! Report build tool not available
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "⚠️  Build tool not available for " // &
                     trim(build_info%system_type) // ", skipping tests"
        end if
    end subroutine report_build_tool_unavailable
    
    subroutine report_build_system_detected(config, build_info)
        !! Report successful build system detection
        type(config_t), intent(in) :: config
        type(build_system_info_t), intent(in) :: build_info
        
        if (.not. config%quiet) then
            print *, "📦 Build system detected: " // trim(build_info%system_type)
        end if
    end subroutine report_build_system_detected

end module build_system_validation