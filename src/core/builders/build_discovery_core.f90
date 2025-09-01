module build_discovery_core
    !! Test Build Auto-Discovery Module
    !!
    !! Provides comprehensive test build detection and configuration for
    !! coverage analysis. Integrates with build_system_detector to identify
    !! available build systems and configure appropriate test commands with
    !! coverage flags.
    !!
    !! Key Features:
    !! - Automatic build system detection (FPM, CMake, Make, Meson)
    !! - Coverage-enabled test command generation
    !! - Build tool availability validation
    !! - Comprehensive error reporting and user guidance
    !!
    !! Integration:
    !! - Uses build_system_detector for core build system identification
    !! - Provides test_build_result_t for standardized result reporting
    !! - Integrates with auto_discovery_utilities workflow orchestration

    use build_detector_core, only: build_system_info_t, get_coverage_test_command
    use build_system_validation, only: detect_and_validate_build_system
    use constants_core, only: EXIT_SUCCESS
    use config_types, only: config_t
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context
    use path_security, only: validate_path_security
    use file_utilities, only: file_exists
    implicit none
    private
    
    ! Public interface
    public :: test_build_result_t
    public :: auto_discover_test_build
    
    ! Test build result type with comprehensive status tracking
    type :: test_build_result_t
        logical :: success = .false.
        character(len=512) :: error_message = ''
        character(len=256) :: test_command = ''
        logical :: has_coverage_support = .false.
        character(len=20) :: detected_build_system = 'unknown'
        character(len=256) :: build_file = ''
        logical :: tool_available = .false.
    end type test_build_result_t
    
    ! Internal constants
    integer, parameter :: MAX_PATH_LEN = 4096
    
contains
    
    subroutine auto_discover_test_build(directory, config, result)
        !! Auto-discover test build capabilities in project directory
        !!
        !! Performs comprehensive analysis of project directory to detect
        !! available build systems, validate build tool availability, and
        !! generate appropriate test commands with coverage instrumentation.
        !!
        !! Workflow:
        !! 1. Validate and secure input directory path
        !! 2. Detect available build system using build_system_detector
        !! 3. Generate coverage-enabled test command
        !! 4. Validate build tool availability
        !! 5. Return comprehensive result with guidance
        !!
        !! Args:
        !!   directory: Project directory to analyze
        !!   config: Configuration context (not used in current implementation)
        !!   result: Comprehensive discovery results and status
        
        character(len=*), intent(in) :: directory
        type(config_t), intent(in) :: config
        type(test_build_result_t), intent(out) :: result
        
        type(build_system_info_t) :: build_info
        type(error_context_t) :: error_ctx
        
        ! Initialize result with defaults
        call initialize_result(result)
        
        ! Preserve specific local validations for message compatibility
        if (len_trim(directory) == 0) then
            call handle_invalid_directory('Empty directory path provided', result)
            return
        end if
        if (len_trim(directory) > MAX_PATH_LEN) then
            call handle_invalid_directory('Directory path too long', result)
            return
        end if

        ! Unified: validate path, detect and standardize reporting
        if (detect_and_validate_build_system(config, build_info, error_ctx, directory) /= EXIT_SUCCESS) then
            call handle_build_detection_error(error_ctx, result)
            return
        end if
        
        ! Populate result with build system information
        call populate_result_from_build_info(build_info, result)
        
        ! Generate coverage test command if build system detected
        if (build_info%system_type /= 'unknown') then
            call generate_coverage_command(build_info, result)
        else
            call handle_unknown_build_system(result)
        end if
        
        ! Final success determination
        call determine_discovery_success(result)
        
    end subroutine auto_discover_test_build
    
    subroutine initialize_result(result)
        !! Initialize test build result with default values
        type(test_build_result_t), intent(out) :: result
        
        result%success = .false.
        result%error_message = ''
        result%test_command = ''
        result%has_coverage_support = .false.
        result%detected_build_system = 'unknown'
        result%build_file = ''
        result%tool_available = .false.
        
    end subroutine initialize_result
    
    subroutine populate_result_from_build_info(build_info, result)
        !! Populate result with information from build system detection
        type(build_system_info_t), intent(in) :: build_info
        type(test_build_result_t), intent(inout) :: result
        
        result%detected_build_system = build_info%system_type
        result%build_file = build_info%build_file
        result%tool_available = build_info%tool_available
        
    end subroutine populate_result_from_build_info
    
    subroutine generate_coverage_command(build_info, result)
        !! Generate coverage-enabled test command for detected build system
        type(build_system_info_t), intent(in) :: build_info
        type(test_build_result_t), intent(inout) :: result
        
        type(error_context_t) :: error_ctx
        character(len=256) :: coverage_command
        
        call get_coverage_test_command(build_info%system_type, &
                                      coverage_command, error_ctx)
        
        if (error_ctx%error_code == ERROR_SUCCESS) then
            result%test_command = coverage_command
            result%has_coverage_support = .true.
        else
            result%has_coverage_support = .false.
            result%error_message = 'Failed to generate coverage command: ' // &
                                  trim(error_ctx%message)
        end if
        
    end subroutine generate_coverage_command
    
    subroutine determine_discovery_success(result)
        !! Determine overall discovery success based on result components
        type(test_build_result_t), intent(inout) :: result
        
        ! Success criteria:
        ! 1. Build system detected (not unknown)
        ! 2. Build tool available
        ! 3. Coverage command generated successfully
        
        if (result%detected_build_system /= 'unknown' .and. &
            result%tool_available .and. &
            result%has_coverage_support .and. &
            len_trim(result%test_command) > 0) then
            
            result%success = .true.
            if (len_trim(result%error_message) == 0) then
                result%error_message = 'Test build auto-discovery successful'
            end if
        else
            result%success = .false.
            if (len_trim(result%error_message) == 0) then
                call generate_failure_guidance(result)
            end if
        end if
        
    end subroutine determine_discovery_success
    
    subroutine generate_failure_guidance(result)
        !! Generate helpful guidance for discovery failures
        type(test_build_result_t), intent(inout) :: result
        
        if (result%detected_build_system == 'unknown') then
            result%error_message = 'No supported build system detected. ' // &
                'Supported: FPM (fpm.toml), CMake (CMakeLists.txt), ' // &
                'Make (Makefile), Meson (meson.build)'
        else if (.not. result%tool_available) then
            result%error_message = 'Build system detected (' // &
                trim(result%detected_build_system) // ') but tool not ' // &
                'available in PATH. Install: ' // &
                trim(result%detected_build_system)
        else if (.not. result%has_coverage_support) then
            result%error_message = 'Build system detected but failed to ' // &
                'generate coverage command for: ' // &
                trim(result%detected_build_system)
        else
            result%error_message = 'Test build auto-discovery failed for ' // &
                'unknown reason'
        end if
        
    end subroutine generate_failure_guidance
    
    ! Error handling subroutines
    subroutine handle_invalid_directory(message, result)
        !! Handle invalid directory errors
        character(len=*), intent(in) :: message
        type(test_build_result_t), intent(inout) :: result
        
        result%success = .false.
        result%error_message = 'Invalid directory: ' // trim(message)
        
    end subroutine handle_invalid_directory
    
    subroutine handle_path_security_error(error_ctx, result)
        !! Handle path security validation errors
        type(error_context_t), intent(in) :: error_ctx
        type(test_build_result_t), intent(inout) :: result
        
        result%success = .false.
        result%error_message = 'Path security validation failed: ' // &
                              trim(error_ctx%message)
        
    end subroutine handle_path_security_error
    
    subroutine handle_build_detection_error(error_ctx, result)
        !! Handle build system detection errors
        type(error_context_t), intent(in) :: error_ctx
        type(test_build_result_t), intent(inout) :: result
        
        result%success = .false.
        result%error_message = 'Build system detection failed: ' // &
                              trim(error_ctx%message)
        
    end subroutine handle_build_detection_error
    
    subroutine handle_unknown_build_system(result)
        !! Handle case where no build system is detected
        type(test_build_result_t), intent(inout) :: result
        
        result%success = .false.
        result%has_coverage_support = .false.
        result%test_command = ''
        ! Error message will be set by generate_failure_guidance
        
    end subroutine handle_unknown_build_system
    
end module build_discovery_core
