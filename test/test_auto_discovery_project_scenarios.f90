program test_auto_discovery_project_scenarios
    !! Auto-Discovery Project Scenario Tests
    !!
    !! This module validates auto-discovery functionality across realistic
    !! project scenarios for different build systems:
    !! - FPM project scenarios with comprehensive structure validation
    !! - CMake project scenarios with build configuration detection
    !! - Make project scenarios with custom build target validation
    !! 
    !! Each scenario tests the complete detection and configuration pipeline
    !! to ensure auto-discovery works correctly across diverse project types.
    
    use iso_fortran_env, only: output_unit, error_unit
    use build_detector_core, only: detect_build_system, build_system_info_t
    use test_auto_discovery_shared_utilities
    implicit none
    
    character(len=256) :: base_test_dir = "test_auto_discovery_scenarios_workspace"
    
    write(output_unit, '(A)') "============================================="
    write(output_unit, '(A)') "  Auto-Discovery Project Scenario Tests   "
    write(output_unit, '(A)') "============================================="
    write(output_unit, '(A)') ""
    
    ! Setup base test workspace
    call setup_test_workspace(base_test_dir)
    
    ! Realistic project scenarios
    call test_fpm_project_scenario()
    call test_cmake_project_scenario()
    call test_make_project_scenario()
    
    ! Cleanup
    call cleanup_test_workspace(base_test_dir)
    
    ! Print summary
    call print_test_summary("PROJECT SCENARIOS")
    
contains

    subroutine test_fpm_project_scenario()
        !! Tests a realistic FPM project scenario with comprehensive validation
        
        character(len=512) :: workspace_path
        type(build_system_info_t) :: build_info
        logical :: detected
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== FPM PROJECT SCENARIO ==="
        
        workspace_path = get_discovery_workspace_path(trim(base_test_dir) // "_fmp")
        ! call execute_command_line( ! CI-disabled:'mkdir -p ' // trim(workspace_path))
        
        call create_fpm_test_project(workspace_path)
        
        ! Test FPM project detection and configuration
        call detect_build_system_with_error_handling(workspace_path, &
                                                     build_info, detected)
        call assert_test(detected, "FPM project scenario detection", &
                        "Should detect FPM project")
        
        if (detected) then
            call assert_test(trim(build_info%system_type) == "fpm", &
                            "FPM system type identification", &
                            "Should identify as fpm system")
            call assert_test(len_trim(build_info%test_command) > 0, &
                            "FPM test command generated", &
                            "Should have valid test command")
            call assert_test(index(build_info%test_command, "fpm test") > 0, &
                            "FPM test command format", &
                            "Should contain 'fpm test' command")
        end if
        
        call test_fpm_project_structure_validation(workspace_path)
        
        ! call execute_command_line( ! CI-disabled:'rm -rf ' // trim(workspace_path))
        
    end subroutine test_fpm_project_scenario

    subroutine test_fpm_project_structure_validation(workspace_path)
        !! Validates FPM project structure detection
        character(len=*), intent(in) :: workspace_path
        integer :: unit_number, iostat
        logical :: file_exists
        
        ! Validate fpm.toml exists and is readable
        inquire(file=trim(workspace_path) // '/fpm.toml', exist=file_exists)
        call assert_test(file_exists, "FPM configuration file exists", &
                        "fpm.toml should exist")
        
        if (file_exists) then
            open(newunit=unit_number, file=trim(workspace_path) // '/fpm.toml', &
                 status='old', iostat=iostat, action='read')
            call assert_test(iostat == 0, "FPM configuration readable", &
                            "fpm.toml should be readable")
            if (iostat == 0) close(unit_number)
        end if
        
        ! Validate source structure
        inquire(file=trim(workspace_path) // '/src', exist=file_exists)
        call assert_test(file_exists, "FPM source directory exists", &
                        "src/ directory should exist")
        
        inquire(file=trim(workspace_path) // '/src/main.f90', exist=file_exists)
        call assert_test(file_exists, "FPM source file exists", &
                        "src/main.f90 should exist")
        
    end subroutine test_fpm_project_structure_validation

    subroutine test_cmake_project_scenario()
        !! Tests a realistic CMake project scenario with build configuration
        
        character(len=512) :: workspace_path
        type(build_system_info_t) :: build_info
        logical :: detected
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== CMAKE PROJECT SCENARIO ==="
        
        workspace_path = get_discovery_workspace_path(trim(base_test_dir) // "_cmake")
        ! call execute_command_line( ! CI-disabled:'mkdir -p ' // trim(workspace_path))
        
        call create_cmake_test_project(workspace_path)
        
        ! Test CMake project detection and configuration
        call detect_build_system_with_error_handling(workspace_path, &
                                                     build_info, detected)
        call assert_test(detected, "CMake project scenario detection", &
                        "Should detect CMake project")
        
        if (detected) then
            call assert_test(trim(build_info%system_type) == "cmake", &
                            "CMake system type identification", &
                            "Should identify as cmake system")
            call assert_test(len_trim(build_info%test_command) > 0, &
                            "CMake test command generated", &
                            "Should have valid test command")
        end if
        
        call test_cmake_project_structure_validation(workspace_path)
        
        ! call execute_command_line( ! CI-disabled:'rm -rf ' // trim(workspace_path))
        
    end subroutine test_cmake_project_scenario

    subroutine test_cmake_project_structure_validation(workspace_path)
        !! Validates CMake project structure detection
        character(len=*), intent(in) :: workspace_path
        integer :: unit_number, iostat
        logical :: file_exists
        character(len=256) :: line
        
        ! Validate CMakeLists.txt exists and contains required content
        inquire(file=trim(workspace_path) // '/CMakeLists.txt', exist=file_exists)
        call assert_test(file_exists, "CMake configuration file exists", &
                        "CMakeLists.txt should exist")
        
        if (file_exists) then
            open(newunit=unit_number, file=trim(workspace_path) // '/CMakeLists.txt', &
                 status='old', iostat=iostat, action='read')
            call assert_test(iostat == 0, "CMake configuration readable", &
                            "CMakeLists.txt should be readable")
            
            if (iostat == 0) then
                read(unit_number, '(A)', iostat=iostat) line
                call assert_test(index(line, "cmake_minimum_required") > 0, &
                                "CMake minimum version specified", &
                                "Should contain cmake_minimum_required")
                close(unit_number)
            end if
        end if
        
    end subroutine test_cmake_project_structure_validation

    subroutine test_make_project_scenario()
        !! Tests a realistic Make project scenario with target validation
        
        character(len=512) :: workspace_path
        type(build_system_info_t) :: build_info
        logical :: detected
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== MAKE PROJECT SCENARIO ==="
        
        workspace_path = get_discovery_workspace_path(trim(base_test_dir) // "_make")
        ! call execute_command_line( ! CI-disabled:'mkdir -p ' // trim(workspace_path))
        
        call create_make_test_project(workspace_path)
        
        ! Test Make project detection and configuration
        call detect_build_system_with_error_handling(workspace_path, &
                                                     build_info, detected)
        call assert_test(detected, "Make project scenario detection", &
                        "Should detect Make project")
        
        if (detected) then
            call assert_test(trim(build_info%system_type) == "make", &
                            "Make system type identification", &
                            "Should identify as make system")
            call assert_test(len_trim(build_info%test_command) > 0, &
                            "Make test command generated", &
                            "Should have valid test command")
        end if
        
        call test_make_project_structure_validation(workspace_path)
        
        ! call execute_command_line( ! CI-disabled:'rm -rf ' // trim(workspace_path))
        
    end subroutine test_make_project_scenario

    subroutine test_make_project_structure_validation(workspace_path)
        !! Validates Make project structure detection
        character(len=*), intent(in) :: workspace_path
        integer :: unit_number, iostat
        logical :: file_exists
        character(len=256) :: line
        
        ! Validate Makefile exists and contains required targets
        inquire(file=trim(workspace_path) // '/Makefile', exist=file_exists)
        call assert_test(file_exists, "Make configuration file exists", &
                        "Makefile should exist")
        
        if (file_exists) then
            open(newunit=unit_number, file=trim(workspace_path) // '/Makefile', &
                 status='old', iostat=iostat, action='read')
            call assert_test(iostat == 0, "Make configuration readable", &
                            "Makefile should be readable")
            
            if (iostat == 0) then
                read(unit_number, '(A)', iostat=iostat) line
                call assert_test(index(line, "all:") > 0, &
                                "Make all target exists", &
                                "Should contain all: target")
                close(unit_number)
            end if
        end if
        
    end subroutine test_make_project_structure_validation

    subroutine detect_build_system_with_error_handling(workspace_path, &
                                                       build_info, detected)
        !! Helper subroutine for build system detection with error handling
        character(len=*), intent(in) :: workspace_path
        type(build_system_info_t), intent(out) :: build_info
        logical, intent(out) :: detected
        
        block
            use error_handling_core, only: error_context_t
            type(error_context_t) :: error_ctx
            call detect_build_system(workspace_path, build_info, error_ctx)
            detected = (error_ctx%error_code == 0)
        end block
    end subroutine detect_build_system_with_error_handling

    subroutine print_test_summary(test_suite_name)
        !! Print comprehensive test summary
        character(len=*), intent(in) :: test_suite_name
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "============================================="
        write(*, '(A,A,A,I0,A,I0,A)') "AUTO-DISCOVERY ", trim(test_suite_name), &
                                      ": ", shared_passed_tests, "/", &
                                      shared_test_count, " tests passed"
        
        if (shared_all_tests_passed) then
            write(output_unit, '(A)') "✅ " // trim(test_suite_name) // &
                                      " FULLY VALIDATED"
        else
            write(output_unit, '(A)') "❌ " // trim(test_suite_name) // &
                                      " VALIDATION FAILED"
            stop 1
        end if
    end subroutine print_test_summary

end program test_auto_discovery_project_scenarios
