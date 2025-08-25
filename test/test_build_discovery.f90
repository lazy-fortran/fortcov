program test_build_discovery
    !! Test Build Discovery Module Tests (Issue #277 - Part 1)
    !!
    !! Tests for the test_build_auto_discovery module focusing on
    !! build system detection and test command configuration.

    use test_build_auto_discovery
    use config_types, only: config_t
    use iso_fortran_env, only: output_unit
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0

    write(output_unit, '(A)') 'Running test build discovery tests...'
    write(output_unit, '(A)') ''

    call test_auto_discover_test_build_with_fpm()
    call test_auto_discover_test_build_with_cmake()
    call test_auto_discover_test_build_with_make()
    call test_auto_discover_test_build_with_meson()
    call test_auto_discover_test_build_unknown_system()

    write(output_unit, '(A)') ''
    write(output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed_count, ' / ', &
                                        test_count, ' tests passed'
    
    if (passed_count /= test_count) then
        write(output_unit, '(A)') 'Some tests failed!'
        stop 1
    end if

contains

    subroutine test_auto_discover_test_build_with_fpm()
        !! Given an FPM project structure
        !! When auto_discover_test_build is called
        !! Then it should detect FPM and configure test command
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 1: Auto-discover test build with FPM'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_fpm_project()
        
        call auto_discover_test_build('test_temp_dir', config, result)
        
        call assert_true(result%success, 'FPM detection succeeded')
        call assert_equal_string(result%build_system, 'fpm', 'Build system detected')
        call assert_true(result%tool_available, 'FPM tool available')
        call assert_true(len_trim(result%test_command) > 0, 'Test command configured')
        
        call cleanup_mock_project()
    end subroutine test_auto_discover_test_build_with_fpm

    subroutine test_auto_discover_test_build_with_cmake()
        !! Given a CMake project structure
        !! When auto_discover_test_build is called
        !! Then it should detect CMake and configure test command
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 2: Auto-discover test build with CMake'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_cmake_project()
        
        call auto_discover_test_build('test_temp_dir', config, result)
        
        call assert_true(result%success, 'CMake detection succeeded')
        call assert_equal_string(result%build_system, 'cmake', 'Build system detected')
        call assert_true(result%tool_available, 'CMake tool available')
        call assert_true(len_trim(result%test_command) > 0, 'Test command configured')
        
        call cleanup_mock_project()
    end subroutine test_auto_discover_test_build_with_cmake

    subroutine test_auto_discover_test_build_with_make()
        !! Given a Make project structure  
        !! When auto_discover_test_build is called
        !! Then it should detect Make and configure test command
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 3: Auto-discover test build with Make'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_make_project()
        
        call auto_discover_test_build('test_temp_dir', config, result)
        
        call assert_true(result%success, 'Make detection succeeded')
        call assert_equal_string(result%build_system, 'make', 'Build system detected')
        call assert_true(result%tool_available, 'Make tool available')
        call assert_true(len_trim(result%test_command) > 0, 'Test command configured')
        
        call cleanup_mock_project()
    end subroutine test_auto_discover_test_build_with_make

    subroutine test_auto_discover_test_build_with_meson()
        !! Given a Meson project structure
        !! When auto_discover_test_build is called  
        !! Then it should detect Meson and configure test command
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 4: Auto-discover test build with Meson'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        call create_mock_meson_project()
        
        call auto_discover_test_build('test_temp_dir', config, result)
        
        call assert_true(result%success, 'Meson detection succeeded')
        call assert_equal_string(result%build_system, 'meson', 'Build system detected')
        call assert_true(result%tool_available, 'Meson tool available')
        call assert_true(len_trim(result%test_command) > 0, 'Test command configured')
        
        call cleanup_mock_project()
    end subroutine test_auto_discover_test_build_with_meson

    subroutine test_auto_discover_test_build_unknown_system()
        !! Given no recognizable build system
        !! When auto_discover_test_build is called
        !! Then it should fail gracefully with guidance
        type(config_t) :: config
        type(test_build_result_t) :: result
        
        write(output_unit, '(A)') 'Test 5: Auto-discover test build with unknown system'
        
        call initialize_default_config(config)
        config%auto_discovery = .true.
        
        ! No mock build files created
        
        call auto_discover_test_build('test_temp_dir', config, result)
        
        call assert_false(result%success, 'Unknown system handled')
        call assert_equal_string(result%build_system, 'unknown', 'Build system unknown')
        call assert_true(len_trim(result%guidance_message) > 0, 'Guidance provided')
        
    end subroutine test_auto_discover_test_build_unknown_system

    ! Mock creation and cleanup subroutines
    
    subroutine create_mock_fpm_project()
        call execute_command_line('mkdir -p test_temp_dir')
        call execute_command_line('touch test_temp_dir/fpm.toml')
    end subroutine create_mock_fpm_project

    subroutine create_mock_cmake_project()
        call execute_command_line('mkdir -p test_temp_dir')
        call execute_command_line('touch test_temp_dir/CMakeLists.txt')
    end subroutine create_mock_cmake_project

    subroutine create_mock_make_project()
        call execute_command_line('mkdir -p test_temp_dir')
        call execute_command_line('touch test_temp_dir/Makefile')
    end subroutine create_mock_make_project

    subroutine create_mock_meson_project()
        call execute_command_line('mkdir -p test_temp_dir')
        call execute_command_line('touch test_temp_dir/meson.build')
    end subroutine create_mock_meson_project

    subroutine cleanup_mock_project()
        call execute_command_line('rm -rf test_temp_dir')
    end subroutine cleanup_mock_project

    ! Test assertion helpers
    
    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            passed_count = passed_count + 1
            write(output_unit, '(A,A)') '  ✓ ', description
        else
            write(output_unit, '(A,A)') '  ✗ ', description
        end if
    end subroutine assert_true

    subroutine assert_false(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        call assert_true(.not. condition, description)
    end subroutine assert_false

    subroutine assert_equal_string(actual, expected, description)
        character(len=*), intent(in) :: actual, expected, description
        
        call assert_true(trim(actual) == trim(expected), description)
    end subroutine assert_equal_string

    subroutine initialize_default_config(config)
        !! Initialize config with default values
        type(config_t), intent(out) :: config
        
        config%auto_discovery = .false.
        config%auto_test_execution = .true.
        config%test_timeout_seconds = 30
        config%verbose = .false.
        config%gcov_executable = 'gcov'
    end subroutine initialize_default_config

end program test_build_discovery