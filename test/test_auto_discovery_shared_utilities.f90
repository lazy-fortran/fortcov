module test_auto_discovery_shared_utilities
    !! Shared utilities for auto-discovery end-to-end validation tests
    !!
    !! This module provides common test infrastructure including:
    !! - Test assertion framework with consistent output formatting
    !! - Test workspace setup and cleanup operations
    !! - Project creation utilities for different build systems
    !! - Mock gcov file generation for testing coverage parsing
    !! - Test environment detection for safe test execution
    
    use iso_fortran_env, only: output_unit
    implicit none
    
    private
    
    ! Public test statistics (shared across all test modules)
    integer, public :: shared_test_count = 0
    integer, public :: shared_passed_tests = 0
    logical, public :: shared_all_tests_passed = .true.
    
    ! Public test utilities
    public :: assert_test
    public :: setup_test_workspace
    public :: cleanup_test_workspace
    public :: create_fpm_test_project
    public :: create_cmake_test_project 
    public :: create_make_test_project
    public :: create_mock_gcov_files
    public :: create_complete_project_scenario
    public :: test_environment_detected
    
    ! Constants
    character(len=*), parameter :: DEFAULT_TEST_DIR = "test_auto_discovery_workspace"

contains

    subroutine assert_test(condition, test_name, details)
        !! Centralized test assertion with consistent formatting and statistics
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name, details
        
        shared_test_count = shared_test_count + 1
        
        if (condition) then
            shared_passed_tests = shared_passed_tests + 1
            write(output_unit, '(A)') "✅ PASS: " // trim(test_name)
        else
            shared_all_tests_passed = .false.
            write(output_unit, '(A)') "❌ FAIL: " // trim(test_name)
            write(output_unit, '(A)') "   Details: " // trim(details)
        end if
    end subroutine assert_test

    subroutine setup_test_workspace(test_dir)
        !! Creates a temporary workspace for testing auto-discovery
        character(len=*), intent(in), optional :: test_dir
        character(len=256) :: workspace_dir
        
        if (present(test_dir)) then
            workspace_dir = test_dir
        else
            workspace_dir = DEFAULT_TEST_DIR
        end if
        
        write(output_unit, '(A)') "Setting up test workspace..."
        
        ! Clean any existing workspace
        call execute_command_line('rm -rf ' // trim(workspace_dir))
        call execute_command_line('mkdir -p ' // trim(workspace_dir))
        call execute_command_line('mkdir -p ' // trim(workspace_dir) // '/src')
        call execute_command_line('mkdir -p ' // trim(workspace_dir) // '/test')
        
    end subroutine setup_test_workspace

    subroutine cleanup_test_workspace(test_dir)
        !! Removes the test workspace
        character(len=*), intent(in), optional :: test_dir
        character(len=256) :: workspace_dir
        
        if (present(test_dir)) then
            workspace_dir = test_dir
        else
            workspace_dir = DEFAULT_TEST_DIR
        end if
        
        write(output_unit, '(A)') "Cleaning up test workspace..."
        call execute_command_line('rm -rf ' // trim(workspace_dir))
        
    end subroutine cleanup_test_workspace

    subroutine create_fpm_test_project(workspace_path)
        !! Creates a realistic FPM project structure for testing
        character(len=*), intent(in) :: workspace_path
        integer :: unit_number
        
        ! Create fpm.toml
        open(newunit=unit_number, file=trim(workspace_path) // '/fpm.toml', &
             status='replace', action='write')
        write(unit_number, '(A)') 'name = "test_project"'
        write(unit_number, '(A)') 'version = "0.1.0"'
        write(unit_number, '(A)') '[build]'
        write(unit_number, '(A)') 'auto-executables = true'
        write(unit_number, '(A)') 'auto-tests = true'
        close(unit_number)
        
        ! Create source file
        call execute_command_line('mkdir -p ' // trim(workspace_path) // '/src')
        open(newunit=unit_number, file=trim(workspace_path) // '/src/main.f90', &
             status='replace', action='write')
        write(unit_number, '(A)') 'program main'
        write(unit_number, '(A)') '  print *, "Hello World"'
        write(unit_number, '(A)') 'end program main'
        close(unit_number)
        
    end subroutine create_fpm_test_project

    subroutine create_cmake_test_project(workspace_path)
        !! Creates a realistic CMake project structure for testing
        character(len=*), intent(in) :: workspace_path
        integer :: unit_number
        
        ! Create CMakeLists.txt
        open(newunit=unit_number, file=trim(workspace_path) // '/CMakeLists.txt', &
             status='replace', action='write')
        write(unit_number, '(A)') 'cmake_minimum_required(VERSION 3.10)'
        write(unit_number, '(A)') 'project(TestProject LANGUAGES Fortran)'
        write(unit_number, '(A)') 'enable_testing()'
        close(unit_number)
        
    end subroutine create_cmake_test_project

    subroutine create_make_test_project(workspace_path)
        !! Creates a realistic Make project structure for testing
        character(len=*), intent(in) :: workspace_path
        integer :: unit_number
        
        ! Create Makefile
        open(newunit=unit_number, file=trim(workspace_path) // '/Makefile', &
             status='replace', action='write')
        write(unit_number, '(A)') 'all:'
        write(unit_number, '(A)') '	@echo "Building project"'
        write(unit_number, '(A)') 'test:'
        write(unit_number, '(A)') '	@echo "Running tests"'
        close(unit_number)
        
    end subroutine create_make_test_project

    subroutine create_mock_gcov_files(workspace_path)
        !! Creates realistic mock gcov files for testing coverage parsing
        character(len=*), intent(in) :: workspace_path
        integer :: unit_number
        
        ! Create realistic mock gcov file with coverage data
        open(newunit=unit_number, file=trim(workspace_path) // '/main.f90.gcov', &
             status='replace', action='write')
        write(unit_number, '(A)') '        -:    0:Source:main.f90'
        write(unit_number, '(A)') '        -:    0:Graph:main.gcno'
        write(unit_number, '(A)') '        -:    0:Data:main.gcda'
        write(unit_number, '(A)') '        -:    0:Runs:1'
        write(unit_number, '(A)') '        -:    0:Programs:1'
        write(unit_number, '(A)') '        -:    1:program main'
        write(unit_number, '(A)') '        1:    2:  print *, "Hello World"'
        write(unit_number, '(A)') '        1:    3:end program main'
        close(unit_number)
        
    end subroutine create_mock_gcov_files

    subroutine create_complete_project_scenario()
        !! Creates a complete realistic project for end-to-end testing
        
        call create_fpm_test_project(".")
        call create_mock_gcov_files(".")
        
    end subroutine create_complete_project_scenario

    function test_environment_detected() result(is_test_env)
        !! Use consistent test environment detection
        use test_environment_utils, only: &
            test_environment_detected_util => test_environment_detected
        logical :: is_test_env
        
        is_test_env = test_environment_detected_util()
    end function test_environment_detected

end module test_auto_discovery_shared_utilities