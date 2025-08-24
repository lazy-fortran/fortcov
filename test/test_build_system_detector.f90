program test_build_system_detector
    !! Comprehensive test suite for build_system_detector module
    !! 
    !! Tests all build system detection scenarios including:
    !! - FPM, CMake, Make, Meson detection
    !! - Priority ordering when multiple systems exist
    !! - Tool availability validation
    !! - Error handling for missing tools
    !! - Edge cases and malformed configurations
    
    use iso_fortran_env, only: error_unit, output_unit
    use build_system_detector
    use error_handling
    implicit none
    
    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_tests_passed = .true.
    
    ! Test variables
    type(build_system_info_t) :: build_info
    type(error_context_t) :: error_ctx
    character(len=256) :: test_dir
    character(len=256) :: current_dir
    logical :: success
    
    write(output_unit, '(A)') 'Running build_system_detector tests...'
    write(output_unit, *)
    
    ! Save current directory
    call getcwd(current_dir)
    
    ! Test 1: FPM detection
    call test_fpm_detection()
    
    ! Test 2: CMake detection
    call test_cmake_detection()
    
    ! Test 3: Make detection  
    call test_make_detection()
    
    ! Test 4: Meson detection
    call test_meson_detection()
    
    ! Test 5: Priority ordering (FPM > CMake > Make > Meson)
    call test_priority_ordering()
    
    ! Test 6: Unknown build system
    call test_unknown_build_system()
    
    ! Test 7: Tool availability validation
    call test_tool_availability()
    
    ! Test 8: Invalid directory handling
    call test_invalid_directory()
    
    ! Test 9: Coverage test command generation
    call test_coverage_commands()
    
    ! Test 10: Edge cases and error conditions
    call test_edge_cases()
    
    ! Cleanup and summary
    call chdir(current_dir)
    call cleanup_test_directories()
    
    write(output_unit, *)
    write(output_unit, '(A,I0,A,I0)') 'Test Results: ', passed_tests, ' / ', &
                                      total_tests
    
    if (all_tests_passed) then
        write(output_unit, '(A)') 'ALL TESTS PASSED'
        stop 0
    else
        write(error_unit, '(A)') 'SOME TESTS FAILED'
        stop 1
    end if

contains

    subroutine test_fpm_detection()
        !! Test FPM build system detection
        write(output_unit, '(A)') 'Test 1: FPM detection'
        
        call setup_test_directory('test_fpm', 'fpm.toml')
        call detect_build_system('.', build_info, error_ctx)
        
        call assert_no_error(error_ctx, 'FPM detection failed')
        call assert_equals_str(build_info%system_type, 'fpm', &
                               'Wrong system type for FPM')
        call assert_equals_str(build_info%build_file, 'fpm.toml', &
                               'Wrong build file for FPM')
        call assert_contains(build_info%test_command, 'fpm test', &
                             'Missing fpm test in command')
        call assert_contains(build_info%test_command, '-fprofile-arcs', &
                             'Missing coverage flag')
        
        call chdir(current_dir)
    end subroutine test_fpm_detection

    subroutine test_cmake_detection()
        !! Test CMake build system detection  
        write(output_unit, '(A)') 'Test 2: CMake detection'
        
        call setup_test_directory('test_cmake', 'CMakeLists.txt')
        call detect_build_system('.', build_info, error_ctx)
        
        call assert_no_error(error_ctx, 'CMake detection failed')
        call assert_equals_str(build_info%system_type, 'cmake', &
                               'Wrong system type for CMake')
        call assert_equals_str(build_info%build_file, 'CMakeLists.txt', &
                               'Wrong build file for CMake')
        call assert_contains(build_info%test_command, 'cmake --build', &
                             'Missing cmake build in command')
        call assert_contains(build_info%test_command, 'ctest', &
                             'Missing ctest in command')
        
        call chdir(current_dir)
    end subroutine test_cmake_detection

    subroutine test_make_detection()
        !! Test Make build system detection
        write(output_unit, '(A)') 'Test 3: Make detection'
        
        call setup_test_directory('test_make', 'Makefile')
        call detect_build_system('.', build_info, error_ctx)
        
        call assert_no_error(error_ctx, 'Make detection failed')
        call assert_equals_str(build_info%system_type, 'make', &
                               'Wrong system type for Make')
        call assert_equals_str(build_info%build_file, 'Makefile', &
                               'Wrong build file for Make')
        call assert_contains(build_info%test_command, 'make test', &
                             'Missing make test in command')
        
        call chdir(current_dir)
    end subroutine test_make_detection

    subroutine test_meson_detection()
        !! Test Meson build system detection
        write(output_unit, '(A)') 'Test 4: Meson detection'
        
        call setup_test_directory('test_meson', 'meson.build')
        call detect_build_system('.', build_info, error_ctx)
        
        call assert_no_error(error_ctx, 'Meson detection failed')
        call assert_equals_str(build_info%system_type, 'meson', &
                               'Wrong system type for Meson')
        call assert_equals_str(build_info%build_file, 'meson.build', &
                               'Wrong build file for Meson')
        call assert_contains(build_info%test_command, 'meson test', &
                             'Missing meson test in command')
        
        call chdir(current_dir)
    end subroutine test_meson_detection

    subroutine test_priority_ordering()
        !! Test priority ordering when multiple build systems exist
        write(output_unit, '(A)') 'Test 5: Priority ordering'
        
        ! Create directory with multiple build files
        call setup_multiple_build_files('test_priority')
        call detect_build_system('.', build_info, error_ctx)
        
        call assert_no_error(error_ctx, 'Priority detection failed')
        call assert_equals_str(build_info%system_type, 'fpm', &
                               'FPM should have highest priority')
        
        call chdir(current_dir)
    end subroutine test_priority_ordering

    subroutine test_unknown_build_system()
        !! Test detection when no known build system exists
        write(output_unit, '(A)') 'Test 6: Unknown build system'
        
        call setup_test_directory('test_unknown', '')
        call detect_build_system('.', build_info, error_ctx)
        
        call assert_no_error(error_ctx, 'Unknown detection should not error')
        call assert_equals_str(build_info%system_type, 'unknown', &
                               'Should detect as unknown')
        call assert_equals_str(build_info%test_command, '', &
                               'Unknown should have empty command')
        
        call chdir(current_dir)
    end subroutine test_unknown_build_system

    subroutine test_tool_availability()
        !! Test build tool availability validation
        write(output_unit, '(A)') 'Test 7: Tool availability'
        
        call setup_test_directory('test_tool', 'fpm.toml')
        call detect_build_system('.', build_info, error_ctx)
        
        call assert_no_error(error_ctx, 'Tool detection failed')
        ! Note: tool_available depends on actual system PATH
        write(output_unit, '(A,L1)') '  FPM tool available: ', &
                                     build_info%tool_available
        
        call chdir(current_dir)
    end subroutine test_tool_availability

    subroutine test_invalid_directory()
        !! Test error handling for invalid directories
        write(output_unit, '(A)') 'Test 8: Invalid directory handling'
        
        call detect_build_system('/nonexistent/directory', build_info, &
                                  error_ctx)
        
        call assert_has_error(error_ctx, 'Should error for invalid directory')
        
        call chdir(current_dir)
    end subroutine test_invalid_directory

    subroutine test_coverage_commands()
        !! Test coverage test command generation
        character(len=512) :: cmd
        write(output_unit, '(A)') 'Test 9: Coverage commands'
        
        call get_coverage_test_command('fpm', cmd, error_ctx)
        call assert_no_error(error_ctx, 'FPM coverage command failed')
        call assert_contains(cmd, '-fprofile-arcs', 'Missing coverage flags')
        call assert_contains(cmd, '-ftest-coverage', 'Missing test coverage flag')
        
        call get_coverage_test_command('cmake', cmd, error_ctx)
        call assert_no_error(error_ctx, 'CMake coverage command failed')
        call assert_contains(cmd, 'cmake --build', 'Missing cmake build')
        
        call get_coverage_test_command('unknown', cmd, error_ctx)
        call assert_has_error(error_ctx, 'Unknown should error')
    end subroutine test_coverage_commands

    subroutine test_edge_cases()
        !! Test edge cases and error conditions
        write(output_unit, '(A)') 'Test 10: Edge cases'
        
        ! Test empty directory path
        call detect_build_system('', build_info, error_ctx)
        call assert_has_error(error_ctx, 'Empty path should error')
        
        ! Test very long path
        call detect_build_system(repeat('a', 5000), build_info, error_ctx)
        call assert_has_error(error_ctx, 'Long path should error')
    end subroutine test_edge_cases

    ! Test utilities
    subroutine setup_test_directory(dirname, build_file)
        character(len=*), intent(in) :: dirname, build_file
        integer :: unit, stat
        
        call execute_command_line('mkdir -p ' // dirname, wait=.true.)
        call chdir(dirname)
        
        if (len_trim(build_file) > 0) then
            open(newunit=unit, file=build_file, status='replace', &
                 iostat=stat)
            if (stat == 0) then
                write(unit, '(A)') '# Test build file'
                close(unit)
            end if
        end if
    end subroutine setup_test_directory

    subroutine setup_multiple_build_files(dirname)
        character(len=*), intent(in) :: dirname
        integer :: unit, stat
        
        call execute_command_line('mkdir -p ' // dirname, wait=.true.)
        call chdir(dirname)
        
        ! Create all build files to test priority
        open(newunit=unit, file='fpm.toml', status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') 'name = "test"'
            close(unit)
        end if
        
        open(newunit=unit, file='CMakeLists.txt', status='replace', &
             iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') 'project(test)'
            close(unit)
        end if
        
        open(newunit=unit, file='Makefile', status='replace', iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') 'all:'
            close(unit)
        end if
        
        open(newunit=unit, file='meson.build', status='replace', &
             iostat=stat)
        if (stat == 0) then
            write(unit, '(A)') 'project("test")'
            close(unit)
        end if
    end subroutine setup_multiple_build_files

    subroutine cleanup_test_directories()
        call execute_command_line('rm -rf test_fpm test_cmake test_make ' // &
                                  'test_meson test_priority test_unknown ' // &
                                  'test_tool', wait=.true.)
    end subroutine cleanup_test_directories

    ! Assertion utilities
    subroutine assert_no_error(error_ctx, message)
        type(error_context_t), intent(in) :: error_ctx
        character(len=*), intent(in) :: message
        
        total_tests = total_tests + 1
        if (error_ctx%error_code == ERROR_SUCCESS) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,A)') '    Error: ', &
                                       trim(error_ctx%message)
        end if
    end subroutine assert_no_error

    subroutine assert_has_error(error_ctx, message)
        type(error_context_t), intent(in) :: error_ctx
        character(len=*), intent(in) :: message
        
        total_tests = total_tests + 1
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A)') '    Expected error but none occurred'
        end if
    end subroutine assert_has_error

    subroutine assert_equals_str(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message
        
        total_tests = total_tests + 1
        if (trim(actual) == trim(expected)) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,A,A,A)') '    Expected: "', &
                                           trim(expected), '", Got: "', &
                                           trim(actual), '"'
        end if
    end subroutine assert_equals_str

    subroutine assert_contains(actual, substring, message)
        character(len=*), intent(in) :: actual, substring, message
        
        total_tests = total_tests + 1
        if (index(actual, substring) > 0) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A)') '  PASS: ', message
        else
            all_tests_passed = .false.
            write(error_unit, '(A,A)') '  FAIL: ', message
            write(error_unit, '(A,A,A,A)') '    String "', trim(actual), &
                                           '" does not contain "', &
                                           trim(substring), '"'
        end if
    end subroutine assert_contains

end program test_build_system_detector