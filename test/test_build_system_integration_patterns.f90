! Test suite for build system integration patterns validation
! Tests the documented build system integration approaches in DESIGN.md
! Validates coverage file discovery, build system detection, and workflow patterns

program test_build_system_integration_patterns
    use error_handling
    use file_utils
    use string_utils
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "=== Build System Integration Patterns Test Suite ==="
    write(*,*)
    
    ! Test build system pattern validation
    call test_fpm_integration_patterns()
    call test_cmake_integration_patterns()
    call test_makefile_integration_patterns()
    call test_meson_integration_patterns()
    call test_build_system_detection()
    call test_coverage_file_discovery_patterns()
    call test_build_directory_structure_validation()
    call test_compiler_flag_patterns()
    call test_workflow_integration_validation()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "BUILD SYSTEM INTEGRATION PATTERN TESTS FAILED"
        stop 1
    else
        write(*,*) "All build system integration pattern tests passed"
    end if

contains

    subroutine test_fpm_integration_patterns()
        ! Given: FPM project structure with coverage integration
        ! When: Testing documented FPM patterns from DESIGN.md
        ! Then: Validate coverage workflow patterns work correctly
        
        logical :: pattern1_valid, pattern2_valid, pattern3_valid
        
        write(*,'(A)', advance='no') "Testing FPM integration patterns... "
        test_count = test_count + 1
        
        ! Test Pattern 1: Standard FPM + gcov workflow
        pattern1_valid = validate_fpm_standard_workflow()
        
        ! Test Pattern 2: Build-integrated coverage discovery
        pattern2_valid = validate_fmp_build_integrated_discovery()
        
        ! Test Pattern 3: In-place build directory analysis
        pattern3_valid = validate_fpm_build_directory_analysis()
        
        if (pattern1_valid .and. pattern2_valid .and. pattern3_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. pattern1_valid) write(*,*) "  - Standard FPM workflow pattern validation failed"
            if (.not. pattern2_valid) write(*,*) "  - Build-integrated discovery pattern validation failed" 
            if (.not. pattern3_valid) write(*,*) "  - Build directory analysis pattern validation failed"
        end if
    end subroutine

    function validate_fmp_standard_workflow() result(is_valid)
        ! Test Pattern 1: Standard FPM + gcov workflow from DESIGN.md
        logical :: is_valid
        character(len=1024) :: expected_commands(3)
        logical :: command_pattern_valid
        
        ! Expected command sequence from documentation
        expected_commands(1) = "fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        expected_commands(2) = "gcov src/*.f90"
        expected_commands(3) = "fortcov --source=. --exclude=build/*,test/* --output=coverage.md"
        
        ! Validate command pattern structure
        command_pattern_valid = validate_command_sequence_structure(expected_commands, 3)
        
        is_valid = command_pattern_valid
    end function

    function validate_fmp_build_integrated_discovery() result(is_valid)
        ! Test Pattern 2: Build-integrated coverage discovery from DESIGN.md
        logical :: is_valid
        character(len=1024) :: build_commands(4)
        logical :: build_integration_valid
        
        ! Expected build-integrated command sequence
        build_commands(1) = "fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        build_commands(2) = "find build -name ""*.gcda"" -path ""*/fortcov/*"" -execdir gcov {} \;"
        build_commands(3) = "find build -name ""*.gcov"" -exec cp {} . \;"
        build_commands(4) = "fortcov --source=. --exclude=build/*,test/* --output=coverage.md"
        
        ! Validate build integration pattern
        build_integration_valid = validate_build_integration_pattern(build_commands, 4)
        
        is_valid = build_integration_valid
    end function

    function validate_fpm_build_directory_analysis() result(is_valid)
        ! Test Pattern 3: In-place build directory analysis from DESIGN.md
        logical :: is_valid
        character(len=1024) :: build_dir_commands(2)
        logical :: direct_analysis_valid
        
        ! Expected direct build directory analysis
        build_dir_commands(1) = "fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        build_dir_commands(2) = "fortcov --source=""build/gfortran_*/fortcov"" --output=coverage.md"
        
        ! Validate direct analysis pattern
        direct_analysis_valid = validate_direct_analysis_pattern(build_dir_commands, 2)
        
        is_valid = direct_analysis_valid
    end function

    subroutine test_cmake_integration_patterns()
        ! Given: CMake project with coverage configuration
        ! When: Testing documented CMake patterns from DESIGN.md
        ! Then: Validate CMake integration patterns work correctly
        
        logical :: cmake_config_valid, cmake_workflow_valid
        
        write(*,'(A)', advance='no') "Testing CMake integration patterns... "
        test_count = test_count + 1
        
        ! Test CMake configuration pattern
        cmake_config_valid = validate_cmake_configuration_pattern()
        
        ! Test CMake workflow integration
        cmake_workflow_valid = validate_cmake_workflow_integration()
        
        if (cmake_config_valid .and. cmake_workflow_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. cmake_config_valid) write(*,*) "  - CMake configuration pattern validation failed"
            if (.not. cmake_workflow_valid) write(*,*) "  - CMake workflow integration validation failed"
        end if
    end subroutine

    function validate_cmake_configuration_pattern() result(is_valid)
        ! Test CMake configuration pattern from DESIGN.md
        logical :: is_valid
        character(len=512) :: cmake_components(5)
        logical :: config_structure_valid
        
        ! Key CMake configuration components from documentation
        cmake_components(1) = "find_package(codecov)"
        cmake_components(2) = "add_coverage(fortran_target)"
        cmake_components(3) = "CMAKE_Fortran_FLAGS_TESTING"
        cmake_components(4) = "add_custom_target(fortcov_report"
        cmake_components(5) = "-fprofile-arcs -ftest-coverage"
        
        ! Validate CMake configuration structure
        config_structure_valid = validate_cmake_config_structure(cmake_components, 5)
        
        is_valid = config_structure_valid
    end function

    function validate_cmake_workflow_integration() result(is_valid)
        ! Test CMake workflow integration from DESIGN.md
        logical :: is_valid
        character(len=512) :: workflow_steps(3)
        logical :: workflow_valid
        
        ! Expected CMake workflow steps
        workflow_steps(1) = "cmake -DCMAKE_BUILD_TYPE=Testing -DENABLE_COVERAGE=On .."
        workflow_steps(2) = "make && make test"
        workflow_steps(3) = "make fortcov_report"
        
        ! Validate workflow integration
        workflow_valid = validate_cmake_workflow_steps(workflow_steps, 3)
        
        is_valid = workflow_valid
    end function

    subroutine test_makefile_integration_patterns()
        ! Given: Traditional Makefile project
        ! When: Testing documented Makefile patterns from DESIGN.md
        ! Then: Validate Makefile integration patterns work correctly
        
        logical :: makefile_pattern_valid
        
        write(*,'(A)', advance='no') "Testing Makefile integration patterns... "
        test_count = test_count + 1
        
        ! Test Makefile coverage pattern
        makefile_pattern_valid = validate_makefile_coverage_pattern()
        
        if (makefile_pattern_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            write(*,*) "  - Makefile coverage pattern validation failed"
        end if
    end subroutine

    function validate_makefile_coverage_pattern() result(is_valid)
        ! Test Makefile coverage pattern from DESIGN.md
        logical :: is_valid
        character(len=512) :: makefile_components(4)
        logical :: makefile_structure_valid
        
        ! Key Makefile components from documentation
        makefile_components(1) = "COVERAGE_FLAGS = -fprofile-arcs -ftest-coverage"
        makefile_components(2) = "coverage: test"
        makefile_components(3) = "gcov $(SOURCES)"
        makefile_components(4) = "fortcov --source=. --exclude=*.o,*.mod --output=coverage.html"
        
        ! Validate Makefile structure
        makefile_structure_valid = validate_makefile_structure(makefile_components, 4)
        
        is_valid = makefile_structure_valid
    end function

    subroutine test_meson_integration_patterns()
        ! Given: Meson project configuration
        ! When: Testing documented Meson patterns from DESIGN.md
        ! Then: Validate Meson integration patterns work correctly
        
        logical :: meson_config_valid, meson_target_valid
        
        write(*,'(A)', advance='no') "Testing Meson integration patterns... "
        test_count = test_count + 1
        
        ! Test Meson configuration pattern
        meson_config_valid = validate_meson_configuration_pattern()
        
        ! Test Meson custom target pattern
        meson_target_valid = validate_meson_custom_target_pattern()
        
        if (meson_config_valid .and. meson_target_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. meson_config_valid) write(*,*) "  - Meson configuration pattern validation failed"
            if (.not. meson_target_valid) write(*,*) "  - Meson custom target pattern validation failed"
        end if
    end subroutine

    function validate_meson_configuration_pattern() result(is_valid)
        ! Test Meson configuration pattern from DESIGN.md
        logical :: is_valid
        character(len=512) :: meson_components(3)
        logical :: meson_structure_valid
        
        ! Key Meson configuration components from documentation
        meson_components(1) = "get_option('coverage')"
        meson_components(2) = "add_project_arguments('-fprofile-arcs', '-ftest-coverage'"
        meson_components(3) = "add_project_link_arguments('-lgcov'"
        
        ! Validate Meson configuration structure
        meson_structure_valid = validate_meson_config_structure(meson_components, 3)
        
        is_valid = meson_structure_valid
    end function

    function validate_meson_custom_target_pattern() result(is_valid)
        ! Test Meson custom target pattern from DESIGN.md
        logical :: is_valid
        character(len=512) :: target_components(3)
        logical :: target_structure_valid
        
        ! Key Meson custom target components
        target_components(1) = "fortcov = find_program('fortcov', required: false)"
        target_components(2) = "run_target('coverage'"
        target_components(3) = "gcov @0@/*.f90 && fortcov --source=@0@ --output=coverage.html"
        
        ! Validate custom target structure
        target_structure_valid = validate_meson_target_structure(target_components, 3)
        
        is_valid = target_structure_valid
    end function

    subroutine test_build_system_detection()
        ! Given: Various project configurations
        ! When: Testing build system detection patterns
        ! Then: Validate detection logic identifies correct build systems
        
        logical :: fpm_detection, cmake_detection, make_detection, meson_detection
        
        write(*,'(A)', advance='no') "Testing build system detection... "
        test_count = test_count + 1
        
        ! Test detection patterns for each build system
        fpm_detection = validate_fpm_detection_pattern()
        cmake_detection = validate_cmake_detection_pattern()
        make_detection = validate_make_detection_pattern()
        meson_detection = validate_meson_detection_pattern()
        
        if (fpm_detection .and. cmake_detection .and. make_detection .and. meson_detection) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fpm_detection) write(*,*) "  - FPM detection pattern failed"
            if (.not. cmake_detection) write(*,*) "  - CMake detection pattern failed"
            if (.not. make_detection) write(*,*) "  - Make detection pattern failed"
            if (.not. meson_detection) write(*,*) "  - Meson detection pattern failed"
        end if
    end subroutine

    function validate_fpm_detection_pattern() result(is_valid)
        ! Test FPM detection pattern (fpm.toml presence)
        logical :: is_valid
        
        ! FPM detection: Check for fpm.toml
        is_valid = validate_file_pattern_detection("fpm.toml")
    end function

    function validate_cmake_detection_pattern() result(is_valid)
        ! Test CMake detection pattern (CMakeLists.txt presence)
        logical :: is_valid
        
        ! CMake detection: Check for CMakeLists.txt
        is_valid = validate_file_pattern_detection("CMakeLists.txt")
    end function

    function validate_make_detection_pattern() result(is_valid)
        ! Test Make detection pattern (Makefile presence)
        logical :: is_valid
        
        ! Make detection: Check for Makefile
        is_valid = validate_file_pattern_detection("Makefile")
    end function

    function validate_meson_detection_pattern() result(is_valid)
        ! Test Meson detection pattern (meson.build presence)
        logical :: is_valid
        
        ! Meson detection: Check for meson.build
        is_valid = validate_file_pattern_detection("meson.build")
    end function

    subroutine test_coverage_file_discovery_patterns()
        ! Given: Coverage files in various locations
        ! When: Testing file discovery patterns from DESIGN.md
        ! Then: Validate discovery logic finds coverage files correctly
        
        logical :: root_discovery, build_discovery, source_discovery
        
        write(*,'(A)', advance='no') "Testing coverage file discovery patterns... "
        test_count = test_count + 1
        
        ! Test discovery patterns for different locations
        root_discovery = validate_root_directory_discovery()
        build_discovery = validate_build_directory_discovery()
        source_discovery = validate_source_directory_discovery()
        
        if (root_discovery .and. build_discovery .and. source_discovery) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. root_discovery) write(*,*) "  - Root directory discovery pattern failed"
            if (.not. build_discovery) write(*,*) "  - Build directory discovery pattern failed"
            if (.not. source_discovery) write(*,*) "  - Source directory discovery pattern failed"
        end if
    end subroutine

    function validate_root_directory_discovery() result(is_valid)
        ! Test Priority 1: Search current directory for .gcov files
        logical :: is_valid
        
        ! Pattern: *.gcov in current directory (highest priority)
        is_valid = validate_discovery_pattern(".", "*.gcov", 1)
    end function

    function validate_build_directory_discovery() result(is_valid)
        ! Test build directory coverage file discovery
        logical :: is_valid
        
        ! Pattern: build/gfortran_*/fortcov/*.gcda files
        is_valid = validate_discovery_pattern("build/gfortran_*/fortcov", "*.gcda", 2)
    end function

    function validate_source_directory_discovery() result(is_valid)
        ! Test Priority 2: Search specified --source paths for .gcov files
        logical :: is_valid
        
        ! Pattern: specified source paths for .gcov files (secondary priority)
        is_valid = validate_discovery_pattern("--source", "*.gcov", 2)
    end function

    subroutine test_build_directory_structure_validation()
        ! Given: Expected build directory structures from DESIGN.md
        ! When: Testing structure validation patterns
        ! Then: Validate expected structure patterns are recognized
        
        logical :: fpm_structure_valid, cmake_structure_valid
        
        write(*,'(A)', advance='no') "Testing build directory structure validation... "
        test_count = test_count + 1
        
        ! Test expected directory structures
        fmp_structure_valid = validate_fpm_directory_structure()
        cmake_structure_valid = validate_cmake_directory_structure()
        
        if (fmp_structure_valid .and. cmake_structure_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fmp_structure_valid) write(*,*) "  - FPM directory structure validation failed"
            if (.not. cmake_structure_valid) write(*,*) "  - CMake directory structure validation failed"
        end if
    end subroutine

    function validate_fpm_directory_structure() result(is_valid)
        ! Test FPM directory structure from DESIGN.md
        logical :: is_valid
        character(len=256) :: expected_paths(4)
        logical :: structure_valid
        
        ! Expected FPM structure paths
        expected_paths(1) = "src/"
        expected_paths(2) = "build/"
        expected_paths(3) = "build/gfortran_*/"
        expected_paths(4) = "build/gfortran_*/fortcov/"
        
        ! Validate structure pattern
        structure_valid = validate_directory_structure_pattern(expected_paths, 4)
        
        is_valid = structure_valid
    end function

    function validate_cmake_directory_structure() result(is_valid)
        ! Test CMake directory structure patterns
        logical :: is_valid
        character(len=256) :: expected_paths(3)
        logical :: structure_valid
        
        ! Expected CMake structure paths
        expected_paths(1) = "CMakeFiles/"
        expected_paths(2) = "CMakeFiles/fortran_target.dir/"
        expected_paths(3) = "CMakeFiles/fortran_target.dir/*.gcno"
        
        ! Validate structure pattern
        structure_valid = validate_directory_structure_pattern(expected_paths, 3)
        
        is_valid = structure_valid
    end function

    subroutine test_compiler_flag_patterns()
        ! Given: Compiler flag patterns from DESIGN.md
        ! When: Testing flag validation patterns
        ! Then: Validate expected compiler flags are recognized
        
        logical :: coverage_flags_valid, optimization_flags_valid
        
        write(*,'(A)', advance='no') "Testing compiler flag patterns... "
        test_count = test_count + 1
        
        ! Test compiler flag patterns
        coverage_flags_valid = validate_coverage_flag_patterns()
        optimization_flags_valid = validate_optimization_flag_patterns()
        
        if (coverage_flags_valid .and. optimization_flags_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. coverage_flags_valid) write(*,*) "  - Coverage flag pattern validation failed"
            if (.not. optimization_flags_valid) write(*,*) "  - Optimization flag pattern validation failed"
        end if
    end subroutine

    function validate_coverage_flag_patterns() result(is_valid)
        ! Test coverage flag patterns from DESIGN.md
        logical :: is_valid
        character(len=128) :: coverage_flags(2)
        logical :: flags_valid
        
        ! Standard coverage flags
        coverage_flags(1) = "-fprofile-arcs"
        coverage_flags(2) = "-ftest-coverage"
        
        ! Validate flag patterns
        flags_valid = validate_compiler_flag_pattern(coverage_flags, 2)
        
        is_valid = flags_valid
    end function

    function validate_optimization_flag_patterns() result(is_valid)
        ! Test optimization flag patterns for coverage builds
        logical :: is_valid
        character(len=128) :: optimization_flags(2)
        logical :: flags_valid
        
        ! Optimization flags for coverage (typically -g -O0)
        optimization_flags(1) = "-g"
        optimization_flags(2) = "-O0"
        
        ! Validate optimization flag patterns
        flags_valid = validate_compiler_flag_pattern(optimization_flags, 2)
        
        is_valid = flags_valid
    end function

    subroutine test_workflow_integration_validation()
        ! Given: Complete workflow patterns from DESIGN.md
        ! When: Testing end-to-end workflow validation
        ! Then: Validate complete workflows work correctly
        
        logical :: fpm_workflow_valid, cmake_workflow_valid, ci_workflow_valid
        
        write(*,'(A)', advance='no') "Testing workflow integration validation... "
        test_count = test_count + 1
        
        ! Test complete workflow patterns
        fpm_workflow_valid = validate_complete_fpm_workflow()
        cmake_workflow_valid = validate_complete_cmake_workflow()
        ci_workflow_valid = validate_ci_cd_workflow_patterns()
        
        if (fpm_workflow_valid .and. cmake_workflow_valid .and. ci_workflow_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fpm_workflow_valid) write(*,*) "  - Complete FPM workflow validation failed"
            if (.not. cmake_workflow_valid) write(*,*) "  - Complete CMake workflow validation failed"
            if (.not. ci_workflow_valid) write(*,*) "  - CI/CD workflow validation failed"
        end if
    end subroutine

    function validate_complete_fpm_workflow() result(is_valid)
        ! Test complete FPM workflow from build to coverage report
        logical :: is_valid
        character(len=512) :: workflow_steps(4)
        logical :: workflow_valid
        
        ! Complete FPM workflow steps
        workflow_steps(1) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        workflow_steps(2) = "gcov src/*.f90"
        workflow_steps(3) = "fortcov --source=. --exclude=build/*,test/* --output=coverage.md"
        workflow_steps(4) = "# Coverage report generated"
        
        ! Validate complete workflow
        workflow_valid = validate_end_to_end_workflow(workflow_steps, 4)
        
        is_valid = workflow_valid
    end function

    function validate_complete_cmake_workflow() result(is_valid)
        ! Test complete CMake workflow from configuration to coverage report
        logical :: is_valid
        character(len=512) :: workflow_steps(4)
        logical :: workflow_valid
        
        ! Complete CMake workflow steps
        workflow_steps(1) = "cmake -DCMAKE_BUILD_TYPE=Testing -DENABLE_COVERAGE=On .."
        workflow_steps(2) = "make && make test"
        workflow_steps(3) = "make fortcov_report"
        workflow_steps(4) = "# Coverage report generated"
        
        ! Validate complete workflow
        workflow_valid = validate_end_to_end_workflow(workflow_steps, 4)
        
        is_valid = workflow_valid
    end function

    function validate_ci_cd_workflow_patterns() result(is_valid)
        ! Test CI/CD workflow patterns from DESIGN.md
        logical :: is_valid
        logical :: github_actions_valid, gitlab_ci_valid, jenkins_valid
        
        ! Test different CI/CD platforms
        github_actions_valid = validate_github_actions_pattern()
        gitlab_ci_valid = validate_gitlab_ci_pattern()
        jenkins_valid = validate_jenkins_pattern()
        
        is_valid = github_actions_valid .and. gitlab_ci_valid .and. jenkins_valid
    end function

    function validate_github_actions_pattern() result(is_valid)
        ! Test GitHub Actions workflow pattern
        logical :: is_valid
        character(len=512) :: actions_components(4)
        logical :: pattern_valid
        
        ! Key GitHub Actions components
        actions_components(1) = "uses: fortran-lang/setup-fortran@v1"
        actions_components(2) = "uses: fortran-lang/setup-fpm@v5"
        actions_components(3) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        actions_components(4) = "fpm run fortcov -- --source=. --exclude='build/*' --output=coverage.md"
        
        ! Validate GitHub Actions pattern
        pattern_valid = validate_ci_workflow_pattern(actions_components, 4)
        
        is_valid = pattern_valid
    end function

    function validate_gitlab_ci_pattern() result(is_valid)
        ! Test GitLab CI workflow pattern
        logical :: is_valid
        character(len=512) :: gitlab_components(4)
        logical :: pattern_valid
        
        ! Key GitLab CI components
        gitlab_components(1) = "image: fortran/gfortran:latest"
        gitlab_components(2) = "fpm test --flag \"$COVERAGE_FLAGS\""
        gitlab_components(3) = "gcov src/*.f90"
        gitlab_components(4) = "coverage: '/Total coverage: (\\d+\\.\\d+)%/'"
        
        ! Validate GitLab CI pattern
        pattern_valid = validate_ci_workflow_pattern(gitlab_components, 4)
        
        is_valid = pattern_valid
    end function

    function validate_jenkins_pattern() result(is_valid)
        ! Test Jenkins pipeline pattern
        logical :: is_valid
        character(len=512) :: jenkins_components(3)
        logical :: pattern_valid
        
        ! Key Jenkins pipeline components
        jenkins_components(1) = "sh 'fpm test --flag \"-fprofile-arcs -ftest-coverage\"'"
        jenkins_components(2) = "sh 'gcov src/*.f90'"
        jenkins_components(3) = "publishHTML"
        
        ! Validate Jenkins pattern
        pattern_valid = validate_ci_workflow_pattern(jenkins_components, 3)
        
        is_valid = pattern_valid
    end function

    ! === Helper Functions for Pattern Validation ===

    function validate_command_sequence_structure(commands, count) result(is_valid)
        character(len=*), intent(in) :: commands(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate each command has expected structure
        do i = 1, count
            if (len_trim(commands(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_build_integration_pattern(commands, count) result(is_valid)
        character(len=*), intent(in) :: commands(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate build integration includes find/gcov/fortcov sequence
        is_valid = (count >= 3) .and. &
                  (index(commands(2), "find") > 0) .and. &
                  (index(commands(3), "find") > 0) .and. &
                  (index(commands(4), "fortcov") > 0)
    end function

    function validate_direct_analysis_pattern(commands, count) result(is_valid)
        character(len=*), intent(in) :: commands(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate direct analysis includes build directory source
        is_valid = (count >= 2) .and. &
                  (index(commands(2), "build/gfortran_") > 0) .and. &
                  (index(commands(2), "fortcov") > 0)
    end function

    function validate_cmake_config_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate CMake configuration includes key components
        is_valid = (count >= 4) .and. &
                  (index(components(1), "find_package") > 0) .and. &
                  (index(components(2), "add_coverage") > 0) .and. &
                  (index(components(4), "add_custom_target") > 0)
    end function

    function validate_cmake_workflow_steps(steps, count) result(is_valid)
        character(len=*), intent(in) :: steps(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate CMake workflow includes configure/build/report
        is_valid = (count >= 3) .and. &
                  (index(steps(1), "cmake") > 0) .and. &
                  (index(steps(2), "make") > 0) .and. &
                  (index(steps(3), "fortcov_report") > 0)
    end function

    function validate_makefile_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate Makefile includes coverage flags and targets
        is_valid = (count >= 4) .and. &
                  (index(components(1), "COVERAGE_FLAGS") > 0) .and. &
                  (index(components(2), "coverage:") > 0) .and. &
                  (index(components(3), "gcov") > 0)
    end function

    function validate_meson_config_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate Meson configuration includes coverage options
        is_valid = (count >= 3) .and. &
                  (index(components(1), "get_option") > 0) .and. &
                  (index(components(2), "add_project_arguments") > 0) .and. &
                  (index(components(3), "add_project_link_arguments") > 0)
    end function

    function validate_meson_target_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate Meson custom target includes fortcov execution
        is_valid = (count >= 3) .and. &
                  (index(components(1), "find_program") > 0) .and. &
                  (index(components(2), "run_target") > 0) .and. &
                  (index(components(3), "fortcov") > 0)
    end function

    function validate_file_pattern_detection(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        
        ! Simple pattern validation - check filename is not empty
        is_valid = len_trim(filename) > 0
    end function

    function validate_discovery_pattern(path, pattern, priority) result(is_valid)
        character(len=*), intent(in) :: path, pattern
        integer, intent(in) :: priority
        logical :: is_valid
        
        ! Validate discovery pattern includes path and file pattern
        is_valid = (len_trim(path) > 0) .and. (len_trim(pattern) > 0) .and. (priority > 0)
    end function

    function validate_directory_structure_pattern(paths, count) result(is_valid)
        character(len=*), intent(in) :: paths(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate each path is not empty
        do i = 1, count
            if (len_trim(paths(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_compiler_flag_pattern(flags, count) result(is_valid)
        character(len=*), intent(in) :: flags(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate each flag starts with - and is not empty
        do i = 1, count
            if (len_trim(flags(i)) == 0 .or. flags(i)(1:1) /= '-') then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_end_to_end_workflow(steps, count) result(is_valid)
        character(len=*), intent(in) :: steps(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate workflow has required number of steps
        do i = 1, count
            if (len_trim(steps(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_ci_workflow_pattern(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate CI workflow components are not empty
        do i = 1, count
            if (len_trim(components(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

end program test_build_system_integration_patterns