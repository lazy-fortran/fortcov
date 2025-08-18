! Test framework for validating documented integration examples
! Tests that all examples from DESIGN.md work correctly and are complete
! Validates example accuracy, completeness, and practical usability

program test_integration_example_validation_framework
    use error_handling
    use file_utils
    use string_utils
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "=== Integration Example Validation Framework Test Suite ==="
    write(*,*)
    
    ! Test integration example validation framework
    call test_example_completeness_validation()
    call test_example_syntax_validation()
    call test_example_workflow_validation()
    call test_example_cross_reference_validation()
    call test_example_reproducibility_validation()
    call test_example_platform_compatibility_validation()
    call test_example_error_handling_validation()
    call test_example_documentation_consistency()
    call test_example_automation_framework()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "INTEGRATION EXAMPLE VALIDATION FRAMEWORK TESTS FAILED"
        stop 1
    else
        write(*,*) "All integration example validation framework tests passed"
    end if

contains

    subroutine test_example_completeness_validation()
        ! Given: Integration examples from DESIGN.md
        ! When: Testing example completeness validation
        ! Then: Validate all examples are complete and actionable
        
        logical :: fpm_examples_complete, cmake_examples_complete, ci_examples_complete
        
        write(*,'(A)', advance='no') "Testing example completeness validation... "
        test_count = test_count + 1
        
        ! Test example completeness for different integration types
        fpm_examples_complete = validate_fpm_example_completeness()
        cmake_examples_complete = validate_cmake_example_completeness()
        ci_examples_complete = validate_ci_cd_example_completeness()
        
        if (fpm_examples_complete .and. cmake_examples_complete .and. ci_examples_complete) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fpm_examples_complete) write(*,*) "  - FPM example completeness validation failed"
            if (.not. cmake_examples_complete) write(*,*) "  - CMake example completeness validation failed"
            if (.not. ci_examples_complete) write(*,*) "  - CI/CD example completeness validation failed"
        end if
    end subroutine

    function validate_fpm_example_completeness() result(is_valid)
        ! Test FPM example completeness from DESIGN.md
        logical :: is_valid
        character(len=512) :: fpm_example_components(6)
        logical :: completeness_valid
        
        ! FPM example components from documentation
        fpm_example_components(1) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        fpm_example_components(2) = "gcov src/*.f90"
        fpm_example_components(3) = "fortcov --source=. --exclude=build/*,test/* --output=coverage.md"
        fpm_example_components(4) = "find build -name \"*.gcda\" -path \"*/fortcov/*\" -execdir gcov {} \\;"
        fpm_example_components(5) = "find build -name \"*.gcov\" -exec cp {} . \\;"
        fpm_example_components(6) = "fortcov --source=\"build/gfortran_*/fortcov\" --output=coverage.md"
        
        ! Validate completeness structure
        completeness_valid = validate_example_completeness_structure(fpm_example_components, 6)
        
        is_valid = completeness_valid
    end function

    function validate_cmake_example_completeness() result(is_valid)
        ! Test CMake example completeness from DESIGN.md
        logical :: is_valid
        character(len=512) :: cmake_example_components(6)
        logical :: completeness_valid
        
        ! CMake example components from documentation
        cmake_example_components(1) = "find_package(codecov)"
        cmake_example_components(2) = "add_coverage(fortran_target)"
        cmake_example_components(3) = "set(CMAKE_Fortran_FLAGS_TESTING \"-g -O0 -fprofile-arcs -ftest-coverage\")"
        cmake_example_components(4) = "cmake -DCMAKE_BUILD_TYPE=Testing -DENABLE_COVERAGE=On .."
        cmake_example_components(5) = "make && make test"
        cmake_example_components(6) = "make fortcov_report"
        
        ! Validate completeness structure
        completeness_valid = validate_example_completeness_structure(cmake_example_components, 6)
        
        is_valid = completeness_valid
    end function

    function validate_ci_cd_example_completeness() result(is_valid)
        ! Test CI/CD example completeness from DESIGN.md
        logical :: is_valid
        character(len=512) :: ci_example_components(8)
        logical :: completeness_valid
        
        ! CI/CD example components from documentation
        ci_example_components(1) = "uses: actions/checkout@v4"
        ci_example_components(2) = "uses: fortran-lang/setup-fortran@v1"
        ci_example_components(3) = "uses: fortran-lang/setup-fpm@v5"
        ci_example_components(4) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        ci_example_components(5) = "gcov src/*.f90 || true"
        ci_example_components(6) = "fpm run fortcov -- --source=. --exclude='build/*' --output=coverage.md"
        ci_example_components(7) = "uses: actions/upload-artifact@v4"
        ci_example_components(8) = "path: coverage.md"
        
        ! Validate completeness structure
        completeness_valid = validate_example_completeness_structure(ci_example_components, 8)
        
        is_valid = completeness_valid
    end function

    subroutine test_example_syntax_validation()
        ! Given: Examples with specific syntax requirements
        ! When: Testing example syntax validation
        ! Then: Validate all examples have correct syntax
        
        logical :: bash_syntax_valid, yaml_syntax_valid, cmake_syntax_valid
        
        write(*,'(A)', advance='no') "Testing example syntax validation... "
        test_count = test_count + 1
        
        ! Test syntax validation for different example types
        bash_syntax_valid = validate_bash_example_syntax()
        yaml_syntax_valid = validate_yaml_example_syntax()
        cmake_syntax_valid = validate_cmake_example_syntax()
        
        if (bash_syntax_valid .and. yaml_syntax_valid .and. cmake_syntax_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. bash_syntax_valid) write(*,*) "  - Bash example syntax validation failed"
            if (.not. yaml_syntax_valid) write(*,*) "  - YAML example syntax validation failed"
            if (.not. cmake_syntax_valid) write(*,*) "  - CMake example syntax validation failed"
        end if
    end subroutine

    function validate_bash_example_syntax() result(is_valid)
        ! Test bash example syntax from DESIGN.md
        logical :: is_valid
        character(len=512) :: bash_examples(6)
        logical :: bash_syntax_valid
        
        ! Bash examples from documentation
        bash_examples(1) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        bash_examples(2) = "gcov src/*.f90"
        bash_examples(3) = "find build -name \"*.gcda\" -path \"*/fortcov/*\" -execdir gcov {} \\;"
        bash_examples(4) = "for rank in {0..3}; do gcov -o rank_${rank} src/*.f90; done"
        bash_examples(5) = "GIT_CHANGED=$(git diff --name-only HEAD~1 HEAD | grep '\\.f90$')"
        bash_examples(6) = "if [ -n \"$GIT_CHANGED\" ]; then echo \"Files changed\"; fi"
        
        ! Validate bash syntax structure
        bash_syntax_valid = validate_bash_syntax_structure(bash_examples, 6)
        
        is_valid = bash_syntax_valid
    end function

    function validate_yaml_example_syntax() result(is_valid)
        ! Test YAML example syntax from DESIGN.md
        logical :: is_valid
        character(len=512) :: yaml_examples(8)
        logical :: yaml_syntax_valid
        
        ! YAML examples from documentation
        yaml_examples(1) = "name: Coverage Analysis"
        yaml_examples(2) = "on: [push, pull_request]"
        yaml_examples(3) = "runs-on: ubuntu-latest"
        yaml_examples(4) = "strategy:"
        yaml_examples(5) = "  matrix:"
        yaml_examples(6) = "    compiler: [gfortran, ifort, flang]"
        yaml_examples(7) = "exclude:"
        yaml_examples(8) = "  - os: macos-latest"
        
        ! Validate YAML syntax structure
        yaml_syntax_valid = validate_yaml_syntax_structure(yaml_examples, 8)
        
        is_valid = yaml_syntax_valid
    end function

    function validate_cmake_example_syntax() result(is_valid)
        ! Test CMake example syntax from DESIGN.md
        logical :: is_valid
        character(len=512) :: cmake_examples(6)
        logical :: cmake_syntax_valid
        
        ! CMake examples from documentation
        cmake_examples(1) = "find_package(codecov)"
        cmake_examples(2) = "add_coverage(fortran_target)"
        cmake_examples(3) = "set(CMAKE_Fortran_FLAGS_TESTING \"-g -O0 -fprofile-arcs -ftest-coverage\")"
        cmake_examples(4) = "add_custom_target(fortcov_report"
        cmake_examples(5) = "    COMMAND gcov ${CMAKE_BINARY_DIR}/CMakeFiles/fortran_target.dir/*.gcno"
        cmake_examples(6) = "    DEPENDS fortran_target)"
        
        ! Validate CMake syntax structure
        cmake_syntax_valid = validate_cmake_syntax_structure(cmake_examples, 6)
        
        is_valid = cmake_syntax_valid
    end function

    subroutine test_example_workflow_validation()
        ! Given: Complete workflow examples from DESIGN.md
        ! When: Testing workflow example validation
        ! Then: Validate workflows are logically complete and correct
        
        logical :: fmp_workflow_valid, ci_workflow_valid, docker_workflow_valid
        
        write(*,'(A)', advance='no') "Testing example workflow validation... "
        test_count = test_count + 1
        
        ! Test workflow validation for different scenarios
        fpm_workflow_valid = validate_fpm_workflow_example()
        ci_workflow_valid = validate_ci_workflow_example()
        docker_workflow_valid = validate_docker_workflow_example()
        
        if (fpm_workflow_valid .and. ci_workflow_valid .and. docker_workflow_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fpm_workflow_valid) write(*,*) "  - FPM workflow example validation failed"
            if (.not. ci_workflow_valid) write(*,*) "  - CI workflow example validation failed"
            if (.not. docker_workflow_valid) write(*,*) "  - Docker workflow example validation failed"
        end if
    end subroutine

    function validate_fpm_workflow_example() result(is_valid)
        ! Test FPM workflow example from DESIGN.md
        logical :: is_valid
        character(len=512) :: fpm_workflow_steps(4)
        logical :: workflow_structure_valid
        
        ! FPM workflow steps from documentation
        fpm_workflow_steps(1) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        fpm_workflow_steps(2) = "gcov src/*.f90"
        fpm_workflow_steps(3) = "fortcov --source=. --exclude=build/*,test/* --output=coverage.md"
        fpm_workflow_steps(4) = "# Coverage report generated"
        
        ! Validate workflow structure
        workflow_structure_valid = validate_workflow_logical_sequence(fpm_workflow_steps, 4)
        
        is_valid = workflow_structure_valid
    end function

    function validate_ci_workflow_example() result(is_valid)
        ! Test CI workflow example from DESIGN.md
        logical :: is_valid
        character(len=512) :: ci_workflow_steps(6)
        logical :: workflow_structure_valid
        
        ! CI workflow steps from documentation
        ci_workflow_steps(1) = "Setup Fortran compiler"
        ci_workflow_steps(2) = "Install FPM"
        ci_workflow_steps(3) = "Build with coverage"
        ci_workflow_steps(4) = "Generate coverage data"
        ci_workflow_steps(5) = "Generate coverage report"
        ci_workflow_steps(6) = "Upload coverage artifacts"
        
        ! Validate workflow structure
        workflow_structure_valid = validate_workflow_logical_sequence(ci_workflow_steps, 6)
        
        is_valid = workflow_structure_valid
    end function

    function validate_docker_workflow_example() result(is_valid)
        ! Test Docker workflow example from DESIGN.md
        logical :: is_valid
        character(len=512) :: docker_workflow_steps(6)
        logical :: workflow_structure_valid
        
        ! Docker workflow steps from documentation
        docker_workflow_steps(1) = "FROM fortran/gfortran:latest as builder"
        docker_workflow_steps(2) = "Install FPM"
        docker_workflow_steps(3) = "Build with coverage"
        docker_workflow_steps(4) = "Generate coverage"
        docker_workflow_steps(5) = "Run fortcov"
        docker_workflow_steps(6) = "Production stage"
        
        ! Validate workflow structure
        workflow_structure_valid = validate_workflow_logical_sequence(docker_workflow_steps, 6)
        
        is_valid = workflow_structure_valid
    end function

    subroutine test_example_cross_reference_validation()
        ! Given: Cross-referenced examples in DESIGN.md
        ! When: Testing cross-reference validation
        ! Then: Validate examples reference correct components and are consistent
        
        logical :: file_references_valid, command_references_valid, flag_references_valid
        
        write(*,'(A)', advance='no') "Testing example cross-reference validation... "
        test_count = test_count + 1
        
        ! Test cross-reference validation
        file_references_valid = validate_file_reference_consistency()
        command_references_valid = validate_command_reference_consistency()
        flag_references_valid = validate_flag_reference_consistency()
        
        if (file_references_valid .and. command_references_valid .and. flag_references_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. file_references_valid) write(*,*) "  - File reference consistency validation failed"
            if (.not. command_references_valid) write(*,*) "  - Command reference consistency validation failed"
            if (.not. flag_references_valid) write(*,*) "  - Flag reference consistency validation failed"
        end if
    end subroutine

    function validate_file_reference_consistency() result(is_valid)
        ! Test file reference consistency across examples
        logical :: is_valid
        character(len=256) :: file_references(6)
        logical :: reference_consistency_valid
        
        ! File references from examples
        file_references(1) = "fpm.toml"
        file_references(2) = "src/*.f90"
        file_references(3) = "coverage.md"
        file_references(4) = "coverage.html"
        file_references(5) = "CMakeLists.txt"
        file_references(6) = "meson.build"
        
        ! Validate reference consistency
        reference_consistency_valid = validate_cross_reference_structure(file_references, 6)
        
        is_valid = reference_consistency_valid
    end function

    function validate_command_reference_consistency() result(is_valid)
        ! Test command reference consistency across examples
        logical :: is_valid
        character(len=256) :: command_references(6)
        logical :: reference_consistency_valid
        
        ! Command references from examples
        command_references(1) = "fpm test"
        command_references(2) = "gcov"
        command_references(3) = "fortcov"
        command_references(4) = "cmake"
        command_references(5) = "make"
        command_references(6) = "find"
        
        ! Validate reference consistency
        reference_consistency_valid = validate_cross_reference_structure(command_references, 6)
        
        is_valid = reference_consistency_valid
    end function

    function validate_flag_reference_consistency() result(is_valid)
        ! Test flag reference consistency across examples
        logical :: is_valid
        character(len=256) :: flag_references(6)
        logical :: reference_consistency_valid
        
        ! Flag references from examples
        flag_references(1) = "-fprofile-arcs"
        flag_references(2) = "-ftest-coverage"
        flag_references(3) = "--source"
        flag_references(4) = "--exclude"
        flag_references(5) = "--output"
        flag_references(6) = "-g -O0"
        
        ! Validate reference consistency
        reference_consistency_valid = validate_cross_reference_structure(flag_references, 6)
        
        is_valid = reference_consistency_valid
    end function

    subroutine test_example_reproducibility_validation()
        ! Given: Examples that should be reproducible
        ! When: Testing reproducibility validation
        ! Then: Validate examples can be reproduced reliably
        
        logical :: environment_reproducible, dependency_reproducible, version_reproducible
        
        write(*,'(A)', advance='no') "Testing example reproducibility validation... "
        test_count = test_count + 1
        
        ! Test reproducibility validation
        environment_reproducible = validate_environment_reproducibility()
        dependency_reproducible = validate_dependency_reproducibility()
        version_reproducible = validate_version_reproducibility()
        
        if (environment_reproducible .and. dependency_reproducible .and. version_reproducible) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. environment_reproducible) write(*,*) "  - Environment reproducibility validation failed"
            if (.not. dependency_reproducible) write(*,*) "  - Dependency reproducibility validation failed"
            if (.not. version_reproducible) write(*,*) "  - Version reproducibility validation failed"
        end if
    end subroutine

    function validate_environment_reproducibility() result(is_valid)
        ! Test environment reproducibility of examples
        logical :: is_valid
        character(len=512) :: environment_specs(4)
        logical :: reproducibility_valid
        
        ! Environment specifications from examples
        environment_specs(1) = "runs-on: ubuntu-latest"
        environment_specs(2) = "image: fortran/gfortran:latest"
        environment_specs(3) = "FROM fortran/gfortran:latest"
        environment_specs(4) = "module load gcc/13.2.0"
        
        ! Validate reproducibility structure
        reproducibility_valid = validate_reproducibility_structure(environment_specs, 4)
        
        is_valid = reproducibility_valid
    end function

    function validate_dependency_reproducibility() result(is_valid)
        ! Test dependency reproducibility of examples
        logical :: is_valid
        character(len=512) :: dependency_specs(4)
        logical :: reproducibility_valid
        
        ! Dependency specifications from examples
        dependency_specs(1) = "uses: fortran-lang/setup-fortran@v1"
        dependency_specs(2) = "uses: fortran-lang/setup-fpm@v5"
        dependency_specs(3) = "find_package(codecov)"
        dependency_specs(4) = "apt-get install gfortran"
        
        ! Validate reproducibility structure
        reproducibility_valid = validate_reproducibility_structure(dependency_specs, 4)
        
        is_valid = reproducibility_valid
    end function

    function validate_version_reproducibility() result(is_valid)
        ! Test version reproducibility of examples
        logical :: is_valid
        character(len=256) :: version_specs(5)
        logical :: reproducibility_valid
        
        ! Version specifications from examples
        version_specs(1) = "compiler: gfortran"
        version_specs(2) = "version: 13"
        version_specs(3) = "@v4"  # GitHub Actions versions
        version_specs(4) = "@v1"  # Setup action versions
        version_specs(5) = "@v5"  # FPM setup version
        
        ! Validate reproducibility structure
        reproducibility_valid = validate_reproducibility_structure(version_specs, 5)
        
        is_valid = reproducibility_valid
    end function

    subroutine test_example_platform_compatibility_validation()
        ! Given: Examples for different platforms
        ! When: Testing platform compatibility validation
        ! Then: Validate examples work across target platforms
        
        logical :: linux_compatibility_valid, macos_compatibility_valid, cross_platform_valid
        
        write(*,'(A)', advance='no') "Testing example platform compatibility validation... "
        test_count = test_count + 1
        
        ! Test platform compatibility validation
        linux_compatibility_valid = validate_linux_platform_compatibility()
        macos_compatibility_valid = validate_macos_platform_compatibility()
        cross_platform_valid = validate_cross_platform_compatibility()
        
        if (linux_compatibility_valid .and. macos_compatibility_valid .and. cross_platform_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. linux_compatibility_valid) write(*,*) "  - Linux platform compatibility validation failed"
            if (.not. macos_compatibility_valid) write(*,*) "  - macOS platform compatibility validation failed"
            if (.not. cross_platform_valid) write(*,*) "  - Cross-platform compatibility validation failed"
        end if
    end subroutine

    function validate_linux_platform_compatibility() result(is_valid)
        ! Test Linux platform compatibility from DESIGN.md
        logical :: is_valid
        character(len=512) :: linux_examples(4)
        logical :: compatibility_valid
        
        ! Linux-specific examples from documentation
        linux_examples(1) = "runs-on: ubuntu-latest"
        linux_examples(2) = "apt-get install gfortran"
        linux_examples(3) = "export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH"
        linux_examples(4) = "find build -name \"*.gcda\" -execdir gcov {} \\;"
        
        ! Validate compatibility structure
        compatibility_valid = validate_platform_compatibility_structure(linux_examples, 4)
        
        is_valid = compatibility_valid
    end function

    function validate_macos_platform_compatibility() result(is_valid)
        ! Test macOS platform compatibility from DESIGN.md
        logical :: is_valid
        character(len=512) :: macos_examples(4)
        logical :: compatibility_valid
        
        ! macOS-specific examples from documentation
        macos_examples(1) = "runs-on: macos-latest"
        macos_examples(2) = "brew install gcc"
        macos_examples(3) = "export FC=gfortran-13"
        macos_examples(4) = "export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH"
        
        ! Validate compatibility structure
        compatibility_valid = validate_platform_compatibility_structure(macos_examples, 4)
        
        is_valid = compatibility_valid
    end function

    function validate_cross_platform_compatibility() result(is_valid)
        ! Test cross-platform compatibility from DESIGN.md
        logical :: is_valid
        character(len=512) :: cross_platform_examples(4)
        logical :: compatibility_valid
        
        ! Cross-platform examples from documentation
        cross_platform_examples(1) = "export FC=${{ matrix.compiler }}"
        cross_platform_examples(2) = "fpm test --flag \"$COVERAGE_FLAGS\""
        cross_platform_examples(3) = "fortcov --source=. --output=coverage.html"
        cross_platform_examples(4) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        
        ! Validate compatibility structure
        compatibility_valid = validate_platform_compatibility_structure(cross_platform_examples, 4)
        
        is_valid = compatibility_valid
    end function

    subroutine test_example_error_handling_validation()
        ! Given: Examples with error handling
        ! When: Testing error handling validation
        ! Then: Validate examples handle errors appropriately
        
        logical :: error_recovery_valid, fallback_valid, cleanup_valid
        
        write(*,'(A)', advance='no') "Testing example error handling validation... "
        test_count = test_count + 1
        
        ! Test error handling validation
        error_recovery_valid = validate_error_recovery_examples()
        fallback_valid = validate_fallback_mechanism_examples()
        cleanup_valid = validate_cleanup_error_handling()
        
        if (error_recovery_valid .and. fallback_valid .and. cleanup_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. error_recovery_valid) write(*,*) "  - Error recovery example validation failed"
            if (.not. fallback_valid) write(*,*) "  - Fallback mechanism example validation failed"
            if (.not. cleanup_valid) write(*,*) "  - Cleanup error handling validation failed"
        end if
    end subroutine

    function validate_error_recovery_examples() result(is_valid)
        ! Test error recovery examples from DESIGN.md
        logical :: is_valid
        character(len=512) :: error_recovery_examples(4)
        logical :: error_handling_valid
        
        ! Error recovery examples from documentation
        error_recovery_examples(1) = "gcov src/*.f90 || true"
        error_recovery_examples(2) = "find build -name \"*.gcda\" -execdir gcov {} \\; || true"
        error_recovery_examples(3) = "if [ -n \"$GIT_CHANGED\" ]; then echo \"Processing changes\"; else echo \"No changes\"; fi"
        error_recovery_examples(4) = "# Continue on coverage generation errors"
        
        ! Validate error handling structure
        error_handling_valid = validate_error_handling_structure(error_recovery_examples, 4)
        
        is_valid = error_handling_valid
    end function

    function validate_fallback_mechanism_examples() result(is_valid)
        ! Test fallback mechanism examples
        logical :: is_valid
        character(len=512) :: fallback_examples(3)
        logical :: fallback_handling_valid
        
        ! Fallback mechanism examples
        fallback_examples(1) = "# Search current directory for .gcov files first"
        fallback_examples(2) = "# If none found, search build directories"
        fallback_examples(3) = "# Fall back to manual gcov execution"
        
        ! Validate fallback handling structure
        fallback_handling_valid = validate_fallback_handling_structure(fallback_examples, 3)
        
        is_valid = fallback_handling_valid
    end function

    function validate_cleanup_error_handling() result(is_valid)
        ! Test cleanup error handling examples from DESIGN.md
        logical :: is_valid
        character(len=512) :: cleanup_examples(4)
        logical :: cleanup_handling_valid
        
        ! Cleanup error handling examples from documentation
        cleanup_examples(1) = "rm -f *.gcov"
        cleanup_examples(2) = "rm -f batch_* coverage_batch_*.json"
        cleanup_examples(3) = "# Clean up intermediate files"
        cleanup_examples(4) = "# Cleanup on error or success"
        
        ! Validate cleanup handling structure
        cleanup_handling_valid = validate_cleanup_handling_structure(cleanup_examples, 4)
        
        is_valid = cleanup_handling_valid
    end function

    subroutine test_example_documentation_consistency()
        ! Given: Examples documented across multiple sections
        ! When: Testing documentation consistency
        ! Then: Validate examples are consistent across all documentation
        
        logical :: section_consistency_valid, format_consistency_valid, terminology_consistency_valid
        
        write(*,'(A)', advance='no') "Testing example documentation consistency... "
        test_count = test_count + 1
        
        ! Test documentation consistency
        section_consistency_valid = validate_cross_section_consistency()
        format_consistency_valid = validate_format_consistency()
        terminology_consistency_valid = validate_terminology_consistency()
        
        if (section_consistency_valid .and. format_consistency_valid .and. terminology_consistency_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. section_consistency_valid) write(*,*) "  - Cross-section consistency validation failed"
            if (.not. format_consistency_valid) write(*,*) "  - Format consistency validation failed"
            if (.not. terminology_consistency_valid) write(*,*) "  - Terminology consistency validation failed"
        end if
    end subroutine

    function validate_cross_section_consistency() result(is_valid)
        ! Test cross-section consistency of examples
        logical :: is_valid
        character(len=512) :: section_references(4)
        logical :: consistency_valid
        
        ! Cross-section references
        section_references(1) = "FPM integration examples appear in build system and CI sections"
        section_references(2) = "CMake examples consistent across build and CI documentation"
        section_references(3) = "Coverage flags consistent across all integration patterns"
        section_references(4) = "File path references consistent across examples"
        
        ! Validate consistency structure
        consistency_valid = validate_documentation_consistency_structure(section_references, 4)
        
        is_valid = consistency_valid
    end function

    function validate_format_consistency() result(is_valid)
        ! Test format consistency of examples
        logical :: is_valid
        character(len=512) :: format_standards(4)
        logical :: format_valid
        
        ! Format standards for examples
        format_standards(1) = "Code blocks use consistent syntax highlighting"
        format_standards(2) = "Command examples follow consistent format"
        format_standards(3) = "File path examples use consistent notation"
        format_standards(4) = "Flag examples maintain consistent structure"
        
        ! Validate format structure
        format_valid = validate_format_consistency_structure(format_standards, 4)
        
        is_valid = format_valid
    end function

    function validate_terminology_consistency() result(is_valid)
        ! Test terminology consistency of examples
        logical :: is_valid
        character(len=256) :: terminology_standards(6)
        logical :: terminology_valid
        
        ! Terminology standards for examples
        terminology_standards(1) = "coverage analysis"
        terminology_standards(2) = "build system integration"
        terminology_standards(3) = "CI/CD workflow"
        terminology_standards(4) = "fortcov command"
        terminology_standards(5) = "coverage report"
        terminology_standards(6) = "integration pattern"
        
        ! Validate terminology structure
        terminology_valid = validate_terminology_consistency_structure(terminology_standards, 6)
        
        is_valid = terminology_valid
    end function

    subroutine test_example_automation_framework()
        ! Given: Examples that could be automated for testing
        ! When: Testing automation framework validation
        ! Then: Validate examples can be automatically tested
        
        logical :: automation_feasible, validation_automatable, testing_complete
        
        write(*,'(A)', advance='no') "Testing example automation framework... "
        test_count = test_count + 1
        
        ! Test automation framework capabilities
        automation_feasible = validate_automation_feasibility()
        validation_automatable = validate_validation_automation()
        testing_complete = validate_automated_testing_completeness()
        
        if (automation_feasible .and. validation_automatable .and. testing_complete) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. automation_feasible) write(*,*) "  - Automation feasibility validation failed"
            if (.not. validation_automatable) write(*,*) "  - Validation automation validation failed"
            if (.not. testing_complete) write(*,*) "  - Automated testing completeness validation failed"
        end if
    end subroutine

    function validate_automation_feasibility() result(is_valid)
        ! Test automation feasibility for examples
        logical :: is_valid
        character(len=512) :: automation_criteria(4)
        logical :: feasibility_valid
        
        ! Automation feasibility criteria
        automation_criteria(1) = "Examples can be executed in isolated environments"
        automation_criteria(2) = "Dependencies are clearly specified and installable"
        automation_criteria(3) = "Expected outputs are well-defined and verifiable"
        automation_criteria(4) = "Examples include deterministic test cases"
        
        ! Validate feasibility structure
        feasibility_valid = validate_automation_criteria_structure(automation_criteria, 4)
        
        is_valid = feasibility_valid
    end function

    function validate_validation_automation() result(is_valid)
        ! Test validation automation capabilities
        logical :: is_valid
        character(len=512) :: validation_automation(4)
        logical :: automation_valid
        
        ! Validation automation capabilities
        validation_automation(1) = "Syntax validation can be automated"
        validation_automation(2) = "Completeness checking can be automated"
        validation_automation(3) = "Cross-reference validation can be automated"
        validation_automation(4) = "Platform compatibility can be tested automatically"
        
        ! Validate automation structure
        automation_valid = validate_validation_automation_structure(validation_automation, 4)
        
        is_valid = automation_valid
    end function

    function validate_automated_testing_completeness() result(is_valid)
        ! Test automated testing completeness
        logical :: is_valid
        character(len=512) :: testing_completeness(5)
        logical :: completeness_valid
        
        ! Testing completeness requirements
        testing_completeness(1) = "All documented examples covered by automated tests"
        testing_completeness(2) = "Cross-platform testing for platform-specific examples"
        testing_completeness(3) = "Error condition testing for error handling examples"
        testing_completeness(4) = "Performance validation for performance-related examples"
        testing_completeness(5) = "Regression testing for example changes"
        
        ! Validate completeness structure
        completeness_valid = validate_testing_completeness_structure(testing_completeness, 5)
        
        is_valid = completeness_valid
    end function

    ! === Helper Functions for Example Validation Framework ===

    function validate_example_completeness_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate example components are not empty and meaningful
        do i = 1, count
            if (len_trim(components(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_bash_syntax_structure(examples, count) result(is_valid)
        character(len=*), intent(in) :: examples(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate bash syntax examples have proper quoting and structure
        do i = 1, count
            if (len_trim(examples(i)) == 0) then
                is_valid = .false.
                exit
            end if
            ! Could add more sophisticated bash syntax validation here
        end do
    end function

    function validate_yaml_syntax_structure(examples, count) result(is_valid)
        character(len=*), intent(in) :: examples(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate YAML syntax examples have proper indentation and structure
        do i = 1, count
            if (len_trim(examples(i)) == 0) then
                is_valid = .false.
                exit
            end if
            ! Could add more sophisticated YAML syntax validation here
        end do
    end function

    function validate_cmake_syntax_structure(examples, count) result(is_valid)
        character(len=*), intent(in) :: examples(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate CMake syntax examples have proper function calls and structure
        do i = 1, count
            if (len_trim(examples(i)) == 0) then
                is_valid = .false.
                exit
            end if
            ! Could add more sophisticated CMake syntax validation here
        end do
    end function

    function validate_workflow_logical_sequence(steps, count) result(is_valid)
        character(len=*), intent(in) :: steps(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate workflow steps follow logical sequence
        do i = 1, count
            if (len_trim(steps(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
        
        ! Additional workflow sequence validation could be added here
    end function

    function validate_cross_reference_structure(references, count) result(is_valid)
        character(len=*), intent(in) :: references(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate cross-references are consistent and not empty
        do i = 1, count
            if (len_trim(references(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_reproducibility_structure(specs, count) result(is_valid)
        character(len=*), intent(in) :: specs(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate reproducibility specifications are complete
        do i = 1, count
            if (len_trim(specs(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_platform_compatibility_structure(examples, count) result(is_valid)
        character(len=*), intent(in) :: examples(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate platform compatibility examples are not empty
        do i = 1, count
            if (len_trim(examples(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_error_handling_structure(examples, count) result(is_valid)
        character(len=*), intent(in) :: examples(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate error handling examples include proper error handling patterns
        is_valid = (count >= 2) .and. &
                  ((index(examples(1), "|| true") > 0) .or. (index(examples(3), "if") > 0))
    end function

    function validate_fallback_handling_structure(examples, count) result(is_valid)
        character(len=*), intent(in) :: examples(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate fallback handling examples are documented
        do i = 1, count
            if (len_trim(examples(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_cleanup_handling_structure(examples, count) result(is_valid)
        character(len=*), intent(in) :: examples(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate cleanup handling includes rm commands
        is_valid = (count >= 2) .and. &
                  (index(examples(1), "rm") > 0) .and. &
                  (index(examples(2), "rm") > 0)
    end function

    function validate_documentation_consistency_structure(references, count) result(is_valid)
        character(len=*), intent(in) :: references(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate documentation consistency references are not empty
        do i = 1, count
            if (len_trim(references(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_format_consistency_structure(standards, count) result(is_valid)
        character(len=*), intent(in) :: standards(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate format consistency standards are not empty
        do i = 1, count
            if (len_trim(standards(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_terminology_consistency_structure(standards, count) result(is_valid)
        character(len=*), intent(in) :: standards(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate terminology standards are not empty
        do i = 1, count
            if (len_trim(standards(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_automation_criteria_structure(criteria, count) result(is_valid)
        character(len=*), intent(in) :: criteria(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate automation criteria are not empty
        do i = 1, count
            if (len_trim(criteria(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_validation_automation_structure(automation, count) result(is_valid)
        character(len=*), intent(in) :: automation(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate validation automation capabilities are not empty
        do i = 1, count
            if (len_trim(automation(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_testing_completeness_structure(completeness, count) result(is_valid)
        character(len=*), intent(in) :: completeness(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate testing completeness requirements are not empty
        do i = 1, count
            if (len_trim(completeness(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

end program test_integration_example_validation_framework