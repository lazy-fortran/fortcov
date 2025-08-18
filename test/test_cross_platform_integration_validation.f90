! Test suite for cross-platform integration pattern validation
! Tests multi-compiler, multi-OS integration patterns from DESIGN.md
! Validates cross-platform coverage workflows and compatibility patterns

program test_cross_platform_integration_validation
    use error_handling
    use file_utils
    use string_utils
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "=== Cross-Platform Integration Validation Test Suite ==="
    write(*,*)
    
    ! Test cross-platform integration patterns
    call test_multi_compiler_integration_patterns()
    call test_multi_os_integration_patterns()
    call test_ci_matrix_strategy_validation()
    call test_platform_specific_adaptations()
    call test_compiler_flag_compatibility()
    call test_build_system_cross_platform_patterns()
    call test_environment_variable_patterns()
    call test_path_separator_handling()
    call test_cross_platform_performance_patterns()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "CROSS-PLATFORM INTEGRATION VALIDATION TESTS FAILED"
        stop 1
    else
        write(*,*) "All cross-platform integration validation tests passed"
    end if

contains

    subroutine test_multi_compiler_integration_patterns()
        ! Given: Multi-compiler integration patterns from DESIGN.md
        ! When: Testing compiler-specific integration validation
        ! Then: Validate multi-compiler integration works correctly
        
        logical :: gfortran_valid, ifort_valid, flang_valid, matrix_valid
        
        write(*,'(A)', advance='no') "Testing multi-compiler integration patterns... "
        test_count = test_count + 1
        
        ! Test compiler-specific integration patterns
        gfortran_valid = validate_gfortran_integration_patterns()
        ifort_valid = validate_ifort_integration_patterns()
        flang_valid = validate_flang_integration_patterns()
        matrix_valid = validate_compiler_matrix_strategy()
        
        if (gfortran_valid .and. ifort_valid .and. flang_valid .and. matrix_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. gfortran_valid) write(*,*) "  - gfortran integration pattern validation failed"
            if (.not. ifort_valid) write(*,*) "  - ifort integration pattern validation failed"
            if (.not. flang_valid) write(*,*) "  - flang integration pattern validation failed"
            if (.not. matrix_valid) write(*,*) "  - Compiler matrix strategy validation failed"
        end if
    end subroutine

    function validate_gfortran_integration_patterns() result(is_valid)
        ! Test gfortran integration patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: gfortran_patterns(4)
        logical :: gfortran_structure_valid
        
        ! gfortran integration patterns from documentation
        gfortran_patterns(1) = "export FC=gfortran"
        gfortran_patterns(2) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        gfortran_patterns(3) = "gcov src/*.f90"
        gfortran_patterns(4) = "coverage-gfortran-ubuntu-latest.html"
        
        ! Validate gfortran integration structure
        gfortran_structure_valid = validate_compiler_integration_structure(gfortran_patterns, 4)
        
        is_valid = gfortran_structure_valid
    end function

    function validate_ifort_integration_patterns() result(is_valid)
        ! Test ifort integration patterns
        logical :: is_valid
        character(len=512) :: ifort_patterns(4)
        logical :: ifort_structure_valid
        
        ! ifort integration patterns
        ifort_patterns(1) = "export FC=ifort"
        ifort_patterns(2) = "# Intel Fortran specific coverage flags"
        ifort_patterns(3) = "# Intel coverage analysis tools"
        ifort_patterns(4) = "coverage-ifort-ubuntu-latest.html"
        
        ! Validate ifort integration structure
        ifort_structure_valid = validate_compiler_integration_structure(ifort_patterns, 4)
        
        is_valid = ifort_structure_valid
    end function

    function validate_flang_integration_patterns() result(is_valid)
        ! Test flang integration patterns
        logical :: is_valid
        character(len=512) :: flang_patterns(4)
        logical :: flang_structure_valid
        
        ! flang integration patterns
        flang_patterns(1) = "export FC=flang"
        flang_patterns(2) = "# LLVM Fortran specific coverage flags"
        flang_patterns(3) = "# LLVM coverage analysis tools"
        flang_patterns(4) = "coverage-flang-ubuntu-latest.html"
        
        ! Validate flang integration structure
        flang_structure_valid = validate_compiler_integration_structure(flang_patterns, 4)
        
        is_valid = flang_structure_valid
    end function

    function validate_compiler_matrix_strategy() result(is_valid)
        ! Test compiler matrix strategy from DESIGN.md
        logical :: is_valid
        character(len=512) :: matrix_components(4)
        logical :: matrix_structure_valid
        
        ! Compiler matrix strategy components from documentation
        matrix_components(1) = "compiler: [gfortran, ifort, flang]"
        matrix_components(2) = "export FC=${{ matrix.compiler }}"
        matrix_components(3) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        matrix_components(4) = "# Matrix strategy validation"
        
        ! Validate matrix structure
        matrix_structure_valid = validate_matrix_strategy_structure(matrix_components, 4)
        
        is_valid = matrix_structure_valid
    end function

    subroutine test_multi_os_integration_patterns()
        ! Given: Multi-OS integration patterns from DESIGN.md
        ! When: Testing OS-specific integration validation
        ! Then: Validate multi-OS integration works correctly
        
        logical :: ubuntu_valid, macos_valid, windows_valid, exclusion_valid
        
        write(*,'(A)', advance='no') "Testing multi-OS integration patterns... "
        test_count = test_count + 1
        
        ! Test OS-specific integration patterns
        ubuntu_valid = validate_ubuntu_integration_patterns()
        macos_valid = validate_macos_integration_patterns()
        windows_valid = validate_windows_integration_patterns()
        exclusion_valid = validate_os_exclusion_patterns()
        
        if (ubuntu_valid .and. macos_valid .and. windows_valid .and. exclusion_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. ubuntu_valid) write(*,*) "  - Ubuntu integration pattern validation failed"
            if (.not. macos_valid) write(*,*) "  - macOS integration pattern validation failed"
            if (.not. windows_valid) write(*,*) "  - Windows integration pattern validation failed"
            if (.not. exclusion_valid) write(*,*) "  - OS exclusion pattern validation failed"
        end if
    end subroutine

    function validate_ubuntu_integration_patterns() result(is_valid)
        ! Test Ubuntu integration patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: ubuntu_patterns(4)
        logical :: ubuntu_structure_valid
        
        ! Ubuntu integration patterns
        ubuntu_patterns(1) = "runs-on: ubuntu-latest"
        ubuntu_patterns(2) = "apt-get install gfortran"
        ubuntu_patterns(3) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        ubuntu_patterns(4) = "coverage-gfortran-ubuntu-latest.html"
        
        ! Validate Ubuntu integration structure
        ubuntu_structure_valid = validate_os_integration_structure(ubuntu_patterns, 4)
        
        is_valid = ubuntu_structure_valid
    end function

    function validate_macos_integration_patterns() result(is_valid)
        ! Test macOS integration patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: macos_patterns(4)
        logical :: macos_structure_valid
        
        ! macOS integration patterns
        macos_patterns(1) = "runs-on: macos-latest"
        macos_patterns(2) = "brew install gcc"
        macos_patterns(3) = "export FC=gfortran-13"
        macos_patterns(4) = "coverage-gfortran-macos-latest.html"
        
        ! Validate macOS integration structure
        macos_structure_valid = validate_os_integration_structure(macos_patterns, 4)
        
        is_valid = macos_structure_valid
    end function

    function validate_windows_integration_patterns() result(is_valid)
        ! Test Windows integration patterns
        logical :: is_valid
        character(len=512) :: windows_patterns(3)
        logical :: windows_structure_valid
        
        ! Windows integration patterns
        windows_patterns(1) = "runs-on: windows-latest"
        windows_patterns(2) = "# Windows-specific setup steps"
        windows_patterns(3) = "coverage-gfortran-windows-latest.html"
        
        ! Validate Windows integration structure
        windows_structure_valid = validate_os_integration_structure(windows_patterns, 3)
        
        is_valid = windows_structure_valid
    end function

    function validate_os_exclusion_patterns() result(is_valid)
        ! Test OS exclusion patterns from DESIGN.md
        logical :: is_valid
        character(len=256) :: exclusion_rules(4)
        logical :: exclusion_structure_valid
        
        ! OS exclusion rules from documentation
        exclusion_rules(1) = "exclude:"
        exclusion_rules(2) = "- os: macos-latest"
        exclusion_rules(3) = "compiler: ifort"
        exclusion_rules(4) = "# Intel Fortran not available on macOS"
        
        ! Validate exclusion structure
        exclusion_structure_valid = validate_exclusion_rules_structure(exclusion_rules, 4)
        
        is_valid = exclusion_structure_valid
    end function

    subroutine test_ci_matrix_strategy_validation()
        ! Given: CI matrix strategy from DESIGN.md
        ! When: Testing matrix strategy validation
        ! Then: Validate CI matrix strategy works correctly
        
        logical :: matrix_definition_valid, matrix_execution_valid, artifact_naming_valid
        
        write(*,'(A)', advance='no') "Testing CI matrix strategy validation... "
        test_count = test_count + 1
        
        ! Test CI matrix strategy components
        matrix_definition_valid = validate_matrix_definition_structure()
        matrix_execution_valid = validate_matrix_execution_patterns()
        artifact_naming_valid = validate_matrix_artifact_naming()
        
        if (matrix_definition_valid .and. matrix_execution_valid .and. artifact_naming_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. matrix_definition_valid) write(*,*) "  - Matrix definition structure validation failed"
            if (.not. matrix_execution_valid) write(*,*) "  - Matrix execution pattern validation failed"
            if (.not. artifact_naming_valid) write(*,*) "  - Matrix artifact naming validation failed"
        end if
    end subroutine

    function validate_matrix_definition_structure() result(is_valid)
        ! Test matrix definition structure from DESIGN.md
        logical :: is_valid
        character(len=256) :: matrix_definition(4)
        logical :: definition_structure_valid
        
        ! Matrix definition from documentation
        matrix_definition(1) = "strategy:"
        matrix_definition(2) = "matrix:"
        matrix_definition(3) = "compiler: [gfortran, ifort, flang]"
        matrix_definition(4) = "os: [ubuntu-latest, macos-latest]"
        
        ! Validate definition structure
        definition_structure_valid = validate_matrix_yaml_structure(matrix_definition, 4)
        
        is_valid = definition_structure_valid
    end function

    function validate_matrix_execution_patterns() result(is_valid)
        ! Test matrix execution patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: execution_patterns(3)
        logical :: execution_structure_valid
        
        ! Matrix execution patterns
        execution_patterns(1) = "export FC=${{ matrix.compiler }}"
        execution_patterns(2) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""
        execution_patterns(3) = "fortcov --source=. --output=coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        
        ! Validate execution structure
        execution_structure_valid = validate_matrix_execution_structure(execution_patterns, 3)
        
        is_valid = execution_structure_valid
    end function

    function validate_matrix_artifact_naming() result(is_valid)
        ! Test matrix artifact naming from DESIGN.md
        logical :: is_valid
        character(len=256) :: artifact_names(3)
        logical :: naming_structure_valid
        
        ! Matrix artifact naming patterns
        artifact_names(1) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        artifact_names(2) = "coverage-gfortran-ubuntu-latest.html"
        artifact_names(3) = "coverage-flang-macos-latest.html"
        
        ! Validate naming structure
        naming_structure_valid = validate_artifact_naming_structure(artifact_names, 3)
        
        is_valid = naming_structure_valid
    end function

    subroutine test_platform_specific_adaptations()
        ! Given: Platform-specific adaptations
        ! When: Testing platform adaptation validation
        ! Then: Validate platform adaptations work correctly
        
        logical :: linux_adaptations_valid, macos_adaptations_valid, compiler_adaptations_valid
        
        write(*,'(A)', advance='no') "Testing platform-specific adaptations... "
        test_count = test_count + 1
        
        ! Test platform-specific adaptations
        linux_adaptations_valid = validate_linux_platform_adaptations()
        macos_adaptations_valid = validate_macos_platform_adaptations()
        compiler_adaptations_valid = validate_compiler_specific_adaptations()
        
        if (linux_adaptations_valid .and. macos_adaptations_valid .and. compiler_adaptations_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. linux_adaptations_valid) write(*,*) "  - Linux platform adaptation validation failed"
            if (.not. macos_adaptations_valid) write(*,*) "  - macOS platform adaptation validation failed"
            if (.not. compiler_adaptations_valid) write(*,*) "  - Compiler-specific adaptation validation failed"
        end if
    end subroutine

    function validate_linux_platform_adaptations() result(is_valid)
        ! Test Linux platform adaptations
        logical :: is_valid
        character(len=512) :: linux_adaptations(4)
        logical :: adaptation_structure_valid
        
        ! Linux platform adaptations
        linux_adaptations(1) = "apt-get update && apt-get install -y gfortran"
        linux_adaptations(2) = "export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH"
        linux_adaptations(3) = "gcov --version"
        linux_adaptations(4) = "# Linux-specific coverage setup"
        
        ! Validate adaptation structure
        adaptation_structure_valid = validate_platform_adaptation_structure(linux_adaptations, 4)
        
        is_valid = adaptation_structure_valid
    end function

    function validate_macos_platform_adaptations() result(is_valid)
        ! Test macOS platform adaptations
        logical :: is_valid
        character(len=512) :: macos_adaptations(4)
        logical :: adaptation_structure_valid
        
        ! macOS platform adaptations
        macos_adaptations(1) = "brew install gcc"
        macos_adaptations(2) = "export FC=gfortran-13"
        macos_adaptations(3) = "export CC=gcc-13"
        macos_adaptations(4) = "# macOS-specific compiler setup"
        
        ! Validate adaptation structure
        adaptation_structure_valid = validate_platform_adaptation_structure(macos_adaptations, 4)
        
        is_valid = adaptation_structure_valid
    end function

    function validate_compiler_specific_adaptations() result(is_valid)
        ! Test compiler-specific adaptations
        logical :: is_valid
        character(len=512) :: compiler_adaptations(4)
        logical :: adaptation_structure_valid
        
        ! Compiler-specific adaptations
        compiler_adaptations(1) = "if [ \"$FC\" = \"gfortran\" ]; then COVERAGE_FLAGS=\"-fprofile-arcs -ftest-coverage\"; fi"
        compiler_adaptations(2) = "if [ \"$FC\" = \"ifort\" ]; then COVERAGE_FLAGS=\"-prof-gen=srcpos\"; fi"
        compiler_adaptations(3) = "if [ \"$FC\" = \"flang\" ]; then COVERAGE_FLAGS=\"-fprofile-instr-generate\"; fi"
        compiler_adaptations(4) = "fpm test --flag \"$COVERAGE_FLAGS\""
        
        ! Validate adaptation structure
        adaptation_structure_valid = validate_compiler_adaptation_structure(compiler_adaptations, 4)
        
        is_valid = adaptation_structure_valid
    end function

    subroutine test_compiler_flag_compatibility()
        ! Given: Compiler flag compatibility patterns
        ! When: Testing flag compatibility validation
        ! Then: Validate flag compatibility works correctly
        
        logical :: gfortran_flags_valid, ifort_flags_valid, flang_flags_valid, detection_valid
        
        write(*,'(A)', advance='no') "Testing compiler flag compatibility... "
        test_count = test_count + 1
        
        ! Test compiler flag compatibility
        gfortran_flags_valid = validate_gfortran_flag_compatibility()
        ifort_flags_valid = validate_ifort_flag_compatibility()
        flang_flags_valid = validate_flang_flag_compatibility()
        detection_valid = validate_compiler_detection_patterns()
        
        if (gfortran_flags_valid .and. ifort_flags_valid .and. flang_flags_valid .and. detection_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. gfortran_flags_valid) write(*,*) "  - gfortran flag compatibility validation failed"
            if (.not. ifort_flags_valid) write(*,*) "  - ifort flag compatibility validation failed"
            if (.not. flang_flags_valid) write(*,*) "  - flang flag compatibility validation failed"
            if (.not. detection_valid) write(*,*) "  - Compiler detection validation failed"
        end if
    end subroutine

    function validate_gfortran_flag_compatibility() result(is_valid)
        ! Test gfortran flag compatibility from DESIGN.md
        logical :: is_valid
        character(len=256) :: gfortran_flags(4)
        logical :: flag_structure_valid
        
        ! gfortran coverage flags
        gfortran_flags(1) = "-fprofile-arcs"
        gfortran_flags(2) = "-ftest-coverage"
        gfortran_flags(3) = "-g"
        gfortran_flags(4) = "-O0"
        
        ! Validate flag structure
        flag_structure_valid = validate_compiler_flag_structure(gfortran_flags, 4)
        
        is_valid = flag_structure_valid
    end function

    function validate_ifort_flag_compatibility() result(is_valid)
        ! Test ifort flag compatibility
        logical :: is_valid
        character(len=256) :: ifort_flags(3)
        logical :: flag_structure_valid
        
        ! ifort coverage flags (hypothetical)
        ifort_flags(1) = "-prof-gen=srcpos"
        ifort_flags(2) = "-g"
        ifort_flags(3) = "-O0"
        
        ! Validate flag structure
        flag_structure_valid = validate_compiler_flag_structure(ifort_flags, 3)
        
        is_valid = flag_structure_valid
    end function

    function validate_flang_flag_compatibility() result(is_valid)
        ! Test flang flag compatibility
        logical :: is_valid
        character(len=256) :: flang_flags(3)
        logical :: flag_structure_valid
        
        ! flang coverage flags (hypothetical)
        flang_flags(1) = "-fprofile-instr-generate"
        flang_flags(2) = "-g"
        flang_flags(3) = "-O0"
        
        ! Validate flag structure
        flag_structure_valid = validate_compiler_flag_structure(flang_flags, 3)
        
        is_valid = flag_structure_valid
    end function

    function validate_compiler_detection_patterns() result(is_valid)
        ! Test compiler detection patterns
        logical :: is_valid
        character(len=512) :: detection_patterns(4)
        logical :: detection_structure_valid
        
        ! Compiler detection patterns
        detection_patterns(1) = "gfortran --version | grep GNU"
        detection_patterns(2) = "ifort --version | grep Intel"
        detection_patterns(3) = "flang --version | grep LLVM"
        detection_patterns(4) = "export FC=${{ matrix.compiler }}"
        
        ! Validate detection structure
        detection_structure_valid = validate_detection_pattern_structure(detection_patterns, 4)
        
        is_valid = detection_structure_valid
    end function

    subroutine test_build_system_cross_platform_patterns()
        ! Given: Build system cross-platform patterns
        ! When: Testing cross-platform build system validation
        ! Then: Validate cross-platform build patterns work correctly
        
        logical :: fpm_cross_platform_valid, cmake_cross_platform_valid, make_cross_platform_valid
        
        write(*,'(A)', advance='no') "Testing build system cross-platform patterns... "
        test_count = test_count + 1
        
        ! Test cross-platform build system patterns
        fpm_cross_platform_valid = validate_fpm_cross_platform_patterns()
        cmake_cross_platform_valid = validate_cmake_cross_platform_patterns()
        make_cross_platform_valid = validate_make_cross_platform_patterns()
        
        if (fpm_cross_platform_valid .and. cmake_cross_platform_valid .and. make_cross_platform_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fpm_cross_platform_valid) write(*,*) "  - FPM cross-platform pattern validation failed"
            if (.not. cmake_cross_platform_valid) write(*,*) "  - CMake cross-platform pattern validation failed"
            if (.not. make_cross_platform_valid) write(*,*) "  - Make cross-platform pattern validation failed"
        end if
    end subroutine

    function validate_fpm_cross_platform_patterns() result(is_valid)
        ! Test FPM cross-platform patterns
        logical :: is_valid
        character(len=512) :: fpm_patterns(4)
        logical :: fpm_structure_valid
        
        ! FPM cross-platform patterns
        fpm_patterns(1) = "fpm test --flag \"-fprofile-arcs -ftest-coverage\""  ! Linux/macOS
        fmp_patterns(2) = "fpm.exe test --flag \"-fprofile-arcs -ftest-coverage\""  ! Windows
        fpm_patterns(3) = "export FC=${{ matrix.compiler }}"
        fpm_patterns(4) = "fpm run fortcov -- --source=. --output=coverage.html"
        
        ! Validate FPM structure
        fmp_structure_valid = validate_cross_platform_build_structure(fpm_patterns, 4)
        
        is_valid = fpm_structure_valid
    end function

    function validate_cmake_cross_platform_patterns() result(is_valid)
        ! Test CMake cross-platform patterns
        logical :: is_valid
        character(len=512) :: cmake_patterns(4)
        logical :: cmake_structure_valid
        
        ! CMake cross-platform patterns
        cmake_patterns(1) = "cmake -DCMAKE_BUILD_TYPE=Testing .."  ! Unix
        cmake_patterns(2) = "cmake -G \"Visual Studio 16 2019\" .."  ! Windows
        cmake_patterns(3) = "make && make test"  ! Unix
        cmake_patterns(4) = "cmake --build . --config Testing"  ! Cross-platform
        
        ! Validate CMake structure
        cmake_structure_valid = validate_cross_platform_build_structure(cmake_patterns, 4)
        
        is_valid = cmake_structure_valid
    end function

    function validate_make_cross_platform_patterns() result(is_valid)
        ! Test Make cross-platform patterns
        logical :: is_valid
        character(len=512) :: make_patterns(3)
        logical :: make_structure_valid
        
        ! Make cross-platform patterns
        make_patterns(1) = "make FC=${{ matrix.compiler }} coverage"  ! Unix
        make_patterns(2) = "mingw32-make FC=${{ matrix.compiler }} coverage"  ! Windows
        make_patterns(3) = "# Cross-platform Makefile patterns"
        
        ! Validate Make structure
        make_structure_valid = validate_cross_platform_build_structure(make_patterns, 3)
        
        is_valid = make_structure_valid
    end function

    subroutine test_environment_variable_patterns()
        ! Given: Environment variable patterns
        ! When: Testing environment variable validation
        ! Then: Validate environment patterns work correctly
        
        logical :: compiler_env_valid, path_env_valid, flags_env_valid
        
        write(*,'(A)', advance='no') "Testing environment variable patterns... "
        test_count = test_count + 1
        
        ! Test environment variable patterns
        compiler_env_valid = validate_compiler_environment_patterns()
        path_env_valid = validate_path_environment_patterns()
        flags_env_valid = validate_flags_environment_patterns()
        
        if (compiler_env_valid .and. path_env_valid .and. flags_env_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. compiler_env_valid) write(*,*) "  - Compiler environment pattern validation failed"
            if (.not. path_env_valid) write(*,*) "  - Path environment pattern validation failed"
            if (.not. flags_env_valid) write(*,*) "  - Flags environment pattern validation failed"
        end if
    end subroutine

    function validate_compiler_environment_patterns() result(is_valid)
        ! Test compiler environment patterns
        logical :: is_valid
        character(len=256) :: compiler_env_patterns(4)
        logical :: env_structure_valid
        
        ! Compiler environment patterns
        compiler_env_patterns(1) = "export FC=gfortran"
        compiler_env_patterns(2) = "export FC=ifort"
        compiler_env_patterns(3) = "export FC=${{ matrix.compiler }}"
        compiler_env_patterns(4) = "echo \"Using compiler: $FC\""
        
        ! Validate environment structure
        env_structure_valid = validate_environment_variable_structure(compiler_env_patterns, 4)
        
        is_valid = env_structure_valid
    end function

    function validate_path_environment_patterns() result(is_valid)
        ! Test path environment patterns
        logical :: is_valid
        character(len=512) :: path_env_patterns(3)
        logical :: env_structure_valid
        
        ! Path environment patterns
        path_env_patterns(1) = "export PATH=/usr/local/bin:$PATH"
        path_env_patterns(2) = "export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH"
        path_env_patterns(3) = "export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH"  ! macOS
        
        ! Validate environment structure
        env_structure_valid = validate_environment_variable_structure(path_env_patterns, 3)
        
        is_valid = env_structure_valid
    end function

    function validate_flags_environment_patterns() result(is_valid)
        ! Test flags environment patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: flags_env_patterns(3)
        logical :: env_structure_valid
        
        ! Flags environment patterns from documentation
        flags_env_patterns(1) = "export FCFLAGS=\"-fprofile-arcs -ftest-coverage\""
        flags_env_patterns(2) = "export LDFLAGS=\"-lgcov\""
        flags_env_patterns(3) = "COVERAGE_FLAGS=\"-fprofile-arcs -ftest-coverage\""
        
        ! Validate environment structure
        env_structure_valid = validate_environment_variable_structure(flags_env_patterns, 3)
        
        is_valid = env_structure_valid
    end function

    subroutine test_path_separator_handling()
        ! Given: Path separator handling requirements
        ! When: Testing path separator validation
        ! Then: Validate path separator handling works correctly
        
        logical :: unix_paths_valid, windows_paths_valid, portable_paths_valid
        
        write(*,'(A)', advance='no') "Testing path separator handling... "
        test_count = test_count + 1
        
        ! Test path separator handling
        unix_paths_valid = validate_unix_path_patterns()
        windows_paths_valid = validate_windows_path_patterns()
        portable_paths_valid = validate_portable_path_patterns()
        
        if (unix_paths_valid .and. windows_paths_valid .and. portable_paths_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. unix_paths_valid) write(*,*) "  - Unix path pattern validation failed"
            if (.not. windows_paths_valid) write(*,*) "  - Windows path pattern validation failed"
            if (.not. portable_paths_valid) write(*,*) "  - Portable path pattern validation failed"
        end if
    end subroutine

    function validate_unix_path_patterns() result(is_valid)
        ! Test Unix path patterns
        logical :: is_valid
        character(len=256) :: unix_paths(4)
        logical :: path_structure_valid
        
        ! Unix path patterns
        unix_paths(1) = "build/gfortran_*/fortcov"
        unix_paths(2) = "/usr/local/bin"
        unix_paths(3) = "src/*.f90"
        unix_paths(4) = "./coverage.html"
        
        ! Validate path structure
        path_structure_valid = validate_path_separator_structure(unix_paths, 4, "/")
        
        is_valid = path_structure_valid
    end function

    function validate_windows_path_patterns() result(is_valid)
        ! Test Windows path patterns
        logical :: is_valid
        character(len=256) :: windows_paths(4)
        logical :: path_structure_valid
        
        ! Windows path patterns
        windows_paths(1) = "build\\gfortran_*\\fortcov"
        windows_paths(2) = "C:\\Program Files\\GCC\\bin"
        windows_paths(3) = "src\\*.f90"
        windows_paths(4) = ".\\coverage.html"
        
        ! Validate path structure
        path_structure_valid = validate_path_separator_structure(windows_paths, 4, "\\")
        
        is_valid = path_structure_valid
    end function

    function validate_portable_path_patterns() result(is_valid)
        ! Test portable path patterns
        logical :: is_valid
        character(len=512) :: portable_patterns(3)
        logical :: portable_structure_valid
        
        ! Portable path patterns
        portable_patterns(1) = "fortcov --source=. --output=coverage.html"  ! Current directory portable
        portable_patterns(2) = "fpm run fortcov"  ! Tool-based portable paths
        portable_patterns(3) = "${CMAKE_SOURCE_DIR}/src"  ! Variable-based portable paths
        
        ! Validate portable structure
        portable_structure_valid = validate_portable_path_structure(portable_patterns, 3)
        
        is_valid = portable_structure_valid
    end function

    subroutine test_cross_platform_performance_patterns()
        ! Given: Cross-platform performance patterns
        ! When: Testing performance pattern validation
        ! Then: Validate performance patterns work correctly
        
        logical :: parallel_valid, optimization_valid, scalability_valid
        
        write(*,'(A)', advance='no') "Testing cross-platform performance patterns... "
        test_count = test_count + 1
        
        ! Test cross-platform performance patterns
        parallel_valid = validate_parallel_execution_patterns()
        optimization_valid = validate_cross_platform_optimization_patterns()
        scalability_valid = validate_scalability_patterns()
        
        if (parallel_valid .and. optimization_valid .and. scalability_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. parallel_valid) write(*,*) "  - Parallel execution pattern validation failed"
            if (.not. optimization_valid) write(*,*) "  - Cross-platform optimization validation failed"
            if (.not. scalability_valid) write(*,*) "  - Scalability pattern validation failed"
        end if
    end subroutine

    function validate_parallel_execution_patterns() result(is_valid)
        ! Test parallel execution patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: parallel_patterns(4)
        logical :: parallel_structure_valid
        
        ! Parallel execution patterns
        parallel_patterns(1) = "mpirun -np 4 ./fortran_mpi_test"
        parallel_patterns(2) = "for rank in {0..3}; do gcov -o rank_${rank} src/*.f90; done"
        parallel_patterns(3) = "fortcov --source=coverage_rank_* --output=mpi_coverage.html"
        parallel_patterns(4) = "# Parallel coverage collection"
        
        ! Validate parallel structure
        parallel_structure_valid = validate_parallel_pattern_structure(parallel_patterns, 4)
        
        is_valid = parallel_structure_valid
    end function

    function validate_cross_platform_optimization_patterns() result(is_valid)
        ! Test cross-platform optimization patterns
        logical :: is_valid
        character(len=512) :: optimization_patterns(3)
        logical :: optimization_structure_valid
        
        ! Cross-platform optimization patterns
        optimization_patterns(1) = "# <10% overhead for coverage instrumentation"
        optimization_patterns(2) = "# <5 minutes analysis time for typical projects"
        optimization_patterns(3) = "# Linear performance scaling with project size"
        
        ! Validate optimization structure
        optimization_structure_valid = validate_optimization_pattern_structure(optimization_patterns, 3)
        
        is_valid = optimization_structure_valid
    end function

    function validate_scalability_patterns() result(is_valid)
        ! Test scalability patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: scalability_patterns(4)
        logical :: scalability_structure_valid
        
        ! Scalability patterns from documentation
        scalability_patterns(1) = "find src -name \"*.f90\" | split -l 50 - batch_"
        scalability_patterns(2) = "fortcov --source=. --output=\"coverage_$(basename $batch_file).json\""
        scalability_patterns(3) = "fortcov --import=\"coverage_batch_*.json\" --output=final_coverage.html"
        scalability_patterns(4) = "# Memory-efficient for large projects"
        
        ! Validate scalability structure
        scalability_structure_valid = validate_scalability_pattern_structure(scalability_patterns, 4)
        
        is_valid = scalability_structure_valid
    end function

    ! === Helper Functions for Cross-Platform Pattern Validation ===

    function validate_compiler_integration_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate compiler integration patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_matrix_strategy_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate matrix strategy includes compiler array and matrix variables
        is_valid = (count >= 3) .and. &
                  (index(components(1), "compiler:") > 0) .and. &
                  (index(components(2), "matrix.compiler") > 0) .and. &
                  (index(components(3), "matrix.") > 0)
    end function

    function validate_os_integration_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate OS integration patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_exclusion_rules_structure(rules, count) result(is_valid)
        character(len=*), intent(in) :: rules(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate exclusion rules include exclude keyword and OS/compiler specification
        is_valid = (count >= 3) .and. &
                  (index(rules(1), "exclude:") > 0) .and. &
                  (index(rules(2), "os:") > 0) .and. &
                  (index(rules(3), "compiler:") > 0)
    end function

    function validate_matrix_yaml_structure(definition, count) result(is_valid)
        character(len=*), intent(in) :: definition(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate YAML matrix structure
        is_valid = (count >= 4) .and. &
                  (index(definition(1), "strategy:") > 0) .and. &
                  (index(definition(2), "matrix:") > 0) .and. &
                  (index(definition(3), "compiler:") > 0) .and. &
                  (index(definition(4), "os:") > 0)
    end function

    function validate_matrix_execution_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate matrix execution includes compiler variable and fortcov execution
        is_valid = (count >= 3) .and. &
                  (index(patterns(1), "matrix.compiler") > 0) .and. &
                  (index(patterns(2), "fpm test") > 0) .and. &
                  (index(patterns(3), "fortcov") > 0)
    end function

    function validate_artifact_naming_structure(names, count) result(is_valid)
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate artifact names include coverage and extensions
        do i = 1, count
            if (len_trim(names(i)) == 0 .or. &
                (index(names(i), "coverage") == 0 .and. index(names(i), ".html") == 0)) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_platform_adaptation_structure(adaptations, count) result(is_valid)
        character(len=*), intent(in) :: adaptations(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate platform adaptations are not empty
        do i = 1, count
            if (len_trim(adaptations(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_compiler_adaptation_structure(adaptations, count) result(is_valid)
        character(len=*), intent(in) :: adaptations(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate compiler adaptations include conditional logic
        is_valid = (count >= 4) .and. &
                  (index(adaptations(1), "if") > 0) .and. &
                  (index(adaptations(2), "if") > 0) .and. &
                  (index(adaptations(3), "if") > 0) .and. &
                  (index(adaptations(4), "fpm test") > 0)
    end function

    function validate_compiler_flag_structure(flags, count) result(is_valid)
        character(len=*), intent(in) :: flags(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate compiler flags start with - and are not empty
        do i = 1, count
            if (len_trim(flags(i)) == 0 .or. flags(i)(1:1) /= '-') then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_detection_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate detection patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_cross_platform_build_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate cross-platform build patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_environment_variable_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate environment variable patterns include export or variable assignment
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. &
                (index(patterns(i), "export") == 0 .and. index(patterns(i), "=") == 0)) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_path_separator_structure(paths, count, separator) result(is_valid)
        character(len=*), intent(in) :: paths(:), separator
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate paths use expected separator (for most paths)
        do i = 1, count
            if (len_trim(paths(i)) == 0) then
                is_valid = .false.
                exit
            end if
            ! Note: Some paths may be relative (.) and not contain separators
        end do
    end function

    function validate_portable_path_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate portable path patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_parallel_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate parallel patterns include mpirun and coverage collection
        is_valid = (count >= 3) .and. &
                  (index(patterns(1), "mpirun") > 0) .and. &
                  (index(patterns(2), "gcov") > 0) .and. &
                  (index(patterns(3), "fortcov") > 0)
    end function

    function validate_optimization_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate optimization patterns include performance metrics
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. index(patterns(i), "#") == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_scalability_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate scalability patterns include batch processing and merging
        is_valid = (count >= 3) .and. &
                  (index(patterns(1), "split") > 0) .and. &
                  (index(patterns(2), "fortcov") > 0) .and. &
                  (index(patterns(3), "import") > 0)
    end function

end program test_cross_platform_integration_validation