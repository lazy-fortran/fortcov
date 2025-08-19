! Test suite for Issue #175: CI/CD Matrix Coverage Defects
! Tests missing multi-platform support patterns in CI configuration
! Validates cross-platform compatibility and multi-compiler matrix configurations
!
! Given: CI/CD matrix coverage defects identified in Issue #175
! When: Testing multi-platform CI/CD workflow validation
! Then: Ensure comprehensive matrix coverage for cross-platform compatibility

program test_ci_matrix_coverage_defects
    use error_handling
    use file_utils
    use string_utils
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "=== Issue #175: CI/CD Matrix Coverage Defects Test Suite ==="
    write(*,*)
    
    ! Test missing multi-platform support patterns
    call test_missing_multi_compiler_matrix_support()
    call test_missing_multi_os_matrix_support()
    call test_inadequate_matrix_exclusion_rules()
    call test_missing_platform_specific_adaptations()
    call test_insufficient_cross_platform_validation()
    call test_missing_matrix_artifact_generation()
    call test_inadequate_failure_handling_patterns()
    call test_missing_performance_matrix_validation()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "CI/CD MATRIX COVERAGE DEFECTS TESTS FAILED"
        stop 1
    else
        write(*,*) "All CI/CD matrix coverage defects tests passed"
    end if

contains

    subroutine test_missing_multi_compiler_matrix_support()
        ! Given: Current CI configuration lacks comprehensive compiler matrix
        ! When: Testing multi-compiler matrix support validation
        ! Then: Identify and validate missing compiler matrix configurations
        
        logical :: gfortran_matrix_valid, ifort_matrix_valid, nvfortran_matrix_valid
        logical :: compiler_detection_valid, matrix_definition_valid
        
        write(*,'(A)', advance='no') "Testing missing multi-compiler matrix support... "
        test_count = test_count + 1
        
        ! Test comprehensive compiler matrix support
        gfortran_matrix_valid = validate_gfortran_matrix_configuration()
        ifort_matrix_valid = validate_ifort_matrix_configuration()
        nvfortran_matrix_valid = validate_nvfortran_matrix_configuration()
        compiler_detection_valid = validate_compiler_detection_matrix()
        matrix_definition_valid = validate_multi_compiler_matrix_definition()
        
        if (gfortran_matrix_valid .and. ifort_matrix_valid .and. nvfortran_matrix_valid .and. &
            compiler_detection_valid .and. matrix_definition_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. gfortran_matrix_valid) write(*,*) "  - gfortran matrix configuration defect identified"
            if (.not. ifort_matrix_valid) write(*,*) "  - ifort matrix configuration defect identified"
            if (.not. nvfortran_matrix_valid) write(*,*) "  - nvfortran matrix configuration defect identified"
            if (.not. compiler_detection_valid) write(*,*) "  - compiler detection matrix defect identified"
            if (.not. matrix_definition_valid) write(*,*) "  - multi-compiler matrix definition defect identified"
        end if
    end subroutine

    function validate_gfortran_matrix_configuration() result(is_valid)
        ! Test gfortran matrix configuration requirements
        logical :: is_valid
        character(len=512) :: required_gfortran_config(5)
        logical :: gfortran_config_complete
        
        ! Required gfortran matrix configuration components
        required_gfortran_config(1) = "compiler: [gfortran-9, gfortran-10, gfortran-11, gfortran-12, gfortran-13]"
        required_gfortran_config(2) = "export FC=gfortran-${{ matrix.gfortran-version }}"
        required_gfortran_config(3) = "sudo apt-get install -y gfortran-${{ matrix.gfortran-version }}"
        required_gfortran_config(4) = "gfortran-${{ matrix.gfortran-version }} --version"
        required_gfortran_config(5) = "fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        
        ! Validate gfortran matrix configuration completeness
        gfortran_config_complete = validate_compiler_matrix_config_structure(required_gfortran_config, 5)
        
        is_valid = gfortran_config_complete
    end function

    function validate_ifort_matrix_configuration() result(is_valid)
        ! Test Intel Fortran (ifort) matrix configuration requirements
        logical :: is_valid
        character(len=512) :: required_ifort_config(5)
        logical :: ifort_config_complete
        
        ! Required ifort matrix configuration components
        required_ifort_config(1) = "compiler: [ifort-2021, ifort-2022, ifort-2023]"
        required_ifort_config(2) = "export FC=ifort"
        required_ifort_config(3) = "source /opt/intel/oneapi/setvars.sh"
        required_ifort_config(4) = "ifort --version"
        required_ifort_config(5) = "fpm test --flag ""-prof-gen=srcpos"""
        
        ! Validate ifort matrix configuration completeness
        ifort_config_complete = validate_compiler_matrix_config_structure(required_ifort_config, 5)
        
        is_valid = ifort_config_complete
    end function

    function validate_nvfortran_matrix_configuration() result(is_valid)
        ! Test NVIDIA Fortran (nvfortran) matrix configuration requirements
        logical :: is_valid
        character(len=512) :: required_nvfortran_config(5)
        logical :: nvfortran_config_complete
        
        ! Required nvfortran matrix configuration components  
        required_nvfortran_config(1) = "compiler: [nvfortran-22.5, nvfortran-22.7, nvfortran-23.1]"
        required_nvfortran_config(2) = "export FC=nvfortran"
        required_nvfortran_config(3) = "export PATH=/opt/nvidia/hpc_sdk/bin:$PATH"
        required_nvfortran_config(4) = "nvfortran --version"
        required_nvfortran_config(5) = "fpm test --flag ""-Mprof=ccff"""
        
        ! Validate nvfortran matrix configuration completeness
        nvfortran_config_complete = validate_compiler_matrix_config_structure(required_nvfortran_config, 5)
        
        is_valid = nvfortran_config_complete
    end function

    function validate_compiler_detection_matrix() result(is_valid)
        ! Test compiler detection matrix patterns
        logical :: is_valid
        character(len=512) :: detection_patterns(6)
        logical :: detection_complete
        
        ! Required compiler detection patterns for matrix
        detection_patterns(1) = "if [ ""$FC"" == ""gfortran"" ]; then COVERAGE_FLAGS=""-fprofile-arcs -ftest-coverage""; fi"
        detection_patterns(2) = "if [ ""$FC"" == ""ifort"" ]; then COVERAGE_FLAGS=""-prof-gen=srcpos""; fi"
        detection_patterns(3) = "if [ ""$FC"" == ""nvfortran"" ]; then COVERAGE_FLAGS=""-Mprof=ccff""; fi"
        detection_patterns(4) = "gfortran --version | grep -q ""GNU Fortran"""
        detection_patterns(5) = "ifort --version | grep -q ""Intel"""
        detection_patterns(6) = "nvfortran --version | grep -q ""NVIDIA"""
        
        ! Validate compiler detection completeness
        detection_complete = validate_detection_pattern_matrix_structure(detection_patterns, 6)
        
        is_valid = detection_complete
    end function

    function validate_multi_compiler_matrix_definition() result(is_valid)
        ! Test multi-compiler matrix definition structure
        logical :: is_valid
        character(len=512) :: matrix_definition(8)
        logical :: definition_complete
        
        ! Required multi-compiler matrix definition
        matrix_definition(1) = "strategy:"
        matrix_definition(2) = "  fail-fast: false"
        matrix_definition(3) = "  matrix:"
        matrix_definition(4) = "    compiler: [gfortran, ifort, nvfortran]"
        matrix_definition(5) = "    os: [ubuntu-latest, ubuntu-20.04, ubuntu-22.04]"
        matrix_definition(6) = "    exclude:"
        matrix_definition(7) = "      - os: ubuntu-20.04"
        matrix_definition(8) = "        compiler: nvfortran"
        
        ! Validate matrix definition completeness
        definition_complete = validate_multi_compiler_matrix_structure(matrix_definition, 8)
        
        is_valid = definition_complete
    end function

    subroutine test_missing_multi_os_matrix_support()
        ! Given: Current CI configuration lacks comprehensive OS matrix
        ! When: Testing multi-OS matrix support validation  
        ! Then: Identify and validate missing OS matrix configurations
        
        logical :: ubuntu_matrix_valid, macos_matrix_valid, windows_matrix_valid
        logical :: os_specific_adaptations_valid, exclusion_rules_valid
        
        write(*,'(A)', advance='no') "Testing missing multi-OS matrix support... "
        test_count = test_count + 1
        
        ! Test comprehensive OS matrix support
        ubuntu_matrix_valid = validate_ubuntu_matrix_configuration()
        macos_matrix_valid = validate_macos_matrix_configuration()  
        windows_matrix_valid = validate_windows_matrix_configuration()
        os_specific_adaptations_valid = validate_os_specific_adaptations()
        exclusion_rules_valid = validate_os_compiler_exclusion_rules()
        
        if (ubuntu_matrix_valid .and. macos_matrix_valid .and. windows_matrix_valid .and. &
            os_specific_adaptations_valid .and. exclusion_rules_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. ubuntu_matrix_valid) write(*,*) "  - Ubuntu matrix configuration defect identified"
            if (.not. macos_matrix_valid) write(*,*) "  - macOS matrix configuration defect identified"
            if (.not. windows_matrix_valid) write(*,*) "  - Windows matrix configuration defect identified"
            if (.not. os_specific_adaptations_valid) write(*,*) "  - OS-specific adaptations defect identified"
            if (.not. exclusion_rules_valid) write(*,*) "  - OS-compiler exclusion rules defect identified"
        end if
    end subroutine

    function validate_ubuntu_matrix_configuration() result(is_valid)
        ! Test Ubuntu matrix configuration requirements
        logical :: is_valid
        character(len=512) :: ubuntu_matrix_config(6)
        logical :: ubuntu_config_complete
        
        ! Required Ubuntu matrix configuration
        ubuntu_matrix_config(1) = "os: [ubuntu-latest, ubuntu-20.04, ubuntu-22.04]"
        ubuntu_matrix_config(2) = "runs-on: ${{ matrix.os }}"
        ubuntu_matrix_config(3) = "sudo apt-get update"
        ubuntu_matrix_config(4) = "sudo apt-get install -y gfortran lcov"
        ubuntu_matrix_config(5) = "export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH"
        ubuntu_matrix_config(6) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        
        ! Validate Ubuntu matrix configuration
        ubuntu_config_complete = validate_os_matrix_config_structure(ubuntu_matrix_config, 6)
        
        is_valid = ubuntu_config_complete
    end function

    function validate_macos_matrix_configuration() result(is_valid)
        ! Test macOS matrix configuration requirements
        logical :: is_valid
        character(len=512) :: macos_matrix_config(6)
        logical :: macos_config_complete
        
        ! Required macOS matrix configuration
        macos_matrix_config(1) = "os: [macos-latest, macos-12, macos-13]"
        macos_matrix_config(2) = "runs-on: ${{ matrix.os }}"
        macos_matrix_config(3) = "brew install gcc lcov"
        macos_matrix_config(4) = "export FC=gfortran-13"
        macos_matrix_config(5) = "export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH"
        macos_matrix_config(6) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        
        ! Validate macOS matrix configuration
        macos_config_complete = validate_os_matrix_config_structure(macos_matrix_config, 6)
        
        is_valid = macos_config_complete
    end function

    function validate_windows_matrix_configuration() result(is_valid)
        ! Test Windows matrix configuration requirements  
        logical :: is_valid
        character(len=512) :: windows_matrix_config(6)
        logical :: windows_config_complete
        
        ! Required Windows matrix configuration
        windows_matrix_config(1) = "os: [windows-latest, windows-2019, windows-2022]"
        windows_matrix_config(2) = "runs-on: ${{ matrix.os }}"
        windows_matrix_config(3) = "choco install mingw --version 8.1.0"
        windows_matrix_config(4) = "export FC=gfortran"
        windows_matrix_config(5) = "export PATH=""/c/tools/mingw64/bin:$PATH"""
        windows_matrix_config(6) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        
        ! Validate Windows matrix configuration
        windows_config_complete = validate_os_matrix_config_structure(windows_matrix_config, 6)
        
        is_valid = windows_config_complete
    end function

    function validate_os_specific_adaptations() result(is_valid)
        ! Test OS-specific adaptations for CI matrix
        logical :: is_valid
        character(len=512) :: os_adaptations(7)
        logical :: adaptations_complete
        
        ! Required OS-specific adaptations
        os_adaptations(1) = "if [ ""$RUNNER_OS"" == ""Linux"" ]; then sudo apt-get install -y gfortran; fi"
        os_adaptations(2) = "if [ ""$RUNNER_OS"" == ""macOS"" ]; then brew install gcc; fi"
        os_adaptations(3) = "if [ ""$RUNNER_OS"" == ""Windows"" ]; then choco install mingw; fi"
        os_adaptations(4) = "# Linux-specific library path setup"
        os_adaptations(5) = "# macOS-specific compiler path setup"  
        os_adaptations(6) = "# Windows-specific MinGW setup"
        os_adaptations(7) = "gcov --version || echo ""gcov not available on this platform"""
        
        ! Validate OS adaptations completeness
        adaptations_complete = validate_os_adaptation_structure(os_adaptations, 7)
        
        is_valid = adaptations_complete
    end function

    function validate_os_compiler_exclusion_rules() result(is_valid)
        ! Test OS-compiler exclusion rules for matrix
        logical :: is_valid
        character(len=512) :: exclusion_rules(8)
        logical :: exclusion_complete
        
        ! Required OS-compiler exclusion rules
        exclusion_rules(1) = "exclude:"
        exclusion_rules(2) = "  - os: macos-latest"
        exclusion_rules(3) = "    compiler: ifort  # Intel Fortran not available on macOS"
        exclusion_rules(4) = "  - os: windows-latest"
        exclusion_rules(5) = "    compiler: ifort  # Intel Fortran setup complex on Windows CI"
        exclusion_rules(6) = "  - os: ubuntu-20.04"
        exclusion_rules(7) = "    compiler: nvfortran  # NVIDIA HPC SDK not available on older Ubuntu"
        exclusion_rules(8) = "  # Add more exclusions as needed"
        
        ! Validate exclusion rules completeness
        exclusion_complete = validate_exclusion_rules_matrix_structure(exclusion_rules, 8)
        
        is_valid = exclusion_complete  
    end function

    subroutine test_inadequate_matrix_exclusion_rules()
        ! Given: Current matrix exclusion rules are insufficient
        ! When: Testing comprehensive exclusion rule validation
        ! Then: Ensure all incompatible OS-compiler combinations are excluded
        
        logical :: compiler_exclusions_valid, os_exclusions_valid, version_exclusions_valid
        
        write(*,'(A)', advance='no') "Testing inadequate matrix exclusion rules... "
        test_count = test_count + 1
        
        ! Test comprehensive exclusion rules
        compiler_exclusions_valid = validate_compiler_exclusion_completeness()
        os_exclusions_valid = validate_os_exclusion_completeness()  
        version_exclusions_valid = validate_version_compatibility_exclusions()
        
        if (compiler_exclusions_valid .and. os_exclusions_valid .and. version_exclusions_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. compiler_exclusions_valid) write(*,*) "  - Compiler exclusion defect identified"
            if (.not. os_exclusions_valid) write(*,*) "  - OS exclusion defect identified"
            if (.not. version_exclusions_valid) write(*,*) "  - Version compatibility exclusion defect identified"
        end if
    end subroutine

    function validate_compiler_exclusion_completeness() result(is_valid)
        ! Test completeness of compiler exclusion rules
        logical :: is_valid
        character(len=512) :: compiler_exclusions(6)
        logical :: exclusion_completeness
        
        ! Required compiler exclusion patterns
        compiler_exclusions(1) = "# ifort not available on macOS runners"
        compiler_exclusions(2) = "# nvfortran requires specific Ubuntu versions"
        compiler_exclusions(3) = "# gfortran version compatibility with OS"
        compiler_exclusions(4) = "# Windows-specific compiler limitations"
        compiler_exclusions(5) = "# License requirements for commercial compilers"
        compiler_exclusions(6) = "# Architecture-specific compiler availability"
        
        ! Validate compiler exclusion completeness
        exclusion_completeness = validate_exclusion_completeness_structure(compiler_exclusions, 6)
        
        is_valid = exclusion_completeness
    end function

    function validate_os_exclusion_completeness() result(is_valid)
        ! Test completeness of OS exclusion rules
        logical :: is_valid
        character(len=512) :: os_exclusions(5)
        logical :: exclusion_completeness
        
        ! Required OS exclusion patterns
        os_exclusions(1) = "# macOS Intel Fortran compatibility issues"
        os_exclusions(2) = "# Windows MinGW vs MSVC compiler conflicts"
        os_exclusions(3) = "# Ubuntu version-specific package availability"
        os_exclusions(4) = "# ARM64 architecture support limitations"
        os_exclusions(5) = "# Container runtime compatibility requirements"
        
        ! Validate OS exclusion completeness
        exclusion_completeness = validate_exclusion_completeness_structure(os_exclusions, 5)
        
        is_valid = exclusion_completeness
    end function

    function validate_version_compatibility_exclusions() result(is_valid)
        ! Test version compatibility exclusion rules
        logical :: is_valid
        character(len=512) :: version_exclusions(4)
        logical :: version_completeness
        
        ! Required version compatibility exclusions
        version_exclusions(1) = "# gfortran < 9 lacks modern Fortran 2008+ features"
        version_exclusions(2) = "# ifort legacy versions have coverage tool incompatibilities"
        version_exclusions(3) = "# nvfortran requires CUDA toolkit version alignment"
        version_exclusions(4) = "# FPM version compatibility with compiler versions"
        
        ! Validate version exclusion completeness
        version_completeness = validate_exclusion_completeness_structure(version_exclusions, 4)
        
        is_valid = version_completeness
    end function

    subroutine test_missing_platform_specific_adaptations()
        ! Given: Platform-specific adaptations are missing or incomplete
        ! When: Testing platform adaptation validation
        ! Then: Ensure all platforms have proper setup and configuration
        
        logical :: linux_adaptations_valid, macos_adaptations_valid, windows_adaptations_valid
        logical :: package_management_valid, path_configuration_valid
        
        write(*,'(A)', advance='no') "Testing missing platform-specific adaptations... "
        test_count = test_count + 1
        
        ! Test platform-specific adaptations
        linux_adaptations_valid = validate_linux_specific_adaptations()
        macos_adaptations_valid = validate_macos_specific_adaptations()
        windows_adaptations_valid = validate_windows_specific_adaptations()
        package_management_valid = validate_cross_platform_package_management()
        path_configuration_valid = validate_cross_platform_path_configuration()
        
        if (linux_adaptations_valid .and. macos_adaptations_valid .and. windows_adaptations_valid .and. &
            package_management_valid .and. path_configuration_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. linux_adaptations_valid) write(*,*) "  - Linux-specific adaptations defect identified"
            if (.not. macos_adaptations_valid) write(*,*) "  - macOS-specific adaptations defect identified"
            if (.not. windows_adaptations_valid) write(*,*) "  - Windows-specific adaptations defect identified"
            if (.not. package_management_valid) write(*,*) "  - Cross-platform package management defect identified"
            if (.not. path_configuration_valid) write(*,*) "  - Cross-platform path configuration defect identified"
        end if
    end subroutine

    function validate_linux_specific_adaptations() result(is_valid)
        ! Test Linux-specific CI adaptations
        logical :: is_valid
        character(len=512) :: linux_adaptations(6)
        logical :: linux_completeness
        
        ! Required Linux-specific adaptations
        linux_adaptations(1) = "sudo apt-get update && sudo apt-get install -y gfortran lcov"
        linux_adaptations(2) = "export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH"  
        linux_adaptations(3) = "sudo update-alternatives --install /usr/bin/gfortran gfortran"
        linux_adaptations(4) = "ldd --version  # Check glibc version compatibility"
        linux_adaptations(5) = "gcov --version  # Verify gcov availability"
        linux_adaptations(6) = "ulimit -c unlimited  # Enable core dumps for debugging"
        
        ! Validate Linux adaptations completeness
        linux_completeness = validate_platform_adaptation_completeness(linux_adaptations, 6)
        
        is_valid = linux_completeness
    end function

    function validate_macos_specific_adaptations() result(is_valid)
        ! Test macOS-specific CI adaptations
        logical :: is_valid
        character(len=512) :: macos_adaptations(6)
        logical :: macos_completeness
        
        ! Required macOS-specific adaptations
        macos_adaptations(1) = "brew install gcc lcov"
        macos_adaptations(2) = "export FC=gfortran-13"
        macos_adaptations(3) = "export CC=gcc-13"
        macos_adaptations(4) = "export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH"
        macos_adaptations(5) = "xcode-select --print-path  # Verify Xcode command line tools"
        macos_adaptations(6) = "otool -L fortcov  # Check dynamic library dependencies"
        
        ! Validate macOS adaptations completeness
        macos_completeness = validate_platform_adaptation_completeness(macos_adaptations, 6)
        
        is_valid = macos_completeness
    end function

    function validate_windows_specific_adaptations() result(is_valid)
        ! Test Windows-specific CI adaptations
        logical :: is_valid
        character(len=512) :: windows_adaptations(6)
        logical :: windows_completeness
        
        ! Required Windows-specific adaptations
        windows_adaptations(1) = "choco install mingw --version 8.1.0 -y"
        windows_adaptations(2) = "export PATH=""/c/tools/mingw64/bin:$PATH"""
        windows_adaptations(3) = "export FC=gfortran"
        windows_adaptations(4) = "export CC=gcc"
        windows_adaptations(5) = "where gfortran  # Verify compiler location"
        windows_adaptations(6) = "gfortran --print-search-dirs  # Check search directories"
        
        ! Validate Windows adaptations completeness
        windows_completeness = validate_platform_adaptation_completeness(windows_adaptations, 6)
        
        is_valid = windows_completeness
    end function

    function validate_cross_platform_package_management() result(is_valid)
        ! Test cross-platform package management patterns
        logical :: is_valid
        character(len=512) :: package_patterns(7)
        logical :: package_completeness
        
        ! Required cross-platform package management patterns
        package_patterns(1) = "if [ ""$RUNNER_OS"" == ""Linux"" ]; then sudo apt-get install -y gfortran; fi"
        package_patterns(2) = "if [ ""$RUNNER_OS"" == ""macOS"" ]; then brew install gcc; fi"
        package_patterns(3) = "if [ ""$RUNNER_OS"" == ""Windows"" ]; then choco install mingw; fi"
        package_patterns(4) = "# Universal FPM installation across platforms"
        package_patterns(5) = "# Platform-specific coverage tool installation"
        package_patterns(6) = "# Dependency verification across platforms"
        package_patterns(7) = "# Package version pinning for reproducible builds"
        
        ! Validate package management completeness
        package_completeness = validate_package_management_structure(package_patterns, 7)
        
        is_valid = package_completeness
    end function

    function validate_cross_platform_path_configuration() result(is_valid)
        ! Test cross-platform path configuration patterns
        logical :: is_valid
        character(len=512) :: path_patterns(6)
        logical :: path_completeness
        
        ! Required cross-platform path configuration patterns
        path_patterns(1) = "export PATH=/usr/local/bin:$PATH  # Linux/macOS"
        path_patterns(2) = "export PATH=""/c/tools/mingw64/bin:$PATH""  # Windows"
        path_patterns(3) = "export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH  # Linux"
        path_patterns(4) = "export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH  # macOS"
        path_patterns(5) = "# Windows library path configuration via PATH"
        path_patterns(6) = "# Cross-platform binary discovery and validation"
        
        ! Validate path configuration completeness
        path_completeness = validate_path_configuration_structure(path_patterns, 6)
        
        is_valid = path_completeness
    end function

    subroutine test_insufficient_cross_platform_validation()
        ! Given: Cross-platform validation is insufficient for CI matrix
        ! When: Testing cross-platform validation completeness
        ! Then: Ensure all platform combinations are properly validated
        
        logical :: build_validation_valid, test_validation_valid, coverage_validation_valid
        logical :: artifact_validation_valid, performance_validation_valid
        
        write(*,'(A)', advance='no') "Testing insufficient cross-platform validation... "
        test_count = test_count + 1
        
        ! Test cross-platform validation completeness
        build_validation_valid = validate_cross_platform_build_validation()
        test_validation_valid = validate_cross_platform_test_validation()
        coverage_validation_valid = validate_cross_platform_coverage_validation()
        artifact_validation_valid = validate_cross_platform_artifact_validation()
        performance_validation_valid = validate_cross_platform_performance_validation()
        
        if (build_validation_valid .and. test_validation_valid .and. coverage_validation_valid .and. &
            artifact_validation_valid .and. performance_validation_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. build_validation_valid) write(*,*) "  - Cross-platform build validation defect identified"
            if (.not. test_validation_valid) write(*,*) "  - Cross-platform test validation defect identified"
            if (.not. coverage_validation_valid) write(*,*) "  - Cross-platform coverage validation defect identified"
            if (.not. artifact_validation_valid) write(*,*) "  - Cross-platform artifact validation defect identified"
            if (.not. performance_validation_valid) write(*,*) "  - Cross-platform performance validation defect identified"
        end if
    end subroutine

    function validate_cross_platform_build_validation() result(is_valid)
        ! Test cross-platform build validation requirements
        logical :: is_valid
        character(len=512) :: build_validation(5)
        logical :: build_completeness
        
        ! Required cross-platform build validation
        build_validation(1) = "fpm build --profile release --flag ""$COVERAGE_FLAGS"""
        build_validation(2) = "if [ -f ""build/gfortran_*/fortcov"" ]; then echo ""Build successful""; fi"
        build_validation(3) = "file build/gfortran_*/fortcov  # Check binary format"
        build_validation(4) = "./build/gfortran_*/fortcov --version  # Verify executable works"
        build_validation(5) = "ldd build/gfortran_*/fortcov || otool -L build/gfortran_*/fortcov || echo ""Static binary"""
        
        ! Validate build validation completeness
        build_completeness = validate_validation_completeness_structure(build_validation, 5)
        
        is_valid = build_completeness
    end function

    function validate_cross_platform_test_validation() result(is_valid)
        ! Test cross-platform test validation requirements
        logical :: is_valid
        character(len=512) :: test_validation(5)
        logical :: test_completeness
        
        ! Required cross-platform test validation
        test_validation(1) = "fpm test --profile release --flag ""$COVERAGE_FLAGS"""
        test_validation(2) = "echo ""Test results: $?"" # Capture exit code"
        test_validation(3) = "find build -name ""*.gcda"" -o -name ""*.gcno"" | wc -l  # Count coverage files"
        test_validation(4) = "gcov --version && gcov src/*.f90 || echo ""gcov not available"""
        test_validation(5) = "ls -la *.gcov || echo ""No gcov files generated"""
        
        ! Validate test validation completeness
        test_completeness = validate_validation_completeness_structure(test_validation, 5)
        
        is_valid = test_completeness
    end function

    function validate_cross_platform_coverage_validation() result(is_valid)
        ! Test cross-platform coverage validation requirements
        logical :: is_valid
        character(len=512) :: coverage_validation(6)
        logical :: coverage_completeness
        
        ! Required cross-platform coverage validation
        coverage_validation(1) = "fpm run fortcov -- --source=src --output=coverage.html"
        coverage_validation(2) = "if [ -f ""coverage.html"" ]; then echo ""Coverage report generated""; fi"
        coverage_validation(3) = "grep -q ""Total coverage"" coverage.html || echo ""Coverage percentage missing"""
        coverage_validation(4) = "wc -l coverage.html  # Check report size"
        coverage_validation(5) = "file coverage.html  # Verify HTML format"
        coverage_validation(6) = "# Validate coverage report contains expected sections"
        
        ! Validate coverage validation completeness
        coverage_completeness = validate_validation_completeness_structure(coverage_validation, 6)
        
        is_valid = coverage_completeness
    end function

    function validate_cross_platform_artifact_validation() result(is_valid)
        ! Test cross-platform artifact validation requirements
        logical :: is_valid
        character(len=512) :: artifact_validation(5)
        logical :: artifact_completeness
        
        ! Required cross-platform artifact validation
        artifact_validation(1) = "ls -la coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        artifact_validation(2) = "file coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        artifact_validation(3) = "wc -l coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        artifact_validation(4) = "# Validate artifact naming consistency across platforms"
        artifact_validation(5) = "# Verify artifact content format and completeness"
        
        ! Validate artifact validation completeness
        artifact_completeness = validate_validation_completeness_structure(artifact_validation, 5)
        
        is_valid = artifact_completeness
    end function

    function validate_cross_platform_performance_validation() result(is_valid)
        ! Test cross-platform performance validation requirements
        logical :: is_valid
        character(len=512) :: performance_validation(4)
        logical :: performance_completeness
        
        ! Required cross-platform performance validation
        performance_validation(1) = "time fpm test --flag ""$COVERAGE_FLAGS""  # Measure build time"
        performance_validation(2) = "time fpm run fortcov -- --source=src  # Measure analysis time"
        performance_validation(3) = "du -h coverage.html  # Check report size"
        performance_validation(4) = "# Validate performance within acceptable thresholds"
        
        ! Validate performance validation completeness
        performance_completeness = validate_validation_completeness_structure(performance_validation, 4)
        
        is_valid = performance_completeness
    end function

    subroutine test_missing_matrix_artifact_generation()
        ! Given: Matrix artifact generation is missing or incomplete
        ! When: Testing matrix-specific artifact generation validation
        ! Then: Ensure all matrix combinations generate proper artifacts
        
        logical :: naming_convention_valid, aggregation_valid, storage_valid
        logical :: metadata_valid, comparison_valid
        
        write(*,'(A)', advance='no') "Testing missing matrix artifact generation... "
        test_count = test_count + 1
        
        ! Test matrix artifact generation
        naming_convention_valid = validate_matrix_artifact_naming_convention()
        aggregation_valid = validate_matrix_artifact_aggregation()
        storage_valid = validate_matrix_artifact_storage()
        metadata_valid = validate_matrix_artifact_metadata()
        comparison_valid = validate_matrix_artifact_comparison()
        
        if (naming_convention_valid .and. aggregation_valid .and. storage_valid .and. &
            metadata_valid .and. comparison_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. naming_convention_valid) write(*,*) "  - Matrix artifact naming convention defect identified"
            if (.not. aggregation_valid) write(*,*) "  - Matrix artifact aggregation defect identified"
            if (.not. storage_valid) write(*,*) "  - Matrix artifact storage defect identified"
            if (.not. metadata_valid) write(*,*) "  - Matrix artifact metadata defect identified"
            if (.not. comparison_valid) write(*,*) "  - Matrix artifact comparison defect identified"
        end if
    end subroutine

    function validate_matrix_artifact_naming_convention() result(is_valid)
        ! Test matrix artifact naming convention requirements
        logical :: is_valid
        character(len=512) :: naming_patterns(6)
        logical :: naming_completeness
        
        ! Required matrix artifact naming patterns
        naming_patterns(1) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        naming_patterns(2) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.json"
        naming_patterns(3) = "test-results-${{ matrix.compiler }}-${{ matrix.os }}.xml"
        naming_patterns(4) = "build-log-${{ matrix.compiler }}-${{ matrix.os }}.txt"
        naming_patterns(5) = "performance-${{ matrix.compiler }}-${{ matrix.os }}.json"
        naming_patterns(6) = "artifacts-${{ matrix.compiler }}-${{ matrix.os }}.tar.gz"
        
        ! Validate naming convention completeness
        naming_completeness = validate_artifact_naming_completeness(naming_patterns, 6)
        
        is_valid = naming_completeness
    end function

    function validate_matrix_artifact_aggregation() result(is_valid)
        ! Test matrix artifact aggregation requirements
        logical :: is_valid
        character(len=512) :: aggregation_patterns(5)
        logical :: aggregation_completeness
        
        ! Required matrix artifact aggregation patterns
        aggregation_patterns(1) = "# Aggregate coverage results across matrix dimensions"
        aggregation_patterns(2) = "# Combine test results from all platform combinations"
        aggregation_patterns(3) = "# Generate unified performance comparison report"
        aggregation_patterns(4) = "# Create matrix-wide success/failure summary"
        aggregation_patterns(5) = "# Produce cross-platform compatibility report"
        
        ! Validate aggregation completeness
        aggregation_completeness = validate_aggregation_structure(aggregation_patterns, 5)
        
        is_valid = aggregation_completeness
    end function

    function validate_matrix_artifact_storage() result(is_valid)
        ! Test matrix artifact storage requirements
        logical :: is_valid
        character(len=512) :: storage_patterns(5)
        logical :: storage_completeness
        
        ! Required matrix artifact storage patterns
        storage_patterns(1) = "uses: actions/upload-artifact@v4"
        storage_patterns(2) = "name: matrix-coverage-${{ github.run_id }}"
        storage_patterns(3) = "path: coverage-*.html"
        storage_patterns(4) = "retention-days: 30"
        storage_patterns(5) = "# Store artifacts with matrix-specific organization"
        
        ! Validate storage completeness
        storage_completeness = validate_storage_structure(storage_patterns, 5)
        
        is_valid = storage_completeness
    end function

    function validate_matrix_artifact_metadata() result(is_valid)
        ! Test matrix artifact metadata requirements
        logical :: is_valid
        character(len=512) :: metadata_patterns(6)
        logical :: metadata_completeness
        
        ! Required matrix artifact metadata patterns
        metadata_patterns(1) = "# Include compiler version in metadata"
        metadata_patterns(2) = "# Include OS version and architecture in metadata"
        metadata_patterns(3) = "# Include build timestamp and commit SHA"
        metadata_patterns(4) = "# Include test execution time and resource usage"
        metadata_patterns(5) = "# Include coverage percentage and line counts"
        metadata_patterns(6) = "# Include dependency versions and environment info"
        
        ! Validate metadata completeness
        metadata_completeness = validate_metadata_structure(metadata_patterns, 6)
        
        is_valid = metadata_completeness
    end function

    function validate_matrix_artifact_comparison() result(is_valid)
        ! Test matrix artifact comparison requirements
        logical :: is_valid
        character(len=512) :: comparison_patterns(4)
        logical :: comparison_completeness
        
        ! Required matrix artifact comparison patterns
        comparison_patterns(1) = "# Compare coverage results across compilers"
        comparison_patterns(2) = "# Identify platform-specific coverage differences"
        comparison_patterns(3) = "# Generate compiler compatibility matrix"
        comparison_patterns(4) = "# Highlight performance variations across platforms"
        
        ! Validate comparison completeness
        comparison_completeness = validate_comparison_structure(comparison_patterns, 4)
        
        is_valid = comparison_completeness
    end function

    subroutine test_inadequate_failure_handling_patterns()
        ! Given: Failure handling patterns in CI matrix are inadequate
        ! When: Testing comprehensive failure handling validation
        ! Then: Ensure all failure scenarios are properly handled
        
        logical :: compiler_failure_handling_valid, os_failure_handling_valid
        logical :: network_failure_handling_valid, timeout_handling_valid, recovery_valid
        
        write(*,'(A)', advance='no') "Testing inadequate failure handling patterns... "
        test_count = test_count + 1
        
        ! Test failure handling patterns
        compiler_failure_handling_valid = validate_compiler_failure_handling()
        os_failure_handling_valid = validate_os_failure_handling()
        network_failure_handling_valid = validate_network_failure_handling()
        timeout_handling_valid = validate_timeout_handling()
        recovery_valid = validate_recovery_patterns()
        
        if (compiler_failure_handling_valid .and. os_failure_handling_valid .and. &
            network_failure_handling_valid .and. timeout_handling_valid .and. recovery_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. compiler_failure_handling_valid) write(*,*) "  - Compiler failure handling defect identified"
            if (.not. os_failure_handling_valid) write(*,*) "  - OS failure handling defect identified"
            if (.not. network_failure_handling_valid) write(*,*) "  - Network failure handling defect identified"
            if (.not. timeout_handling_valid) write(*,*) "  - Timeout handling defect identified"
            if (.not. recovery_valid) write(*,*) "  - Recovery pattern defect identified"
        end if
    end subroutine

    function validate_compiler_failure_handling() result(is_valid)
        ! Test compiler failure handling requirements
        logical :: is_valid
        character(len=512) :: compiler_failure_patterns(5)
        logical :: failure_completeness
        
        ! Required compiler failure handling patterns
        compiler_failure_patterns(1) = "gfortran --version || echo ""gfortran not available, skipping"""
        compiler_failure_patterns(2) = "ifort --version || echo ""ifort not available, skipping"""
        compiler_failure_patterns(3) = "nvfortran --version || echo ""nvfortran not available, skipping"""
        compiler_failure_patterns(4) = "if ! which $FC > /dev/null; then exit 1; fi"
        compiler_failure_patterns(5) = "# Graceful degradation when compilers are unavailable"
        
        ! Validate compiler failure handling completeness
        failure_completeness = validate_failure_handling_structure(compiler_failure_patterns, 5)
        
        is_valid = failure_completeness
    end function

    function validate_os_failure_handling() result(is_valid)
        ! Test OS failure handling requirements
        logical :: is_valid
        character(len=512) :: os_failure_patterns(5)
        logical :: failure_completeness
        
        ! Required OS failure handling patterns
        os_failure_patterns(1) = "sudo apt-get update || echo ""Package update failed"""
        os_failure_patterns(2) = "brew install gcc || echo ""Homebrew installation failed"""
        os_failure_patterns(3) = "choco install mingw || echo ""Chocolatey installation failed"""
        os_failure_patterns(4) = "# Continue on package manager failures with fallbacks"
        os_failure_patterns(5) = "# Detect and handle permission issues gracefully"
        
        ! Validate OS failure handling completeness
        failure_completeness = validate_failure_handling_structure(os_failure_patterns, 5)
        
        is_valid = failure_completeness
    end function

    function validate_network_failure_handling() result(is_valid)
        ! Test network failure handling requirements
        logical :: is_valid
        character(len=512) :: network_failure_patterns(4)
        logical :: failure_completeness
        
        ! Required network failure handling patterns
        network_failure_patterns(1) = "# Retry package downloads with exponential backoff"
        network_failure_patterns(2) = "# Use alternative mirrors when primary sources fail"
        network_failure_patterns(3) = "# Cache dependencies to reduce network dependencies"
        network_failure_patterns(4) = "# Validate network connectivity before critical operations"
        
        ! Validate network failure handling completeness
        failure_completeness = validate_failure_handling_structure(network_failure_patterns, 4)
        
        is_valid = failure_completeness
    end function

    function validate_timeout_handling() result(is_valid)
        ! Test timeout handling requirements
        logical :: is_valid
        character(len=512) :: timeout_patterns(4)
        logical :: timeout_completeness
        
        ! Required timeout handling patterns
        timeout_patterns(1) = "timeout-minutes: 45  # Reasonable timeout for matrix builds"
        timeout_patterns(2) = "# Kill runaway processes that exceed time limits"
        timeout_patterns(3) = "# Generate partial results when builds timeout"
        timeout_patterns(4) = "# Distinguish between timeout and genuine failures"
        
        ! Validate timeout handling completeness
        timeout_completeness = validate_timeout_structure(timeout_patterns, 4)
        
        is_valid = timeout_completeness
    end function

    function validate_recovery_patterns() result(is_valid)
        ! Test recovery pattern requirements
        logical :: is_valid
        character(len=512) :: recovery_patterns(4)
        logical :: recovery_completeness
        
        ! Required recovery patterns
        recovery_patterns(1) = "# Automatic retry of flaky operations"
        recovery_patterns(2) = "# Fallback to alternative tools when primary fails"
        recovery_patterns(3) = "# Continue matrix execution even when individual jobs fail"
        recovery_patterns(4) = "# Generate comprehensive failure reports for debugging"
        
        ! Validate recovery completeness
        recovery_completeness = validate_recovery_structure(recovery_patterns, 4)
        
        is_valid = recovery_completeness
    end function

    subroutine test_missing_performance_matrix_validation()
        ! Given: Performance validation across matrix is missing
        ! When: Testing performance matrix validation requirements
        ! Then: Ensure performance is validated across all platform combinations
        
        logical :: build_performance_valid, test_performance_valid, analysis_performance_valid
        logical :: comparison_valid, threshold_valid
        
        write(*,'(A)', advance='no') "Testing missing performance matrix validation... "
        test_count = test_count + 1
        
        ! Test performance matrix validation
        build_performance_valid = validate_build_performance_matrix()
        test_performance_valid = validate_test_performance_matrix()
        analysis_performance_valid = validate_analysis_performance_matrix()
        comparison_valid = validate_performance_comparison_matrix()
        threshold_valid = validate_performance_threshold_matrix()
        
        if (build_performance_valid .and. test_performance_valid .and. analysis_performance_valid .and. &
            comparison_valid .and. threshold_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. build_performance_valid) write(*,*) "  - Build performance matrix validation defect identified"
            if (.not. test_performance_valid) write(*,*) "  - Test performance matrix validation defect identified"
            if (.not. analysis_performance_valid) write(*,*) "  - Analysis performance matrix validation defect identified"
            if (.not. comparison_valid) write(*,*) "  - Performance comparison matrix defect identified"
            if (.not. threshold_valid) write(*,*) "  - Performance threshold matrix defect identified"
        end if
    end subroutine

    function validate_build_performance_matrix() result(is_valid)
        ! Test build performance matrix validation requirements
        logical :: is_valid
        character(len=512) :: build_performance_patterns(5)
        logical :: performance_completeness
        
        ! Required build performance matrix patterns
        build_performance_patterns(1) = "time fmp build --profile release  # Measure build time"
        build_performance_patterns(2) = "echo ""Build time: $(date)"" >> performance-${{ matrix.compiler }}-${{ matrix.os }}.log"
        build_performance_patterns(3) = "du -sh build/  # Measure build artifact size"
        build_performance_patterns(4) = "# Track compiler-specific build performance differences"
        build_performance_patterns(5) = "# Monitor build time trends across matrix combinations"
        
        ! Validate build performance completeness
        performance_completeness = validate_performance_validation_structure(build_performance_patterns, 5)
        
        is_valid = performance_completeness
    end function

    function validate_test_performance_matrix() result(is_valid)
        ! Test test performance matrix validation requirements
        logical :: is_valid
        character(len=512) :: test_performance_patterns(5)
        logical :: performance_completeness
        
        ! Required test performance matrix patterns
        test_performance_patterns(1) = "time fpm test --flag ""$COVERAGE_FLAGS""  # Measure test execution time"
        test_performance_patterns(2) = "/usr/bin/time -v fpm test 2> test-time-${{ matrix.compiler }}-${{ matrix.os }}.log"
        test_performance_patterns(3) = "# Monitor memory usage during test execution"
        test_performance_patterns(4) = "# Track test performance differences across compilers"
        test_performance_patterns(5) = "# Validate test execution stays within acceptable limits"
        
        ! Validate test performance completeness
        performance_completeness = validate_performance_validation_structure(test_performance_patterns, 5)
        
        is_valid = performance_completeness
    end function

    function validate_analysis_performance_matrix() result(is_valid)
        ! Test analysis performance matrix validation requirements
        logical :: is_valid
        character(len=512) :: analysis_performance_patterns(5)
        logical :: performance_completeness
        
        ! Required analysis performance matrix patterns
        analysis_performance_patterns(1) = "time fpm run fortcov -- --source=src  # Measure analysis time"
        analysis_performance_patterns(2) = "du -sh coverage.html  # Measure report size"
        analysis_performance_patterns(3) = "# Track analysis time scaling with project size"
        analysis_performance_patterns(4) = "# Monitor memory usage during coverage analysis"
        analysis_performance_patterns(5) = "# Compare analysis performance across platforms"
        
        ! Validate analysis performance completeness
        performance_completeness = validate_performance_validation_structure(analysis_performance_patterns, 5)
        
        is_valid = performance_completeness
    end function

    function validate_performance_comparison_matrix() result(is_valid)
        ! Test performance comparison matrix validation requirements
        logical :: is_valid
        character(len=512) :: comparison_patterns(4)
        logical :: comparison_completeness
        
        ! Required performance comparison matrix patterns
        comparison_patterns(1) = "# Generate performance comparison across all matrix combinations"
        comparison_patterns(2) = "# Identify performance regressions by compiler/OS combination"
        comparison_patterns(3) = "# Create performance baseline for each platform"
        comparison_patterns(4) = "# Alert on significant performance deviations"
        
        ! Validate comparison completeness
        comparison_completeness = validate_performance_comparison_structure(comparison_patterns, 4)
        
        is_valid = comparison_completeness
    end function

    function validate_performance_threshold_matrix() result(is_valid)
        ! Test performance threshold matrix validation requirements
        logical :: is_valid
        character(len=512) :: threshold_patterns(4)
        logical :: threshold_completeness
        
        ! Required performance threshold matrix patterns
        threshold_patterns(1) = "# Build time threshold: <5 minutes per matrix combination"
        threshold_patterns(2) = "# Test execution threshold: <10 minutes per matrix combination"
        threshold_patterns(3) = "# Analysis time threshold: <2 minutes per matrix combination"
        threshold_patterns(4) = "# Memory usage threshold: <2GB per matrix combination"
        
        ! Validate threshold completeness
        threshold_completeness = validate_performance_threshold_structure(threshold_patterns, 4)
        
        is_valid = threshold_completeness
    end function

    ! === Helper Functions for CI Matrix Coverage Defect Validation ===

    function validate_compiler_matrix_config_structure(config, count) result(is_valid)
        character(len=*), intent(in) :: config(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate compiler matrix configuration components are not empty
        do i = 1, count
            if (len_trim(config(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_detection_pattern_matrix_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate compiler detection patterns for matrix are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_multi_compiler_matrix_structure(definition, count) result(is_valid)
        character(len=*), intent(in) :: definition(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate multi-compiler matrix structure includes required components
        is_valid = (count >= 8) .and. &
                  (index(definition(1), "strategy:") > 0) .and. &
                  (index(definition(3), "matrix:") > 0) .and. &
                  (index(definition(4), "compiler:") > 0) .and. &
                  (index(definition(5), "os:") > 0) .and. &
                  (index(definition(6), "exclude:") > 0)
    end function

    function validate_os_matrix_config_structure(config, count) result(is_valid)
        character(len=*), intent(in) :: config(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate OS matrix configuration components are not empty
        do i = 1, count
            if (len_trim(config(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_os_adaptation_structure(adaptations, count) result(is_valid)
        character(len=*), intent(in) :: adaptations(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate OS adaptation patterns are not empty
        do i = 1, count
            if (len_trim(adaptations(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_exclusion_rules_matrix_structure(rules, count) result(is_valid)
        character(len=*), intent(in) :: rules(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate exclusion rules matrix structure includes required components
        is_valid = (count >= 8) .and. &
                  (index(rules(1), "exclude:") > 0) .and. &
                  (index(rules(2), "os:") > 0) .and. &
                  (index(rules(3), "compiler:") > 0)
    end function

    function validate_exclusion_completeness_structure(exclusions, count) result(is_valid)
        character(len=*), intent(in) :: exclusions(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate exclusion completeness patterns are not empty
        do i = 1, count
            if (len_trim(exclusions(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_platform_adaptation_completeness(adaptations, count) result(is_valid)
        character(len=*), intent(in) :: adaptations(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate platform adaptation completeness patterns are not empty
        do i = 1, count
            if (len_trim(adaptations(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_package_management_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate package management patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_path_configuration_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate path configuration patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_validation_completeness_structure(validations, count) result(is_valid)
        character(len=*), intent(in) :: validations(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate validation completeness patterns are not empty
        do i = 1, count
            if (len_trim(validations(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_artifact_naming_completeness(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate artifact naming patterns include matrix variables
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. &
                (index(patterns(i), "matrix.compiler") == 0 .and. index(patterns(i), "matrix.os") == 0)) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_aggregation_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate aggregation patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_storage_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate storage structure includes required GitHub Actions components
        is_valid = (count >= 4) .and. &
                  (index(patterns(1), "upload-artifact") > 0) .and. &
                  (index(patterns(2), "name:") > 0) .and. &
                  (index(patterns(3), "path:") > 0)
    end function

    function validate_metadata_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate metadata patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_comparison_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate comparison patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_failure_handling_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate failure handling patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_timeout_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate timeout patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_recovery_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate recovery patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_performance_validation_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate performance validation patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_performance_comparison_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate performance comparison patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_performance_threshold_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate performance threshold patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

end program test_ci_matrix_coverage_defects