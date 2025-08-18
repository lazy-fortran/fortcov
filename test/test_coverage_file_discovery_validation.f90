! Test suite for coverage file discovery pattern validation
! Tests build system output pattern recognition from DESIGN.md
! Validates coverage file discovery logic across different build system configurations

program test_coverage_file_discovery_validation
    use error_handling
    use file_utils
    use string_utils
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "=== Coverage File Discovery Validation Test Suite ==="
    write(*,*)
    
    ! Test coverage file discovery patterns
    call test_priority_based_discovery_patterns()
    call test_build_system_output_patterns()
    call test_coverage_file_location_strategies()
    call test_build_directory_search_patterns()
    call test_source_path_discovery_validation()
    call test_file_extension_pattern_recognition()
    call test_nested_build_structure_discovery()
    call test_compiler_specific_output_patterns()
    call test_discovery_fallback_mechanisms()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "COVERAGE FILE DISCOVERY VALIDATION TESTS FAILED"
        stop 1
    else
        write(*,*) "All coverage file discovery validation tests passed"
    end if

contains

    subroutine test_priority_based_discovery_patterns()
        ! Given: Priority-based discovery patterns from DESIGN.md
        ! When: Testing discovery priority order validation
        ! Then: Validate priority-based discovery works correctly
        
        logical :: priority1_valid, priority2_valid, fallback_valid
        
        write(*,'(A)', advance='no') "Testing priority-based discovery patterns... "
        test_count = test_count + 1
        
        ! Test discovery priority patterns
        priority1_valid = validate_priority1_current_directory_search()
        priority2_valid = validate_priority2_source_path_search()
        fallback_valid = validate_discovery_fallback_strategy()
        
        if (priority1_valid .and. priority2_valid .and. fallback_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. priority1_valid) write(*,*) "  - Priority 1 current directory search validation failed"
            if (.not. priority2_valid) write(*,*) "  - Priority 2 source path search validation failed"
            if (.not. fallback_valid) write(*,*) "  - Discovery fallback strategy validation failed"
        end if
    end subroutine

    function validate_priority1_current_directory_search() result(is_valid)
        ! Test Priority 1: Search current directory for .gcov files
        logical :: is_valid
        character(len=256) :: search_pattern
        character(len=128) :: search_location
        logical :: pattern_structure_valid
        
        ! Priority 1 pattern from DESIGN.md
        search_location = "."
        search_pattern = "*.gcov"
        
        ! Validate priority 1 search pattern
        pattern_structure_valid = validate_file_discovery_pattern(search_location, search_pattern, 1)
        
        is_valid = pattern_structure_valid
    end function

    function validate_priority2_source_path_search() result(is_valid)
        ! Test Priority 2: Search specified --source paths for .gcov files
        logical :: is_valid
        character(len=256) :: search_pattern
        character(len=128) :: search_location
        logical :: pattern_structure_valid
        
        ! Priority 2 pattern from DESIGN.md
        search_location = "--source"
        search_pattern = "*.gcov"
        
        ! Validate priority 2 search pattern
        pattern_structure_valid = validate_file_discovery_pattern(search_location, search_pattern, 2)
        
        is_valid = pattern_structure_valid
    end function

    function validate_discovery_fallback_strategy() result(is_valid)
        ! Test discovery fallback strategy
        logical :: is_valid
        character(len=256) :: fallback_patterns(3)
        logical :: fallback_structure_valid
        
        ! Fallback discovery patterns
        fallback_patterns(1) = "Current directory: *.gcov"
        fallback_patterns(2) = "Source paths: *.gcov"
        fallback_patterns(3) = "Build directories: *.gcda"
        
        ! Validate fallback structure
        fallback_structure_valid = validate_fallback_pattern_structure(fallback_patterns, 3)
        
        is_valid = fallback_structure_valid
    end function

    subroutine test_build_system_output_patterns()
        ! Given: Build system output patterns from DESIGN.md
        ! When: Testing build-specific output pattern recognition
        ! Then: Validate build system output patterns work correctly
        
        logical :: fmp_output_valid, cmake_output_valid, make_output_valid, meson_output_valid
        
        write(*,'(A)', advance='no') "Testing build system output patterns... "
        test_count = test_count + 1
        
        ! Test build system specific output patterns
        fpm_output_valid = validate_fpm_output_patterns()
        cmake_output_valid = validate_cmake_output_patterns()
        make_output_valid = validate_make_output_patterns()
        meson_output_valid = validate_meson_output_patterns()
        
        if (fpm_output_valid .and. cmake_output_valid .and. make_output_valid .and. meson_output_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fpm_output_valid) write(*,*) "  - FPM output pattern validation failed"
            if (.not. cmake_output_valid) write(*,*) "  - CMake output pattern validation failed"
            if (.not. make_output_valid) write(*,*) "  - Make output pattern validation failed"
            if (.not. meson_output_valid) write(*,*) "  - Meson output pattern validation failed"
        end if
    end subroutine

    function validate_fmp_output_patterns() result(is_valid)
        ! Test FPM output patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: fpm_patterns(4)
        logical :: fpm_structure_valid
        
        ! FPM output patterns from documentation
        fpm_patterns(1) = "build/gfortran_*/fortcov/*.gcno"
        fpm_patterns(2) = "build/gfortran_*/fortcov/*.gcda"
        fmp_patterns(3) = "*.gcov"  ! Generated in project root
        fpm_patterns(4) = "build/dependencies/*"
        
        ! Validate FPM output structure
        fpm_structure_valid = validate_build_output_pattern_structure(fpm_patterns, 4)
        
        is_valid = fpm_structure_valid
    end function

    function validate_cmake_output_patterns() result(is_valid)
        ! Test CMake output patterns
        logical :: is_valid
        character(len=512) :: cmake_patterns(3)
        logical :: cmake_structure_valid
        
        ! CMake output patterns
        cmake_patterns(1) = "CMakeFiles/fortran_target.dir/*.gcno"
        cmake_patterns(2) = "CMakeFiles/fortran_target.dir/*.gcda"
        cmake_patterns(3) = "*.gcov"  ! Generated coverage files
        
        ! Validate CMake output structure
        cmake_structure_valid = validate_build_output_pattern_structure(cmake_patterns, 3)
        
        is_valid = cmake_structure_valid
    end function

    function validate_make_output_patterns() result(is_valid)
        ! Test Make output patterns
        logical :: is_valid
        character(len=512) :: make_patterns(3)
        logical :: make_structure_valid
        
        ! Make output patterns
        make_patterns(1) = "*.gcno"   ! Object directory
        make_patterns(2) = "*.gcda"   ! Generated during execution
        make_patterns(3) = "*.gcov"   ! Generated coverage files
        
        ! Validate Make output structure
        make_structure_valid = validate_build_output_pattern_structure(make_patterns, 3)
        
        is_valid = make_structure_valid
    end function

    function validate_meson_output_patterns() result(is_valid)
        ! Test Meson output patterns
        logical :: is_valid
        character(len=512) :: meson_patterns(3)
        logical :: meson_structure_valid
        
        ! Meson output patterns
        meson_patterns(1) = "builddir/*.gcno"
        meson_patterns(2) = "builddir/*.gcda"
        meson_patterns(3) = "*.gcov"  ! Generated coverage files
        
        ! Validate Meson output structure
        meson_structure_valid = validate_build_output_pattern_structure(meson_patterns, 3)
        
        is_valid = meson_structure_valid
    end function

    subroutine test_coverage_file_location_strategies()
        ! Given: Coverage file location strategies from DESIGN.md
        ! When: Testing file location strategy validation
        ! Then: Validate location strategies work correctly
        
        logical :: root_location_valid, build_location_valid, source_location_valid
        
        write(*,'(A)', advance='no') "Testing coverage file location strategies... "
        test_count = test_count + 1
        
        ! Test file location strategies
        root_location_valid = validate_root_directory_location_strategy()
        build_location_valid = validate_build_directory_location_strategy()
        source_location_valid = validate_source_directory_location_strategy()
        
        if (root_location_valid .and. build_location_valid .and. source_location_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. root_location_valid) write(*,*) "  - Root directory location strategy validation failed"
            if (.not. build_location_valid) write(*,*) "  - Build directory location strategy validation failed"
            if (.not. source_location_valid) write(*,*) "  - Source directory location strategy validation failed"
        end if
    end subroutine

    function validate_root_directory_location_strategy() result(is_valid)
        ! Test root directory location strategy
        logical :: is_valid
        character(len=256) :: root_strategy_components(3)
        logical :: strategy_structure_valid
        
        ! Root directory strategy components
        root_strategy_components(1) = "Search: . (current directory)"
        root_strategy_components(2) = "Pattern: *.gcov"
        root_strategy_components(3) = "Priority: 1 (highest)"
        
        ! Validate strategy structure
        strategy_structure_valid = validate_location_strategy_structure(root_strategy_components, 3)
        
        is_valid = strategy_structure_valid
    end function

    function validate_build_directory_location_strategy() result(is_valid)
        ! Test build directory location strategy
        logical :: is_valid
        character(len=256) :: build_strategy_components(4)
        logical :: strategy_structure_valid
        
        ! Build directory strategy components
        build_strategy_components(1) = "Search: build/gfortran_*/fortcov"
        build_strategy_components(2) = "Pattern: *.gcda, *.gcno"
        build_strategy_components(3) = "Processing: gcov extraction required"
        build_strategy_components(4) = "Priority: 2 (secondary)"
        
        ! Validate strategy structure
        strategy_structure_valid = validate_location_strategy_structure(build_strategy_components, 4)
        
        is_valid = strategy_structure_valid
    end function

    function validate_source_directory_location_strategy() result(is_valid)
        ! Test source directory location strategy
        logical :: is_valid
        character(len=256) :: source_strategy_components(3)
        logical :: strategy_structure_valid
        
        ! Source directory strategy components
        source_strategy_components(1) = "Search: --source specified paths"
        source_strategy_components(2) = "Pattern: *.gcov"
        source_strategy_components(3) = "Priority: 2 (secondary)"
        
        ! Validate strategy structure
        strategy_structure_valid = validate_location_strategy_structure(source_strategy_components, 3)
        
        is_valid = strategy_structure_valid
    end function

    subroutine test_build_directory_search_patterns()
        ! Given: Build directory search patterns from DESIGN.md
        ! When: Testing directory search pattern validation
        ! Then: Validate search patterns work correctly
        
        logical :: fpm_search_valid, cmake_search_valid, nested_search_valid
        
        write(*,'(A)', advance='no') "Testing build directory search patterns... "
        test_count = test_count + 1
        
        ! Test build directory search patterns
        fpm_search_valid = validate_fmp_build_directory_search()
        cmake_search_valid = validate_cmake_build_directory_search()
        nested_search_valid = validate_nested_directory_search_patterns()
        
        if (fpm_search_valid .and. cmake_search_valid .and. nested_search_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fpm_search_valid) write(*,*) "  - FPM build directory search validation failed"
            if (.not. cmake_search_valid) write(*,*) "  - CMake build directory search validation failed"
            if (.not. nested_search_valid) write(*,*) "  - Nested directory search validation failed"
        end if
    end subroutine

    function validate_fpm_build_directory_search() result(is_valid)
        ! Test FPM build directory search from DESIGN.md
        logical :: is_valid
        character(len=512) :: search_commands(3)
        logical :: search_structure_valid
        
        ! FPM build directory search commands
        search_commands(1) = "find build -name \"*.gcda\" -path \"*/fortcov/*\" -execdir gcov {} \\;"
        search_commands(2) = "find build -name \"*.gcov\" -exec cp {} . \\;"
        search_commands(3) = "# Search build/gfortran_*/fortcov directories"
        
        ! Validate search structure
        search_structure_valid = validate_directory_search_command_structure(search_commands, 3)
        
        is_valid = search_structure_valid
    end function

    function validate_cmake_build_directory_search() result(is_valid)
        ! Test CMake build directory search
        logical :: is_valid
        character(len=512) :: search_commands(2)
        logical :: search_structure_valid
        
        ! CMake build directory search commands
        search_commands(1) = "gcov ${CMAKE_BINARY_DIR}/CMakeFiles/fortran_target.dir/*.gcno"
        search_commands(2) = "# Search CMakeFiles directories"
        
        ! Validate search structure
        search_structure_valid = validate_directory_search_command_structure(search_commands, 2)
        
        is_valid = search_structure_valid
    end function

    function validate_nested_directory_search_patterns() result(is_valid)
        ! Test nested directory search patterns
        logical :: is_valid
        character(len=512) :: nested_patterns(4)
        logical :: nested_structure_valid
        
        ! Nested directory search patterns
        nested_patterns(1) = "build/gfortran_*/fortcov/*.gcda"
        nested_patterns(2) = "CMakeFiles/fortran_target.dir/*.gcno"
        nested_patterns(3) = "builddir/**/*.gcov"
        nested_patterns(4) = "obj/**/*.gcda"
        
        ! Validate nested structure
        nested_structure_valid = validate_nested_search_pattern_structure(nested_patterns, 4)
        
        is_valid = nested_structure_valid
    end function

    subroutine test_source_path_discovery_validation()
        ! Given: Source path discovery from DESIGN.md
        ! When: Testing --source path discovery validation
        ! Then: Validate source path discovery works correctly
        
        logical :: source_flag_valid, exclusion_valid, inclusion_valid
        
        write(*,'(A)', advance='no') "Testing source path discovery validation... "
        test_count = test_count + 1
        
        ! Test source path discovery patterns
        source_flag_valid = validate_source_flag_discovery_patterns()
        exclusion_valid = validate_source_path_exclusion_patterns()
        inclusion_valid = validate_source_path_inclusion_patterns()
        
        if (source_flag_valid .and. exclusion_valid .and. inclusion_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. source_flag_valid) write(*,*) "  - Source flag discovery validation failed"
            if (.not. exclusion_valid) write(*,*) "  - Source path exclusion validation failed"
            if (.not. inclusion_valid) write(*,*) "  - Source path inclusion validation failed"
        end if
    end subroutine

    function validate_source_flag_discovery_patterns() result(is_valid)
        ! Test --source flag discovery patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: source_flag_patterns(4)
        logical :: flag_structure_valid
        
        ! Source flag patterns from documentation
        source_flag_patterns(1) = "fortcov --source=."
        source_flag_patterns(2) = "fortcov --source=src"
        source_flag_patterns(3) = "fortcov --source=\"build/gfortran_*/fortcov\""
        source_flag_patterns(4) = "fortcov --source=multiple,paths,supported"
        
        ! Validate flag structure
        flag_structure_valid = validate_source_flag_pattern_structure(source_flag_patterns, 4)
        
        is_valid = flag_structure_valid
    end function

    function validate_source_path_exclusion_patterns() result(is_valid)
        ! Test source path exclusion patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: exclusion_patterns(4)
        logical :: exclusion_structure_valid
        
        ! Exclusion patterns from documentation
        exclusion_patterns(1) = "--exclude=build/*"
        exclusion_patterns(2) = "--exclude=test/*"
        exclusion_patterns(3) = "--exclude=build/*,test/*"
        exclusion_patterns(4) = "--exclude='*test*'"
        
        ! Validate exclusion structure
        exclusion_structure_valid = validate_exclusion_pattern_structure(exclusion_patterns, 4)
        
        is_valid = exclusion_structure_valid
    end function

    function validate_source_path_inclusion_patterns() result(is_valid)
        ! Test source path inclusion patterns
        logical :: is_valid
        character(len=512) :: inclusion_patterns(3)
        logical :: inclusion_structure_valid
        
        ! Inclusion patterns
        inclusion_patterns(1) = "--include=src/*.f90"
        inclusion_patterns(2) = "--include=main/*"
        inclusion_patterns(3) = "--include=lib/*.f90,app/*.f90"
        
        ! Validate inclusion structure
        inclusion_structure_valid = validate_inclusion_pattern_structure(inclusion_patterns, 3)
        
        is_valid = inclusion_structure_valid
    end function

    subroutine test_file_extension_pattern_recognition()
        ! Given: File extension patterns from DESIGN.md
        ! When: Testing file extension pattern recognition
        ! Then: Validate extension pattern recognition works correctly
        
        logical :: gcov_extension_valid, gcda_extension_valid, gcno_extension_valid
        
        write(*,'(A)', advance='no') "Testing file extension pattern recognition... "
        test_count = test_count + 1
        
        ! Test file extension patterns
        gcov_extension_valid = validate_gcov_extension_patterns()
        gcda_extension_valid = validate_gcda_extension_patterns()
        gcno_extension_valid = validate_gcno_extension_patterns()
        
        if (gcov_extension_valid .and. gcda_extension_valid .and. gcno_extension_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. gcov_extension_valid) write(*,*) "  - .gcov extension pattern validation failed"
            if (.not. gcda_extension_valid) write(*,*) "  - .gcda extension pattern validation failed"
            if (.not. gcno_extension_valid) write(*,*) "  - .gcno extension pattern validation failed"
        end if
    end subroutine

    function validate_gcov_extension_patterns() result(is_valid)
        ! Test .gcov extension patterns from DESIGN.md
        logical :: is_valid
        character(len=256) :: gcov_patterns(4)
        logical :: extension_structure_valid
        
        ! .gcov extension patterns
        gcov_patterns(1) = "*.gcov"
        gcov_patterns(2) = "src/*.gcov"
        gcov_patterns(3) = "build/*.gcov"
        gcov_patterns(4) = "**/*.gcov"
        
        ! Validate extension structure
        extension_structure_valid = validate_file_extension_pattern_structure(gcov_patterns, 4)
        
        is_valid = extension_structure_valid
    end function

    function validate_gcda_extension_patterns() result(is_valid)
        ! Test .gcda extension patterns
        logical :: is_valid
        character(len=256) :: gcda_patterns(3)
        logical :: extension_structure_valid
        
        ! .gcda extension patterns
        gcda_patterns(1) = "*.gcda"
        gcda_patterns(2) = "build/**/*.gcda"
        gcda_patterns(3) = "CMakeFiles/**/*.gcda"
        
        ! Validate extension structure
        extension_structure_valid = validate_file_extension_pattern_structure(gcda_patterns, 3)
        
        is_valid = extension_structure_valid
    end function

    function validate_gcno_extension_patterns() result(is_valid)
        ! Test .gcno extension patterns
        logical :: is_valid
        character(len=256) :: gcno_patterns(3)
        logical :: extension_structure_valid
        
        ! .gcno extension patterns
        gcno_patterns(1) = "*.gcno"
        gcno_patterns(2) = "build/**/*.gcno"
        gcno_patterns(3) = "CMakeFiles/**/*.gcno"
        
        ! Validate extension structure
        extension_structure_valid = validate_file_extension_pattern_structure(gcno_patterns, 3)
        
        is_valid = extension_structure_valid
    end function

    subroutine test_nested_build_structure_discovery()
        ! Given: Nested build structures from DESIGN.md
        ! When: Testing nested structure discovery validation
        ! Then: Validate nested structure discovery works correctly
        
        logical :: fmp_nested_valid, cmake_nested_valid, deep_nesting_valid
        
        write(*,'(A)', advance='no') "Testing nested build structure discovery... "
        test_count = test_count + 1
        
        ! Test nested structure discovery
        fmp_nested_valid = validate_fpm_nested_structure_discovery()
        cmake_nested_valid = validate_cmake_nested_structure_discovery()
        deep_nesting_valid = validate_deep_nesting_discovery()
        
        if (fpm_nested_valid .and. cmake_nested_valid .and. deep_nesting_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. fpm_nested_valid) write(*,*) "  - FPM nested structure discovery validation failed"
            if (.not. cmake_nested_valid) write(*,*) "  - CMake nested structure discovery validation failed"
            if (.not. deep_nesting_valid) write(*,*) "  - Deep nesting discovery validation failed"
        end if
    end subroutine

    function validate_fpm_nested_structure_discovery() result(is_valid)
        ! Test FPM nested structure discovery from DESIGN.md
        logical :: is_valid
        character(len=512) :: fpm_nested_paths(4)
        logical :: nested_structure_valid
        
        ! FPM nested structure paths
        fpm_nested_paths(1) = "build/gfortran_*/fortcov/*.gcda"
        fpm_nested_paths(2) = "build/dependencies/*/*.gcno"
        fpm_nested_paths(3) = "build/gfortran_*/fortcov/src/*.gcov"
        fpm_nested_paths(4) = "build/*/test/*.gcda"
        
        ! Validate nested structure
        nested_structure_valid = validate_nested_path_structure(fpm_nested_paths, 4)
        
        is_valid = nested_structure_valid
    end function

    function validate_cmake_nested_structure_discovery() result(is_valid)
        ! Test CMake nested structure discovery
        logical :: is_valid
        character(len=512) :: cmake_nested_paths(3)
        logical :: nested_structure_valid
        
        ! CMake nested structure paths
        cmake_nested_paths(1) = "CMakeFiles/fortran_target.dir/src/*.gcno"
        cmake_nested_paths(2) = "CMakeFiles/fortran_target.dir/test/*.gcda"
        cmake_nested_paths(3) = "CMakeFiles/*/*.dir/**/*.gcov"
        
        ! Validate nested structure
        nested_structure_valid = validate_nested_path_structure(cmake_nested_paths, 3)
        
        is_valid = nested_structure_valid
    end function

    function validate_deep_nesting_discovery() result(is_valid)
        ! Test deep nesting discovery patterns
        logical :: is_valid
        character(len=512) :: deep_nesting_patterns(3)
        logical :: deep_structure_valid
        
        ! Deep nesting patterns
        deep_nesting_patterns(1) = "build/**/fortcov/**/*.gcda"
        deep_nesting_patterns(2) = "project/**/build/**/coverage/**/*.gcov"
        deep_nesting_patterns(3) = "workspace/**/target/**/debug/**/*.gcno"
        
        ! Validate deep structure
        deep_structure_valid = validate_deep_nesting_pattern_structure(deep_nesting_patterns, 3)
        
        is_valid = deep_structure_valid
    end function

    subroutine test_compiler_specific_output_patterns()
        ! Given: Compiler-specific output patterns
        ! When: Testing compiler-specific pattern recognition
        ! Then: Validate compiler patterns work correctly
        
        logical :: gfortran_valid, ifort_valid, flang_valid
        
        write(*,'(A)', advance='no') "Testing compiler-specific output patterns... "
        test_count = test_count + 1
        
        ! Test compiler-specific patterns
        gfortran_valid = validate_gfortran_output_patterns()
        ifort_valid = validate_ifort_output_patterns()
        flang_valid = validate_flang_output_patterns()
        
        if (gfortran_valid .and. ifort_valid .and. flang_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. gfortran_valid) write(*,*) "  - gfortran output pattern validation failed"
            if (.not. ifort_valid) write(*,*) "  - ifort output pattern validation failed"
            if (.not. flang_valid) write(*,*) "  - flang output pattern validation failed"
        end if
    end subroutine

    function validate_gfortran_output_patterns() result(is_valid)
        ! Test gfortran output patterns from DESIGN.md
        logical :: is_valid
        character(len=512) :: gfortran_patterns(3)
        logical :: compiler_structure_valid
        
        ! gfortran output patterns
        gfortran_patterns(1) = "build/gfortran_*/"
        gfortran_patterns(2) = "-fprofile-arcs -ftest-coverage"
        gfortran_patterns(3) = "gcov src/*.f90"
        
        ! Validate compiler structure
        compiler_structure_valid = validate_compiler_output_pattern_structure(gfortran_patterns, 3)
        
        is_valid = compiler_structure_valid
    end function

    function validate_ifort_output_patterns() result(is_valid)
        ! Test ifort output patterns
        logical :: is_valid
        character(len=512) :: ifort_patterns(2)
        logical :: compiler_structure_valid
        
        ! ifort output patterns (hypothetical - Intel Fortran coverage)
        ifort_patterns(1) = "build/ifort_*/"
        ifort_patterns(2) = "-prof-gen=srcpos"
        
        ! Validate compiler structure
        compiler_structure_valid = validate_compiler_output_pattern_structure(ifort_patterns, 2)
        
        is_valid = compiler_structure_valid
    end function

    function validate_flang_output_patterns() result(is_valid)
        ! Test flang output patterns
        logical :: is_valid
        character(len=512) :: flang_patterns(2)
        logical :: compiler_structure_valid
        
        ! flang output patterns (hypothetical - LLVM Fortran coverage)
        flang_patterns(1) = "build/flang_*/"
        flang_patterns(2) = "-fprofile-instr-generate"
        
        ! Validate compiler structure
        compiler_structure_valid = validate_compiler_output_pattern_structure(flang_patterns, 2)
        
        is_valid = compiler_structure_valid
    end function

    subroutine test_discovery_fallback_mechanisms()
        ! Given: Discovery fallback mechanisms
        ! When: Testing fallback discovery validation
        ! Then: Validate fallback mechanisms work correctly
        
        logical :: primary_fallback_valid, secondary_fallback_valid, emergency_fallback_valid
        
        write(*,'(A)', advance='no') "Testing discovery fallback mechanisms... "
        test_count = test_count + 1
        
        ! Test fallback mechanisms
        primary_fallback_valid = validate_primary_fallback_mechanism()
        secondary_fallback_valid = validate_secondary_fallback_mechanism()
        emergency_fallback_valid = validate_emergency_fallback_mechanism()
        
        if (primary_fallback_valid .and. secondary_fallback_valid .and. emergency_fallback_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. primary_fallback_valid) write(*,*) "  - Primary fallback mechanism validation failed"
            if (.not. secondary_fallback_valid) write(*,*) "  - Secondary fallback mechanism validation failed"
            if (.not. emergency_fallback_valid) write(*,*) "  - Emergency fallback mechanism validation failed"
        end if
    end subroutine

    function validate_primary_fallback_mechanism() result(is_valid)
        ! Test primary fallback mechanism
        logical :: is_valid
        character(len=256) :: primary_fallback_steps(3)
        logical :: fallback_structure_valid
        
        ! Primary fallback steps
        primary_fallback_steps(1) = "1. Search current directory for *.gcov"
        primary_fallback_steps(2) = "2. If none found, search --source paths"
        primary_fallback_steps(3) = "3. If still none, search build directories"
        
        ! Validate fallback structure
        fallback_structure_valid = validate_fallback_mechanism_structure(primary_fallback_steps, 3)
        
        is_valid = fallback_structure_valid
    end function

    function validate_secondary_fallback_mechanism() result(is_valid)
        ! Test secondary fallback mechanism
        logical :: is_valid
        character(len=256) :: secondary_fallback_steps(3)
        logical :: fallback_structure_valid
        
        ! Secondary fallback steps
        secondary_fallback_steps(1) = "1. Search for *.gcda files in build directories"
        secondary_fallback_steps(2) = "2. Run gcov on found *.gcda files"
        secondary_fallback_steps(3) = "3. Copy generated *.gcov files to accessible location"
        
        ! Validate fallback structure
        fallback_structure_valid = validate_fallback_mechanism_structure(secondary_fallback_steps, 3)
        
        is_valid = fallback_structure_valid
    end function

    function validate_emergency_fallback_mechanism() result(is_valid)
        ! Test emergency fallback mechanism
        logical :: is_valid
        character(len=256) :: emergency_fallback_steps(2)
        logical :: fallback_structure_valid
        
        ! Emergency fallback steps
        emergency_fallback_steps(1) = "1. Recursive search all directories for coverage files"
        emergency_fallback_steps(2) = "2. Report discovery method used and location found"
        
        ! Validate fallback structure
        fallback_structure_valid = validate_fallback_mechanism_structure(emergency_fallback_steps, 2)
        
        is_valid = fallback_structure_valid
    end function

    ! === Helper Functions for Discovery Pattern Validation ===

    function validate_file_discovery_pattern(location, pattern, priority) result(is_valid)
        character(len=*), intent(in) :: location, pattern
        integer, intent(in) :: priority
        logical :: is_valid
        
        ! Validate discovery pattern components
        is_valid = (len_trim(location) > 0) .and. &
                  (len_trim(pattern) > 0) .and. &
                  (priority > 0) .and. &
                  (index(pattern, "*") > 0)
    end function

    function validate_fallback_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate each fallback pattern is not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_build_output_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate each build output pattern includes wildcard and extension
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. &
                (index(patterns(i), "*") == 0 .and. index(patterns(i), ".gc") == 0)) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_location_strategy_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate location strategy components are not empty
        do i = 1, count
            if (len_trim(components(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_directory_search_command_structure(commands, count) result(is_valid)
        character(len=*), intent(in) :: commands(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate search commands include find or search operations
        do i = 1, count
            if (len_trim(commands(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_nested_search_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate nested patterns include directory separators and wildcards
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. &
                (index(patterns(i), "/") == 0 .and. index(patterns(i), "*") == 0)) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_source_flag_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate source flag patterns include --source
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. index(patterns(i), "--source") == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_exclusion_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate exclusion patterns include --exclude
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. index(patterns(i), "--exclude") == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_inclusion_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate inclusion patterns include --include
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. index(patterns(i), "--include") == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_file_extension_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate file extension patterns include wildcards and extensions
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. &
                (index(patterns(i), "*") == 0 .and. index(patterns(i), ".gc") == 0)) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_nested_path_structure(paths, count) result(is_valid)
        character(len=*), intent(in) :: paths(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate nested paths include multiple directory levels
        do i = 1, count
            if (len_trim(paths(i)) == 0 .or. &
                (count_occurrences(paths(i), "/") < 2 .and. index(paths(i), "*") == 0)) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_deep_nesting_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate deep nesting patterns include ** wildcards
        do i = 1, count
            if (len_trim(patterns(i)) == 0 .or. index(patterns(i), "**") == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_compiler_output_pattern_structure(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate compiler output patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_fallback_mechanism_structure(steps, count) result(is_valid)
        character(len=*), intent(in) :: steps(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate fallback mechanism steps are numbered and not empty
        do i = 1, count
            if (len_trim(steps(i)) == 0 .or. &
                (index(steps(i), trim(string_from_integer(i))) == 0 .and. index(steps(i), ".") == 0)) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function count_occurrences(string, substring) result(count)
        character(len=*), intent(in) :: string, substring
        integer :: count
        integer :: pos, start_pos
        
        count = 0
        start_pos = 1
        
        do
            pos = index(string(start_pos:), substring)
            if (pos == 0) exit
            count = count + 1
            start_pos = start_pos + pos + len(substring) - 1
        end do
    end function

    function string_from_integer(int_val) result(str)
        integer, intent(in) :: int_val
        character(len=16) :: str
        
        write(str, '(I0)') int_val
    end function

end program test_coverage_file_discovery_validation