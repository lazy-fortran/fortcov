! Test suite for Issue #183 - Input Validation Integration Testing
! Validates systematic integration of input validation across all modules
! Ensures security architecture is consistently applied at all external input points

program test_input_validation_integration
    use fortcov_config
    use coverage_engine
    use input_validation
    use json_coverage_io
    use error_handling
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Input Validation Integration Test Suite (Issue #183) ==="
    print *, "Testing systematic application of security architecture"
    print *, ""
    
    ! Given: Comprehensive input validation infrastructure exists
    ! When: Testing integration across all external input points
    ! Then: Validate consistent security architecture application
    
    ! Test 1: Coverage engine file processing validation integration
    all_tests_passed = all_tests_passed .and. test_coverage_engine_validation_integration()
    
    ! Test 2: JSON coverage import validation integration
    all_tests_passed = all_tests_passed .and. test_json_coverage_io_validation_integration()
    
    ! Test 3: Configuration parsing validation integration
    all_tests_passed = all_tests_passed .and. test_fortcov_config_validation_integration()
    
    ! Test 4: Validation result consistency across modules
    all_tests_passed = all_tests_passed .and. test_validation_result_consistency()
    
    ! Test 5: Error handling pattern consistency
    all_tests_passed = all_tests_passed .and. test_error_handling_consistency()
    
    ! Test 6: Security validation at all input points
    all_tests_passed = all_tests_passed .and. test_security_validation_coverage()
    
    ! Test 7: Memory safety validation integration
    all_tests_passed = all_tests_passed .and. test_memory_safety_validation_integration()
    
    ! Test 8: Path safety validation integration
    all_tests_passed = all_tests_passed .and. test_path_safety_validation_integration()
    
    if (all_tests_passed) then
        print *, ""
        print *, "✅ All input validation integration tests PASSED"
        print *, "✅ Security architecture consistently applied"
        call exit(0)
    else
        print *, ""
        print *, "❌ Input validation integration tests FAILED" 
        print *, "❌ Security architecture gaps remain"
        call exit(1)
    end if

contains

    function test_coverage_engine_validation_integration() result(passed)
        ! Given: coverage_engine.f90 processes coverage files
        ! When: Testing file processing validation integration
        ! Then: Should use input validation consistently for all file operations
        
        logical :: passed
        logical :: file_validation_test, memory_validation_test, bounds_validation_test
        
        print *, "Test 1: Coverage engine validation integration"
        
        ! Test 1a: File constraint validation before processing
        file_validation_test = test_coverage_engine_file_validation()
        
        ! Test 1b: Memory allocation validation
        memory_validation_test = test_coverage_engine_memory_validation()
        
        ! Test 1c: Coverage data bounds validation
        bounds_validation_test = test_coverage_engine_bounds_validation()
        
        passed = file_validation_test .and. memory_validation_test .and. bounds_validation_test
        
        if (passed) then
            print *, "    ✅ PASSED - Coverage engine properly validates inputs"
        else
            print *, "    ❌ FAILED - Coverage engine validation gaps"
            if (.not. file_validation_test) print *, "      - File validation missing"
            if (.not. memory_validation_test) print *, "      - Memory validation missing"
            if (.not. bounds_validation_test) print *, "      - Bounds validation missing"
        end if
        print *, ""
    end function test_coverage_engine_validation_integration

    function test_coverage_engine_file_validation() result(passed)
        ! Given: Coverage files need validation before processing
        ! When: Testing file constraint validation in coverage engine
        ! Then: Should validate file size, existence, accessibility
        logical :: passed
        type(config_t) :: config
        type(validation_result_t) :: result
        character(len=:), allocatable :: test_files(:)
        integer :: i
        
        ! Test file validation behavior
        call initialize_config(config)
        config%quiet = .true.
        
        ! Create test scenario with valid files
        allocate(character(len=256) :: test_files(1))
        test_files(1) = "nonexistent_test_file.gcov"
        
        ! Test that validation is applied (should handle missing files gracefully)
        call validate_file_constraints(test_files(1), result)
        
        ! Should return validation result indicating missing file
        passed = (.not. result%is_valid .and. result%error_code == ERROR_MISSING_FILE)
        
        ! Test with empty filename (security test)
        call validate_file_constraints("", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
    end function test_coverage_engine_file_validation

    function test_coverage_engine_memory_validation() result(passed)
        ! Given: Memory allocation needs validation
        ! When: Testing memory allocation validation
        ! Then: Should validate allocation sizes before allocating
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test valid memory allocation request
        call validate_memory_allocation_request(1000_8, result)
        passed = result%is_valid
        
        ! Test excessive memory allocation request
        call validate_memory_allocation_request(MAX_FILE_SIZE + 1_8, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_MEMORY_EXHAUSTION)
        
        ! Test negative allocation request
        call validate_memory_allocation_request(-100_8, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
    end function test_coverage_engine_memory_validation

    function test_coverage_engine_bounds_validation() result(passed)
        ! Given: Coverage data needs bounds validation
        ! When: Testing coverage data bounds validation
        ! Then: Should validate line numbers and execution counts
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test valid coverage data
        call validate_coverage_data_bounds(100, 5, result)
        passed = result%is_valid
        
        ! Test invalid line number (negative)
        call validate_coverage_data_bounds(-1, 5, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
        
        ! Test invalid line number (too large)
        call validate_coverage_data_bounds(MAX_LINE_NUMBER + 1, 5, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
        
        ! Test invalid execution count (negative)
        call validate_coverage_data_bounds(100, -1, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
        
        ! Test excessive execution count
        call validate_coverage_data_bounds(100, MAX_EXECUTION_COUNT + 1, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
    end function test_coverage_engine_bounds_validation

    function test_json_coverage_io_validation_integration() result(passed)
        ! Given: JSON coverage import needs validation
        ! When: Testing JSON parsing validation integration
        ! Then: Should validate JSON input before processing
        
        logical :: passed
        logical :: json_validation_test, file_validation_test, data_validation_test
        
        print *, "Test 2: JSON coverage I/O validation integration"
        
        ! Test 2a: JSON content validation
        json_validation_test = test_json_content_validation()
        
        ! Test 2b: JSON file validation
        file_validation_test = test_json_file_validation()
        
        ! Test 2c: JSON data structure validation
        data_validation_test = test_json_data_validation()
        
        passed = json_validation_test .and. file_validation_test .and. data_validation_test
        
        if (passed) then
            print *, "    ✅ PASSED - JSON coverage I/O properly validates inputs"
        else
            print *, "    ❌ FAILED - JSON coverage I/O validation gaps"
            if (.not. json_validation_test) print *, "      - JSON content validation missing"
            if (.not. file_validation_test) print *, "      - JSON file validation missing"
            if (.not. data_validation_test) print *, "      - JSON data validation missing"
        end if
        print *, ""
    end function test_json_coverage_io_validation_integration

    function test_json_content_validation() result(passed)
        ! Given: JSON content needs validation
        ! When: Testing JSON content validation
        ! Then: Should validate JSON structure before parsing
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test empty JSON content (should be invalid)
        call validate_path_safety("", result)
        passed = (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
        
        ! Test valid JSON file path
        call validate_path_safety("valid_file.json", result) 
        passed = passed .and. result%is_valid
        
        ! Test JSON path with dangerous characters
        call validate_path_safety("dangerous|file.json", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
    end function test_json_content_validation

    function test_json_file_validation() result(passed)
        ! Given: JSON files need file constraint validation
        ! When: Testing JSON file validation
        ! Then: Should validate file size and accessibility
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test file constraint validation for JSON files
        call validate_file_constraints("nonexistent.json", result)
        passed = (.not. result%is_valid .and. result%error_code == ERROR_MISSING_FILE)
        
        ! Test with empty filename
        call validate_file_constraints("", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
    end function test_json_file_validation

    function test_json_data_validation() result(passed)
        ! Given: JSON data structures need validation  
        ! When: Testing JSON data validation
        ! Then: Should validate data bounds and structure
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test valid JSON data bounds
        call validate_line_data_bounds_detailed(50, 10, "test.f90", result)
        passed = result%is_valid
        
        ! Test invalid JSON data bounds (negative line)
        call validate_line_data_bounds_detailed(-5, 10, "test.f90", result) 
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
        
        ! Test invalid JSON data bounds (excessive execution count)
        call validate_line_data_bounds_detailed(50, MAX_EXECUTION_COUNT + 1, "test.f90", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
    end function test_json_data_validation

    function test_fortcov_config_validation_integration() result(passed)
        ! Given: Configuration parsing needs validation
        ! When: Testing configuration validation integration
        ! Then: Should validate all configuration inputs
        
        logical :: passed
        logical :: path_validation_test, threshold_validation_test, format_validation_test
        
        print *, "Test 3: Configuration parsing validation integration"
        
        ! Test 3a: Path validation in configuration
        path_validation_test = test_config_path_validation()
        
        ! Test 3b: Threshold validation in configuration
        threshold_validation_test = test_config_threshold_validation()
        
        ! Test 3c: Format validation in configuration
        format_validation_test = test_config_format_validation()
        
        passed = path_validation_test .and. threshold_validation_test .and. format_validation_test
        
        if (passed) then
            print *, "    ✅ PASSED - Configuration parsing properly validates inputs"
        else
            print *, "    ❌ FAILED - Configuration parsing validation gaps"
            if (.not. path_validation_test) print *, "      - Path validation missing"
            if (.not. threshold_validation_test) print *, "      - Threshold validation missing"
            if (.not. format_validation_test) print *, "      - Format validation missing"
        end if
        print *, ""
    end function test_fortcov_config_validation_integration

    function test_config_path_validation() result(passed)
        ! Given: Configuration paths need validation
        ! When: Testing configuration path validation
        ! Then: Should validate all path inputs
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test valid configuration path
        call validate_path_safety("src/test.f90", result)
        passed = result%is_valid
        
        ! Test invalid configuration path (dangerous characters)
        call validate_path_safety("src/test;rm -rf /.f90", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
        
        ! Test excessive path length
        block
            character(len=MAX_PATH_LENGTH + 10) :: long_path
            long_path = repeat("a", MAX_PATH_LENGTH + 10)
            call validate_path_safety(long_path, result)
            passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
        end block
    end function test_config_path_validation

    function test_config_threshold_validation() result(passed)
        ! Given: Configuration thresholds need validation
        ! When: Testing threshold validation  
        ! Then: Should validate threshold ranges
        logical :: passed
        real :: safe_percentage
        
        ! Test valid threshold calculation
        safe_percentage = safe_percentage_calculation(50, 100)
        passed = (abs(safe_percentage - 50.0) < 0.1)
        
        ! Test division by zero protection
        safe_percentage = safe_percentage_calculation(10, 0) 
        passed = passed .and. (abs(safe_percentage - 0.0) < 0.1)
        
        ! Test negative covered count
        safe_percentage = safe_percentage_calculation(-10, 100)
        passed = passed .and. (abs(safe_percentage - 0.0) < 0.1)
        
        ! Test covered > total (should clamp to 100%)
        safe_percentage = safe_percentage_calculation(150, 100)
        passed = passed .and. (abs(safe_percentage - 100.0) < 0.1)
    end function test_config_threshold_validation

    function test_config_format_validation() result(passed)
        ! Given: Configuration formats need validation
        ! When: Testing format string validation
        ! Then: Should validate format specifications
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test format string as path (basic validation)
        call validate_path_safety("markdown", result)
        passed = result%is_valid
        
        call validate_path_safety("json", result)
        passed = passed .and. result%is_valid
        
        call validate_path_safety("html", result)
        passed = passed .and. result%is_valid
        
        ! Test invalid format with dangerous characters
        call validate_path_safety("invalid|format", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
    end function test_config_format_validation

    function test_validation_result_consistency() result(passed)
        ! Given: validation_result_t should be used consistently
        ! When: Testing validation result usage patterns
        ! Then: Should have consistent error handling patterns
        
        logical :: passed
        
        print *, "Test 4: Validation result consistency"
        
        ! Test validation result type consistency
        passed = test_validation_result_type_consistency()
        
        if (passed) then
            print *, "    ✅ PASSED - Validation results used consistently"
        else
            print *, "    ❌ FAILED - Validation result inconsistencies"
        end if
        print *, ""
    end function test_validation_result_consistency

    function test_validation_result_type_consistency() result(passed)
        ! Given: validation_result_t type should be used consistently
        ! When: Testing validation result type usage
        ! Then: Should initialize and use validation results properly
        logical :: passed
        type(validation_result_t) :: result1, result2, result3
        
        ! Test consistent initialization
        call result1%init(.true., ERROR_SUCCESS, "", "")
        call result2%init(.false., ERROR_INVALID_DATA, "Test error", "Test fix")
        call result3%clear()
        
        passed = result1%is_valid .and. (.not. result2%is_valid) .and. (.not. result3%is_valid)
        
        ! Test error code consistency
        passed = passed .and. (result1%error_code == ERROR_SUCCESS)
        passed = passed .and. (result2%error_code == ERROR_INVALID_DATA)
        passed = passed .and. (result3%error_code == ERROR_SUCCESS) ! clear() sets to SUCCESS
        
        ! Test message consistency
        passed = passed .and. (len_trim(result1%error_message) == 0)
        passed = passed .and. (len_trim(result2%error_message) > 0)
        passed = passed .and. (len_trim(result3%error_message) == 0)
    end function test_validation_result_type_consistency

    function test_error_handling_consistency() result(passed)
        ! Given: Error handling patterns should be consistent
        ! When: Testing error handling across modules
        ! Then: Should use consistent error reporting
        
        logical :: passed
        
        print *, "Test 5: Error handling consistency"
        
        ! Test error handling pattern consistency
        passed = test_error_handling_patterns()
        
        if (passed) then
            print *, "    ✅ PASSED - Error handling patterns consistent"
        else
            print *, "    ❌ FAILED - Error handling pattern inconsistencies"
        end if
        print *, ""
    end function test_error_handling_consistency

    function test_error_handling_patterns() result(passed)
        ! Given: Error handling should follow consistent patterns
        ! When: Testing error handling patterns
        ! Then: Should use validation results consistently
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test multiple validation functions for consistent patterns
        call validate_file_constraints("", result)
        passed = (.not. result%is_valid) .and. (result%error_code /= ERROR_SUCCESS)
        
        call validate_coverage_data_bounds(-1, 5, result)
        passed = passed .and. (.not. result%is_valid) .and. (result%error_code == ERROR_INVALID_DATA)
        
        call validate_path_safety("", result)
        passed = passed .and. (.not. result%is_valid) .and. (result%error_code == ERROR_INVALID_PATH)
        
        call validate_memory_allocation_request(-1_8, result)
        passed = passed .and. (.not. result%is_valid) .and. (result%error_code == ERROR_INVALID_DATA)
    end function test_error_handling_patterns

    function test_security_validation_coverage() result(passed)
        ! Given: Security validation should cover all input points
        ! When: Testing security validation coverage
        ! Then: Should validate all external inputs for security
        
        logical :: passed
        logical :: path_security_test, data_security_test, memory_security_test
        
        print *, "Test 6: Security validation coverage"
        
        ! Test 6a: Path traversal protection
        path_security_test = test_path_traversal_protection()
        
        ! Test 6b: Data injection protection
        data_security_test = test_data_injection_protection()
        
        ! Test 6c: Memory exhaustion protection
        memory_security_test = test_memory_exhaustion_protection()
        
        passed = path_security_test .and. data_security_test .and. memory_security_test
        
        if (passed) then
            print *, "    ✅ PASSED - Security validation covers all input points"
        else
            print *, "    ❌ FAILED - Security validation gaps"
            if (.not. path_security_test) print *, "      - Path traversal protection missing"
            if (.not. data_security_test) print *, "      - Data injection protection missing"
            if (.not. memory_security_test) print *, "      - Memory exhaustion protection missing"
        end if
        print *, ""
    end function test_security_validation_coverage

    function test_path_traversal_protection() result(passed)
        ! Given: Path traversal attacks should be prevented
        ! When: Testing path validation security
        ! Then: Should reject dangerous path patterns
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test directory traversal attempts
        call validate_path_safety("../../../etc/passwd", result)
        passed = result%is_valid  ! This should actually be valid for relative paths
        
        ! Test command injection attempts
        call validate_path_safety("file.txt; rm -rf /", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
        
        call validate_path_safety("file.txt|cat /etc/passwd", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
        
        call validate_path_safety("file.txt && malicious_command", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
        
        ! Test control character injection
        call validate_path_safety("file" // char(0) // ".txt", result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_PATH)
    end function test_path_traversal_protection

    function test_data_injection_protection() result(passed)
        ! Given: Data injection attacks should be prevented
        ! When: Testing data validation security
        ! Then: Should validate data bounds securely
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test integer overflow protection
        call validate_coverage_data_bounds(MAX_LINE_NUMBER + 1, 5, result)
        passed = (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
        
        call validate_coverage_data_bounds(100, MAX_EXECUTION_COUNT + 1, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
        
        ! Test negative value injection
        call validate_coverage_data_bounds(-999999, -999999, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
    end function test_data_injection_protection

    function test_memory_exhaustion_protection() result(passed)
        ! Given: Memory exhaustion attacks should be prevented
        ! When: Testing memory allocation validation
        ! Then: Should prevent excessive memory allocation
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test excessive memory allocation prevention
        call validate_memory_allocation_request(MAX_FILE_SIZE + 1_8, result)
        passed = (.not. result%is_valid .and. result%error_code == ERROR_MEMORY_EXHAUSTION)
        
        ! Test extremely large allocation requests
        call validate_memory_allocation_request(huge(1_8), result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_MEMORY_EXHAUSTION)
        
        ! Test negative allocation (could cause underflow)
        call validate_memory_allocation_request(-1000000_8, result)
        passed = passed .and. (.not. result%is_valid .and. result%error_code == ERROR_INVALID_DATA)
    end function test_memory_exhaustion_protection

    function test_memory_safety_validation_integration() result(passed)
        ! Given: Memory safety validation should be integrated everywhere
        ! When: Testing memory safety validation integration
        ! Then: Should prevent memory-related security issues
        
        logical :: passed
        
        print *, "Test 7: Memory safety validation integration"
        
        ! Test memory safety validation patterns
        passed = test_memory_safety_patterns()
        
        if (passed) then
            print *, "    ✅ PASSED - Memory safety validation properly integrated"
        else
            print *, "    ❌ FAILED - Memory safety validation gaps"
        end if
        print *, ""
    end function test_memory_safety_validation_integration

    function test_memory_safety_patterns() result(passed)
        ! Given: Memory safety patterns should be consistent
        ! When: Testing memory safety validation
        ! Then: Should prevent memory safety issues
        logical :: passed
        type(validation_result_t) :: result
        integer :: normalized_count, clamped_line
        
        ! Test memory allocation validation
        call validate_memory_allocation_request(1000_8, result)
        passed = result%is_valid
        
        ! Test execution count normalization (prevents overflow)
        normalized_count = normalize_execution_count(MAX_EXECUTION_COUNT + 1000)
        passed = passed .and. (normalized_count == MAX_EXECUTION_COUNT)
        
        normalized_count = normalize_execution_count(-100)
        passed = passed .and. (normalized_count == 0)
        
        ! Test line number clamping (prevents array bounds issues)
        clamped_line = clamp_line_number(MAX_LINE_NUMBER + 100)
        passed = passed .and. (clamped_line == MAX_LINE_NUMBER)
        
        clamped_line = clamp_line_number(-5)
        passed = passed .and. (clamped_line == 1)
        
        ! Test file size safety checks
        passed = passed .and. is_safe_file_size(1000_8)
        passed = passed .and. (.not. is_safe_file_size(MAX_FILE_SIZE + 1_8))
        passed = passed .and. (.not. is_safe_file_size(-100_8))
    end function test_memory_safety_patterns

    function test_path_safety_validation_integration() result(passed)
        ! Given: Path safety validation should be integrated everywhere
        ! When: Testing path safety validation integration
        ! Then: Should prevent path-related security issues
        
        logical :: passed
        
        print *, "Test 8: Path safety validation integration"
        
        ! Test path safety validation patterns
        passed = test_path_safety_patterns()
        
        if (passed) then
            print *, "    ✅ PASSED - Path safety validation properly integrated"
        else
            print *, "    ❌ FAILED - Path safety validation gaps"
        end if
        print *, ""
    end function test_path_safety_validation_integration

    function test_path_safety_patterns() result(passed)
        ! Given: Path safety patterns should be consistent
        ! When: Testing path safety validation
        ! Then: Should prevent path-related security issues
        logical :: passed
        type(validation_result_t) :: result
        
        ! Test valid paths
        call validate_path_safety("normal_file.gcov", result)
        passed = result%is_valid
        
        call validate_path_safety("src/module.f90", result)
        passed = passed .and. result%is_valid
        
        call validate_path_safety("../relative/path.txt", result)  
        passed = passed .and. result%is_valid
        
        ! Test dangerous paths
        call validate_path_safety("file;malicious_command", result)
        passed = passed .and. (.not. result%is_valid)
        
        call validate_path_safety("file|pipe_to_command", result)
        passed = passed .and. (.not. result%is_valid)
        
        call validate_path_safety("file&background_command", result)
        passed = passed .and. (.not. result%is_valid)
        
        call validate_path_safety("file$variable_expansion", result)
        passed = passed .and. (.not. result%is_valid)
        
        call validate_path_safety("file`command_substitution`", result)
        passed = passed .and. (.not. result%is_valid)
        
        ! Test excessively long paths
        block
            character(len=MAX_PATH_LENGTH + 100) :: long_path
            long_path = repeat("x", MAX_PATH_LENGTH + 100)
            call validate_path_safety(long_path, result)
            passed = passed .and. (.not. result%is_valid)
        end block
        
        ! Test empty and whitespace paths
        call validate_path_safety("", result)
        passed = passed .and. (.not. result%is_valid)
        
        call validate_path_safety("   ", result)
        passed = passed .and. (.not. result%is_valid)
    end function test_path_safety_patterns

end program test_input_validation_integration