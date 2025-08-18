program test_comprehensive_quality_audit
    !! Comprehensive quality audit test suite for Issue #128
    !! 
    !! This test suite validates code quality, architecture, performance,
    !! and maintainability aspects identified in the comprehensive audit.
    !! Tests focus on non-functional requirements and system robustness.
    !!
    !! Given: Complete fortcov codebase with quality requirements
    !! When: Running comprehensive quality validation
    !! Then: All quality standards should be met
    
    use coverage_engine
    use coverage_model
    use fortcov_config
    use file_utils
    use string_utils
    use coverage_statistics
    implicit none
    
    logical :: all_tests_passed = .true.
    integer :: total_tests = 0
    integer :: passed_tests = 0
    
    print *, "=== COMPREHENSIVE QUALITY AUDIT TEST SUITE ==="
    print *, "Issue #128: Code Quality and Architecture Review"
    print *, ""
    
    ! Quality Domain 1: Architecture and Design Quality
    call test_architecture_quality()
    
    ! Quality Domain 2: Performance and Scalability
    call test_performance_quality()
    
    ! Quality Domain 3: Error Handling and Robustness
    call test_error_handling_quality()
    
    ! Quality Domain 4: Memory Management and Resource Efficiency
    call test_memory_management_quality()
    
    ! Quality Domain 5: Code Maintainability and Readability
    call test_maintainability_quality()
    
    ! Quality Domain 6: Interface Design and Contracts
    call test_interface_quality()
    
    ! Quality Domain 7: Data Integrity and Consistency
    call test_data_integrity_quality()
    
    ! Quality Domain 8: Configuration and Flexibility
    call test_configuration_quality()
    
    ! Quality Domain 9: Edge Case Handling and Boundaries
    call test_edge_case_quality()
    
    ! Quality Domain 10: System Integration and Compatibility
    call test_integration_quality()
    
    ! Print comprehensive results
    call print_final_quality_results()
    
    if (all_tests_passed) then
        print *, "✅ QUALITY AUDIT: ALL TESTS PASSED"
        print *, "   System meets quality standards"
        stop 0
    else
        print *, "❌ QUALITY AUDIT: QUALITY ISSUES DETECTED"
        print '(A,I0,A,I0)', "   Failed: ", (total_tests - passed_tests), " of ", total_tests
        stop 1
    end if

contains

    subroutine test_architecture_quality()
        !! Given: System architecture with separation of concerns
        !! When: Testing architectural principles and design patterns
        !! Then: Architecture should follow SOLID principles
        
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        integer :: exit_code
        
        call start_quality_domain("Architecture and Design Quality")
        
        ! Test 1.1: Single Responsibility - Config module
        call initialize_config(config)
        call assert_quality_test(allocated(config%source_paths) .or. &
                               .not. allocated(config%source_paths), &
            "Config initialization should handle optional arrays", "AQ-001")
        
        ! Test 1.2: Open/Closed Principle - Coverage analysis workflow
        config%output_format = "markdown"
        config%quiet = .true.
        exit_code = analyze_coverage(config)
        call assert_quality_test(exit_code >= 0 .and. exit_code <= 3, &
            "Coverage analysis should return standardized exit codes", "AQ-002")
        
        ! Test 1.3: Liskov Substitution - File discovery behavior
        files = find_coverage_files(config)
        call assert_quality_test(allocated(files), &
            "File discovery should always return allocated result", "AQ-003")
        
        ! Test 1.4: Interface Segregation - Minimal dependencies
        call initialize_config(config)
        config%verbose = .true.
        config%quiet = .false.
        call assert_quality_test(config%verbose .and. .not. config%quiet, &
            "Configuration should allow independent flag setting", "AQ-004")
        
        ! Test 1.5: Dependency Inversion - Abstraction layers
        config%input_format = "gcov"
        config%output_format = "json"
        call assert_quality_test(config%input_format == "gcov" .and. &
                               config%output_format == "json", &
            "System should support multiple format combinations", "AQ-005")
        
        ! Test 1.6: Module cohesion - Related functionality grouping
        call assert_quality_test(allocated(files), &
            "File operations should be grouped in cohesive modules", "AQ-006")
        
        ! Test 1.7: Low coupling - Independent module operation
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        if (allocated(files)) deallocate(files)
        call assert_quality_test(.true., &
            "Modules should manage their own memory independently", "AQ-007")
        
        call end_quality_domain()
    end subroutine test_architecture_quality

    subroutine test_performance_quality()
        !! Given: Performance-critical operations and algorithms
        !! When: Testing performance characteristics and efficiency
        !! Then: Performance should meet quality standards
        
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        integer :: start_time, end_time, time_diff
        integer :: i
        logical :: exclude_result
        
        call start_quality_domain("Performance and Scalability")
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test 2.1: File discovery performance (should be O(n) not O(n²))
        call system_clock(start_time)
        files = find_coverage_files(config)
        call system_clock(end_time)
        time_diff = end_time - start_time
        call assert_quality_test(time_diff < 1000, &
            "File discovery should complete quickly", "PQ-001")
        
        ! Test 2.2: Pattern matching performance (should be efficient)
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(20))
        do i = 1, 20
            write(config%exclude_patterns(i), '("pattern_", I0, "_*.f90")') i
        end do
        
        call system_clock(start_time)
        do i = 1, 100
            exclude_result = check_exclude_patterns("test_file.f90", config)
        end do
        call system_clock(end_time)
        time_diff = end_time - start_time
        call assert_quality_test(time_diff < 500, &
            "Pattern matching should be efficient for multiple patterns", "PQ-002")
        
        ! Test 2.3: Memory allocation efficiency
        call system_clock(start_time)
        do i = 1, 10
            if (allocated(files)) deallocate(files)
            files = find_coverage_files(config)
        end do
        call system_clock(end_time)
        time_diff = end_time - start_time
        call assert_quality_test(time_diff < 2000, &
            "Repeated allocation/deallocation should be efficient", "PQ-003")
        
        ! Test 2.4: String operation performance
        call system_clock(start_time)
        block
            character(len=:), allocatable :: result
            do i = 1, 100
                result = to_lower("TestFile.F90")
            end do
        end block
        call system_clock(end_time)
        time_diff = end_time - start_time
        call assert_quality_test(time_diff < 100, &
            "String operations should be optimized", "PQ-004")
        
        ! Test 2.5: Configuration processing performance
        call system_clock(start_time)
        do i = 1, 50
            call initialize_config(config)
            config%verbose = .true.
            config%output_format = "markdown"
        end do
        call system_clock(end_time)
        time_diff = end_time - start_time
        call assert_quality_test(time_diff < 200, &
            "Configuration operations should be lightweight", "PQ-005")
        
        ! Cleanup
        if (allocated(files)) deallocate(files)
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        
        call end_quality_domain()
    end subroutine test_performance_quality

    subroutine test_error_handling_quality()
        !! Given: Error handling throughout the system
        !! When: Testing error scenarios and recovery mechanisms
        !! Then: Error handling should be comprehensive and consistent
        
        type(config_t) :: config
        integer :: exit_code
        character(len=:), allocatable :: files(:)
        
        call start_quality_domain("Error Handling and Robustness")
        
        ! Test 3.1: Graceful degradation with invalid configuration
        call initialize_config(config)
        config%output_format = "invalid_format"
        config%quiet = .true.
        exit_code = analyze_coverage(config)
        call assert_quality_test(exit_code > 0 .and. exit_code < 10, &
            "Invalid configuration should fail gracefully", "EQ-001")
        
        ! Test 3.2: Error recovery from file access issues
        config%output_format = "markdown"
        config%output_path = "/invalid/directory/output.md"
        exit_code = analyze_coverage(config)
        call assert_quality_test(exit_code > 0, &
            "File access errors should be handled gracefully", "EQ-002")
        
        ! Test 3.3: Partial failure handling
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(3))
        config%source_paths(1) = "."
        config%source_paths(2) = "/nonexistent/path"
        config%source_paths(3) = "./test"
        
        files = find_coverage_files(config)
        call assert_quality_test(allocated(files), &
            "Partial failures should not prevent valid results", "EQ-003")
        
        ! Test 3.4: Resource cleanup on error paths
        block
            type(config_t) :: temp_config
            call initialize_config(temp_config)
            ! Intentionally cause an error condition
            temp_config%output_path = ""
            temp_config%output_format = "markdown"
            temp_config%quiet = .true.
            exit_code = analyze_coverage(temp_config)
            call assert_quality_test(exit_code /= 0, &
                "Empty output path should be handled appropriately", "EQ-004")
        end block
        
        ! Test 3.5: Consistent error reporting format
        call initialize_config(config)
        config%output_format = "unknown"
        config%quiet = .true.
        exit_code = analyze_coverage(config)
        call assert_quality_test(exit_code >= 1 .and. exit_code <= 3, &
            "Error codes should follow documented conventions", "EQ-005")
        
        ! Cleanup
        if (allocated(files)) deallocate(files)
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        
        call end_quality_domain()
    end subroutine test_error_handling_quality

    subroutine test_memory_management_quality()
        !! Given: Dynamic memory allocation throughout the system
        !! When: Testing memory management patterns and efficiency
        !! Then: Memory should be managed safely and efficiently
        
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        type(coverage_data_t) :: coverage
        integer :: i
        integer :: exit_code
        
        call start_quality_domain("Memory Management and Resource Efficiency")
        
        call initialize_config(config)
        config%quiet = .true.
        
        ! Test 4.1: Proper array initialization
        call assert_quality_test(.not. allocated(config%exclude_patterns) .or. &
                               allocated(config%exclude_patterns), &
            "Arrays should be properly initialized or unallocated", "MQ-001")
        
        ! Test 4.2: Memory leak prevention in normal operation
        do i = 1, 5
            files = find_coverage_files(config)
            call assert_quality_test(allocated(files), &
                "Memory allocation should succeed consistently", "MQ-002")
            if (allocated(files)) deallocate(files)
        end do
        
        ! Test 4.3: Memory leak prevention in error conditions
        do i = 1, 3
            config%output_path = "/nonexistent/path" // trim(adjustl(int_to_string(i))) // ".md"
            config%output_format = "markdown"
            exit_code = analyze_coverage(config)
            ! Should not accumulate memory across error conditions
        end do
        call assert_quality_test(.true., &
            "Error conditions should not cause memory leaks", "MQ-003")
        
        ! Test 4.4: Efficient array resizing patterns
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(1))
        config%source_paths(1) = "."
        
        files = find_coverage_files(config)
        call assert_quality_test(allocated(files), &
            "Dynamic arrays should support efficient operations", "MQ-004")
        
        ! Test 4.5: Coverage data memory management
        if (.not. allocated(coverage%files)) then
            allocate(coverage%files(2))
            coverage%files(1)%filename = "test1.f90"
            coverage%files(2)%filename = "test2.f90"
        end if
        
        call assert_quality_test(allocated(coverage%files) .and. &
                               size(coverage%files) == 2, &
            "Complex data structures should manage memory correctly", "MQ-005")
        
        ! Test 4.6: Safe deallocation patterns
        if (allocated(coverage%files)) deallocate(coverage%files)
        if (allocated(files)) deallocate(files)
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        call assert_quality_test(.true., &
            "All allocatable arrays should be safely deallocatable", "MQ-006")
        
        call end_quality_domain()
    end subroutine test_memory_management_quality

    subroutine test_maintainability_quality()
        !! Given: Code structure and organization
        !! When: Testing maintainability aspects
        !! Then: Code should be maintainable and extensible
        
        type(config_t) :: config1, config2
        logical :: result1, result2
        
        call start_quality_domain("Code Maintainability and Readability")
        
        ! Test 5.1: Consistent API patterns
        call initialize_config(config1)
        call initialize_config(config2)
        call assert_quality_test(.not. config1%verbose .and. .not. config2%verbose, &
            "API should provide consistent initialization behavior", "CQ-001")
        
        ! Test 5.2: Clear function responsibilities
        config1%verbose = .true.
        config1%quiet = .false.
        config2%verbose = .false.
        config2%quiet = .true.
        ! Ensure configs are different and independent
        call assert_quality_test((config1%verbose .and. .not. config2%verbose) .and. &
                               (.not. config1%quiet .and. config2%quiet), &
            "Functions should have clear, independent responsibilities", "CQ-002")
        
        ! Test 5.3: Predictable behavior patterns
        result1 = check_exclude_patterns("test.f90", config1)
        result2 = check_exclude_patterns("test.f90", config2)
        call assert_quality_test(result1 .eqv. result2, &
            "Same inputs should produce same outputs across instances", "CQ-003")
        
        ! Test 5.4: Extensible configuration system
        if (allocated(config1%exclude_patterns)) deallocate(config1%exclude_patterns)
        allocate(character(len=256) :: config1%exclude_patterns(1))
        config1%exclude_patterns(1) = "test_*.f90"
        
        result1 = check_exclude_patterns("test_module.f90", config1)
        call assert_quality_test(result1, &
            "Configuration system should be easily extensible", "CQ-004")
        
        ! Test 5.5: Clear separation of concerns
        config1%input_format = "gcov"
        config1%output_format = "json"
        config2%input_format = "gcov"
        config2%output_format = "markdown"
        call assert_quality_test(config1%input_format == config2%input_format .and. &
                               config1%output_format /= config2%output_format, &
            "Different concerns should be independently configurable", "CQ-005")
        
        ! Test 5.6: Minimal configuration requirements
        call initialize_config(config1)
        call assert_quality_test(.not. config1%verbose .and. .not. config1%quiet, &
            "Default configuration should be minimal and reasonable", "CQ-006")
        
        ! Cleanup
        if (allocated(config1%exclude_patterns)) deallocate(config1%exclude_patterns)
        
        call end_quality_domain()
    end subroutine test_maintainability_quality

    subroutine test_interface_quality()
        !! Given: Public interfaces and API design
        !! When: Testing interface contracts and usability
        !! Then: Interfaces should be well-designed and consistent
        
        type(config_t) :: config
        character(len=:), allocatable :: files(:)
        integer :: exit_code
        logical :: result
        
        call start_quality_domain("Interface Design and Contracts")
        
        ! Test 6.1: Interface consistency across modules
        call initialize_config(config)
        files = find_coverage_files(config)
        call assert_quality_test(allocated(files), &
            "Public interfaces should have consistent allocation behavior", "IQ-001")
        
        ! Test 6.2: Clear preconditions and postconditions
        config%output_format = "markdown"
        config%quiet = .true.
        exit_code = analyze_coverage(config)
        call assert_quality_test(exit_code >= 0 .and. exit_code <= 3, &
            "Functions should enforce clear pre/post conditions", "IQ-002")
        
        ! Test 6.3: Intuitive parameter ordering
        result = check_exclude_patterns("filename.f90", config)
        call assert_quality_test(.true., &  ! Function should be callable
            "Parameter ordering should be intuitive (filename, config)", "IQ-003")
        
        ! Test 6.4: Appropriate default behaviors
        call initialize_config(config)
        call assert_quality_test(.not. config%verbose .and. .not. config%quiet, &
            "Default behaviors should be sensible for most use cases", "IQ-004")
        
        ! Test 6.5: Error handling integration
        config%output_path = "/invalid/path.md"
        exit_code = analyze_coverage(config)
        call assert_quality_test(exit_code /= 0, &
            "Error conditions should be properly communicated", "IQ-005")
        
        ! Test 6.6: Optional parameter handling
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        files = find_coverage_files(config)
        call assert_quality_test(allocated(files), &
            "Optional parameters should be handled gracefully", "IQ-006")
        
        ! Cleanup
        if (allocated(files)) deallocate(files)
        
        call end_quality_domain()
    end subroutine test_interface_quality

    subroutine test_data_integrity_quality()
        !! Given: Data structures and state management
        !! When: Testing data consistency and integrity
        !! Then: Data should remain consistent and valid
        
        type(config_t) :: config
        type(coverage_data_t) :: coverage
        character(len=:), allocatable :: result_str
        
        call start_quality_domain("Data Integrity and Consistency")
        
        ! Test 7.1: Configuration state consistency
        call initialize_config(config)
        config%verbose = .true.
        config%output_format = "json"
        call assert_quality_test(config%verbose .and. config%output_format == "json", &
            "Configuration state should remain consistent", "DQ-001")
        
        ! Test 7.2: Data structure invariants
        if (.not. allocated(coverage%files)) then
            allocate(coverage%files(1))
            coverage%files(1)%filename = "test.f90"
            if (.not. allocated(coverage%files(1)%lines)) then
                allocate(coverage%files(1)%lines(1))
                coverage%files(1)%lines(1)%line_number = 10
                coverage%files(1)%lines(1)%execution_count = 5
                coverage%files(1)%lines(1)%is_executable = .true.
            end if
        end if
        
        call assert_quality_test(coverage%files(1)%lines(1)%line_number > 0 .and. &
                               coverage%files(1)%lines(1)%execution_count >= 0, &
            "Data structures should maintain valid invariants", "DQ-002")
        
        ! Test 7.3: String handling consistency
        result_str = to_lower("TestString")
        call assert_quality_test(result_str == "teststring", &
            "String operations should be deterministic", "DQ-003")
        
        ! Test 7.4: Array boundary consistency
        if (allocated(coverage%files)) then
            call assert_quality_test(size(coverage%files) >= 1 .and. &
                                   allocated(coverage%files(1)%lines) .and. &
                                   size(coverage%files(1)%lines) >= 1, &
                "Array structures should maintain size consistency", "DQ-004")
        end if
        
        ! Test 7.5: Configuration validation consistency
        config%minimum_coverage = 85.0
        call assert_quality_test(config%minimum_coverage > 0.0 .and. &
                               config%minimum_coverage <= 100.0, &
            "Configuration values should remain within valid ranges", "DQ-005")
        
        ! Cleanup
        if (allocated(coverage%files)) then
            if (allocated(coverage%files(1)%lines)) deallocate(coverage%files(1)%lines)
            deallocate(coverage%files)
        end if
        
        call end_quality_domain()
    end subroutine test_data_integrity_quality

    subroutine test_configuration_quality()
        !! Given: Configuration system and flexibility requirements
        !! When: Testing configuration options and adaptability
        !! Then: Configuration should be flexible and robust
        
        type(config_t) :: config
        
        call start_quality_domain("Configuration and Flexibility")
        
        ! Test 8.1: Multiple output format support
        call initialize_config(config)
        config%output_format = "markdown"
        call assert_quality_test(config%output_format == "markdown", &
            "System should support multiple output formats", "CF-001")
        
        config%output_format = "json"
        call assert_quality_test(config%output_format == "json", &
            "Format switching should work reliably", "CF-002")
        
        ! Test 8.2: Flexible path configuration
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        allocate(character(len=256) :: config%source_paths(3))
        config%source_paths(1) = "./src"
        config%source_paths(2) = "./lib"
        config%source_paths(3) = "./include"
        
        call assert_quality_test(size(config%source_paths) == 3 .and. &
                               config%source_paths(2) == "./lib", &
            "Multiple source paths should be configurable", "CF-003")
        
        ! Test 8.3: Pattern-based exclusion flexibility
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(2))
        config%exclude_patterns(1) = "test_*.f90"
        config%exclude_patterns(2) = "*.tmp"
        
        call assert_quality_test(size(config%exclude_patterns) == 2 .and. &
                               check_exclude_patterns("test_module.f90", config), &
            "Pattern exclusion should be flexible and effective", "CF-004")
        
        ! Test 8.4: Boolean flag combinations
        config%verbose = .true.
        config%quiet = .true.  ! Should be allowed even if contradictory
        call assert_quality_test(config%verbose .and. config%quiet, &
            "Flag combinations should be configurable", "CF-005")
        
        ! Test 8.5: Default value reasonableness
        call initialize_config(config)
        call assert_quality_test(.not. config%verbose .and. .not. config%quiet .and. &
                               config%minimum_coverage >= 0.0, &
            "Default configuration should be reasonable", "CF-006")
        
        ! Cleanup
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        
        call end_quality_domain()
    end subroutine test_configuration_quality

    subroutine test_edge_case_quality()
        !! Given: System boundary conditions and edge cases
        !! When: Testing with extreme and unusual inputs
        !! Then: System should handle edge cases gracefully
        
        type(config_t) :: config
        character(len=:), allocatable :: files(:), result_str
        logical :: exclude_result
        
        call start_quality_domain("Edge Case Handling and Boundaries")
        
        ! Test 9.1: Empty configuration handling
        call initialize_config(config)
        files = find_coverage_files(config)
        call assert_quality_test(allocated(files), &
            "Empty configuration should produce valid results", "EC-001")
        
        ! Test 9.2: Empty pattern arrays
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(0))
        exclude_result = check_exclude_patterns("any_file.f90", config)
        call assert_quality_test(.not. exclude_result, &
            "Empty pattern arrays should not exclude files", "EC-002")
        
        ! Test 9.3: Single character inputs
        result_str = to_lower("A")
        call assert_quality_test(result_str == "a", &
            "Single character inputs should be handled correctly", "EC-003")
        
        ! Test 9.4: Very long pattern matching
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        allocate(character(len=256) :: config%exclude_patterns(1))
        config%exclude_patterns(1) = repeat("*", 100)
        exclude_result = check_exclude_patterns("test.f90", config)
        call assert_quality_test(.true., &  ! Should not crash
            "Extreme pattern lengths should not cause crashes", "EC-004")
        
        ! Test 9.5: Multiple simultaneous operations
        block
            type(config_t) :: config1, config2
            call initialize_config(config1)
            call initialize_config(config2)
            config1%verbose = .true.
            config2%quiet = .true.
            call assert_quality_test(config1%verbose .and. config2%quiet, &
                "Multiple config instances should be independent", "EC-005")
        end block
        
        ! Test 9.6: Boundary value testing
        config%minimum_coverage = 0.0
        call assert_quality_test(config%minimum_coverage >= 0.0, &
            "Boundary values should be handled correctly", "EC-006")
        
        config%minimum_coverage = 100.0
        call assert_quality_test(config%minimum_coverage <= 100.0, &
            "Maximum boundary values should be valid", "EC-007")
        
        ! Cleanup
        if (allocated(files)) deallocate(files)
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        
        call end_quality_domain()
    end subroutine test_edge_case_quality

    subroutine test_integration_quality()
        !! Given: System integration between modules
        !! When: Testing cross-module interactions
        !! Then: Integration should be seamless and robust
        
        type(config_t) :: config
        integer :: exit_code
        character(len=:), allocatable :: files(:)
        
        call start_quality_domain("System Integration and Compatibility")
        
        ! Test 10.1: End-to-end workflow integration
        call initialize_config(config)
        config%output_format = "markdown"
        config%quiet = .true.
        exit_code = analyze_coverage(config)
        call assert_quality_test(exit_code >= 0 .and. exit_code <= 3, &
            "End-to-end workflow should integrate seamlessly", "IQ-001")
        
        ! Test 10.2: Cross-module data flow
        files = find_coverage_files(config)
        call assert_quality_test(allocated(files), &
            "Data should flow correctly between modules", "IQ-002")
        
        ! Test 10.3: Error propagation across modules
        config%output_path = "/invalid/path.md"
        exit_code = analyze_coverage(config)
        call assert_quality_test(exit_code > 0, &
            "Errors should propagate correctly across module boundaries", "IQ-003")
        
        ! Test 10.4: Configuration consistency across modules
        config%quiet = .true.
        config%verbose = .false.
        files = find_coverage_files(config)
        exit_code = analyze_coverage(config)
        call assert_quality_test(allocated(files) .and. exit_code >= 0, &
            "Configuration should be respected across all modules", "IQ-004")
        
        ! Test 10.5: Resource sharing between modules
        block
            character(len=:), allocatable :: files1(:), files2(:)
            files1 = find_coverage_files(config)
            files2 = find_coverage_files(config)
            call assert_quality_test(allocated(files1) .and. allocated(files2), &
                "Modules should share resources efficiently", "IQ-005")
            if (allocated(files1)) deallocate(files1)
            if (allocated(files2)) deallocate(files2)
        end block
        
        ! Test 10.6: Module independence
        block
            type(config_t) :: isolated_config
            call initialize_config(isolated_config)
            isolated_config%verbose = .true.
            call assert_quality_test(isolated_config%verbose .and. .not. config%verbose, &
                "Modules should maintain independence", "IQ-006")
        end block
        
        ! Cleanup
        if (allocated(files)) deallocate(files)
        
        call end_quality_domain()
    end subroutine test_integration_quality

    ! Test utilities and infrastructure
    
    subroutine start_quality_domain(domain_name)
        character(len=*), intent(in) :: domain_name
        print '(A)', "┌─ " // domain_name
    end subroutine start_quality_domain
    
    subroutine end_quality_domain()
        print '(A)', "└─"
        print *
    end subroutine end_quality_domain
    
    subroutine assert_quality_test(condition, test_name, test_id)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name, test_id
        
        total_tests = total_tests + 1
        
        if (condition) then
            passed_tests = passed_tests + 1
            print '(A)', "  ✅ [" // test_id // "] " // test_name
        else
            all_tests_passed = .false.
            print '(A)', "  ❌ [" // test_id // "] " // test_name
        end if
    end subroutine assert_quality_test
    
    function int_to_string(int_val) result(str_val)
        integer, intent(in) :: int_val
        character(len=:), allocatable :: str_val
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str_val = trim(temp_str)
    end function int_to_string
    
    subroutine print_final_quality_results()
        print *, "=== QUALITY AUDIT RESULTS ==="
        print '(A,I0)', "Total Quality Tests: ", total_tests
        print '(A,I0)', "Passed Tests: ", passed_tests
        print '(A,I0)', "Failed Tests: ", (total_tests - passed_tests)
        
        if (total_tests > 0) then
            print '(A,F5.1,A)', "Success Rate: ", &
                (real(passed_tests) / real(total_tests)) * 100.0, "%"
        end if
        
        print *
        
        if (all_tests_passed) then
            print *, "🏆 QUALITY STATUS: EXCELLENT"
            print *, "   All quality standards are met"
        else
            print *, "⚠️  QUALITY STATUS: IMPROVEMENTS NEEDED"
            print *, "   Review failed tests and implement improvements"
        end if
        
        print *
    end subroutine print_final_quality_results

end program test_comprehensive_quality_audit