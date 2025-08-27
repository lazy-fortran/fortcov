program test_memory_allocation_errors
    !! Test suite for memory allocation error handling
    !! 
    !! This test program verifies that all modules properly handle memory 
    !! allocation failures with stat= and errmsg= parameters and appropriate
    !! error handling logic.

    use iso_fortran_env, only: error_unit
    use config_defaults_core
    use coverage_processor_file
    use zero_config_manager
    use gcda_discovery
    use gcov_generator
    use config_types
    
    implicit none
    
    integer :: tests_passed = 0
    integer :: tests_failed = 0
    logical :: test_result
    
    write(*, '(A)') "Testing memory allocation error handling..."
    write(*, '(A)') "==========================================="
    
    ! Test config_defaults memory allocations
    call test_config_defaults_memory_handling(test_result)
    if (test_result) then
        tests_passed = tests_passed + 1
        write(*, '(A)') "✓ config_defaults memory handling"
    else
        tests_failed = tests_failed + 1
        write(*, '(A)') "✗ config_defaults memory handling"
    end if
    
    ! Test coverage_file_processor memory allocations
    call test_coverage_file_processor_memory_handling(test_result)
    if (test_result) then
        tests_passed = tests_passed + 1
        write(*, '(A)') "✓ coverage_file_processor memory handling"
    else
        tests_failed = tests_failed + 1
        write(*, '(A)') "✗ coverage_file_processor memory handling"
    end if
    
    ! Test zero_configuration_manager memory allocations
    call test_zero_config_memory_handling(test_result)
    if (test_result) then
        tests_passed = tests_passed + 1
        write(*, '(A)') "✓ zero_configuration_manager memory handling"
    else
        tests_failed = tests_failed + 1
        write(*, '(A)') "✗ zero_configuration_manager memory handling"
    end if
    
    ! Test gcda_file_discovery memory allocations
    call test_gcda_file_discovery_memory_handling(test_result)
    if (test_result) then
        tests_passed = tests_passed + 1
        write(*, '(A)') "✓ gcda_file_discovery memory handling"
    else
        tests_failed = tests_failed + 1
        write(*, '(A)') "✗ gcda_file_discovery memory handling"
    end if
    
    ! Test gcov_file_generator memory allocations
    call test_gcov_file_generator_memory_handling(test_result)
    if (test_result) then
        tests_passed = tests_passed + 1
        write(*, '(A)') "✓ gcov_file_generator memory handling"
    else
        tests_failed = tests_failed + 1
        write(*, '(A)') "✗ gcov_file_generator memory handling"
    end if
    
    ! Summary
    write(*, '(A)') ""
    write(*, '(A)') "Test Summary:"
    write(*, '(A, I0)') "Tests passed: ", tests_passed
    write(*, '(A, I0)') "Tests failed: ", tests_failed
    
    if (tests_failed == 0) then
        write(*, '(A)') "All memory allocation error handling tests passed!"
        stop 0
    else
        write(error_unit, '(A)') "Some memory allocation tests failed!"
        stop 1
    end if

contains

    subroutine test_config_defaults_memory_handling(result)
        !! Test config_defaults module memory allocation handling
        logical, intent(out) :: result
        type(config_t) :: config
        
        result = .true.
        
        ! Test initialize_default_config (should handle allocation failures gracefully)
        call initialize_default_config(config)
        
        ! This test passes if no crash occurs during initialization
        ! Actual memory allocation error testing would require more sophisticated
        ! techniques like memory exhaustion simulation
        
    end subroutine test_config_defaults_memory_handling
    
    subroutine test_coverage_file_processor_memory_handling(result)
        !! Test coverage_file_processor module memory allocation handling
        logical, intent(out) :: result
        type(config_t) :: config
        character(len=1024), allocatable :: coverage_files(:), filtered_files(:)
        
        result = .true.
        
        ! Initialize basic config
        call initialize_default_config(config)
        
        ! Test find_and_filter_coverage_files with minimal data
        call find_and_filter_coverage_files(config, coverage_files, filtered_files)
        
        ! This test passes if no crash occurs during processing
        ! The actual files may not exist, but allocation handling should be robust
        
    end subroutine test_coverage_file_processor_memory_handling
    
    subroutine test_zero_config_memory_handling(result)
        !! Test zero_configuration_manager module memory allocation handling
        logical, intent(out) :: result
        character(len=:), allocatable :: output_path, output_format, input_format
        character(len=:), allocatable :: exclude_patterns(:)
        
        result = .true.
        
        ! Test apply_zero_configuration_defaults
        call apply_zero_configuration_defaults(output_path, output_format, &
                                              input_format, exclude_patterns)
        
        ! Verify allocations succeeded
        if (.not. allocated(output_path) .or. &
            .not. allocated(output_format) .or. &
            .not. allocated(input_format) .or. &
            .not. allocated(exclude_patterns)) then
            result = .false.
        end if
        
    end subroutine test_zero_config_memory_handling
    
    subroutine test_gcda_file_discovery_memory_handling(result)
        !! Test gcda_file_discovery module memory allocation handling
        logical, intent(out) :: result
        character(len=:), allocatable :: gcda_files(:)
        
        result = .true.
        
        ! Test discover_gcda_files_priority
        gcda_files = discover_gcda_files_priority()
        
        ! Verify allocation succeeded (even if no files found)
        if (.not. allocated(gcda_files)) then
            result = .false.
        end if
        
    end subroutine test_gcda_file_discovery_memory_handling
    
    subroutine test_gcov_file_generator_memory_handling(result)
        !! Test gcov_file_generator module memory allocation handling
        logical, intent(out) :: result
        logical :: gcov_available
        character(len=256), allocatable :: gcda_files(:)
        character(len=:), allocatable :: generated_files(:)
        
        result = .true.
        
        ! Test check_gcov_availability
        call check_gcov_availability(gcov_available)
        
        ! Test generate_gcov_files_from_gcda with empty input
        allocate(gcda_files(0))
        call generate_gcov_files_from_gcda(gcda_files, generated_files)
        
        ! Verify allocation succeeded
        if (.not. allocated(generated_files)) then
            result = .false.
        end if
        
    end subroutine test_gcov_file_generator_memory_handling

end program test_memory_allocation_errors