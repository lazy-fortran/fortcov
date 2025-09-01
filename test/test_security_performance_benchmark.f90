program test_security_performance_benchmark
    !! Performance benchmark for security assessment optimizations (Issue #312)
    !!
    !! GIVEN: Optimized security assessment and path validation functions
    !! WHEN: Processing large numbers of patterns and path validations
    !! THEN: Performance should be significantly improved with caching and early exits
    !!
    !! This test measures:
    !! 1. Pattern matching performance improvements
    !! 2. Path validation caching effectiveness
    !! 3. Early exit logic benefits
    !! 4. Overall security assessment throughput
    use security_assessment_core, only: assess_pattern_security_risks, &
                                   assess_deletion_security_risks, &
                                   check_file_location
    use path_security, only: validate_path_security
    use file_ops_secure, only: safe_find_files
    use error_handling_core
    implicit none
    
    integer :: i, iterations
    real :: start_time, end_time, total_time
    integer :: test_count = 0
    integer :: passed_count = 0
    
    print *, "=================================================================="
    print *, "PERFORMANCE: Security Assessment Benchmark (Issue #312)"
    print *, "=================================================================="
    print *, ""
    print *, "Benchmarking optimized security assessment functions:"
    print *, "- Pattern matching with caching"
    print *, "- Path validation with early exits"
    print *, "- Consolidated pattern scanning"
    print *, ""
    
    ! Run performance benchmarks
    call benchmark_pattern_assessment_performance()
    call benchmark_path_validation_caching()
    call benchmark_file_location_checks()
    call benchmark_repeated_pattern_processing()
    
    ! Report results
    print *, ""
    print *, "=================================================================="
    print *, "Performance Benchmark Results"
    print *, "=================================================================="
    write(*, '(A, I0, A, I0, A, I0)') "Benchmarks run: ", test_count, &
        ", Performance targets met: ", passed_count, "/", test_count
    
    if (passed_count == test_count) then
        print *, ""
        print *, "✅ ALL PERFORMANCE TARGETS MET"
        print *, "   Security assessment optimizations successful"
        print *, ""
    else
        print *, ""
        print *, "⚠️  SOME PERFORMANCE TARGETS MISSED"
        print *, "   Review optimization implementation"
        print *, ""
    end if
    
contains

    subroutine benchmark_pattern_assessment_performance()
        !! Benchmark pattern security assessment with common patterns
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: test_patterns(8) = [ &
            '*.f90          ', &
            '/usr/local/*.so', &
            '**/*.f90       ', &
            '/proc/meminfo  ', &
            'ssh_config     ', &
            '/home/user/*.sh', &
            'temp_files     ', &
            'nonexistent    ' ]
        real :: baseline_threshold
        
        call start_benchmark("Pattern Assessment Performance")
        
        ! Always use fast iteration counts to keep tests quick
        iterations = 1000
        baseline_threshold = 2.0
        
        call cpu_time(start_time)
        
        do i = 1, iterations
            ! Test pattern assessment with different patterns
            call clear_error_context(error_ctx)
            call assess_pattern_security_risks(test_patterns(mod(i-1, 8) + 1), error_ctx)
        end do
        
        call cpu_time(end_time)
        total_time = end_time - start_time
        
        write(*, '(A, F8.4, A, I0, A)') "   Time: ", total_time, "s for ", &
            iterations, " pattern assessments"
        write(*, '(A, F8.2, A)') "   Rate: ", real(iterations) / total_time, " ops/sec"
        
        if (total_time < baseline_threshold) then
            call pass_benchmark("Pattern assessment performance excellent")
        else
            call fail_benchmark("Pattern assessment performance below target")
        end if
    end subroutine benchmark_pattern_assessment_performance

    subroutine benchmark_path_validation_caching()
        !! Benchmark path validation with repeated paths (cache effectiveness)
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: test_paths(4) = [ &
            '/valid/path/file.f90', &
            'relative/path.txt   ', &
            '/another/valid/path ', &
            'simple_filename     ' ]
        real :: cache_threshold
        
        call start_benchmark("Path Validation Caching")
        
        ! Always use fast iteration counts to keep tests quick
        iterations = 500
        cache_threshold = 0.2
        
        call cpu_time(start_time)
        
        do i = 1, iterations
            ! Repeatedly validate same paths to test cache effectiveness
            call validate_path_security(test_paths(mod(i-1, 4) + 1), safe_path, error_ctx)
        end do
        
        call cpu_time(end_time)
        total_time = end_time - start_time
        
        write(*, '(A, F8.4, A, I0, A)') "   Time: ", total_time, "s for ", &
            iterations, " cached validations"
        write(*, '(A, F8.2, A)') "   Rate: ", real(iterations) / total_time, " ops/sec"
        
        if (total_time < cache_threshold) then
            call pass_benchmark("Path validation caching highly effective")
        else
            call fail_benchmark("Path validation caching needs improvement")
        end if
    end subroutine benchmark_path_validation_caching

    subroutine benchmark_file_location_checks()
        !! Benchmark optimized file location checking with early exits
        logical :: is_temp, is_readonly
        character(len=*), parameter :: test_files(6) = [ &
            '/tmp/tempfile.txt      ', &
            '/usr/lib/library.so    ', &
            'fortcov_secure_temp.f90', &
            '/proc/cpuinfo          ', &
            'regular_file.txt       ', &
            'temp_backup.bak        ' ]
        real :: location_threshold
        
        call start_benchmark("File Location Checks (Early Exit)")
        
        ! Always use fast iteration counts to keep tests quick
        iterations = 2000
        location_threshold = 0.1
        
        call cpu_time(start_time)
        
        do i = 1, iterations
            ! Test file location checking - should be very fast with early exits
            call check_file_location(test_files(mod(i-1, 6) + 1), is_temp, is_readonly)
        end do
        
        call cpu_time(end_time)
        total_time = end_time - start_time
        
        write(*, '(A, F8.4, A, I0, A)') "   Time: ", total_time, "s for ", &
            iterations, " location checks"
        write(*, '(A, F8.2, A)') "   Rate: ", real(iterations) / total_time, " ops/sec"
        
        if (total_time < location_threshold) then
            call pass_benchmark("File location checks optimized successfully")
        else
            call fail_benchmark("File location checks need further optimization")
        end if
        
    end subroutine benchmark_file_location_checks

    subroutine benchmark_repeated_pattern_processing()
        !! Benchmark real-world scenario with repeated pattern processing
        character(len=:), allocatable :: files(:)
        type(error_context_t) :: error_ctx
        character(len=*), parameter :: common_patterns(3) = [ &
            '*.f90   ', &
            '**/*.so ', &
            '*.txt   ' ]
        real :: scenario_threshold
        
        call start_benchmark("Repeated Pattern Processing Scenario")
        
        ! Always use fast iteration counts to keep tests quick
        iterations = 100
        scenario_threshold = 1.0
        
        call cpu_time(start_time)
        
        do i = 1, iterations
            ! Simulate realistic security assessment workload
            call clear_error_context(error_ctx)
            call safe_find_files(common_patterns(mod(i-1, 3) + 1), files, error_ctx)
            ! Note: This will trigger security assessment and path validation
        end do
        
        call cpu_time(end_time)
        total_time = end_time - start_time
        
        write(*, '(A, F8.4, A, I0, A)') "   Time: ", total_time, "s for ", &
            iterations, " realistic operations"
        write(*, '(A, F8.2, A)') "   Rate: ", real(iterations) / total_time, " ops/sec"
        
        if (total_time < scenario_threshold) then
            call pass_benchmark("Realistic workload performance excellent")
        else
            call fail_benchmark("Realistic workload performance needs improvement")
        end if
    end subroutine benchmark_repeated_pattern_processing

    subroutine start_benchmark(benchmark_name)
        character(len=*), intent(in) :: benchmark_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') "Benchmark ", test_count, ": ", benchmark_name
    end subroutine start_benchmark
    
    subroutine pass_benchmark(message)
        character(len=*), intent(in) :: message
        passed_count = passed_count + 1
        print *, "   ✅ PASS: " // trim(message)
        print *, ""
    end subroutine pass_benchmark
    
    subroutine fail_benchmark(message)
        character(len=*), intent(in) :: message
        print *, "   ⚠️  FAIL: " // trim(message)
        print *, ""
    end subroutine fail_benchmark

    logical function is_ci_environment()
        !! Detect if running in CI environment for performance adjustments
        character(len=100) :: env_value
        integer :: status
        
        ! Check common CI environment variables
        call get_environment_variable("CI", env_value, status)
        ! Status 0 = success, status 4 = variable exists but length truncated
        if ((status == 0 .or. status == 4) .and. (trim(env_value) == "true" .or. trim(env_value) == "1")) then
            is_ci_environment = .true.
            return
        end if
        
        call get_environment_variable("GITHUB_ACTIONS", env_value, status)
        if ((status == 0 .or. status == 4) .and. trim(env_value) == "true") then
            is_ci_environment = .true.
            return
        end if
        
        ! Default to local environment
        is_ci_environment = .false.
    end function is_ci_environment

end program test_security_performance_benchmark
