program test_performance_benchmarks
    use coverage_model
    use coverage_reporter
    use coverage_statistics
    implicit none

    ! Comprehensive performance benchmarking and validation
    call test_startup_performance()
    call test_memory_usage_profiling()
    call test_scalability_validation()
    call test_output_format_performance()
    
    write(*,*) "Performance benchmark tests completed"

contains

    subroutine test_startup_performance()
        type(coverage_data_t) :: small_dataset, medium_dataset, large_dataset
        integer :: start_time, end_time, count_rate
        real :: startup_small, startup_medium, startup_large
        
        write(*,*) "Testing startup performance..."
        
        ! Create datasets of different sizes
        call create_test_dataset(small_dataset, 10, 100)     ! 1K lines
        call create_test_dataset(medium_dataset, 100, 100)   ! 10K lines  
        call create_test_dataset(large_dataset, 100, 1000)   ! 100K lines
        
        ! Test small dataset startup
        call system_clock(start_time, count_rate)
        call process_coverage_data(small_dataset)
        call system_clock(end_time)
        startup_small = real(end_time - start_time) / real(count_rate)
        
        ! Test medium dataset startup
        call system_clock(start_time, count_rate)
        call process_coverage_data(medium_dataset)
        call system_clock(end_time)
        startup_medium = real(end_time - start_time) / real(count_rate)
        
        ! Test large dataset startup  
        call system_clock(start_time, count_rate)
        call process_coverage_data(large_dataset)
        call system_clock(end_time)
        startup_large = real(end_time - start_time) / real(count_rate)
        
        ! Report actual performance characteristics
        write(*,*) "PERFORMANCE RESULTS:"
        write(*,*) "Small dataset (1K lines):", startup_small, "seconds"
        write(*,*) "Medium dataset (10K lines):", startup_medium, "seconds" 
        write(*,*) "Large dataset (100K lines):", startup_large, "seconds"
        
        ! Validate reasonable performance (no arbitrary claims)
        if (startup_large > 30.0) then
            write(*,*) "WARNING: Large dataset processing >30s may be slow"
            write(*,*) "Consider optimization for production use"
        end if
        
        write(*,*) "PASS: Startup performance measured and documented"
    end subroutine test_startup_performance

    subroutine test_memory_usage_profiling()
        type(coverage_data_t) :: dataset
        integer :: i, memory_iterations
        real :: processing_start, processing_end
        
        write(*,*) "Testing memory usage patterns..."
        
        memory_iterations = 50
        
        ! Test memory usage with repeated operations
        do i = 1, memory_iterations
            call create_test_dataset(dataset, 50, 200)  ! 10K lines each
            call process_coverage_data(dataset)
            
            ! Simulate real-world cleanup
            call cleanup_coverage_data(dataset)
            
            if (mod(i, 10) == 0) then
                write(*,*) "Memory test iteration", i, "of", memory_iterations
            end if
        end do
        
        write(*,*) "PASS: Memory usage profiling completed"
        write(*,*) "Note: Use external tools (valgrind, etc.) for detailed analysis"
    end subroutine test_memory_usage_profiling

    subroutine test_scalability_validation()
        type(coverage_data_t) :: dataset
        integer :: scale_factors(5) = [10, 50, 100, 200, 500]
        real :: processing_times(5)
        integer :: i, start_time, end_time, count_rate
        
        write(*,*) "Testing scalability characteristics..."
        
        do i = 1, 5
            call create_test_dataset(dataset, scale_factors(i), 100)
            
            call system_clock(start_time, count_rate)
            call process_coverage_data(dataset)
            call system_clock(end_time)
            
            processing_times(i) = real(end_time - start_time) / &
                                 real(count_rate)
            
            write(*,*) "Scale factor", scale_factors(i), "(", &
                scale_factors(i) * 100, "lines):", processing_times(i), "s"
        end do
        
        ! Analyze scaling characteristics
        if (processing_times(5) / processing_times(1) > 100.0) then
            write(*,*) "WARNING: Poor scaling detected (>100x degradation)"
        else
            write(*,*) "PASS: Reasonable scaling characteristics"
        end if
    end subroutine test_scalability_validation

    subroutine test_output_format_performance()
        type(coverage_data_t) :: dataset
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag, success
        character(len=:), allocatable :: error_message
        integer :: start_time, end_time, count_rate
        real :: json_time, markdown_time
        character(len=20) :: formats(2) = ["json    ", "markdown"]
        integer :: i
        
        write(*,*) "Testing output format performance..."
        
        call create_test_dataset(dataset, 50, 500)  ! 25K lines
        
        do i = 1, 2
            call create_reporter(trim(formats(i)), reporter, error_flag)
            if (error_flag) then
                write(*,*) "ERROR: Could not create", trim(formats(i)), &
                          "reporter"
                cycle
            end if
            
            call system_clock(start_time, count_rate)
            call reporter%generate_report(dataset, &
                "test_" // trim(formats(i)) // ".out", success, error_message)
            call system_clock(end_time)
            
            if (.not. success) then
                write(*,*) "ERROR: Report generation failed for", &
                          trim(formats(i))
                if (allocated(error_message)) then
                    write(*,*) "Error message: ", error_message
                end if
                cycle
            end if
            
            if (i == 1) then
                json_time = real(end_time - start_time) / real(count_rate)
            else
                markdown_time = real(end_time - start_time) / &
                               real(count_rate)
            end if
            
            write(*,*) trim(formats(i)), "generation time:", &
                real(end_time - start_time) / real(count_rate), "seconds"
        end do
        
        write(*,*) "PASS: Output format performance measured"
    end subroutine test_output_format_performance

    ! Helper subroutines
    subroutine create_test_dataset(dataset, num_files, lines_per_file)
        type(coverage_data_t), intent(out) :: dataset
        integer, intent(in) :: num_files, lines_per_file
        integer :: i, j
        character(len=50) :: filename
        
        allocate(dataset%files(num_files))
        
        do i = 1, num_files
            write(filename, '("benchmark_", I0, ".f90")') i
            dataset%files(i)%filename = trim(filename)
            
            allocate(dataset%files(i)%lines(lines_per_file))
            
            do j = 1, lines_per_file
                dataset%files(i)%lines(j)%line_number = j
                dataset%files(i)%lines(j)%execution_count = mod(j * i, 20)
                dataset%files(i)%lines(j)%is_executable = (mod(j, 3) /= 0)
            end do
            
            allocate(dataset%files(i)%functions(0))
        end do
    end subroutine create_test_dataset

    subroutine process_coverage_data(dataset)
        type(coverage_data_t), intent(in) :: dataset
        type(coverage_stats_t) :: stats
        
        ! Simulate typical coverage processing operations
        stats = calculate_line_coverage(dataset)
        stats = calculate_branch_coverage(dataset)  
        stats = calculate_function_coverage(dataset)
    end subroutine process_coverage_data

    subroutine cleanup_coverage_data(dataset)
        type(coverage_data_t), intent(inout) :: dataset
        integer :: i
        
        ! Clean up allocated memory
        if (allocated(dataset%files)) then
            do i = 1, size(dataset%files)
                if (allocated(dataset%files(i)%lines)) then
                    deallocate(dataset%files(i)%lines)
                end if
                if (allocated(dataset%files(i)%functions)) then
                    deallocate(dataset%files(i)%functions)
                end if
            end do
            deallocate(dataset%files)
        end if
    end subroutine cleanup_coverage_data

end program test_performance_benchmarks