program test_scaling_analysis
    use coverage_model
    use coverage_statistics
    implicit none

    ! Deep analysis of scaling characteristics to identify bottlenecks
    call test_algorithmic_complexity()
    call test_memory_allocation_patterns()
    call test_statistics_calculation_scaling()
    
    write(*,*) "Scaling analysis completed"

contains

    subroutine test_algorithmic_complexity()
        type(coverage_data_t) :: dataset
        integer :: sizes(8) = [10, 20, 50, 100, 200, 500, 1000, 2000]
        real :: times(8)
        integer :: i, start_time, end_time, count_rate
        real :: complexity_ratio
        
        write(*,*) "Analyzing algorithmic complexity..."
        
        do i = 1, 8
            call create_test_dataset(dataset, sizes(i), 50)  ! Fixed lines per file
            
            call system_clock(start_time, count_rate)
            call process_dataset_operations(dataset)
            call system_clock(end_time)
            
            times(i) = real(end_time - start_time) / real(count_rate)
            
            write(*,*) "Size", sizes(i), "files: ", times(i), "seconds"
            
            if (i > 1) then
                complexity_ratio = (times(i) / times(i-1)) / &
                                  (real(sizes(i)) / real(sizes(i-1)))
                
                if (complexity_ratio > 2.0) then
                    write(*,*) "WARNING: Non-linear scaling detected at size", &
                        sizes(i), "ratio:", complexity_ratio
                end if
            end if
            
            call cleanup_dataset(dataset)
        end do
        
        ! Analyze overall complexity
        if (times(8) / times(1) > (real(sizes(8)) / real(sizes(1))) * 5) then
            write(*,*) "CRITICAL: Algorithm appears to have worse than O(n) complexity"
            write(*,*) "Expected linear ratio:", real(sizes(8)) / real(sizes(1))
            write(*,*) "Actual time ratio:", times(8) / times(1)
        else
            write(*,*) "PASS: Scaling appears roughly linear"
        end if
    end subroutine test_algorithmic_complexity

    subroutine test_memory_allocation_patterns()
        type(coverage_data_t) :: dataset
        integer :: allocation_sizes(5) = [100, 500, 1000, 2000, 5000]
        integer :: i, start_time, end_time, count_rate
        real :: alloc_times(5)
        
        write(*,*) "Testing memory allocation patterns..."
        
        do i = 1, 5
            call system_clock(start_time, count_rate)
            call create_test_dataset(dataset, 50, allocation_sizes(i))
            call system_clock(end_time)
            
            alloc_times(i) = real(end_time - start_time) / real(count_rate)
            
            write(*,*) "Allocation for", allocation_sizes(i), &
                      "lines per file:", alloc_times(i), "s"
            
            call cleanup_dataset(dataset)
        end do
        
        if (alloc_times(5) > alloc_times(1) * 100) then
            write(*,*) "WARNING: Memory allocation scaling poorly"
        else
            write(*,*) "PASS: Memory allocation scaling reasonably"
        end if
    end subroutine test_memory_allocation_patterns

    subroutine test_statistics_calculation_scaling()
        type(coverage_data_t) :: dataset
        type(coverage_stats_t) :: stats
        integer :: file_counts(6) = [10, 50, 100, 200, 500, 1000]
        real :: calc_times(6)
        integer :: i, start_time, end_time, count_rate
        
        write(*,*) "Testing statistics calculation scaling..."
        
        do i = 1, 6
            call create_test_dataset(dataset, file_counts(i), 100)
            
            call system_clock(start_time, count_rate)
            
            ! Test all statistics calculations
            stats = calculate_line_coverage(dataset)
            stats = calculate_branch_coverage(dataset)
            stats = calculate_function_coverage(dataset)
            
            call system_clock(end_time)
            
            calc_times(i) = real(end_time - start_time) / real(count_rate)
            
            write(*,*) "Statistics for", file_counts(i), "files:", &
                      calc_times(i), "s"
            
            call cleanup_dataset(dataset)
        end do
        
        ! Check if statistics calculation is the bottleneck
        if (calc_times(6) / calc_times(1) > 20.0) then
            write(*,*) "IDENTIFIED: Statistics calculation may be bottleneck"
            write(*,*) "Ratio:", calc_times(6) / calc_times(1)
        else
            write(*,*) "PASS: Statistics calculation scales well"
        end if
    end subroutine test_statistics_calculation_scaling

    subroutine process_dataset_operations(dataset)
        type(coverage_data_t), intent(in) :: dataset
        type(coverage_stats_t) :: stats
        integer :: i, j, line_count
        
        ! Simulate typical operations that might cause scaling issues
        
        ! 1. Statistics calculations
        stats = calculate_line_coverage(dataset)
        stats = calculate_branch_coverage(dataset)
        stats = calculate_function_coverage(dataset)
        
        ! 2. Data traversal operations
        do i = 1, size(dataset%files)
            line_count = 0
            do j = 1, size(dataset%files(i)%lines)
                if (dataset%files(i)%lines(j)%is_executable) then
                    line_count = line_count + 1
                end if
            end do
        end do
    end subroutine process_dataset_operations

    subroutine create_test_dataset(dataset, num_files, lines_per_file)
        type(coverage_data_t), intent(out) :: dataset
        integer, intent(in) :: num_files, lines_per_file
        integer :: i, j
        character(len=50) :: filename
        
        allocate(dataset%files(num_files))
        
        do i = 1, num_files
            write(filename, '("scaling_test_", I0, ".f90")') i
            dataset%files(i)%filename = trim(filename)
            
            allocate(dataset%files(i)%lines(lines_per_file))
            
            do j = 1, lines_per_file
                dataset%files(i)%lines(j)%line_number = j
                dataset%files(i)%lines(j)%execution_count = mod(j * i, 50)
                dataset%files(i)%lines(j)%is_executable = (mod(j, 3) /= 0)
            end do
            
            allocate(dataset%files(i)%functions(0))
        end do
    end subroutine create_test_dataset

    subroutine cleanup_dataset(dataset)
        type(coverage_data_t), intent(inout) :: dataset
        integer :: i
        
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
    end subroutine cleanup_dataset

end program test_scaling_analysis