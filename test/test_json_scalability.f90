program test_json_scalability
    use coverage_model
    use coverage_reporter, only: json_reporter_t
    implicit none

    ! Test scalability with large datasets
    call test_large_dataset_performance()
    call test_memory_efficiency()
    call test_linear_scaling()
    
    write(*,*) "JSON scalability tests completed"

contains

    subroutine test_large_dataset_performance()
        type(json_reporter_t) :: reporter
        type(coverage_data_t) :: large_dataset
        logical :: error_flag
        integer :: start_time, end_time, count_rate
        real :: elapsed_time
        
        write(*,*) "Testing large dataset performance..."
        
        ! Create moderately large dataset (100 files, 1000 lines each = 100K lines)
        call create_large_dataset(large_dataset, 100, 1000)
        
        ! Measure performance
        call system_clock(start_time, count_rate)
        call reporter%generate_report(large_dataset, "test_large.json", &
                                     error_flag)
        call system_clock(end_time)
        
        ! Calculate elapsed time
        elapsed_time = real(end_time - start_time) / real(count_rate)
        
        ! Performance should be reasonable (< 5 seconds for 100K lines)
        if (elapsed_time > 5.0) then
            write(*,*) "FAIL: Large dataset took too long:", elapsed_time, "s"
            write(*,*) "Expected: < 5 seconds for 100K lines"
            stop 1
        end if
        
        if (error_flag) then
            write(*,*) "FAIL: Large dataset generation failed"
            stop 1
        end if
        
        write(*,*) "PASS: Large dataset processed in", elapsed_time, "seconds"
    end subroutine test_large_dataset_performance

    subroutine test_memory_efficiency()
        type(json_reporter_t) :: reporter
        type(coverage_data_t) :: dataset
        logical :: error_flag
        integer :: i
        
        write(*,*) "Testing memory efficiency with repeated calls..."
        
        ! Test multiple calls don't cause memory leaks
        do i = 1, 100
            call create_large_dataset(dataset, 10, 100)
            call reporter%generate_report(dataset, "test_memory.json", &
                                         error_flag)
            if (error_flag) then
                write(*,*) "FAIL: Memory efficiency test failed at iteration", i
                stop 1
            end if
        end do
        
        write(*,*) "PASS: Memory efficiency test completed"
    end subroutine test_memory_efficiency

    subroutine test_linear_scaling()
        type(json_reporter_t) :: reporter
        type(coverage_data_t) :: dataset
        logical :: error_flag
        integer :: start_time, end_time, count_rate
        real :: time_100, time_1000, scaling_factor
        
        write(*,*) "Testing linear scaling characteristics..."
        
        ! Test with 100 files, 100 lines each
        call create_large_dataset(dataset, 100, 100)
        call system_clock(start_time, count_rate)
        call reporter%generate_report(dataset, "test_100.json", error_flag)
        call system_clock(end_time)
        time_100 = real(end_time - start_time) / real(count_rate)
        
        if (error_flag) then
            write(*,*) "FAIL: Small dataset test failed"
            stop 1
        end if
        
        ! Test with 1000 files, 100 lines each (10x more data)
        call create_large_dataset(dataset, 1000, 100)
        call system_clock(start_time, count_rate)
        call reporter%generate_report(dataset, "test_1000.json", error_flag)
        call system_clock(end_time)
        time_1000 = real(end_time - start_time) / real(count_rate)
        
        if (error_flag) then
            write(*,*) "FAIL: Large dataset test failed"
            stop 1
        end if
        
        ! Calculate scaling factor
        scaling_factor = time_1000 / time_100
        
        ! Should scale approximately linearly (factor should be ~10)
        ! Allow some overhead, but factor > 50 indicates quadratic behavior
        if (scaling_factor > 50.0) then
            write(*,*) "FAIL: Poor scaling detected"
            write(*,*) "Time for 10K lines:", time_100, "s"
            write(*,*) "Time for 100K lines:", time_1000, "s" 
            write(*,*) "Scaling factor:", scaling_factor, " (expected ~10)"
            stop 1
        end if
        
        write(*,*) "PASS: Scaling factor:", scaling_factor
    end subroutine test_linear_scaling

    subroutine create_large_dataset(dataset, num_files, lines_per_file)
        type(coverage_data_t), intent(out) :: dataset
        integer, intent(in) :: num_files, lines_per_file
        integer :: i, j
        character(len=50) :: filename
        
        ! Allocate files array
        allocate(dataset%files(num_files))
        
        do i = 1, num_files
            write(filename, '("test_file_", I0, ".f90")') i
            dataset%files(i)%filename = trim(filename)
            
            ! Allocate lines for this file
            allocate(dataset%files(i)%lines(lines_per_file))
            
            do j = 1, lines_per_file
                dataset%files(i)%lines(j)%line_number = j
                dataset%files(i)%lines(j)%execution_count = mod(j, 10)
                dataset%files(i)%lines(j)%is_executable = (mod(j, 2) == 0)
            end do
            
            ! Initialize functions array to prevent issues
            allocate(dataset%files(i)%functions(0))
        end do
    end subroutine create_large_dataset

end program test_json_scalability