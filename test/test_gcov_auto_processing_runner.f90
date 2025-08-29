program test_gcov_auto_processing_runner
    !! Test runner for GCov auto-processing functionality
    !! Executes comprehensive tests for the gcov auto-processor module
    
    use test_framework_utilities, only: test_counter_t, init_test_counter, &
                                       print_test_summary
    use test_gcov_auto_processing, only: test_gcov_file_processing, &
                                        test_gcov_error_handling, &
                                        test_gcov_directory_validation, &
                                        test_gcov_bulk_processing, &
                                        test_gcov_no_files_scenario, &
                                        test_gcov_result_statistics
    implicit none
    
    type(test_counter_t) :: counter
    
    print *, "=============================================="
    print *, "GCov Auto-Processing Test Suite"
    print *, "=============================================="
    print *, ""
    
    ! Initialize test counter
    call init_test_counter(counter)
    
    ! Run all gcov auto-processing tests
    call test_gcov_file_processing(counter)
    call test_gcov_error_handling(counter)
    call test_gcov_directory_validation(counter)
    call test_gcov_bulk_processing(counter)
    call test_gcov_no_files_scenario(counter)
    call test_gcov_result_statistics(counter)
    
    ! Print final test summary
    call print_test_summary(counter, "GCov Auto-Processing")
    
    ! Exit with appropriate code
    if (counter%failed > 0) then
        stop 1
    else
        stop 0
    end if
    
end program test_gcov_auto_processing_runner