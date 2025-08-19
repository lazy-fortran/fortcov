! Test suite for Meson demo I/O module
program test_demo
    use demo_io
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "Running Meson Demo I/O Tests"
    write(*,*) "============================="
    
    ! Test validation function
    call test_validate_number()
    
    ! Test file I/O functions
    call test_file_operations()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "TESTS FAILED"
        stop 1
    else
        write(*,*) "All tests passed"
    end if

contains

    subroutine test_validate_number()
        call assert_logical_equal(validate_number(5.0, 0.0, 10.0), .true., "validate_number(5.0, 0.0, 10.0)")
        call assert_logical_equal(validate_number(-1.0, 0.0, 10.0), .false., "validate_number(-1.0, 0.0, 10.0)")
        call assert_logical_equal(validate_number(15.0, 0.0, 10.0), .false., "validate_number(15.0, 0.0, 10.0)")
        call assert_logical_equal(validate_number(0.0, 0.0, 10.0), .true., "validate_number(0.0, 0.0, 10.0)")
        call assert_logical_equal(validate_number(10.0, 0.0, 10.0), .true., "validate_number(10.0, 0.0, 10.0)")
    end subroutine test_validate_number

    subroutine test_file_operations()
        real :: test_data(3) = [1.5, 2.5, 3.5]
        real :: read_data(10)
        integer :: count
        logical :: file_exists
        
        ! Test write and read
        call write_numbers('test_temp.dat', test_data, 3)
        
        ! Check if file was created
        inquire(file='test_temp.dat', exist=file_exists)
        call assert_logical_equal(file_exists, .true., "file creation test")
        
        if (file_exists) then
            call read_numbers('test_temp.dat', read_data, count, 10)
            call assert_int_equal(count, 3, "read correct number of values")
            
            if (count >= 3) then
                call assert_real_equal(read_data(1), 1.5, "read first value")
                call assert_real_equal(read_data(2), 2.5, "read second value")  
                call assert_real_equal(read_data(3), 3.5, "read third value")
            end if
        end if
        
        ! Clean up test file
        open(unit=99, file='test_temp.dat', status='old')
        close(99, status='delete')
    end subroutine test_file_operations

    subroutine assert_logical_equal(actual, expected, test_name)
        logical, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (actual .eqv. expected) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,L1,A,L1)') "✗ ", test_name, " FAILED (expected ", expected, ", got ", actual, ")"
            failed_count = failed_count + 1
        end if
    end subroutine assert_logical_equal

    subroutine assert_int_equal(actual, expected, test_name)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (actual == expected) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,I0,A,I0)') "✗ ", test_name, " FAILED (expected ", expected, ", got ", actual, ")"
            failed_count = failed_count + 1
        end if
    end subroutine assert_int_equal

    subroutine assert_real_equal(actual, expected, test_name)
        real, intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (abs(actual - expected) < 1.0e-6) then
            write(*,'(A,A,A)') "✓ ", test_name, " PASSED"
            passed_count = passed_count + 1
        else
            write(*,'(A,A,A,F0.6,A,F0.6)') "✗ ", test_name, " FAILED (expected ", expected, ", got ", actual, ")"
            failed_count = failed_count + 1
        end if
    end subroutine assert_real_equal

end program test_demo