! Demo application for Meson integration testing
program demo_app
    use demo_io
    implicit none
    
    real :: test_numbers(5) = [1.0, 2.0, 3.0, 4.0, 5.0]
    real :: read_numbers_array(10)
    integer :: count, i
    logical :: valid
    
    write(*,*) "Meson Demo I/O Application"
    write(*,*) "=========================="
    
    ! Create test input file
    call write_numbers('test_input.dat', test_numbers, 5)
    write(*,*) "Created test input file with 5 numbers"
    
    ! Read the file back
    call read_numbers('test_input.dat', read_numbers_array, count, 10)
    write(*,*) "Read", count, "numbers from file:"
    do i = 1, count
        write(*,'(A,I0,A,F8.3)') "  Number ", i, ": ", read_numbers_array(i)
    end do
    
    ! Process the file (square the numbers)
    call process_file('test_input.dat', 'test_output.dat')
    
    ! Read the processed file
    call read_numbers('test_output.dat', read_numbers_array, count, 10)
    write(*,*) "Processed numbers (squared):"
    do i = 1, count
        write(*,'(A,I0,A,F8.3)') "  Number ", i, ": ", read_numbers_array(i)
    end do
    
    ! Test validation
    write(*,*) "Testing number validation:"
    valid = validate_number(5.0, 0.0, 10.0)
    write(*,*) "5.0 in range [0.0, 10.0]:", valid
    
    valid = validate_number(15.0, 0.0, 10.0)
    write(*,*) "15.0 in range [0.0, 10.0]:", valid

end program demo_app