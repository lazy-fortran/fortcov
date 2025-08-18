! Demo application using the calculator module
program demo_app
    use demo_calculator
    implicit none
    
    real :: a, b, result
    
    ! Test basic operations
    a = 10.0
    b = 5.0
    
    write(*,*) "Demo Calculator Application"
    write(*,*) "=========================="
    
    ! Addition
    result = add_numbers(a, b)
    write(*,'(A,F0.1,A,F0.1,A,F0.1)') "Addition: ", a, " + ", b, " = ", result
    
    ! Multiplication  
    result = multiply_numbers(a, b)
    write(*,'(A,F0.1,A,F0.1,A,F0.1)') "Multiplication: ", a, " * ", b, " = ", result
    
    ! Division
    result = divide_numbers(a, b)
    write(*,'(A,F0.1,A,F0.1,A,F0.1)') "Division: ", a, " / ", b, " = ", result
    
    ! Test division by zero handling
    result = divide_numbers(a, 0.0)
    write(*,'(A,F0.1,A,F0.1,A,F0.1)') "Division by zero: ", a, " / 0.0 = ", result
    
end program demo_app