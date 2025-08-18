! Demo application for Makefile integration testing
program demo_app
    use demo_utils
    implicit none
    
    character(len=100) :: test_string
    integer :: length, words
    logical :: palindrome
    
    write(*,*) "Makefile Demo String Utilities"
    write(*,*) "============================="
    
    ! Test string length
    test_string = "Hello World"
    length = string_length(test_string)
    write(*,'(A,A,A,I0)') "Length of '", trim(test_string), "': ", length
    
    ! Test reverse string
    write(*,'(A,A,A,A)') "Reverse of '", trim(test_string), "': '", trim(reverse_string(test_string)), "'"
    
    ! Test palindrome
    test_string = "racecar"
    palindrome = is_palindrome(test_string)
    if (palindrome) then
        write(*,'(A,A,A)') "'", trim(test_string), "' is a palindrome"
    else
        write(*,'(A,A,A)') "'", trim(test_string), "' is not a palindrome"
    end if
    
    test_string = "hello"
    palindrome = is_palindrome(test_string)
    if (palindrome) then
        write(*,'(A,A,A)') "'", trim(test_string), "' is a palindrome"
    else
        write(*,'(A,A,A)') "'", trim(test_string), "' is not a palindrome"
    end if
    
    ! Test word count
    test_string = "The quick brown fox"
    words = count_words(test_string)
    write(*,'(A,A,A,I0)') "Word count in '", trim(test_string), "': ", words

end program demo_app