! Demo utilities module for Makefile coverage testing
module demo_utils
    implicit none
    private
    
    public :: string_length, reverse_string, is_palindrome, count_words
    
contains

    pure function string_length(str) result(length)
        character(len=*), intent(in) :: str
        integer :: length
        
        length = len_trim(str)
    end function string_length

    function reverse_string(str) result(reversed)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: reversed
        integer :: i, n
        
        n = len_trim(str)
        do i = 1, n
            reversed(i:i) = str(n-i+1:n-i+1)
        end do
        
        ! Pad with spaces if needed
        if (n < len(str)) then
            reversed(n+1:) = ' '
        end if
    end function reverse_string

    function is_palindrome(str) result(palindrome)
        character(len=*), intent(in) :: str
        logical :: palindrome
        character(len=len(str)) :: clean_str, reversed
        integer :: i, j, n
        
        ! Remove spaces and convert to lowercase (simplified)
        n = 0
        do i = 1, len_trim(str)
            if (str(i:i) /= ' ') then
                n = n + 1
                clean_str(n:n) = str(i:i)
            end if
        end do
        
        ! Reverse the cleaned string
        do i = 1, n
            reversed(i:i) = clean_str(n-i+1:n-i+1)
        end do
        
        ! Compare
        palindrome = (clean_str(1:n) == reversed(1:n))
    end function is_palindrome

    function count_words(str) result(word_count)
        character(len=*), intent(in) :: str
        integer :: word_count
        integer :: i
        logical :: in_word
        
        word_count = 0
        in_word = .false.
        
        do i = 1, len_trim(str)
            if (str(i:i) /= ' ' .and. .not. in_word) then
                word_count = word_count + 1
                in_word = .true.
            else if (str(i:i) == ' ') then
                in_word = .false.
            end if
        end do
    end function count_words

end module demo_utils