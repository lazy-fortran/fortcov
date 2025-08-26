module string_utils
    implicit none
    private
    
    ! Public procedures
    public :: compress_ranges
    public :: format_percentage
    public :: split
    public :: trim_string
    public :: to_lower
    public :: matches_pattern
    public :: check_exclude_patterns_list
    ! String conversion functions (merged from string_utilities)
    public :: int_to_str
    public :: real_to_str
    public :: int_to_string
    public :: real_to_string
    public :: int64_to_string
    
contains

    ! Compress consecutive line numbers into ranges (1,2,3,5,6 -> "1-3,5-6")
    ! OPTIMIZED: Uses pre-allocated buffer to avoid O(n²) string concatenation
    function compress_ranges(numbers) result(compressed)
        integer, intent(in) :: numbers(:)
        character(len=:), allocatable :: compressed
        
        ! Use buffered approach for O(n) performance
        integer, parameter :: BUFFER_SIZE = 4096
        character(len=BUFFER_SIZE) :: buffer
        integer :: buffer_pos, i, start_range, end_range, range_count
        logical :: in_range, first_item
        character(len=20) :: temp_str
        
        if (size(numbers) == 0) then
            compressed = ""
            return
        end if
        
        ! Initialize buffer
        buffer = ""
        buffer_pos = 1
        i = 1
        first_item = .true.
        range_count = 0
        
        do while (i <= size(numbers))
            start_range = numbers(i)
            end_range = start_range
            in_range = .false.
            
            ! Find consecutive numbers
            do while (i < size(numbers))
                if (numbers(i + 1) == numbers(i) + 1) then
                    end_range = numbers(i + 1)
                    in_range = .true.
                    i = i + 1
                else
                    exit
                end if
            end do
            
            ! Add separator if not first item
            if (.not. first_item) then
                if (buffer_pos + 2 <= BUFFER_SIZE) then
                    buffer(buffer_pos:buffer_pos+1) = ", "
                    buffer_pos = buffer_pos + 2
                end if
            end if
            first_item = .false.
            
            ! Add range or single number to buffer
            if (in_range) then
                write(temp_str, '(I0,"-",I0)') start_range, end_range
            else
                write(temp_str, '(I0)') start_range
            end if
            
            ! Copy temp_str to buffer if space available
            temp_str = adjustl(temp_str)
            if (buffer_pos + len_trim(temp_str) <= BUFFER_SIZE) then
                buffer(buffer_pos:buffer_pos+len_trim(temp_str)-1) = &
                    temp_str(1:len_trim(temp_str))
                buffer_pos = buffer_pos + len_trim(temp_str)
            end if
            
            i = i + 1
            range_count = range_count + 1
        end do
        
        ! Allocate result with exact size needed
        if (buffer_pos > 1) then
            compressed = buffer(1:buffer_pos-1)
        else
            compressed = ""
        end if
    end function compress_ranges

    ! Format percentage with specified precision
    function format_percentage(value, precision) result(formatted)
        real, intent(in) :: value
        integer, intent(in) :: precision
        character(len=:), allocatable :: formatted
        character(len=50) :: buffer, fmt
        integer :: safe_precision
        
        ! Validate precision parameter (must be non-negative and reasonable)
        safe_precision = max(0, min(precision, 10))  ! Clamp between 0 and 10
        
        ! Create format string with minimum width to ensure leading zero
        write(fmt, '(A,I0,A,I0,A)') "(F", 4 + safe_precision, ".", safe_precision, ",A)"
        
        ! Format the value
        write(buffer, fmt) value, "%"
        
        formatted = trim(adjustl(buffer))
    end function format_percentage

    ! Convert integer to string (primary interface)
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(I0)') num
        str = trim(temp_str)
    end function int_to_str
    
    ! Convert real to string (primary interface)
    function real_to_str(num) result(str)
        real, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(F0.1)') num
        str = trim(temp_str)
    end function real_to_str
    
    ! Convert integer to string (alternative interface)
    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') value
        str = trim(temp_str)
    end function int_to_string
    
    ! Convert real to string (alternative interface with higher precision)
    function real_to_string(value) result(str)
        real, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(F0.6)') value
        str = trim(temp_str)
    end function real_to_string
    
    ! Convert int64 to string
    function int64_to_string(int_val) result(str_val)
        use iso_fortran_env, only: int64
        integer(int64), intent(in) :: int_val
        character(len=:), allocatable :: str_val
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str_val = trim(temp_str)
    end function int64_to_string

    ! Split string by delimiter
    function split(input_string, delimiter) result(parts)
        character(len=*), intent(in) :: input_string
        character(len=*), intent(in) :: delimiter
        character(len=:), allocatable :: parts(:)
        character(len=len(input_string)) :: work_string
        integer :: i, count, start, pos
        integer, parameter :: max_parts = 100
        character(len=len(input_string)) :: temp_parts(max_parts)
        
        work_string = input_string
        count = 0
        start = 1
        
        ! Handle empty delimiter case - return original string
        if (len(delimiter) == 0) then
            allocate(character(len=len(input_string)) :: parts(1))
            parts(1) = trim(input_string)
            return
        end if
        
        ! Count and extract parts
        do
            pos = index(work_string(start:), delimiter)
            if (pos == 0) then
                ! Last part
                count = count + 1
                temp_parts(count) = work_string(start:)
                exit
            else
                ! Found delimiter
                count = count + 1
                temp_parts(count) = work_string(start:start + pos - 2)
                start = start + pos + len(delimiter) - 1
            end if
        end do
        
        ! Allocate result array
        allocate(character(len=len(input_string)) :: parts(count))
        do i = 1, count
            parts(i) = trim(temp_parts(i))
        end do
    end function split

    ! Trim whitespace from both ends
    function trim_string(input_string) result(trimmed)
        character(len=*), intent(in) :: input_string
        character(len=:), allocatable :: trimmed
        
        integer :: start_pos, end_pos, i
        
        ! Find first non-whitespace character (tabs, spaces, etc.)
        start_pos = 0  ! Initialize to 0 to indicate not found
        do i = 1, len(input_string)
            if (.not. is_whitespace_char(input_string(i:i))) then
                start_pos = i
                exit
            end if
        end do
        
        ! Find last non-whitespace character
        end_pos = 0  ! Initialize to 0 to indicate not found
        do i = len(input_string), 1, -1
            if (.not. is_whitespace_char(input_string(i:i))) then
                end_pos = i
                exit
            end if
        end do
        
        ! Handle empty or all-whitespace string
        if (start_pos == 0 .or. end_pos == 0 .or. start_pos > end_pos) then
            trimmed = ""
        else
            trimmed = input_string(start_pos:end_pos)
        end if
    end function trim_string
    
    ! Helper function to check if character is whitespace (space, tab, etc.)
    function is_whitespace_char(char) result(is_ws)
        character(len=1), intent(in) :: char
        logical :: is_ws
        
        is_ws = (char == ' ' .or. char == achar(9) .or. &  ! space, tab
                 char == achar(10) .or. char == achar(13))  ! newline, carriage return
    end function is_whitespace_char





    ! Convert string to lowercase (helper function)
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i, ascii_val
        
        lower_str = str
        do i = 1, len(str)
            ascii_val = ichar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then  ! A-Z
                lower_str(i:i) = char(ascii_val + 32)
            end if
        end do
    end function to_lower

    ! Simple pattern matching (supports * wildcard)
    function matches_pattern(filepath, pattern) result(matches)
        character(len=*), intent(in) :: filepath
        character(len=*), intent(in) :: pattern
        logical :: matches
        
        character(len=:), allocatable :: pattern_trimmed, filepath_trimmed
        integer :: star_pos, prefix_len, suffix_len
        integer :: second_star_pos
        
        ! Use case-sensitive matching to maintain backward compatibility
        pattern_trimmed = trim(pattern)
        filepath_trimmed = trim(filepath)
        
        ! Handle edge cases first
        if (len(pattern_trimmed) == 0) then
            ! Empty pattern only matches empty filepath
            matches = (len(filepath_trimmed) == 0)
            return
        end if
        
        if (len(filepath_trimmed) == 0) then
            ! Empty filepath only matches empty pattern or single "*"
            matches = (pattern_trimmed == "*")
            return
        end if
        
        star_pos = index(pattern_trimmed, "*")
        
        if (star_pos == 0) then
            ! No wildcard, exact match
            matches = (filepath_trimmed == pattern_trimmed)
        else if (star_pos == len(pattern_trimmed)) then
            ! Pattern ends with *, check prefix
            prefix_len = star_pos - 1
            if (prefix_len == 0) then
                ! Just "*", matches everything non-empty
                matches = .true.
            else if (len(filepath_trimmed) >= prefix_len) then
                matches = (filepath_trimmed(1:prefix_len) == pattern_trimmed(1:prefix_len))
            else
                matches = .false.
            end if
        else if (star_pos == 1) then
            ! Pattern starts with *, check for second wildcard
            second_star_pos = index(pattern_trimmed(2:), "*")
            if (second_star_pos == 0) then
                ! Pattern is "*suffix", check suffix
                suffix_len = len(pattern_trimmed) - 1
                if (suffix_len == 0) then
                    ! Just "*", matches everything
                    matches = .true.
                else if (len(filepath_trimmed) >= suffix_len) then
                    matches = (filepath_trimmed(len(filepath_trimmed) - suffix_len + 1:) == &
                              pattern_trimmed(2:))
                else
                    matches = .false.
                end if
            else
                ! Pattern is "*middle*", check if middle is contained in filepath
                ! Extract the middle part (between the two asterisks)
                ! second_star_pos is relative to pattern_trimmed(2:), so adjust
                matches = (index(filepath_trimmed, pattern_trimmed(2:1+second_star_pos-1)) > 0)
            end if
        else
            ! Wildcard in middle - check both prefix and suffix match
            prefix_len = star_pos - 1
            suffix_len = len(pattern_trimmed) - star_pos
            if (len(filepath_trimmed) >= prefix_len + suffix_len) then
                matches = (filepath_trimmed(1:prefix_len) == pattern_trimmed(1:prefix_len)) .and. &
                         (filepath_trimmed(len(filepath_trimmed) - suffix_len + 1:) == &
                          pattern_trimmed(star_pos + 1:))
            else
                matches = .false.
            end if
        end if
    end function matches_pattern

    ! Check if filepath matches any exclude pattern in a list
    function check_exclude_patterns_list(filepath, exclude_patterns) result(should_exclude)
        character(len=*), intent(in) :: filepath
        character(len=*), intent(in) :: exclude_patterns(:)
        logical :: should_exclude
        
        integer :: i
        
        should_exclude = .false.
        
        ! Check if exclude patterns are empty
        if (size(exclude_patterns) == 0) then
            return
        end if
        
        do i = 1, size(exclude_patterns)
            if (matches_pattern(filepath, exclude_patterns(i))) then
                should_exclude = .true.
                return
            end if
        end do
    end function check_exclude_patterns_list

end module string_utils
