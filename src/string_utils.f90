module string_utils
    implicit none
    private
    
    ! Public procedures
    public :: compress_ranges
    public :: format_percentage
    public :: format_integer
    public :: split
    public :: trim_string
    public :: validate_string_input
    public :: sanitize_filename
    public :: is_safe_path
    
contains

    ! Compress consecutive line numbers into ranges (1,2,3,5,6 -> "1-3,5-6")
    function compress_ranges(numbers) result(compressed)
        integer, intent(in) :: numbers(:)
        character(len=:), allocatable :: compressed
        character(len=:), allocatable :: result_str
        integer :: i, start_range, end_range
        logical :: in_range, first_item
        
        if (size(numbers) == 0) then
            compressed = ""
            return
        end if
        
        result_str = ""
        i = 1
        first_item = .true.
        
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
                result_str = result_str // ", "
            end if
            first_item = .false.
            
            ! Add range or single number
            if (in_range) then
                result_str = result_str // int_to_string(start_range) // &
                            "-" // int_to_string(end_range)
            else
                result_str = result_str // int_to_string(start_range)
            end if
            
            i = i + 1
        end do
        
        compressed = result_str
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

    ! Format integer as string
    function format_integer(value) result(formatted)
        integer, intent(in) :: value
        character(len=:), allocatable :: formatted
        character(len=20) :: buffer
        
        write(buffer, '(I0)') value
        formatted = trim(buffer)
    end function format_integer

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
        
        trimmed = trim(adjustl(input_string))
    end function trim_string

    ! Helper function to convert integer to string
    function int_to_string(int_val) result(str)
        integer, intent(in) :: int_val
        character(len=:), allocatable :: str
        character(len=20) :: buffer
        
        write(buffer, '(I0)') int_val
        str = trim(buffer)
    end function int_to_string

    ! Security: Validate string input for safety constraints
    function validate_string_input(input_str, max_length) result(is_valid)
        character(len=*), intent(in) :: input_str
        integer, intent(in) :: max_length
        logical :: is_valid
        integer :: i, char_code
        
        is_valid = .true.
        
        ! Check length limit
        if (len_trim(input_str) > max_length) then
            is_valid = .false.
            return
        end if
        
        ! Check for dangerous characters
        do i = 1, len_trim(input_str)
            char_code = ichar(input_str(i:i))
            
            ! Reject null characters and most control characters
            if (char_code == 0 .or. (char_code < 32 .and. char_code /= 9 .and. char_code /= 10)) then
                is_valid = .false.
                return
            end if
            
            ! Reject shell command injection characters
            if (index(';|&`$<>', input_str(i:i)) > 0) then
                is_valid = .false.
                return
            end if
        end do
    end function validate_string_input

    ! Security: Sanitize filename by removing dangerous characters
    function sanitize_filename(filename) result(safe_filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: safe_filename
        character(len=len(filename)) :: temp_filename
        integer :: i, j
        
        temp_filename = ''
        j = 1
        
        do i = 1, len_trim(filename)
            ! Keep alphanumeric, period, underscore, hyphen, forward slash
            if (verify(filename(i:i), &
                'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-/') &
                == 0) then
                temp_filename(j:j) = filename(i:i)
                j = j + 1
            end if
        end do
        
        safe_filename = trim(temp_filename)
        
        ! Security: Enforce maximum length
        if (len(safe_filename) > 1024) then
            safe_filename = safe_filename(1:1024)
        end if
    end function sanitize_filename

    ! Security: Check if path is safe (no directory traversal)
    function is_safe_path(path) result(is_safe)
        character(len=*), intent(in) :: path
        logical :: is_safe
        
        is_safe = .true.
        
        ! Reject directory traversal attempts
        if (index(path, '../') > 0 .or. index(path, '..\\') > 0) then
            is_safe = .false.
            return
        end if
        
        ! Reject absolute paths starting with sensitive directories
        if (index(path, '/etc/') == 1 .or. index(path, '/root/') == 1 .or. &
            index(path, '/boot/') == 1 .or. index(path, '/sys/') == 1) then
            is_safe = .false.
            return
        end if
        
        ! Reject Windows system paths
        if (index(path, 'C:\Windows') == 1 .or. index(path, 'C:\System') == 1 .or. &
            index(path, 'C:\\Windows') == 1 .or. index(path, 'C:\\System') == 1) then
            is_safe = .false.
            return
        end if
        
        ! Basic validation
        if (.not. validate_string_input(path, 2048)) then
            is_safe = .false.
            return
        end if
    end function is_safe_path

end module string_utils
