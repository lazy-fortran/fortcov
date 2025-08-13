module string_utils
    implicit none
    private
    
    ! Public procedures
    public :: compress_ranges
    public :: format_percentage
    public :: split
    public :: trim_string
    
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
        
        ! Create format string with minimum width to ensure leading zero
        write(fmt, '(A,I0,A,I0,A)') "(F", 4 + precision, ".", precision, ",A)"
        
        ! Format the value
        write(buffer, fmt) value, "%"
        
        formatted = trim(adjustl(buffer))
    end function format_percentage

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

end module string_utils