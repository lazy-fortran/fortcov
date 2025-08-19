! Demo I/O module for Meson coverage testing
module demo_io
    implicit none
    private
    
    public :: read_numbers, write_numbers, process_file, validate_number
    
contains

    subroutine read_numbers(filename, numbers, count, max_count)
        character(len=*), intent(in) :: filename
        real, intent(out) :: numbers(:)
        integer, intent(out) :: count
        integer, intent(in) :: max_count
        
        integer :: unit, iostat
        real :: value
        
        count = 0
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        
        if (iostat /= 0) then
            write(*,*) "Error opening file: ", filename
            return
        end if
        
        do
            read(unit, *, iostat=iostat) value
            if (iostat /= 0) exit
            
            count = count + 1
            if (count <= max_count) then
                numbers(count) = value
            else
                write(*,*) "Warning: too many numbers, truncating at", max_count
                exit
            end if
        end do
        
        close(unit)
    end subroutine read_numbers

    subroutine write_numbers(filename, numbers, count)
        character(len=*), intent(in) :: filename
        real, intent(in) :: numbers(:)
        integer, intent(in) :: count
        
        integer :: unit, iostat, i
        
        open(newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        
        if (iostat /= 0) then
            write(*,*) "Error creating file: ", filename
            return
        end if
        
        do i = 1, count
            write(unit, '(F12.6)') numbers(i)
        end do
        
        close(unit)
    end subroutine write_numbers

    subroutine process_file(input_filename, output_filename)
        character(len=*), intent(in) :: input_filename, output_filename
        real :: numbers(100), processed(100)
        integer :: count, i
        
        call read_numbers(input_filename, numbers, count, 100)
        
        if (count > 0) then
            ! Simple processing: square each number
            do i = 1, count
                processed(i) = numbers(i) ** 2
            end do
            
            call write_numbers(output_filename, processed, count)
            write(*,*) "Processed", count, "numbers from", trim(input_filename), "to", trim(output_filename)
        else
            write(*,*) "No numbers to process"
        end if
    end subroutine process_file

    function validate_number(value, min_val, max_val) result(valid)
        real, intent(in) :: value, min_val, max_val
        logical :: valid
        
        valid = (value >= min_val) .and. (value <= max_val)
    end function validate_number

end module demo_io