module xml_utils_core
    !! XML Utility Functions
    !! 
    !! Extracted from xml_utils.f90 for SRP compliance (Issue #718).
    !! Handles utility functions for XML processing and file operations.
    use coverage_model_core
    use string_utils, only: int_to_string
    implicit none
    private
    
    public :: calculate_file_line_rate
    public :: get_current_timestamp
    public :: get_directory_path
    public :: get_base_name
    
contains
    
    subroutine calculate_file_line_rate(file_data, line_rate)
        type(coverage_file_t), intent(in) :: file_data
        real, intent(out) :: line_rate
        
        integer :: total_lines, covered_lines, i
        
        total_lines = 0
        covered_lines = 0
        
        ! Memory safety: Check if lines array is allocated
        if (allocated(file_data%lines)) then
            do i = 1, size(file_data%lines)
                if (file_data%lines(i)%is_executable) then
                    total_lines = total_lines + 1
                    if (file_data%lines(i)%execution_count > 0) then
                        covered_lines = covered_lines + 1
                    end if
                end if
            end do
        end if
        
        ! Calculate line rate
        if (total_lines > 0) then
            line_rate = real(covered_lines) / real(total_lines)
        else
            line_rate = 0.0
        end if
        
    end subroutine calculate_file_line_rate
    
    function get_current_timestamp() result(timestamp)
        character(len=19) :: timestamp
        integer :: values(8)
        
        call date_and_time(values=values)
        write(timestamp, '(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') &
            values(1), '-', values(2), '-', values(3), 'T', &
            values(5), ':', values(6), ':', values(7)
            
    end function get_current_timestamp
    
    function get_directory_path(filename) result(dir_path)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dir_path
        integer :: last_slash
        
        last_slash = index(filename, '/', back=.true.)
        if (last_slash > 0) then
            dir_path = filename(1:last_slash-1)
        else
            dir_path = '.'
        end if
        
    end function get_directory_path
    
    function get_base_name(filename) result(base_name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: base_name
        integer :: last_slash, last_dot
        
        last_slash = index(filename, '/', back=.true.)
        last_dot = index(filename, '.', back=.true.)
        
        if (last_slash > 0 .and. last_dot > last_slash) then
            base_name = filename(last_slash+1:last_dot-1)
        else if (last_slash > 0) then
            base_name = filename(last_slash+1:)
        else if (last_dot > 0) then
            base_name = filename(1:last_dot-1)
        else
            base_name = filename
        end if
        
    end function get_base_name
    
end module xml_utils_core