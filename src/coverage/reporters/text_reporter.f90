module text_reporter
    !! Text reporter implementation extracted from coverage_reporter_impl
    !! 
    !! Focused solely on text format report generation.
    !! Provides clean separation of text formatting logic.
    use coverage_types
    use coverage_reporter
    use coverage_stats_core, only: stats_t => coverage_stats_t
    use string_utils, only: int_to_string, format_percentage
    implicit none
    private
    
    public :: text_reporter_t
    
    type, extends(coverage_reporter_t) :: text_reporter_t
    contains
        procedure :: generate_report => text_generate_report
        procedure :: get_format_name => text_get_format_name
        procedure :: supports_diff => text_supports_diff
    end type text_reporter_t
    
contains
    
    subroutine text_generate_report(this, coverage_data, output_path, &
                                  success, error_message, &
                                  diff_data, threshold)
        use file_utilities, only: write_text_file
        use coverage_stats_core, only: calculate_line_coverage, coverage_stats_t
        class(text_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: text_content
        type(coverage_stats_t) :: stats
        logical :: write_error
        integer :: i
        
        ! Calculate statistics
        stats = calculate_line_coverage(coverage_data)
        
        ! Generate text report content
        text_content = "Coverage Report" // new_line('a') // &
                      "===============" // new_line('a') // new_line('a')
        
        ! Add summary
        text_content = text_content // "Overall Coverage: " // format_percentage(stats%percentage, 2) // new_line('a')
        text_content = text_content // "Lines Covered: " // int_to_string(stats%covered_count) // &
                      " of " // int_to_string(stats%total_count) // new_line('a')
        if (allocated(coverage_data%files)) then
            text_content = text_content // "Files Analyzed: " // int_to_string(size(coverage_data%files)) // &
                          new_line('a') // new_line('a')
        else
            text_content = text_content // "Files Analyzed: 0" // new_line('a') // new_line('a')
        end if
        
        ! Add file details
        text_content = text_content // "File Details:" // new_line('a')
        text_content = text_content // "--------------" // new_line('a')
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                text_content = text_content // trim(coverage_data%files(i)%filename) // ": " // &
                              format_percentage(coverage_data%files(i)%line_coverage, 2) // new_line('a')
            end do
        end if
        
        ! Write to file
        call write_text_file(output_path, text_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write text file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
        
        ! Suppress unused warnings
        if (present(diff_data)) continue
        if (present(threshold)) continue
    end subroutine text_generate_report
    
    function text_get_format_name(this) result(format_name)
        class(text_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "text"
    end function text_get_format_name
    
    function text_supports_diff(this) result(supported)
        class(text_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function text_supports_diff
    
end module text_reporter