module html_reporter_impl
    !! HTML reporter implementation extracted from coverage_reporter_impl
    !! 
    !! Focused solely on HTML format report generation.
    !! Provides clean separation of HTML formatting logic.
    use coverage_types
    use coverage_reporter
    use coverage_stats_core, only: stats_t => coverage_stats_t
    use string_utils, only: int_to_string, format_percentage
    implicit none
    private
    
    public :: html_reporter_t
    
    type, extends(coverage_reporter_t) :: html_reporter_t
    contains
        procedure :: generate_report => html_generate_report
        procedure :: get_format_name => html_get_format_name
        procedure :: supports_diff => html_supports_diff
    end type html_reporter_t
    
contains
    
    subroutine html_generate_report(this, coverage_data, output_path, &
                                  success, error_message, &
                                  diff_data, threshold)
        use file_utilities, only: write_text_file
        use coverage_stats_core, only: calculate_line_coverage, coverage_stats_t
        class(html_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: html_content
        type(coverage_stats_t) :: stats
        logical :: write_error
        integer :: i
        
        ! Calculate statistics
        stats = calculate_line_coverage(coverage_data)
        
        ! Generate HTML content
        html_content = "<!DOCTYPE html>" // new_line('a') // &
            "<html><head><title>Coverage Report</title>" // new_line('a') // &
            "<style>" // new_line('a') // &
            "body { font-family: Arial, sans-serif; margin: 20px; }" // new_line('a') // &
            ".summary { background: #f0f0f0; padding: 10px; border-radius: 5px; }" // new_line('a') // &
            ".file { margin: 10px 0; padding: 5px; border-left: 3px solid #007acc; }" // new_line('a') // &
            "</style></head><body>" // new_line('a') // &
            "<h1>Coverage Report</h1>" // new_line('a') // &
            "<div class='summary'>" // new_line('a')
        
        html_content = html_content // "<p><strong>Overall Coverage:</strong> " // &
                      format_percentage(stats%percentage, 2) // "</p>" // new_line('a')
        html_content = html_content // "<p><strong>Lines Covered:</strong> " // &
                      int_to_string(stats%covered_count) // " of " // &
                      int_to_string(stats%total_count) // "</p>" // new_line('a')
        if (allocated(coverage_data%files)) then
            html_content = html_content // "<p><strong>Files Analyzed:</strong> " // &
                          int_to_string(size(coverage_data%files)) // "</p>" // new_line('a')
        else
            html_content = html_content // "<p><strong>Files Analyzed:</strong> 0</p>" // new_line('a')
        end if
        
        html_content = html_content // "</div>" // new_line('a') // &
            "<h2>File Details</h2>" // new_line('a')
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                html_content = html_content // "<div class='file'><strong>" // &
                              trim(coverage_data%files(i)%filename) // ":</strong> " // &
                              format_percentage(coverage_data%files(i)%line_coverage, 2) // &
                              "</div>" // new_line('a')
            end do
        end if
        
        html_content = html_content // "</body></html>"
        
        ! Write to file
        call write_text_file(output_path, html_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write HTML file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
        
        ! Suppress unused warnings
        if (present(diff_data)) continue
        if (present(threshold)) continue
    end subroutine html_generate_report
    
    function html_get_format_name(this) result(format_name)
        class(html_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "html"
    end function html_get_format_name
    
    function html_supports_diff(this) result(supported)
        class(html_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function html_supports_diff
    
end module html_reporter_impl