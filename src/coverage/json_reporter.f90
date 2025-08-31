module json_reporter
    !! JSON reporter implementation extracted from coverage_reporter_impl
    !! 
    !! Focused solely on JSON format report generation.
    !! Provides clean separation of JSON formatting logic.
    use coverage_types
    use coverage_reporter
    implicit none
    private
    
    public :: json_reporter_t
    
    type, extends(coverage_reporter_t) :: json_reporter_t
    contains
        procedure :: generate_report => json_generate_report
        procedure :: get_format_name => json_get_format_name
        procedure :: supports_diff => json_supports_diff
    end type json_reporter_t
    
contains
    
    subroutine json_generate_report(this, coverage_data, output_path, &
                                  success, error_message, &
                                  diff_data, threshold)
        use json_io, only: export_coverage_to_json
        use file_utilities, only: write_text_file
        class(json_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: json_content
        logical :: write_error
        
        ! Convert coverage data to JSON
        call export_coverage_to_json(coverage_data, json_content)
        
        if (.not. allocated(json_content)) then
            success = .false.
            error_message = "Failed to generate JSON content"
            return
        end if
        
        ! Write JSON content to file
        call write_text_file(output_path, json_content, write_error)
        
        if (write_error) then
            success = .false.
            error_message = "Failed to write JSON file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
        
        ! Suppress unused parameter warnings
        if (present(diff_data)) continue
        if (present(threshold)) continue
        
    end subroutine json_generate_report
    
    function json_get_format_name(this) result(format_name)
        class(json_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "json"
    end function json_get_format_name
    
    function json_supports_diff(this) result(supported)
        class(json_reporter_t), intent(in) :: this
        logical :: supported
        supported = .false.
    end function json_supports_diff
    
end module json_reporter