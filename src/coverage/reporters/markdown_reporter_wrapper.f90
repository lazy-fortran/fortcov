module markdown_reporter_wrapper
    !! Markdown reporter implementation extracted from coverage_reporter_impl
    !! 
    !! Focused solely on markdown format report generation.
    !! Provides clean separation of markdown formatting logic.
    use coverage_types
    use coverage_reporter
    implicit none
    private
    
    public :: markdown_reporter_t
    
    type, extends(coverage_reporter_t) :: markdown_reporter_t
    contains
        procedure :: generate_report => markdown_generate_report
        procedure :: get_format_name => markdown_get_format_name
        procedure :: supports_diff => markdown_supports_diff
    end type markdown_reporter_t
    
contains
    
    subroutine markdown_generate_report(this, coverage_data, output_path, &
                                      success, error_message, &
                                      diff_data, threshold)
        use markdown_reporter, only: generate_markdown_report, markdown_report_options_t
        use file_utilities, only: write_text_file_safe
        use error_handling_core, only: error_context_t, ERROR_SUCCESS
        class(markdown_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        character(len=:), allocatable :: markdown_content
        type(markdown_report_options_t) :: options
        type(error_context_t) :: err_ctx
        
        ! Initialize markdown options with defaults
        call options%init()
        
        ! Generate markdown content
        markdown_content = generate_markdown_report(coverage_data, options)
        
        if (.not. allocated(markdown_content)) then
            success = .false.
            error_message = "Failed to generate markdown content"
            return
        end if
        
        ! Write markdown content to file (safe: ensures directory exists)
        call write_text_file_safe(output_path, markdown_content, err_ctx)
        if (err_ctx%error_code /= ERROR_SUCCESS) then
            success = .false.
            error_message = "Failed to write markdown file: " // trim(output_path)
        else
            success = .true.
            error_message = ""
        end if
    end subroutine markdown_generate_report
    
    function markdown_get_format_name(this) result(format_name)
        class(markdown_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        format_name = "markdown"
    end function markdown_get_format_name
    
    function markdown_supports_diff(this) result(supported)
        class(markdown_reporter_t), intent(in) :: this
        logical :: supported
        supported = .true.
    end function markdown_supports_diff
    
end module markdown_reporter_wrapper
