module report_formatter_core
    !! Report Formatter - Handles report formatting and styling
    !! 
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    !! Responsible for applying formatting and highlighting to reports.
    use coverage_model_core
    use data_transformer_core
    use data_transformer_types
    use theme_manager_core
    use syntax_highlighter
    implicit none
    private
    
    public :: report_formatter_t
    
    type :: report_formatter_t
        logical :: initialized = .false.
    contains
        procedure :: init => formatter_init
        procedure :: format_report => formatter_format_report
    end type report_formatter_t
    
contains
    
    ! Initialize formatter
    subroutine formatter_init(this)
        class(report_formatter_t), intent(out) :: this
        this%initialized = .true.
    end subroutine formatter_init
    
    ! Format report with styling and highlighting
    subroutine formatter_format_report(this, source_data, transformer, &
                                       theme_manager, highlighter, format, &
                                       output, success, error_msg)
        class(report_formatter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: source_data
        type(data_transformer_t), intent(inout) :: transformer
        type(theme_manager_t), intent(inout) :: theme_manager
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: format
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(transformed_data_t) :: transformed_data
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: source_content
        character(len=:), allocatable :: highlighted_content, css_variables
        
        success = .false.
        error_msg = ""
        
        ! Transform data
        call transformer%transform_data(source_data, transformed_data, &
                                       success, error_msg)
        if (.not. success) return
        
        ! Load theme
        call theme_manager%load_cyberpunk_theme(theme, success, error_msg)
        if (.not. success) return
        
        ! Create sample source content from transformed data
        call create_sample_content(transformed_data, source_content)
        
        ! Apply format-specific highlighting
        select case (trim(format))
        case ("html")
            call format_html_output(highlighter, theme_manager, &
                                   source_content, theme, &
                                   output, success, error_msg)
            
        case ("terminal")
            call format_terminal_output(highlighter, source_content, &
                                       output, success, error_msg)
            
        case default
            error_msg = "Unsupported output format: " // format
            return
        end select
        
    end subroutine formatter_format_report
    
    ! Create sample content for formatting
    subroutine create_sample_content(transformed_data, content)
        type(transformed_data_t), intent(in) :: transformed_data
        character(len=:), allocatable, intent(out) :: content
        
        if (allocated(transformed_data%files) .and. &
            size(transformed_data%files) > 0) then
            content = "program example" // new_line('a') // &
                     "    ! Test comment" // new_line('a') // &
                     "    integer :: x = 1" // new_line('a') // &
                     "end program"
        else
            content = "! No source files available"
        end if
    end subroutine create_sample_content
    
    ! Format HTML output with theme
    subroutine format_html_output(highlighter, theme_manager, &
                                  source_content, theme, &
                                  output, success, error_msg)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        type(theme_manager_t), intent(inout) :: theme_manager
        character(len=*), intent(in) :: source_content
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        character(len=:), allocatable :: highlighted_content, css_variables
        
        call highlighter%highlight_for_html(source_content, &
                                           highlighted_content, success)
        if (.not. success) then
            error_msg = "Failed to generate HTML highlighting"
            return
        end if
        
        ! Add theme CSS variables
        call theme_manager%generate_css_variables(theme, css_variables)
        output = '<style>' // new_line('a') // css_variables // &
                 new_line('a') // '</style>' // new_line('a') // &
                 highlighted_content
        
        success = .true.
    end subroutine format_html_output
    
    ! Format terminal output
    subroutine format_terminal_output(highlighter, source_content, &
                                      output, success, error_msg)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        character(len=*), intent(in) :: source_content
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        call highlighter%highlight_for_terminal(source_content, &
                                               output, success)
        if (.not. success) then
            error_msg = "Failed to generate terminal highlighting"
            return
        end if
        
        success = .true.
    end subroutine format_terminal_output
    
end module report_formatter_core