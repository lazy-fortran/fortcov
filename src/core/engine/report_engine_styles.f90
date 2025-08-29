module report_engine_styles
    !! Report Engine Styled Output
    !! 
    !! Handles styled report generation for different formats.
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use data_transformer_core
    use data_transformer_types
    use theme_manager_core
    use syntax_highlighter
    implicit none
    private
    
    public :: generate_styled_report_content
    public :: prepare_styled_report_data
    public :: generate_format_specific_output

contains

    ! Generate styled report content
    subroutine generate_styled_report_content(source_data, transformer, &
                                             theme_manager, highlighter, &
                                             format, output, success, error_msg)
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
        
        success = .false.
        error_msg = ""
        
        ! Prepare report data (theme, transformed data, source content)
        call prepare_styled_report_data(source_data, transformer, theme_manager, &
                                       transformed_data, theme, source_content, &
                                       success, error_msg)
        if (.not. success) return
        
        ! Generate format-specific output
        call generate_format_specific_output(highlighter, theme_manager, &
                                            format, source_content, theme, &
                                            output, success, error_msg)
        
    end subroutine generate_styled_report_content
    
    ! Prepare styled report data (theme, transformed data, source content)
    subroutine prepare_styled_report_data(source_data, transformer, theme_manager, &
                                         transformed_data, theme, source_content, &
                                         success, error_msg)
        type(coverage_data_t), intent(in) :: source_data
        type(data_transformer_t), intent(inout) :: transformer
        type(theme_manager_t), intent(inout) :: theme_manager
        type(transformed_data_t), intent(out) :: transformed_data
        type(color_scheme_t), intent(out) :: theme
        character(len=:), allocatable, intent(out) :: source_content
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
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
        if (allocated(transformed_data%files) .and. &
            & size(transformed_data%files) > 0) then
            source_content = "program example" // new_line('a') // &
                           "    ! Test comment" // new_line('a') // &
                           "    integer :: x = 1" // new_line('a') // &
                           "end program"
        else
            source_content = "! No source files available"
        end if
        
        success = .true.
    end subroutine prepare_styled_report_data
    
    ! Generate format-specific output
    subroutine generate_format_specific_output(highlighter, theme_manager, &
                                              format, source_content, theme, &
                                              output, success, error_msg)
        type(syntax_highlighter_t), intent(inout) :: highlighter
        type(theme_manager_t), intent(inout) :: theme_manager
        character(len=*), intent(in) :: format
        character(len=*), intent(in) :: source_content
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        character(len=:), allocatable :: highlighted_content, css_variables
        
        success = .false.
        error_msg = ""
        
        ! Apply syntax highlighting based on format
        select case (trim(format))
        case ("html")
            call highlighter%highlight_for_html(source_content, &
                                               highlighted_content, success)
            if (.not. success) then
                error_msg = "Failed to generate HTML highlighting"
                return
            end if
            
            ! Add theme CSS variables
            call theme_manager%generate_css_variables(theme, css_variables)
            output = '<style>' // new_line('a') // css_variables // &
                     new_line('a') // &
                     '</style>' // new_line('a') // highlighted_content
            
        case ("terminal")
            call highlighter%highlight_for_terminal(source_content, &
                                                   highlighted_content, success)
            if (.not. success) then
                error_msg = "Failed to generate terminal highlighting"
                return
            end if
            output = highlighted_content
            
        case default
            error_msg = "Unsupported output format: " // format
            return
        end select
        
        success = .true.
    end subroutine generate_format_specific_output

end module report_engine_styles