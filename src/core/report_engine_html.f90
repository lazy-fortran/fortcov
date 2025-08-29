module report_engine_html
    !! Report Engine HTML Generation
    !! 
    !! Handles HTML report generation functionality.
    !! Extracted from report_engine.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use data_transformer_core
    use data_transformer_types
    use theme_manager_core
    use syntax_highlighter
    use html_reporter
    implicit none
    private
    
    public :: generate_html_report_content
    public :: generate_styled_html_output

contains

    ! Generate HTML report content
    subroutine generate_html_report_content(source_data, transformer, &
                                           theme_manager, output_path, &
                                           success, error_msg)
        type(coverage_data_t), intent(in) :: source_data
        type(data_transformer_t), intent(inout) :: transformer
        type(theme_manager_t), intent(inout) :: theme_manager
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(transformed_data_t) :: transformed_data
        type(color_scheme_t) :: theme
        character(len=:), allocatable :: html_content, css_variables
        integer :: unit, iostat
        
        success = .false.
        error_msg = ""
        
        ! Transform data
        call transformer%transform_data(source_data, transformed_data, &
                                       success, error_msg)
        if (.not. success) return
        
        ! Load theme
        call theme_manager%load_cyberpunk_theme(theme, success, error_msg)
        if (.not. success) return
        
        ! Generate CSS variables
        call theme_manager%generate_css_variables(theme, css_variables)
        
        ! Build HTML content
        html_content = generate_html_structure(transformed_data, &
                                               css_variables, theme)
        
        ! Write to file
        open(newunit=unit, file=output_path, status='replace', action='write', &
             iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Failed to open output file: " // output_path
            return
        end if
        
        write(unit, '(A)') html_content
        close(unit)
        
        success = .true.
    end subroutine generate_html_report_content
    
    ! Generate styled HTML output
    subroutine generate_styled_html_output(source_content, highlighter, &
                                          theme_manager, theme, output, &
                                          success, error_msg)
        character(len=*), intent(in) :: source_content
        type(syntax_highlighter_t), intent(inout) :: highlighter
        type(theme_manager_t), intent(inout) :: theme_manager
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        character(len=:), allocatable :: highlighted_content, css_variables
        
        success = .false.
        error_msg = ""
        
        ! Apply HTML syntax highlighting
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
        
        success = .true.
    end subroutine generate_styled_html_output

end module report_engine_html