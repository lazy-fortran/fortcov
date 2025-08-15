module coverage_reporter
    use coverage_model
    use markdown_reporter, only: generate_markdown_report, markdown_report_options_t
    use report_engine
    implicit none
    private
    
    ! DRY helper for unused parameter warnings
    interface suppress_unused_warning
        module procedure :: suppress_unused_warning_reporter
    end interface suppress_unused_warning
    
    ! Public types
    public :: coverage_reporter_t
    public :: markdown_reporter_t
    public :: json_reporter_t
    public :: xml_reporter_t
    public :: html_reporter_t
    public :: mock_reporter_t
    
    ! Public procedures
    public :: create_reporter
    
    ! Abstract reporter interface
    type, abstract :: coverage_reporter_t
    contains
        procedure(generate_report_interface), deferred :: generate_report
        procedure(get_format_name_interface), deferred :: get_format_name
        procedure(supports_diff_interface), deferred :: supports_diff
    end type coverage_reporter_t
    
    ! Concrete markdown reporter implementation
    type, extends(coverage_reporter_t) :: markdown_reporter_t
        character(len=:), allocatable :: last_error
    contains
        procedure :: generate_report => markdown_generate_report
        procedure :: get_format_name => markdown_get_format_name
        procedure :: supports_diff => markdown_supports_diff
    end type markdown_reporter_t
    
    ! Concrete JSON reporter implementation
    type, extends(coverage_reporter_t) :: json_reporter_t
        character(len=:), allocatable :: last_error
    contains
        procedure :: generate_report => json_generate_report
        procedure :: get_format_name => json_get_format_name
        procedure :: supports_diff => json_supports_diff
    end type json_reporter_t
    
    ! Concrete XML reporter implementation
    type, extends(coverage_reporter_t) :: xml_reporter_t
        character(len=:), allocatable :: last_error
    contains
        procedure :: generate_report => xml_generate_report
        procedure :: get_format_name => xml_get_format_name
        procedure :: supports_diff => xml_supports_diff
    end type xml_reporter_t
    
    ! HTML reporter using report_engine_t
    type, extends(coverage_reporter_t) :: html_reporter_t
        type(report_engine_t) :: engine
        logical :: initialized = .false.
        character(len=:), allocatable :: last_error
    contains
        procedure :: generate_report => html_generate_report
        procedure :: get_format_name => html_get_format_name
        procedure :: supports_diff => html_supports_diff
    end type html_reporter_t
    
    ! Mock reporter for testing
    type, extends(coverage_reporter_t) :: mock_reporter_t
        logical :: was_called = .false.
        type(coverage_data_t) :: captured_data
        character(len=:), allocatable :: captured_output_path
    contains
        procedure :: generate_report => mock_generate_report
        procedure :: get_format_name => mock_get_format_name
        procedure :: supports_diff => mock_supports_diff
    end type mock_reporter_t
    
    ! Abstract interfaces
    abstract interface
        subroutine generate_report_interface(this, coverage_data, output_path, &
                                            error_flag)
            import :: coverage_reporter_t, coverage_data_t
            class(coverage_reporter_t), intent(inout) :: this
            type(coverage_data_t), intent(in) :: coverage_data
            character(len=*), intent(in) :: output_path
            logical, intent(out) :: error_flag
        end subroutine generate_report_interface
        
        function get_format_name_interface(this) result(format_name)
            import :: coverage_reporter_t
            class(coverage_reporter_t), intent(in) :: this
            character(len=:), allocatable :: format_name
        end function get_format_name_interface
        
        function supports_diff_interface(this) result(supported)
            import :: coverage_reporter_t
            class(coverage_reporter_t), intent(in) :: this
            logical :: supported
        end function supports_diff_interface
    end interface

contains

    ! Factory function to create reporter based on format string
    subroutine create_reporter(format, reporter, error_flag)
        character(len=*), intent(in) :: format
        class(coverage_reporter_t), allocatable, intent(out) :: reporter
        logical, intent(out) :: error_flag
        
        error_flag = .false.
        
        ! Select reporter based on format
        select case (trim(format))
        case ("markdown", "md")
            allocate(markdown_reporter_t :: reporter)
        case ("json")
            allocate(json_reporter_t :: reporter)
        case ("xml")
            allocate(xml_reporter_t :: reporter)
        case ("html")
            allocate(html_reporter_t :: reporter)
        case ("mock")
            allocate(mock_reporter_t :: reporter)
        case default
            error_flag = .true.
        end select
    end subroutine create_reporter

    ! Markdown reporter implementations
    subroutine markdown_generate_report(this, coverage_data, output_path, &
                                       error_flag)
        class(markdown_reporter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: error_flag
        integer :: unit, stat
        logical :: use_stdout
        character(len=:), allocatable :: report_content
        type(markdown_report_options_t) :: options
        
        error_flag = .false.
        use_stdout = (trim(output_path) == "-")
        
        ! Configure report options
        call options%init()
        
        ! Generate report using proper markdown reporter
        report_content = generate_markdown_report(coverage_data, options)
        
        ! Open output stream
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            open(newunit=unit, file=output_path, status='replace', iostat=stat)
            if (stat /= 0) then
                error_flag = .true.
                ! Store error context in reporter instance for later retrieval
                this%last_error = "Cannot write to output file '" // &
                                 trim(output_path) // "' (iostat=" // &
                                 trim(int_to_string(stat)) // ")"
                return
            end if
        end if
        
        ! Write the complete markdown report
        write(unit, '(A)') "# Coverage Report"
        write(unit, '(A)') ""
        write(unit, '(A)') report_content
        
        if (.not. use_stdout) close(unit)
        
        call suppress_unused_warning(this)
    end subroutine markdown_generate_report

    function markdown_get_format_name(this) result(format_name)
        class(markdown_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        format_name = "markdown"
        
        call suppress_unused_warning(this)
    end function markdown_get_format_name

    function markdown_supports_diff(this) result(supported)
        class(markdown_reporter_t), intent(in) :: this
        logical :: supported
        
        ! Markdown reporter will support diff in the future
        supported = .false.
        
        call suppress_unused_warning(this)
    end function markdown_supports_diff

    ! JSON reporter implementations
    subroutine json_generate_report(this, coverage_data, output_path, &
                                   error_flag)
        use coverage_statistics
        class(json_reporter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: error_flag
        integer :: unit, stat
        logical :: use_stdout
        type(coverage_stats_t) :: line_stats, branch_stats, func_stats
        
        error_flag = .false.
        use_stdout = (trim(output_path) == "-")
        
        ! Calculate coverage statistics
        line_stats = calculate_line_coverage(coverage_data)
        branch_stats = calculate_branch_coverage(coverage_data)
        func_stats = calculate_function_coverage(coverage_data)
        
        ! Open output stream with buffering for performance
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            ! Open with recl for better buffering on supported systems
            open(newunit=unit, file=output_path, status='replace', &
                 form='formatted', access='sequential', iostat=stat)
            if (stat /= 0) then
                error_flag = .true.
                this%last_error = "Cannot write to output file '" // &
                                 trim(output_path) // "' (iostat=" // &
                                 trim(int_to_string(stat)) // ")"
                return
            end if
        end if
        
        ! Generate JSON using highly optimized approach
        call write_json_optimized(unit, coverage_data, line_stats, &
                                  branch_stats, func_stats)
        
        if (.not. use_stdout) close(unit)
        
        call suppress_unused_warning(this)
    end subroutine json_generate_report

    ! Secure JSON generation with chunked writing to avoid memory issues
    subroutine write_json_optimized(unit, coverage_data, line_stats, &
                                    branch_stats, func_stats)
        use coverage_statistics
        integer, intent(in) :: unit
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_stats_t), intent(in) :: line_stats, branch_stats, &
                                              func_stats
        integer :: i, j, line_count
        logical :: first_file, first_line
        
        ! Secure approach: Write JSON directly without large string building
        ! This eliminates buffer overflow risks and memory scalability issues
        
        ! Write JSON header directly
        write(unit, '(A)', advance='no') '{"coverage_report":{"line_coverage":'
        write(unit, '(A)', advance='no') real_to_string(line_stats%percentage)
        write(unit, '(A)', advance='no') ',"lines_covered":'
        write(unit, '(A)', advance='no') int_to_string(line_stats%covered_count)
        write(unit, '(A)', advance='no') ',"lines_total":'
        write(unit, '(A)', advance='no') int_to_string(line_stats%total_count)
        write(unit, '(A)', advance='no') ',"branch_coverage":'
        write(unit, '(A)', advance='no') real_to_string(branch_stats%percentage)
        write(unit, '(A)', advance='no') ',"branches_covered":'
        write(unit, '(A)', advance='no') int_to_string(branch_stats%covered_count)
        write(unit, '(A)', advance='no') ',"branches_total":'
        write(unit, '(A)', advance='no') int_to_string(branch_stats%total_count)
        write(unit, '(A)', advance='no') ',"function_coverage":'
        write(unit, '(A)', advance='no') real_to_string(func_stats%percentage)
        write(unit, '(A)', advance='no') ',"functions_covered":'
        write(unit, '(A)', advance='no') int_to_string(func_stats%covered_count)
        write(unit, '(A)', advance='no') ',"functions_total":'
        write(unit, '(A)', advance='no') int_to_string(func_stats%total_count)
        write(unit, '(A)', advance='no') ',"files":['
        
        ! Process files with direct writing
        first_file = .true.
        do i = 1, size(coverage_data%files)
            line_count = size(coverage_data%files(i)%lines)
            
            ! Add file separator
            if (.not. first_file) then
                write(unit, '(A)', advance='no') ','
            end if
            first_file = .false.
            
            ! Write file header
            write(unit, '(A)', advance='no') '{"filename":"'
            write(unit, '(A)', advance='no') trim(coverage_data%files(i)%filename)
            write(unit, '(A)', advance='no') '","line_count":'
            write(unit, '(A)', advance='no') int_to_string(line_count)
            write(unit, '(A)', advance='no') ',"lines":['
            
            ! Process all lines for this file
            first_line = .true.
            do j = 1, line_count
                if (.not. first_line) then
                    write(unit, '(A)', advance='no') ','
                end if
                first_line = .false.
                
                ! Write line entry directly
                write(unit, '(A)', advance='no') '{"line_number":'
                write(unit, '(A)', advance='no') int_to_string(coverage_data%files(i)%lines(j)%line_number)
                write(unit, '(A)', advance='no') ',"execution_count":'
                write(unit, '(A)', advance='no') int_to_string(coverage_data%files(i)%lines(j)%execution_count)
                write(unit, '(A)', advance='no') ',"is_executable":'
                
                if (coverage_data%files(i)%lines(j)%is_executable) then
                    write(unit, '(A)', advance='no') 'true}'
                else
                    write(unit, '(A)', advance='no') 'false}'
                end if
            end do
            
            ! Close file
            write(unit, '(A)', advance='no') ']}'
        end do
        
        ! Write JSON footer
        write(unit, '(A)') ']}}'
    end subroutine write_json_optimized
    
    ! Helper function to convert real to string
    function real_to_string(num) result(str)
        real, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(F0.2)') num
        str = trim(temp_str)
    end function real_to_string

    function json_get_format_name(this) result(format_name)
        class(json_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        format_name = "json"
        
        call suppress_unused_warning(this)
    end function json_get_format_name
    
    ! Helper function to convert integer to string  
    function int_to_string(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(I0)') num
        str = trim(temp_str)
    end function int_to_string

    function json_supports_diff(this) result(supported)
        class(json_reporter_t), intent(in) :: this
        logical :: supported
        
        ! JSON reporter supports diff through structured data
        supported = .true.
        
        call suppress_unused_warning(this)
    end function json_supports_diff

    ! XML reporter implementations
    subroutine xml_generate_report(this, coverage_data, output_path, error_flag)
        use coverage_statistics
        class(xml_reporter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: error_flag
        integer :: unit, stat, i, j
        logical :: use_stdout
        type(coverage_stats_t) :: line_stats, branch_stats, func_stats
        
        error_flag = .false.
        use_stdout = (trim(output_path) == "-")
        
        ! Calculate coverage statistics
        line_stats = calculate_line_coverage(coverage_data)
        branch_stats = calculate_branch_coverage(coverage_data)
        func_stats = calculate_function_coverage(coverage_data)
        
        ! Open output stream
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            open(newunit=unit, file=output_path, status='replace', iostat=stat)
            if (stat /= 0) then
                error_flag = .true.
                this%last_error = "Cannot write to output file '" // &
                                 trim(output_path) // "' (iostat=" // &
                                 trim(int_to_string(stat)) // ")"
                return
            end if
        end if
        
        ! Generate XML structure (Cobertura-style)
        write(unit, '(A)') '<?xml version="1.0" encoding="UTF-8"?>'
        write(unit, '(A)') '<coverage version="1.0">'
        write(unit, '(A,F0.2,A)') '  <summary line-rate="', &
                                  line_stats%percentage/100.0, '"'
        write(unit, '(A,F0.2,A)') '           branch-rate="', &
                                  branch_stats%percentage/100.0, '"'
        write(unit, '(A,I0,A)') '           lines-covered="', &
                               line_stats%covered_count, '"'
        write(unit, '(A,I0,A)') '           lines-valid="', &
                               line_stats%total_count, '"'
        write(unit, '(A,I0,A)') '           branches-covered="', &
                               branch_stats%covered_count, '"'
        write(unit, '(A,I0,A)') '           branches-valid="', &
                               branch_stats%total_count, '"'
        write(unit, '(A)') '           complexity="0.0"/>'
        write(unit, '(A)') '  <sources>'
        write(unit, '(A)') '    <source>.</source>'
        write(unit, '(A)') '  </sources>'
        write(unit, '(A)') '  <packages>'
        write(unit, '(A)') '    <package name="fortran_project" complexity="0.0">'
        write(unit, '(A)') '      <classes>'
        
        ! File-level details
        do i = 1, size(coverage_data%files)
            write(unit, '(A,A,A)') '        <class name="', &
                trim(coverage_data%files(i)%filename), '" filename="', &
                trim(coverage_data%files(i)%filename), '"'
            write(unit, '(A)') '               complexity="0.0">'
            write(unit, '(A)') '          <methods/>'
            write(unit, '(A)') '          <lines>'
            
            ! Line details
            do j = 1, size(coverage_data%files(i)%lines)
                if (coverage_data%files(i)%lines(j)%is_executable) then
                    write(unit, '(A,I0,A,I0,A)') '            <line number="', &
                        coverage_data%files(i)%lines(j)%line_number, &
                        '" hits="', &
                        coverage_data%files(i)%lines(j)%execution_count, &
                        '" branch="false"/>'
                end if
            end do
            
            write(unit, '(A)') '          </lines>'
            write(unit, '(A)') '        </class>'
        end do
        
        write(unit, '(A)') '      </classes>'
        write(unit, '(A)') '    </package>'
        write(unit, '(A)') '  </packages>'
        write(unit, '(A)') '</coverage>'
        
        if (.not. use_stdout) close(unit)
        
        call suppress_unused_warning(this)
    end subroutine xml_generate_report

    function xml_get_format_name(this) result(format_name)
        class(xml_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        format_name = "xml"
        
        call suppress_unused_warning(this)
    end function xml_get_format_name

    function xml_supports_diff(this) result(supported)
        class(xml_reporter_t), intent(in) :: this
        logical :: supported
        
        ! XML reporter supports diff through structured data
        supported = .true.
        
        call suppress_unused_warning(this)
    end function xml_supports_diff

    ! HTML reporter implementations
    subroutine html_generate_report(this, coverage_data, output_path, error_flag)
        use coverage_statistics
        class(html_reporter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: error_flag
        integer :: unit, stat, i, j
        logical :: use_stdout
        type(coverage_stats_t) :: line_stats
        character(len=:), allocatable :: html_content, file_details, css_styles
        
        error_flag = .false.
        use_stdout = (trim(output_path) == "-")
        
        ! Calculate coverage statistics
        line_stats = calculate_line_coverage(coverage_data)
        
        ! Generate comprehensive CSS styles
        css_styles = generate_comprehensive_html_css()
        
        ! Generate file details section
        file_details = generate_html_file_coverage_details(coverage_data)
        
        ! Build comprehensive HTML content
        html_content = '<!DOCTYPE html>' // new_line('a') // &
                      '<html lang="en">' // new_line('a') // &
                      '<head>' // new_line('a') // &
                      '    <meta charset="UTF-8">' // new_line('a') // &
                      '    <meta name="viewport" content="width=device-width, initial-scale=1.0">' // new_line('a') // &
                      '    <title>Fortcov Coverage Report</title>' // new_line('a') // &
                      '    <meta name="description" content="Code coverage report generated by Fortcov">' // new_line('a') // &
                      css_styles // new_line('a') // &
                      '</head>' // new_line('a') // &
                      '<body>' // new_line('a') // &
                      '    <header class="report-header">' // new_line('a') // &
                      '        <h1>Coverage Report</h1>' // new_line('a') // &
                      '        <div class="theme-info">Theme: Cyberpunk</div>' // new_line('a') // &
                      '    </header>' // new_line('a') // &
                      '    <main class="report-main">' // new_line('a') // &
                      '        <section class="summary-section cyberpunk">' // new_line('a') // &
                      '            <h2>Coverage Summary</h2>' // new_line('a') // &
                      '            <div class="summary-grid">' // new_line('a') // &
                      '                <div class="summary-item">' // new_line('a') // &
                      '                    <span class="summary-label">Total Files:</span>' // new_line('a') // &
                      '                    <span class="summary-value">' // int_to_string(size(coverage_data%files)) // '</span>' // new_line('a') // &
                      '                </div>' // new_line('a') // &
                      '                <div class="summary-item">' // new_line('a') // &
                      '                    <span class="summary-label">Total Lines:</span>' // new_line('a') // &
                      '                    <span class="summary-value">' // int_to_string(line_stats%total_count) // '</span>' // new_line('a') // &
                      '                </div>' // new_line('a') // &
                      '                <div class="summary-item">' // new_line('a') // &
                      '                    <span class="summary-label">Covered Lines:</span>' // new_line('a') // &
                      '                    <span class="summary-value">' // int_to_string(line_stats%covered_count) // '</span>' // new_line('a') // &
                      '                </div>' // new_line('a') // &
                      '                <div class="summary-item highlight">' // new_line('a') // &
                      '                    <span class="summary-label">Coverage:</span>' // new_line('a') // &
                      '                    <span class="summary-value coverage-percent">' // real_to_string(line_stats%percentage) // '%</span>' // new_line('a') // &
                      '                </div>' // new_line('a') // &
                      '            </div>' // new_line('a') // &
                      '        </section>' // new_line('a') // &
                      file_details // new_line('a') // &
                      '    </main>' // new_line('a') // &
                      '    <footer class="report-footer">' // new_line('a') // &
                      '        <p>Generated by <strong>Fortcov</strong> - Fortran Coverage Analysis Tool</p>' // new_line('a') // &
                      '        <p class="timestamp">Report generated at: ' // get_html_timestamp() // '</p>' // new_line('a') // &
                      '    </footer>' // new_line('a') // &
                      '</body>' // new_line('a') // &
                      '</html>'
        
        ! Open output stream
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            open(newunit=unit, file=output_path, status='replace', iostat=stat)
            if (stat /= 0) then
                error_flag = .true.
                this%last_error = "Cannot write to output file '" // &
                                 trim(output_path) // "' (iostat=" // &
                                 trim(int_to_string(stat)) // ")"
                return
            end if
        end if
        
        ! Write HTML content
        write(unit, '(A)') html_content
        
        if (.not. use_stdout) close(unit)
    end subroutine html_generate_report
    
    function html_get_format_name(this) result(format_name)
        class(html_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        format_name = "html"
        
        call suppress_unused_warning(this)
    end function html_get_format_name
    
    function html_supports_diff(this) result(supported)
        class(html_reporter_t), intent(in) :: this
        logical :: supported
        
        ! HTML reporter supports diff through report engine
        supported = .true.
        
        call suppress_unused_warning(this)
    end function html_supports_diff

    ! Mock reporter implementations
    subroutine mock_generate_report(this, coverage_data, output_path, error_flag)
        class(mock_reporter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: error_flag
        
        error_flag = .false.
        this%was_called = .true.
        this%captured_data = coverage_data
        this%captured_output_path = output_path
    end subroutine mock_generate_report

    function mock_get_format_name(this) result(format_name)
        class(mock_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        format_name = "mock"
        
        call suppress_unused_warning(this)
    end function mock_get_format_name

    function mock_supports_diff(this) result(supported)
        class(mock_reporter_t), intent(in) :: this
        logical :: supported
        
        ! Mock reporter can simulate any capability
        supported = .true.
        
        call suppress_unused_warning(this)
    end function mock_supports_diff

    ! DRY helper to suppress unused parameter warnings
    subroutine suppress_unused_warning_reporter(reporter)
        class(coverage_reporter_t), intent(in) :: reporter
        
        ! Use associate construct to avoid unused parameter warning
        associate(dummy => reporter)
        end associate
    end subroutine suppress_unused_warning_reporter

    ! HTML helper functions
    function generate_comprehensive_html_css() result(css)
        character(len=:), allocatable :: css
        
        css = '    <style>' // new_line('a') // &
              '        /* Modern HTML5 reset and base styles */' // new_line('a') // &
              '        * { margin: 0; padding: 0; box-sizing: border-box; }' // new_line('a') // &
              '        body {' // new_line('a') // &
              '            font-family: "SF Mono", Monaco, Inconsolata, "Roboto Mono", monospace;' // new_line('a') // &
              '            background: #1a1a1a; color: #ffffff; line-height: 1.6; margin: 0; padding: 20px;' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .report-header {' // new_line('a') // &
              '            background: linear-gradient(135deg, #ff00ff, #00ffff);' // new_line('a') // &
              '            padding: 2rem; margin-bottom: 2rem; border-radius: 8px;' // new_line('a') // &
              '            box-shadow: 0 4px 12px rgba(0,0,0,0.3);' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .report-header h1 { font-size: 2.5rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.5); }' // new_line('a') // &
              '        .theme-info { font-size: 1.1rem; opacity: 0.9; margin-top: 0.5rem; }' // new_line('a') // &
              '        .cyberpunk {' // new_line('a') // &
              '            border: 2px solid #ff00ff; border-radius: 8px; padding: 1.5rem; margin: 1rem 0;' // new_line('a') // &
              '            background: rgba(255, 0, 255, 0.1); backdrop-filter: blur(5px);' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .summary-grid {' // new_line('a') // &
              '            display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));' // new_line('a') // &
              '            gap: 1rem; margin-top: 1rem;' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .summary-item {' // new_line('a') // &
              '            background: rgba(0, 255, 255, 0.1); padding: 1rem; border-radius: 6px;' // new_line('a') // &
              '            border-left: 4px solid #ff00ff;' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .summary-item.highlight {' // new_line('a') // &
              '            background: rgba(255, 0, 255, 0.2); border-left: 4px solid #00ffff;' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .summary-label { display: block; font-weight: bold; margin-bottom: 0.5rem; }' // new_line('a') // &
              '        .summary-value { font-size: 1.5rem; color: #ff00ff; }' // new_line('a') // &
              '        .coverage-percent { font-weight: bold; text-shadow: 0 0 10px currentColor; }' // new_line('a') // &
              '        .file-list { margin-top: 2rem; }' // new_line('a') // &
              '        .file-item {' // new_line('a') // &
              '            background: rgba(255, 255, 255, 0.05); margin: 0.5rem 0; padding: 1rem;' // new_line('a') // &
              '            border-radius: 6px; border-left: 3px solid #00ffff;' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .file-name { font-weight: bold; color: #ff00ff; }' // new_line('a') // &
              '        .file-coverage { float: right; color: #00ffff; }' // new_line('a') // &
              '        .report-footer {' // new_line('a') // &
              '            margin-top: 3rem; padding: 1.5rem; text-align: center;' // new_line('a') // &
              '            border-top: 1px solid #ff00ff; opacity: 0.8;' // new_line('a') // &
              '        }' // new_line('a') // &
              '        .timestamp { font-size: 0.9rem; margin-top: 0.5rem; opacity: 0.7; }' // new_line('a') // &
              '        @media (max-width: 768px) {' // new_line('a') // &
              '            body { padding: 10px; }' // new_line('a') // &
              '            .report-header h1 { font-size: 2rem; }' // new_line('a') // &
              '            .summary-grid { grid-template-columns: 1fr; }' // new_line('a') // &
              '        }' // new_line('a') // &
              '    </style>'
    end function generate_comprehensive_html_css
    
    function generate_html_file_coverage_details(coverage_data) result(details)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: details
        character(len=:), allocatable :: file_list
        integer :: i, j, total_lines, covered_lines
        real :: file_percentage
        
        ! Generate file list
        file_list = ''
        if (allocated(coverage_data%files) .and. size(coverage_data%files) > 0) then
            do i = 1, size(coverage_data%files)
                ! Calculate file coverage manually
                total_lines = 0
                covered_lines = 0
                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        if (coverage_data%files(i)%lines(j)%is_executable) then
                            total_lines = total_lines + 1
                            if (coverage_data%files(i)%lines(j)%execution_count > 0) then
                                covered_lines = covered_lines + 1
                            end if
                        end if
                    end do
                end if
                
                if (total_lines > 0) then
                    file_percentage = real(covered_lines) / real(total_lines) * 100.0
                else
                    file_percentage = 0.0
                end if
                
                file_list = file_list // &
                    '            <div class="file-item">' // new_line('a') // &
                    '                <span class="file-name">' // trim(coverage_data%files(i)%filename) // '</span>' // new_line('a') // &
                    '                <span class="file-coverage">' // real_to_string(file_percentage) // '%</span>' // new_line('a') // &
                    '                <div style="clear: both;"></div>' // new_line('a') // &
                    '                <div style="font-size: 0.9rem; margin-top: 0.5rem; opacity: 0.8;">' // new_line('a') // &
                    '                    Lines: ' // int_to_string(covered_lines) // '/' // int_to_string(total_lines) // &
                    '                </div>' // new_line('a') // &
                    '            </div>' // new_line('a')
            end do
        else
            file_list = '            <div class="file-item">' // new_line('a') // &
                       '                <span class="file-name">No files found</span>' // new_line('a') // &
                       '                <span class="file-coverage">0.0%</span>' // new_line('a') // &
                       '            </div>' // new_line('a')
        end if
        
        details = '        <section class="file-section">' // new_line('a') // &
                  '            <h2>File Coverage Details</h2>' // new_line('a') // &
                  '            <div class="file-list cyberpunk">' // new_line('a') // &
                  file_list // &
                  '            </div>' // new_line('a') // &
                  '        </section>' // new_line('a')
    end function generate_html_file_coverage_details
    
    function get_html_timestamp() result(timestamp)
        character(len=:), allocatable :: timestamp
        character(len=8) :: date_str
        character(len=10) :: time_str
        
        call date_and_time(date_str, time_str)
        timestamp = date_str(1:4) // '-' // date_str(5:6) // '-' // date_str(7:8) // &
                   ' ' // time_str(1:2) // ':' // time_str(3:4) // ':' // time_str(5:6)
    end function get_html_timestamp


end module coverage_reporter