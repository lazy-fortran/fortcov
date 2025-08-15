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
        class(html_reporter_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: error_flag
        logical :: init_success
        character(len=:), allocatable :: init_error
        
        error_flag = .false.
        
        ! Initialize report engine if not already done
        if (.not. this%initialized) then
            call this%engine%init(init_success, init_error)
            if (.not. init_success) then
                this%last_error = "Failed to initialize HTML report engine: " // init_error
                error_flag = .true.
                return
            end if
            this%initialized = .true.
        end if
        
        ! Set source data in the engine
        this%engine%source_data = coverage_data
        
        ! Generate HTML report using the report engine
        call this%engine%generate_html_report(output_path, init_success, init_error)
        if (.not. init_success) then
            this%last_error = "Failed to generate HTML report: " // init_error
            error_flag = .true.
            return
        end if
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


end module coverage_reporter