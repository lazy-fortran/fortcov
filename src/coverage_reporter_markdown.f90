module coverage_reporter_markdown
    !! Markdown Coverage Reporter Implementation
    !! 
    !! Extracted from coverage_reporter_impl for Issue #182 module size compliance.
    use coverage_model
    use coverage_statistics, only: stats_t => coverage_stats_t
    use markdown_reporter, only: generate_markdown_report, markdown_report_options_t
    use coverage_reporter_base
    use coverage_reporter_utils
    implicit none
    private
    
    public :: markdown_reporter_t
    
    ! Markdown reporter implementation
    type, extends(coverage_reporter_t) :: markdown_reporter_t
        logical :: include_diff = .false.
    contains
        procedure :: generate_report => markdown_generate_report
        procedure :: get_format_name => markdown_get_format_name
        procedure :: supports_diff => markdown_supports_diff
    end type markdown_reporter_t

contains

    subroutine markdown_generate_report(this, coverage_data, output_path, &
                                      & success, error_message, &
                                      & diff_data, threshold)
        use zero_configuration_manager, only: ensure_output_directory_structure
        use error_handling, only: error_context_t
        class(markdown_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        type(markdown_report_options_t) :: options
        type(stats_t) :: stats
        type(error_context_t) :: error_ctx
        
        success = .false.
        
        ! Ensure output directory exists (Issue #204 zero-configuration support)
        call ensure_output_directory_structure(output_path, error_ctx)
        if (error_ctx%error_code /= 0) then
            error_message = trim(error_ctx%message)
            return
        end if
        
        ! Configure markdown options
        call options%init()
        
        ! Calculate coverage statistics
        call calculate_manual_line_stats(coverage_data, stats)
        
        ! Apply threshold if provided
        if (present(threshold)) then
            if (stats%percentage < threshold) then
                error_message = "Coverage below threshold"
                return
            end if
        end if
        
        ! Generate the markdown report
        block
            character(len=:), allocatable :: report_content
            integer :: unit, stat
            
            report_content = generate_markdown_report(coverage_data, options)
            
            ! Write to file
            open(newunit=unit, file=output_path, status='replace', &
                 form='formatted', access='sequential', iostat=stat)
            if (stat /= 0) then
                success = .false.
                error_message = "Cannot write to output file '" // &
                              trim(output_path) // "'"
                return
            end if
            
            write(unit, '(A)') report_content
            close(unit)
            
            success = .true.
        end block
        
        ! Suppress unused parameter warnings
        call suppress_unused_warning_diff_data(diff_data)
        
    end subroutine markdown_generate_report

    function markdown_get_format_name(this) result(format_name)
        class(markdown_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        call suppress_unused_warning_reporter(this)
        format_name = "markdown"
        
    end function markdown_get_format_name

    function markdown_supports_diff(this) result(supported)
        class(markdown_reporter_t), intent(in) :: this
        logical :: supported
        
        call suppress_unused_warning_reporter(this)
        supported = .true.
        
    end function markdown_supports_diff

    ! DRY helper for unused parameter warnings
    subroutine suppress_unused_warning_reporter(reporter)
        class(coverage_reporter_t), intent(in) :: reporter
        ! This subroutine intentionally does nothing to suppress unused warnings
        associate(r => reporter); end associate
    end subroutine suppress_unused_warning_reporter

    subroutine suppress_unused_warning_diff_data(diff_data)
        type(coverage_diff_t), intent(in), optional :: diff_data
        if (present(diff_data)) then
            associate(d => diff_data); end associate
        end if
    end subroutine suppress_unused_warning_diff_data


end module coverage_reporter_markdown