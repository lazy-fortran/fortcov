module coverage_reporter_text
    !! Text Coverage Reporter Implementation
    !! 
    !! Simple text-based coverage reporter that outputs plain text format.
    !! Provides human-readable coverage statistics without formatting.
    use coverage_model_core
    use coverage_stats_core, only: stats_t => coverage_stats_t
    use coverage_reporter_base
    use coverage_reporter_utils
    use string_utils, only: int_to_string, format_percentage
    implicit none
    private
    
    public :: text_reporter_t
    
    ! Text reporter implementation
    type, extends(coverage_reporter_t) :: text_reporter_t
    contains
        procedure :: generate_report => text_generate_report
        procedure :: get_format_name => text_get_format_name
        procedure :: supports_diff => text_supports_diff
    end type text_reporter_t

contains

    subroutine text_generate_report(this, coverage_data, output_path, &
                                  & success, error_message, &
                                  & diff_data, threshold)
        use zero_config_manager, only: ensure_output_directory_structure
        use error_handling_core, only: error_context_t
        class(text_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        integer :: unit, stat
        logical :: use_stdout
        type(stats_t) :: line_stats, branch_stats, func_stats
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: report_content
        
        success = .true.
        use_stdout = (trim(output_path) == "-")
        
        ! Ensure output directory exists for file output
        if (.not. use_stdout) then
            call ensure_output_directory_structure(output_path, error_ctx)
            if (error_ctx%error_code /= 0) then
                success = .false.
                error_message = trim(error_ctx%message)
                return
            end if
        end if
        
        ! Calculate coverage statistics
        call calculate_manual_line_stats(coverage_data, line_stats)
        call calculate_manual_branch_stats(coverage_data, branch_stats)
        call calculate_manual_function_stats(coverage_data, func_stats)
        
        ! Apply threshold if provided
        if (present(threshold)) then
            if (line_stats%percentage < threshold) then
                success = .false.
                error_message = "Coverage below threshold"
                return
            end if
        end if
        
        ! Generate report content
        report_content = generate_text_report(coverage_data, line_stats, &
                                            branch_stats, func_stats)
        
        ! Open output stream
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            open(newunit=unit, file=output_path, status='replace', &
                 form='formatted', access='sequential', iostat=stat)
            if (stat /= 0) then
                success = .false.
                error_message = "Cannot write to output file '" // &
                              trim(output_path) // "'"
                return
            end if
        end if
        
        ! Write report
        write(unit, '(A)') report_content
        
        if (.not. use_stdout) close(unit)
        
        ! Suppress unused parameter warnings
        call suppress_unused_warning_text(this, diff_data)
        
    end subroutine text_generate_report

    function text_get_format_name(this) result(format_name)
        class(text_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        call suppress_unused_warning_simple_text(this)
        format_name = "text"
        
    end function text_get_format_name

    function text_supports_diff(this) result(supported)
        class(text_reporter_t), intent(in) :: this
        logical :: supported
        
        call suppress_unused_warning_simple_text(this)
        supported = .false.
        
    end function text_supports_diff

    ! Generate text report content
    function generate_text_report(coverage_data, line_stats, branch_stats, &
                                 func_stats) result(report)
        type(coverage_data_t), intent(in) :: coverage_data
        type(stats_t), intent(in) :: line_stats, branch_stats, func_stats
        character(len=:), allocatable :: report
        character(len=:), allocatable :: header, summary, file_details
        
        ! Generate report sections
        header = generate_text_header()
        summary = generate_text_summary(line_stats, branch_stats, func_stats)
        file_details = generate_text_file_details(coverage_data)
        
        ! Combine sections
        report = header // new_line('a') // &
                summary // new_line('a') // &
                file_details
        
    end function generate_text_report

    ! Generate report header
    function generate_text_header() result(header)
        character(len=:), allocatable :: header
        character(len=:), allocatable :: timestamp
        character(len=8) :: date_str
        character(len=10) :: time_str
        
        ! Get current timestamp
        call date_and_time(date_str, time_str)
        timestamp = date_str(1:4) // '-' // date_str(5:6) // '-' // &
                   date_str(7:8) // ' ' // time_str(1:2) // ':' // &
                   time_str(3:4) // ':' // time_str(5:6)
        
        header = "FORTCOV COVERAGE REPORT" // new_line('a') // &
                "=======================" // new_line('a') // &
                "Generated: " // timestamp // new_line('a')
        
    end function generate_text_header

    ! Generate coverage summary
    function generate_text_summary(line_stats, branch_stats, func_stats) &
             result(summary)
        type(stats_t), intent(in) :: line_stats, branch_stats, func_stats
        character(len=:), allocatable :: summary
        
        summary = "COVERAGE SUMMARY" // new_line('a') // &
                 "----------------" // new_line('a') // &
                 "Lines:     " // &
                 int_to_string(line_stats%covered_count) // "/" // &
                 int_to_string(line_stats%total_count) // " (" // &
                 format_percentage(line_stats%percentage, 1) // ")" // &
                 new_line('a') // &
                 "Branches:  " // &
                 int_to_string(branch_stats%covered_count) // "/" // &
                 int_to_string(branch_stats%total_count) // " (" // &
                 format_percentage(branch_stats%percentage, 1) // ")" // &
                 new_line('a') // &
                 "Functions: " // &
                 int_to_string(func_stats%covered_count) // "/" // &
                 int_to_string(func_stats%total_count) // " (" // &
                 format_percentage(func_stats%percentage, 1) // ")" // &
                 new_line('a')
        
    end function generate_text_summary

    ! Generate file-by-file details
    function generate_text_file_details(coverage_data) result(details)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: details
        character(len=:), allocatable :: file_list
        integer :: i
        real :: file_coverage
        
        details = "FILE DETAILS" // new_line('a') // &
                 "------------" // new_line('a')
        
        file_list = ""
        if (allocated(coverage_data%files) .and. size(coverage_data%files) > 0) then
            do i = 1, size(coverage_data%files)
                ! Calculate file coverage
                if (coverage_data%files(i)%total_lines > 0) then
                    file_coverage = real(coverage_data%files(i)%covered_lines) / &
                                   real(coverage_data%files(i)%total_lines) * 100.0
                else
                    file_coverage = 0.0
                end if
                
                file_list = file_list // &
                    trim(coverage_data%files(i)%filename) // ": " // &
                    int_to_string(coverage_data%files(i)%covered_lines) // "/" // &
                    int_to_string(coverage_data%files(i)%total_lines) // " (" // &
                    format_percentage(file_coverage, 1) // ")" // new_line('a')
            end do
        else
            file_list = "No files found" // new_line('a')
        end if
        
        details = details // file_list
        
    end function generate_text_file_details

    ! Helper subroutines for unused parameter warnings
    subroutine suppress_unused_warning_text(reporter, diff_data)
        class(text_reporter_t), intent(in) :: reporter
        type(coverage_diff_t), intent(in), optional :: diff_data
        associate(r => reporter); end associate
        if (present(diff_data)) then
            associate(d => diff_data); end associate
        end if
    end subroutine suppress_unused_warning_text
    
    subroutine suppress_unused_warning_simple_text(reporter)
        class(text_reporter_t), intent(in) :: reporter
        associate(r => reporter); end associate
    end subroutine suppress_unused_warning_simple_text

end module coverage_reporter_text