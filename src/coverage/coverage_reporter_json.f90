module coverage_reporter_json
    !! JSON Coverage Reporter Implementation
    !! 
    !! Extracted from coverage_reporter_impl for Issue #182 module size compliance.
    use coverage_model_core
    use coverage_stats_core, only: stats_t => coverage_stats_t
    use coverage_reporter_base
    use coverage_reporter_utils
    implicit none
    private
    
    public :: json_reporter_t
    
    ! JSON reporter implementation  
    type, extends(coverage_reporter_t) :: json_reporter_t
        character(len=:), allocatable :: last_error
    contains
        procedure :: generate_report => json_generate_report
        procedure :: get_format_name => json_get_format_name
        procedure :: supports_diff => json_supports_diff
    end type json_reporter_t

contains

    subroutine json_generate_report(this, coverage_data, output_path, &
                                  & success, error_message, &
                                  & diff_data, threshold)
        use zero_config_manager, only: ensure_output_directory_structure
        use error_handling_core, only: error_context_t
        class(json_reporter_t), intent(in) :: this
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
        
        success = .true.
        use_stdout = (trim(output_path) == "-")
        
        ! Ensure output directory exists for file output (Issue #204 zero-configuration support)
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
        
        ! Generate JSON using optimized approach
        call write_json_optimized(unit, coverage_data, line_stats, &
                                  branch_stats, func_stats)
        
        if (.not. use_stdout) close(unit)
        
        ! Suppress unused parameter warnings
        call suppress_unused_warning(this, diff_data)
        
    end subroutine json_generate_report

    function json_get_format_name(this) result(format_name)
        class(json_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        call suppress_unused_warning_simple(this)
        format_name = "json"
        
    end function json_get_format_name

    function json_supports_diff(this) result(supported)
        class(json_reporter_t), intent(in) :: this
        logical :: supported
        
        call suppress_unused_warning_simple(this)
        supported = .false.
        
    end function json_supports_diff

    subroutine write_json_optimized(unit, coverage_data, line_stats, &
                                   branch_stats, func_stats)
        integer, intent(in) :: unit
        type(coverage_data_t), intent(in) :: coverage_data
        type(stats_t), intent(in) :: line_stats, branch_stats, func_stats
        
        ! High-performance JSON generation - single-pass, minimal memory allocation
        ! This format is compatible with both diff parser and export functions
        write(unit, '(A)') '{'
        write(unit, '(A,F0.2,A)') '  "line_coverage": ', line_stats%percentage, ','
        write(unit, '(A,I0,A)') '  "lines_covered": ', line_stats%covered_count, ','
        write(unit, '(A,I0,A)') '  "lines_total": ', line_stats%total_count, ','
        write(unit, '(A,F0.2,A)') '  "branch_coverage": ', branch_stats%percentage, ','
        write(unit, '(A,I0,A)') '  "branches_covered": ', branch_stats%covered_count, ','
        write(unit, '(A,I0,A)') '  "branches_total": ', branch_stats%total_count, ','
        write(unit, '(A,F0.2,A)') '  "function_coverage": ', func_stats%percentage, ','
        write(unit, '(A,I0,A)') '  "functions_covered": ', func_stats%covered_count, ','
        write(unit, '(A,I0,A)') '  "functions_total": ', func_stats%total_count, ','
        
        ! File-level details
        write(unit, '(A)') '  "files": ['
        call write_file_details_json(unit, coverage_data)
        write(unit, '(A)') '  ]'
        write(unit, '(A)') '}'
        
    end subroutine write_json_optimized

    subroutine write_file_details_json(unit, coverage_data)
        integer, intent(in) :: unit
        type(coverage_data_t), intent(in) :: coverage_data
        integer :: i, j
        
        if (.not. allocated(coverage_data%files)) return
        
        do i = 1, size(coverage_data%files)
            write(unit, '(A)', advance='no') '    {'
            write(unit, '(A)', advance='no') '"name": "' // trim(coverage_data%files(i)%filename) // '",'
            write(unit, '(A)', advance='no') '"lines": ['
            
            if (allocated(coverage_data%files(i)%lines)) then
                do j = 1, size(coverage_data%files(i)%lines)
                    if (j > 1) write(unit, '(A)', advance='no') ','
                    write(unit, '(A,I0,A,I0,A)', advance='no') &
                        '{"line": ', coverage_data%files(i)%lines(j)%line_number, &
                        ', "count": ', coverage_data%files(i)%lines(j)%execution_count, '}'
                end do
            end if
            
            write(unit, '(A)', advance='no') ']}'
            if (i < size(coverage_data%files)) write(unit, '(A)') ','
            write(unit, '(A)') ''
        end do
        
    end subroutine write_file_details_json

    ! Helper subroutines for unused parameter warnings
    subroutine suppress_unused_warning(reporter, diff_data)
        class(json_reporter_t), intent(in) :: reporter
        type(coverage_diff_t), intent(in), optional :: diff_data
        associate(r => reporter); end associate
        if (present(diff_data)) then
            associate(d => diff_data); end associate
        end if
    end subroutine suppress_unused_warning
    
    subroutine suppress_unused_warning_simple(reporter)
        class(json_reporter_t), intent(in) :: reporter
        associate(r => reporter); end associate
    end subroutine suppress_unused_warning_simple

end module coverage_reporter_json