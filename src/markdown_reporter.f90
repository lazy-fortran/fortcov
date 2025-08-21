module markdown_reporter
    use coverage_model, only: source_location_t, coverage_line_t, &
                              coverage_branch_t, coverage_function_t, &
                              coverage_file_t, coverage_data_t, &
                              coverage_diff_t, line_diff_t, file_diff_t, &
                              line_coverage_t, file_coverage_t, &
                              calculate_statistics, merge_coverage, &
                              compare_coverage
    use coverage_statistics, only: coverage_stats_t, calculate_line_coverage
    use string_utils
    implicit none
    private
    
    ! Public types and procedures
    public :: markdown_report_options_t
    public :: generate_markdown_report
    
    ! Maximum line length for missing ranges before truncation
    integer, parameter :: MAX_MISSING_LENGTH = 200
    
    ! Report generation options
    type :: markdown_report_options_t
        character(len=:), allocatable :: sort_by
        logical :: show_modules = .false.
        logical :: include_total = .true.
    contains
        procedure :: init => options_init
    end type markdown_report_options_t
    
contains

    ! Initialize markdown report options with defaults
    subroutine options_init(this)
        class(markdown_report_options_t), intent(inout) :: this
        
        this%sort_by = "name"
        this%show_modules = .false.
        this%include_total = .true.
    end subroutine options_init

    ! Generate complete markdown coverage report
    function generate_markdown_report(coverage_data, options) result(report)
        type(coverage_data_t), intent(in) :: coverage_data
        type(markdown_report_options_t), intent(in) :: options
        character(len=:), allocatable :: report
        character(len=:), allocatable :: header, body, total_row
        type(coverage_stats_t) :: file_stats, total_stats
        integer :: i
        integer, allocatable :: sort_indices(:)
        
        ! Initialize report with header
        header = generate_table_header()
        body = ""
        
        ! Sort files if requested
        sort_indices = get_sorted_indices(coverage_data, options%sort_by)
        
        ! Generate rows for each file
        do i = 1, size(sort_indices)
            associate(file => coverage_data%files(sort_indices(i)))
                file_stats = calculate_file_coverage(file)
                body = body // generate_table_row(file%filename, file_stats)
                body = body // new_line('A')
            end associate
        end do
        
        ! Add total row if requested
        if (options%include_total) then
            ! Calculate totals manually from coverage_data
            call calculate_total_stats(coverage_data, total_stats)
            total_row = generate_table_row("TOTAL", total_stats)
            body = body // total_row
        end if
        
        report = header // new_line('A') // body
    end function generate_markdown_report

    ! Generate table header with column names and separator
    function generate_table_header() result(header)
        character(len=:), allocatable :: header
        
        header = "| Filename | Stmts | Covered | Cover | Missing |" // &
                 new_line('A') // &
                 "|----------|-------|---------|-------|---------|"
    end function generate_table_header

    ! Generate a single table row for a file
    function generate_table_row(filename, stats) result(row)
        character(len=*), intent(in) :: filename
        type(coverage_stats_t), intent(in) :: stats
        character(len=:), allocatable :: row
        character(len=:), allocatable :: escaped_name, percentage_str, missing_str
        character(len=:), allocatable :: clean_filename
        integer :: missed_count
        
        ! Clean filename by removing "0:Source:" prefix
        clean_filename = clean_gcov_filename(filename)
        
        ! Escape markdown special characters in filename
        escaped_name = escape_markdown(clean_filename)
        
        ! Format percentage
        percentage_str = format_percentage(real(stats%percentage), 2)
        
        ! Calculate missed count
        missed_count = stats%total_count - stats%covered_count
        
        ! Format missing ranges
        if (len_trim(stats%missing_ranges) > 0) then
            if (len_trim(stats%missing_ranges) > MAX_MISSING_LENGTH) then
                missing_str = stats%missing_ranges(1:MAX_MISSING_LENGTH-3) // "..."
            else
                missing_str = trim(stats%missing_ranges)
            end if
        else
            missing_str = ""
        end if
        
        ! Build the row
        row = "| " // escaped_name // " | " // &
              int_to_string(stats%total_count) // " | " // &
              int_to_string(stats%covered_count) // " | " // &
              percentage_str // " | " // &
              missing_str // " |"
    end function generate_table_row

    ! Calculate coverage statistics for a single file
    function calculate_file_coverage(file) result(stats)
        type(coverage_file_t), intent(in) :: file
        type(coverage_stats_t) :: stats
        integer :: total_lines, covered_lines, line_idx
        integer, allocatable :: uncovered_lines(:)
        integer :: uncovered_count
        
        ! Use file's built-in fields which are working correctly
        total_lines = file%total_lines
        covered_lines = file%covered_lines
        
        ! Count uncovered lines for missing ranges
        uncovered_count = total_lines - covered_lines
        
        ! Collect uncovered line numbers
        if (uncovered_count > 0) then
            allocate(uncovered_lines(uncovered_count))
            uncovered_count = 0  ! Reset for collection
            do line_idx = 1, size(file%lines)
                if (file%lines(line_idx)%is_executable .and. &
                    .not. file%lines(line_idx)%is_covered()) then
                    uncovered_count = uncovered_count + 1
                    uncovered_lines(uncovered_count) = file%lines(line_idx)%line_number
                end if
            end do
        else
            allocate(uncovered_lines(0))
        end if
        
        ! Calculate statistics
        if (total_lines == 0) then
            ! Handle empty files properly - show 0% coverage, not 100%
            call stats%init(0.0, 0, 0, "")
        else
            call stats%init(file%get_line_coverage(), &
                           covered_lines, total_lines, &
                           compress_ranges(uncovered_lines))
        end if
        
        deallocate(uncovered_lines)
    end function calculate_file_coverage

    ! Get sorted indices for files based on sort criteria
    function get_sorted_indices(coverage_data, sort_by) result(indices)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: sort_by
        integer, allocatable :: indices(:)
        integer :: n, i, j, min_idx
        character(len=:), allocatable :: temp_filename
        
        n = size(coverage_data%files)
        allocate(indices(n))
        
        ! Initialize indices
        do i = 1, n
            indices(i) = i
        end do
        
        ! Sort based on criteria
        select case (trim(sort_by))
        case ("name")
            ! Simple selection sort by filename
            do i = 1, n - 1
                min_idx = i
                do j = i + 1, n
                    if (coverage_data%files(indices(j))%filename < &
                        coverage_data%files(indices(min_idx))%filename) then
                        min_idx = j
                    end if
                end do
                if (min_idx /= i) then
                    ! Swap indices
                    j = indices(i)
                    indices(i) = indices(min_idx)
                    indices(min_idx) = j
                end if
            end do
        case default
            ! No sorting - keep original order
        end select
    end function get_sorted_indices

    ! Escape markdown special characters
    function escape_markdown(input) result(escaped)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: escaped
        character(len=len(input)*2) :: work_str
        integer :: i, out_pos, in_len
        
        work_str = ""
        out_pos = 1
        in_len = len_trim(input)
        
        ! Replace pipe characters with escaped version
        do i = 1, in_len
            if (input(i:i) == "|") then
                work_str(out_pos:out_pos+1) = "\|"
                out_pos = out_pos + 2
            else
                work_str(out_pos:out_pos) = input(i:i)
                out_pos = out_pos + 1
            end if
        end do
        
        escaped = work_str(1:out_pos-1)
    end function escape_markdown

    ! Convert integer to string
    function int_to_string(int_val) result(str)
        integer, intent(in) :: int_val
        character(len=:), allocatable :: str
        character(len=20) :: buffer
        
        write(buffer, '(I0)') int_val
        str = trim(buffer)
    end function int_to_string

    ! Clean gcov filename by removing "0:Source:" prefix
    function clean_gcov_filename(filename) result(cleaned)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: cleaned
        integer :: colon_pos
        
        ! Find the pattern "0:Source:"
        colon_pos = index(filename, ":Source:")
        if (colon_pos > 0) then
            ! Remove the "0:Source:" prefix
            cleaned = filename(colon_pos + 8:)
        else
            ! No prefix found, return as-is
            cleaned = trim(filename)
        end if
    end function clean_gcov_filename

    ! Calculate total statistics across all files
    subroutine calculate_total_stats(coverage_data, total_stats)
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_stats_t), intent(out) :: total_stats
        integer :: total_lines, covered_lines, i
        real :: percentage
        
        total_lines = 0
        covered_lines = 0
        
        ! Sum across all files
        do i = 1, size(coverage_data%files)
            total_lines = total_lines + coverage_data%files(i)%total_lines
            covered_lines = covered_lines + coverage_data%files(i)%covered_lines
        end do
        
        ! Calculate percentage
        if (total_lines > 0) then
            percentage = real(covered_lines) / real(total_lines) * 100.0
        else
            ! Handle empty project properly - show 0% coverage, not 100%
            percentage = 0.0
        end if
        
        ! Initialize stats
        call total_stats%init(percentage, covered_lines, total_lines, "")
    end subroutine calculate_total_stats

end module markdown_reporter
