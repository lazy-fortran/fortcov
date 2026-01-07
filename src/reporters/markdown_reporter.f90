module markdown_reporter
    use coverage_model_core, only: coverage_file_t, coverage_data_t
    use coverage_stats_core, only: coverage_stats_t
    use string_utils, only: int_to_string, compress_ranges, format_percentage
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
        integer :: total_lines, covered_lines, line_idx, stat
        integer, allocatable :: uncovered_lines(:)
        integer :: uncovered_count
        character(len=256) :: errmsg
        
        ! Use file's built-in fields which are working correctly
        total_lines = file%total_lines
        covered_lines = file%covered_lines
        
        ! Count uncovered lines for missing ranges
        uncovered_count = total_lines - covered_lines
        
        ! Collect uncovered line numbers
        if (uncovered_count > 0) then
            allocate(uncovered_lines(uncovered_count), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                ! Graceful fallback - return basic stats without ranges
                call stats%init(file%get_line_coverage(), covered_lines, total_lines, "")
                return
            end if
            uncovered_count = 0  ! Reset for collection
            do line_idx = 1, size(file%lines)
                if (file%lines(line_idx)%is_executable .and. &
                    .not. file%lines(line_idx)%is_covered()) then
                    uncovered_count = uncovered_count + 1
                    uncovered_lines(uncovered_count) = file%lines(line_idx)%line_number
                end if
            end do
        else
            allocate(uncovered_lines(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                ! Graceful fallback for empty allocation failure
                call stats%init(file%get_line_coverage(), covered_lines, total_lines, "")
                return
            end if
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
        
        if (allocated(uncovered_lines)) then
            deallocate(uncovered_lines, stat=stat, errmsg=errmsg)
        end if
    end function calculate_file_coverage

    ! Get sorted indices for files based on sort criteria
    function get_sorted_indices(coverage_data, sort_by) result(indices)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: sort_by
        integer, allocatable :: indices(:)
        integer :: n, i, j, min_idx, stat
        character(len=:), allocatable :: temp_filename
        character(len=256) :: errmsg
        
        n = size(coverage_data%files)
        allocate(indices(n), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            ! Graceful fallback - return empty array for sorting failure
            allocate(indices(0))
            return
        end if
        
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
        
        ! Handle empty input defensively to avoid zero-length slicing
        if (in_len <= 0) then
            escaped = ""
            return
        end if
        
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


    ! Clean gcov filename by removing "0:Source:" prefix, collapsing
    ! duplicate path separators, and stripping duplicated extensions
    function clean_gcov_filename(filename) result(cleaned)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: cleaned
        character(len=:), allocatable :: work
        character(len=:), allocatable :: result_path
        integer :: colon_pos
        integer :: i, out_pos, in_len
        integer :: last_dot, prev_dot
        character(len=:), allocatable :: ext_last, ext_prev
        character(len=:), allocatable :: base_no_dup_ext
        character(len=:), allocatable :: tmp
        
        ! Step 1: remove gcov source prefix if present
        colon_pos = index(filename, ":Source:")
        if (colon_pos > 0) then
            work = filename(colon_pos + 8:)
        else
            work = trim(filename)
        end if

        ! Defensive empty handling
        if (len_trim(work) == 0) then
            cleaned = ""
            return
        end if

        ! Step 2: collapse duplicate '/' path separators
        tmp = work
        result_path = repeat(' ', len(tmp))
        out_pos = 1
        in_len = len_trim(tmp)
        do i = 1, in_len
            if (tmp(i:i) == '/') then
                if (i < in_len) then
                    if (tmp(i+1:i+1) == '/') cycle
                end if
            end if
            result_path(out_pos:out_pos) = tmp(i:i)
            out_pos = out_pos + 1
        end do
        work = result_path(1:out_pos-1)

        ! Step 3: strip duplicated extensions at the end (e.g., .f90.f90)
        last_dot = index(work, '.', back=.true.)
        if (last_dot > 0) then
            prev_dot = index(work(1:last_dot-1), '.', back=.true.)
        else
            prev_dot = 0
        end if
        if (last_dot > 0 .and. prev_dot > 0) then
            ext_last = work(last_dot+1:)
            ext_prev = work(prev_dot+1:last_dot-1)
            if (len_trim(ext_last) > 0 .and. trim(ext_last) == trim(ext_prev)) then
                base_no_dup_ext = work(1:last_dot-1)
                work = base_no_dup_ext
            end if
        end if

        cleaned = work
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
