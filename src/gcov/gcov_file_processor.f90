module gcov_file_processor
    use coverage_model_core
    use error_handling_core
    use iostat_utilities
    use gcov_line_parser
    use coverage_data_builder
    implicit none
    private
    
    ! Public procedures
    public :: process_gcov_file
    
contains

    ! Main entry point for processing gcov files
    subroutine process_gcov_file(path, coverage_data, error_flag)
        character(len=*), intent(in) :: path
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_flag
        
        integer, parameter :: unit = 10
        type(coverage_file_t), allocatable :: files_array(:)
        integer :: files_count
        
        ! Initialize parser state
        call initialize_parser_state(error_flag, coverage_data, files_array, files_count)
        if (error_flag) return
        
        ! Open and parse the gcov file
        call open_and_parse_gcov_file(unit, path, files_array, files_count, error_flag)
        if (error_flag) return
        
        ! Build final coverage data
        call build_coverage_data_from_files(coverage_data, files_array, files_count, &
                                           error_flag)
    end subroutine process_gcov_file
    
    ! Initialize parser state with empty arrays and defaults
    subroutine initialize_parser_state(error_flag, coverage_data, files_array, files_count)
        logical, intent(out) :: error_flag
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_file_t), allocatable, intent(out) :: files_array(:)
        integer, intent(out) :: files_count
        
        integer :: stat
        character(len=512) :: errmsg
        
        error_flag = .false.
        coverage_data = coverage_data_t()
        files_count = 0
        allocate(coverage_file_t :: files_array(10), stat=stat, errmsg=errmsg)  ! Initial capacity
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to allocate files_array: " // trim(errmsg)
            error_flag = .true.
            return
        end if
    end subroutine initialize_parser_state
    
    ! Open gcov file and parse all content line by line
    subroutine open_and_parse_gcov_file(unit, path, files_array, files_count, error_flag)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: path
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        integer, intent(inout) :: files_count
        logical, intent(inout) :: error_flag

        integer :: iostat_val
        character(len=256) :: line
        character(len=:), allocatable :: source_filename
        type(coverage_line_t), allocatable :: lines_array(:)
        integer :: lines_count
        logical :: has_source
        integer :: stat
        character(len=512) :: errmsg
        logical :: file_opened

        ! Initialize file parsing state
        lines_count = 0
        has_source = .false.
        source_filename = "unknown"
        file_opened = .false.

        allocate(coverage_line_t :: lines_array(100), stat=stat, errmsg=errmsg)  ! Initial capacity
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to allocate lines_array: " // trim(errmsg)
            error_flag = .true.
            return
        end if

        ! Open file with error handling
        call open_gcov_file_with_validation(unit, path, error_flag)
        if (error_flag) return
        file_opened = .true.

        ! Process each line
        call process_gcov_lines(unit, path, source_filename, lines_array, lines_count, &
                               files_array, files_count, has_source)

        ! Ensure file is always closed
        if (file_opened) then
            close(unit)
        end if

        ! Add final file if we have data
        if (has_source .and. lines_count > 0) then
            call add_file_to_array(files_array, files_count, source_filename, &
                                 lines_array(1:lines_count))
        end if
    end subroutine open_and_parse_gcov_file
    
    ! Open gcov file with enhanced error handling
    subroutine open_gcov_file_with_validation(unit, path, error_flag)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        
        integer :: iostat_val
        
        open(unit, file=path, status="old", iostat=iostat_val)
        if (iostat_val /= 0) then
            error_flag = .true.
            block
                type(error_context_t) :: error_ctx
                call interpret_iostat_open_error(iostat_val, path, error_ctx)
                call log_error(error_ctx)
            end block
        else
            error_flag = .false.
        end if
    end subroutine open_gcov_file_with_validation
    
    ! Process all lines from gcov file
    ! Handles function block separators (------------------) to avoid duplicate lines
    subroutine process_gcov_lines(unit, path, source_filename, lines_array, &
                                 lines_count, files_array, files_count, has_source)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(inout) :: source_filename
        type(coverage_line_t), allocatable, intent(inout) :: lines_array(:)
        integer, intent(inout) :: lines_count
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        integer, intent(inout) :: files_count
        logical, intent(inout) :: has_source

        character(len=256) :: line
        integer :: iostat_val
        logical :: in_function_block

        in_function_block = .false.

        do
            call read_gcov_line_with_validation(unit, path, line, iostat_val)
            if (iostat_val /= 0) exit

            line = trim(line)
            if (len(line) == 0) cycle

            ! Detect function block separator (line of dashes)
            ! gcov outputs: ------------------ to separate function-level data
            if (is_function_block_separator(line)) then
                in_function_block = .true.
                cycle
            end if

            ! New Source: header resets function block state
            if (index(line, "Source:") > 0) then
                in_function_block = .false.
            end if

            ! Skip lines inside function blocks (they duplicate main file data)
            if (in_function_block) cycle

            call parse_gcov_line(line, path, source_filename, lines_array, &
                               lines_count, files_array, files_count, has_source)
        end do
    end subroutine process_gcov_lines

    ! Check if line is a function block separator (line of dashes)
    pure function is_function_block_separator(line) result(is_separator)
        character(len=*), intent(in) :: line
        logical :: is_separator
        integer :: i, dash_count, line_len

        is_separator = .false.
        line_len = len_trim(line)

        ! Must have at least 10 dashes to be a separator
        if (line_len < 10) return

        dash_count = 0
        do i = 1, line_len
            if (line(i:i) == '-') then
                dash_count = dash_count + 1
            else
                ! Non-dash character found, not a separator
                return
            end if
        end do

        ! Valid separator has 10+ consecutive dashes and nothing else
        is_separator = (dash_count >= 10)
    end function is_function_block_separator
    
    ! Read single line from gcov file with error handling
    subroutine read_gcov_line_with_validation(unit, path, line, iostat_val)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: path
        character(len=256), intent(out) :: line
        integer, intent(out) :: iostat_val
        
        read(unit, '(A)', iostat=iostat_val) line
        if (iostat_val /= 0 .and. iostat_val /= -1) then
            block
                type(error_context_t) :: error_ctx
                call interpret_iostat_read_error(iostat_val, path, error_ctx)
                call log_error(error_ctx)
            end block
        end if
    end subroutine read_gcov_line_with_validation

end module gcov_file_processor