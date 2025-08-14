module coverage_parser
    use coverage_model
    implicit none
    private
    
    ! Public types
    public :: coverage_parser_t
    public :: gcov_parser_t
    public :: mock_parser_t
    
    ! Public procedures
    public :: create_parser
    
    ! Abstract parser interface
    type, abstract :: coverage_parser_t
    contains
        procedure(parse_interface), deferred :: parse
        procedure(can_parse_interface), deferred :: can_parse
        procedure(get_required_files_interface), deferred :: get_required_files
    end type coverage_parser_t
    
    ! Concrete GCov parser implementation
    type, extends(coverage_parser_t) :: gcov_parser_t
    contains
        procedure :: parse => gcov_parse
        procedure :: can_parse => gcov_can_parse
        procedure :: get_required_files => gcov_get_required_files
    end type gcov_parser_t
    
    ! Mock parser for testing
    type, extends(coverage_parser_t) :: mock_parser_t
        type(coverage_data_t) :: injected_data
    contains
        procedure :: parse => mock_parse
        procedure :: can_parse => mock_can_parse
        procedure :: get_required_files => mock_get_required_files
        procedure :: inject_data => mock_inject_data
    end type mock_parser_t
    
    ! Abstract interfaces
    abstract interface
        function parse_interface(this, path, error_flag) result(coverage_data)
            import :: coverage_parser_t, coverage_data_t
            class(coverage_parser_t), intent(in) :: this
            character(len=*), intent(in) :: path
            logical, intent(out) :: error_flag
            type(coverage_data_t) :: coverage_data
        end function parse_interface
        
        function can_parse_interface(this, path) result(supported)
            import :: coverage_parser_t
            class(coverage_parser_t), intent(in) :: this
            character(len=*), intent(in) :: path
            logical :: supported
        end function can_parse_interface
        
        function get_required_files_interface(this) result(extensions)
            import :: coverage_parser_t
            class(coverage_parser_t), intent(in) :: this
            character(len=:), allocatable :: extensions(:)
        end function get_required_files_interface
    end interface

contains

    ! Factory function to create parser based on file extension
    subroutine create_parser(path, parser, error_flag)
        character(len=*), intent(in) :: path
        class(coverage_parser_t), allocatable, intent(out) :: parser
        logical, intent(out) :: error_flag
        character(len=10) :: extension
        integer :: dot_pos
        
        error_flag = .false.
        
        ! Extract file extension
        dot_pos = index(path, ".", back=.true.)
        if (dot_pos > 0) then
            extension = path(dot_pos:)
        else
            extension = ""
        end if
        
        ! Select parser based on extension  
        ! Select parser based on file extension
        select case (trim(extension))
        case (".gcov")
            allocate(gcov_parser_t :: parser)
        case (".gcda", ".gcno")
            ! Binary formats not supported - use gcov text output
            error_flag = .true.
        case default
            ! Unsupported file format
            error_flag = .true.
        end select
    end subroutine create_parser

    ! GCov parser implementation - text parsing
    function gcov_parse(this, path, error_flag) result(coverage_data)
        use string_utils
        class(gcov_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        type(coverage_data_t) :: coverage_data
        
        ! Local variables for parsing
        integer, parameter :: unit = 10
        character(len=256) :: line
        integer :: iostat_val, line_num, exec_count
        character(len=:), allocatable :: source_filename, line_content
        character(len=:), allocatable :: parts(:)
        type(coverage_line_t), allocatable :: lines_array(:)
        type(coverage_file_t), allocatable :: files_array(:)
        integer :: lines_count, files_count
        logical :: is_executable, has_source
        
        ! Initialize output
        error_flag = .false.
        coverage_data = coverage_data_t()
        lines_count = 0
        files_count = 0
        has_source = .false.
        source_filename = "unknown"
        allocate(coverage_line_t :: lines_array(100)) ! Initial capacity for lines
        allocate(coverage_file_t :: files_array(10))    ! Initial capacity for files
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
        
        ! Open and parse the gcov file
        open(unit, file=path, status="old", iostat=iostat_val)
        if (iostat_val /= 0) then
            error_flag = .true.
            return
        end if
        
        ! Parse line by line
        do
            read(unit, '(A)', iostat=iostat_val) line
            if (iostat_val /= 0) exit ! End of file or error
            
            line = trim(line)
            if (len(line) == 0) cycle ! Skip empty lines
            
            ! Parse header lines
            if (index(line, "Source:") > 0) then
                ! New source file - save previous file if we have data
                if (has_source .and. lines_count > 0) then
                    call add_file_to_array(files_array, files_count, source_filename, &
                                         lines_array(1:lines_count))
                    lines_count = 0  ! Reset for new file
                end if
                
                ! Extract source filename from "Source:filename" pattern
                source_filename = extract_source_filename(line)
                has_source = .true.
                cycle
            else if (index(line, "Graph:") > 0 .or. index(line, "Data:") > 0) then
                ! Skip graph and data header lines
                cycle
            else if (index(line, "function ") > 0 .and. index(line, " called ") > 0) then
                ! Skip function summary lines
                cycle
            end if
            
            ! Parse coverage data lines (format: execution_count:line_number:content)
            parts = split(line, ":")
            if (size(parts) < 3) cycle ! Invalid format, skip
            
            ! Extract execution count (trim whitespace)
            if (trim(adjustl(parts(1))) == "-") then
                exec_count = -1  ! Non-executable line
                is_executable = .false.
            else if (trim(adjustl(parts(1))) == "#####") then
                exec_count = 0   ! Unexecuted line
                is_executable = .true.
            else
                read(parts(1), *, iostat=iostat_val) exec_count
                if (iostat_val /= 0) cycle ! Invalid number, skip
                is_executable = .true.
            end if
            
            ! Extract line number (trim whitespace)
            read(parts(2), *, iostat=iostat_val) line_num
            if (iostat_val /= 0) cycle ! Invalid line number, skip
            
            ! Skip line 0 (header lines)
            if (line_num == 0) cycle
            
            ! Extract line content (join remaining parts with ":")
            line_content = join_parts(parts, 3)
            
            ! Create coverage line
            lines_count = lines_count + 1
            if (lines_count > size(lines_array)) then
                ! Resize array if needed
                call resize_lines_array(lines_array)
            end if
            
            call lines_array(lines_count)%init(exec_count, line_num, &
                                             source_filename, is_executable)
        end do
        
        close(unit)
        
        ! Add the final file if we have data
        if (has_source .and. lines_count > 0) then
            call add_file_to_array(files_array, files_count, source_filename, &
                                 lines_array(1:lines_count))
        end if
        
        ! Build coverage data if we have valid files
        if (files_count > 0) then
            ! Create coverage data with all files
            call coverage_data%init(files_array(1:files_count))
        else
            ! Set error flag if we have no useful data
            error_flag = .true.
        end if
        
    end function gcov_parse

    ! Helper function to extract source filename from "Source:filename" line
    function extract_source_filename(line) result(filename)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: filename
        integer :: colon_pos
        
        colon_pos = index(line, ":")
        if (colon_pos > 0 .and. colon_pos < len(line)) then
            filename = trim(line(colon_pos+1:))
        else
            filename = "unknown"
        end if
    end function extract_source_filename
    
    ! Helper function to join parts from index onwards with ":"
    function join_parts(parts, start_index) result(joined)
        character(len=:), allocatable, intent(in) :: parts(:)
        integer, intent(in) :: start_index
        character(len=:), allocatable :: joined
        integer :: i
        
        joined = ""
        if (start_index <= size(parts)) then
            joined = parts(start_index)
            do i = start_index + 1, size(parts)
                joined = joined // ":" // parts(i)
            end do
        end if
    end function join_parts
    
    ! Helper subroutine to resize lines array when needed
    subroutine resize_lines_array(lines_array)
        type(coverage_line_t), allocatable, intent(inout) :: lines_array(:)
        type(coverage_line_t), allocatable :: temp_array(:)
        integer :: current_size, new_size
        
        current_size = size(lines_array)
        new_size = current_size * 2
        
        ! Copy existing data to temporary array
        allocate(temp_array(current_size))
        temp_array = lines_array
        
        ! Reallocate with larger size
        deallocate(lines_array)
        allocate(lines_array(new_size))
        
        ! Copy data back
        lines_array(1:current_size) = temp_array
        
        deallocate(temp_array)
    end subroutine resize_lines_array
    
    ! Helper subroutine to add a file to the files array
    subroutine add_file_to_array(files_array, files_count, filename, lines)
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        integer, intent(inout) :: files_count
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(in) :: lines(:)
        
        files_count = files_count + 1
        
        ! Resize array if needed
        if (files_count > size(files_array)) then
            call resize_files_array(files_array)
        end if
        
        ! Initialize the new file
        call files_array(files_count)%init(filename, lines)
    end subroutine add_file_to_array
    
    ! Helper subroutine to resize files array when needed
    subroutine resize_files_array(files_array)
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        type(coverage_file_t), allocatable :: temp_array(:)
        integer :: current_size, new_size
        
        current_size = size(files_array)
        new_size = current_size * 2
        
        ! Copy existing data to temporary array
        allocate(temp_array(current_size))
        temp_array = files_array
        
        ! Reallocate with larger size
        deallocate(files_array)
        allocate(files_array(new_size))
        
        ! Copy data back
        files_array(1:current_size) = temp_array
        
        deallocate(temp_array)
    end subroutine resize_files_array

    function gcov_can_parse(this, path) result(supported)
        class(gcov_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical :: supported
        character(len=10) :: extension
        integer :: dot_pos
        
        ! Extract extension and check if supported
        dot_pos = index(path, ".", back=.true.)
        if (dot_pos > 0) then
            extension = path(dot_pos:)
            supported = (trim(extension) == ".gcov")
        else
            supported = .false.
        end if
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end function gcov_can_parse

    function gcov_get_required_files(this) result(extensions)
        class(gcov_parser_t), intent(in) :: this
        character(len=:), allocatable :: extensions(:)
        
        ! Placeholder - will support .gcov text files when text parsing is implemented
        allocate(character(len=5) :: extensions(1))
        extensions(1) = ".gcov"
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end function gcov_get_required_files

    ! Mock parser implementations
    function mock_parse(this, path, error_flag) result(coverage_data)
        class(mock_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        type(coverage_data_t) :: coverage_data
        
        error_flag = .false.
        coverage_data = this%injected_data
        
        ! Suppress unused variable warning
        associate(dummy => path)
        end associate
    end function mock_parse

    function mock_can_parse(this, path) result(supported)
        class(mock_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical :: supported
        
        ! Mock parser supports anything for testing
        supported = .true.
        
        ! Suppress unused variable warnings
        associate(dummy1 => this, dummy2 => path)
        end associate
    end function mock_can_parse

    function mock_get_required_files(this) result(extensions)
        class(mock_parser_t), intent(in) :: this
        character(len=:), allocatable :: extensions(:)
        
        allocate(character(len=5) :: extensions(1))
        extensions(1) = ".mock"
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end function mock_get_required_files

    subroutine mock_inject_data(this, data)
        class(mock_parser_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: data
        
        this%injected_data = data
    end subroutine mock_inject_data





end module coverage_parser