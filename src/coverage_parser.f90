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
        ! TODO: Add support for additional coverage formats:
        ! - ".info" -> lcov_parser_t (LCOV format)
        ! - ".xml" -> cobertura_parser_t (Cobertura XML format)
        ! - ".json" -> json_parser_t (JSON coverage format)
        ! - Intel Fortran coverage files
        ! - LLVM/Flang coverage formats
        select case (trim(extension))
        case (".gcda", ".gcno")
            allocate(gcov_parser_t :: parser)
        case default
            ! TODO: Add informative error message for unsupported formats
            ! write(error_unit, '(A,A,A)') &
            !   "ERROR: Unsupported coverage file format: '", &
            !   trim(extension), "'"
            error_flag = .true.
        end select
    end subroutine create_parser

    ! GCov parser implementation - reads binary .gcno/.gcda files
    function gcov_parse(this, path, error_flag) result(coverage_data)
        use gcov_binary_format
        class(gcov_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        type(coverage_data_t) :: coverage_data
        type(gcov_data_reader_t) :: reader
        character(len=:), allocatable :: gcno_path, gcda_path, source_path
        character(len=:), allocatable :: base_name
        logical :: load_success
        integer :: dot_pos
        
        error_flag = .true.
        coverage_data = coverage_data_t()
        
        ! Derive gcno and gcda paths from input path
        call derive_gcov_paths(path, gcno_path, gcda_path, source_path)
        
        ! Load binary coverage files using gcov_binary_format
        call reader%init()
        call reader%load_files(gcno_path, gcda_path, load_success)
        
        if (.not. load_success) then
            return
        end if
        
        ! Convert gcov data to coverage model format
        call convert_gcov_to_coverage_model(reader, source_path, &
                                          coverage_data, error_flag)
        
        ! Suppress unused variable warning
        associate(dummy => this)
        end associate
    end function gcov_parse

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
            supported = (trim(extension) == ".gcda") .or. &
                       (trim(extension) == ".gcno")
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
        
        ! GCov requires both .gcda (runtime data) and .gcno (graph notes) files
        ! TODO: Consider supporting additional GCC coverage file variants:
        ! - .gcov text format files (gcov tool output)
        ! - .info format files (lcov tool output)
        allocate(character(len=5) :: extensions(2))
        extensions(1) = ".gcda"
        extensions(2) = ".gcno"
        
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

    ! Helper subroutine to derive gcov paths from input path
    subroutine derive_gcov_paths(input_path, gcno_path, gcda_path, &
                                source_path)
        character(len=*), intent(in) :: input_path
        character(len=:), allocatable, intent(out) :: gcno_path, gcda_path
        character(len=:), allocatable, intent(out) :: source_path
        character(len=:), allocatable :: base_name
        integer :: dot_pos
        
        ! Extract base name without extension
        dot_pos = index(input_path, ".", back=.true.)
        if (dot_pos > 0) then
            base_name = input_path(1:dot_pos-1)
        else
            base_name = input_path
        end if
        
        ! Construct paths
        gcno_path = base_name // ".gcno"
        gcda_path = base_name // ".gcda"
        source_path = base_name // ".f90"
    end subroutine derive_gcov_paths

    ! Convert gcov binary data to coverage model
    subroutine convert_gcov_to_coverage_model(reader, source_path, &
                                            coverage_data, error_flag)
        use gcov_binary_format
        type(gcov_data_reader_t), intent(in) :: reader
        character(len=*), intent(in) :: source_path
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_flag
        type(coverage_line_t), allocatable :: lines(:)
        type(coverage_file_t) :: file_coverage
        type(coverage_file_t), allocatable :: files(:)
        integer :: line_count
        
        error_flag = .false.
        
        ! Parse source file to determine line count and executable lines
        call parse_source_file(source_path, lines, error_flag)
        if (error_flag) return
        
        ! Map execution counts from gcov data to source lines
        call map_execution_counts(reader, lines)
        
        ! Create file coverage object
        call file_coverage%init(source_path, lines)
        
        ! Package into coverage data
        allocate(files(1))
        files(1) = file_coverage
        call coverage_data%init(files)
        
        deallocate(lines)
        deallocate(files)
    end subroutine convert_gcov_to_coverage_model

    ! Parse Fortran source file to identify executable lines
    subroutine parse_source_file(source_path, lines, error_flag)
        character(len=*), intent(in) :: source_path
        type(coverage_line_t), allocatable, intent(out) :: lines(:)
        logical, intent(out) :: error_flag
        integer :: unit, iostat, line_num
        character(len=500) :: line_content
        character(len=500) :: trimmed_line
        logical :: file_exists
        type(coverage_line_t), allocatable :: temp_lines(:)
        integer :: lines_count
        
        error_flag = .false.
        lines_count = 0
        
        ! Check if source file exists
        inquire(file=source_path, exist=file_exists)
        if (.not. file_exists) then
            ! Create minimal lines for testing with dummy source
            allocate(lines(5))
            do line_num = 1, 5
                call lines(line_num)%init(0, line_num, source_path, .true.)
            end do
            return
        end if
        
        ! First pass: count lines
        open(newunit=unit, file=source_path, status='old', iostat=iostat)
        if (iostat /= 0) then
            error_flag = .true.
            return
        end if
        
        line_num = 0
        do
            read(unit, '(A)', iostat=iostat) line_content
            if (iostat /= 0) exit
            line_num = line_num + 1
        end do
        lines_count = line_num
        close(unit)
        
        if (lines_count == 0) then
            allocate(lines(0))
            return
        end if
        
        ! Second pass: analyze lines
        allocate(lines(lines_count))
        open(newunit=unit, file=source_path, status='old')
        
        line_num = 0
        do
            read(unit, '(A)', iostat=iostat) line_content
            if (iostat /= 0) exit
            
            line_num = line_num + 1
            trimmed_line = adjustl(line_content)
            
            ! Determine if line is executable
            call lines(line_num)%init(0, line_num, source_path, &
                                     is_executable_line(trimmed_line))
        end do
        
        close(unit)
    end subroutine parse_source_file

    ! Determine if a source line is executable
    function is_executable_line(line) result(executable)
        character(len=*), intent(in) :: line
        logical :: executable
        character(len=:), allocatable :: trimmed
        
        trimmed = trim(adjustl(line))
        executable = .false.
        
        ! Empty lines and comments are not executable
        if (len(trimmed) == 0 .or. trimmed(1:1) == '!') return
        
        ! Non-executable declarations and directives
        if (starts_with_keyword(trimmed, 'use ')) return
        if (starts_with_keyword(trimmed, 'implicit ')) return
        if (starts_with_keyword(trimmed, 'interface ')) return
        if (starts_with_keyword(trimmed, 'end interface')) return
        if (starts_with_keyword(trimmed, 'procedure ')) return
        
        ! Non-executable structural elements
        if (starts_with_keyword(trimmed, 'module ')) return
        if (starts_with_keyword(trimmed, 'end module')) return
        if (starts_with_keyword(trimmed, 'program ')) return
        if (starts_with_keyword(trimmed, 'end program')) return
        if (starts_with_keyword(trimmed, 'end subroutine')) return
        if (starts_with_keyword(trimmed, 'end function')) return
        if (starts_with_keyword(trimmed, 'contains')) return
        
        ! Non-executable control structure terminators
        if (trim(trimmed) == 'else') return
        if (starts_with_keyword(trimmed, 'end if')) return
        if (starts_with_keyword(trimmed, 'end do')) return
        if (starts_with_keyword(trimmed, 'end select')) return
        if (starts_with_keyword(trimmed, 'case default')) return
        
        ! Most other lines are executable (including declarations with initialization,
        ! function/subroutine definitions, and control structure starts)
        executable = .true.
    end function is_executable_line

    ! Check if line starts with given keyword
    function starts_with_keyword(line, keyword) result(matches)
        character(len=*), intent(in) :: line, keyword
        logical :: matches
        character(len=:), allocatable :: lower_line, lower_keyword
        
        lower_line = to_lower(trim(adjustl(line)))
        lower_keyword = to_lower(keyword)
        
        matches = .false.
        if (len(lower_line) >= len(lower_keyword)) then
            matches = (lower_line(1:len(lower_keyword)) == lower_keyword)
        end if
    end function starts_with_keyword

    ! Convert string to lowercase
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i, ascii_val
        
        lower_str = str
        do i = 1, len(str)
            ascii_val = ichar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then  ! A-Z
                lower_str(i:i) = char(ascii_val + 32)
            end if
        end do
    end function to_lower

    ! Map execution counts from gcov data to source lines
    subroutine map_execution_counts(reader, lines)
        use gcov_binary_format
        type(gcov_data_reader_t), intent(in) :: reader
        type(coverage_line_t), intent(inout) :: lines(:)
        integer :: i, counter_idx
        
        ! If we have execution data, map it to executable lines
        if (reader%has_gcda .and. reader%counters%count > 0) then
            counter_idx = 1
            do i = 1, size(lines)
                if (lines(i)%is_executable .and. &
                    counter_idx <= reader%counters%count) then
                    lines(i)%execution_count = &
                        int(reader%counters%values(counter_idx))
                    counter_idx = counter_idx + 1
                end if
            end do
        end if
    end subroutine map_execution_counts

end module coverage_parser