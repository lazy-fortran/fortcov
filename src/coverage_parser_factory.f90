module coverage_parser_factory
    use coverage_model
    use input_validation
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
        
        ! Input validation: Check path validity using validation framework
        block
            type(validation_result_t) :: validation_result
            call validate_path_safety(path, validation_result)
            if (.not. validation_result%is_valid) then
                error_flag = .true.
                return
            end if
        end block
        
        ! Extract file extension
        dot_pos = index(path, ".", back=.true.)
        if (dot_pos > 0) then
            extension = path(dot_pos:)
        else
            extension = ""
        end if
        
        ! Select parser based on extension  
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

    ! GCov parser implementation - delegates to gcov_file_processor
    function gcov_parse(this, path, error_flag) result(coverage_data)
        use gcov_file_processor
        class(gcov_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        type(coverage_data_t) :: coverage_data
        
        call process_gcov_file(path, coverage_data, error_flag)
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
            supported = (trim(extension) == ".gcov")
        else
            supported = .false.
        end if
    end function gcov_can_parse

    function gcov_get_required_files(this) result(extensions)
        class(gcov_parser_t), intent(in) :: this
        character(len=:), allocatable :: extensions(:)
        
        allocate(character(len=5) :: extensions(1))
        extensions(1) = ".gcov"
    end function gcov_get_required_files

    ! Mock parser implementations
    function mock_parse(this, path, error_flag) result(coverage_data)
        class(mock_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        type(coverage_data_t) :: coverage_data
        
        error_flag = .false.
        coverage_data = this%injected_data
    end function mock_parse

    function mock_can_parse(this, path) result(supported)
        class(mock_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical :: supported
        
        ! Mock parser supports anything for testing
        supported = .true.
    end function mock_can_parse

    function mock_get_required_files(this) result(extensions)
        class(mock_parser_t), intent(in) :: this
        character(len=:), allocatable :: extensions(:)
        
        allocate(character(len=5) :: extensions(1))
        extensions(1) = ".mock"
    end function mock_get_required_files

    subroutine mock_inject_data(this, data)
        class(mock_parser_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: data
        
        this%injected_data = data
    end subroutine mock_inject_data

end module coverage_parser_factory