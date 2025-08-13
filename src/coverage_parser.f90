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
        select case (trim(extension))
        case (".gcda", ".gcno")
            allocate(gcov_parser_t :: parser)
        case default
            error_flag = .true.
        end select
    end subroutine create_parser

    ! GCov parser implementations (stubs for now)
    function gcov_parse(this, path, error_flag) result(coverage_data)
        class(gcov_parser_t), intent(in) :: this
        character(len=*), intent(in) :: path
        logical, intent(out) :: error_flag
        type(coverage_data_t) :: coverage_data
        
        ! Minimal implementation - returns empty data
        error_flag = .false.
        coverage_data = coverage_data_t()
        
        ! Suppress unused variable warnings
        associate(dummy1 => this, dummy2 => path)
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

end module coverage_parser