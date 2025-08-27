module syntax_tokens_core
    implicit none
    private
    
    ! Public types
    public :: syntax_token_t
    public :: syntax_tokens_t
    
    ! Token type constants
    integer, parameter, public :: TOKEN_KEYWORD = 1
    integer, parameter, public :: TOKEN_COMMENT = 2
    integer, parameter, public :: TOKEN_STRING = 3
    integer, parameter, public :: TOKEN_NUMBER = 4
    integer, parameter, public :: TOKEN_IDENTIFIER = 5
    integer, parameter, public :: TOKEN_OPERATOR = 6
    integer, parameter, public :: TOKEN_WHITESPACE = 7
    
    ! CSS class constants
    character(len=*), parameter, public :: CSS_KEYWORD = "keyword"
    character(len=*), parameter, public :: CSS_COMMENT = "comment"
    character(len=*), parameter, public :: CSS_STRING = "string"
    character(len=*), parameter, public :: CSS_NUMBER = "number"
    character(len=*), parameter, public :: CSS_IDENTIFIER = "identifier"
    character(len=*), parameter, public :: CSS_OPERATOR = "operator"
    character(len=*), parameter, public :: CSS_COVERED = "covered"
    character(len=*), parameter, public :: CSS_UNCOVERED = "uncovered"
    
    ! ANSI color constants
    character(len=*), parameter, public :: ANSI_RESET = char(27)//'[0m'
    character(len=*), parameter, public :: ANSI_KEYWORD = char(27)//'[1;34m'    ! Bold blue
    character(len=*), parameter, public :: ANSI_COMMENT = char(27)//'[32m'      ! Green
    character(len=*), parameter, public :: ANSI_STRING = char(27)//'[33m'       ! Yellow
    character(len=*), parameter, public :: ANSI_NUMBER = char(27)//'[35m'       ! Magenta
    character(len=*), parameter, public :: ANSI_COVERED = char(27)//'[42m'      ! Green bg
    character(len=*), parameter, public :: ANSI_UNCOVERED = char(27)//'[41m'    ! Red bg
    
    type :: syntax_token_t
        integer :: start_pos = 0
        integer :: end_pos = 0
        integer :: token_type = 0
        character(len=:), allocatable :: text
        character(len=:), allocatable :: css_class
        character(len=:), allocatable :: ansi_sequence
    contains
        procedure :: init => syntax_token_init
    end type syntax_token_t
    
    type :: syntax_tokens_t
        type(syntax_token_t), allocatable :: tokens(:)
        integer :: token_count = 0
    contains
        procedure :: init => syntax_tokens_init
        procedure :: add_token => syntax_tokens_add_token
    end type syntax_tokens_t

contains

    ! Initialize syntax token
    subroutine syntax_token_init(this)
        class(syntax_token_t), intent(out) :: this
        
        this%start_pos = 0
        this%end_pos = 0
        this%token_type = 0
        if (allocated(this%text)) deallocate(this%text)
        if (allocated(this%css_class)) deallocate(this%css_class)
        if (allocated(this%ansi_sequence)) deallocate(this%ansi_sequence)
    end subroutine syntax_token_init
    
    ! Initialize syntax tokens collection
    subroutine syntax_tokens_init(this)
        class(syntax_tokens_t), intent(out) :: this
        
        if (allocated(this%tokens)) deallocate(this%tokens)
        allocate(this%tokens(100))  ! Initial capacity
        this%token_count = 0
    end subroutine syntax_tokens_init
    
    ! Add token to collection
    subroutine syntax_tokens_add_token(this, token)
        class(syntax_tokens_t), intent(inout) :: this
        type(syntax_token_t), intent(in) :: token
        type(syntax_token_t), allocatable :: temp_tokens(:)
        integer :: new_size
        
        ! Grow array if needed
        if (this%token_count >= size(this%tokens)) then
            new_size = size(this%tokens) * 2
            allocate(temp_tokens(new_size))
            temp_tokens(1:this%token_count) = this%tokens(1:this%token_count)
            call move_alloc(temp_tokens, this%tokens)
        end if
        
        this%token_count = this%token_count + 1
        this%tokens(this%token_count) = token
    end subroutine syntax_tokens_add_token

end module syntax_tokens_core