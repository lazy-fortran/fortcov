module syntax_token_types
    !! Syntax token types for syntax highlighting
    !!
    !! This module provides types for representing tokens
    !! and language rules for syntax highlighting.
    
    implicit none
    private

    ! Token type enumeration
    integer, parameter, public :: TOKEN_UNKNOWN = 0
    integer, parameter, public :: TOKEN_KEYWORD = 1
    integer, parameter, public :: TOKEN_IDENTIFIER = 2
    integer, parameter, public :: TOKEN_NUMBER = 3
    integer, parameter, public :: TOKEN_STRING = 4
    integer, parameter, public :: TOKEN_COMMENT = 5
    integer, parameter, public :: TOKEN_OPERATOR = 6
    integer, parameter, public :: TOKEN_PUNCTUATION = 7
    integer, parameter, public :: TOKEN_WHITESPACE = 8
    integer, parameter, public :: TOKEN_PREPROCESSOR = 9
    integer, parameter, public :: TOKEN_TYPE = 10
    integer, parameter, public :: TOKEN_INTRINSIC = 11

    ! Maximum lengths
    integer, parameter, public :: MAX_TOKEN_TEXT = 512
    integer, parameter, public :: MAX_COLOR_CODE = 32
    integer, parameter, public :: MAX_KEYWORD_LEN = 64

    ! Syntax token type
    type, public :: syntax_token_t
        integer :: token_type = TOKEN_UNKNOWN
        integer :: start_pos = 0
        integer :: end_pos = 0
        character(len=MAX_TOKEN_TEXT) :: text = ""
        integer :: line_number = 0
        integer :: column = 0
    contains
        procedure :: init => syntax_token_init
    end type syntax_token_t

    ! Collection of syntax tokens
    type, public :: syntax_tokens_t
        type(syntax_token_t), allocatable :: tokens(:)
        integer :: count = 0
    contains
        procedure :: init => syntax_tokens_init
        procedure :: add_token => syntax_tokens_add_token
    end type syntax_tokens_t

    ! Language rules for syntax highlighting
    type, public :: language_rules_t
        character(len=MAX_KEYWORD_LEN), allocatable :: keywords(:)
        character(len=MAX_KEYWORD_LEN), allocatable :: types(:)
        character(len=MAX_KEYWORD_LEN), allocatable :: intrinsics(:)
        character(len=8), allocatable :: operators(:)
    contains
        procedure :: init => language_rules_init
        procedure :: is_keyword => language_rules_is_keyword
        procedure :: is_operator => language_rules_is_operator
        procedure :: is_type => language_rules_is_type
        procedure :: is_intrinsic => language_rules_is_intrinsic
    end type language_rules_t

    ! Color scheme for syntax highlighting
    type, public :: color_scheme_t
        character(len=MAX_COLOR_CODE) :: keyword_color = "#569CD6"
        character(len=MAX_COLOR_CODE) :: string_color = "#CE9178"
        character(len=MAX_COLOR_CODE) :: comment_color = "#6A9955"
        character(len=MAX_COLOR_CODE) :: number_color = "#B5CEA8"
        character(len=MAX_COLOR_CODE) :: operator_color = "#D4D4D4"
    contains
        procedure :: init => color_scheme_init
    end type color_scheme_t

contains

    subroutine syntax_token_init(this)
        !! Initialize a syntax token
        class(syntax_token_t), intent(out) :: this
        
        this%token_type = TOKEN_UNKNOWN
        this%start_pos = 0
        this%end_pos = 0
        this%text = ""
        this%line_number = 0
        this%column = 0
    end subroutine syntax_token_init

    subroutine syntax_tokens_init(this)
        !! Initialize a collection of syntax tokens
        class(syntax_tokens_t), intent(out) :: this
        
        if (allocated(this%tokens)) deallocate(this%tokens)
        allocate(this%tokens(0))
        this%count = 0
    end subroutine syntax_tokens_init

    subroutine syntax_tokens_add_token(this, token)
        !! Add a token to the collection
        class(syntax_tokens_t), intent(inout) :: this
        type(syntax_token_t), intent(in) :: token
        
        type(syntax_token_t), allocatable :: temp_tokens(:)
        integer :: new_size
        
        ! Grow array if needed
        if (.not. allocated(this%tokens)) then
            allocate(this%tokens(100))
            this%count = 0
        end if
        
        if (this%count >= size(this%tokens)) then
            new_size = size(this%tokens) * 2
            allocate(temp_tokens(new_size))
            temp_tokens(1:this%count) = this%tokens(1:this%count)
            call move_alloc(temp_tokens, this%tokens)
        end if
        
        this%count = this%count + 1
        this%tokens(this%count) = token
    end subroutine syntax_tokens_add_token

    subroutine language_rules_init(this)
        !! Initialize language rules with empty arrays
        class(language_rules_t), intent(out) :: this
        
        allocate(this%keywords(0))
        allocate(this%types(0))
        allocate(this%intrinsics(0))
        allocate(this%operators(0))
    end subroutine language_rules_init

    function language_rules_is_keyword(this, word) result(is_kw)
        !! Check if a word is a keyword
        class(language_rules_t), intent(in) :: this
        character(len=*), intent(in) :: word
        logical :: is_kw
        
        integer :: i
        character(len=:), allocatable :: lower_word
        
        is_kw = .false.
        lower_word = to_lower(word)
        
        do i = 1, size(this%keywords)
            if (trim(lower_word) == trim(this%keywords(i))) then
                is_kw = .true.
                return
            end if
        end do
    end function language_rules_is_keyword

    function language_rules_is_operator(this, text) result(is_op)
        !! Check if text is an operator
        class(language_rules_t), intent(in) :: this
        character(len=*), intent(in) :: text
        logical :: is_op
        
        integer :: i
        
        is_op = .false.
        
        do i = 1, size(this%operators)
            if (trim(text) == trim(this%operators(i))) then
                is_op = .true.
                return
            end if
        end do
    end function language_rules_is_operator

    function language_rules_is_type(this, word) result(is_type)
        !! Check if a word is a type keyword
        class(language_rules_t), intent(in) :: this
        character(len=*), intent(in) :: word
        logical :: is_type
        
        integer :: i
        character(len=:), allocatable :: lower_word
        
        is_type = .false.
        lower_word = to_lower(word)
        
        do i = 1, size(this%types)
            if (trim(lower_word) == trim(this%types(i))) then
                is_type = .true.
                return
            end if
        end do
    end function language_rules_is_type

    function language_rules_is_intrinsic(this, word) result(is_intrinsic)
        !! Check if a word is an intrinsic function
        class(language_rules_t), intent(in) :: this
        character(len=*), intent(in) :: word
        logical :: is_intrinsic
        
        integer :: i
        character(len=:), allocatable :: lower_word
        
        is_intrinsic = .false.
        lower_word = to_lower(word)
        
        do i = 1, size(this%intrinsics)
            if (trim(lower_word) == trim(this%intrinsics(i))) then
                is_intrinsic = .true.
                return
            end if
        end do
    end function language_rules_is_intrinsic

    subroutine color_scheme_init(this)
        !! Initialize color scheme with default colors
        class(color_scheme_t), intent(out) :: this
        
        this%keyword_color = "#569CD6"
        this%string_color = "#CE9178"
        this%comment_color = "#6A9955"
        this%number_color = "#B5CEA8"
        this%operator_color = "#D4D4D4"
    end subroutine color_scheme_init

    ! Helper function - should be in string_utils
    pure function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i
        
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            else
                lower_str(i:i) = str(i:i)
            end if
        end do
    end function to_lower

end module syntax_token_types