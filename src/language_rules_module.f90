module language_rules_module
    implicit none
    private
    
    public :: language_rules_t
    public :: color_scheme_t
    
    type :: language_rules_t
        character(len=:), allocatable :: keywords(:)
        character(len=:), allocatable :: operators(:)
        character(len=1) :: comment_char = '!'
        character(len=1) :: string_delims(2) = ['"', "'"]
        logical :: case_sensitive = .false.
    contains
        procedure :: init => language_rules_init
        procedure :: is_keyword => language_rules_is_keyword
        procedure :: is_operator => language_rules_is_operator
    end type language_rules_t
    
    type :: color_scheme_t
        character(len=:), allocatable :: keyword_color
        character(len=:), allocatable :: comment_color
        character(len=:), allocatable :: string_color
        character(len=:), allocatable :: number_color
    contains
        procedure :: init => color_scheme_init
    end type color_scheme_t

contains

    ! Initialize language rules
    subroutine language_rules_init(this)
        class(language_rules_t), intent(out) :: this
        
        this%comment_char = '!'
        this%string_delims = ['"', "'"]
        this%case_sensitive = .false.
    end subroutine language_rules_init
    
    ! Check if word is a keyword
    function language_rules_is_keyword(this, word) result(is_kw)
        class(language_rules_t), intent(in) :: this
        character(len=*), intent(in) :: word
        logical :: is_kw
        integer :: i
        character(len=:), allocatable :: test_word, keyword
        
        is_kw = .false.
        if (.not. allocated(this%keywords)) return
        
        if (this%case_sensitive) then
            test_word = word
        else
            test_word = to_lowercase(word)
        end if
        
        do i = 1, size(this%keywords)
            if (this%case_sensitive) then
                keyword = this%keywords(i)
            else
                keyword = to_lowercase(this%keywords(i))
            end if
            if (test_word == keyword) then
                is_kw = .true.
                return
            end if
        end do
    end function language_rules_is_keyword
    
    ! Check if text is an operator
    function language_rules_is_operator(this, text) result(is_op)
        class(language_rules_t), intent(in) :: this
        character(len=*), intent(in) :: text
        logical :: is_op
        integer :: i
        
        is_op = .false.
        if (.not. allocated(this%operators)) return
        
        do i = 1, size(this%operators)
            if (text == this%operators(i)) then
                is_op = .true.
                return
            end if
        end do
    end function language_rules_is_operator
    
    ! Initialize color scheme
    subroutine color_scheme_init(this)
        class(color_scheme_t), intent(out) :: this
        
        this%keyword_color = "#0000FF"
        this%comment_color = "#008000"
        this%string_color = "#FF8C00"
        this%number_color = "#8B008B"
    end subroutine color_scheme_init
    
    ! Convert string to lowercase
    pure function to_lowercase(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: lower_str
        integer :: i, code
        
        lower_str = str
        do i = 1, len(lower_str)
            code = iachar(lower_str(i:i))
            if (code >= 65 .and. code <= 90) then
                lower_str(i:i) = achar(code + 32)
            end if
        end do
    end function to_lowercase

end module language_rules_module