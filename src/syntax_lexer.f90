module syntax_lexer
    use syntax_tokens_core
    use language_rules_core
    implicit none
    private
    
    public :: lexer_t
    public :: is_alpha, is_digit, is_alnum
    public :: skip_whitespace
    public :: parse_comment, parse_string, parse_number
    public :: parse_identifier, parse_operator
    
    type :: lexer_t
        type(language_rules_t) :: rules
    contains
        procedure :: tokenize => lexer_tokenize
    end type lexer_t

contains

    ! Tokenize source code
    subroutine lexer_tokenize(this, source_code, tokens, success)
        class(lexer_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        type(syntax_tokens_t), intent(out) :: tokens
        logical, intent(out) :: success
        
        integer :: pos, source_len
        type(syntax_token_t) :: token
        character(len=:), allocatable :: word
        
        success = .true.
        call tokens%init()
        
        pos = 1
        source_len = len_trim(source_code)
        
        do while (pos <= source_len)
            ! Skip whitespace
            if (source_code(pos:pos) == ' ' .or. source_code(pos:pos) == achar(9)) then
                pos = pos + 1
                cycle
            end if
            
            ! Handle comments
            if (source_code(pos:pos) == this%rules%comment_char) then
                call parse_comment(source_code, pos, source_len, token)
                call tokens%add_token(token)
                cycle
            end if
            
            ! Handle strings
            if (source_code(pos:pos) == '"' .or. source_code(pos:pos) == "'") then
                call parse_string(source_code, pos, source_len, &
                    source_code(pos:pos), token)
                call tokens%add_token(token)
                cycle
            end if
            
            ! Handle numbers
            if (is_digit(source_code(pos:pos))) then
                call parse_number(source_code, pos, source_len, token)
                call tokens%add_token(token)
                cycle
            end if
            
            ! Handle identifiers and keywords
            if (is_alpha(source_code(pos:pos)) .or. source_code(pos:pos) == '_') then
                call parse_identifier(source_code, pos, source_len, word)
                
                call token%init()
                token%start_pos = pos - len(word)
                token%end_pos = pos - 1
                token%text = word
                
                if (this%rules%is_keyword(word)) then
                    token%token_type = TOKEN_KEYWORD
                    token%css_class = CSS_KEYWORD
                    token%ansi_sequence = ANSI_KEYWORD
                else
                    token%token_type = TOKEN_IDENTIFIER
                    token%css_class = CSS_IDENTIFIER
                    token%ansi_sequence = ""
                end if
                
                call tokens%add_token(token)
                cycle
            end if
            
            ! Handle operators (anything else)
            call parse_operator(source_code, pos, source_len, token, this%rules)
            call tokens%add_token(token)
        end do
    end subroutine lexer_tokenize
    
    ! Check if character is alphabetic
    function is_alpha(char) result(is_letter)
        character(len=1), intent(in) :: char
        logical :: is_letter
        integer :: ascii_val
        
        ascii_val = iachar(char)
        is_letter = (ascii_val >= 65 .and. ascii_val <= 90) .or. &  ! A-Z
                   (ascii_val >= 97 .and. ascii_val <= 122)         ! a-z
    end function is_alpha
    
    ! Check if character is a digit
    function is_digit(char) result(is_num)
        character(len=1), intent(in) :: char
        logical :: is_num
        integer :: ascii_val
        
        ascii_val = iachar(char)
        is_num = (ascii_val >= 48 .and. ascii_val <= 57)  ! 0-9
    end function is_digit
    
    ! Check if character is alphanumeric
    function is_alnum(char) result(is_alphanum)
        character(len=1), intent(in) :: char
        logical :: is_alphanum
        
        is_alphanum = is_alpha(char) .or. is_digit(char)
    end function is_alnum
    
    ! Skip whitespace characters
    subroutine skip_whitespace(source, pos, source_len)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos
        integer, intent(in) :: source_len
        
        do while (pos <= source_len)
            if (source(pos:pos) /= ' ' .and. source(pos:pos) /= achar(9) .and. &
                source(pos:pos) /= achar(10) .and. source(pos:pos) /= achar(13)) then
                exit
            end if
            pos = pos + 1
        end do
    end subroutine skip_whitespace
    
    ! Parse a comment token
    subroutine parse_comment(source, pos, source_len, token)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos
        integer, intent(in) :: source_len
        type(syntax_token_t), intent(out) :: token
        
        integer :: start_pos
        
        call token%init()
        start_pos = pos
        
        ! Skip to end of line with safe bounds checking
        do while (pos <= source_len)
            if (source(pos:pos) == achar(10)) exit
            pos = pos + 1
        end do
        
        token%start_pos = start_pos
        token%end_pos = pos - 1
        token%token_type = TOKEN_COMMENT
        ! Safe substring for comment
        if (pos > start_pos .and. pos-1 <= source_len) then
            token%text = source(start_pos:pos-1)
        else if (start_pos <= source_len) then
            token%text = source(start_pos:source_len)
        else
            token%text = ""
        end if
        token%css_class = CSS_COMMENT
        token%ansi_sequence = ANSI_COMMENT
    end subroutine parse_comment
    
    ! Parse a string token
    subroutine parse_string(source, pos, source_len, delimiter, token)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos
        integer, intent(in) :: source_len
        character(len=1), intent(in) :: delimiter
        type(syntax_token_t), intent(out) :: token
        
        integer :: start_pos
        
        call token%init()
        start_pos = pos
        pos = pos + 1  ! Skip opening delimiter
        
        ! Find closing delimiter
        do while (pos <= source_len)
            if (source(pos:pos) == delimiter) then
                pos = pos + 1  ! Include closing delimiter
                exit
            end if
            pos = pos + 1
        end do
        
        token%start_pos = start_pos
        token%end_pos = pos - 1
        token%token_type = TOKEN_STRING
        ! Safe substring for string
        if (pos > start_pos .and. pos-1 <= source_len) then
            token%text = source(start_pos:pos-1)
        else if (start_pos <= source_len) then
            token%text = source(start_pos:source_len)
        else
            token%text = ""
        end if
        token%css_class = CSS_STRING
        token%ansi_sequence = ANSI_STRING
    end subroutine parse_string
    
    ! Parse a number token
    subroutine parse_number(source, pos, source_len, token)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos
        integer, intent(in) :: source_len
        type(syntax_token_t), intent(out) :: token
        
        integer :: start_pos
        
        call token%init()
        start_pos = pos
        
        ! Parse digits with safe bounds checking
        do while (pos <= source_len)
            if (.not. is_digit(source(pos:pos))) exit
            pos = pos + 1
        end do
        
        ! Handle decimal point
        if (pos <= source_len .and. source(pos:pos) == '.') then
            pos = pos + 1
            do while (pos <= source_len)
                if (.not. is_digit(source(pos:pos))) exit
                pos = pos + 1
            end do
        end if
        
        token%start_pos = start_pos
        token%end_pos = pos - 1
        token%token_type = TOKEN_NUMBER
        ! Safe substring for number
        if (pos > start_pos .and. pos-1 <= source_len) then
            token%text = source(start_pos:pos-1)
        else if (start_pos <= source_len) then
            token%text = source(start_pos:source_len)
        else
            token%text = ""
        end if
        token%css_class = CSS_NUMBER
        token%ansi_sequence = ANSI_NUMBER
    end subroutine parse_number
    
    ! Parse an identifier
    subroutine parse_identifier(source, pos, source_len, word)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos
        integer, intent(in) :: source_len
        character(len=:), allocatable, intent(out) :: word
        
        integer :: start_pos
        
        start_pos = pos
        
        ! Parse identifier characters with bounds checking
        do while (pos <= source_len)
            if (.not. (is_alnum(source(pos:pos)) .or. source(pos:pos) == '_')) exit
            pos = pos + 1
        end do
        
        ! Ensure bounds are valid for substring
        if (pos > start_pos .and. pos-1 <= source_len) then
            word = source(start_pos:pos-1)
        else
            word = ""
        end if
    end subroutine parse_identifier
    
    ! Parse an operator token
    subroutine parse_operator(source, pos, source_len, token, rules)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: pos
        integer, intent(in) :: source_len
        type(syntax_token_t), intent(out) :: token
        type(language_rules_t), intent(in) :: rules
        
        integer :: start_pos
        
        ! Note: rules parameter reserved for future operator-specific parsing
        
        call token%init()
        start_pos = pos
        
        ! Simple operator parsing - just single characters for now
        if (pos <= source_len) then
            token%start_pos = start_pos
            token%end_pos = pos
            token%token_type = TOKEN_OPERATOR
            token%text = source(pos:pos)
            token%css_class = CSS_OPERATOR
            token%ansi_sequence = ""
            
            pos = pos + 1
        end if
    end subroutine parse_operator

end module syntax_lexer