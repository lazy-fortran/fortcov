module syntax_highlighter
    use coverage_model, only: coverage_line_t
    implicit none
    private
    
    ! Public types
    public :: syntax_highlighter_t
    public :: language_rules_t
    public :: syntax_tokens_t
    public :: syntax_token_t
    
    ! Token type constants
    integer, parameter, public :: TOKEN_KEYWORD = 1
    integer, parameter, public :: TOKEN_COMMENT = 2
    integer, parameter, public :: TOKEN_STRING = 3
    integer, parameter, public :: TOKEN_NUMBER = 4
    integer, parameter, public :: TOKEN_IDENTIFIER = 5
    integer, parameter, public :: TOKEN_OPERATOR = 6
    integer, parameter, public :: TOKEN_WHITESPACE = 7
    
    ! CSS class constants
    character(len=*), parameter :: CSS_KEYWORD = "keyword"
    character(len=*), parameter :: CSS_COMMENT = "comment"
    character(len=*), parameter :: CSS_STRING = "string"
    character(len=*), parameter :: CSS_NUMBER = "number"
    character(len=*), parameter :: CSS_IDENTIFIER = "identifier"
    character(len=*), parameter :: CSS_OPERATOR = "operator"
    character(len=*), parameter :: CSS_COVERED = "covered"
    character(len=*), parameter :: CSS_UNCOVERED = "uncovered"
    
    ! ANSI color constants
    character(len=*), parameter :: ANSI_RESET = char(27)//'[0m'
    character(len=*), parameter :: ANSI_KEYWORD = char(27)//'[1;34m'    ! Bold blue
    character(len=*), parameter :: ANSI_COMMENT = char(27)//'[32m'      ! Green
    character(len=*), parameter :: ANSI_STRING = char(27)//'[33m'       ! Yellow
    character(len=*), parameter :: ANSI_NUMBER = char(27)//'[35m'       ! Magenta
    character(len=*), parameter :: ANSI_COVERED = char(27)//'[42m'  ! Green bg
    character(len=*), parameter :: ANSI_UNCOVERED = char(27)//'[41m'    ! Red background
    
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
    
    type :: syntax_highlighter_t
        type(language_rules_t) :: fortran_rules
        type(color_scheme_t) :: colors
        logical :: html_mode = .false.
        logical :: terminal_mode = .false.
    contains
        procedure :: init => syntax_highlighter_init
        procedure :: load_fortran_rules => syntax_highlighter_load_fortran_rules
        procedure :: tokenize_source => syntax_highlighter_tokenize_source
        procedure :: highlight_for_html => syntax_highlighter_highlight_for_html
        procedure :: highlight_for_terminal => syntax_highlighter_highlight_for_terminal
        procedure :: highlight_with_coverage => &
            syntax_highlighter_highlight_with_coverage
    end type syntax_highlighter_t

contains

    ! Initialize syntax token
    subroutine syntax_token_init(this)
        class(syntax_token_t), intent(out) :: this
        
        this%start_pos = 0
        this%end_pos = 0
        this%token_type = 0
        this%text = ""
        this%css_class = ""
        this%ansi_sequence = ""
    end subroutine syntax_token_init
    
    ! Initialize syntax tokens collection
    subroutine syntax_tokens_init(this)
        class(syntax_tokens_t), intent(out) :: this
        
        this%token_count = 0
        ! Start with reasonable allocation
        allocate(this%tokens(100))
    end subroutine syntax_tokens_init
    
    ! Add token to collection
    subroutine syntax_tokens_add_token(this, token)
        class(syntax_tokens_t), intent(inout) :: this
        type(syntax_token_t), intent(in) :: token
        
        type(syntax_token_t), allocatable :: temp_tokens(:)
        integer :: old_size, new_size
        
        this%token_count = this%token_count + 1
        
        ! Grow array if needed
        if (this%token_count > size(this%tokens)) then
            old_size = size(this%tokens)
            new_size = old_size * 2
            
            allocate(temp_tokens(new_size))
            temp_tokens(1:old_size) = this%tokens(1:old_size)
            
            deallocate(this%tokens)
            allocate(this%tokens(new_size))
            this%tokens = temp_tokens
            
            deallocate(temp_tokens)
        end if
        
        this%tokens(this%token_count) = token
    end subroutine syntax_tokens_add_token
    
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
        character(len=:), allocatable :: test_word
        
        is_kw = .false.
        
        if (.not. allocated(this%keywords)) return
        
        ! Handle case sensitivity
        if (this%case_sensitive) then
            test_word = word
        else
            test_word = to_lowercase(word)
        end if
        
        do i = 1, size(this%keywords)
            if (this%keywords(i) == test_word) then
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
            if (this%operators(i) == text) then
                is_op = .true.
                return
            end if
        end do
    end function language_rules_is_operator
    
    ! Initialize color scheme
    subroutine color_scheme_init(this)
        class(color_scheme_t), intent(out) :: this
        
        this%keyword_color = "#0000FF"   ! Blue
        this%comment_color = "#008000"   ! Green
        this%string_color = "#FF0000"    ! Red
        this%number_color = "#800080"    ! Purple
    end subroutine color_scheme_init
    
    ! Initialize syntax highlighter
    subroutine syntax_highlighter_init(this)
        class(syntax_highlighter_t), intent(out) :: this
        
        call this%fortran_rules%init()
        call this%colors%init()
        this%html_mode = .false.
        this%terminal_mode = .false.
    end subroutine syntax_highlighter_init
    
    ! Load Fortran language rules
    subroutine syntax_highlighter_load_fortran_rules(this, rules, success)
        class(syntax_highlighter_t), intent(inout) :: this
        type(language_rules_t), intent(out) :: rules
        logical, intent(out) :: success
        
        call rules%init()
        
        ! Define Fortran keywords
        allocate(character(len=20) :: rules%keywords(50))
        
        rules%keywords(1) = "program"
        rules%keywords(2) = "module"
        rules%keywords(3) = "subroutine"
        rules%keywords(4) = "function"
        rules%keywords(5) = "if"
        rules%keywords(6) = "then"
        rules%keywords(7) = "else"
        rules%keywords(8) = "endif"
        rules%keywords(9) = "do"
        rules%keywords(10) = "end"
        rules%keywords(11) = "integer"
        rules%keywords(12) = "real"
        rules%keywords(13) = "character"
        rules%keywords(14) = "logical"
        rules%keywords(15) = "complex"
        rules%keywords(16) = "implicit"
        rules%keywords(17) = "none"
        rules%keywords(18) = "intent"
        rules%keywords(19) = "in"
        rules%keywords(20) = "out"
        rules%keywords(21) = "inout"
        rules%keywords(22) = "allocatable"
        rules%keywords(23) = "pointer"
        rules%keywords(24) = "target"
        rules%keywords(25) = "parameter"
        rules%keywords(26) = "dimension"
        rules%keywords(27) = "contains"
        rules%keywords(28) = "use"
        rules%keywords(29) = "only"
        rules%keywords(30) = "private"
        rules%keywords(31) = "public"
        rules%keywords(32) = "interface"
        rules%keywords(33) = "type"
        rules%keywords(34) = "class"
        rules%keywords(35) = "abstract"
        rules%keywords(36) = "procedure"
        rules%keywords(37) = "call"
        rules%keywords(38) = "return"
        rules%keywords(39) = "stop"
        rules%keywords(40) = "case"
        rules%keywords(41) = "select"
        rules%keywords(42) = "where"
        rules%keywords(43) = "forall"
        rules%keywords(44) = "pure"
        rules%keywords(45) = "elemental"
        rules%keywords(46) = "recursive"
        rules%keywords(47) = "result"
        rules%keywords(48) = "goto"
        rules%keywords(49) = "continue"
        rules%keywords(50) = "cycle"
        
        ! Define operators
        allocate(character(len=10) :: rules%operators(20))
        rules%operators(1) = "+"
        rules%operators(2) = "-"
        rules%operators(3) = "*"
        rules%operators(4) = "/"
        rules%operators(5) = "**"
        rules%operators(6) = "="
        rules%operators(7) = "=="
        rules%operators(8) = "/="
        rules%operators(9) = "<"
        rules%operators(10) = ">"
        rules%operators(11) = "<="
        rules%operators(12) = ">="
        rules%operators(13) = ".and."
        rules%operators(14) = ".or."
        rules%operators(15) = ".not."
        rules%operators(16) = ".eq."
        rules%operators(17) = ".ne."
        rules%operators(18) = ".lt."
        rules%operators(19) = ".gt."
        rules%operators(20) = ".le."
        
        ! Fortran-specific settings
        rules%comment_char = '!'
        rules%string_delims = ['"', "'"]
        rules%case_sensitive = .false.
        
        this%fortran_rules = rules
        success = .true.
    end subroutine syntax_highlighter_load_fortran_rules
    
    ! Tokenize source code
    subroutine syntax_highlighter_tokenize_source(this, source_code, tokens, success)
        class(syntax_highlighter_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        type(syntax_tokens_t), intent(out) :: tokens
        logical, intent(out) :: success
        
        integer :: pos, start_pos, source_len
        character(len=1) :: char
        type(syntax_token_t) :: token
        character(len=:), allocatable :: word
        
        call tokens%init()
        success = .false.
        
        source_len = len(source_code)
        pos = 1
        
        do while (pos <= source_len)
            char = source_code(pos:pos)
            start_pos = pos
            
            ! Skip whitespace
            if (char == ' ' .or. char == achar(9) .or. &
                char == achar(10) .or. char == achar(13)) then
                call skip_whitespace(source_code, pos, source_len)
                cycle
            end if
            
            ! Handle comments
            if (char == this%fortran_rules%comment_char) then
                call parse_comment(source_code, pos, source_len, token)
                call tokens%add_token(token)
                cycle
            end if
            
            ! Handle string literals
            if (char == '"' .or. char == "'") then
                call parse_string(source_code, pos, source_len, char, token)
                call tokens%add_token(token)
                cycle
            end if
            
            ! Handle numbers
            if (is_digit(char)) then
                call parse_number(source_code, pos, source_len, token)
                call tokens%add_token(token)
                cycle
            end if
            
            ! Handle identifiers and keywords
            if (is_alpha(char) .or. char == '_') then
                call parse_identifier(source_code, pos, source_len, word)
                
                call token%init()
                token%start_pos = start_pos
                token%end_pos = pos - 1
                token%text = word
                
                if (this%fortran_rules%is_keyword(word)) then
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
            
            ! Handle operators and punctuation
            call parse_operator(source_code, pos, source_len, token, this%fortran_rules)
            if (token%token_type /= 0) then
                call tokens%add_token(token)
                cycle
            end if
            
            ! Skip unknown characters
            pos = pos + 1
        end do
        
        success = .true.
    end subroutine syntax_highlighter_tokenize_source
    
    ! Generate HTML output with syntax highlighting
    subroutine syntax_highlighter_highlight_for_html(this, source_code, &
                                                      html_output, success)
        class(syntax_highlighter_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: html_output
        logical, intent(out) :: success
        
        type(syntax_tokens_t) :: tokens
        integer :: i, last_pos
        character(len=:), allocatable :: temp_output
        
        success = .false.
        
        ! Tokenize source code
        call this%tokenize_source(source_code, tokens, success)
        if (.not. success) return
        
        ! Start building HTML output
        temp_output = '<pre class="syntax-highlighted">'
        last_pos = 1
        
        ! Process each token
        do i = 1, tokens%token_count
            ! Add any text between last token and this token
            if (tokens%tokens(i)%start_pos > last_pos) then
                temp_output = temp_output // &
                    html_escape(source_code(last_pos:tokens%tokens(i)%start_pos-1))
            end if
            
            ! Add highlighted token
            if (len_trim(tokens%tokens(i)%css_class) > 0) then
                temp_output = temp_output // '<span class="' // &
                    trim(tokens%tokens(i)%css_class) // '">' // &
                    html_escape(tokens%tokens(i)%text) // '</span>'
            else
                temp_output = temp_output // html_escape(tokens%tokens(i)%text)
            end if
            
            last_pos = tokens%tokens(i)%end_pos + 1
        end do
        
        ! Add any remaining text
        if (last_pos <= len(source_code)) then
            temp_output = temp_output // html_escape(source_code(last_pos:))
        end if
        
        temp_output = temp_output // '</pre>'
        html_output = temp_output
        success = .true.
    end subroutine syntax_highlighter_highlight_for_html
    
    ! Generate ANSI colored output for terminal
    subroutine syntax_highlighter_highlight_for_terminal(this, source_code, &
                                                         ansi_output, success)
        class(syntax_highlighter_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: ansi_output
        logical, intent(out) :: success
        
        type(syntax_tokens_t) :: tokens
        integer :: i, last_pos
        character(len=:), allocatable :: temp_output
        
        success = .false.
        
        ! Tokenize source code
        call this%tokenize_source(source_code, tokens, success)
        if (.not. success) return
        
        temp_output = ""
        last_pos = 1
        
        ! Process each token
        do i = 1, tokens%token_count
            ! Add any text between last token and this token
            if (tokens%tokens(i)%start_pos > last_pos) then
                temp_output = temp_output // &
                    source_code(last_pos:tokens%tokens(i)%start_pos-1)
            end if
            
            ! Add colored token
            if (len_trim(tokens%tokens(i)%ansi_sequence) > 0) then
                temp_output = temp_output // tokens%tokens(i)%ansi_sequence // &
                    tokens%tokens(i)%text // ANSI_RESET
            else
                temp_output = temp_output // tokens%tokens(i)%text
            end if
            
            last_pos = tokens%tokens(i)%end_pos + 1
        end do
        
        ! Add any remaining text
        if (last_pos <= len(source_code)) then
            temp_output = temp_output // source_code(last_pos:)
        end if
        
        ansi_output = temp_output
        success = .true.
    end subroutine syntax_highlighter_highlight_for_terminal
    
    ! Highlight with coverage annotations
    subroutine syntax_highlighter_highlight_with_coverage(this, source_code, &
                                                          coverage_lines, &
                                                         output, success)
        class(syntax_highlighter_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        type(coverage_line_t), intent(in) :: coverage_lines(:)
        character(len=:), allocatable, intent(out) :: output
        logical, intent(out) :: success
        
        character(len=:), allocatable :: highlighted_output, line, temp_output
        character(len=:), allocatable :: lines(:)
        integer :: i, line_count
        
        success = .false.
        
        ! First apply syntax highlighting
        call this%highlight_for_html(source_code, highlighted_output, success)
        if (.not. success) return
        
        ! Split into lines and apply coverage annotations
        call split_lines(highlighted_output, lines, line_count)
        
        temp_output = ""
        do i = 1, min(line_count, size(coverage_lines))
            line = lines(i)
            
            ! Add coverage annotation
            if (coverage_lines(i)%is_executable) then
                if (coverage_lines(i)%execution_count > 0) then
                    line = '<span class="' // CSS_COVERED // '">' // line // '</span>'
                else
                    line = '<span class="' // CSS_UNCOVERED // '">' // line // '</span>'
                end if
            end if
            
            temp_output = temp_output // line
            if (i < line_count) temp_output = temp_output // new_line('a')
        end do
        
        output = temp_output
        success = .true.
    end subroutine syntax_highlighter_highlight_with_coverage
    
    ! Helper functions
    
    function to_lowercase(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: lower_str
        integer :: i, ascii_val
        
        lower_str = str
        do i = 1, len(str)
            ascii_val = iachar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then  ! A-Z
                lower_str(i:i) = achar(ascii_val + 32)
            end if
        end do
    end function to_lowercase
    
    function is_alpha(char) result(is_letter)
        character(len=1), intent(in) :: char
        logical :: is_letter
        integer :: ascii_val
        
        ascii_val = iachar(char)
        is_letter = (ascii_val >= 65 .and. ascii_val <= 90) .or. &  ! A-Z
                   (ascii_val >= 97 .and. ascii_val <= 122)         ! a-z
    end function is_alpha
    
    function is_digit(char) result(is_num)
        character(len=1), intent(in) :: char
        logical :: is_num
        integer :: ascii_val
        
        ascii_val = iachar(char)
        is_num = (ascii_val >= 48 .and. ascii_val <= 57)  ! 0-9
    end function is_digit
    
    function is_alnum(char) result(is_alphanum)
        character(len=1), intent(in) :: char
        logical :: is_alphanum
        
        is_alphanum = is_alpha(char) .or. is_digit(char)
    end function is_alnum
    
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
    
    function html_escape(text) result(escaped)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: escaped
        integer :: i
        
        escaped = ""
        
        do i = 1, len(text)
            select case (text(i:i))
            case ('<')
                escaped = escaped // "&lt;"
            case ('>')
                escaped = escaped // "&gt;"
            case ('&')
                escaped = escaped // "&amp;"
            case ('"')
                escaped = escaped // "&quot;"
            case ("'")
                escaped = escaped // "&#39;"
            case default
                escaped = escaped // text(i:i)
            end select
        end do
    end function html_escape
    
    subroutine split_lines(text, lines, line_count)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: line_count
        
        integer :: i, start_pos, text_len
        character(len=1000), allocatable :: temp_lines(:)  ! Dynamic allocation
        
        text_len = len(text)
        line_count = 0
        start_pos = 1
        
        ! Allocate temporary array for line processing
        allocate(temp_lines(100))
        
        do i = 1, text_len
            if (text(i:i) == achar(10) .or. i == text_len) then
                line_count = line_count + 1
                if (line_count <= 100) then
                    if (i == text_len .and. text(i:i) /= achar(10)) then
                        temp_lines(line_count) = text(start_pos:i)
                    else
                        temp_lines(line_count) = text(start_pos:i-1)
                    end if
                end if
                start_pos = i + 1
            end if
        end do
        
        ! Allocate and copy result
        allocate(character(len=1000) :: lines(line_count))  ! Line length limit
        lines(1:line_count) = temp_lines(1:line_count)
    end subroutine split_lines

end module syntax_highlighter
