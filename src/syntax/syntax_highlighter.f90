module syntax_highlighter
    use syntax_tokens_core
    use language_rules_core
    use syntax_lexer
    use syntax_formatter
    use coverage_model_core, only: coverage_line_t
    implicit none
    private
    
    ! Public types (re-export)
    public :: syntax_highlighter_t
    public :: language_rules_t
    public :: syntax_tokens_t
    public :: syntax_token_t
    
    ! Token type constants (re-export)
    public :: TOKEN_KEYWORD, TOKEN_COMMENT, TOKEN_STRING
    public :: TOKEN_NUMBER, TOKEN_IDENTIFIER, TOKEN_OPERATOR, TOKEN_WHITESPACE
    
    ! CSS class constants (re-export)
    public :: CSS_KEYWORD, CSS_COMMENT, CSS_STRING, CSS_NUMBER
    public :: CSS_IDENTIFIER, CSS_OPERATOR, CSS_COVERED, CSS_UNCOVERED
    
    ! ANSI color constants (re-export)
    public :: ANSI_RESET, ANSI_KEYWORD, ANSI_COMMENT, ANSI_STRING
    public :: ANSI_NUMBER, ANSI_COVERED, ANSI_UNCOVERED
    
    type :: syntax_highlighter_t
        type(language_rules_t) :: fortran_rules
        type(color_scheme_t) :: colors
        type(lexer_t) :: lexer
        type(formatter_t) :: formatter
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
        
        integer :: i
        character(len=20), allocatable :: keywords(:)
        character(len=3), allocatable :: operators(:)
        
        success = .true.
        
        ! Fortran keywords
        allocate(keywords(70))
        keywords = [ character(len=20) :: &
            "program", "module", "subroutine", "function", "end", &
            "use", "implicit", "none", "contains", "interface", &
            "type", "class", "abstract", "extends", "generic", &
            "public", "private", "protected", "bind", "import", &
            "integer", "real", "complex", "logical", "character", &
            "double", "precision", "parameter", "allocatable", "dimension", &
            "pointer", "target", "optional", "intent", "in", &
            "out", "inout", "intrinsic", "external", "save", &
            "data", "common", "equivalence", "namelist", "format", &
            "if", "then", "else", "elseif", "endif", &
            "do", "while", "cycle", "exit", "enddo", &
            "select", "case", "default", "where", "elsewhere", &
            "forall", "concurrent", "associate", "block", "critical", &
            "call", "return", "stop", "error", "pause", &
            "open", "close", "read", "write", "print" &
        ]
        
        ! Common Fortran operators
        allocate(operators(20))
        operators = [ character(len=3) :: &
            "=", "+", "-", "*", "/", "**", &
            ".eq.", ".ne.", ".lt.", ".le.", ".gt.", ".ge.", &
            ".and.", ".or.", ".not.", ".eqv.", ".neqv.", &
            "=>", "%", "::" &
        ]
        
        ! Set up rules
        rules%keywords = keywords
        rules%operators = operators
        rules%comment_char = '!'
        rules%string_delims = ['"', "'"]
        rules%case_sensitive = .false.
        
        ! Update instance rules and lexer
        this%fortran_rules = rules
        this%lexer%rules = rules
    end subroutine syntax_highlighter_load_fortran_rules
    
    ! Tokenize source code
    subroutine syntax_highlighter_tokenize_source(this, source_code, tokens, success)
        class(syntax_highlighter_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        type(syntax_tokens_t), intent(out) :: tokens
        logical, intent(out) :: success
        
        call this%lexer%tokenize(source_code, tokens, success)
    end subroutine syntax_highlighter_tokenize_source
    
    ! Highlight source code for HTML output
    subroutine syntax_highlighter_highlight_for_html(this, source_code, &
        highlighted, success)
        class(syntax_highlighter_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: highlighted
        logical, intent(out) :: success
        
        type(syntax_tokens_t) :: tokens
        
        ! Set formatter mode
        this%formatter%html_mode = .true.
        this%formatter%terminal_mode = .false.
        
        ! Tokenize the source
        call this%lexer%tokenize(source_code, tokens, success)
        if (.not. success) return
        
        ! Format as HTML
        call this%formatter%format_html(source_code, tokens, highlighted, success)
    end subroutine syntax_highlighter_highlight_for_html
    
    ! Highlight source code for terminal output
    subroutine syntax_highlighter_highlight_for_terminal(this, source_code, &
        highlighted, success)
        class(syntax_highlighter_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: highlighted
        logical, intent(out) :: success
        
        type(syntax_tokens_t) :: tokens
        
        ! Set formatter mode
        this%formatter%html_mode = .false.
        this%formatter%terminal_mode = .true.
        
        ! Tokenize the source
        call this%lexer%tokenize(source_code, tokens, success)
        if (.not. success) return
        
        ! Format for terminal
        call this%formatter%format_terminal(source_code, tokens, highlighted, success)
    end subroutine syntax_highlighter_highlight_for_terminal
    
    ! Highlight with coverage information
    subroutine syntax_highlighter_highlight_with_coverage(this, source_code, &
        coverage_lines, highlighted, success)
        class(syntax_highlighter_t), intent(inout) :: this
        character(len=*), intent(in) :: source_code
        type(coverage_line_t), intent(in) :: coverage_lines(:)
        character(len=:), allocatable, intent(out) :: highlighted
        logical, intent(out) :: success
        
        ! Use formatter for coverage highlighting
        call this%formatter%format_with_coverage(source_code, coverage_lines, &
            highlighted, success)
    end subroutine syntax_highlighter_highlight_with_coverage

end module syntax_highlighter