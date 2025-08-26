module syntax_formatter
    use syntax_tokens_module
    use coverage_model, only: coverage_line_t
    implicit none
    private
    
    public :: formatter_t
    public :: html_escape
    public :: split_lines
    
    type :: formatter_t
        logical :: html_mode = .false.
        logical :: terminal_mode = .false.
    contains
        procedure :: format_html => formatter_format_html
        procedure :: format_terminal => formatter_format_terminal
        procedure :: format_with_coverage => formatter_format_with_coverage
    end type formatter_t

contains

    ! Format tokens as HTML
    subroutine formatter_format_html(this, source_code, tokens, formatted, success)
        class(formatter_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        type(syntax_tokens_t), intent(in) :: tokens
        character(len=:), allocatable, intent(out) :: formatted
        logical, intent(out) :: success
        
        integer :: i, current_pos
        character(len=:), allocatable :: escaped_text
        
        success = .true.
        formatted = ""
        current_pos = 1
        
        ! Process each token
        do i = 1, tokens%token_count
            ! Add any text before this token
            if (tokens%tokens(i)%start_pos > current_pos) then
                escaped_text = html_escape( &
                    source_code(current_pos:tokens%tokens(i)%start_pos-1))
                formatted = formatted // escaped_text
            end if
            
            ! Add the token with styling
            if (len_trim(tokens%tokens(i)%css_class) > 0) then
                formatted = formatted // '<span class="' // &
                    trim(tokens%tokens(i)%css_class) // '">'
                escaped_text = html_escape(tokens%tokens(i)%text)
                formatted = formatted // escaped_text
                formatted = formatted // '</span>'
            else
                escaped_text = html_escape(tokens%tokens(i)%text)
                formatted = formatted // escaped_text
            end if
            
            current_pos = tokens%tokens(i)%end_pos + 1
        end do
        
        ! Add any remaining text
        if (current_pos <= len(source_code)) then
            escaped_text = html_escape(source_code(current_pos:))
            formatted = formatted // escaped_text
        end if
    end subroutine formatter_format_html
    
    ! Format tokens for terminal display
    subroutine formatter_format_terminal(this, source_code, tokens, formatted, success)
        class(formatter_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        type(syntax_tokens_t), intent(in) :: tokens
        character(len=:), allocatable, intent(out) :: formatted
        logical, intent(out) :: success
        
        integer :: i, current_pos
        
        success = .true.
        formatted = ""
        current_pos = 1
        
        ! Process each token
        do i = 1, tokens%token_count
            ! Add any text before this token
            if (tokens%tokens(i)%start_pos > current_pos) then
                formatted = formatted // &
                    source_code(current_pos:tokens%tokens(i)%start_pos-1)
            end if
            
            ! Add the token with ANSI coloring
            if (len_trim(tokens%tokens(i)%ansi_sequence) > 0) then
                formatted = formatted // tokens%tokens(i)%ansi_sequence
                formatted = formatted // tokens%tokens(i)%text
                formatted = formatted // ANSI_RESET
            else
                formatted = formatted // tokens%tokens(i)%text
            end if
            
            current_pos = tokens%tokens(i)%end_pos + 1
        end do
        
        ! Add any remaining text
        if (current_pos <= len(source_code)) then
            formatted = formatted // source_code(current_pos:)
        end if
    end subroutine formatter_format_terminal
    
    ! Format with coverage information
    subroutine formatter_format_with_coverage(this, source_code, &
        coverage_lines, formatted, success)
        class(formatter_t), intent(in) :: this
        character(len=*), intent(in) :: source_code
        type(coverage_line_t), intent(in) :: coverage_lines(:)
        character(len=:), allocatable, intent(out) :: formatted
        logical, intent(out) :: success
        
        character(len=:), allocatable :: lines(:)
        integer :: line_count, i
        character(len=:), allocatable :: line_html
        
        success = .true.
        formatted = ""
        
        ! Split source into lines
        call split_lines(source_code, lines, line_count)
        
        ! Process each line with coverage info
        do i = 1, line_count
            if (i <= size(coverage_lines)) then
                if (coverage_lines(i)%covered) then
                    line_html = '<div class="' // CSS_COVERED // '">'
                else if (coverage_lines(i)%is_executable) then
                    line_html = '<div class="' // CSS_UNCOVERED // '">'
                else
                    line_html = '<div>'
                end if
            else
                line_html = '<div>'
            end if
            
            if (i <= size(lines)) then
                line_html = line_html // html_escape(lines(i))
            end if
            line_html = line_html // '</div>'
            
            formatted = formatted // line_html
            if (i < line_count) then
                formatted = formatted // achar(10)
            end if
        end do
    end subroutine formatter_format_with_coverage
    
    ! Escape HTML special characters
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
    
    ! Split text into lines
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
        do i = 1, line_count
            lines(i) = temp_lines(i)
        end do
    end subroutine split_lines

end module syntax_formatter