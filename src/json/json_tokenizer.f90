module json_tokenizer
    !! JSON Tokenization Module
    !!
    !! Handles tokenization of JSON content including:
    !! - Token type definitions and structures
    !! - String, number, and literal parsing
    !! - Token stream creation and management
    implicit none
    private
    
    ! JSON token types
    integer, parameter, public :: JSON_NULL = 0
    integer, parameter, public :: JSON_STRING = 1
    integer, parameter, public :: JSON_NUMBER = 2
    integer, parameter, public :: JSON_OBJECT = 3
    integer, parameter, public :: JSON_ARRAY = 4
    integer, parameter, public :: JSON_BOOLEAN = 5
    
    ! JSON token type
    type, public :: json_token_t
        integer :: type = JSON_NULL
        character(len=:), allocatable :: value
        integer :: start_pos = 0
        integer :: end_pos = 0
    end type json_token_t
    
    ! Public interface
    public :: tokenize_json_content
    
contains
    
    subroutine tokenize_json_content(json_content, tokens, token_count, error_caught)
        !! JSON tokenization implementation
        character(len=*), intent(in) :: json_content
        type(json_token_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: token_count
        logical, intent(out) :: error_caught
        
        integer :: pos, content_len
        character(len=1) :: current_char
        type(json_token_t) :: temp_tokens(1000)
        
        error_caught = .false.
        token_count = 0
        content_len = len_trim(json_content)
        pos = 1
        
        do while (pos <= content_len)
            current_char = json_content(pos:pos)
            
            if (is_whitespace(current_char)) then
                pos = pos + 1
                cycle
            end if
            
            select case (current_char)
            case ('{')
                call add_token(temp_tokens, token_count, JSON_OBJECT, "{", pos, pos)
                pos = pos + 1
            case ('}')
                call add_token(temp_tokens, token_count, JSON_OBJECT, "}", pos, pos)
                pos = pos + 1
            case ('[')
                call add_token(temp_tokens, token_count, JSON_ARRAY, "[", pos, pos)
                pos = pos + 1
            case (']')
                call add_token(temp_tokens, token_count, JSON_ARRAY, "]", pos, pos)
                pos = pos + 1
            case (',')
                call add_token(temp_tokens, token_count, JSON_NULL, ",", pos, pos)
                pos = pos + 1
            case (':')
                call add_token(temp_tokens, token_count, JSON_NULL, ":", pos, pos)
                pos = pos + 1
            case ('"')
                call parse_string_token(json_content, pos, temp_tokens, &
                                      token_count, error_caught)
                if (error_caught) return
            case default
                if (is_digit(current_char) .or. current_char == '-') then
                    call parse_number_token(json_content, pos, temp_tokens, &
                                          token_count, error_caught)
                    if (error_caught) return
                else if (is_alpha(current_char)) then
                    call parse_literal_token(json_content, pos, temp_tokens, &
                                           token_count, error_caught)
                    if (error_caught) return
                else
                    error_caught = .true.
                    return
                end if
            end select
        end do
        
        if (token_count > 0) then
            allocate(tokens(token_count))
            tokens(1:token_count) = temp_tokens(1:token_count)
        end if
    end subroutine tokenize_json_content
    
    subroutine parse_string_token(json_content, pos, tokens, token_count, error_caught)
        !! Parses a JSON string token
        character(len=*), intent(in) :: json_content
        integer, intent(inout) :: pos
        type(json_token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count
        logical, intent(out) :: error_caught
        
        integer :: start_pos, end_pos
        character(len=:), allocatable :: string_value
        
        error_caught = .false.
        start_pos = pos + 1
        end_pos = start_pos
        
        do while (end_pos <= len(json_content))
            if (json_content(end_pos:end_pos) == '"') then
                exit
            end if
            end_pos = end_pos + 1
        end do
        
        if (end_pos > len(json_content)) then
            error_caught = .true.
            return
        end if
        
        string_value = json_content(start_pos:end_pos-1)
        call unescape_json_string(string_value)
        
        call add_token(tokens, token_count, JSON_STRING, string_value, start_pos, end_pos)
        pos = end_pos + 1
    end subroutine parse_string_token
    
    subroutine parse_number_token(json_content, pos, tokens, token_count, error_caught)
        !! Parses a JSON number token
        character(len=*), intent(in) :: json_content
        integer, intent(inout) :: pos
        type(json_token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count
        logical, intent(out) :: error_caught
        
        integer :: start_pos, end_pos
        character(len=:), allocatable :: number_value
        
        error_caught = .false.
        start_pos = pos
        end_pos = pos
        
        do while (end_pos <= len(json_content))
            if (.not. (is_digit(json_content(end_pos:end_pos)) .or. &
                      json_content(end_pos:end_pos) == '.' .or. &
                      json_content(end_pos:end_pos) == '-' .or. &
                      json_content(end_pos:end_pos) == 'e' .or. &
                      json_content(end_pos:end_pos) == 'E')) then
                exit
            end if
            end_pos = end_pos + 1
        end do
        
        number_value = json_content(start_pos:end_pos-1)
        call add_token(tokens, token_count, JSON_NUMBER, number_value, start_pos, end_pos-1)
        pos = end_pos
    end subroutine parse_number_token
    
    subroutine parse_literal_token(json_content, pos, tokens, token_count, error_caught)
        !! Parses JSON literal tokens (true, false, null)
        character(len=*), intent(in) :: json_content
        integer, intent(inout) :: pos
        type(json_token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count
        logical, intent(out) :: error_caught
        
        integer :: start_pos, end_pos, token_type
        character(len=:), allocatable :: literal_value
        
        error_caught = .false.
        start_pos = pos
        end_pos = pos
        
        do while (end_pos <= len(json_content) .and. is_alpha(json_content(end_pos:end_pos)))
            end_pos = end_pos + 1
        end do
        
        literal_value = json_content(start_pos:end_pos-1)
        
        select case (trim(literal_value))
        case ("true", "false")
            token_type = JSON_BOOLEAN
        case ("null")
            token_type = JSON_NULL
        case default
            error_caught = .true.
            return
        end select
        
        call add_token(tokens, token_count, token_type, literal_value, start_pos, end_pos-1)
        pos = end_pos
    end subroutine parse_literal_token
    
    ! === HELPER FUNCTIONS ===
    
    function is_whitespace(c) result(is_ws)
        character(len=1), intent(in) :: c
        logical :: is_ws
        
        is_ws = (c == ' ' .or. c == achar(9) .or. c == achar(10) .or. c == achar(13))
    end function is_whitespace
    
    function is_digit(c) result(is_d)
        character(len=1), intent(in) :: c
        logical :: is_d
        
        is_d = (c >= '0' .and. c <= '9')
    end function is_digit
    
    function is_alpha(c) result(is_a)
        character(len=1), intent(in) :: c
        logical :: is_a
        
        is_a = ((c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z'))
    end function is_alpha
    
    subroutine add_token(tokens, token_count, token_type, value, start_pos, end_pos)
        !! Adds a token to the tokens array
        type(json_token_t), intent(inout) :: tokens(:)
        integer, intent(inout) :: token_count
        integer, intent(in) :: token_type
        character(len=*), intent(in) :: value
        integer, intent(in) :: start_pos, end_pos
        
        token_count = token_count + 1
        tokens(token_count)%type = token_type
        tokens(token_count)%value = value
        tokens(token_count)%start_pos = start_pos
        tokens(token_count)%end_pos = end_pos
    end subroutine add_token
    
    subroutine unescape_json_string(string_value)
        !! Unescapes JSON string content
        character(len=:), allocatable, intent(inout) :: string_value
        
        ! Simple implementation for now
    end subroutine unescape_json_string

end module json_tokenizer