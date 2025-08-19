module json_parser
    !! JSON Parser (Decomposed from json_coverage_io.f90)
    !! 
    !! Focused on JSON parsing logic and tokenization.
    !! Separated from I/O operations and validation for better separation of concerns.
    use foundation_constants
    use foundation_layer_utils
    use coverage_model
    use iso_fortran_env, only: int64
    implicit none
    private
    
    public :: json_token_t
    public :: tokenize_json_content
    public :: parse_coverage_object_from_tokens
    public :: expect_token_type
    public :: parse_files_array_from_tokens
    public :: parse_file_object_from_tokens
    public :: parse_lines_array_from_tokens
    
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
    
contains
    
    subroutine tokenize_json_content(json_content, tokens, token_count, error_caught)
        !! JSON tokenization implementation
        !! Extracted from original tokenize_json function
        character(len=*), intent(in) :: json_content
        type(json_token_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: token_count
        logical, intent(out) :: error_caught
        
        integer :: pos, content_len
        character(len=1) :: current_char
        type(json_token_t) :: temp_tokens(1000)  ! Temporary storage
        
        error_caught = .false.
        token_count = 0
        content_len = len_trim(json_content)
        pos = 1
        
        ! Tokenize character by character
        do while (pos <= content_len)
            current_char = json_content(pos:pos)
            
            ! Skip whitespace
            if (is_whitespace(current_char)) then
                pos = pos + 1
                cycle
            end if
            
            ! Parse different token types
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
                call parse_string_token(json_content, pos, temp_tokens, token_count, error_caught)
                if (error_caught) return
            case default
                if (is_digit(current_char) .or. current_char == '-') then
                    call parse_number_token(json_content, pos, temp_tokens, token_count, error_caught)
                    if (error_caught) return
                else if (is_alpha(current_char)) then
                    call parse_literal_token(json_content, pos, temp_tokens, token_count, error_caught)
                    if (error_caught) return
                else
                    error_caught = .true.
                    return
                end if
            end select
        end do
        
        ! Copy tokens to output array
        if (token_count > 0) then
            allocate(tokens(token_count))
            tokens(1:token_count) = temp_tokens(1:token_count)
        end if
        
    end subroutine tokenize_json_content
    
    subroutine parse_coverage_object_from_tokens(tokens, current_pos, token_count, &
                                               coverage_data, parse_error)
        !! Parses coverage object from JSON tokens
        !! Extracted from original parse_coverage_object function
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: parse_error
        
        character(len=:), allocatable :: key_name
        integer :: files_start_pos
        
        parse_error = .false.
        
        ! Expect opening brace
        if (.not. expect_token_type(tokens, current_pos, token_count, "{")) then
            parse_error = .true.
            return
        end if
        
        ! Parse object fields
        do while (current_pos <= token_count)
            ! Check for closing brace
            if (tokens(current_pos)%value == "}") then
                current_pos = current_pos + 1
                exit
            end if
            
            ! Parse key-value pair
            call parse_key_value_pair(tokens, current_pos, token_count, key_name, parse_error)
            if (parse_error) return
            
            ! Handle specific keys
            select case (trim(key_name))
            case ("files")
                files_start_pos = current_pos
                call parse_files_array_from_tokens(tokens, current_pos, token_count, &
                                                 coverage_data, parse_error)
                if (parse_error) return
            case ("version")
                call parse_string_value(tokens, current_pos, coverage_data%version, parse_error)
                if (parse_error) return
            case ("tool")
                call parse_string_value(tokens, current_pos, coverage_data%tool, parse_error)
                if (parse_error) return
            case default
                ! Skip unknown fields
                call skip_value(tokens, current_pos, token_count, parse_error)
                if (parse_error) return
            end select
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
    end subroutine parse_coverage_object_from_tokens
    
    subroutine parse_files_array_from_tokens(tokens, current_pos, token_count, &
                                           coverage_data, parse_error)
        !! Parses files array from JSON tokens
        !! Extracted from original parse_files_array function
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_data_t), intent(inout) :: coverage_data
        logical, intent(out) :: parse_error
        
        type(file_coverage_t), allocatable :: temp_files(:)
        integer :: files_count, capacity
        
        parse_error = .false.
        files_count = 0
        capacity = 10
        
        ! Allocate initial capacity
        allocate(temp_files(capacity))
        
        ! Expect opening bracket
        if (.not. expect_token_type(tokens, current_pos, token_count, "[")) then
            parse_error = .true.
            return
        end if
        
        ! Parse array elements
        do while (current_pos <= token_count)
            ! Check for closing bracket
            if (tokens(current_pos)%value == "]") then
                current_pos = current_pos + 1
                exit
            end if
            
            ! Grow array if needed
            if (files_count >= capacity) then
                call grow_files_array(temp_files, capacity)
            end if
            
            files_count = files_count + 1
            call parse_file_object_from_tokens(tokens, current_pos, token_count, &
                                             temp_files(files_count), parse_error)
            if (parse_error) return
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
        ! Copy files to coverage data
        if (files_count > 0) then
            allocate(coverage_data%files_json(files_count))
            coverage_data%files_json(1:files_count) = temp_files(1:files_count)
        end if
        
    end subroutine parse_files_array_from_tokens
    
    subroutine parse_file_object_from_tokens(tokens, current_pos, token_count, &
                                           file_obj, parse_error)
        !! Parses file object from JSON tokens
        !! Extracted from original parse_file_object function
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(file_coverage_t), intent(out) :: file_obj
        logical, intent(out) :: parse_error
        
        character(len=:), allocatable :: key_name
        
        parse_error = .false.
        
        ! Expect opening brace
        if (.not. expect_token_type(tokens, current_pos, token_count, "{")) then
            parse_error = .true.
            return
        end if
        
        ! Parse object fields
        do while (current_pos <= token_count)
            ! Check for closing brace
            if (tokens(current_pos)%value == "}") then
                current_pos = current_pos + 1
                exit
            end if
            
            ! Parse key-value pair
            call parse_key_value_pair(tokens, current_pos, token_count, key_name, parse_error)
            if (parse_error) return
            
            ! Handle specific keys
            select case (trim(key_name))
            case ("filename")
                call parse_string_value(tokens, current_pos, file_obj%filename, parse_error)
                if (parse_error) return
            case ("lines")
                call parse_lines_array_from_tokens(tokens, current_pos, token_count, &
                                                 file_obj, parse_error)
                if (parse_error) return
            case default
                ! Skip unknown fields
                call skip_value(tokens, current_pos, token_count, parse_error)
                if (parse_error) return
            end select
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
    end subroutine parse_file_object_from_tokens
    
    subroutine parse_lines_array_from_tokens(tokens, current_pos, token_count, &
                                           file_obj, parse_error)
        !! Parses lines array from JSON tokens
        !! Extracted from original parse_lines_array function
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(file_coverage_t), intent(inout) :: file_obj
        logical, intent(out) :: parse_error
        
        type(line_coverage_t), allocatable :: temp_lines(:)
        integer :: lines_count, capacity
        
        parse_error = .false.
        lines_count = 0
        capacity = 100
        
        ! Allocate initial capacity
        allocate(temp_lines(capacity))
        
        ! Expect opening bracket
        if (.not. expect_token_type(tokens, current_pos, token_count, "[")) then
            parse_error = .true.
            return
        end if
        
        ! Parse array elements
        do while (current_pos <= token_count)
            ! Check for closing bracket
            if (tokens(current_pos)%value == "]") then
                current_pos = current_pos + 1
                exit
            end if
            
            ! Grow array if needed
            if (lines_count >= capacity) then
                call grow_lines_array(temp_lines, capacity)
            end if
            
            lines_count = lines_count + 1
            call parse_line_object(tokens, current_pos, token_count, &
                                 temp_lines(lines_count), parse_error)
            if (parse_error) return
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
        ! Copy lines to file object
        if (lines_count > 0) then
            allocate(file_obj%lines(lines_count))
            file_obj%lines(1:lines_count) = temp_lines(1:lines_count)
        end if
        
    end subroutine parse_lines_array_from_tokens
    
    ! Helper parsing functions
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
        start_pos = pos + 1  ! Skip opening quote
        end_pos = start_pos
        
        ! Find closing quote
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
        
        ! Extract string value
        string_value = json_content(start_pos:end_pos-1)
        call unescape_json_string(string_value)
        
        call add_token(tokens, token_count, JSON_STRING, string_value, start_pos, end_pos)
        pos = end_pos + 1  ! Skip closing quote
        
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
        
        ! Find end of number
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
        
        integer :: start_pos, end_pos
        character(len=:), allocatable :: literal_value
        integer :: token_type
        
        error_caught = .false.
        start_pos = pos
        end_pos = pos
        
        ! Find end of literal
        do while (end_pos <= len(json_content) .and. is_alpha(json_content(end_pos:end_pos)))
            end_pos = end_pos + 1
        end do
        
        literal_value = json_content(start_pos:end_pos-1)
        
        ! Determine token type
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
    
    ! Utility functions
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
        ! Would need full JSON string unescaping logic
        
    end subroutine unescape_json_string
    
    subroutine parse_key_value_pair(tokens, current_pos, token_count, key_name, parse_error)
        !! Parses a key-value pair from JSON tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=:), allocatable, intent(out) :: key_name
        logical, intent(out) :: parse_error
        
        parse_error = .false.
        
        ! Expect string key
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_STRING) then
            parse_error = .true.
            return
        end if
        
        key_name = tokens(current_pos)%value
        current_pos = current_pos + 1
        
        ! Expect colon
        if (current_pos > token_count .or. tokens(current_pos)%value /= ":") then
            parse_error = .true.
            return
        end if
        
        current_pos = current_pos + 1
        
    end subroutine parse_key_value_pair

    subroutine parse_string_value(tokens, current_pos, output_string, parse_error)
        !! Parses a string value from JSON tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        character(len=:), allocatable, intent(out) :: output_string
        logical, intent(out) :: parse_error
        
        parse_error = .false.
        
        if (tokens(current_pos)%type /= JSON_STRING) then
            parse_error = .true.
            return
        end if
        
        output_string = tokens(current_pos)%value
        current_pos = current_pos + 1
        
    end subroutine parse_string_value

    subroutine parse_number_value(tokens, current_pos, output_number, parse_error)
        !! Parses a number value from JSON tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(out) :: output_number
        logical, intent(out) :: parse_error
        
        integer :: iostat_value
        
        parse_error = .false.
        
        if (tokens(current_pos)%type /= JSON_NUMBER) then
            parse_error = .true.
            return
        end if
        
        read(tokens(current_pos)%value, *, iostat=iostat_value) output_number
        parse_error = (iostat_value /= 0)
        
        if (.not. parse_error) then
            current_pos = current_pos + 1
        end if
        
    end subroutine parse_number_value

    subroutine skip_value(tokens, current_pos, token_count, parse_error)
        !! Skips a JSON value (string, number, object, array, literal)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        logical, intent(out) :: parse_error
        
        integer :: depth
        
        parse_error = .false.
        
        if (current_pos > token_count) then
            parse_error = .true.
            return
        end if
        
        select case (tokens(current_pos)%type)
        case (JSON_STRING, JSON_NUMBER, JSON_BOOLEAN, JSON_NULL)
            ! Simple values - just skip
            current_pos = current_pos + 1
        case (JSON_OBJECT)
            if (tokens(current_pos)%value == "{") then
                depth = 1
                current_pos = current_pos + 1
                do while (current_pos <= token_count .and. depth > 0)
                    if (tokens(current_pos)%value == "{") then
                        depth = depth + 1
                    else if (tokens(current_pos)%value == "}") then
                        depth = depth - 1
                    end if
                    current_pos = current_pos + 1
                end do
            end if
        case (JSON_ARRAY)
            if (tokens(current_pos)%value == "[") then
                depth = 1
                current_pos = current_pos + 1
                do while (current_pos <= token_count .and. depth > 0)
                    if (tokens(current_pos)%value == "[") then
                        depth = depth + 1
                    else if (tokens(current_pos)%value == "]") then
                        depth = depth - 1
                    end if
                    current_pos = current_pos + 1
                end do
            end if
        case default
            current_pos = current_pos + 1
        end select
        
    end subroutine skip_value

    subroutine grow_files_array(temp_files, capacity)
        !! Grows the files array by doubling capacity
        type(file_coverage_t), allocatable, intent(inout) :: temp_files(:)
        integer, intent(inout) :: capacity
        
        type(file_coverage_t), allocatable :: new_files(:)
        integer :: old_capacity
        
        old_capacity = capacity
        capacity = capacity * 2
        
        allocate(new_files(capacity))
        new_files(1:old_capacity) = temp_files(1:old_capacity)
        call move_alloc(new_files, temp_files)
        
    end subroutine grow_files_array

    subroutine parse_line_object(tokens, current_pos, token_count, line_obj, parse_error)
        !! Parses a line coverage object from JSON tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(line_coverage_t), intent(out) :: line_obj
        logical, intent(out) :: parse_error
        
        character(len=:), allocatable :: key_name
        
        parse_error = .false.
        
        ! Initialize line object
        line_obj%line_number = 0
        line_obj%execution_count = 0
        
        ! Expect opening brace
        if (.not. expect_token_type(tokens, current_pos, token_count, "{")) then
            parse_error = .true.
            return
        end if
        
        ! Parse object fields
        do while (current_pos <= token_count)
            ! Check for closing brace
            if (tokens(current_pos)%value == "}") then
                current_pos = current_pos + 1
                exit
            end if
            
            ! Parse key-value pair
            call parse_key_value_pair(tokens, current_pos, token_count, key_name, parse_error)
            if (parse_error) return
            
            ! Handle specific keys
            select case (trim(key_name))
            case ("line_number")
                call parse_number_value(tokens, current_pos, line_obj%line_number, parse_error)
                if (parse_error) return
            case ("execution_count")
                call parse_number_value(tokens, current_pos, line_obj%execution_count, parse_error)
                if (parse_error) return
            case default
                ! Skip unknown fields
                call skip_value(tokens, current_pos, token_count, parse_error)
                if (parse_error) return
            end select
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
    end subroutine parse_line_object

    subroutine grow_lines_array(temp_lines, capacity)
        !! Grows the lines array by doubling capacity
        type(line_coverage_t), allocatable, intent(inout) :: temp_lines(:)
        integer, intent(inout) :: capacity
        
        type(line_coverage_t), allocatable :: new_lines(:)
        integer :: old_capacity
        
        old_capacity = capacity
        capacity = capacity * 2
        
        allocate(new_lines(capacity))
        new_lines(1:old_capacity) = temp_lines(1:old_capacity)
        call move_alloc(new_lines, temp_lines)
        
    end subroutine grow_lines_array

    function expect_token_type(tokens, current_pos, token_count, expected_value) result(matches)
        !! Checks if current token matches expected value
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: expected_value
        logical :: matches
        
        matches = .false.
        
        if (current_pos <= token_count) then
            if (tokens(current_pos)%value == expected_value) then
                matches = .true.
                current_pos = current_pos + 1
            end if
        end if
        
    end function expect_token_type
    
end module json_parser