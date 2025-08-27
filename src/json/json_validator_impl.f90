module json_validator_impl
    !! JSON Validator (Decomposed from json_coverage_io.f90)
    !! 
    !! Focused on JSON validation and format checking for coverage data.
    !! Separated from parsing and I/O operations for better separation of concerns.
    use constants_core
    use json_parser_core, only: json_token_t, expect_token_type, JSON_STRING, JSON_NUMBER, &
                          JSON_OBJECT, JSON_ARRAY, JSON_BOOLEAN, JSON_NULL, &
                          tokenize_json_content
    use coverage_model_core
    implicit none
    private
    
    public :: validate_coverage_json_structure
    public :: validate_json_syntax
    public :: validate_coverage_schema
    public :: validate_file_object_schema
    public :: validate_line_object_schema
    
contains
    
    function validate_coverage_json_structure(json_content) result(is_valid)
        !! Validates JSON structure for coverage data
        character(len=*), intent(in) :: json_content
        logical :: is_valid
        
        logical :: syntax_valid, schema_valid
        
        is_valid = .true.
        
        ! First validate JSON syntax
        syntax_valid = validate_json_syntax(json_content)
        if (.not. syntax_valid) then
            is_valid = .false.
            return
        end if
        
        ! Then validate coverage schema
        schema_valid = validate_coverage_schema(json_content)
        if (.not. schema_valid) then
            is_valid = .false.
            return
        end if
        
    end function validate_coverage_json_structure
    
    function validate_json_syntax(json_content) result(is_valid)
        !! Validates basic JSON syntax
        character(len=*), intent(in) :: json_content
        logical :: is_valid
        
        type(json_token_t), allocatable :: tokens(:)
        integer :: token_count
        logical :: tokenize_error
        
        is_valid = .true.
        
        ! Try to tokenize the JSON
        call tokenize_json_content(json_content, tokens, token_count, tokenize_error)
        if (tokenize_error) then
            is_valid = .false.
            return
        end if
        
        ! Validate bracket/brace matching
        if (.not. validate_bracket_matching(tokens, token_count)) then
            is_valid = .false.
            return
        end if
        
        ! Validate basic structure
        if (.not. validate_json_structure(tokens, token_count)) then
            is_valid = .false.
            return
        end if
        
    end function validate_json_syntax
    
    function validate_coverage_schema(json_content) result(is_valid)
        !! Validates coverage data schema requirements
        character(len=*), intent(in) :: json_content
        logical :: is_valid
        
        type(json_token_t), allocatable :: tokens(:)
        integer :: token_count, current_pos
        logical :: tokenize_error
        logical :: has_files_array, has_valid_structure
        
        is_valid = .true.
        
        ! Tokenize JSON
        call tokenize_json_content(json_content, tokens, token_count, tokenize_error)
        if (tokenize_error) then
            is_valid = .false.
            return
        end if
        
        ! Check for required fields
        current_pos = 1
        has_files_array = find_key_in_object(tokens, current_pos, token_count, "files")
        if (.not. has_files_array) then
            is_valid = .false.
            return
        end if
        
        ! Validate files array structure
        current_pos = 1
        has_valid_structure = validate_files_array_structure(tokens, current_pos, token_count)
        if (.not. has_valid_structure) then
            is_valid = .false.
            return
        end if
        
    end function validate_coverage_schema
    
    function validate_file_object_schema(tokens, current_pos, token_count) result(is_valid)
        !! Validates file object schema
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        logical :: is_valid
        
        logical :: has_filename, has_lines
        integer :: start_pos
        
        is_valid = .true.
        start_pos = current_pos
        
        ! Skip to object content
        if (.not. expect_token_type(tokens, current_pos, token_count, "{")) then
            is_valid = .false.
            return
        end if
        
        ! Check for required fields
        has_filename = find_key_in_current_object(tokens, current_pos, token_count, "filename")
        if (.not. has_filename) then
            is_valid = .false.
            return
        end if
        
        current_pos = start_pos + 1  ! Reset position
        has_lines = find_key_in_current_object(tokens, current_pos, token_count, "lines")
        if (.not. has_lines) then
            is_valid = .false.
            return
        end if
        
        ! Validate lines array
        current_pos = start_pos + 1
        if (.not. validate_lines_array_structure(tokens, current_pos, token_count)) then
            is_valid = .false.
            return
        end if
        
    end function validate_file_object_schema
    
    function validate_line_object_schema(tokens, current_pos, token_count) result(is_valid)
        !! Validates line object schema
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        logical :: is_valid
        
        logical :: has_line_number, has_execution_count
        integer :: start_pos
        
        is_valid = .true.
        start_pos = current_pos
        
        ! Skip to object content
        if (.not. expect_token_type(tokens, current_pos, token_count, "{")) then
            is_valid = .false.
            return
        end if
        
        ! Check for required fields
        has_line_number = find_key_in_current_object(tokens, current_pos, token_count, "line_number")
        if (.not. has_line_number) then
            is_valid = .false.
            return
        end if
        
        current_pos = start_pos + 1  ! Reset position
        has_execution_count = find_key_in_current_object(tokens, current_pos, token_count, &
                                                        "execution_count")
        if (.not. has_execution_count) then
            is_valid = .false.
            return
        end if
        
    end function validate_line_object_schema
    
    ! Helper validation functions
    function validate_bracket_matching(tokens, token_count) result(is_valid)
        !! Validates that brackets and braces are properly matched
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(in) :: token_count
        logical :: is_valid
        
        integer :: i, brace_count, bracket_count
        
        is_valid = .true.
        brace_count = 0
        bracket_count = 0
        
        do i = 1, token_count
            select case (trim(tokens(i)%value))
            case ("{")
                brace_count = brace_count + 1
            case ("}")
                brace_count = brace_count - 1
                if (brace_count < 0) then
                    is_valid = .false.
                    return
                end if
            case ("[")
                bracket_count = bracket_count + 1
            case ("]")
                bracket_count = bracket_count - 1
                if (bracket_count < 0) then
                    is_valid = .false.
                    return
                end if
            end select
        end do
        
        if (brace_count /= 0 .or. bracket_count /= 0) then
            is_valid = .false.
        end if
        
    end function validate_bracket_matching
    
    function validate_json_structure(tokens, token_count) result(is_valid)
        !! Validates basic JSON structure rules
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(in) :: token_count
        logical :: is_valid
        
        integer :: i
        
        is_valid = .true.
        
        ! Basic structure validation
        if (token_count < 2) then
            is_valid = .false.
            return
        end if
        
        ! Should start with { or [
        if (.not. (tokens(1)%value == "{" .or. tokens(1)%value == "[")) then
            is_valid = .false.
            return
        end if
        
        ! Should end with } or ]
        if (.not. (tokens(token_count)%value == "}" .or. tokens(token_count)%value == "]")) then
            is_valid = .false.
            return
        end if
        
        ! Additional structure validation could be added here
        
    end function validate_json_structure
    
    function validate_files_array_structure(tokens, current_pos, token_count) result(is_valid)
        !! Validates the structure of the files array
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        logical :: is_valid
        
        integer :: array_start
        
        is_valid = .true.
        
        ! Find files array
        if (.not. find_key_in_object(tokens, current_pos, token_count, "files")) then
            is_valid = .false.
            return
        end if
        
        ! Validate array structure
        array_start = current_pos
        if (.not. expect_token_type(tokens, current_pos, token_count, "[")) then
            is_valid = .false.
            return
        end if
        
        ! Validate file objects in array
        do while (current_pos <= token_count .and. tokens(current_pos)%value /= "]")
            if (tokens(current_pos)%value == "{") then
                if (.not. validate_file_object_schema(tokens, current_pos, token_count)) then
                    is_valid = .false.
                    return
                end if
            end if
            current_pos = current_pos + 1
        end do
        
    end function validate_files_array_structure
    
    function validate_lines_array_structure(tokens, current_pos, token_count) result(is_valid)
        !! Validates the structure of a lines array
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        logical :: is_valid
        
        is_valid = .true.
        
        ! Find lines array within current object
        if (.not. find_key_in_current_object(tokens, current_pos, token_count, "lines")) then
            is_valid = .false.
            return
        end if
        
        ! Validate array structure
        if (.not. expect_token_type(tokens, current_pos, token_count, "[")) then
            is_valid = .false.
            return
        end if
        
        ! Validate line objects in array
        do while (current_pos <= token_count .and. tokens(current_pos)%value /= "]")
            if (tokens(current_pos)%value == "{") then
                if (.not. validate_line_object_schema(tokens, current_pos, token_count)) then
                    is_valid = .false.
                    return
                end if
            end if
            current_pos = current_pos + 1
        end do
        
    end function validate_lines_array_structure
    
    function find_key_in_object(tokens, current_pos, token_count, key_name) result(found)
        !! Finds a key in a JSON object
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: key_name
        logical :: found
        
        integer :: i
        
        found = .false.
        
        do i = current_pos, token_count
            if (tokens(i)%type == JSON_STRING .and. tokens(i)%value == key_name) then
                ! Check if next token is colon
                if (i + 1 <= token_count .and. tokens(i + 1)%value == ":") then
                    found = .true.
                    current_pos = i + 2  ! Position after colon
                    return
                end if
            end if
        end do
        
    end function find_key_in_object
    
    function find_key_in_current_object(tokens, current_pos, token_count, key_name) result(found)
        !! Finds a key within the current object only
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: key_name
        logical :: found
        
        integer :: i, brace_count
        
        found = .false.
        brace_count = 0
        
        do i = current_pos, token_count
            ! Track brace depth
            if (tokens(i)%value == "{") then
                brace_count = brace_count + 1
            else if (tokens(i)%value == "}") then
                brace_count = brace_count - 1
                if (brace_count < 0) exit  ! Exited object
            end if
            
            if (tokens(i)%type == JSON_STRING .and. tokens(i)%value == key_name) then
                if (i + 1 <= token_count .and. tokens(i + 1)%value == ":") then
                    found = .true.
                    current_pos = i + 2
                    return
                end if
            end if
        end do
        
    end function find_key_in_current_object
    
    
end module json_validator_impl