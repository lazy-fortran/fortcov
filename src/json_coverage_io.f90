module json_coverage_io
    use coverage_model
    implicit none
    private
    
    ! Public interface procedures
    public :: import_json_coverage
    public :: export_json_coverage
    public :: import_json_coverage_safe
    
    ! Internal parameters for JSON parsing
    integer, parameter :: MAX_JSON_DEPTH = 10
    integer, parameter :: MAX_TOKEN_LENGTH = 1000
    
    ! JSON token types
    integer, parameter :: JSON_NULL = 0
    integer, parameter :: JSON_STRING = 1
    integer, parameter :: JSON_NUMBER = 2
    integer, parameter :: JSON_OBJECT = 3
    integer, parameter :: JSON_ARRAY = 4
    integer, parameter :: JSON_BOOLEAN = 5
    
    ! Simple JSON token type
    type :: json_token_t
        integer :: type = JSON_NULL
        character(len=:), allocatable :: value
        integer :: start_pos = 0
        integer :: end_pos = 0
    end type json_token_t

contains

    ! Helper subroutine to convert string to lowercase in place
    subroutine to_lower_inplace(str)
        character(len=*), intent(inout) :: str
        integer :: i, ascii_val
        
        do i = 1, len(str)
            ascii_val = iachar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then  ! A-Z
                str(i:i) = achar(ascii_val + 32)  ! Convert to a-z
            end if
        end do
    end subroutine to_lower_inplace

    ! Helper subroutine to unescape JSON string content
    subroutine unescape_json_string(escaped_str, unescaped_str)
        character(len=*), intent(in) :: escaped_str
        character(len=:), allocatable, intent(out) :: unescaped_str
        
        character(len=len(escaped_str)) :: temp_str
        integer :: i, j, escaped_len
        
        escaped_len = len(escaped_str)
        j = 1
        i = 1
        
        do while (i <= escaped_len)
            if (escaped_str(i:i) == '\' .and. i < escaped_len) then
                ! Handle escape sequence
                select case (escaped_str(i+1:i+1))
                case ('"')
                    temp_str(j:j) = '"'
                    i = i + 2
                case ('\')
                    temp_str(j:j) = '\'
                    i = i + 2
                case ('/')
                    temp_str(j:j) = '/'
                    i = i + 2
                case ('n')
                    temp_str(j:j) = new_line('A')
                    i = i + 2
                case ('r')
                    temp_str(j:j) = achar(13)  ! Carriage return
                    i = i + 2
                case ('t')
                    temp_str(j:j) = achar(9)   ! Tab
                    i = i + 2
                case default
                    ! Unknown escape sequence, keep as is
                    temp_str(j:j) = escaped_str(i:i)
                    i = i + 1
                end select
            else
                temp_str(j:j) = escaped_str(i:i)
                i = i + 1
            end if
            j = j + 1
        end do
        
        unescaped_str = temp_str(1:j-1)
    end subroutine unescape_json_string

    ! Import coverage data from JSON string
    subroutine import_json_coverage(json_content, coverage_data)
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        logical :: error_occurred
        
        call import_json_coverage_safe(json_content, coverage_data, error_occurred)
        
        if (error_occurred) then
            ! For non-safe version, we might want to stop execution
            ! For now, just create empty coverage data
            call coverage_data%init()
        end if
    end subroutine import_json_coverage

    ! Safe version with error handling
    subroutine import_json_coverage_safe(json_content, coverage_data, error_caught)
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_caught
        
        type(json_token_t), allocatable :: tokens(:)
        integer :: token_count, current_pos
        
        error_caught = .false.
        
        ! Initialize empty coverage data
        call coverage_data%init()
        
        ! Tokenize JSON
        call tokenize_json(json_content, tokens, token_count, error_caught)
        if (error_caught) return
        
        ! Parse tokens into coverage data - detect format
        current_pos = 1
        
        if (token_count > 0) then
            if (tokens(1)%value == '{') then
                ! Object-wrapped format: {"files": [...]}
                call parse_coverage_object(tokens, current_pos, token_count, &
                                          coverage_data, error_caught)
            else if (tokens(1)%value == '[') then
                ! Array format: [{...}, {...}]
                call parse_coverage_array(tokens, current_pos, token_count, &
                                         coverage_data, error_caught)
            else
                error_caught = .true.
            end if
        else
            error_caught = .true.
        end if
        
        if (error_caught) then
            ! Clean up and return empty data
            call coverage_data%init()
        end if
    end subroutine import_json_coverage_safe

    ! Export coverage data to JSON string
    subroutine export_json_coverage(coverage_data, json_output)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable, intent(out) :: json_output
        
        character(len=:), allocatable :: result
        integer :: i, j
        character(len=200) :: line_buffer, func_buffer
        
        result = '{"files": ['
        
        ! Memory safety: Check if files array is allocated
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
            if (i > 1) result = result // ', '
            
            result = result // '{"filename": "' // &
                    trim(coverage_data%files(i)%filename) // '", "lines": ['
            
                ! Memory safety: Check if lines array is allocated for this file
                if (allocated(coverage_data%files(i)%lines)) then
                    ! Add lines
                    do j = 1, size(coverage_data%files(i)%lines)
                if (j > 1) result = result // ', '
                
                block
                    character(len=5) :: bool_str
                    if (coverage_data%files(i)%lines(j)%is_executable) then
                        bool_str = "true"
                    else
                        bool_str = "false"
                    end if
                    write(line_buffer, '(A,I0,A,I0,A,A,A)') &
                        '{"line_number": ', &
                        coverage_data%files(i)%lines(j)%line_number, &
                        ', "execution_count": ', &
                        coverage_data%files(i)%lines(j)%execution_count, &
                        ', "is_executable": ', bool_str, '}'
                end block
                
                        result = result // trim(line_buffer)
                    end do
                end if
                
                result = result // ']'
            
                ! Add functions if they exist
                if (allocated(coverage_data%files(i)%functions)) then
                    if (size(coverage_data%files(i)%functions) > 0) then
                        result = result // ', "functions": ['
                        
                        do j = 1, size(coverage_data%files(i)%functions)
                        if (j > 1) result = result // ', '
                        
                        block
                            character(len=5) :: func_bool_str
                            if (coverage_data%files(i)%functions(j)%&
                                is_module_procedure) then
                                func_bool_str = "true"
                            else
                                func_bool_str = "false"
                            end if
                            write(func_buffer, '(A,A,A,A,A,A,A,I0,A,I0,A,A,A)') &
                                '{"name": "', &
                                trim(coverage_data%files(i)%functions(j)%name), &
                                '", "parent_module": "', &
                                trim(coverage_data%files(i)%functions(j)%&
                                     parent_module), &
                                '", "is_module_procedure": ', func_bool_str, &
                                ', "execution_count": ', &
                                coverage_data%files(i)%functions(j)%execution_count, &
                                ', "line_number": ', &
                                coverage_data%files(i)%functions(j)%line_number, &
                                ', "filename": "', &
                                trim(coverage_data%files(i)%functions(j)%filename), '"}'
                        end block
                        
                            result = result // trim(func_buffer)
                        end do
                        
                        result = result // ']'
                    end if
                end if
                
                result = result // '}'
            end do
        end if
        
        
        result = result // ']}'
        json_output = result
    end subroutine export_json_coverage

    ! Tokenize JSON string into tokens
    subroutine tokenize_json(json_content, tokens, token_count, error_caught)
        character(len=*), intent(in) :: json_content
        type(json_token_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: token_count
        logical, intent(out) :: error_caught
        
        integer :: pos, start_pos, content_length
        character :: current_char
        type(json_token_t) :: temp_tokens(1000)  ! Fixed size for simplicity
        
        error_caught = .false.
        token_count = 0
        content_length = len(json_content)
        pos = 1
        
        do while (pos <= content_length)
            current_char = json_content(pos:pos)
            
            ! Skip whitespace
            if (current_char == ' ' .or. current_char == char(9) .or. &
                current_char == char(10) .or. current_char == char(13)) then
                pos = pos + 1
                cycle
            end if
            
            token_count = token_count + 1
            if (token_count > size(temp_tokens)) then
                error_caught = .true.
                return
            end if
            
            start_pos = pos
            
            select case (current_char)
            case ('{', '}', '[', ']', ':', ',')
                ! Single character tokens
                temp_tokens(token_count)%type = JSON_OBJECT  ! Simplified
                temp_tokens(token_count)%value = current_char
                temp_tokens(token_count)%start_pos = pos
                temp_tokens(token_count)%end_pos = pos
                pos = pos + 1
                
            case ('"')
                ! String token with escape sequence handling
                pos = pos + 1  ! Skip opening quote
                start_pos = pos
                do while (pos <= content_length)
                    if (json_content(pos:pos) == '"') then
                        ! Found closing quote, check if it's escaped
                        if (pos > start_pos .and. json_content(pos-1:pos-1) == '\') then
                            ! Check for double-escaped backslash
                            if (pos > start_pos + 1 .and. &
                                json_content(pos-2:pos-1) == '\\') then
                                ! Properly escaped quote after backslash
                                exit
                            else
                                ! This is an escaped quote, continue looking
                                pos = pos + 1
                                cycle
                            end if
                        else
                            ! Unescaped closing quote found
                            exit
                        end if
                    end if
                    pos = pos + 1
                end do
                if (pos > content_length) then
                    error_caught = .true.
                    return
                end if
                
                temp_tokens(token_count)%type = JSON_STRING
                ! Unescape the string content
                call unescape_json_string(json_content(start_pos:pos-1), &
                                         temp_tokens(token_count)%value)
                temp_tokens(token_count)%start_pos = start_pos
                temp_tokens(token_count)%end_pos = pos - 1
                pos = pos + 1  ! Skip closing quote
                
            case ('0':'9', '-')
                ! Number token
                do while (pos <= content_length .and. &
                         (json_content(pos:pos) >= '0' .and. &
                          json_content(pos:pos) <= '9' .or. &
                          json_content(pos:pos) == '.' .or. &
                          json_content(pos:pos) == '-'))
                    pos = pos + 1
                end do
                
                temp_tokens(token_count)%type = JSON_NUMBER
                temp_tokens(token_count)%value = json_content(start_pos:pos-1)
                temp_tokens(token_count)%start_pos = start_pos
                temp_tokens(token_count)%end_pos = pos - 1
                
            case ('t', 'f', 'T', 'F')
                ! Boolean token (case-insensitive)
                block
                    logical :: found_boolean
                    found_boolean = .false.
                    
                    ! First check for 'true' (4 characters)
                    if (pos + 3 <= content_length) then
                        block
                            character(len=4) :: bool_text
                            bool_text = json_content(pos:pos+3)
                            call to_lower_inplace(bool_text)
                            if (bool_text == 'true') then
                                temp_tokens(token_count)%type = JSON_BOOLEAN
                                temp_tokens(token_count)%value = 'true'
                                pos = pos + 4
                                found_boolean = .true.
                            end if
                        end block
                    end if
                    
                    ! Then check for 'false' (5 characters) if true not found
                    if (.not. found_boolean .and. pos + 4 <= content_length) then
                        block
                            character(len=5) :: bool_text
                            bool_text = json_content(pos:pos+4)
                            call to_lower_inplace(bool_text)
                            if (bool_text == 'false') then
                                temp_tokens(token_count)%type = JSON_BOOLEAN
                                temp_tokens(token_count)%value = 'false'
                                pos = pos + 5
                                found_boolean = .true.
                            end if
                        end block
                    end if
                    
                    ! If neither true nor false matched, it's an error
                    if (.not. found_boolean) then
                        error_caught = .true.
                        return
                    end if
                end block
                temp_tokens(token_count)%start_pos = start_pos
                temp_tokens(token_count)%end_pos = pos - 1
                
            case default
                error_caught = .true.
                return
            end select
        end do
        
        ! Allocate and copy tokens
        allocate(tokens(token_count))
        tokens(1:token_count) = temp_tokens(1:token_count)
    end subroutine tokenize_json

    ! Parse coverage object from tokens
    subroutine parse_coverage_object(tokens, current_pos, token_count, coverage_data, &
                                   & error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_caught
        
        type(coverage_file_t), allocatable :: files(:)
        integer :: files_count
        
        error_caught = .false.
        
        ! Expect opening brace
        if (current_pos > token_count .or. tokens(current_pos)%value /= '{') then
            error_caught = .true.
            return
        end if
        current_pos = current_pos + 1
        
        ! Look for "files" key
        call skip_to_key(tokens, current_pos, token_count, 'files', error_caught)
        if (error_caught) return
        
        ! Parse files array
        call parse_files_array(tokens, current_pos, token_count, files, files_count, &
                             error_caught)
        if (error_caught) return
        
        ! Initialize coverage data with parsed files
        if (files_count > 0) then
            call coverage_data%init(files(1:files_count))
        else
            call coverage_data%init()
        end if
        
        if (allocated(files)) deallocate(files)
    end subroutine parse_coverage_object

    ! Parse JSON array format: [{...}, {...}]
    subroutine parse_coverage_array(tokens, pos, count, data, error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: pos
        integer, intent(in) :: count
        type(coverage_data_t), intent(out) :: data
        logical, intent(out) :: error_caught
        
        type(coverage_file_t), allocatable :: files(:)
        integer :: files_count
        
        error_caught = .false.
        
        ! Parse array of file objects directly (parse_files_array handles brackets)
        call parse_files_array(tokens, pos, count, files, &
                              files_count, error_caught)
        if (error_caught) return
        
        ! Initialize coverage data with parsed files
        if (files_count > 0) then
            call data%init(files(1:files_count))
        else
            call data%init()
        end if
        
        if (allocated(files)) deallocate(files)
    end subroutine parse_coverage_array

    ! Skip to specified key in JSON object
    subroutine skip_to_key(tokens, current_pos, token_count, key_name, error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: key_name
        logical, intent(out) :: error_caught
        
        error_caught = .false.
        
        do while (current_pos <= token_count)
            if (current_pos > token_count) then
                error_caught = .true.
                return
            end if
            
            if (tokens(current_pos)%type == JSON_STRING .and. &
                tokens(current_pos)%value == key_name) then
                current_pos = current_pos + 1  ! Skip key
                if (current_pos > token_count .or. &
                    tokens(current_pos)%value /= ':') then
                    error_caught = .true.
                    return
                end if
                current_pos = current_pos + 1  ! Skip colon
                return
            end if
            current_pos = current_pos + 1
        end do
        
        error_caught = .true.  ! Key not found
    end subroutine skip_to_key

    ! Parse files array from JSON
    subroutine parse_files_array(tokens, current_pos, token_count, files, files_count, &
                                & error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_file_t), allocatable, intent(out) :: files(:)
        integer, intent(out) :: files_count
        logical, intent(out) :: error_caught
        
        type(coverage_file_t) :: temp_files(100)  ! Fixed size for simplicity
        
        error_caught = .false.
        files_count = 0
        
        ! Expect opening bracket
        if (current_pos > token_count .or. tokens(current_pos)%value /= '[') then
            error_caught = .true.
            return
        end if
        current_pos = current_pos + 1
        
        ! Handle empty array
        if (current_pos <= token_count .and. tokens(current_pos)%value == ']') then
            current_pos = current_pos + 1
            allocate(files(0))
            return
        end if
        
        ! Parse each file object
        do while (current_pos <= token_count)
            if (tokens(current_pos)%value == ']') then
                current_pos = current_pos + 1
                exit
            end if
            
            files_count = files_count + 1
            if (files_count > size(temp_files)) then
                error_caught = .true.
                return
            end if
            
            call parse_file_object(tokens, current_pos, token_count, &
                                 temp_files(files_count), error_caught)
            if (error_caught) return
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ',') then
                current_pos = current_pos + 1
            end if
        end do
        
        ! Allocate and copy files
        if (files_count > 0) then
            allocate(files, source=temp_files(1:files_count))
        else
            allocate(files(0))
        end if
    end subroutine parse_files_array

    ! Parse individual file object from JSON
    subroutine parse_file_object(tokens, current_pos, token_count, &
                                 file_obj, error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_file_t), intent(out) :: file_obj
        logical, intent(out) :: error_caught
        
        character(len=:), allocatable :: filename
        type(coverage_line_t), allocatable :: lines(:)
        type(coverage_function_t), allocatable :: functions(:)
        integer :: lines_count, functions_count
        integer :: object_start_pos
        logical :: filename_found, lines_found
        
        error_caught = .false.
        filename_found = .false.
        lines_found = .false.
        
        ! Expect opening brace
        if (current_pos > token_count .or. tokens(current_pos)%value /= '{') then
            error_caught = .true.
            return
        end if
        current_pos = current_pos + 1
        object_start_pos = current_pos
        
        ! Parse filename - try from current position
        call try_parse_key_value(tokens, object_start_pos, token_count, 'filename', &
                                filename, filename_found)
        if (.not. filename_found) then
            error_caught = .true.
            return
        end if
        
        ! Parse lines - try from object start
        call try_parse_lines_field(tokens, object_start_pos, token_count, filename, &
                                  lines, lines_count, current_pos, lines_found)
        if (.not. lines_found) then
            error_caught = .true.
            return
        end if
        
        ! Try to parse functions (optional) - try from object start
        call try_parse_functions_array(tokens, object_start_pos, token_count, filename, &
                                     functions, functions_count)
        
        ! Skip to closing brace
        do while (current_pos <= token_count .and. tokens(current_pos)%value /= '}')
            current_pos = current_pos + 1
        end do
        if (current_pos <= token_count) &
            current_pos = current_pos + 1  ! Skip closing brace
        
        ! Initialize file object
        if (lines_count > 0) then
            call file_obj%init(filename, lines(1:lines_count))
        else
            ! Create empty lines array for file if not already allocated
            if (.not. allocated(lines)) then
                allocate(lines(0))
            end if
            call file_obj%init(filename, lines)
        end if
        
        ! Add functions if parsed
        if (functions_count > 0) then
            allocate(file_obj%functions, source=functions(1:functions_count))
        end if
        
        ! Clean up
        if (allocated(lines)) deallocate(lines)
        if (allocated(functions)) deallocate(functions)
    end subroutine parse_file_object

    ! Parse lines array from JSON
    subroutine parse_lines_array(tokens, current_pos, token_count, filename, lines, &
                                & lines_count, error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: filename
        type(coverage_line_t), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: lines_count
        logical, intent(out) :: error_caught
        
        type(coverage_line_t) :: temp_lines(1000)  ! Fixed size for simplicity
        
        error_caught = .false.
        lines_count = 0
        
        ! Expect opening bracket
        if (current_pos > token_count .or. tokens(current_pos)%value /= '[') then
            error_caught = .true.
            return
        end if
        current_pos = current_pos + 1
        
        ! Handle empty array
        if (current_pos <= token_count .and. tokens(current_pos)%value == ']') then
            current_pos = current_pos + 1
            allocate(lines(0))
            return
        end if
        
        ! Parse each line object
        do while (current_pos <= token_count)
            if (tokens(current_pos)%value == ']') then
                current_pos = current_pos + 1
                exit
            end if
            
            lines_count = lines_count + 1
            if (lines_count > size(temp_lines)) then
                error_caught = .true.
                return
            end if
            
            call parse_line_object(tokens, current_pos, token_count, filename, &
                                 temp_lines(lines_count), error_caught)
            if (error_caught) return
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ',') then
                current_pos = current_pos + 1
            end if
        end do
        
        ! Allocate and copy lines
        if (lines_count > 0) then
            allocate(lines, source=temp_lines(1:lines_count))
        else
            allocate(lines(0))
        end if
    end subroutine parse_lines_array

    ! Parse individual line object from JSON with field order independence
    subroutine parse_line_object(tokens, current_pos, token_count, filename, line_obj, &
                               & error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(out) :: line_obj
        logical, intent(out) :: error_caught
        
        integer :: line_number, execution_count
        logical :: is_executable
        integer :: object_start_pos
        logical :: line_number_found, execution_count_found, is_executable_found
        character(len=:), allocatable :: temp_value
        
        error_caught = .false.
        line_number_found = .false.
        execution_count_found = .false.
        is_executable_found = .false.
        
        ! Expect opening brace
        if (current_pos > token_count .or. tokens(current_pos)%value /= '{') then
            error_caught = .true.
            return
        end if
        current_pos = current_pos + 1
        object_start_pos = current_pos
        
        ! Parse line_number from any position in object
        call try_parse_integer_key(tokens, object_start_pos, token_count, 'line_number', &
                                  line_number, line_number_found)
        if (.not. line_number_found) then
            error_caught = .true.
            return
        end if
        
        ! Parse execution_count from any position in object
        call try_parse_integer_key(tokens, object_start_pos, token_count, 'execution_count', &
                                  execution_count, execution_count_found)
        if (.not. execution_count_found) then
            error_caught = .true.
            return
        end if
        
        ! Parse is_executable from any position in object
        call try_parse_boolean_key(tokens, object_start_pos, token_count, 'is_executable', &
                                  is_executable, is_executable_found)
        if (.not. is_executable_found) then
            error_caught = .true.
            return
        end if
        
        ! Skip to closing brace
        do while (current_pos <= token_count .and. tokens(current_pos)%value /= '}')
            current_pos = current_pos + 1
        end do
        if (current_pos <= token_count) &
            current_pos = current_pos + 1  ! Skip closing brace
        
        ! Initialize line object
        call line_obj%init(execution_count, line_number, filename, is_executable)
        error_caught = .false.
    end subroutine parse_line_object

    ! Try to parse functions array (optional field)
    subroutine try_parse_functions_array(tokens, current_pos, token_count, filename, &
                                       & functions, functions_count)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: filename
        type(coverage_function_t), allocatable, intent(out) :: functions(:)
        integer, intent(out) :: functions_count
        
        type(coverage_function_t) :: temp_functions(100)  ! Fixed size for simplicity
        integer :: saved_pos
        logical :: error_occurred
        
        functions_count = 0
        saved_pos = current_pos
        
        ! Try to find functions key
        call skip_to_key(tokens, current_pos, token_count, 'functions', error_occurred)
        if (error_occurred) then
            current_pos = saved_pos
            allocate(functions(0))
            return
        end if
        
        ! Parse functions array
        if (current_pos > token_count .or. tokens(current_pos)%value /= '[') then
            current_pos = saved_pos
            allocate(functions(0))
            return
        end if
        current_pos = current_pos + 1
        
        ! Handle empty array
        if (current_pos <= token_count .and. tokens(current_pos)%value == ']') then
            current_pos = current_pos + 1
            allocate(functions(0))
            return
        end if
        
        ! Parse each function object
        do while (current_pos <= token_count)
            if (tokens(current_pos)%value == ']') then
                current_pos = current_pos + 1
                exit
            end if
            
            functions_count = functions_count + 1
            if (functions_count > size(temp_functions)) exit
            
            call parse_function_object(tokens, current_pos, token_count, filename, &
                                     temp_functions(functions_count), error_occurred)
            if (error_occurred) then
                functions_count = functions_count - 1
                exit
            end if
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ',') then
                current_pos = current_pos + 1
            end if
        end do
        
        ! Allocate and copy functions
        allocate(functions(functions_count))
        if (functions_count > 0) then
            functions(1:functions_count) = temp_functions(1:functions_count)
        end if
    end subroutine try_parse_functions_array

    ! Parse individual function object from JSON
    subroutine parse_function_object(tokens, current_pos, token_count, filename, &
                                   & func_obj, error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: filename
        type(coverage_function_t), intent(out) :: func_obj
        logical, intent(out) :: error_caught
        
        character(len=:), allocatable :: name, parent_module
        logical :: is_module_procedure
        integer :: execution_count, line_number
        
        error_caught = .false.
        
        ! Expect opening brace
        if (current_pos > token_count .or. tokens(current_pos)%value /= '{') then
            error_caught = .true.
            return
        end if
        current_pos = current_pos + 1
        
        ! Parse name
        call skip_to_key(tokens, current_pos, token_count, 'name', error_caught)
        if (error_caught) return
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_STRING) then
            error_caught = .true.
            return
        end if
        name = tokens(current_pos)%value
        current_pos = current_pos + 1
        
        ! Parse parent_module
        call skip_to_key(tokens, current_pos, token_count, &
                         'parent_module', error_caught)
        if (error_caught) return
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_STRING) then
            error_caught = .true.
            return
        end if
        parent_module = tokens(current_pos)%value
        current_pos = current_pos + 1
        
        ! Parse is_module_procedure
        call skip_to_key(tokens, current_pos, token_count, &
                         'is_module_procedure', error_caught)
        if (error_caught) return
        if (current_pos > token_count .or. &
            tokens(current_pos)%type /= JSON_BOOLEAN) then
            error_caught = .true.
            return
        end if
        is_module_procedure = (tokens(current_pos)%value == 'true')
        current_pos = current_pos + 1
        
        ! Parse execution_count
        call skip_to_key(tokens, current_pos, token_count, &
                         'execution_count', error_caught)
        if (error_caught) return
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_NUMBER) then
            error_caught = .true.
            return
        end if
        block
            integer :: iostat_val
            read(tokens(current_pos)%value, *, iostat=iostat_val) execution_count
            if (iostat_val /= 0) then
                error_caught = .true.
                return
            end if
        end block
        current_pos = current_pos + 1
        
        ! Parse line_number
        call skip_to_key(tokens, current_pos, token_count, 'line_number', error_caught)
        if (error_caught) return
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_NUMBER) then
            error_caught = .true.
            return
        end if
        block
            integer :: iostat_val
            read(tokens(current_pos)%value, *, iostat=iostat_val) line_number
            if (iostat_val /= 0) then
                error_caught = .true.
                return
            end if
        end block
        current_pos = current_pos + 1
        
        ! Skip to closing brace
        do while (current_pos <= token_count .and. tokens(current_pos)%value /= '}')
            current_pos = current_pos + 1
        end do
        if (current_pos <= token_count) &
            current_pos = current_pos + 1  ! Skip closing brace
        
        ! Initialize function object
        call func_obj%init(name, parent_module, is_module_procedure, execution_count, &
                          line_number, filename)
        error_caught = .false.
    end subroutine parse_function_object

    ! Helper subroutine to parse any key-value pair of string type
    subroutine try_parse_key_value(tokens, start_pos, token_count, key_name, value, found)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos, token_count
        character(len=*), intent(in) :: key_name
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        
        integer :: pos
        
        found = .false.
        pos = start_pos
        
        do while (pos <= token_count)
            if (tokens(pos)%type == JSON_STRING .and. &
                tokens(pos)%value == key_name) then
                pos = pos + 1  ! Skip key
                if (pos <= token_count .and. tokens(pos)%value == ':') then
                    pos = pos + 1  ! Skip colon
                    if (pos <= token_count .and. tokens(pos)%type == JSON_STRING) then
                        value = tokens(pos)%value
                        found = .true.
                        return
                    end if
                end if
            end if
            pos = pos + 1
        end do
    end subroutine try_parse_key_value
    
    ! Helper subroutine to parse integer key-value pairs
    subroutine try_parse_integer_key(tokens, start_pos, token_count, key_name, value, found)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos, token_count
        character(len=*), intent(in) :: key_name
        integer, intent(out) :: value
        logical, intent(out) :: found
        
        integer :: pos
        
        found = .false.
        pos = start_pos
        
        do while (pos <= token_count)
            if (tokens(pos)%type == JSON_STRING .and. &
                tokens(pos)%value == key_name) then
                pos = pos + 1  ! Skip key
                if (pos <= token_count .and. tokens(pos)%value == ':') then
                    pos = pos + 1  ! Skip colon
                    if (pos <= token_count .and. tokens(pos)%type == JSON_NUMBER) then
                        block
                            integer :: iostat_val
                            read(tokens(pos)%value, *, iostat=iostat_val) value
                            if (iostat_val == 0) then
                                found = .true.
                                return
                            end if
                        end block
                    end if
                end if
            end if
            pos = pos + 1
        end do
    end subroutine try_parse_integer_key
    
    ! Helper subroutine to parse boolean key-value pairs
    subroutine try_parse_boolean_key(tokens, start_pos, token_count, key_name, value, found)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos, token_count
        character(len=*), intent(in) :: key_name
        logical, intent(out) :: value
        logical, intent(out) :: found
        
        integer :: pos
        
        found = .false.
        pos = start_pos
        
        do while (pos <= token_count)
            if (tokens(pos)%type == JSON_STRING .and. &
                tokens(pos)%value == key_name) then
                pos = pos + 1  ! Skip key
                if (pos <= token_count .and. tokens(pos)%value == ':') then
                    pos = pos + 1  ! Skip colon
                    if (pos <= token_count .and. tokens(pos)%type == JSON_BOOLEAN) then
                        value = (tokens(pos)%value == 'true')
                        found = .true.
                        return
                    end if
                end if
            end if
            pos = pos + 1
        end do
    end subroutine try_parse_boolean_key
    
    ! Helper subroutine to parse lines field from any position in object
    subroutine try_parse_lines_field(tokens, start_pos, token_count, filename, lines, &
                                    lines_count, final_pos, found)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos, token_count
        character(len=*), intent(in) :: filename
        type(coverage_line_t), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: lines_count, final_pos
        logical, intent(out) :: found
        
        integer :: pos
        logical :: error_caught
        
        found = .false.
        pos = start_pos
        
        do while (pos <= token_count)
            if (tokens(pos)%type == JSON_STRING .and. &
                tokens(pos)%value == 'lines') then
                pos = pos + 1  ! Skip key
                if (pos <= token_count .and. tokens(pos)%value == ':') then
                    pos = pos + 1  ! Skip colon
                    if (pos <= token_count .and. tokens(pos)%value == '[') then
                        call parse_lines_array(tokens, pos, token_count, filename, &
                                              lines, lines_count, error_caught)
                        if (.not. error_caught) then
                            found = .true.
                            final_pos = pos
                            return
                        end if
                    end if
                end if
            end if
            pos = pos + 1
        end do
        
        lines_count = 0
        final_pos = start_pos
        allocate(lines(0))
    end subroutine try_parse_lines_field

end module json_coverage_io
