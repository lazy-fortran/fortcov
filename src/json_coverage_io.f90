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
        
        do i = 1, size(coverage_data%files)
            if (i > 1) result = result // ', '
            
            result = result // '{"filename": "' // &
                    trim(coverage_data%files(i)%filename) // '", "lines": ['
            
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
                        '{"line_number": ', coverage_data%files(i)%lines(j)%line_number, &
                        ', "execution_count": ', coverage_data%files(i)%lines(j)%execution_count, &
                        ', "is_executable": ', bool_str, '}'
                end block
                
                result = result // trim(line_buffer)
            end do
            
            result = result // ']'
            
            ! Add functions if they exist
            if (allocated(coverage_data%files(i)%functions)) then
                if (size(coverage_data%files(i)%functions) > 0) then
                    result = result // ', "functions": ['
                    
                    do j = 1, size(coverage_data%files(i)%functions)
                        if (j > 1) result = result // ', '
                        
                        block
                            character(len=5) :: func_bool_str
                            if (coverage_data%files(i)%functions(j)%is_module_procedure) then
                                func_bool_str = "true"
                            else
                                func_bool_str = "false"
                            end if
                            write(func_buffer, '(A,A,A,A,A,A,A,I0,A,I0,A,A,A)') &
                                '{"name": "', trim(coverage_data%files(i)%functions(j)%name), &
                                '", "parent_module": "', &
                                trim(coverage_data%files(i)%functions(j)%parent_module), &
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
                ! String token
                pos = pos + 1  ! Skip opening quote
                start_pos = pos
                do while (pos <= content_length .and. json_content(pos:pos) /= '"')
                    pos = pos + 1
                end do
                if (pos > content_length) then
                    error_caught = .true.
                    return
                end if
                
                temp_tokens(token_count)%type = JSON_STRING
                temp_tokens(token_count)%value = json_content(start_pos:pos-1)
                temp_tokens(token_count)%start_pos = start_pos
                temp_tokens(token_count)%end_pos = pos - 1
                pos = pos + 1  ! Skip closing quote
                
            case ('0':'9', '-')
                ! Number token
                do while (pos <= content_length .and. &
                         (json_content(pos:pos) >= '0' .and. json_content(pos:pos) <= '9' .or. &
                          json_content(pos:pos) == '.' .or. json_content(pos:pos) == '-'))
                    pos = pos + 1
                end do
                
                temp_tokens(token_count)%type = JSON_NUMBER
                temp_tokens(token_count)%value = json_content(start_pos:pos-1)
                temp_tokens(token_count)%start_pos = start_pos
                temp_tokens(token_count)%end_pos = pos - 1
                
            case ('t', 'f')
                ! Boolean token
                if (pos + 3 <= content_length .and. &
                    json_content(pos:pos+3) == 'true') then
                    temp_tokens(token_count)%type = JSON_BOOLEAN
                    temp_tokens(token_count)%value = 'true'
                    pos = pos + 4
                else if (pos + 4 <= content_length .and. &
                         json_content(pos:pos+4) == 'false') then
                    temp_tokens(token_count)%type = JSON_BOOLEAN
                    temp_tokens(token_count)%value = 'false'
                    pos = pos + 5
                else
                    error_caught = .true.
                    return
                end if
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
                                   error_caught)
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
    subroutine parse_coverage_array(tokens, current_pos, token_count, &
                                   coverage_data, error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_caught
        
        type(coverage_file_t), allocatable :: files(:)
        integer :: files_count
        
        error_caught = .false.
        
        ! Parse array of file objects directly (parse_files_array handles brackets)
        call parse_files_array(tokens, current_pos, token_count, files, &
                              files_count, error_caught)
        if (error_caught) return
        
        ! Initialize coverage data with parsed files
        call coverage_data%init(files)
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
                if (current_pos > token_count .or. tokens(current_pos)%value /= ':') then
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
                                error_caught)
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
        allocate(files(files_count))
        files(1:files_count) = temp_files(1:files_count)
    end subroutine parse_files_array

    ! Parse individual file object from JSON
    subroutine parse_file_object(tokens, current_pos, token_count, file_obj, error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_file_t), intent(out) :: file_obj
        logical, intent(out) :: error_caught
        
        character(len=:), allocatable :: filename
        type(coverage_line_t), allocatable :: lines(:)
        type(coverage_function_t), allocatable :: functions(:)
        integer :: lines_count, functions_count
        
        error_caught = .false.
        
        ! Expect opening brace
        if (current_pos > token_count .or. tokens(current_pos)%value /= '{') then
            error_caught = .true.
            return
        end if
        current_pos = current_pos + 1
        
        ! Parse filename
        call skip_to_key(tokens, current_pos, token_count, 'filename', error_caught)
        if (error_caught) return
        
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_STRING) then
            error_caught = .true.
            return
        end if
        filename = tokens(current_pos)%value
        current_pos = current_pos + 1
        
        ! Parse lines
        call skip_to_key(tokens, current_pos, token_count, 'lines', error_caught)
        if (error_caught) return
        
        call parse_lines_array(tokens, current_pos, token_count, filename, lines, &
                             lines_count, error_caught)
        if (error_caught) return
        
        ! Try to parse functions (optional)
        call try_parse_functions_array(tokens, current_pos, token_count, filename, &
                                     functions, functions_count)
        
        ! Skip to closing brace
        do while (current_pos <= token_count .and. tokens(current_pos)%value /= '}')
            current_pos = current_pos + 1
        end do
        if (current_pos <= token_count) current_pos = current_pos + 1  ! Skip closing brace
        
        ! Initialize file object
        if (lines_count > 0) then
            call file_obj%init(filename, lines(1:lines_count))
        else
            ! Create empty lines array for file
            allocate(lines(0))
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
                                lines_count, error_caught)
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
        allocate(lines(lines_count))
        lines(1:lines_count) = temp_lines(1:lines_count)
    end subroutine parse_lines_array

    ! Parse individual line object from JSON
    subroutine parse_line_object(tokens, current_pos, token_count, filename, line_obj, &
                               error_caught)
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(out) :: line_obj
        logical, intent(out) :: error_caught
        
        integer :: line_number, execution_count
        logical :: is_executable
        
        error_caught = .false.
        
        ! Expect opening brace
        if (current_pos > token_count .or. tokens(current_pos)%value /= '{') then
            error_caught = .true.
            return
        end if
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
        
        ! Parse execution_count
        call skip_to_key(tokens, current_pos, token_count, 'execution_count', error_caught)
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
        
        ! Parse is_executable
        call skip_to_key(tokens, current_pos, token_count, 'is_executable', error_caught)
        if (error_caught) return
        
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_BOOLEAN) then
            error_caught = .true.
            return
        end if
        is_executable = (tokens(current_pos)%value == 'true')
        current_pos = current_pos + 1
        
        ! Skip to closing brace
        do while (current_pos <= token_count .and. tokens(current_pos)%value /= '}')
            current_pos = current_pos + 1
        end do
        if (current_pos <= token_count) current_pos = current_pos + 1  ! Skip closing brace
        
        ! Initialize line object
        call line_obj%init(execution_count, line_number, filename, is_executable)
        error_caught = .false.
    end subroutine parse_line_object

    ! Try to parse functions array (optional field)
    subroutine try_parse_functions_array(tokens, current_pos, token_count, filename, &
                                       functions, functions_count)
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
                                   func_obj, error_caught)
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
        call skip_to_key(tokens, current_pos, token_count, 'parent_module', error_caught)
        if (error_caught) return
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_STRING) then
            error_caught = .true.
            return
        end if
        parent_module = tokens(current_pos)%value
        current_pos = current_pos + 1
        
        ! Parse is_module_procedure
        call skip_to_key(tokens, current_pos, token_count, 'is_module_procedure', error_caught)
        if (error_caught) return
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_BOOLEAN) then
            error_caught = .true.
            return
        end if
        is_module_procedure = (tokens(current_pos)%value == 'true')
        current_pos = current_pos + 1
        
        ! Parse execution_count
        call skip_to_key(tokens, current_pos, token_count, 'execution_count', error_caught)
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
        if (current_pos <= token_count) current_pos = current_pos + 1  ! Skip closing brace
        
        ! Initialize function object
        call func_obj%init(name, parent_module, is_module_procedure, execution_count, &
                          line_number, filename)
        error_caught = .false.
    end subroutine parse_function_object

end module json_coverage_io