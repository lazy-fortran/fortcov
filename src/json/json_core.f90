module json_core
    !! JSON Core Module - Consolidated tokenization, parsing, and utilities
    !!
    !! Consolidated from multiple over-modularized components:
    !! - json_token_types.f90: Token type definitions
    !! - json_tokenizer_impl.f90: Tokenization logic
    !! - json_parser_object_impl.f90: Object parsing
    !! - json_parser_array_impl.f90: Array utilities
    !! - json_parser_value_impl.f90: Value parsing
    !! - json_parser_utils.f90: Conversion utilities
    !!
    !! This consolidation eliminates architectural debt while maintaining
    !! all functionality within reasonable size limits (target <500 lines).
    use coverage_model_core
    use string_utils, only: int_to_string
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
    public :: parse_coverage_object_from_tokens
    public :: parse_files_array_from_tokens
    public :: parse_file_object_from_tokens
    public :: parse_lines_array_from_tokens
    public :: expect_token_type
    public :: parse_string_value
    public :: parse_number_value
    public :: skip_value
    public :: grow_files_array
    public :: grow_lines_array
    public :: extract_coverage_rates_from_json
    public :: generate_packages_from_json
    public :: generate_classes_from_json_files
    public :: generate_lines_from_json_file
    public :: extract_json_real_value
    public :: extract_json_int_value
    public :: extract_json_string_value
    
contains
    
    ! === TOKENIZATION LOGIC ===
    
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
    
    ! === PARSING LOGIC ===
    
    subroutine parse_coverage_object_from_tokens(tokens, current_pos, token_count, &
        coverage_data, parse_error)
        !! Parses the coverage JSON object from tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: parse_error
        
        character(len=:), allocatable :: key_name
        
        parse_error = .false.
        coverage_data%total_files = 0
        
        if (.not. expect_token_type(tokens, current_pos, token_count, "{")) then
            parse_error = .true.
            return
        end if
        
        do while (current_pos <= token_count)
            if (tokens(current_pos)%value == "}") then
                current_pos = current_pos + 1
                exit
            end if
            
            call parse_key_value_pair(tokens, current_pos, token_count, key_name, parse_error)
            if (parse_error) return
            
            select case (trim(key_name))
            case ("files")
                call parse_files_array_from_tokens(tokens, current_pos, token_count, &
                    coverage_data%files_json, coverage_data%total_files, parse_error)
                if (parse_error) return
            case default
                call skip_value(tokens, current_pos, token_count, parse_error)
                if (parse_error) return
            end select
            
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
    end subroutine parse_coverage_object_from_tokens
    
    subroutine parse_files_array_from_tokens(tokens, current_pos, token_count, &
        files_array, file_count, parse_error)
        !! Parses the files array from tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(file_coverage_t), allocatable, intent(out) :: files_array(:)
        integer, intent(out) :: file_count
        logical, intent(out) :: parse_error
        
        type(file_coverage_t), allocatable :: temp_files(:)
        integer :: capacity
        
        parse_error = .false.
        file_count = 0
        capacity = 10
        allocate(temp_files(capacity))
        
        if (.not. expect_token_type(tokens, current_pos, token_count, "[")) then
            parse_error = .true.
            return
        end if
        
        do while (current_pos <= token_count)
            if (tokens(current_pos)%value == "]") then
                current_pos = current_pos + 1
                exit
            end if
            
            if (file_count >= capacity) then
                call grow_files_array(temp_files, capacity)
            end if
            
            file_count = file_count + 1
            call parse_file_object_from_tokens(tokens, current_pos, token_count, &
                temp_files(file_count), parse_error)
            if (parse_error) return
            
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
        if (file_count > 0) then
            allocate(files_array(file_count))
            files_array = temp_files(1:file_count)
        end if
        
        deallocate(temp_files)
    end subroutine parse_files_array_from_tokens
    
    subroutine parse_file_object_from_tokens(tokens, current_pos, token_count, &
        file_coverage, parse_error)
        !! Parses a file coverage object from tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(file_coverage_t), intent(out) :: file_coverage
        logical, intent(out) :: parse_error
        
        character(len=:), allocatable :: key_name
        integer :: line_count_temp
        
        parse_error = .false.
        
        if (.not. expect_token_type(tokens, current_pos, token_count, "{")) then
            parse_error = .true.
            return
        end if
        
        do while (current_pos <= token_count)
            if (tokens(current_pos)%value == "}") then
                current_pos = current_pos + 1
                exit
            end if
            
            call parse_key_value_pair(tokens, current_pos, token_count, key_name, parse_error)
            if (parse_error) return
            
            select case (trim(key_name))
            case ("filename")
                call parse_string_value(tokens, current_pos, file_coverage%filename, parse_error)
                if (parse_error) return
            case ("lines")
                call parse_lines_array_from_tokens(tokens, current_pos, token_count, &
                    file_coverage%lines, line_count_temp, parse_error)
                if (parse_error) return
            case default
                call skip_value(tokens, current_pos, token_count, parse_error)
                if (parse_error) return
            end select
            
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
    end subroutine parse_file_object_from_tokens
    
    subroutine parse_lines_array_from_tokens(tokens, current_pos, token_count, &
        lines_array, line_count, parse_error)
        !! Parses the lines array from tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(line_coverage_t), allocatable, intent(out) :: lines_array(:)
        integer, intent(out) :: line_count
        logical, intent(out) :: parse_error
        
        type(line_coverage_t), allocatable :: temp_lines(:)
        integer :: capacity
        
        parse_error = .false.
        line_count = 0
        capacity = 100
        allocate(temp_lines(capacity))
        
        if (.not. expect_token_type(tokens, current_pos, token_count, "[")) then
            parse_error = .true.
            return
        end if
        
        do while (current_pos <= token_count)
            if (tokens(current_pos)%value == "]") then
                current_pos = current_pos + 1
                exit
            end if
            
            if (line_count >= capacity) then
                call grow_lines_array(temp_lines, capacity)
            end if
            
            line_count = line_count + 1
            call parse_line_object(tokens, current_pos, token_count, &
                temp_lines(line_count), parse_error)
            if (parse_error) return
            
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
        if (line_count > 0) then
            allocate(lines_array(line_count))
            lines_array = temp_lines(1:line_count)
        end if
        
        deallocate(temp_lines)
    end subroutine parse_lines_array_from_tokens
    
    ! === VALUE PARSING AND UTILITIES ===
    
    subroutine parse_string_value(tokens, current_pos, output_string, parse_error)
        !! Parses a string value from tokens
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
        !! Parses a number value from tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(out) :: output_number
        logical, intent(out) :: parse_error
        character(len=100) :: error_msg
        integer :: ios
        
        parse_error = .false.
        output_number = 0
        
        if (tokens(current_pos)%type /= JSON_NUMBER) then
            parse_error = .true.
            return
        end if
        
        read(tokens(current_pos)%value, *, iostat=ios, iomsg=error_msg) output_number
        if (ios /= 0) then
            parse_error = .true.
            return
        end if
        
        current_pos = current_pos + 1
    end subroutine parse_number_value
    
    subroutine skip_value(tokens, current_pos, token_count, parse_error)
        !! Skips over any JSON value in the token stream
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
    
    function expect_token_type(tokens, current_pos, token_count, expected_value) &
        result(matches)
        !! Checks if the current token matches expected value
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
    
    ! === ARRAY GROWTH UTILITIES ===
    
    subroutine grow_files_array(temp_files, capacity)
        !! Grows the temporary files array when needed
        type(file_coverage_t), allocatable, intent(inout) :: temp_files(:)
        integer, intent(inout) :: capacity
        type(file_coverage_t), allocatable :: new_array(:)
        
        capacity = capacity * 2
        allocate(new_array(capacity))
        
        if (allocated(temp_files)) then
            new_array(1:size(temp_files)) = temp_files
            deallocate(temp_files)
        end if
        
        call move_alloc(new_array, temp_files)
    end subroutine grow_files_array
    
    subroutine grow_lines_array(temp_lines, capacity)
        !! Grows the temporary lines array when needed
        type(line_coverage_t), allocatable, intent(inout) :: temp_lines(:)
        integer, intent(inout) :: capacity
        type(line_coverage_t), allocatable :: new_array(:)
        
        capacity = capacity * 2
        allocate(new_array(capacity))
        
        if (allocated(temp_lines)) then
            new_array(1:size(temp_lines)) = temp_lines
            deallocate(temp_lines)
        end if
        
        call move_alloc(new_array, temp_lines)
    end subroutine grow_lines_array
    
    ! === CONVERSION UTILITIES ===
    
    subroutine extract_coverage_rates_from_json(json_content, line_rate, branch_rate, &
                                               covered_lines, total_lines, success)
        character(len=*), intent(in) :: json_content
        real, intent(out) :: line_rate, branch_rate
        integer, intent(out) :: covered_lines, total_lines
        logical, intent(out) :: success
        
        integer :: summary_start, summary_end
        character(len=:), allocatable :: summary_section
        
        success = .false.
        line_rate = 0.0
        branch_rate = 0.0
        covered_lines = 0
        total_lines = 0
        
        summary_start = index(json_content, '"summary":')
        if (summary_start == 0) return
        
        summary_start = index(json_content(summary_start:), '{')
        if (summary_start == 0) return
        summary_start = summary_start + summary_start - 1
        
        summary_end = index(json_content(summary_start:), '}')
        if (summary_end == 0) return
        summary_end = summary_start + summary_end - 1
        
        summary_section = json_content(summary_start:summary_end)
        
        call extract_json_real_value(summary_section, 'line_coverage', line_rate, success)
        if (.not. success) return
        
        call extract_json_int_value(summary_section, 'covered_lines', covered_lines, success)
        if (.not. success) return
        
        call extract_json_int_value(summary_section, 'total_lines', total_lines, success)
        if (.not. success) return
        
        branch_rate = line_rate
        line_rate = line_rate / 100.0
        branch_rate = branch_rate / 100.0
        
        success = .true.
    end subroutine extract_coverage_rates_from_json
    
    function generate_packages_from_json(json_content) result(packages_xml)
        character(len=*), intent(in) :: json_content
        character(len=:), allocatable :: packages_xml
        
        integer :: files_start, files_end
        character(len=:), allocatable :: files_section
        
        packages_xml = '<packages>' // new_line('') // &
                      '  <package name="fortcov-coverage">' // new_line('') // &
                      '    <classes>' // new_line('')
        
        files_start = index(json_content, '"files":')
        if (files_start > 0) then
            files_start = index(json_content(files_start:), '[')
            if (files_start > 0) then
                files_start = files_start + files_start - 1
                files_end = index(json_content(files_start:), ']')
                if (files_end > 0) then
                    files_end = files_start + files_end - 1
                    files_section = json_content(files_start:files_end)
                    packages_xml = packages_xml // generate_classes_from_json_files(files_section)
                end if
            end if
        end if
        
        packages_xml = packages_xml // &
                      '    </classes>' // new_line('') // &
                      '  </package>' // new_line('') // &
                      '</packages>'
    end function generate_packages_from_json
    
    function generate_classes_from_json_files(files_json) result(classes_xml)
        character(len=*), intent(in) :: files_json
        character(len=:), allocatable :: classes_xml
        
        integer :: pos, file_start, file_end
        character(len=:), allocatable :: filename, lines_xml
        
        classes_xml = ''
        pos = 1
        
        do
            file_start = index(files_json(pos:), '{"filename":')
            if (file_start == 0) exit
            file_start = file_start + pos - 1
            
            file_end = index(files_json(file_start:), '}')
            if (file_end == 0) exit
            file_end = file_start + file_end - 1
            
            call extract_json_string_value(files_json(file_start:file_end), 'filename', filename)
            
            call generate_lines_from_json_file(files_json(file_start:file_end), lines_xml)
            
            classes_xml = classes_xml // &
                         '      <class filename="' // trim(filename) // &
                         '" name="' // get_base_name(filename) // &
                         '" line-rate="1.0" branch-rate="1.0" complexity="0.0">' // &
                         new_line('') // &
                         '        <lines>' // new_line('') // &
                         lines_xml // &
                         '        </lines>' // new_line('') // &
                         '      </class>' // new_line('')
            
            pos = file_end + 1
            if (pos > len(files_json)) exit
        end do
    end function generate_classes_from_json_files
    
    subroutine generate_lines_from_json_file(file_json, lines_xml)
        character(len=*), intent(in) :: file_json
        character(len=:), allocatable, intent(out) :: lines_xml
        
        integer :: lines_start, lines_end, pos, line_start, line_end
        character(len=:), allocatable :: lines_section
        integer :: line_number, execution_count
        logical :: success
        
        lines_xml = ''
        
        lines_start = index(file_json, '"lines":')
        if (lines_start == 0) return
        
        lines_start = index(file_json(lines_start:), '[')
        if (lines_start == 0) return
        lines_start = lines_start + lines_start - 1
        
        lines_end = index(file_json(lines_start:), ']')
        if (lines_end == 0) return
        lines_end = lines_start + lines_end - 1
        
        lines_section = file_json(lines_start:lines_end)
        pos = 1
        
        do
            line_start = index(lines_section(pos:), '{"line_number":')
            if (line_start == 0) exit
            line_start = line_start + pos - 1
            
            line_end = index(lines_section(line_start:), '}')
            if (line_end == 0) exit
            line_end = line_start + line_end - 1
            
            call extract_json_int_value(lines_section(line_start:line_end), 'line_number', &
                                       line_number, success)
            if (.not. success) then
                pos = line_end + 1
                cycle
            end if
            
            call extract_json_int_value(lines_section(line_start:line_end), 'execution_count', &
                                       execution_count, success)
            if (.not. success) then
                pos = line_end + 1
                cycle
            end if
            
            lines_xml = lines_xml // &
                       '          <line number="' // int_to_string(line_number) // &
                       '" hits="' // int_to_string(execution_count) // &
                       '" branch="false"/>' // new_line('')
            
            pos = line_end + 1
            if (pos > len(lines_section)) exit
        end do
    end subroutine generate_lines_from_json_file
    
    subroutine extract_json_real_value(json_text, key, value, success)
        character(len=*), intent(in) :: json_text, key
        real, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: key_start, value_start, value_end, iostat_var
        character(len=20) :: value_str
        
        success = .false.
        value = 0.0
        
        key_start = index(json_text, '"' // trim(key) // '":')
        if (key_start == 0) return
        
        value_start = key_start + len(trim(key)) + 3
        value_end = index(json_text(value_start:), ',')
        if (value_end == 0) then
            value_end = index(json_text(value_start:), '}')
            if (value_end == 0) return
        end if
        value_end = value_start + value_end - 2
        
        if (value_end <= value_start) return
        
        value_str = adjustl(json_text(value_start:value_end))
        read(value_str, *, iostat=iostat_var) value
        
        success = (iostat_var == 0)
    end subroutine extract_json_real_value
    
    subroutine extract_json_int_value(json_text, key, value, success)
        character(len=*), intent(in) :: json_text, key
        integer, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: key_start, value_start, value_end, iostat_var
        character(len=20) :: value_str
        
        success = .false.
        value = 0
        
        key_start = index(json_text, '"' // trim(key) // '":')
        if (key_start == 0) return
        
        value_start = key_start + len(trim(key)) + 3
        value_end = index(json_text(value_start:), ',')
        if (value_end == 0) then
            value_end = index(json_text(value_start:), '}')
            if (value_end == 0) return
        end if
        value_end = value_start + value_end - 2
        
        if (value_end <= value_start) return
        
        value_str = adjustl(json_text(value_start:value_end))
        read(value_str, *, iostat=iostat_var) value
        
        success = (iostat_var == 0)
    end subroutine extract_json_int_value
    
    subroutine extract_json_string_value(json_text, key, value)
        character(len=*), intent(in) :: json_text, key
        character(len=:), allocatable, intent(out) :: value
        
        integer :: key_start, value_start, value_end
        
        value = ''
        
        key_start = index(json_text, '"' // trim(key) // '":')
        if (key_start == 0) return
        
        value_start = index(json_text(key_start:), '"')
        if (value_start == 0) return
        value_start = key_start + value_start
        
        value_end = index(json_text(value_start:), '"')
        if (value_end == 0) return
        value_end = value_start + value_end - 2
        
        if (value_end < value_start) return
        
        value = json_text(value_start:value_end)
    end subroutine extract_json_string_value
    
    ! === HELPER FUNCTIONS ===
    
    subroutine parse_key_value_pair(tokens, current_pos, token_count, key_name, parse_error)
        !! Parses a key-value pair from JSON tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        character(len=:), allocatable, intent(out) :: key_name
        logical, intent(out) :: parse_error
        
        parse_error = .false.
        
        if (current_pos > token_count .or. tokens(current_pos)%type /= JSON_STRING) then
            parse_error = .true.
            return
        end if
        
        key_name = tokens(current_pos)%value
        current_pos = current_pos + 1
        
        if (current_pos > token_count .or. tokens(current_pos)%value /= ":") then
            parse_error = .true.
            return
        end if
        
        current_pos = current_pos + 1
    end subroutine parse_key_value_pair
    
    subroutine parse_line_object(tokens, current_pos, token_count, line_obj, parse_error)
        !! Parses a line coverage object from JSON tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(line_coverage_t), intent(out) :: line_obj
        logical, intent(out) :: parse_error
        
        character(len=:), allocatable :: key_name
        
        parse_error = .false.
        line_obj%line_number = 0
        line_obj%execution_count = 0
        
        if (.not. expect_token_type(tokens, current_pos, token_count, "{")) then
            parse_error = .true.
            return
        end if
        
        do while (current_pos <= token_count)
            if (tokens(current_pos)%value == "}") then
                current_pos = current_pos + 1
                exit
            end if
            
            call parse_key_value_pair(tokens, current_pos, token_count, key_name, parse_error)
            if (parse_error) return
            
            select case (trim(key_name))
            case ("line_number")
                call parse_number_value(tokens, current_pos, line_obj%line_number, parse_error)
                if (parse_error) return
            case ("execution_count")
                call parse_number_value(tokens, current_pos, line_obj%execution_count, parse_error)
                if (parse_error) return
            case default
                call skip_value(tokens, current_pos, token_count, parse_error)
                if (parse_error) return
            end select
            
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
    end subroutine parse_line_object
    
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
    
    function get_base_name(filename) result(base_name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: base_name
        integer :: last_slash, last_dot
        
        last_slash = index(filename, '/', back=.true.)
        last_dot = index(filename, '.', back=.true.)
        
        if (last_slash > 0 .and. last_dot > last_slash) then
            base_name = filename(last_slash+1:last_dot-1)
        else if (last_slash > 0) then
            base_name = filename(last_slash+1:)
        else if (last_dot > 0) then
            base_name = filename(1:last_dot-1)
        else
            base_name = filename
        end if
    end function get_base_name

end module json_core