module json_parser
    !! JSON Parser Module
    !!
    !! Handles parsing of JSON tokens into coverage data structures:
    !! - Coverage object parsing 
    !! - File array and object parsing
    !! - Line array and object parsing
    !! - Value parsing utilities
    use coverage_model_core
    use json_tokenizer, only: json_token_t, JSON_STRING, JSON_NUMBER, JSON_OBJECT, &
                              JSON_ARRAY, JSON_BOOLEAN, JSON_NULL
    implicit none
    private
    
    ! Public interface
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
    
contains
    
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
    
    ! === VALUE PARSING UTILITIES ===
    
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

end module json_parser