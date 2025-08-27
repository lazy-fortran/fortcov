module json_parser_object_impl
    !! JSON object and array parsing module
    !!
    !! This module provides parsing for JSON coverage objects and their
    !! nested structures. It delegates array management and value parsing
    !! to specialized modules for better separation of concerns.
    use json_token_types
    use coverage_model_core
    use json_parser_array_impl, only: grow_files_array, grow_lines_array
    use json_parser_value_impl, only: parse_string_value, parse_number_value, &
                                 skip_value, expect_token_type
    implicit none
    private
    
    ! Public interfaces for object parsing
    public :: parse_coverage_object_from_tokens
    public :: parse_files_array_from_tokens
    public :: parse_file_object_from_tokens
    public :: parse_lines_array_from_tokens
    public :: parse_line_object
    public :: parse_key_value_pair
    
    ! Re-export from specialized modules for API compatibility
    public :: parse_string_value
    public :: parse_number_value
    public :: skip_value
    public :: expect_token_type
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
        
        ! Initialize coverage data
        coverage_data%total_files = 0
        
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
                call parse_files_array_from_tokens(tokens, current_pos, token_count, &
                    coverage_data%files_json, coverage_data%total_files, parse_error)
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
            if (file_count >= capacity) then
                call grow_files_array(temp_files, capacity)
            end if
            
            ! Parse file object
            file_count = file_count + 1
            call parse_file_object_from_tokens(tokens, current_pos, token_count, &
                temp_files(file_count), parse_error)
            if (parse_error) return
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
        ! Allocate final array
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
        
        ! Initialize file coverage (filename will be set during parsing)
        
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
                call parse_string_value(tokens, current_pos, file_coverage%filename, parse_error)
                if (parse_error) return
            case ("lines")
                call parse_lines_array_from_tokens(tokens, current_pos, token_count, &
                    file_coverage%lines, line_count_temp, parse_error)
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
            if (line_count >= capacity) then
                call grow_lines_array(temp_lines, capacity)
            end if
            
            ! Parse line object
            line_count = line_count + 1
            call parse_line_object(tokens, current_pos, token_count, &
                temp_lines(line_count), parse_error)
            if (parse_error) return
            
            ! Skip comma if present
            if (current_pos <= token_count .and. tokens(current_pos)%value == ",") then
                current_pos = current_pos + 1
            end if
        end do
        
        ! Allocate final array
        if (line_count > 0) then
            allocate(lines_array(line_count))
            lines_array = temp_lines(1:line_count)
        end if
        
        deallocate(temp_lines)
        
    end subroutine parse_lines_array_from_tokens
    
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
    
    ! Removed - now in json_value_parser module
    ! parse_string_value is imported and re-exported for API compatibility
    ! Removed - now in json_value_parser module
    ! parse_number_value is imported and re-exported for API compatibility
    ! Removed - now in json_value_parser module
    ! skip_value is imported and re-exported for API compatibility
    
    ! Removed - now in json_array_parser module  
    ! grow_files_array and grow_lines_array are imported and re-exported for API compatibility
    ! Removed - now in json_value_parser module
    ! expect_token_type is imported and re-exported for API compatibility

end module json_parser_object_impl