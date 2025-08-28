module json_object_parser
    !! JSON object parsing extracted from json_parser
    !! 
    !! Focused on high-level JSON object parsing for coverage data structures.
    !! Handles the main JSON parsing logic while delegating value parsing to
    !! specialized modules.
    use coverage_model_core
    use json_tokenizer, only: json_token_t, JSON_STRING, JSON_NUMBER, JSON_OBJECT, &
                              JSON_ARRAY, JSON_BOOLEAN, JSON_NULL
    use json_value_parser
    use json_array_utils
    implicit none
    private
    
    ! Public interface
    public :: parse_coverage_object_from_tokens
    public :: parse_files_array_from_tokens
    public :: parse_file_object_from_tokens
    public :: parse_lines_array_from_tokens
    
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
    
end module json_object_parser