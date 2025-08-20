module json_io_core
    !! JSON I/O Core Operations (Decomposed from json_coverage_io.f90)
    !! 
    !! Focused on core JSON import/export operations for coverage data.
    !! Separated from parsing and validation for better separation of concerns.
    use foundation_constants
    use foundation_layer_utils
    use coverage_model
    use coverage_operations, only: calculate_coverage_statistics
    use json_parser
    use json_validator
    use input_validation
    use error_handling
    use iso_fortran_env, only: int64
    implicit none
    private
    
    public :: import_coverage_from_json
    public :: export_coverage_to_json
    public :: import_coverage_from_json_safe
    public :: validate_json_coverage_format
    
contains
    
    subroutine import_coverage_from_json(json_content, coverage_data)
        !! Core JSON coverage import implementation
        !! Extracted from original import_json_coverage function
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        
        type(json_token_t), allocatable :: tokens(:)
        integer :: token_count, current_pos
        logical :: parse_error
        
        ! Initialize coverage data
        call initialize_coverage_data(coverage_data)
        
        ! Tokenize JSON content
        call tokenize_json_content(json_content, tokens, token_count, parse_error)
        if (parse_error) then
            print *, "❌ Failed to tokenize JSON content"
            return
        end if
        
        ! Parse coverage data from tokens
        current_pos = 1
        call parse_coverage_data_from_tokens(tokens, current_pos, token_count, &
                                           coverage_data, parse_error)
        
        if (parse_error) then
            print *, "❌ Failed to parse coverage data from JSON"
            return
        end if
        
    end subroutine import_coverage_from_json
    
    subroutine import_coverage_from_json_safe(json_content, coverage_data, error_caught)
        !! Safe JSON coverage import with error handling
        !! Extracted from original import_json_coverage_safe function
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_caught
        
        type(error_context_t) :: error_ctx
        
        ! Initialize error context
        ! Initialize error context (simplified)
        error_ctx%error_code = ERROR_SUCCESS
        error_ctx%message = ""
        error_ctx%suggestion = ""
        
        error_caught = .false.
        
        ! Validate JSON format before processing
        if (.not. validate_json_coverage_format(json_content)) then
            error_ctx%error_code = ERROR_INVALID_DATA
            error_ctx%message = "Invalid JSON format"
            error_caught = .true.
            return
        end if
        
        ! Perform import with error tracking
        call import_coverage_from_json(json_content, coverage_data)
        
        ! Check if import was successful by validating coverage data
        if (.not. is_coverage_data_valid(coverage_data)) then
            error_ctx%error_code = ERROR_INVALID_DATA
            error_ctx%message = "Coverage data validation failed"
            error_caught = .true.
            return
        end if
        
        error_ctx%error_code = ERROR_SUCCESS
        error_ctx%message = "JSON import completed successfully"
        
    end subroutine import_coverage_from_json_safe
    
    subroutine export_coverage_to_json(coverage_data, json_output)
        !! Core JSON coverage export implementation
        !! Extracted from original export_json_coverage function
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable, intent(out) :: json_output
        
        character(len=:), allocatable :: files_json
        character(len=LONG_STRING_LEN) :: summary_json
        integer :: i
        
        ! Start JSON object
        json_output = "{"
        
        ! Add metadata
        json_output = json_output // '"version": "1.0",'
        json_output = json_output // '"timestamp": "' // get_current_timestamp() // '",'
        json_output = json_output // '"tool": "fortcov",'
        
        ! Add summary statistics
        call format_summary_json(coverage_data, summary_json)
        json_output = json_output // '"summary": ' // trim(summary_json) // ','
        
        ! Add files array
        call format_files_json(coverage_data, files_json)
        json_output = json_output // '"files": ' // files_json
        
        ! Close JSON object
        json_output = json_output // "}"
        
    end subroutine export_coverage_to_json
    
    function validate_json_coverage_format(json_content) result(is_valid)
        !! Validates JSON format for coverage data
        character(len=*), intent(in) :: json_content
        logical :: is_valid
        
        ! Delegate to json_validator module
        is_valid = validate_coverage_json_structure(json_content)
        
    end function validate_json_coverage_format
    
    ! Helper functions for JSON processing
    subroutine parse_coverage_data_from_tokens(tokens, current_pos, token_count, &
                                             coverage_data, parse_error)
        !! Parses coverage data from JSON tokens
        type(json_token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: current_pos
        integer, intent(in) :: token_count
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: parse_error
        
        ! Delegate to json_parser module
        call parse_coverage_object_from_tokens(tokens, current_pos, token_count, &
                                             coverage_data, parse_error)
        
    end subroutine parse_coverage_data_from_tokens
    
    subroutine format_summary_json(coverage_data, summary_json)
        !! Formats coverage summary as JSON
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(out) :: summary_json
        
        type(extended_coverage_stats_t) :: stats
        
        ! Calculate statistics
        call calculate_coverage_statistics(coverage_data, stats)
        
        ! Format as JSON object
        write(summary_json, '(A, F6.2, A, I0, A, I0, A, I0, A, I0, A)') &
            '{"line_coverage": ', stats%line_coverage, &
            ', "total_lines": ', stats%total_lines, &
            ', "covered_lines": ', stats%covered_lines, &
            ', "total_files": ', stats%total_files, &
            ', "covered_files": ', stats%covered_files, '}'
        
    end subroutine format_summary_json
    
    subroutine format_files_json(coverage_data, files_json)
        !! Formats files array as JSON
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable, intent(out) :: files_json
        
        character(len=:), allocatable :: file_json
        integer :: i
        
        files_json = "["
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (i > 1) files_json = files_json // ","
                
                call format_coverage_file_json(coverage_data%files(i), file_json)
                files_json = files_json // file_json
            end do
        end if
        
        files_json = files_json // "]"
        
    end subroutine format_files_json
    
    subroutine format_coverage_file_json(file_data, file_json)
        !! Formats single coverage file data as JSON
        type(coverage_file_t), intent(in) :: file_data
        character(len=:), allocatable, intent(out) :: file_json
        
        character(len=:), allocatable :: lines_json
        
        file_json = '{"filename": "' // escape_json_string(file_data%filename) // '"'
        
        ! Add lines array
        call format_coverage_lines_json(file_data, lines_json)
        file_json = file_json // ', "lines": ' // lines_json
        
        file_json = file_json // '}'
        
    end subroutine format_coverage_file_json
    
    subroutine format_coverage_lines_json(file_data, lines_json)
        !! Formats coverage lines array as JSON
        type(coverage_file_t), intent(in) :: file_data
        character(len=:), allocatable, intent(out) :: lines_json
        
        character(len=256) :: line_json
        integer :: i
        
        lines_json = "["
        
        if (allocated(file_data%lines)) then
            do i = 1, size(file_data%lines)
                if (i > 1) lines_json = lines_json // ","
                
                write(line_json, '(A, I0, A, I0, A)') &
                    '{"line_number": ', file_data%lines(i)%line_number, &
                    ', "execution_count": ', file_data%lines(i)%execution_count, '}'
                
                lines_json = lines_json // trim(line_json)
            end do
        end if
        
        lines_json = lines_json // "]"
        
    end subroutine format_coverage_lines_json
    
    function escape_json_string(input_str) result(escaped_str)
        !! Escapes special characters in JSON string
        character(len=*), intent(in) :: input_str
        character(len=:), allocatable :: escaped_str
        
        integer :: i, output_len
        character(len=1) :: c
        
        ! Calculate required output length
        output_len = len(input_str)
        do i = 1, len(input_str)
            c = input_str(i:i)
            if (c == '"' .or. c == '\' .or. c == achar(10) .or. c == achar(13)) then
                output_len = output_len + 1
            end if
        end do
        
        allocate(character(len=output_len) :: escaped_str)
        escaped_str = ""
        
        ! Escape special characters
        do i = 1, len(input_str)
            c = input_str(i:i)
            select case (c)
            case ('"')
                escaped_str = escaped_str // '\"'
            case ('\')
                escaped_str = escaped_str // '\\'
            case (achar(10))  ! newline
                escaped_str = escaped_str // '\n'
            case (achar(13))  ! carriage return
                escaped_str = escaped_str // '\r'
            case default
                escaped_str = escaped_str // c
            end select
        end do
        
    end function escape_json_string
    
    function get_current_timestamp() result(timestamp)
        !! Gets current timestamp in ISO format
        character(len=:), allocatable :: timestamp
        character(len=19) :: date_time
        
        ! Simple timestamp for now
        date_time = "2024-01-01T00:00:00"
        timestamp = date_time
        
    end function get_current_timestamp
    
    subroutine initialize_coverage_data(coverage_data)
        !! Initializes coverage data structure
        type(coverage_data_t), intent(out) :: coverage_data
        
        ! Initialize with default values
        coverage_data%version = "1.0"
        coverage_data%tool = "fortcov"
        
    end subroutine initialize_coverage_data
    
    function is_coverage_data_valid(coverage_data) result(is_valid)
        !! Validates coverage data structure
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: is_valid
        
        is_valid = .true.
        
        ! Basic validation checks
        if (.not. allocated(coverage_data%files)) then
            is_valid = .false.
            return
        end if
        
        if (size(coverage_data%files) == 0) then
            is_valid = .false.
            return
        end if
        
    end function is_coverage_data_valid
    
end module json_io_core