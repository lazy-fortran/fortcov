module json_value_parser
    !! JSON value parsing module
    !!
    !! Provides specialized parsing for JSON values including
    !! strings, numbers, and value skipping functionality.
    use json_token_types
    implicit none
    private
    
    public :: parse_string_value
    public :: parse_number_value
    public :: skip_value
    public :: expect_token_type
    
contains
    
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
        
        read(tokens(current_pos)%value, *, iostat=ios, iomsg=error_msg) &
            output_number
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
    
end module json_value_parser