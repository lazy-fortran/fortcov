module config_parser_complex
    !! Complex parsing utilities for specialized configuration formats
    !!
    !! Handles parsing of complex argument patterns including diff file pairs
    !! and other specialized configuration string formats.
    
    implicit none
    private
    
    public :: parse_diff_files
    
contains

    subroutine parse_diff_files(diff_arg, before_file, after_file, success, error_message)
        !! Parse diff files argument in format "before,after"
        character(len=*), intent(in) :: diff_arg
        character(len=:), allocatable, intent(out) :: before_file, after_file
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: comma_pos
        
        success = .false.
        error_message = ""
        
        ! Find comma separator
        comma_pos = index(diff_arg, ",")
        if (comma_pos == 0) then
            error_message = "Invalid diff format: '" // trim(diff_arg) // &
                "' (expected: before_file,after_file)"
            return
        end if
        
        ! Extract before and after filenames
        before_file = trim(diff_arg(1:comma_pos-1))
        after_file = trim(diff_arg(comma_pos+1:))
        
        ! Validate both filenames are non-empty
        if (len_trim(before_file) == 0 .or. len_trim(after_file) == 0) then
            error_message = "Invalid diff format: empty filename in '" // trim(diff_arg) // "'"
            return
        end if
        
        success = .true.
    end subroutine parse_diff_files

end module config_parser_complex