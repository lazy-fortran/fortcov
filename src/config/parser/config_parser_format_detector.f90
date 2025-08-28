module config_parser_format_detector
    !! Auto-detect output format from file extension
    !!
    !! This module provides automatic format detection based on the output file
    !! extension, allowing users to specify only --output=file.ext without
    !! requiring a separate --format flag.
    
    implicit none
    private
    
    public :: detect_format_from_extension
    public :: apply_format_auto_detection
    
contains

    function detect_format_from_extension(file_path) result(format)
        !! Detect output format from file extension
        !!
        !! Maps common file extensions to their corresponding output formats:
        !!   .html → html
        !!   .json → json
        !!   .xml  → xml
        !!   .md   → markdown
        !!   .txt  → text
        !!
        !! Returns empty string if no recognized extension found
        
        character(len=*), intent(in) :: file_path
        character(len=:), allocatable :: format
        
        integer :: dot_pos, path_len
        character(len=:), allocatable :: extension
        
        format = ""
        
        ! Find last dot position
        path_len = len_trim(file_path)
        dot_pos = index(file_path, '.', back=.true.)
        
        ! No extension found
        if (dot_pos == 0 .or. dot_pos >= path_len) then
            return
        end if
        
        ! Extract extension (lowercase for comparison)
        extension = file_path(dot_pos+1:path_len)
        call to_lowercase(extension)
        
        ! Map extension to format
        select case (trim(extension))
        case ("html", "htm")
            format = "html"
        case ("json")
            format = "json"
        case ("xml")
            format = "xml"
        case ("md", "markdown")
            format = "markdown"
        case ("txt", "text")
            format = "text"
        case default
            ! Unknown extension - leave format empty
            format = ""
        end select
        
    end function detect_format_from_extension
    
    subroutine apply_format_auto_detection(config)
        !! Apply automatic format detection to configuration
        !!
        !! If output_path is specified but output_format was not explicitly set,
        !! automatically detect the format from the file extension.
        !! The --format flag always takes precedence when explicitly specified.
        
        use config_types, only: config_t
        
        type(config_t), intent(inout) :: config
        character(len=:), allocatable :: detected_format
        
        ! Only auto-detect if:
        ! 1. Output path is specified
        ! 2. Format was not explicitly set by user via --format flag
        if (allocated(config%output_path) .and. len_trim(config%output_path) > 0) then
            if (.not. config%format_explicitly_set) then
                detected_format = detect_format_from_extension(config%output_path)
                
                ! Apply detected format if found
                if (len_trim(detected_format) > 0) then
                    config%output_format = detected_format
                    ! Note: We don't set format_explicitly_set here as this is auto-detection
                end if
            end if
        end if
        
    end subroutine apply_format_auto_detection
    
    subroutine to_lowercase(str)
        !! Convert string to lowercase in-place
        character(len=*), intent(inout) :: str
        integer :: i, char_code
        
        do i = 1, len_trim(str)
            char_code = iachar(str(i:i))
            if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                str(i:i) = achar(char_code + 32)
            end if
        end do
    end subroutine to_lowercase

end module config_parser_format_detector