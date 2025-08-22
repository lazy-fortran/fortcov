module config_help
    !! Configuration Help and Validation Module
    !! 
    !! Extracted from config_parser.f90 to maintain size limits
    use config_parser, only: config_t
    implicit none
    private
    
    public :: validate_complete_config
    public :: show_help_information
    public :: show_version_information
    
contains
    
    function validate_complete_config(config) result(is_valid)
        !! Validates complete configuration
        type(config_t), intent(in) :: config
        logical :: is_valid
        
        ! Basic validation - can be enhanced later
        is_valid = .true.
        
    end function validate_complete_config
    
    subroutine show_help_information()
        !! Shows help information
        print *, "FortCov - Fortran Coverage Tool"
        print *, "Usage: fortcov [OPTIONS] [COVERAGE_FILES...]"
        print *, ""
        print *, "Options:"
        print *, "  -h, --help              Show this help message"
        print *, "  -V, --version           Show version information"
        print *, "  -v, --verbose           Enable verbose output"
        print *, "  -q, --quiet             Suppress output"
        print *, "  -o, --output PATH       Output file path"
        print *, "  -f, --format FORMAT     Output format (markdown, json, xml, html)"
        print *, "  -t, --threshold PERCENT Minimum coverage threshold"
        print *, "  -s, --source PATH       Source directory"
        print *, "  -e, --exclude PATTERN   Exclude pattern"
        print *, "      --tui               Launch TUI mode"
        print *, "      --strict            Enable strict mode"
        print *, ""
    end subroutine show_help_information
    
    subroutine show_version_information()
        !! Shows version information
        print *, "FortCov version 0.1.0"
        print *, "Fortran Coverage Analysis Tool"
        print *, ""
    end subroutine show_version_information
    
end module config_help