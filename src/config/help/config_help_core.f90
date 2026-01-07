module config_help_core
    !! Help and version information display
    !! 
    !! This module provides functions to display help text and version information
    !! for the fortcov tool.


    implicit none
    private

    public :: show_help_information
    public :: show_version_information

contains

    subroutine show_help_information()
        !! Display help information
        print '(A)', "FortCov - Fortran Code Coverage Tool"
        print '(A)', ""
        print '(A)', "Usage: fortcov [OPTIONS] [COVERAGE_FILES...]"
        print '(A)', ""
        print '(A)', "Note: FortCov analyzes existing .gcov files by default."
        print '(A)', "      Use --gcov (alias: --discover-and-gcov) to auto-discover builds and run gcov."
        print '(A)', ""
        print '(A)', "Options:"
        print '(A)', "  -h, --help                Show this help and exit"
        print '(A)', "  -V, --version             Show version and exit"
        print '(A)', "  -v, --verbose             Enable verbose output"
        print '(A)', "  -q, --quiet               Suppress non-essential output"
        print '(A)', ""
        print '(A)', "Input:"
        print '(A)', "  -s, --source PATH         Add source directory or file"
        print '(A)', ""
        print '(A)', "Output:"
        print '(A)', "  -o, --output PATH         Output Markdown file (default: coverage.md)"
        print '(A)', "  -f, --format FORMAT       Only 'markdown' is supported"
        print '(A)', ""
        print '(A)', "Coverage:"
        print '(A)', "  --fail-under PERCENT      Exit non-zero if coverage below PERCENT"
        print '(A)', ""
        print '(A)', "Automation:"
        print '(A)', "  --gcov                    Auto-discover builds and run gcov before analysis"
        print '(A)', "                             (alias: --discover-and-gcov)"
        print '(A)', ""
        print '(A)', "Environment:"
        print '(A)', "  FORTCOV_USE_REAL_GCOV=0   Force synthetic gcov output for tests"
        print '(A)', ""
        print '(A)', "Examples:"
        print '(A)', "  fortcov --source=src *.gcov --output=report.md"
        print '(A)', "  fortcov --gcov --fail-under 80"
        print '(A)', ""
        print '(A)', "For more information, visit: https://github.com/lazy-fortran/fortcov"

    end subroutine show_help_information

    subroutine show_version_information()
        !! Display version information

        print *, "fortcov version 0.4.0"
        print *, "Fortran Coverage Analysis Tool"
        print *, ""
        print *, "Built with:"
        print *, "  - Fortran compiler support"
        print *, "  - gcov file analysis (use --gcov to invoke gcov)"
        print *, "  - Markdown output format"
        ! TUI removed
        print *, "  - Parallel processing: not implemented (threads flag ignored)"
        print *, ""
        print *, "Repository: https://github.com/lazy-fortran/fortcov"
        print *, "License: MIT"

    end subroutine show_version_information

end module config_help_core
