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
        print '(A)', "Note: By default, FortCov analyzes existing .gcov files and"
        print '(A)', "      does not invoke gcov. Use --gcov to auto-discover builds"
        print '(A)', "      and generate .gcov from coverage artifacts when needed."
        print '(A)', ""
        print '(A)', "Options:"
        print '(A)', "  -h, --help                Show this help message and exit"
        print '(A)', "  -V, --version             Show version information and exit"
        print '(A)', "  -v, --verbose             Enable verbose output"
        print '(A)', "  -q, --quiet               Suppress non-essential output"
        print '(A)', ""
        print '(A)', "Input Options:"
        print '(A)', "  -s, --source PATH         Add source directory or file to analyze"
        print '(A)', "  -e, --exclude PATTERN     Exclude files matching pattern"
        print '(A)', "  -i, --include PATTERN     Include only files matching pattern"
        ! Import option for JSON/XML removed; only .gcov is analyzed directly
        ! SECURITY: No user-defined gcov executable flag; FortCov uses a
        ! built-in safe path when --gcov is specified. External overrides removed.
        print '(A)', ""
        print '(A)', "Output Options:"
        print '(A)', "  -o, --output PATH         Output file path"
        print '(A)', "  -f, --format FORMAT       Output format: markdown (default)"
        print '(A)', ""
        print '(A)', "Coverage Options:"
        print '(A)', "  -m, --minimum PERCENT     Set minimum coverage percentage"
        print '(A)', "  --fail-under PERCENT      Fail if coverage is below threshold"
        print '(A)', ""
        print '(A)', "Configuration:"
        print '(A)', "  -c, --config FILE         Load configuration from file"
        print '(A)', "  --validate                Validate configuration and exit"
        print '(A)', ""
        
        print '(A)', "Auto-Discovery Options:"
        print '(A)', "  --auto-discovery          Enable auto-discovery of sources (default)"
        print '(A)', "  --no-auto-discovery       Disable auto-discovery"
        print '(A)', "  --gcov                    Discover build dirs and generate .gcov"
        print '(A)', "  --discover-and-gcov       Alias for --gcov"
        print '(A)', "  --auto-test               Enable automatic test execution (default)"
        print '(A)', "  --no-auto-test            Disable automatic test execution"
        print '(A)', "  --test-timeout SECONDS    Test execution timeout in seconds (default: 300)"
        print '(A)', ""
        print '(A)', "Advanced Options:"
        print '(A)', "  -t, --threads N           Reserved: parallel processing not implemented;"
        print '(A)', "                            this flag is currently ignored (single-threaded)"
        ! TUI removed
        print '(A)', "  --strict                  Enable strict mode (fail on warnings)"
        print '(A)', ""
        print '(A)', "Examples:"
        print '(A)', "  fortcov --source=src *.gcov             # Analyze gcov files with source"
        print '(A)', "  fortcov --source=src *.gcov --output=report.md # Generate markdown report"
        ! Non-markdown formats removed
        print '(A)', "  fortcov --gcov --output=coverage.md     # Auto-generate .gcov then analyze"
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
        print *, "  - gcov file analysis (does not invoke gcov)"
        print *, "  - Markdown output format"
        ! TUI removed
        print *, "  - Parallel processing: not implemented (threads flag ignored)"
        print *, ""
        print *, "Repository: https://github.com/lazy-fortran/fortcov"
        print *, "License: MIT"

    end subroutine show_version_information

end module config_help_core
