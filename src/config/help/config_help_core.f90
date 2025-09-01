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
        print '(A)', "Note: FortCov analyzes .gcov files; it does not run gcov."
        print '(A)', "      Generate .gcov with gcov (or scripts/fpm_coverage_bridge.sh) first."
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
        print '(A)', "  --import FILE             Import coverage data from JSON/XML file"
        ! SECURITY FIX Issue #963: --gcov-executable REMOVED - shell injection vulnerability
        print '(A)', "  --gcov-args ARGS          Additional arguments for gcov"
        print '(A)', "  --keep-gcov-files         Keep intermediate .gcov files"
        print '(A)', ""
        print '(A)', "Output Options:"
        print '(A)', "  -o, --output PATH         Output file path"
        print '(A)', "  -f, --format FORMAT       Output format:"
        print '(A)', "                            text, markdown (default), json, html, xml"
        print '(A)', ""
        print '(A)', "Coverage Options:"
        print '(A)', "  -m, --minimum PERCENT     Set minimum coverage percentage"
        print '(A)', "  --fail-under PERCENT      Fail if coverage is below threshold"
        print '(A)', ""
        print '(A)', "Configuration:"
        print '(A)', "  -c, --config FILE         Load configuration from file"
        print '(A)', "  --validate                Validate configuration and exit"
        print '(A)', ""
        print '(A)', "Architectural Quality (Issue #718):"
        print '(A)', "  --validate-architecture   Check file/directory size compliance"
        print '(A)', "  --architecture-format FMT Output format: human, ci, json (default: human)"
        print '(A)', "  --fail-on-size-warnings   Fail CI when size warnings detected"
        print '(A)', ""
        print '(A)', "Auto-Discovery Options:"
        print '(A)', "  --auto-discovery          Enable auto-discovery of sources (default)"
        print '(A)', "  --no-auto-discovery       Disable auto-discovery"
        print '(A)', "  --auto-test               Enable automatic test execution (default)"
        print '(A)', "  --no-auto-test            Disable automatic test execution"
        print '(A)', "  --test-timeout SECONDS    Test execution timeout in seconds (default: 300)"
        print '(A)', ""
        print '(A)', "Advanced Options:"
        print '(A)', "  -t, --threads N           Reserved: parallel processing not implemented;"
        print '(A)', "                            this flag is currently ignored (single-threaded)"
        print '(A)', "  --tui                     Launch terminal UI mode"
        print '(A)', "  --strict                  Enable strict mode (fail on warnings)"
        print '(A)', ""
        print '(A)', "Examples:"
        print '(A)', "  fortcov --source=src *.gcov             # Analyze gcov files with source"
        print '(A)', "  fortcov --source=src *.gcov --output=report.md # Generate markdown report"
        print '(A)', "  fortcov --source=. *.gcov --format=json        # JSON output format"
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
        print *, "  - JSON/XML/HTML output formats"
        print *, "  - Terminal User Interface (TUI)"
        print *, "  - Parallel processing: not implemented (threads flag ignored)"
        print *, ""
        print *, "Repository: https://github.com/lazy-fortran/fortcov"
        print *, "License: MIT"

    end subroutine show_version_information

end module config_help_core
