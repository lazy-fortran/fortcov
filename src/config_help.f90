module config_help
    !! Help and version information display
    !! 
    !! This module provides functions to display help text and version information
    !! for the fortcov tool.

    use foundation_constants

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
        print '(A)', "  --gcov-executable PATH    Path to gcov executable (default: gcov)"
        print '(A)', "  --gcov-args ARGS          Additional arguments for gcov"
        print '(A)', "  --keep-gcov-files         Keep intermediate .gcov files"
        print '(A)', ""
        print '(A)', "Output Options:"
        print '(A)', "  -o, --output PATH         Output file path"
        print '(A)', "  -f, --format FORMAT       Output format:"
        print '(A)', "                            terminal (default), json, xml, html, lcov, cobertura"
        print '(A)', ""
        print '(A)', "Coverage Options:"
        print '(A)', "  -m, --minimum PERCENT     Set minimum coverage percentage"
        print '(A)', "  --fail-under PERCENT      Fail if coverage is below threshold"
        print '(A)', ""
        print '(A)', "Diff Options:"
        print '(A)', "  --diff BASE,CURRENT       Compare coverage between two JSON files"
        print '(A)', "  --diff-threshold PERCENT  Threshold for diff warnings"
        print '(A)', "  --include-unchanged       Show unchanged files in diff output"
        print '(A)', ""
        print '(A)', "Configuration:"
        print '(A)', "  -c, --config FILE         Load configuration from file"
        print '(A)', "  --validate                Validate configuration and exit"
        print '(A)', ""
        print '(A)', "Auto-Discovery Options:"
        print '(A)', "  --auto-discovery          Enable auto-discovery of sources (default)"
        print '(A)', "  --no-auto-discovery       Disable auto-discovery"
        print '(A)', "  --auto-test               Enable automatic test execution (default)"
        print '(A)', "  --no-auto-test            Disable automatic test execution"
        print '(A)', "  --test-timeout SECONDS    Test execution timeout in seconds (default: 300)"
        print '(A)', ""
        print '(A)', "Advanced Options:"
        print '(A)', "  -t, --threads N           Number of threads for parallel processing"
        print '(A)', "  --tui                     Launch terminal UI mode"
        print '(A)', "  --strict                  Enable strict mode (fail on warnings)"
        print '(A)', ""
        print '(A)', "Zero-Configuration Mode:"
        print '(A)', "  When run without arguments in a project directory, fortcov will:"
        print '(A)', "  - Auto-detect source files and build system"
        print '(A)', "  - Generate coverage reports automatically"
        print '(A)', "  - Create HTML output in coverage-report/"
        print '(A)', ""
        print '(A)', "Examples:"
        print '(A)', "  fortcov                              # Zero-configuration mode"
        print '(A)', "  fortcov *.gcov                       # Process .gcov files"
        print '(A)', "  fortcov -s src/ -o coverage.html    # Analyze source directory"
        print '(A)', "  fortcov --diff base.json,new.json   # Compare coverage"
        print '(A)', ""
        print '(A)', "For more information, visit: https://github.com/fortran-lang/fortcov"

    end subroutine show_help_information

    subroutine show_version_information()
        !! Display version information

        print *, "fortcov version 0.4.0"
        print *, "Fortran Coverage Analysis Tool"
        print *, ""
        print *, "Built with:"
        print *, "  - Fortran compiler support"
        print *, "  - gcov integration"
        print *, "  - JSON/XML/HTML output formats"
        print *, "  - Terminal User Interface (TUI)"
        print *, "  - Parallel processing support"
        print *, ""
        print *, "Repository: https://github.com/fortran-lang/fortcov"
        print *, "License: Apache-2.0"

    end subroutine show_version_information

end module config_help