module test_cli_format_auto_detection_issue_703
    !! Integration tests for Issue #703: Auto-detect format from output extension
    !!
    !! Tests the complete command-line parsing flow to ensure that users can
    !! specify --output=coverage.html and get HTML output without needing
    !! to also specify --format=html
    
    use config_core, only: config_t, parse_config
    use test_utils_core, only: assert_test
    
    implicit none
    private
    
    public :: test_cli_html_auto_detection
    public :: test_cli_json_auto_detection
    public :: test_cli_xml_auto_detection
    public :: test_cli_markdown_auto_detection
    public :: test_cli_explicit_format_override
    public :: test_cli_no_extension_keeps_default
    public :: test_cli_unknown_extension_keeps_default
    public :: test_cli_path_with_directories
    public :: test_cli_format_flag_precedence
    public :: test_cli_combined_with_other_flags
    
contains

    subroutine test_cli_html_auto_detection()
        !! Test: fortcov --output=coverage.html
        !! Expected: HTML format auto-detected from .html extension
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(1))
        args(1) = "--output=coverage.html"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("coverage.html", config%output_path, &
                          "Output path should be coverage.html")
        call assert_equals("html", config%output_format, &
                          "Format should be auto-detected as HTML")
        
        deallocate(args)
    end subroutine test_cli_html_auto_detection
    
    subroutine test_cli_json_auto_detection()
        !! Test: fortcov --output=data.json --source=src
        !! Expected: JSON format auto-detected from .json extension
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(2))
        args(1) = "--output=data.json"
        args(2) = "--source=src"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("data.json", config%output_path, &
                          "Output path should be data.json")
        call assert_equals("json", config%output_format, &
                          "Format should be auto-detected as JSON")
        
        deallocate(args)
    end subroutine test_cli_json_auto_detection
    
    subroutine test_cli_xml_auto_detection()
        !! Test: fortcov -o cobertura.xml
        !! Expected: XML format auto-detected from .xml extension
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(2))
        args(1) = "-o"
        args(2) = "cobertura.xml"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("cobertura.xml", config%output_path, &
                          "Output path should be cobertura.xml")
        call assert_equals("xml", config%output_format, &
                          "Format should be auto-detected as XML")
        
        deallocate(args)
    end subroutine test_cli_xml_auto_detection
    
    subroutine test_cli_markdown_auto_detection()
        !! Test: fortcov --output=README.md
        !! Expected: Markdown format auto-detected from .md extension
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(1))
        args(1) = "--output=README.md"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("README.md", config%output_path, &
                          "Output path should be README.md")
        call assert_equals("markdown", config%output_format, &
                          "Format should be auto-detected as Markdown")
        
        deallocate(args)
    end subroutine test_cli_markdown_auto_detection
    
    subroutine test_cli_explicit_format_override()
        !! Test: fortcov --format=json --output=report.html
        !! Expected: Explicit format=json overrides .html extension
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(2))
        args(1) = "--format=json"
        args(2) = "--output=report.html"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("report.html", config%output_path, &
                          "Output path should be report.html")
        call assert_equals("json", config%output_format, &
                          "Format should remain JSON (explicit override)")
        call assert_true(config%format_explicitly_set, &
                        "Format should be marked as explicitly set")
        
        deallocate(args)
    end subroutine test_cli_explicit_format_override
    
    subroutine test_cli_no_extension_keeps_default()
        !! Test: fortcov --output=coverage
        !! Expected: Default markdown format when no extension
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(1))
        args(1) = "--output=coverage"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("coverage", config%output_path, &
                          "Output path should be coverage")
        call assert_equals("markdown", config%output_format, &
                          "Format should remain default markdown")
        
        deallocate(args)
    end subroutine test_cli_no_extension_keeps_default
    
    subroutine test_cli_unknown_extension_keeps_default()
        !! Test: fortcov --output=report.xyz
        !! Expected: Default markdown format for unknown extension
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(1))
        args(1) = "--output=report.xyz"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("report.xyz", config%output_path, &
                          "Output path should be report.xyz")
        call assert_equals("markdown", config%output_format, &
                          "Format should remain default markdown for unknown extension")
        
        deallocate(args)
    end subroutine test_cli_unknown_extension_keeps_default
    
    subroutine test_cli_path_with_directories()
        !! Test: fortcov --output=/var/reports/coverage.html
        !! Expected: HTML format detected from path with directories
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(1))
        args(1) = "--output=/var/reports/coverage.html"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("/var/reports/coverage.html", config%output_path, &
                          "Full path should be preserved")
        call assert_equals("html", config%output_format, &
                          "Format should be auto-detected as HTML from full path")
        
        deallocate(args)
    end subroutine test_cli_path_with_directories
    
    subroutine test_cli_format_flag_precedence()
        !! Test various orderings of --format and --output flags
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        ! Test 1: --output before --format
        allocate(args(2))
        args(1) = "--output=report.html"
        args(2) = "--format=xml"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("xml", config%output_format, &
                          "Explicit format should override auto-detection")
        
        deallocate(args)
        
        ! Test 2: --format before --output
        allocate(args(2))
        args(1) = "--format=text"
        args(2) = "--output=coverage.json"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed")
        call assert_equals("text", config%output_format, &
                          "Explicit format should override extension regardless of order")
        
        deallocate(args)
    end subroutine test_cli_format_flag_precedence
    
    subroutine test_cli_combined_with_other_flags()
        !! Test auto-detection combined with other common flags
        type(config_t) :: config
        character(len=256), allocatable :: args(:)
        character(len=256) :: error_msg
        logical :: success
        
        allocate(args(5))
        args(1) = "--source=src"
        args(2) = "--exclude=test"
        args(3) = "--output=build/coverage.html"
        args(4) = "--verbose"
        args(5) = "--minimum=80.0"
        
        call parse_config(args, config, success, error_msg)
        
        call assert_true(success, "Parsing should succeed with multiple flags")
        call assert_equals("build/coverage.html", config%output_path, &
                          "Output path should be preserved")
        call assert_equals("html", config%output_format, &
                          "HTML format should be auto-detected")
        call assert_true(config%verbose, "Verbose flag should be set")
        call assert_equals(80.0, config%minimum_coverage, 0.01, &
                          "Minimum coverage should be set")
        
        ! Verify source paths were parsed
        call assert_true(allocated(config%source_paths), &
                        "Source paths should be allocated")
        if (allocated(config%source_paths)) then
            call assert_equals(1, size(config%source_paths), &
                              "Should have one source path")
            if (size(config%source_paths) > 0) then
                call assert_equals("src", config%source_paths(1), &
                                  "Source path should be src")
            end if
        end if
        
        deallocate(args)
    end subroutine test_cli_combined_with_other_flags

end module test_cli_format_auto_detection_issue_703