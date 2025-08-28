module test_format_auto_detection
    !! Test auto-detection of output format from file extension
    !!
    !! Validates Issue #703 fix: Users can specify --output=coverage.html
    !! without needing --format=html flag - the format is auto-detected
    
    use config_types, only: config_t
    use config_parser_format_detector, only: detect_format_from_extension, &
                                             apply_format_auto_detection
    use config_defaults_core, only: initialize_default_config
    use test_utils_core, only: assert_test
    
    implicit none
    private
    
    public :: test_detect_format_from_extension_html
    public :: test_detect_format_from_extension_json
    public :: test_detect_format_from_extension_xml
    public :: test_detect_format_from_extension_markdown
    public :: test_detect_format_from_extension_text
    public :: test_detect_format_from_extension_unknown
    public :: test_detect_format_from_extension_no_extension
    public :: test_detect_format_case_insensitive
    public :: test_apply_format_auto_detection_basic
    public :: test_apply_format_explicit_override
    public :: test_apply_format_no_output_path
    public :: test_user_scenario_html_output
    public :: test_user_scenario_json_output
    public :: test_user_scenario_mixed_flags
    
contains

    subroutine test_detect_format_from_extension_html()
        !! Test HTML format detection from .html extension
        character(len=:), allocatable :: format
        
        format = detect_format_from_extension("coverage.html")
        call assert_test(format == "html", "Should detect HTML from .html")
        
        format = detect_format_from_extension("/path/to/coverage.html")
        call assert_test(format == "html", "Should detect HTML from path with .html")
        
        format = detect_format_from_extension("report.htm")
        call assert_test(format == "html", "Should detect HTML from .htm")
    end subroutine test_detect_format_from_extension_html
    
    subroutine test_detect_format_from_extension_json()
        !! Test JSON format detection from .json extension
        character(len=:), allocatable :: format
        
        format = detect_format_from_extension("coverage.json")
        call assert_test(format == "json", "Should detect JSON from .json")
        
        format = detect_format_from_extension("/some/path/data.json")
        call assert_test(format == "json", "Should detect JSON from path with .json")
    end subroutine test_detect_format_from_extension_json
    
    subroutine test_detect_format_from_extension_xml()
        !! Test XML format detection from .xml extension
        character(len=:), allocatable :: format
        
        format = detect_format_from_extension("coverage.xml")
        call assert_test(format == "xml", "Should detect XML from .xml")
        
        format = detect_format_from_extension("cobertura.xml")
        call assert_test(format == "xml", "Should detect XML from cobertura.xml")
    end subroutine test_detect_format_from_extension_xml
    
    subroutine test_detect_format_from_extension_markdown()
        !! Test Markdown format detection from .md extension
        character(len=:), allocatable :: format
        
        format = detect_format_from_extension("coverage.md")
        call assert_test(format == "markdown", "Should detect Markdown from .md")
        
        format = detect_format_from_extension("README.markdown")
        call assert_test(format == "markdown", "Should detect Markdown from .markdown")
    end subroutine test_detect_format_from_extension_markdown
    
    subroutine test_detect_format_from_extension_text()
        !! Test text format detection from .txt extension
        character(len=:), allocatable :: format
        
        format = detect_format_from_extension("coverage.txt")
        call assert_test(format == "text", "Should detect text from .txt")
        
        format = detect_format_from_extension("report.text")
        call assert_test(format == "text", "Should detect text from .text")
    end subroutine test_detect_format_from_extension_text
    
    subroutine test_detect_format_from_extension_unknown()
        !! Test unknown extension returns empty string
        character(len=:), allocatable :: format
        
        format = detect_format_from_extension("coverage.pdf")
        call assert_test(format == "", "Should return empty for unknown extension")
        
        format = detect_format_from_extension("file.xyz")
        call assert_test(format == "", "Should return empty for .xyz")
    end subroutine test_detect_format_from_extension_unknown
    
    subroutine test_detect_format_from_extension_no_extension()
        !! Test file without extension returns empty string
        character(len=:), allocatable :: format
        
        format = detect_format_from_extension("coverage")
        call assert_test(format == "", "Should return empty for no extension")
        
        format = detect_format_from_extension("/path/to/file")
        call assert_test(format == "", "Should return empty for path without extension")
    end subroutine test_detect_format_from_extension_no_extension
    
    subroutine test_detect_format_case_insensitive()
        !! Test case-insensitive extension detection
        character(len=:), allocatable :: format
        
        format = detect_format_from_extension("coverage.HTML")
        call assert_test(format == "html", "Should detect HTML from .HTML (uppercase)")
        
        format = detect_format_from_extension("data.JSON")
        call assert_test(format == "json", "Should detect JSON from .JSON (uppercase)")
        
        format = detect_format_from_extension("report.Xml")
        call assert_test(format == "xml", "Should detect XML from .Xml (mixed case)")
    end subroutine test_detect_format_case_insensitive
    
    subroutine test_apply_format_auto_detection_basic()
        !! Test basic auto-detection application to config
        type(config_t) :: config
        
        call initialize_default_config(config)
        
        ! Default format is markdown, not explicitly set
        call assert_test(.not. config%format_explicitly_set, &
                         "Format should not be explicitly set initially")
        call assert_test(config%output_format == "markdown", &
                           "Default format should be markdown")
        
        ! Set output path with HTML extension
        config%output_path = "coverage.html"
        call apply_format_auto_detection(config)
        
        call assert_test(config%output_format == "html", &
                          "Should auto-detect HTML from extension")
        call assert_test(.not. config%format_explicitly_set, &
                         "Auto-detection should not mark as explicitly set")
    end subroutine test_apply_format_auto_detection_basic
    
    subroutine test_apply_format_explicit_override()
        !! Test that explicit --format flag overrides auto-detection
        type(config_t) :: config
        
        call initialize_default_config(config)
        
        ! Simulate user providing --format=json --output=coverage.html
        config%output_format = "json"
        config%format_explicitly_set = .true.  ! Flag was explicitly provided
        config%output_path = "coverage.html"
        
        call apply_format_auto_detection(config)
        
        call assert_test(config%output_format == "json", &
                          "Explicit format should override extension detection")
        call assert_test(config%format_explicitly_set, &
                        "Explicit flag should remain set")
    end subroutine test_apply_format_explicit_override
    
    subroutine test_apply_format_no_output_path()
        !! Test auto-detection does nothing when no output path
        type(config_t) :: config
        
        call initialize_default_config(config)
        config%output_path = ""  ! No output path specified
        
        call apply_format_auto_detection(config)
        
        call assert_test(config%output_format == "markdown", &
                          "Should keep default format when no output path")
    end subroutine test_apply_format_no_output_path
    
    subroutine test_user_scenario_html_output()
        !! Test the user scenario: fortcov --output=coverage.html
        !! Should generate HTML without needing --format=html
        type(config_t) :: config
        
        call initialize_default_config(config)
        
        ! User provides only --output=coverage.html (no --format flag)
        config%output_path = "coverage.html"
        config%format_explicitly_set = .false.
        
        ! Apply auto-detection (as done in command-line parser)
        call apply_format_auto_detection(config)
        
        ! Verify HTML format was auto-detected
        call assert_test(config%output_format == "html", &
                          "Should auto-detect HTML format from .html extension")
        call assert_test(config%output_path == "coverage.html", &
                          "Output path should remain unchanged")
        
        ! This fixes Issue #703: Users expect coverage.html to generate HTML
        ! Without this fix, it would generate markdown in a .html file!
    end subroutine test_user_scenario_html_output
    
    subroutine test_user_scenario_json_output()
        !! Test the user scenario: fortcov --output=/reports/coverage.json
        !! Should generate JSON without needing --format=json
        type(config_t) :: config
        
        call initialize_default_config(config)
        
        ! User provides only --output with full path
        config%output_path = "/reports/coverage.json"
        config%format_explicitly_set = .false.
        
        call apply_format_auto_detection(config)
        
        call assert_test(config%output_format == "json", &
                          "Should auto-detect JSON format from .json extension")
    end subroutine test_user_scenario_json_output
    
    subroutine test_user_scenario_mixed_flags()
        !! Test mixed scenarios with various flag combinations
        type(config_t) :: config
        
        ! Scenario 1: User wants XML in a .dat file (explicit override)
        call initialize_default_config(config)
        config%output_path = "coverage.dat"
        config%output_format = "xml"
        config%format_explicitly_set = .true.
        
        call apply_format_auto_detection(config)
        
        call assert_test(config%output_format == "xml", &
                          "Explicit XML should override .dat extension")
        
        ! Scenario 2: User specifies path without extension
        call initialize_default_config(config)
        config%output_path = "coverage_report"
        config%format_explicitly_set = .false.
        
        call apply_format_auto_detection(config)
        
        call assert_test(config%output_format == "markdown", &
                          "Should keep default markdown for no extension")
        
        ! Scenario 3: User specifies .md extension
        call initialize_default_config(config)
        config%output_path = "docs/coverage.md"
        config%format_explicitly_set = .false.
        
        call apply_format_auto_detection(config)
        
        call assert_test(config%output_format == "markdown", &
                          "Should detect markdown from .md extension")
    end subroutine test_user_scenario_mixed_flags

end module test_format_auto_detection