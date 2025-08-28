program test_format_detection_basic
    !! Basic test for format auto-detection
    
    use config_parser_format_detector, only: detect_format_from_extension
    use test_utils_core, only: assert_test, reset_test_counters, &
                               print_test_summary, all_tests_passed
    
    implicit none
    
    character(len=:), allocatable :: format
    
    call reset_test_counters()
    
    ! Test HTML detection
    format = detect_format_from_extension("coverage.html")
    call assert_test(format == "html", "HTML detection", "Should detect HTML from .html extension")
    
    format = detect_format_from_extension("report.htm")
    call assert_test(format == "html", "HTM detection", "Should detect HTML from .htm extension")
    
    ! Test JSON detection
    format = detect_format_from_extension("data.json")
    call assert_test(format == "json", "JSON detection", "Should detect JSON from .json extension")
    
    ! Test XML detection
    format = detect_format_from_extension("coverage.xml")
    call assert_test(format == "xml", "XML detection", "Should detect XML from .xml extension")
    
    ! Test Markdown detection
    format = detect_format_from_extension("README.md")
    call assert_test(format == "markdown", "Markdown detection", "Should detect Markdown from .md extension")
    
    ! Test text detection
    format = detect_format_from_extension("report.txt")
    call assert_test(format == "text", "Text detection", "Should detect text from .txt extension")
    
    ! Test unknown extension
    format = detect_format_from_extension("file.xyz")
    call assert_test(format == "", "Unknown extension", "Should return empty for unknown .xyz extension")
    
    ! Test no extension
    format = detect_format_from_extension("coverage")
    call assert_test(format == "", "No extension", "Should return empty when no extension present")
    
    ! Test case insensitive
    format = detect_format_from_extension("COVERAGE.HTML")
    call assert_test(format == "html", "Case insensitive HTML", "Should detect HTML from uppercase .HTML")
    
    format = detect_format_from_extension("Data.JSON")
    call assert_test(format == "json", "Case insensitive JSON", "Should detect JSON from mixed case .JSON")
    
    ! Print summary
    call print_test_summary("format_detection_basic")
    
    ! Exit with appropriate code
    if (.not. all_tests_passed) then
        stop 1
    end if
    
end program test_format_detection_basic