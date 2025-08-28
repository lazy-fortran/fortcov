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
    call assert_test(format == "html", "Should detect HTML from .html")
    
    format = detect_format_from_extension("report.htm")
    call assert_test(format == "html", "Should detect HTML from .htm")
    
    ! Test JSON detection
    format = detect_format_from_extension("data.json")
    call assert_test(format == "json", "Should detect JSON from .json")
    
    ! Test XML detection
    format = detect_format_from_extension("coverage.xml")
    call assert_test(format == "xml", "Should detect XML from .xml")
    
    ! Test Markdown detection
    format = detect_format_from_extension("README.md")
    call assert_test(format == "markdown", "Should detect Markdown from .md")
    
    ! Test text detection
    format = detect_format_from_extension("report.txt")
    call assert_test(format == "text", "Should detect text from .txt")
    
    ! Test unknown extension
    format = detect_format_from_extension("file.xyz")
    call assert_test(format == "", "Should return empty for unknown extension")
    
    ! Test no extension
    format = detect_format_from_extension("coverage")
    call assert_test(format == "", "Should return empty for no extension")
    
    ! Test case insensitive
    format = detect_format_from_extension("COVERAGE.HTML")
    call assert_test(format == "html", "Should detect HTML case-insensitive")
    
    format = detect_format_from_extension("Data.JSON")
    call assert_test(format == "json", "Should detect JSON case-insensitive")
    
    ! Print summary
    call print_test_summary()
    
    ! Exit with appropriate code
    if (.not. all_tests_passed) then
        stop 1
    end if
    
end program test_format_detection_basic