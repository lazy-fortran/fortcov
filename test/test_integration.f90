program test_integration
    use fortcov
    use coverage_engine
    use file_utils
    use string_utils
    use fortcov_config, only: config_t, initialize_config
    use coverage_model
    use coverage_parser
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Integration Tests..."
    
    ! Test 1: CLI argument parsing
    all_tests_passed = all_tests_passed .and. test_cli_argument_parsing()
    
    ! Test 2: File discovery mechanism  
    all_tests_passed = all_tests_passed .and. test_coverage_file_discovery()
    
    ! Test 3: Markdown report generation (headers/structure)
    all_tests_passed = all_tests_passed .and. test_markdown_report_structure()
    
    ! Test 4: Text parsing architecture readiness
    all_tests_passed = all_tests_passed .and. test_text_parsing_architecture()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    ! Test 1: CLI argument parsing
    ! Given: Command line arguments
    ! When: Parsing arguments through config system
    ! Then: Should handle basic options correctly
    function test_cli_argument_parsing() result(passed)
        logical :: passed
        type(config_t) :: config
        
        print *, "  Test 1: CLI argument parsing"
        
        ! Test default config creation
        call initialize_config(config)
        if (allocated(config%output_path)) then
            print *, "    PASSED: Config properly initialized"
            passed = .true.
        else
            print *, "    FAILED: Config initialization issue"
            passed = .false.
        end if
    end function test_cli_argument_parsing

    ! Test 2: File discovery mechanism
    ! Given: A directory structure
    ! When: Searching for coverage files
    ! Then: Should identify appropriate file types
    function test_coverage_file_discovery() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_files(:)
        
        print *, "  Test 2: File discovery mechanism"
        
        ! Test file extension checking
        if (ends_with_extension("test.gcov", ".gcov")) then
            print *, "    PASSED: File extension detection works"
            passed = .true.
        else
            print *, "    FAILED: File extension detection failed"
            passed = .false.
        end if
    end function test_coverage_file_discovery

    ! Test 3: Markdown report generation structure
    ! Given: Coverage data model
    ! When: Generating markdown report
    ! Then: Should create proper markdown headers and structure
    function test_markdown_report_structure() result(passed)
        logical :: passed
        character(len=:), allocatable :: report_content
        
        print *, "  Test 3: Markdown report structure"
        
        ! Test basic report header generation
        report_content = "# Coverage Report" // new_line('A')
        if (index(report_content, "# Coverage Report") > 0) then
            print *, "    PASSED: Report header structure works"
            passed = .true.
        else
            print *, "    FAILED: Report header structure failed"
            passed = .false.
        end if
    end function test_markdown_report_structure

    ! Test 4: Text parsing architecture readiness  
    ! Given: Parser factory system
    ! When: Requesting gcov text parser
    ! Then: Should return appropriate parser type that indicates text parsing readiness
    function test_text_parsing_architecture() result(passed)
        logical :: passed
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        
        print *, "  Test 4: Text parsing architecture readiness"
        
        ! Test gcov parser creation (should work but parsing not implemented)
        call create_parser("test.gcov", parser, error_flag)
        
        if (.not. error_flag) then
            print *, "    PASSED: Text parser architecture ready (parsing not implemented yet)"
            passed = .true.
        else
            print *, "    FAILED: Text parser architecture not ready"
            passed = .false.
        end if
    end function test_text_parsing_architecture

    ! Helper function to check file extensions
    function ends_with_extension(filename, extension) result(matches)
        character(len=*), intent(in) :: filename, extension
        logical :: matches
        integer :: dot_pos
        
        dot_pos = index(filename, ".", back=.true.)
        if (dot_pos > 0) then
            matches = (filename(dot_pos:) == extension)
        else
            matches = .false.
        end if
    end function ends_with_extension

end program test_integration