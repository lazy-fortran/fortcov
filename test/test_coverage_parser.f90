program test_coverage_parser
    use coverage_parser
    use coverage_model
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Coverage Parser Abstraction..."
    
    ! Test 1: Abstract parser interface compliance (tested by compilation)
    all_tests_passed = all_tests_passed .and. test_interface_compliance()
    
    ! Test 2: Parser can identify supported files
    all_tests_passed = all_tests_passed .and. test_can_parse_supported()
    
    ! Test 3: Parser rejects unsupported files
    all_tests_passed = all_tests_passed .and. test_can_parse_unsupported()
    
    ! Test 4: Parser lists required files
    all_tests_passed = all_tests_passed .and. test_required_files()
    
    ! Test 5: Parser factory selects correct parser
    all_tests_passed = all_tests_passed .and. test_parser_factory_select()
    
    ! Test 6: Parser factory handles unknown format
    all_tests_passed = all_tests_passed .and. test_parser_factory_unknown()
    
    ! Test 7: Mock parser for testing
    all_tests_passed = all_tests_passed .and. test_mock_parser()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_interface_compliance() result(passed)
        logical :: passed
        type(mock_parser_t) :: parser
        
        print *, "  Test 1: Abstract parser interface compliance"
        
        ! Given: A concrete parser implementation (mock_parser)
        ! When: Extending coverage_parser_t
        ! Then: Must implement parse(), can_parse(), and get_required_files()
        
        ! This test passes if the code compiles (interface compliance)
        passed = .true.
        
        print *, "    PASSED - Mock parser implements required interface"
    end function test_interface_compliance

    function test_can_parse_supported() result(passed)
        logical :: passed
        type(gcov_parser_t) :: parser
        
        print *, "  Test 2: Parser can identify supported files"
        
        ! Given: A gcov_parser instance
        ! When: Calling can_parse("data.gcov") (text format only after cleanup)
        ! Then: Should return .true.
        passed = parser%can_parse("data.gcov")
        
        if (.not. passed) then
            print *, "    FAILED: gcov_parser should support .gcov files"
        else
            print *, "    PASSED"
        end if
    end function test_can_parse_supported

    function test_can_parse_unsupported() result(passed)
        logical :: passed
        type(gcov_parser_t) :: parser
        
        print *, "  Test 3: Parser rejects unsupported files"
        
        ! Given: A gcov_parser instance  
        ! When: Calling can_parse("data.xml")
        ! Then: Should return .false.
        passed = .not. parser%can_parse("data.xml")
        
        if (.not. passed) then
            print *, "    FAILED: gcov_parser should not support .xml files"
        else
            print *, "    PASSED"
        end if
    end function test_can_parse_unsupported

    function test_required_files() result(passed)
        logical :: passed
        type(gcov_parser_t) :: parser
        character(len=:), allocatable :: extensions(:)
        
        print *, "  Test 4: Parser lists required files"
        
        ! Given: A gcov_parser instance
        ! When: Calling get_required_files()
        ! Then: Should return [".gcov"] (text format only after cleanup)
        extensions = parser%get_required_files()
        
        passed = (size(extensions) == 1) .and. &
                 (trim(extensions(1)) == ".gcov")
        
        if (.not. passed) then
            print *, "    FAILED: Expected ['.gcov']"
            if (allocated(extensions)) then
                print *, "    Got", size(extensions), "extensions"
                if (size(extensions) > 0) then
                    print *, "    First extension:", trim(extensions(1))
                end if
            end if
        else
            print *, "    PASSED"
        end if
    end function test_required_files

    function test_parser_factory_select() result(passed)
        logical :: passed
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        
        print *, "  Test 5: Parser factory selects correct parser"
        
        ! Given: A file path "coverage.gcov" (text format only after cleanup)
        ! When: Calling create_parser(path)
        ! Then: Should return gcov_parser instance
        call create_parser("coverage.gcov", parser, error_flag)
        
        passed = (.not. error_flag) .and. allocated(parser)
        if (passed) then
            ! Check that it's the right type by testing a known behavior
            passed = parser%can_parse("test.gcov")
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Factory should create gcov_parser for .gcov"
            print *, "    Error flag:", error_flag
        else
            print *, "    PASSED"
        end if
    end function test_parser_factory_select

    function test_parser_factory_unknown() result(passed)
        logical :: passed
        class(coverage_parser_t), allocatable :: parser
        logical :: error_flag
        
        print *, "  Test 6: Parser factory handles unknown format"
        
        ! Given: A file path "coverage.unknown"
        ! When: Calling create_parser(path)
        ! Then: Should return null pointer with error
        call create_parser("coverage.unknown", parser, error_flag)
        
        passed = error_flag .and. (.not. allocated(parser))
        
        if (.not. passed) then
            print *, "    FAILED: Factory should fail for unknown format"
            print *, "    Error flag:", error_flag
            print *, "    Parser allocated:", allocated(parser)
        else
            print *, "    PASSED"
        end if
    end function test_parser_factory_unknown

    function test_mock_parser() result(passed)
        logical :: passed
        type(mock_parser_t) :: parser
        type(coverage_data_t) :: expected_data, parsed_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        logical :: error_flag
        
        print *, "  Test 7: Mock parser for testing"
        
        ! Given: A test_parser with injected coverage data
        allocate(lines(1))
        lines(1) = coverage_line_t(execution_count=10, line_number=5, &
                                   filename="mock.f90", is_executable=.true.)
        
        allocate(files(1))  
        files(1) = coverage_file_t(filename="mock.f90", lines=lines)
        
        expected_data = coverage_data_t(files=files)
        call parser%inject_data(expected_data)
        
        ! When: Calling parse()
        parsed_data = parser%parse("mock_file.gcda", error_flag)
        
        ! Then: Should return the injected coverage_data_t
        passed = (.not. error_flag) .and. (size(parsed_data%files) == 1)
        if (passed .and. size(parsed_data%files(1)%lines) > 0) then
            passed = (parsed_data%files(1)%lines(1)%execution_count == 10)
        end if
        
        if (.not. passed) then
            print *, "    FAILED: Mock parser should return injected data"
            print *, "    Error flag:", error_flag
            if (allocated(parsed_data%files)) then
                print *, "    Files count:", size(parsed_data%files)
            end if
        else
            print *, "    PASSED"
        end if
    end function test_mock_parser

end program test_coverage_parser