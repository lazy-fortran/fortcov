! Test for Issue #103: JSON and XML output formats contain malformed filename data
!
! This test verifies that the fix for Issue #103 correctly cleans filenames
! in JSON and XML output formats, ensuring they don't contain gcov formatting.

program test_issue_103_filename_cleaning
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing Issue #103 fix: Clean filename data in output formats..."
    
    all_tests_passed = .true.
    
    ! Test 1: Verify JSON output has clean filenames
    all_tests_passed = all_tests_passed .and. test_json_clean_filenames()
    
    ! Test 2: Verify XML output has clean filenames
    all_tests_passed = all_tests_passed .and. test_xml_clean_filenames()
    
    if (all_tests_passed) then
        print *, "All Issue #103 filename cleaning tests PASSED"
        call exit(0)
    else
        print *, "Some Issue #103 filename cleaning tests FAILED"
        call exit(1)
    end if

contains

    function test_json_clean_filenames() result(passed)
        logical :: passed
        character(len=256) :: gcov_file, json_file
        integer :: unit, stat
        character(len=1000) :: line
        
        print *, "  Test 1: JSON output contains clean filenames"
        
        ! Create test gcov file with typical gcov Source: format
        gcov_file = "test_issue_103.gcov"
        open(newunit=unit, file=gcov_file, status='replace')
        write(unit, '(A)') "        -:    0:Source:clean_test.f90"
        write(unit, '(A)') "        1:    1:program clean_test"
        write(unit, '(A)') "        -:    2:end program"
        close(unit)
        
        ! Generate JSON output
        call execute_command_line('fpm run fortcov -- ' // trim(gcov_file) // &
                                ' --output-format=json --output=test_issue_103.json')
        
        ! Check JSON file for clean filename
        json_file = "test_issue_103.json"
        open(newunit=unit, file=json_file, status='old', iostat=stat)
        if (stat /= 0) then
            print *, "    FAILED: Could not open JSON file"
            passed = .false.
            return
        end if
        
        passed = .false.
        read(unit, '(A)', iostat=stat) line
        if (stat == 0) then
            ! Should contain clean filename, not malformed
            if (index(line, '"filename":"clean_test.f90"') > 0) then
                passed = .true.
                print *, "    PASSED: JSON contains clean filename"
            else if (index(line, '0:Source:') > 0) then
                print *, "    FAILED: JSON still contains malformed filename"
                print *, "      Line: ", trim(line)
            else
                print *, "    FAILED: Could not find filename in JSON"
            end if
        else
            print *, "    FAILED: Could not read JSON file"
        end if
        close(unit)
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(gcov_file) // ' ' // trim(json_file))
    end function test_json_clean_filenames

    function test_xml_clean_filenames() result(passed)
        logical :: passed
        character(len=256) :: gcov_file, xml_file
        integer :: unit, stat
        character(len=1000) :: line
        logical :: found_clean_filename
        
        print *, "  Test 2: XML output contains clean filenames"
        
        ! Create test gcov file
        gcov_file = "test_issue_103_xml.gcov"
        open(newunit=unit, file=gcov_file, status='replace')
        write(unit, '(A)') "        -:    0:Source:clean_xml_test.f90"
        write(unit, '(A)') "        1:    1:program clean_xml_test"
        write(unit, '(A)') "        -:    2:end program"
        close(unit)
        
        ! Generate XML output
        call execute_command_line('fmp run fortcov -- ' // trim(gcov_file) // &
                                ' --output-format=xml --output=test_issue_103.xml')
        
        ! Check XML file for clean filename
        xml_file = "test_issue_103.xml"
        open(newunit=unit, file=xml_file, status='old', iostat=stat)
        if (stat /= 0) then
            print *, "    FAILED: Could not open XML file"
            passed = .false.
            return
        end if
        
        passed = .false.
        found_clean_filename = .false.
        do
            read(unit, '(A)', iostat=stat) line
            if (stat /= 0) exit
            
            ! Look for filename attribute
            if (index(line, 'filename=') > 0) then
                if (index(line, 'clean_xml_test.f90') > 0 .and. &
                    index(line, '0:Source:') == 0) then
                    found_clean_filename = .true.
                else if (index(line, '0:Source:') > 0) then
                    print *, "    FAILED: XML contains malformed filename"
                    print *, "      Line: ", trim(line)
                    passed = .false.
                    exit
                end if
            end if
        end do
        close(unit)
        
        if (found_clean_filename) then
            passed = .true.
            print *, "    PASSED: XML contains clean filename"
        else if (.not. passed) then
            print *, "    FAILED: Could not find clean filename in XML"
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(gcov_file) // ' ' // trim(xml_file))
    end function test_xml_clean_filenames

end program test_issue_103_filename_cleaning