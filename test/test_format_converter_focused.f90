program test_format_converter_focused
    use coverage_model
    use json_coverage_io
    use system_diff_converter
    use iso_fortran_env
    implicit none
    
    ! Test counter
    integer :: test_count = 0
    integer :: passed_count = 0
    
    write(*, '(A)') 'Starting Focused Format Converter Tests'
    write(*, '(A)') '======================================='
    
    ! Test Issue 45.1: Basic JSON to XML conversion
    call test_json_to_xml_basic_conversion()
    
    ! Test Issue 45.2: XML to JSON round-trip verification
    call test_xml_to_json_round_trip()
    
    ! Test Issue 45.3: Numerical tolerance checking
    call test_numerical_tolerance_basic()
    
    ! Test Issue 45.4: Error handling for malformed input
    call test_error_handling_malformed_input()
    
    ! Test Issue 45.5: End-to-end workflow integration
    call test_end_to_end_workflow()
    
    ! Print summary
    write(*, '(A)') ''
    write(*, '(A, I0, A, I0, A)') 'Tests completed: ', passed_count, '/', test_count
    
    if (passed_count == test_count) then
        write(*, '(A)') 'All tests passed!'
        call exit(0)
    else
        write(*, '(A)') 'Some tests failed!'
        call exit(1)
    end if
    
contains

    ! Given a simple coverage data structure
    ! When converting to XML format
    ! Then output should contain required XML elements
    subroutine test_json_to_xml_basic_conversion()
        character(len=*), parameter :: test_name = 'JSON to XML Basic Conversion'
        type(coverage_data_t) :: test_data
        character(len=:), allocatable :: json_content, xml_output
        logical :: success
        
        call test_start(test_name)
        
        ! Given: Simple test coverage data
        call create_minimal_coverage_data(test_data)
        call export_json_coverage(test_data, json_content)
        
        ! When: Converting JSON to XML
        call convert_json_to_cobertura_xml(json_content, xml_output, success)
        
        ! Then: Conversion should succeed and contain required elements
        if (success .and. &
            index(xml_output, '<coverage') > 0 .and. &
            index(xml_output, '</coverage>') > 0 .and. &
            index(xml_output, 'line-rate=') > 0) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Basic XML conversion failed or missing elements')
        end if
        
    end subroutine test_json_to_xml_basic_conversion
    
    ! Given XML output from conversion
    ! When converting back to JSON
    ! Then should preserve coverage data structure
    subroutine test_xml_to_json_round_trip()
        character(len=*), parameter :: test_name = 'XML to JSON Round Trip'
        type(coverage_data_t) :: original_data, recovered_data
        character(len=:), allocatable :: json1, xml_content, json2
        logical :: success, data_preserved
        
        call test_start(test_name)
        
        ! Given: Original coverage data
        call create_minimal_coverage_data(original_data)
        call export_json_coverage(original_data, json1)
        
        ! When: Converting JSON -> XML -> JSON
        call convert_json_to_cobertura_xml(json1, xml_content, success)
        if (.not. success) then
            call test_fail(test_name, 'JSON to XML conversion failed')
            return
        end if
        
        call convert_cobertura_xml_to_json(xml_content, json2, success)
        if (.not. success) then
            call test_fail(test_name, 'XML to JSON conversion failed')
            return
        end if
        
        call import_json_coverage(json2, recovered_data)
        
        ! Then: Data should be preserved
        call compare_coverage_data(original_data, recovered_data, data_preserved)
        
        if (data_preserved) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Round-trip conversion lost data')
        end if
        
    end subroutine test_xml_to_json_round_trip
    
    ! Given two numerical values with small difference
    ! When checking tolerance with ±0.1% default
    ! Then should correctly identify within tolerance
    subroutine test_numerical_tolerance_basic()
        character(len=*), parameter :: test_name = 'Numerical Tolerance Basic'
        real :: value1, value2
        logical :: within_tolerance
        real, parameter :: tolerance = 0.001  ! 0.1% tolerance
        
        call test_start(test_name)
        
        ! Given: Values within 0.1% tolerance
        value1 = 0.75000
        value2 = 0.75050  ! 0.05% difference
        
        ! When: Checking numerical tolerance
        call check_numerical_tolerance(value1, value2, tolerance, within_tolerance)
        
        ! Then: Should detect as within tolerance
        if (within_tolerance) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Values within 0.1% not detected as equivalent')
        end if
        
    end subroutine test_numerical_tolerance_basic
    
    ! Given malformed JSON input
    ! When attempting conversion
    ! Then should handle error gracefully
    subroutine test_error_handling_malformed_input()
        character(len=*), parameter :: test_name = 'Error Handling Malformed Input'
        character(len=:), allocatable :: malformed_json, xml_output
        logical :: success
        
        call test_start(test_name)
        
        ! Given: Malformed JSON input
        malformed_json = '{"invalid": json syntax'
        
        ! When: Attempting conversion
        call convert_json_to_cobertura_xml(malformed_json, xml_output, success)
        
        ! Then: Should fail gracefully
        if (.not. success) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Malformed input should cause conversion failure')
        end if
        
    end subroutine test_error_handling_malformed_input
    
    ! Given sample coverage data
    ! When running complete workflow
    ! Then should process without errors
    subroutine test_end_to_end_workflow()
        character(len=*), parameter :: test_name = 'End to End Workflow'
        type(coverage_data_t) :: test_data
        character(len=:), allocatable :: json_content, xml_output
        logical :: success, xml_valid
        
        call test_start(test_name)
        
        ! Given: Sample coverage data with multiple files
        call create_realistic_coverage_data(test_data)
        
        ! When: Running complete workflow
        call export_json_coverage(test_data, json_content)
        call convert_json_to_cobertura_xml(json_content, xml_output, success)
        
        if (.not. success) then
            call test_fail(test_name, 'JSON to XML conversion failed')
            return
        end if
        
        call validate_cobertura_xml_schema(xml_output, xml_valid)
        
        ! Then: Should complete successfully with valid XML
        if (xml_valid) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Generated XML is not schema valid')
        end if
        
    end subroutine test_end_to_end_workflow
    
    ! Helper subroutines
    
    subroutine create_minimal_coverage_data(coverage_data)
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        
        allocate(files(1))
        allocate(lines(2))
        
        call lines(1)%init(5, 1, 'test.f90', .true.)
        call lines(2)%init(0, 2, 'test.f90', .true.)
        
        call files(1)%init('test.f90', lines)
        call coverage_data%init(files)
        
    end subroutine create_minimal_coverage_data
    
    subroutine create_realistic_coverage_data(coverage_data)
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines1(:), lines2(:)
        
        allocate(files(2))
        allocate(lines1(3))
        allocate(lines2(2))
        
        ! File 1: test.f90
        call lines1(1)%init(10, 1, 'test.f90', .true.)
        call lines1(2)%init(0, 2, 'test.f90', .true.)
        call lines1(3)%init(5, 3, 'test.f90', .true.)
        call files(1)%init('test.f90', lines1)
        
        ! File 2: module.f90
        call lines2(1)%init(2, 1, 'module.f90', .true.)
        call lines2(2)%init(8, 2, 'module.f90', .true.)
        call files(2)%init('module.f90', lines2)
        
        call coverage_data%init(files)
        
    end subroutine create_realistic_coverage_data
    
    ! Test framework helper subroutines
    
    subroutine test_start(name)
        character(len=*), intent(in) :: name
        test_count = test_count + 1
        write(*, '(A, A)') 'Running: ', name
    end subroutine test_start
    
    subroutine test_pass(name)
        character(len=*), intent(in) :: name
        passed_count = passed_count + 1
        write(*, '(A, A, A)') '  PASS: ', name, ''
    end subroutine test_pass
    
    subroutine test_fail(name, message)
        character(len=*), intent(in) :: name, message
        write(*, '(A, A, A, A)') '  FAIL: ', name, ' - ', message
    end subroutine test_fail

end program test_format_converter_focused