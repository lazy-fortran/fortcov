program test_system_diff_core
    use coverage_model
    use json_coverage_io
    use system_diff_converter
    use iso_fortran_env
    implicit none
    
    ! Test counter
    integer :: test_count = 0
    integer :: passed_count = 0
    
    write(*, '(A)') 'Starting Core System Diff Tests'
    write(*, '(A)') '==============================='
    
    ! Test core format conversion functionality
    call test_json_to_xml_conversion()
    call test_xml_schema_validation()
    call test_numerical_tolerance_validation()
    call test_structural_equivalence_validation()
    call test_error_handling_functionality()
    
    ! Print summary
    write(*, '(A)') ''
    write(*, '(A, I0, A, I0, A)') 'Tests completed: ', passed_count, '/', test_count
    
    if (passed_count == test_count) then
        write(*, '(A)') 'All core tests passed!'
        call exit(0)
    else
        write(*, '(A)') 'Some tests failed!'
        call exit(1)
    end if
    
contains

    ! Given: Simple coverage data
    ! When: Converting to XML format
    ! Then: Should produce valid Cobertura XML structure
    subroutine test_json_to_xml_conversion()
        character(len=*), parameter :: test_name = 'JSON to XML Conversion'
        type(coverage_data_t) :: test_data
        character(len=:), allocatable :: json_content, xml_output
        logical :: success
        
        call test_start(test_name)
        
        ! Given: Simple test coverage data
        call create_test_coverage_data(test_data)
        call export_json_coverage(test_data, json_content)
        
        ! When: Converting JSON to XML
        call convert_json_to_cobertura_xml(json_content, xml_output, success)
        
        ! Then: Should produce valid XML with required elements
        if (success .and. &
            index(xml_output, '<?xml version=') > 0 .and. &
            index(xml_output, '<coverage') > 0 .and. &
            index(xml_output, 'line-rate=') > 0 .and. &
            index(xml_output, '<class filename=') > 0 .and. &
            index(xml_output, '<line number=') > 0 .and. &
            index(xml_output, '</coverage>') > 0) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'XML conversion missing required elements')
        end if
        
    end subroutine test_json_to_xml_conversion
    
    ! Given: Generated XML content
    ! When: Validating against Cobertura schema
    ! Then: Should pass basic XML validation
    subroutine test_xml_schema_validation()
        character(len=*), parameter :: test_name = 'XML Schema Validation'
        type(coverage_data_t) :: test_data
        character(len=:), allocatable :: json_content, xml_output
        logical :: success, is_valid
        
        call test_start(test_name)
        
        ! Given: Valid XML content from conversion
        call create_test_coverage_data(test_data)
        call export_json_coverage(test_data, json_content)
        call convert_json_to_cobertura_xml(json_content, xml_output, success)
        
        if (.not. success) then
            call test_fail(test_name, 'Failed to generate XML for validation')
            return
        end if
        
        ! When: Validating XML schema
        call validate_cobertura_xml_schema(xml_output, is_valid)
        
        ! Then: Should be valid XML
        if (is_valid) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Generated XML failed schema validation')
        end if
        
    end subroutine test_xml_schema_validation
    
    ! Given: Two numerical values with different tolerances
    ! When: Checking numerical tolerance
    ! Then: Should correctly identify within/outside tolerance
    subroutine test_numerical_tolerance_validation()
        character(len=*), parameter :: test_name = 'Numerical Tolerance Validation'
        real :: value1, value2
        logical :: within_tolerance
        real, parameter :: strict_tolerance = 0.001  ! 0.1%
        real, parameter :: loose_tolerance = 0.01    ! 1.0%
        
        call test_start(test_name)
        
        ! Given: Values within strict tolerance
        value1 = 0.75000
        value2 = 0.75050  ! 0.067% difference
        
        ! When: Checking with strict tolerance
        call check_numerical_tolerance(value1, value2, strict_tolerance, within_tolerance)
        
        ! Then: Should be within tolerance
        if (.not. within_tolerance) then
            call test_fail(test_name, 'Values within 0.1% not detected as equivalent')
            return
        end if
        
        ! Given: Values outside strict tolerance
        value2 = 0.76000  ! 1.33% difference
        
        ! When: Checking with strict tolerance
        call check_numerical_tolerance(value1, value2, strict_tolerance, within_tolerance)
        
        ! Then: Should be outside tolerance
        if (within_tolerance) then
            call test_fail(test_name, 'Values outside 0.1% incorrectly detected as equivalent')
            return
        end if
        
        ! When: Checking with loose tolerance
        call check_numerical_tolerance(value1, value2, loose_tolerance, within_tolerance)
        
        ! Then: Should be within loose tolerance
        if (within_tolerance) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Values within 1.0% not detected as equivalent')
        end if
        
    end subroutine test_numerical_tolerance_validation
    
    ! Given: Two coverage data structures with same structure
    ! When: Validating structural equivalence
    ! Then: Should detect structural match
    subroutine test_structural_equivalence_validation()
        character(len=*), parameter :: test_name = 'Structural Equivalence Validation'
        type(coverage_data_t) :: data1, data2
        logical :: structures_match
        
        call test_start(test_name)
        
        ! Given: Two data structures with same structure, different counts
        call create_test_coverage_data(data1)
        call create_modified_coverage_data(data2)  ! Same structure, different execution counts
        
        ! When: Validating structural equivalence
        call validate_structural_equivalence(data1, data2, structures_match)
        
        ! Then: Should detect structural match
        if (structures_match) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Identical structures not detected as equivalent')
        end if
        
    end subroutine test_structural_equivalence_validation
    
    ! Given: Various malformed inputs
    ! When: Attempting conversion
    ! Then: Should handle errors gracefully
    subroutine test_error_handling_functionality()
        character(len=*), parameter :: test_name = 'Error Handling Functionality'
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
        
    end subroutine test_error_handling_functionality
    
    ! Helper subroutines
    
    subroutine create_test_coverage_data(coverage_data)
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        
        allocate(files(1))
        allocate(lines(3))
        
        call lines(1)%init(10, 1, 'test.f90', .true.)
        call lines(2)%init(0, 2, 'test.f90', .true.)
        call lines(3)%init(5, 3, 'test.f90', .true.)
        
        call files(1)%init('test.f90', lines)
        call coverage_data%init(files)
        
    end subroutine create_test_coverage_data
    
    subroutine create_modified_coverage_data(coverage_data)
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        
        allocate(files(1))
        allocate(lines(3))
        
        ! Same structure, different execution counts
        call lines(1)%init(15, 1, 'test.f90', .true.)
        call lines(2)%init(2, 2, 'test.f90', .true.)
        call lines(3)%init(8, 3, 'test.f90', .true.)
        
        call files(1)%init('test.f90', lines)
        call coverage_data%init(files)
        
    end subroutine create_modified_coverage_data
    
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

end program test_system_diff_core