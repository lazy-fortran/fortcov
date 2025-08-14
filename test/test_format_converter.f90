program test_format_converter
    use coverage_model
    use json_coverage_io
    use system_diff_converter
    use iso_fortran_env
    implicit none
    
    ! Simple focused test for format conversion
    type(coverage_data_t) :: test_data
    type(coverage_file_t), allocatable :: files(:)
    type(coverage_line_t), allocatable :: lines(:)
    character(len=:), allocatable :: json_content, xml_output
    logical :: success
    
    write(*, '(A)') 'Testing Format Converter...'
    
    ! Create minimal test data
    allocate(files(1))
    allocate(lines(1))
    
    call lines(1)%init(5, 1, 'test.f90', .true.)
    call files(1)%init('test.f90', lines)
    call test_data%init(files)
    
    ! Export to JSON
    call export_json_coverage(test_data, json_content)
    write(*, '(A)') 'JSON exported successfully'
    write(*, '(A, A)') 'JSON content: ', json_content(1:min(len(json_content), 200))
    
    ! Convert JSON to XML
    call convert_json_to_cobertura_xml(json_content, xml_output, success)
    
    if (success) then
        write(*, '(A)') 'XML conversion SUCCESS'
        write(*, '(A, A)') 'XML content (first 300 chars): ', xml_output(1:min(len(xml_output), 300))
    else
        write(*, '(A)') 'XML conversion FAILED'
        call exit(1)
    end if
    
    write(*, '(A)') 'Test completed successfully'
    
end program test_format_converter