program test_cobertura_output_format
    !! Cobertura XML output format wiring and basic content test.
    !! Regression test for issue 1292.

    use, intrinsic :: iso_fortran_env, only: output_unit
    use config_defaults_core, only: initialize_default_config
    use config_parser, only: process_single_flag
    use config_types, only: config_t
    use config_validators_format, only: is_supported_output_format
    use coverage_reporter, only: coverage_reporter_t
    use coverage_reporter_factory, only: create_reporter
    use coverage_types, only: coverage_data_t, coverage_file_t, coverage_line_t
    use file_utilities, only: read_file_content
    use coverage_constructors_core, only: initialize_coverage_line
    implicit none

    integer :: tests = 0
    integer :: passed = 0

    call test_supported_output_format()
    call test_flag_parsing_sets_cobertura()
    call test_cobertura_reporter_writes_xml()

    write (output_unit, '(A,I0,A,I0,A)') 'Test Results: ', passed, ' / ', &
        tests, ' passed'
    if (passed /= tests) stop 1

contains

    subroutine test_supported_output_format()
        tests = tests + 1

        if (is_supported_output_format("markdown") .and. &
            is_supported_output_format("cobertura") .and. &
            is_supported_output_format("cobertura-xml") .and. &
            is_supported_output_format("xml")) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] Supported formats include cobertura'
        else
            write (output_unit, '(A)') '  [FAIL] Supported formats missing cobertura'
        end if
    end subroutine test_supported_output_format

    subroutine test_flag_parsing_sets_cobertura()
        type(config_t) :: config
        logical :: success
        character(len=256) :: error_message

        tests = tests + 1

        call initialize_default_config(config)
        call process_single_flag("--format=cobertura", config, success, error_message)

        if (success .and. trim(config%output_format) == "cobertura") then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] --format=cobertura applies'
        else
            write (output_unit, '(A)') '  [FAIL] --format=cobertura did not apply'
            write (output_unit, '(A,L1)') '    Success: ', success
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
            write (output_unit, '(A,A)') '    Got: ', trim(config%output_format)
        end if
    end subroutine test_flag_parsing_sets_cobertura

    subroutine test_cobertura_reporter_writes_xml()
        type(coverage_line_t), allocatable :: lines(:)
        type(coverage_file_t) :: file
        type(coverage_data_t) :: data
        type(coverage_file_t), allocatable :: files(:)
        class(coverage_reporter_t), allocatable :: reporter
        logical :: success, factory_error, read_error
        character(len=:), allocatable :: error_message
        character(len=:), allocatable :: xml_content
        character(len=*), parameter :: out_path = "/tmp/fortcov-test-cobertura.xml"

        tests = tests + 1

        allocate (lines(2))
        call initialize_coverage_line(lines(1), "src/a&b<.f90", 1, 0)
        call initialize_coverage_line(lines(2), "src/a&b<.f90", 2, 1)
        call file%init("src/a&b<.f90", lines)
        call file%calculate_coverage()

        allocate (files(1))
        files(1) = file
        call data%init(files)

        call create_reporter("cobertura", reporter, factory_error)
        if (factory_error) then
            write (output_unit, '(A)') '  [FAIL] Reporter factory did not create '// &
                'cobertura reporter'
            return
        end if

        call reporter%generate_report(data, out_path, success, error_message)
        if (.not. success) then
            write (output_unit, '(A)') '  [FAIL] Cobertura reporter failed to write XML'
            write (output_unit, '(A,A)') '    Error: ', trim(error_message)
            return
        end if

        call read_file_content(out_path, xml_content, read_error)
        if (read_error) then
            write (output_unit, '(A)') '  [FAIL] Could not read Cobertura XML output'
            return
        end if

        if (index(xml_content, '<coverage') > 0 .and. &
            index(xml_content, 'lines-valid="2"') > 0 .and. &
            index(xml_content, 'lines-covered="1"') > 0 .and. &
            index(xml_content, 'line-rate="0.500000"') > 0 .and. &
            index(xml_content, '<packages>') > 0 .and. &
            index(xml_content, 'filename="src/a&amp;b&lt;.f90"') > 0) then
            passed = passed + 1
            write (output_unit, '(A)') '  [PASS] Cobertura XML contains expected '// &
                'structure'
        else
            write (output_unit, '(A)') '  [FAIL] Cobertura XML missing expected content'
        end if
    end subroutine test_cobertura_reporter_writes_xml

end program test_cobertura_output_format
