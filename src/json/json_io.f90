module json_io
    !! JSON I/O Module - Consolidated high-level JSON operations
    !!
    !! Consolidated from json_io_core.f90 while preserving all functionality.
    !! This module provides the primary interface for JSON import/export operations
    !! for coverage data, eliminating the _core suffix pattern and providing
    !! a clear, unified API for JSON operations.
    use constants_core
    use coverage_model_core
    use coverage_operations, only: calculate_coverage_statistics
    use timestamp_utils, only: get_current_timestamp
    use input_validation_core
    use error_handling_core
    ! Replace manual JSON parsing with json-fortran library
    use json_module, only: json_file, json_value, json_core
    use json_kinds, only: RK, IK
    use json_parsing_core, only: parse_coverage_from_json_value, &
                                 parse_coverage_from_json_file, &
                                 parse_files_from_json_array, &
                                 parse_file_from_json_file
    implicit none
    private
    
    ! Define token types for backward compatibility (now unused)
    type :: json_token_t
        character(len=:), allocatable :: value
        character(len=:), allocatable :: token_type
    end type json_token_t
    
    ! Token type constants for backward compatibility
    integer, parameter :: JSON_NULL = 0
    integer, parameter :: JSON_STRING = 1 
    integer, parameter :: JSON_NUMBER = 2
    integer, parameter :: JSON_OBJECT = 3
    integer, parameter :: JSON_ARRAY = 4
    integer, parameter :: JSON_BOOLEAN = 5
    
    public :: import_coverage_from_json
    public :: export_coverage_to_json
    public :: import_coverage_from_json_safe
    public :: import_coverage_from_json_file
    public :: validate_json_coverage_format
    
    ! Compatibility exports for legacy interface
    public :: import_json_coverage
    public :: export_json_coverage
    public :: import_json_coverage_safe
    
    ! Re-export token types for backward compatibility
    public :: json_token_t
    public :: JSON_NULL, JSON_STRING, JSON_NUMBER, JSON_OBJECT, JSON_ARRAY, JSON_BOOLEAN
    
contains
    
    subroutine import_coverage_from_json(json_content, coverage_data)
        !! Core JSON coverage import implementation using json-fortran
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        
        type(json_core) :: json_parser
        type(json_value), pointer :: root_obj => null()
        logical :: found
        
        call initialize_coverage_data(coverage_data)
        
        ! Parse JSON content using json-fortran
        call json_parser%initialize()
        call json_parser%deserialize(root_obj, json_content)
        
        ! Check for parsing errors
        if (json_parser%failed()) then
            call json_parser%print_error_message()
            print *, "❌ Failed to parse JSON content with json-fortran"
            if (associated(root_obj)) call json_parser%destroy(root_obj)
            return
        end if
        
        ! Extract coverage data from JSON
        call parse_coverage_from_json_value(json_parser, root_obj, coverage_data, found)
        
        if (.not. found) then
            print *, "❌ Failed to extract coverage data from JSON"
        end if
        
        ! Clean up
        if (associated(root_obj)) call json_parser%destroy(root_obj)
    end subroutine import_coverage_from_json
    
    subroutine import_coverage_from_json_safe(json_content, coverage_data, error_caught)
        !! Safe JSON coverage import with error handling
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_caught
        
        type(error_context_t) :: error_ctx
        
        error_ctx%error_code = ERROR_SUCCESS
        error_ctx%message = ""
        error_ctx%suggestion = ""
        
        error_caught = .false.
        
        if (.not. validate_json_coverage_format(json_content)) then
            error_ctx%error_code = ERROR_INVALID_DATA
            error_ctx%message = "Invalid JSON format"
            error_caught = .true.
            return
        end if
        
        call import_coverage_from_json(json_content, coverage_data)
        
        if (.not. is_coverage_data_valid(coverage_data)) then
            error_ctx%error_code = ERROR_INVALID_DATA
            error_ctx%message = "Coverage data validation failed"
            error_caught = .true.
            return
        end if
        
        error_ctx%error_code = ERROR_SUCCESS
        error_ctx%message = "JSON import completed successfully"
    end subroutine import_coverage_from_json_safe
    
    subroutine export_coverage_to_json(coverage_data, json_output)
        !! Core JSON coverage export implementation using json-fortran
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable, intent(out) :: json_output
        
        type(json_core) :: json
        type(json_value), pointer :: json_root => null()
        type(json_value), pointer :: summary_obj => null()
        type(extended_coverage_stats_t) :: stats
        logical :: status_ok
        
        ! Initialize json-fortran
        call json%initialize()
        
        ! Create root object
        call json%create_object(json_root, '')
        
        ! Add metadata
        call json%add(json_root, 'version', '1.0')
        call json%add(json_root, 'timestamp', get_current_timestamp())
        call json%add(json_root, 'tool', 'fortcov')
        
        ! Calculate and add summary
        call calculate_coverage_statistics(coverage_data, stats)
        call json%create_object(summary_obj, 'summary')
        call json%add(summary_obj, 'line_coverage', stats%line_coverage)
        call json%add(summary_obj, 'total_lines', stats%total_lines)
        call json%add(summary_obj, 'covered_lines', stats%covered_lines)
        call json%add(summary_obj, 'total_files', stats%total_files)
        call json%add(summary_obj, 'covered_files', stats%covered_files)
        call json%add(json_root, summary_obj)
        
        ! Add files array to JSON
        call add_files_array_to_json(json, json_root, coverage_data)
        
        ! Convert to string with error handling
        call json%print_to_string(json_root, json_output)
        call json%check_for_errors(status_ok)
        
        ! Handle errors with fallback
        if (.not. status_ok .or. .not. allocated(json_output)) then
            if (allocated(json_output)) deallocate(json_output)
            json_output = '{"error": "JSON export failed", "tool": "fortcov"}'
        end if
        
        ! Validate output is not empty
        if (len_trim(json_output) == 0) then
            json_output = '{"error": "Empty JSON output", "tool": "fortcov"}'
        end if
        
        ! Cleanup
        call json%destroy(json_root)
        nullify(json_root, summary_obj)
    end subroutine export_coverage_to_json
    
    function validate_json_coverage_format(json_content) result(is_valid)
        !! Validates JSON format for coverage data using json-fortran
        character(len=*), intent(in) :: json_content
        logical :: is_valid
        
        type(json_core) :: json_parser
        type(json_value), pointer :: root_obj => null()
        
        ! Use json-fortran to validate JSON structure
        call json_parser%initialize()
        call json_parser%deserialize(root_obj, json_content)
        
        is_valid = .not. json_parser%failed()
        
        if (associated(root_obj)) call json_parser%destroy(root_obj)
    end function validate_json_coverage_format
    
    ! === COMPATIBILITY WRAPPERS ===
    
    subroutine import_json_coverage(json_content, coverage_data)
        !! Compatibility wrapper for legacy interface
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        
        call import_coverage_from_json(json_content, coverage_data)
    end subroutine import_json_coverage
    
    subroutine export_json_coverage(coverage_data, json_output)
        !! Compatibility wrapper for legacy interface
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable, intent(out) :: json_output
        
        call export_coverage_to_json(coverage_data, json_output)
    end subroutine export_json_coverage
    
    subroutine import_json_coverage_safe(json_content, coverage_data, error_caught)
        !! Compatibility wrapper for legacy interface
        character(len=*), intent(in) :: json_content
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_caught
        
        call import_coverage_from_json_safe(json_content, coverage_data, error_caught)
    end subroutine import_json_coverage_safe
    
    subroutine import_coverage_from_json_file(filename, coverage_data, error_caught)
        !! Import coverage from JSON file using json-fortran
        character(len=*), intent(in) :: filename
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_caught
        
        type(json_file) :: json
        logical :: found
        
        error_caught = .false.
        call initialize_coverage_data(coverage_data)
        
        ! Load JSON file using json-fortran
        call json%initialize()
        call json%load(filename=filename)
        
        if (json%failed()) then
            call json%print_error_message()
            print *, "❌ Failed to load JSON file:", filename
            error_caught = .true.
            call json%destroy()
            return
        end if
        
        call parse_coverage_from_json_file(json, coverage_data, found)
        
        if (.not. found) then
            print *, "❌ Failed to extract coverage data from JSON file:", filename
            error_caught = .true.
        end if
        
        call json%destroy()
    end subroutine import_coverage_from_json_file
    
    ! === HELPER FUNCTIONS (json-fortran implementation) ===
    
    
    subroutine add_files_array_to_json(json, json_root, coverage_data)
        !! Adds files array to JSON using json-fortran with robust error handling
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: json_root
        type(coverage_data_t), intent(in) :: coverage_data
        
        type(json_value), pointer :: files_array => null()
        type(json_value), pointer :: file_obj => null()
        type(json_value), pointer :: lines_array => null()
        type(json_value), pointer :: line_obj => null()
        integer :: i, j
        logical :: status_ok
        
        ! Validate input
        if (.not. associated(json_root)) return
        
        ! Create files array with error checking
        call json%create_array(files_array, 'files')
        call json%check_for_errors(status_ok)
        if (.not. status_ok .or. .not. associated(files_array)) return
        
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                ! Create file object with safe filename handling
                call json%create_object(file_obj, '')
                call json%check_for_errors(status_ok)
                if (.not. status_ok .or. .not. associated(file_obj)) cycle
                
                ! Add filename with validation
                if (len_trim(coverage_data%files(i)%filename) > 0) then
                    call json%add(file_obj, 'filename', &
                                trim(coverage_data%files(i)%filename))
                else
                    call json%add(file_obj, 'filename', 'unknown')
                end if
                
                ! Create lines array for this file
                call json%create_array(lines_array, 'lines')
                call json%check_for_errors(status_ok)
                if (.not. status_ok .or. .not. associated(lines_array)) then
                    call json%destroy(file_obj)
                    cycle
                end if
                
                if (allocated(coverage_data%files(i)%lines)) then
                    do j = 1, size(coverage_data%files(i)%lines)
                        ! Create line object with validation
                        call json%create_object(line_obj, '')
                        call json%check_for_errors(status_ok)
                        if (.not. status_ok .or. .not. associated(line_obj)) cycle
                        
                        call json%add(line_obj, 'line_number', &
                                    coverage_data%files(i)%lines(j)%line_number)
                        call json%add(line_obj, 'execution_count', &
                                    coverage_data%files(i)%lines(j)%execution_count)
                        
                        ! Add line to lines array
                        call json%add(lines_array, line_obj)
                        nullify(line_obj)
                    end do
                end if
                
                ! Add lines array to file object
                call json%add(file_obj, lines_array)
                nullify(lines_array)
                
                ! Add file to files array
                call json%add(files_array, file_obj)
                nullify(file_obj)
            end do
        end if
        
        ! Add files array to root
        call json%add(json_root, files_array)
        nullify(files_array)
    end subroutine add_files_array_to_json
    
    
    
    
    
    
    
    subroutine initialize_coverage_data(coverage_data)
        !! Initializes coverage data structure
        type(coverage_data_t), intent(out) :: coverage_data
        
        coverage_data%version = "1.0"
        coverage_data%tool = "fortcov"
    end subroutine initialize_coverage_data
    
    function is_coverage_data_valid(coverage_data) result(is_valid)
        !! Validates coverage data structure
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: is_valid
        
        is_valid = .true.
        
        if (.not. allocated(coverage_data%files)) then
            is_valid = .false.
            return
        end if
        
        ! Empty coverage data (0 files) is valid
    end function is_coverage_data_valid


end module json_io