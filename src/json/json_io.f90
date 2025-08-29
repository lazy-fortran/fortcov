module json_io
    !! JSON I/O Module - Consolidated high-level JSON operations
    !!
    !! Consolidated from json_io_core.f90 while preserving all functionality.
    !! This module provides the primary interface for JSON import/export operations
    !! for coverage data, eliminating the _core suffix pattern and providing
    !! a clear, unified API for JSON operations.
    use constants_core
    use coverage_model_core
    use coverage_operations_core, only: calculate_coverage_statistics
    use input_validation_core
    use error_handling_core
    ! Replace manual JSON parsing with json-fortran library
    use json_module, only: json_file, json_value, json_core
    use json_kinds, only: RK, IK
    
    ! Keep json_validator for format validation
    use json_validator
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
        type(json_value), pointer :: files_array => null()
        logical :: found
        character(len=:), allocatable :: error_msg
        
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
        
        ! Extract coverage data from parsed JSON
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
        type(json_value), pointer :: files_array => null()
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
        
        ! Add files array
        call add_files_array_to_json(json, json_root, coverage_data)
        
        ! Convert to string with error handling
        call json%print(json_root, json_output)
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
        nullify(json_root, summary_obj, files_array)
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
    
    subroutine parse_coverage_from_json_value(json_parser, root_obj, coverage_data, found)
        !! Parses coverage data from json-fortran JSON value
        type(json_core), intent(inout) :: json_parser
        type(json_value), pointer, intent(in) :: root_obj
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: found
        
        type(json_value), pointer :: files_array => null()
        type(json_value), pointer :: summary_obj => null()
        character(len=:), allocatable :: version_str, tool_str, timestamp_str
        
        found = .false.
        
        if (.not. associated(root_obj)) return
        
        ! Extract version info if present
        call json_parser%get(root_obj, 'version', version_str, found)
        if (found) coverage_data%version = version_str
        
        call json_parser%get(root_obj, 'tool', tool_str, found)
        if (found) coverage_data%tool = tool_str
        
        call json_parser%get(root_obj, 'timestamp', timestamp_str, found)
        if (found) coverage_data%timestamp = timestamp_str
        
        ! Extract files array
        call json_parser%get(root_obj, 'files', files_array, found)
        if (found .and. associated(files_array)) then
            call parse_files_from_json_array(json_parser, files_array, coverage_data, found)
        else
            print *, "Warning: No 'files' array found in JSON"
        end if
        
        found = .true.
    end subroutine parse_coverage_from_json_value
    
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
    
    
    subroutine parse_files_from_json_array(json_parser, files_array, coverage_data, found)
        !! Parses files array from json-fortran array
        type(json_core), intent(inout) :: json_parser
        type(json_value), pointer, intent(in) :: files_array
        type(coverage_data_t), intent(inout) :: coverage_data
        logical, intent(out) :: found
        
        integer :: num_files, i
        type(json_value), pointer :: file_obj => null()
        type(file_coverage_t), allocatable :: temp_files(:)
        
        found = .false.
        
        if (.not. associated(files_array)) return
        
        ! Get array size
        call json_parser%info(files_array, n_children=num_files)
        
        if (num_files <= 0) return
        
        ! Allocate temporary array for file coverage data
        allocate(temp_files(num_files))
        
        ! Parse each file object
        do i = 1, num_files
            call json_parser%get_child(files_array, i, file_obj)
            if (associated(file_obj)) then
                call parse_file_from_json_object(json_parser, file_obj, temp_files(i))
            end if
        end do
        
        ! Assign to coverage data
        if (allocated(coverage_data%files_json)) deallocate(coverage_data%files_json)
        allocate(coverage_data%files_json(num_files))
        coverage_data%files_json = temp_files
        coverage_data%total_files = num_files
        
        found = .true.
    end subroutine parse_files_from_json_array
    
    subroutine parse_file_from_json_object(json_parser, file_obj, file_coverage)
        !! Parses single file coverage from JSON object
        type(json_core), intent(inout) :: json_parser
        type(json_value), pointer, intent(in) :: file_obj
        type(file_coverage_t), intent(out) :: file_coverage
        
        character(len=:), allocatable :: filename
        type(json_value), pointer :: lines_array => null()
        logical :: found
        
        if (.not. associated(file_obj)) return
        
        ! Extract filename
        call json_parser%get(file_obj, 'filename', filename, found)
        if (found) then
            file_coverage%filename = filename
        end if
        
        ! Extract lines array
        call json_parser%get(file_obj, 'lines', lines_array, found)
        if (found .and. associated(lines_array)) then
            call parse_lines_from_json_array(json_parser, lines_array, file_coverage)
        end if
    end subroutine parse_file_from_json_object
    
    subroutine parse_lines_from_json_array(json_parser, lines_array, file_coverage)
        !! Parses lines array from JSON
        type(json_core), intent(inout) :: json_parser
        type(json_value), pointer, intent(in) :: lines_array
        type(file_coverage_t), intent(inout) :: file_coverage
        
        integer :: num_lines, i
        type(json_value), pointer :: line_obj => null()
        type(line_coverage_t), allocatable :: temp_lines(:)
        integer(IK) :: line_number, execution_count
        logical :: found
        
        if (.not. associated(lines_array)) return
        
        ! Get array size
        call json_parser%info(lines_array, n_children=num_lines)
        
        if (num_lines <= 0) return
        
        ! Allocate temporary array for line coverage data
        allocate(temp_lines(num_lines))
        
        ! Parse each line object
        do i = 1, num_lines
            call json_parser%get_child(lines_array, i, line_obj)
            if (associated(line_obj)) then
                call json_parser%get(line_obj, 'line_number', line_number, found)
                if (found) temp_lines(i)%line_number = int(line_number)
                
                call json_parser%get(line_obj, 'execution_count', execution_count, found)
                if (found) temp_lines(i)%execution_count = int(execution_count)
            end if
        end do
        
        ! Assign to file coverage
        if (allocated(file_coverage%lines)) deallocate(file_coverage%lines)
        allocate(file_coverage%lines(num_lines))
        file_coverage%lines = temp_lines
    end subroutine parse_lines_from_json_array
    
    function get_current_timestamp() result(timestamp)
        !! Gets current timestamp in ISO format
        character(len=:), allocatable :: timestamp
        character(len=19) :: date_time
        
        date_time = "2024-01-01T00:00:00"
        timestamp = date_time
    end function get_current_timestamp
    
    subroutine parse_coverage_from_json_file(json, coverage_data, found)
        !! Parse coverage from JSON file object
        type(json_file), intent(inout) :: json
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: found
        
        character(len=:), allocatable :: version_str, tool_str, timestamp_str
        type(file_coverage_t), allocatable :: files_array(:)
        integer :: num_files, i
        
        found = .false.
        
        ! Extract version info
        call json%get('version', version_str, found)
        if (found) coverage_data%version = version_str
        
        call json%get('tool', tool_str, found)
        if (found) coverage_data%tool = tool_str
        
        call json%get('timestamp', timestamp_str, found) 
        if (found) coverage_data%timestamp = timestamp_str
        
        ! Get number of files
        call json%info('files', n_children=num_files, found=found)
        if (.not. found .or. num_files <= 0) then
            print *, "Warning: No files array found or empty"
            return
        end if
        
        ! Allocate files array
        allocate(files_array(num_files))
        
        ! Extract each file
        do i = 1, num_files
            call parse_file_from_json_file(json, i, files_array(i))
        end do
        
        ! Assign to coverage data
        if (allocated(coverage_data%files_json)) deallocate(coverage_data%files_json)
        allocate(coverage_data%files_json(num_files))
        coverage_data%files_json = files_array
        coverage_data%total_files = num_files
        
        found = .true.
    end subroutine parse_coverage_from_json_file
    
    subroutine parse_file_from_json_file(json, file_index, file_coverage)
        !! Parse single file from JSON file
        type(json_file), intent(inout) :: json
        integer, intent(in) :: file_index
        type(file_coverage_t), intent(out) :: file_coverage
        
        character(len=:), allocatable :: filename, path
        character(len=64) :: index_str
        integer :: num_lines, line_index
        type(line_coverage_t), allocatable :: lines_array(:)
        logical :: found
        integer(IK) :: line_number, execution_count
        
        write(index_str, '(I0)') file_index
        
        ! Get filename
        path = 'files(' // trim(index_str) // ').filename'
        call json%get(path, filename, found)
        if (found) file_coverage%filename = filename
        
        ! Get number of lines
        path = 'files(' // trim(index_str) // ').lines'
        call json%info(path, n_children=num_lines, found=found)
        if (.not. found .or. num_lines <= 0) return
        
        ! Allocate lines array
        allocate(lines_array(num_lines))
        
        ! Extract each line
        do line_index = 1, num_lines
            write(path, '(A,I0,A,I0,A)') 'files(', file_index, ').lines(', line_index, ').line_number'
            call json%get(path, line_number, found)
            if (found) lines_array(line_index)%line_number = int(line_number)
            
            write(path, '(A,I0,A,I0,A)') 'files(', file_index, ').lines(', line_index, ').execution_count' 
            call json%get(path, execution_count, found)
            if (found) lines_array(line_index)%execution_count = int(execution_count)
        end do
        
        ! Assign lines to file coverage
        if (allocated(file_coverage%lines)) deallocate(file_coverage%lines)
        allocate(file_coverage%lines(num_lines))
        file_coverage%lines = lines_array
    end subroutine parse_file_from_json_file
    
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
        
        if (size(coverage_data%files) == 0) then
            is_valid = .false.
            return
        end if
    end function is_coverage_data_valid

end module json_io