module json_parsing_core
    !! Core JSON Parsing Implementation
    !! 
    !! Focused module for low-level JSON parsing using json-fortran library.
    !! Extracted from json_io.f90 to maintain QADS size standards.
    !! Handles core JSON value parsing and structure navigation.
    use coverage_model_core
    use json_module, only: json_file, json_value, json_core
    use json_kinds, only: RK, IK
    implicit none
    private
    
    public :: parse_coverage_from_json_value
    public :: parse_coverage_from_json_file
    public :: parse_files_from_json_array
    public :: parse_file_from_json_object
    public :: parse_lines_from_json_array
    public :: parse_file_from_json_file
    
contains
    
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
        
        ! Assign to coverage data - use proper files field
        if (allocated(coverage_data%files)) deallocate(coverage_data%files)
        ! Note: Temporarily using files_json for compatibility during refactoring
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
        
        ! Assign to coverage data - use proper files field during refactoring
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
    
end module json_parsing_core