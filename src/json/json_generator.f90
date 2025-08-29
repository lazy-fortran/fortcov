module json_generator
    !! JSON Generation Implementation
    !! 
    !! Focused module for JSON generation using json-fortran library.
    !! Extracted from json_io.f90 to maintain QADS size standards.
    !! Handles JSON structure creation and serialization.
    use coverage_model_core
    use coverage_operations_core, only: calculate_coverage_statistics
    use json_module, only: json_file, json_value, json_core
    use json_kinds, only: RK, IK
    use timestamp_utils, only: get_current_timestamp
    implicit none
    private
    
    public :: add_files_array_to_json
    
contains
    
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
        
        ! Process files array - use proper files field or JSON compatibility field
        if (allocated(coverage_data%files)) then
            call add_files_from_standard_array(json, files_array, coverage_data%files)
        else if (allocated(coverage_data%files_json)) then
            call add_files_from_json_array(json, files_array, coverage_data%files_json)
        end if
        
        ! Add files array to root
        call json%add(json_root, files_array)
        nullify(files_array)
    end subroutine add_files_array_to_json
    
    subroutine add_files_from_standard_array(json, files_array, files)
        !! Add files from standard coverage_file_t array
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: files_array
        type(coverage_file_t), intent(in) :: files(:)
        
        type(json_value), pointer :: file_obj => null()
        type(json_value), pointer :: lines_array => null()
        type(json_value), pointer :: line_obj => null()
        integer :: i, j
        logical :: status_ok
        
        do i = 1, size(files)
            ! Create file object with safe filename handling
            call json%create_object(file_obj, '')
            call json%check_for_errors(status_ok)
            if (.not. status_ok .or. .not. associated(file_obj)) cycle
            
            ! Add filename with validation
            if (len_trim(files(i)%filename) > 0) then
                call json%add(file_obj, 'filename', trim(files(i)%filename))
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
            
            if (allocated(files(i)%lines)) then
                do j = 1, size(files(i)%lines)
                    ! Create line object with validation
                    call json%create_object(line_obj, '')
                    call json%check_for_errors(status_ok)
                    if (.not. status_ok .or. .not. associated(line_obj)) cycle
                    
                    call json%add(line_obj, 'line_number', files(i)%lines(j)%line_number)
                    call json%add(line_obj, 'execution_count', files(i)%lines(j)%execution_count)
                    
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
    end subroutine add_files_from_standard_array
    
    subroutine add_files_from_json_array(json, files_array, files_json)
        !! Add files from JSON compatibility file_coverage_t array
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: files_array
        type(file_coverage_t), intent(in) :: files_json(:)
        
        type(json_value), pointer :: file_obj => null()
        type(json_value), pointer :: lines_array => null()
        type(json_value), pointer :: line_obj => null()
        integer :: i, j
        logical :: status_ok
        
        do i = 1, size(files_json)
            ! Create file object with safe filename handling
            call json%create_object(file_obj, '')
            call json%check_for_errors(status_ok)
            if (.not. status_ok .or. .not. associated(file_obj)) cycle
            
            ! Add filename with validation
            if (len_trim(files_json(i)%filename) > 0) then
                call json%add(file_obj, 'filename', trim(files_json(i)%filename))
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
            
            if (allocated(files_json(i)%lines)) then
                do j = 1, size(files_json(i)%lines)
                    ! Create line object with validation
                    call json%create_object(line_obj, '')
                    call json%check_for_errors(status_ok)
                    if (.not. status_ok .or. .not. associated(line_obj)) cycle
                    
                    call json%add(line_obj, 'line_number', files_json(i)%lines(j)%line_number)
                    call json%add(line_obj, 'execution_count', files_json(i)%lines(j)%execution_count)
                    
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
    end subroutine add_files_from_json_array
    
end module json_generator