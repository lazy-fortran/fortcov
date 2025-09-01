module gcov_line_parser
    use coverage_model_core
    use string_utils
    use error_handling_core
    use coverage_data_builder
    use input_validation_core, only: normalize_execution_count
    implicit none
    private
    
    ! Public procedures
    public :: parse_gcov_line
    public :: extract_source_filename
    
contains

    ! Process single gcov line - dispatch to header or coverage data processing
    subroutine parse_gcov_line(line, path, source_filename, lines_array, &
                              lines_count, files_array, files_count, has_source)
        character(len=*), intent(in) :: line, path
        character(len=:), allocatable, intent(inout) :: source_filename
        type(coverage_line_t), allocatable, intent(inout) :: lines_array(:)
        integer, intent(inout) :: lines_count
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        integer, intent(inout) :: files_count
        logical, intent(inout) :: has_source
        
        ! Handle source file headers
        if (index(line, "Source:") > 0) then
            call handle_source_header(line, source_filename, lines_array, lines_count, &
                                     files_array, files_count, has_source)
        else if (index(line, "Graph:") > 0 .or. index(line, "Data:") > 0) then
            ! Skip graph and data headers
            return
        else if (index(line, "function ") > 0 .and. index(line, " called ") > 0) then
            call parse_function_summary_line(line, source_filename, files_array, files_count)
        else
            ! Process coverage data line
            call process_coverage_data_line(line, path, source_filename, lines_array, &
                                          lines_count)
        end if
    end subroutine parse_gcov_line
    
    ! Handle Source: header lines
    subroutine handle_source_header(line, source_filename, lines_array, lines_count, &
                                   files_array, files_count, has_source)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(inout) :: source_filename
        type(coverage_line_t), allocatable, intent(inout) :: lines_array(:)
        integer, intent(inout) :: lines_count
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        integer, intent(inout) :: files_count
        logical, intent(inout) :: has_source
        
        ! Save previous file if we have data
        if (has_source .and. lines_count > 0) then
            call add_file_to_array(files_array, files_count, source_filename, &
                                 lines_array(1:lines_count))
            lines_count = 0
        end if
        
        source_filename = extract_source_filename(line)
        has_source = .true.
    end subroutine handle_source_header
    
    ! Process coverage data line (format: execution_count:line_number:content)
    subroutine process_coverage_data_line(line, path, source_filename, lines_array, &
                                         lines_count)
        character(len=*), intent(in) :: line, path, source_filename
        type(coverage_line_t), allocatable, intent(inout) :: lines_array(:)
        integer, intent(inout) :: lines_count
        
        character(len=:), allocatable :: parts(:)
        integer :: line_num, exec_count
        logical :: is_executable
        
        parts = split(line, ":")
        if (size(parts) < 3) return
        
        call parse_execution_count(parts(1), path, exec_count, is_executable)
        call parse_line_number(parts(2), path, line_num)
        
        if (line_num <= 0) return
        if (is_executable .and. exec_count < 0) return
        if (line_num == 0) return
        
        call add_coverage_line(source_filename, line_num, exec_count, is_executable, &
                             lines_array, lines_count)
    end subroutine process_coverage_data_line
    
    ! Parse execution count from gcov line part
    subroutine parse_execution_count(count_str, path, exec_count, is_executable)
        character(len=*), intent(in) :: count_str, path
        integer, intent(out) :: exec_count
        logical, intent(out) :: is_executable
        
        integer :: iostat_val
        character(len=:), allocatable :: trimmed_str
        
        trimmed_str = trim(adjustl(count_str))
        
        if (trimmed_str == "-") then
            exec_count = -1
            is_executable = .false.
        else if (trimmed_str == "#####") then
            exec_count = 0
            is_executable = .true.
        else
            read(trimmed_str, *, iostat=iostat_val) exec_count
            if (iostat_val /= 0) then
                call log_execution_count_error(trimmed_str, path)
                exec_count = 0
                is_executable = .false.
            else
                exec_count = normalize_execution_count(exec_count)
                is_executable = .true.
            end if
        end if
    end subroutine parse_execution_count
    
    ! Parse line number from gcov line part
    subroutine parse_line_number(line_str, path, line_num)
        character(len=*), intent(in) :: line_str, path
        integer, intent(out) :: line_num
        
        integer :: iostat_val
        
        read(line_str, *, iostat=iostat_val) line_num
        if (iostat_val /= 0) then
            call log_line_number_error(line_str, path)
            line_num = -1
        end if
    end subroutine parse_line_number
    
    ! Add coverage line to lines array
    subroutine add_coverage_line(source_filename, line_num, exec_count, is_executable, &
                                lines_array, lines_count)
        character(len=*), intent(in) :: source_filename
        integer, intent(in) :: line_num, exec_count
        logical, intent(in) :: is_executable
        type(coverage_line_t), allocatable, intent(inout) :: lines_array(:)
        integer, intent(inout) :: lines_count
        
        lines_count = lines_count + 1
        if (lines_count > size(lines_array)) then
            call resize_lines_array(lines_array)
        end if
        
        call lines_array(lines_count)%init(source_filename, line_num, exec_count, &
                                          is_executable)
    end subroutine add_coverage_line
    
    ! Log execution count parsing errors
    subroutine log_execution_count_error(count_str, path)
        character(len=*), intent(in) :: count_str, path
        
        type(error_context_t) :: error_ctx
        call clear_error_context(error_ctx)
        error_ctx%error_code = ERROR_INVALID_DATA
        call safe_write_message(error_ctx, "Invalid execution count '" // &
                               trim(count_str) // "' in gcov file: " // trim(path))
        call safe_write_suggestion(error_ctx, &
                                  "Check gcov file format and regenerate if corrupted")
        call safe_write_context(error_ctx, "Gcov parsing")
        call log_error(error_ctx)
    end subroutine log_execution_count_error
    
    ! Log line number parsing errors
    subroutine log_line_number_error(line_str, path)
        character(len=*), intent(in) :: line_str, path
        
        type(error_context_t) :: error_ctx
        call clear_error_context(error_ctx)
        error_ctx%error_code = ERROR_INVALID_DATA
        call safe_write_message(error_ctx, "Invalid line number '" // &
                               trim(line_str) // "' in gcov file: " // trim(path))
        call safe_write_suggestion(error_ctx, &
                                  "Check gcov file format and regenerate if corrupted")
        call safe_write_context(error_ctx, "Gcov parsing")
        call log_error(error_ctx)
    end subroutine log_line_number_error
    
    ! Helper function to extract source filename from "Source:filename" line
    ! Fix for Issue #103: Clean extraction to avoid malformed filenames
    ! Handles gcov format: "        -:    0:Source:src/coverage_engine.f90"
    function extract_source_filename(line) result(filename)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: filename
        integer :: source_pos
        
        ! Look specifically for "Source:" pattern to avoid gcov formatting
        source_pos = index(line, "Source:")
        if (source_pos > 0) then
            ! Extract everything after "Source:" with bounds checking
            source_pos = source_pos + 7  ! Length of "Source:"
            if (source_pos <= len(line)) then
                filename = trim(line(source_pos:))
            else
                filename = "unknown"
            end if
        else
            filename = "unknown"
        end if
    end function extract_source_filename
    
    ! Parse function summary line and add function to appropriate file
    ! Format: "function FUNCTION_NAME called N returned P%"
    subroutine parse_function_summary_line(line, current_filename, files_array, &
                                          files_count)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: current_filename  
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        integer, intent(inout) :: files_count
        
        character(len=:), allocatable :: function_name, temp_str
        integer :: function_call_count, return_percentage
        integer :: func_start, func_end, called_pos, returned_pos
        integer :: iostat_val
        type(coverage_function_t) :: new_function
        
        ! Extract function name
        func_start = index(line, "function ") + 9
        called_pos = index(line, " called ")
        
        if (func_start <= 9 .or. called_pos <= 0 .or. called_pos <= func_start) then
            return ! Malformed line, skip
        end if
        
        func_end = called_pos - 1
        function_name = trim(line(func_start:func_end))
        
        if (len(function_name) == 0) then
            return ! Empty function name, skip
        end if
        
        ! Extract call count
        called_pos = called_pos + 8  ! Move past " called "
        returned_pos = index(line, " returned ")
        
        if (returned_pos <= 0 .or. returned_pos <= called_pos) then
            return ! Malformed line, skip
        end if
        
        temp_str = trim(line(called_pos:returned_pos-1))
        read(temp_str, *, iostat=iostat_val) function_call_count
        
        if (iostat_val /= 0) then
            function_call_count = 0  ! Default to 0 if parsing fails
        end if
        
        ! Extract return percentage (optional, for future use)
        returned_pos = returned_pos + 10  ! Move past " returned "
        temp_str = trim(line(returned_pos:))
        
        ! Remove '%' if present
        if (len(temp_str) > 0 .and. temp_str(len(temp_str):len(temp_str)) == '%') then
            temp_str = temp_str(1:len(temp_str)-1)
        end if
        
        read(temp_str, *, iostat=iostat_val) return_percentage
        
        if (iostat_val /= 0) then
            return_percentage = 0  ! Default if parsing fails
        end if
        
        ! Create function object with parsed data
        call new_function%init(function_name, current_filename, 0, &
                              function_call_count)
        
        ! Add function to the current file (find or create file entry)
        call add_function_to_current_file(new_function, current_filename, &
                                         files_array, files_count)
    end subroutine parse_function_summary_line
    
    ! Add function to current file in files array
    subroutine add_function_to_current_file(func, filename, files_array, files_count)
        type(coverage_function_t), intent(in) :: func
        character(len=*), intent(in) :: filename
        type(coverage_file_t), allocatable, intent(inout) :: files_array(:)
        integer, intent(inout) :: files_count
        
        integer :: file_idx
        type(coverage_function_t), allocatable :: temp_functions(:)
        integer :: current_func_count
        integer :: stat
        character(len=512) :: errmsg
        
        ! Find the current file in the array
        file_idx = 0
        do file_idx = 1, files_count
            if (files_array(file_idx)%filename == filename) then
                exit
            end if
        end do
        
        ! If file not found, this function will be added when file is created
        ! For now, we'll store it for later association
        if (file_idx > files_count) then
            return  ! File not found, skip for now
        end if
        
        ! Add function to the file's function array
        if (.not. allocated(files_array(file_idx)%functions)) then
            allocate(files_array(file_idx)%functions(1), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate functions array: " // trim(errmsg)
                return
            end if
            files_array(file_idx)%functions(1) = func
        else
            ! Extend the functions array
            current_func_count = size(files_array(file_idx)%functions)
            allocate(temp_functions(current_func_count + 1), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to allocate temp_functions: " // trim(errmsg)
                return
            end if
            temp_functions(1:current_func_count) = files_array(file_idx)%functions
            temp_functions(current_func_count + 1) = func
            
            deallocate(files_array(file_idx)%functions, stat=stat)
            allocate(files_array(file_idx)%functions, source=temp_functions, stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Failed to reallocate functions array: " // trim(errmsg)
                deallocate(temp_functions, stat=stat)
                return
            end if
            deallocate(temp_functions, stat=stat)
        end if
    end subroutine add_function_to_current_file
    
    ! Helper subroutine to resize lines array when needed
    subroutine resize_lines_array(lines_array)
        type(coverage_line_t), allocatable, intent(inout) :: lines_array(:)
        type(coverage_line_t), allocatable :: temp_array(:)
        integer :: current_size, new_size
        integer :: stat
        character(len=512) :: errmsg
        
        current_size = size(lines_array)
        new_size = current_size * 2
        
        ! Copy existing data to temporary array
        allocate(temp_array(current_size), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to allocate temp_array for resize: " // trim(errmsg)
            return
        end if
        temp_array = lines_array
        
        ! Reallocate with larger size
        deallocate(lines_array, stat=stat)
        allocate(lines_array(new_size), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Failed to resize lines_array: " // trim(errmsg)
            deallocate(temp_array, stat=stat)
            return
        end if
        
        ! Copy data back
        lines_array(1:current_size) = temp_array
        
        deallocate(temp_array, stat=stat)
    end subroutine resize_lines_array

end module gcov_line_parser
