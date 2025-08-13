program test_gcov_binary_format
    use gcov_binary_format
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running gcov text parser tests with REAL gcov data..."
    
    ! Test 1: Parse real gcov text file
    call test_parse_real_gcov_text()
    
    ! Test 2: Load real gcno/gcda files and generate gcov text
    call test_load_real_gcov_files()
    
    ! Test 3: Validate gcov line data parsing
    call test_gcov_line_data_parsing()
    
    ! Test 4: Test gcov command execution
    call test_gcov_command_execution()
    
    ! Test 5: Handle missing gcda gracefully
    call test_handle_missing_gcda()
    
    ! Test 6: Test data integrity validation
    call test_data_integrity_validation()
    
    ! Test 7: Test executable line detection from gcov
    call test_executable_line_detection()
    
    ! Test 8: Test counter extraction
    call test_counter_extraction()
    
    ! Test 9: Test error handling for invalid files
    call test_error_handling()
    
    ! Test 10: Test complete workflow with real data
    call test_complete_workflow()
    
    ! Report results
    write(*,*) ""
    if (test_count > 0) then
        write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
                   pass_count, " (", (pass_count * 100) / test_count, "%)"
    else
        write(*,*) "No tests run"
    end if
    
    if (pass_count /= test_count) then
        stop 1  ! Exit with error code
    end if
    
contains

    subroutine assert(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: expected, actual
        
        test_count = test_count + 1
        if (condition) then
            write(*,'(A,A)') "PASS: ", test_name
            pass_count = pass_count + 1
        else
            write(*,'(A,A)') "FAIL: ", test_name
            write(*,'(A,A)') "  Expected: ", expected
            write(*,'(A,A)') "  Actual:   ", actual
        end if
    end subroutine assert

    subroutine assert_int(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        integer, intent(in) :: expected, actual
        character(len=20) :: exp_str, act_str
        
        write(exp_str, '(I0)') expected
        write(act_str, '(I0)') actual
        call assert(condition, test_name, trim(exp_str), trim(act_str))
    end subroutine assert_int

    ! Test 1: Parse existing gcov text file
    subroutine test_parse_real_gcov_text()
        type(gcov_data_reader_t) :: reader
        logical :: success, file_exists
        character(len=:), allocatable :: gcov_path
        
        gcov_path = "/home/ert/code/fortcov/test_data/sample.f90.gcov"
        
        ! Check if gcov text file exists
        inquire(file=gcov_path, exist=file_exists)
        if (.not. file_exists) then
            call assert(.false., "Real gcov text file exists", "exists", "missing")
            return
        end if
        
        call reader%init()
        call reader%parse_gcov_text(gcov_path, success)
        
        call assert(success, "Parse real gcov text file", "success", &
                   merge("success", "failed ", success))
        
        if (success) then
            call assert(allocated(reader%lines), "Lines data allocated", &
                       "allocated", "present")
            
            if (allocated(reader%lines)) then
                call assert_int(size(reader%lines) > 0, "Lines count > 0", &
                              1, size(reader%lines))
            end if
        end if
    end subroutine test_parse_real_gcov_text

    ! Test 2: Load real gcno/gcda files and run gcov
    subroutine test_load_real_gcov_files()
        type(gcov_data_reader_t) :: reader
        logical :: success, gcno_exists, gcda_exists
        character(len=:), allocatable :: gcno_path, gcda_path
        
        gcno_path = "/home/ert/code/fortcov/test_data/sample.gcno"
        gcda_path = "/home/ert/code/fortcov/test_data/sample.gcda"
        
        ! Check if files exist
        inquire(file=gcno_path, exist=gcno_exists)
        inquire(file=gcda_path, exist=gcda_exists)
        
        if (.not. gcno_exists .or. .not. gcda_exists) then
            call assert(.false., "Real gcov files exist", "both exist", "missing")
            return
        end if
        
        call reader%init()
        call reader%load_files(gcno_path, gcda_path, success)
        
        call assert(success, "Load real gcov files", "success", &
                   merge("success", "failed ", success))
        
        if (success) then
            call assert(reader%has_gcda, "GCDA data loaded", "true", &
                       merge("true ", "false", reader%has_gcda))
            
            call assert(allocated(reader%functions), "Functions loaded", &
                       "allocated", "present")
            
            call assert(allocated(reader%lines), "Line data loaded", &
                       "allocated", "present")
        end if
    end subroutine test_load_real_gcov_files

    ! Test 3: Validate gcov line data parsing from real data
    subroutine test_gcov_line_data_parsing()
        type(gcov_data_reader_t) :: reader
        logical :: success
        character(len=:), allocatable :: gcov_path
        integer :: i, exec_lines, non_exec_lines, uncovered_lines
        
        gcov_path = "/home/ert/code/fortcov/test_data/sample.f90.gcov"
        
        call reader%init()
        call reader%parse_gcov_text(gcov_path, success)
        
        if (.not. success) then
            call assert(.false., "Line data parsing setup", "success", "failed")
            return
        end if
        
        ! Count different line types from real gcov data
        exec_lines = 0
        non_exec_lines = 0
        uncovered_lines = 0
        
        do i = 1, size(reader%lines)
            if (reader%lines(i)%is_executable) then
                if (reader%lines(i)%execution_count == 0) then
                    uncovered_lines = uncovered_lines + 1
                else
                    exec_lines = exec_lines + 1
                end if
            else
                non_exec_lines = non_exec_lines + 1
            end if
        end do
        
        call assert(exec_lines > 0, "Found executed lines", ">0", "found some")
        call assert(non_exec_lines > 0, "Found non-executable lines", ">0", "found some")
        call assert(uncovered_lines >= 0, "Uncovered lines count valid", ">=0", "valid")
        
        ! Check specific line details match expected gcov format
        if (size(reader%lines) > 0) then
            call assert(reader%lines(1)%line_number > 0, "Line numbers valid", &
                       ">0", "valid")
        end if
    end subroutine test_gcov_line_data_parsing

    ! Test 4: Test gcov command execution
    subroutine test_gcov_command_execution()
        type(gcov_data_reader_t) :: reader
        logical :: success, source_exists
        character(len=:), allocatable :: source_path
        
        source_path = "/home/ert/code/fortcov/test_data/sample.f90"
        
        inquire(file=source_path, exist=source_exists)
        if (.not. source_exists) then
            call assert(.false., "Source file for gcov test", "exists", "missing")
            return
        end if
        
        call reader%init()
        call reader%run_gcov_command(source_path, .true., success)
        
        call assert(success, "Gcov command execution", "success", &
                   merge("success", "failed ", success))
    end subroutine test_gcov_command_execution

    ! Test 5: Handle missing GCDA file gracefully
    subroutine test_handle_missing_gcda()
        type(gcov_data_reader_t) :: reader
        logical :: success, gcno_exists
        character(len=:), allocatable :: gcno_path
        
        gcno_path = "/home/ert/code/fortcov/test_data/sample.gcno"
        
        inquire(file=gcno_path, exist=gcno_exists)
        if (.not. gcno_exists) then
            call assert(.false., "GCNO file for missing GCDA test", "exists", "missing")
            return
        end if
        
        call reader%init()
        call reader%load_files(gcno_path, "nonexistent.gcda", success)
        
        call assert(success, "Handle missing GCDA", "handled", &
                   merge("graceful", "failed  ", success))
        
        if (success) then
            call assert(.not. reader%has_gcda, "GCDA flag properly set", &
                       "false", merge("false", "true ", .not. reader%has_gcda))
        end if
    end subroutine test_handle_missing_gcda

    ! Test 6: Test data integrity validation
    subroutine test_data_integrity_validation()
        type(gcov_data_reader_t) :: reader
        logical :: success, valid
        character(len=:), allocatable :: gcno_path, gcda_path
        
        gcno_path = "/home/ert/code/fortcov/test_data/sample.gcno"
        gcda_path = "/home/ert/code/fortcov/test_data/sample.gcda"
        
        call reader%init()
        call reader%load_files(gcno_path, gcda_path, success)
        
        if (.not. success) then
            call assert(.false., "Data integrity setup", "success", "failed")
            return
        end if
        
        valid = reader%validate_data_integrity()
        
        call assert(valid, "Data integrity validation", "valid", &
                   merge("valid  ", "invalid", valid))
    end subroutine test_data_integrity_validation

    ! Test 7: Test executable line detection from gcov
    subroutine test_executable_line_detection()
        type(gcov_data_reader_t) :: reader
        logical :: success, found_executable, found_non_executable
        character(len=:), allocatable :: gcov_path
        integer :: i
        
        gcov_path = "/home/ert/code/fortcov/test_data/sample.f90.gcov"
        
        call reader%init()
        call reader%parse_gcov_text(gcov_path, success)
        
        if (.not. success) then
            call assert(.false., "Executable line test setup", "success", "failed")
            return
        end if
        
        found_executable = .false.
        found_non_executable = .false.
        
        do i = 1, size(reader%lines)
            if (reader%lines(i)%is_executable) then
                found_executable = .true.
            else
                found_non_executable = .true.
            end if
        end do
        
        call assert(found_executable, "Found executable lines", "found", &
                   "detected")
        call assert(found_non_executable, "Found non-executable lines", "found", &
                   "detected")
    end subroutine test_executable_line_detection

    ! Test 8: Test counter extraction from gcov data
    subroutine test_counter_extraction()
        type(gcov_data_reader_t) :: reader
        logical :: success
        character(len=:), allocatable :: gcov_path
        
        gcov_path = "/home/ert/code/fortcov/test_data/sample.f90.gcov"
        
        call reader%init()
        call reader%parse_gcov_text(gcov_path, success)
        
        if (.not. success) then
            call assert(.false., "Counter extraction setup", "success", "failed")
            return
        end if
        
        call assert(reader%counters%count >= 0, "Counter count valid", ">=0", &
                   "valid")
        
        if (reader%counters%count > 0) then
            call assert(allocated(reader%counters%values), "Counter values allocated", &
                       "allocated", "present")
        end if
    end subroutine test_counter_extraction

    ! Test 9: Test error handling for invalid files
    subroutine test_error_handling()
        type(gcov_data_reader_t) :: reader
        logical :: success
        
        call reader%init()
        call reader%parse_gcov_text("nonexistent_file.gcov", success)
        
        call assert(.not. success, "Handle nonexistent file", "failed", &
                   merge("handled", "crashed", .not. success))
        
        if (.not. success) then
            call assert(allocated(reader%error_message), "Error message set", &
                       "set", "present")
        end if
    end subroutine test_error_handling

    ! Test 10: Test complete workflow with real data
    subroutine test_complete_workflow()
        type(gcov_data_reader_t) :: reader
        logical :: success
        character(len=:), allocatable :: gcno_path, gcda_path
        integer :: line_count, func_count
        
        gcno_path = "/home/ert/code/fortcov/test_data/sample.gcno"
        gcda_path = "/home/ert/code/fortcov/test_data/sample.gcda"
        
        ! Complete workflow: load files, parse data, validate
        call reader%init()
        call reader%load_files(gcno_path, gcda_path, success)
        
        call assert(success, "Complete workflow execution", "success", &
                   merge("success", "failed ", success))
        
        if (success) then
            ! Validate all components are present
            call assert(allocated(reader%functions), "Functions present", &
                       "present", "allocated")
            call assert(allocated(reader%lines), "Lines present", &
                       "present", "allocated")
            
            if (allocated(reader%functions)) then
                func_count = size(reader%functions)
                call assert_int(func_count >= 1, "Function count", 1, func_count)
            end if
            
            if (allocated(reader%lines)) then
                line_count = size(reader%lines)
                call assert_int(line_count > 0, "Line count", 1, line_count)
            end if
            
            call assert(reader%validate_data_integrity(), "Final validation", &
                       "valid", "passed")
        end if
    end subroutine test_complete_workflow

end program test_gcov_binary_format