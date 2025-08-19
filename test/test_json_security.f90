program test_json_security
    use iso_fortran_env, only: real64, int32
    use coverage_model
    use coverage_reporter
    implicit none

    logical :: test_passed = .true.

    print *, "Running JSON security tests..."

    call test_extremely_large_dataset()
    call test_very_long_filename_security()
    call test_json_structure_validation()

    if (test_passed) then
        print *, "All JSON security tests passed"
        call exit(0)
    else
        print *, "Some JSON security tests failed"
        call exit(1)
    end if

contains

    ! Test extremely large dataset to verify no buffer overflows
    subroutine test_extremely_large_dataset()
        type(json_reporter_t) :: reporter
        type(coverage_data_t) :: dataset
        logical :: success
        character(len=:), allocatable :: error_msg
        integer :: i, j
        
        print *, "Test: Extremely large dataset security..."
        
        ! Create dataset with 100 files, 1000 lines each (100K total lines)
        ! This would cause buffer overflow in the old implementation
        allocate(dataset%files(100))
        
        do i = 1, 100
            dataset%files(i)%filename = "security_test_file.f90"
            
            allocate(dataset%files(i)%lines(1000))
            do j = 1, 1000
                dataset%files(i)%lines(j)%line_number = j
                dataset%files(i)%lines(j)%execution_count = mod(j, 10)
                dataset%files(i)%lines(j)%is_executable = (mod(j, 2) == 0)
            end do
            
            allocate(dataset%files(i)%functions(0))
        end do
        
        ! This should work without buffer overflow or crashes
        call reporter%generate_report(dataset, "test_security_large.json", success, error_msg)
        
        if (.not. success) then
            print *, "FAIL: Large dataset security test failed"
            test_passed = .false.
        else
            print *, "PASS: Large dataset security test completed"
        end if
        
        ! Clean up
        do i = 1, 100
            deallocate(dataset%files(i)%lines)
            deallocate(dataset%files(i)%functions)
        end do
        deallocate(dataset%files)
    end subroutine test_extremely_large_dataset

    ! Test very long filenames to check for buffer overflows
    subroutine test_very_long_filename_security()
        type(json_reporter_t) :: reporter
        type(coverage_data_t) :: dataset
        logical :: success
        character(len=:), allocatable :: error_msg
        character(len=1000) :: very_long_filename
        integer :: i
        
        print *, "Test: Very long filename security..."
        
        ! Create filename that would overflow old buffer implementation
        very_long_filename = ""
        do i = 1, 100
            very_long_filename = trim(very_long_filename) // "long_path_"
        end do
        very_long_filename = trim(very_long_filename) // ".f90"
        
        allocate(dataset%files(1))
        dataset%files(1)%filename = trim(very_long_filename)
        
        allocate(dataset%files(1)%lines(10))
        do i = 1, 10
            dataset%files(1)%lines(i)%line_number = i
            dataset%files(1)%lines(i)%execution_count = i * 5
            dataset%files(1)%lines(i)%is_executable = .true.
        end do
        
        allocate(dataset%files(1)%functions(0))
        
        call reporter%generate_report(dataset, "test_security_filename.json", success, error_msg)
        
        if (.not. success) then
            print *, "FAIL: Long filename security test failed"
            test_passed = .false.
        else
            print *, "PASS: Long filename security test completed"
        end if
        
        deallocate(dataset%files(1)%lines)
        deallocate(dataset%files(1)%functions)
        deallocate(dataset%files)
    end subroutine test_very_long_filename_security

    ! Validate JSON structure is correct
    subroutine test_json_structure_validation()
        type(json_reporter_t) :: reporter
        type(coverage_data_t) :: dataset
        logical :: success
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: json_content
        integer :: unit, ios, file_size
        character :: single_char
        integer :: brace_count, bracket_count, quote_count
        logical :: in_string
        
        print *, "Test: JSON structure validation..."
        
        ! Create simple test dataset
        call create_simple_test_dataset(dataset)
        
        call reporter%generate_report(dataset, "test_json_structure.json", success, error_msg)
        
        if (.not. success) then
            print *, "FAIL: JSON structure generation failed"
            test_passed = .false.
            return
        end if
        
        ! Read and validate JSON structure
        open(newunit=unit, file="test_json_structure.json", status='old', iostat=ios)
        if (ios /= 0) then
            print *, "FAIL: Cannot read generated JSON file"
            test_passed = .false.
            return
        end if
        
        ! Simple validation: count braces and brackets
        brace_count = 0
        bracket_count = 0
        quote_count = 0
        in_string = .false.
        
        do
            read(unit, '(A)', advance='no', iostat=ios) single_char
            if (ios /= 0) exit
            
            if (single_char == '"' .and. .not. in_string) then
                in_string = .true.
                quote_count = quote_count + 1
            else if (single_char == '"' .and. in_string) then
                in_string = .false.
            else if (.not. in_string) then
                if (single_char == '{') brace_count = brace_count + 1
                if (single_char == '}') brace_count = brace_count - 1
                if (single_char == '[') bracket_count = bracket_count + 1
                if (single_char == ']') bracket_count = bracket_count - 1
            end if
        end do
        
        close(unit)
        
        ! Validate balanced braces and brackets
        if (brace_count /= 0 .or. bracket_count /= 0) then
            print *, "FAIL: JSON structure validation - unbalanced brackets/braces"
            print *, "Brace balance:", brace_count, "Bracket balance:", bracket_count
            test_passed = .false.
        else if (mod(quote_count, 2) /= 0) then
            print *, "FAIL: JSON structure validation - unbalanced quotes"
            test_passed = .false.
        else
            print *, "PASS: JSON structure validation completed"
        end if
        
        ! Clean up
        deallocate(dataset%files(1)%lines)
        deallocate(dataset%files(1)%functions)
        deallocate(dataset%files)
    end subroutine test_json_structure_validation

    subroutine create_simple_test_dataset(dataset)
        type(coverage_data_t), intent(out) :: dataset
        integer :: i
        
        allocate(dataset%files(1))
        dataset%files(1)%filename = "test_simple.f90"
        
        allocate(dataset%files(1)%lines(5))
        do i = 1, 5
            dataset%files(1)%lines(i)%line_number = i
            dataset%files(1)%lines(i)%execution_count = i * 2
            dataset%files(1)%lines(i)%is_executable = (i <= 3)
        end do
        
        allocate(dataset%files(1)%functions(0))
    end subroutine create_simple_test_dataset

end program test_json_security