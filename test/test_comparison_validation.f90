program test_comparison_validation
    use coverage_model
    use json_coverage_io
    use system_diff_converter
    use iso_fortran_env
    implicit none
    
    ! Test counter
    integer :: test_count = 0
    integer :: passed_count = 0
    
    write(*, '(A)') 'Starting Comparison Validation Tests'
    write(*, '(A)') '==================================='
    
    ! Test comparison functionality for pycobertura validation
    call test_coverage_percentage_comparison()
    call test_tolerance_boundary_conditions()
    call test_realistic_coverage_data()
    
    ! Print summary
    write(*, '(A)') ''
    write(*, '(A, I0, A, I0, A)') 'Tests completed: ', passed_count, '/', test_count
    
    if (passed_count == test_count) then
        write(*, '(A)') 'All comparison tests passed!'
        call exit(0)
    else
        write(*, '(A)') 'Some tests failed!'
        call exit(1)
    end if
    
contains

    ! Given: Two coverage datasets with similar percentages
    ! When: Comparing coverage percentages with ±0.1% tolerance
    ! Then: Should correctly validate equivalence
    subroutine test_coverage_percentage_comparison()
        character(len=*), parameter :: test_name = 'Coverage Percentage Comparison'
        type(coverage_data_t) :: fortcov_data, pycobertura_equivalent_data
        real :: fortcov_rate, pycobertura_rate
        logical :: within_tolerance
        real, parameter :: tolerance = 0.001  ! 0.1% tolerance (default for pycobertura comparison)
        
        call test_start(test_name)
        
        ! Given: Coverage data representing fortcov results
        call create_fortcov_sample_data(fortcov_data)
        
        ! Given: Equivalent data representing pycobertura results (slight difference)
        call create_pycobertura_equivalent_data(pycobertura_equivalent_data)
        
        ! When: Calculating coverage rates
        call calculate_coverage_rates(fortcov_data, fortcov_rate, fortcov_rate)
        call calculate_coverage_rates(pycobertura_equivalent_data, pycobertura_rate, pycobertura_rate)
        
        ! When: Comparing with tolerance
        call check_numerical_tolerance(fortcov_rate, pycobertura_rate, tolerance, within_tolerance)
        
        ! Then: Should be within tolerance (simulating real-world scenario)
        if (within_tolerance) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Coverage percentages should be equivalent within tolerance')
        end if
        
    end subroutine test_coverage_percentage_comparison
    
    ! Given: Values at tolerance boundaries
    ! When: Testing boundary conditions
    ! Then: Should correctly handle edge cases
    subroutine test_tolerance_boundary_conditions()
        character(len=*), parameter :: test_name = 'Tolerance Boundary Conditions'
        real :: baseline_value, test_value
        logical :: within_tolerance
        real, parameter :: tolerance = 0.001  ! 0.1%
        
        call test_start(test_name)
        
        ! Test exactly at tolerance boundary
        baseline_value = 0.750000
        test_value = 0.750999  ! Exactly 0.0999% difference (within tolerance)
        
        call check_numerical_tolerance(baseline_value, test_value, tolerance, within_tolerance)
        if (.not. within_tolerance) then
            call test_fail(test_name, 'Value exactly within tolerance boundary failed')
            return
        end if
        
        ! Test just outside tolerance boundary
        test_value = 0.751001  ! Exactly 0.1001% difference (outside tolerance)
        
        call check_numerical_tolerance(baseline_value, test_value, tolerance, within_tolerance)
        if (within_tolerance) then
            call test_fail(test_name, 'Value outside tolerance boundary incorrectly passed')
            return
        end if
        
        call test_pass(test_name)
        
    end subroutine test_tolerance_boundary_conditions
    
    ! Given: Realistic coverage data with multiple files
    ! When: Processing through complete workflow
    ! Then: Should handle realistic scenarios successfully
    subroutine test_realistic_coverage_data()
        character(len=*), parameter :: test_name = 'Realistic Coverage Data Processing'
        type(coverage_data_t) :: test_data
        character(len=:), allocatable :: json_content, xml_output
        logical :: success, xml_valid
        real :: line_rate, branch_rate
        
        call test_start(test_name)
        
        ! Given: Realistic multi-file coverage data
        call create_realistic_multi_file_data(test_data)
        
        ! When: Processing through complete workflow
        call export_json_coverage(test_data, json_content)
        call convert_json_to_cobertura_xml(json_content, xml_output, success)
        
        if (.not. success) then
            call test_fail(test_name, 'JSON to XML conversion failed for realistic data')
            return
        end if
        
        call validate_cobertura_xml_schema(xml_output, xml_valid)
        if (.not. xml_valid) then
            call test_fail(test_name, 'Generated XML failed validation for realistic data')
            return
        end if
        
        ! Calculate coverage rates to ensure they're realistic
        call calculate_coverage_rates(test_data, line_rate, branch_rate)
        
        ! Then: Should produce realistic coverage rates
        if (line_rate >= 0.0 .and. line_rate <= 1.0 .and. &
            branch_rate >= 0.0 .and. branch_rate <= 1.0) then
            call test_pass(test_name)
        else
            call test_fail(test_name, 'Unrealistic coverage rates calculated')
        end if
        
    end subroutine test_realistic_coverage_data
    
    ! Helper subroutines for creating test data
    
    subroutine create_fortcov_sample_data(coverage_data)
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        
        allocate(files(1))
        allocate(lines(4))
        
        ! 75% coverage: 3 out of 4 lines covered
        call lines(1)%init('module.f90', 1, 10, .true.)
        call lines(2)%init('module.f90', 2, 5, .true.)
        call lines(3)%init('module.f90', 3, 0, .true.)  ! Uncovered
        call lines(4)%init('module.f90', 4, 15, .true.)
        
        call files(1)%init('module.f90')
        files(1)%lines = lines
        call coverage_data%init()
        coverage_data%files = files
        
    end subroutine create_fortcov_sample_data
    
    subroutine create_pycobertura_equivalent_data(coverage_data)
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        
        allocate(files(1))
        allocate(lines(4))
        
        ! Similar to fortcov but slightly different execution counts
        ! (simulating rounding differences or measurement variations)
        call lines(1)%init('module.f90', 1, 12, .true.)  ! Different count, still covered
        call lines(2)%init('module.f90', 2, 7, .true.)   ! Different count, still covered
        call lines(3)%init('module.f90', 3, 0, .true.)   ! Same - uncovered
        call lines(4)%init('module.f90', 4, 18, .true.)  ! Different count, still covered
        
        call files(1)%init('module.f90')
        files(1)%lines = lines
        call coverage_data%init()
        coverage_data%files = files
        
    end subroutine create_pycobertura_equivalent_data
    
    subroutine create_realistic_multi_file_data(coverage_data)
        type(coverage_data_t), intent(out) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines1(:), lines2(:), lines3(:)
        
        allocate(files(3))
        allocate(lines1(5))
        allocate(lines2(3))
        allocate(lines3(6))
        
        ! File 1: High coverage module
        call lines1(1)%init('math_utils.f90', 5, 25, .true.)
        call lines1(2)%init('math_utils.f90', 8, 18, .true.)
        call lines1(3)%init('math_utils.f90', 12, 30, .true.)
        call lines1(4)%init('math_utils.f90', 15, 0, .true.)  ! One uncovered line
        call lines1(5)%init('math_utils.f90', 20, 22, .true.)
        call files(1)%init('math_utils.f90')
        files(1)%lines = lines1
        
        ! File 2: Medium coverage module
        call lines2(1)%init('io_utils.f90', 3, 8, .true.)
        call lines2(2)%init('io_utils.f90', 7, 0, .true.)    ! Uncovered
        call lines2(3)%init('io_utils.f90', 10, 12, .true.)
        call files(2)%init('io_utils.f90')
        files(2)%lines = lines2
        
        ! File 3: Lower coverage module
        call lines3(1)%init('string_utils.f90', 2, 5, .true.)
        call lines3(2)%init('string_utils.f90', 4, 0, .true.)  ! Uncovered
        call lines3(3)%init('string_utils.f90', 6, 0, .true.)  ! Uncovered
        call lines3(4)%init('string_utils.f90', 8, 3, .true.)
        call lines3(5)%init('string_utils.f90', 11, 0, .true.) ! Uncovered
        call lines3(6)%init('string_utils.f90', 15, 7, .true.)
        call files(3)%init('string_utils.f90')
        files(3)%lines = lines3
        
        call coverage_data%init()
        coverage_data%files = files
        
    end subroutine create_realistic_multi_file_data
    
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

end program test_comparison_validation