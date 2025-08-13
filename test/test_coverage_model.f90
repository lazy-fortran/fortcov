program test_coverage_model
    use coverage_model
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Coverage Data Model..."
    
    ! Test 1: Create and access line coverage data
    all_tests_passed = all_tests_passed .and. test_line_coverage_basic()
    
    ! Test 2: Create uncovered line
    all_tests_passed = all_tests_passed .and. test_uncovered_line()
    
    ! Test 3: Branch coverage calculation
    all_tests_passed = all_tests_passed .and. test_branch_coverage()
    
    ! Test 4: Function coverage with Fortran module info
    all_tests_passed = all_tests_passed .and. test_module_procedure()
    
    ! Test 5: File coverage aggregation
    all_tests_passed = all_tests_passed .and. test_file_coverage_percentage()
    
    ! Test 6: Coverage data serialization round-trip
    all_tests_passed = all_tests_passed .and. test_serialization_roundtrip()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_line_coverage_basic() result(passed)
        logical :: passed
        type(coverage_line_t) :: line_cov
        
        print *, "  Test 1: Create and access line coverage data"
        
        ! Initialize line coverage with execution_count=5 and line_number=42
        line_cov = coverage_line_t(execution_count=5, line_number=42, &
                                   filename="test.f90", is_executable=.true.)
        
        ! Test the expected values
        passed = (line_cov%execution_count == 5) .and. &
                 (line_cov%line_number == 42) .and. &
                 line_cov%is_covered()
        
        if (.not. passed) then
            print *, "    FAILED: Expected execution_count=5, line_number=42, is_covered=true"
        else
            print *, "    PASSED"
        end if
    end function test_line_coverage_basic

    function test_uncovered_line() result(passed)
        logical :: passed
        type(coverage_line_t) :: line_cov
        
        print *, "  Test 2: Create uncovered line"
        
        ! Initialize line coverage with execution_count=0
        line_cov = coverage_line_t(execution_count=0, line_number=10, &
                                   filename="test.f90", is_executable=.true.)
        
        ! Test that is_covered returns false for zero execution count
        passed = .not. line_cov%is_covered()
        
        if (.not. passed) then
            print *, "    FAILED: Expected is_covered=false for execution_count=0"
        else
            print *, "    PASSED"
        end if
    end function test_uncovered_line

    function test_branch_coverage() result(passed)
        logical :: passed
        type(coverage_branch_t) :: branch_cov
        
        print *, "  Test 3: Branch coverage calculation"
        
        ! Initialize branch with taken_count=3, not_taken_count=0
        branch_cov = coverage_branch_t(taken_count=3, not_taken_count=0, &
                                       branch_id=1, line_number=15, &
                                       filename="test.f90")
        
        ! Test partial coverage (taken but not all branches)
        passed = branch_cov%is_partially_covered() .and. &
                 .not. branch_cov%is_fully_covered()
        
        if (.not. passed) then
            print *, "    FAILED: Expected is_partially_covered=true, ", &
                        "is_fully_covered=false"
        else
            print *, "    PASSED"
        end if
    end function test_branch_coverage

    function test_module_procedure() result(passed)
        logical :: passed
        type(coverage_function_t) :: func_cov
        
        print *, "  Test 4: Function coverage with Fortran module info"
        
        ! Initialize function as module procedure
        func_cov = coverage_function_t(name="my_subroutine", &
                                       parent_module="my_module", &
                                       is_module_procedure=.true., &
                                       execution_count=2, &
                                       line_number=20, &
                                       filename="my_module.f90")
        
        ! Test module procedure identification
        passed = func_cov%is_module_procedure .and. &
                 (func_cov%parent_module == "my_module")
        
        if (.not. passed) then
            print *, "    FAILED: Expected is_module_procedure=true, ", &
                        "parent_module='my_module'"
        else
            print *, "    PASSED"
        end if
    end function test_module_procedure

    function test_file_coverage_percentage() result(passed)
        logical :: passed
        type(coverage_file_t) :: file_cov
        type(coverage_line_t), allocatable :: lines(:)
        real :: expected_percentage, actual_percentage
        integer :: i
        
        print *, "  Test 5: File coverage aggregation"
        
        ! Create 10 lines: 7 covered, 3 uncovered
        allocate(lines(10))
        do i = 1, 7
            lines(i) = coverage_line_t(execution_count=1, line_number=i, &
                                       filename="test.f90", is_executable=.true.)
        end do
        do i = 8, 10
            lines(i) = coverage_line_t(execution_count=0, line_number=i, &
                                       filename="test.f90", is_executable=.true.)
        end do
        
        file_cov = coverage_file_t(filename="test.f90", lines=lines)
        
        expected_percentage = 70.0
        actual_percentage = file_cov%get_line_coverage_percentage()
        
        ! Allow small floating point tolerance
        passed = abs(actual_percentage - expected_percentage) < 0.01
        
        if (.not. passed) then
            print *, "    FAILED: Expected 70.0%, got ", actual_percentage, "%"
        else
            print *, "    PASSED"
        end if
    end function test_file_coverage_percentage

    function test_serialization_roundtrip() result(passed)
        logical :: passed
        type(coverage_data_t) :: original, deserialized
        character(len=:), allocatable :: serialized
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        
        print *, "  Test 6: Coverage data serialization round-trip"
        
        ! Create minimal coverage data
        allocate(lines(2))
        lines(1) = coverage_line_t(execution_count=5, line_number=1, &
                                   filename="test.f90", is_executable=.true.)
        lines(2) = coverage_line_t(execution_count=0, line_number=2, &
                                   filename="test.f90", is_executable=.true.)
        
        allocate(files(1))
        files(1) = coverage_file_t(filename="test.f90", lines=lines)
        
        original = coverage_data_t(files=files)
        
        ! Serialize and deserialize
        serialized = original%serialize()
        deserialized = coverage_data_t()
        call deserialized%deserialize(serialized)
        
        ! Test basic structure preservation
        passed = (size(deserialized%files) == 1) .and. &
                 (deserialized%files(1)%filename == "test.f90") .and. &
                 (size(deserialized%files(1)%lines) == 2) .and. &
                 (deserialized%files(1)%lines(1)%execution_count == 5)
        
        if (.not. passed) then
            print *, "    FAILED: Serialization round-trip failed"
        else
            print *, "    PASSED"
        end if
    end function test_serialization_roundtrip

end program test_coverage_model