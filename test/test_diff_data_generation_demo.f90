program test_diff_data_generation_demo
    use test_diff_data_generation
    use coverage_model
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "Testing Coverage Diff Data Generation..."
    
    ! Test data generation functions
    all_tests_passed = all_tests_passed .and. test_baseline_data_generation()
    all_tests_passed = all_tests_passed .and. test_current_data_generation()
    all_tests_passed = all_tests_passed .and. test_empty_data_generation()
    all_tests_passed = all_tests_passed .and. test_identical_data_generation()
    all_tests_passed = all_tests_passed .and. test_realistic_project_generation()
    all_tests_passed = all_tests_passed .and. test_large_project_generation()
    
    ! Test scenario creation
    all_tests_passed = all_tests_passed .and. test_scenario_creation()
    all_tests_passed = all_tests_passed .and. test_json_file_generation()
    all_tests_passed = all_tests_passed .and. test_data_validation()
    
    ! Test all scenario types
    all_tests_passed = all_tests_passed .and. test_all_scenario_types()
    
    if (all_tests_passed) then
        print *, "All tests PASSED"
        call exit(0)
    else
        print *, "Some tests FAILED"
        call exit(1)
    end if

contains

    function test_baseline_data_generation() result(passed)
        logical :: passed
        type(coverage_data_t) :: baseline_data
        
        passed = .false.
        
        ! Given: Request for baseline coverage data
        ! When: Generating baseline data
        baseline_data = generate_baseline_coverage_data()
        
        ! Then: Should create valid baseline data with expected structure
        if (allocated(baseline_data%files) .and. &
            size(baseline_data%files) == 3 .and. &
            baseline_data%files(1)%filename == "src/main.f90" .and. &
            baseline_data%files(2)%filename == "src/utils.f90" .and. &
            baseline_data%files(3)%filename == "src/math.f90" .and. &
            validate_generated_data(baseline_data)) then
            passed = .true.
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_baseline_data_generation - baseline data generation failed"
        end if
    end function test_baseline_data_generation

    function test_current_data_generation() result(passed)
        logical :: passed
        type(coverage_data_t) :: current_data
        
        passed = .false.
        
        ! Given: Request for current coverage data
        ! When: Generating current data
        current_data = generate_current_coverage_data()
        
        ! Then: Should create valid current data with improvements
        if (allocated(current_data%files) .and. &
            size(current_data%files) == 3 .and. &
            validate_generated_data(current_data)) then
            
            ! Check that some lines show improvement (execution count > 0 where baseline was 0)
            if (current_data%files(1)%lines(2)%execution_count > 0 .and. &
                current_data%files(3)%lines(1)%execution_count > 0) then
                passed = .true.
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_current_data_generation - current data generation failed"
        end if
    end function test_current_data_generation

    function test_empty_data_generation() result(passed)
        logical :: passed
        type(coverage_data_t) :: empty_data
        
        passed = .false.
        
        ! Given: Request for empty coverage data
        ! When: Generating empty data
        empty_data = generate_empty_coverage_data()
        
        ! Then: Should create valid empty data structure
        if (allocated(empty_data%files) .and. &
            size(empty_data%files) == 0) then
            passed = .true.
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_empty_data_generation - empty data generation failed"
        end if
    end function test_empty_data_generation

    function test_identical_data_generation() result(passed)
        logical :: passed
        type(coverage_data_t) :: identical_data
        
        passed = .false.
        
        ! Given: Request for identical coverage data
        ! When: Generating identical data
        identical_data = generate_identical_coverage_data()
        
        ! Then: Should create valid identical data
        if (allocated(identical_data%files) .and. &
            size(identical_data%files) == 2 .and. &
            validate_generated_data(identical_data)) then
            passed = .true.
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_identical_data_generation - identical data generation failed"
        end if
    end function test_identical_data_generation

    function test_realistic_project_generation() result(passed)
        logical :: passed
        type(coverage_data_t) :: project_data
        
        passed = .false.
        
        ! Given: Request for realistic project data
        ! When: Generating realistic project with 5 files
        project_data = generate_realistic_project_coverage(SCENARIO_BASIC_IMPROVEMENT, 5)
        
        ! Then: Should create valid project data
        if (allocated(project_data%files) .and. &
            size(project_data%files) == 5 .and. &
            validate_generated_data(project_data)) then
            
            ! Check that files have expected line counts
            if (size(project_data%files(1)%lines) == 50) then
                passed = .true.
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_realistic_project_generation - realistic project generation failed"
        end if
    end function test_realistic_project_generation

    function test_large_project_generation() result(passed)
        logical :: passed
        type(coverage_data_t) :: large_data
        
        passed = .false.
        
        ! Given: Request for large project data
        ! When: Generating large project with 10 files, 200 lines each
        large_data = generate_large_project_coverage(10, 200)
        
        ! Then: Should create valid large project data
        if (allocated(large_data%files) .and. &
            size(large_data%files) == 10 .and. &
            validate_generated_data(large_data)) then
            
            ! Check file and line counts
            if (size(large_data%files(1)%lines) == 200 .and. &
                size(large_data%files(10)%lines) == 200) then
                passed = .true.
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_large_project_generation - large project generation failed"
        end if
    end function test_large_project_generation

    function test_scenario_creation() result(passed)
        logical :: passed
        
        passed = .false.
        
        ! Given: Request to create specific diff scenario
        ! When: Creating basic improvement scenario
        call create_diff_test_scenario(SCENARIO_BASIC_IMPROVEMENT, &
                                     "test_baseline.json", "test_current.json")
        
        ! Then: Should create scenario files (check if files exist)
        block
            logical :: baseline_exists, current_exists
            inquire(file="test_baseline.json", exist=baseline_exists)
            inquire(file="test_current.json", exist=current_exists)
            
            if (baseline_exists .and. current_exists) then
                passed = .true.
            end if
        end block
        
        if (.not. passed) then
            print *, "FAIL: test_scenario_creation - scenario file creation failed"
        end if
    end function test_scenario_creation

    function test_json_file_generation() result(passed)
        logical :: passed
        type(coverage_data_t) :: test_data
        character(len=100) :: test_content
        integer :: unit, iostat
        
        passed = .false.
        
        ! Given: Coverage data to export
        test_data = generate_baseline_coverage_data()
        
        ! When: Generating JSON file
        call generate_baseline_json_file("test_json_export.json", test_data)
        
        ! Then: Should create readable JSON file
        inquire(file="test_json_export.json", exist=passed)
        
        if (passed) then
            ! Try to read first line to verify content
            open(newunit=unit, file="test_json_export.json", status='old', &
                 action='read', iostat=iostat)
            if (iostat == 0) then
                read(unit, '(A)', iostat=iostat) test_content
                close(unit)
                if (iostat == 0 .and. index(test_content, "files") > 0) then
                    passed = .true.
                else
                    passed = .false.
                end if
            else
                passed = .false.
            end if
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_json_file_generation - JSON file generation failed"
        end if
    end function test_json_file_generation

    function test_data_validation() result(passed)
        logical :: passed
        type(coverage_data_t) :: valid_data, empty_data
        
        passed = .false.
        
        ! Given: Valid and empty coverage data
        valid_data = generate_baseline_coverage_data()
        empty_data = generate_empty_coverage_data()
        
        ! When: Validating both datasets
        ! Then: Should correctly identify valid and empty data
        if (validate_generated_data(valid_data) .and. &
            validate_generated_data(empty_data)) then
            passed = .true.
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_data_validation - data validation failed"
        end if
    end function test_data_validation

    function test_all_scenario_types() result(passed)
        logical :: passed
        integer :: scenario
        character(len=30) :: baseline_file, current_file
        logical :: all_scenarios_valid
        
        passed = .false.
        all_scenarios_valid = .true.
        
        ! Given: All scenario types
        ! When: Creating each scenario type
        do scenario = SCENARIO_BASIC_IMPROVEMENT, SCENARIO_LARGE_PROJECT
            write(baseline_file, '(A,I0,A)') "baseline_", scenario, ".json"
            write(current_file, '(A,I0,A)') "current_", scenario, ".json"
            
            call create_diff_test_scenario(scenario, baseline_file, current_file)
            
            ! Check if files were created
            block
                logical :: baseline_exists, current_exists
                inquire(file=baseline_file, exist=baseline_exists)
                inquire(file=current_file, exist=current_exists)
                
                if (.not. (baseline_exists .and. current_exists)) then
                    all_scenarios_valid = .false.
                    print *, "Scenario", scenario, "failed to create files"
                end if
            end block
        end do
        
        ! Then: All scenarios should be created successfully
        if (all_scenarios_valid) then
            passed = .true.
        end if
        
        if (.not. passed) then
            print *, "FAIL: test_all_scenario_types - not all scenario types working"
        end if
    end function test_all_scenario_types

end program test_diff_data_generation_demo