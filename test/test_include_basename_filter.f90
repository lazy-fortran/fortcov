program test_include_basename_filter
    !! Ensure basename-only filenames are included by source include filters
    use coverage_model_core, only: coverage_data_t, coverage_file_t, coverage_line_t
    use coverage_data_filter, only: apply_filter_criteria
    use report_config_core, only: filter_criteria_t
    implicit none

    logical :: test_passed
    type(coverage_data_t) :: input_data, filtered_data
    type(coverage_file_t) :: file
    type(coverage_line_t), allocatable :: lines(:)
    type(filter_criteria_t) :: criteria
    logical :: success
    character(len=:), allocatable :: error_msg

    test_passed = .true.

    ! Build a coverage file with a basename-only filename (as gcov may report)
    allocate(lines(2))
    call lines(1)%init("demo_calculator.f90", 6, 1, .true.)
    call lines(2)%init("demo_calculator.f90", 7, 1, .true.)
    call file%init("demo_calculator.f90", lines)

    call input_data%init()
    if (allocated(input_data%files)) then
        deallocate(input_data%files)
    end if
    allocate(input_data%files(1))
    input_data%files(1) = file

    ! Set include pattern derived from a source directory
    call criteria%init()
    if (allocated(criteria%include_patterns)) deallocate(criteria%include_patterns)
    allocate(character(len=256) :: criteria%include_patterns(1))
    criteria%include_patterns(1) = "examples/build_systems/fpm/basic_example/src/*"

    call apply_filter_criteria(input_data, criteria, filtered_data, success, error_msg)
    if (.not. success) then
        print *, "Failed to apply filter: ", trim(error_msg)
        test_passed = .false.
    else
        if (.not. allocated(filtered_data%files)) then
            print *, "Filtered data has no files"
            test_passed = .false.
        else if (size(filtered_data%files) /= 1) then
            print *, "Expected 1 file after filtering, got", size(filtered_data%files)
            test_passed = .false.
        else if (trim(filtered_data%files(1)%filename) /= "demo_calculator.f90") then
            print *, "Unexpected filename after filtering: ", trim(filtered_data%files(1)%filename)
            test_passed = .false.
        end if
    end if

    if (test_passed) then
        print *, "All tests passed"
        stop 0
    else
        print *, "failed"
        stop 1
    end if
end program test_include_basename_filter
