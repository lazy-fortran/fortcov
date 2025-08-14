module test_diff_data_generation
    use coverage_model
    use json_coverage_io
    implicit none
    private
    
    ! Public procedures for generating test data
    public :: generate_baseline_coverage_data
    public :: generate_current_coverage_data  
    public :: generate_empty_coverage_data
    public :: generate_identical_coverage_data
    public :: generate_realistic_project_coverage
    public :: generate_large_project_coverage
    public :: generate_baseline_json_file
    public :: generate_current_json_file
    public :: create_diff_test_scenario
    public :: validate_generated_data
    
    ! Test scenario types
    integer, parameter, public :: SCENARIO_BASIC_IMPROVEMENT = 1
    integer, parameter, public :: SCENARIO_REGRESSION = 2
    integer, parameter, public :: SCENARIO_MIXED_CHANGES = 3
    integer, parameter, public :: SCENARIO_NEW_FILES = 4
    integer, parameter, public :: SCENARIO_REMOVED_FILES = 5
    integer, parameter, public :: SCENARIO_IDENTICAL = 6
    integer, parameter, public :: SCENARIO_LARGE_PROJECT = 7

contains

    function generate_baseline_coverage_data() result(coverage_data)
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: files(3)
        type(coverage_line_t) :: lines1(5), lines2(4), lines3(6)
        
        ! Given: Baseline coverage data with moderate coverage
        
        ! File 1: main.f90 - 60% coverage (3/5 lines)
        call lines1(1)%init(3, 1, "src/main.f90", .true.)      ! covered
        call lines1(2)%init(0, 2, "src/main.f90", .true.)      ! uncovered
        call lines1(3)%init(5, 3, "src/main.f90", .true.)      ! covered
        call lines1(4)%init(0, 4, "src/main.f90", .true.)      ! uncovered  
        call lines1(5)%init(2, 5, "src/main.f90", .true.)      ! covered
        call files(1)%init("src/main.f90", lines1)
        
        ! File 2: utils.f90 - 75% coverage (3/4 lines)
        call lines2(1)%init(1, 10, "src/utils.f90", .true.)    ! covered
        call lines2(2)%init(4, 11, "src/utils.f90", .true.)    ! covered
        call lines2(3)%init(0, 12, "src/utils.f90", .true.)    ! uncovered
        call lines2(4)%init(2, 13, "src/utils.f90", .true.)    ! covered
        call files(2)%init("src/utils.f90", lines2)
        
        ! File 3: math.f90 - 50% coverage (3/6 lines)
        call lines3(1)%init(0, 20, "src/math.f90", .true.)     ! uncovered
        call lines3(2)%init(3, 21, "src/math.f90", .true.)     ! covered
        call lines3(3)%init(0, 22, "src/math.f90", .true.)     ! uncovered
        call lines3(4)%init(1, 23, "src/math.f90", .true.)     ! covered
        call lines3(5)%init(0, 24, "src/math.f90", .true.)     ! uncovered
        call lines3(6)%init(5, 25, "src/math.f90", .true.)     ! covered
        call files(3)%init("src/math.f90", lines3)
        
        call coverage_data%init(files)
    end function generate_baseline_coverage_data

    function generate_current_coverage_data() result(coverage_data)
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: files(3)
        type(coverage_line_t) :: lines1(5), lines2(4), lines3(6)
        
        ! Given: Current coverage data with improvements and regressions
        
        ! File 1: main.f90 - 80% coverage (4/5 lines) - IMPROVEMENT
        call lines1(1)%init(4, 1, "src/main.f90", .true.)      ! improved
        call lines1(2)%init(2, 2, "src/main.f90", .true.)      ! newly covered  
        call lines1(3)%init(6, 3, "src/main.f90", .true.)      ! improved
        call lines1(4)%init(0, 4, "src/main.f90", .true.)      ! still uncovered
        call lines1(5)%init(3, 5, "src/main.f90", .true.)      ! improved
        call files(1)%init("src/main.f90", lines1)
        
        ! File 2: utils.f90 - 50% coverage (2/4 lines) - REGRESSION
        call lines2(1)%init(0, 10, "src/utils.f90", .true.)    ! newly uncovered
        call lines2(2)%init(5, 11, "src/utils.f90", .true.)    ! improved
        call lines2(3)%init(0, 12, "src/utils.f90", .true.)    ! still uncovered
        call lines2(4)%init(1, 13, "src/utils.f90", .true.)    ! reduced
        call files(2)%init("src/utils.f90", lines2)
        
        ! File 3: math.f90 - 83% coverage (5/6 lines) - MAJOR IMPROVEMENT
        call lines3(1)%init(2, 20, "src/math.f90", .true.)     ! newly covered
        call lines3(2)%init(4, 21, "src/math.f90", .true.)     ! improved
        call lines3(3)%init(1, 22, "src/math.f90", .true.)     ! newly covered
        call lines3(4)%init(3, 23, "src/math.f90", .true.)     ! improved
        call lines3(5)%init(0, 24, "src/math.f90", .true.)     ! still uncovered
        call lines3(6)%init(7, 25, "src/math.f90", .true.)     ! improved
        call files(3)%init("src/math.f90", lines3)
        
        call coverage_data%init(files)
    end function generate_current_coverage_data

    function generate_empty_coverage_data() result(coverage_data)
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: empty_files(0)
        
        ! Given: Empty coverage data for edge case testing
        call coverage_data%init(empty_files)
    end function generate_empty_coverage_data

    function generate_identical_coverage_data() result(coverage_data)
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t) :: files(2)
        type(coverage_line_t) :: lines1(3), lines2(3)
        
        ! Given: Identical baseline and current data
        
        ! File 1: identical coverage
        call lines1(1)%init(5, 1, "test/identical.f90", .true.)
        call lines1(2)%init(0, 2, "test/identical.f90", .true.)
        call lines1(3)%init(3, 3, "test/identical.f90", .true.)
        call files(1)%init("test/identical.f90", lines1)
        
        ! File 2: identical coverage  
        call lines2(1)%init(2, 10, "test/same.f90", .true.)
        call lines2(2)%init(4, 11, "test/same.f90", .true.)
        call lines2(3)%init(1, 12, "test/same.f90", .true.)
        call files(2)%init("test/same.f90", lines2)
        
        call coverage_data%init(files)
    end function generate_identical_coverage_data

    function generate_realistic_project_coverage(scenario_type, total_files) result(coverage_data)
        integer, intent(in) :: scenario_type
        integer, intent(in) :: total_files
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t) :: lines(50)  ! Standard file size
        integer :: i, j
        character(len=50) :: filename
        integer :: base_coverage, current_coverage
        
        allocate(files(total_files))
        
        do i = 1, total_files
            write(filename, '(A,I0,A)') "src/module_", i, ".f90"
            
            ! Generate lines based on scenario type
            do j = 1, 50
                select case (scenario_type)
                case (SCENARIO_BASIC_IMPROVEMENT)
                    ! Baseline: moderate coverage, Current: improved
                    base_coverage = merge(j, 0, mod(j, 3) /= 0)
                    current_coverage = merge(j + 1, 0, mod(j, 2) /= 0)
                    
                case (SCENARIO_REGRESSION)
                    ! Baseline: good coverage, Current: degraded
                    base_coverage = merge(j + 2, 0, mod(j, 2) /= 0)
                    current_coverage = merge(j, 0, mod(j, 3) /= 0)
                    
                case (SCENARIO_MIXED_CHANGES)
                    ! Mixed improvements and regressions
                    if (mod(i + j, 2) == 0) then
                        base_coverage = merge(j, 0, mod(j, 4) /= 0)
                        current_coverage = base_coverage + 1
                    else
                        base_coverage = merge(j + 1, 0, mod(j, 3) /= 0)
                        current_coverage = merge(j, 0, mod(j, 5) /= 0)
                    end if
                    
                case default
                    base_coverage = j
                    current_coverage = j
                end select
                
                call lines(j)%init(base_coverage, j, filename, .true.)
            end do
            
            call files(i)%init(filename, lines)
        end do
        
        call coverage_data%init(files)
        deallocate(files)
    end function generate_realistic_project_coverage

    function generate_large_project_coverage(num_files, lines_per_file) result(coverage_data)
        integer, intent(in) :: num_files
        integer, intent(in) :: lines_per_file
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t), allocatable :: files(:)
        type(coverage_line_t), allocatable :: lines(:)
        integer :: i, j
        character(len=60) :: filename
        
        allocate(files(num_files))
        allocate(lines(lines_per_file))
        
        do i = 1, num_files
            write(filename, '(A,I0,A)') "large_project/src/component_", i, ".f90"
            
            do j = 1, lines_per_file
                ! Create realistic coverage pattern
                if (mod(j, 10) == 0) then
                    ! 10% uncovered lines
                    call lines(j)%init(0, j, filename, .true.)
                else if (mod(j, 5) == 0) then
                    ! 20% lightly covered lines  
                    call lines(j)%init(1, j, filename, .true.)
                else if (mod(j, 3) == 0) then
                    ! 30% moderately covered lines
                    call lines(j)%init(j / 3, j, filename, .true.)
                else
                    ! 40% well covered lines
                    call lines(j)%init(j, j, filename, .true.)
                end if
            end do
            
            call files(i)%init(filename, lines)
        end do
        
        call coverage_data%init(files)
        deallocate(files)
        deallocate(lines)
    end function generate_large_project_coverage

    subroutine generate_baseline_json_file(filename, coverage_data)
        character(len=*), intent(in) :: filename
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: json_content
        integer :: unit, iostat
        
        ! Given: Coverage data to export as JSON baseline
        call export_json_coverage(coverage_data, json_content)
        
        ! When: Writing to file
        open(newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') json_content
            close(unit)
        end if
    end subroutine generate_baseline_json_file

    subroutine generate_current_json_file(filename, coverage_data)
        character(len=*), intent(in) :: filename
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: json_content
        integer :: unit, iostat
        
        ! Given: Coverage data to export as JSON current
        call export_json_coverage(coverage_data, json_content)
        
        ! When: Writing to file
        open(newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') json_content
            close(unit)
        end if
    end subroutine generate_current_json_file

    subroutine create_diff_test_scenario(scenario_type, baseline_file, current_file)
        integer, intent(in) :: scenario_type
        character(len=*), intent(in) :: baseline_file
        character(len=*), intent(in) :: current_file
        type(coverage_data_t) :: baseline_data, current_data
        
        ! Given: Specific diff scenario type
        select case (scenario_type)
        case (SCENARIO_BASIC_IMPROVEMENT)
            baseline_data = generate_baseline_coverage_data()
            current_data = generate_current_coverage_data()
            
        case (SCENARIO_REGRESSION)
            current_data = generate_baseline_coverage_data()  ! Swap for regression
            baseline_data = generate_current_coverage_data()
            
        case (SCENARIO_MIXED_CHANGES)
            baseline_data = generate_realistic_project_coverage(SCENARIO_MIXED_CHANGES, 5)
            current_data = generate_realistic_project_coverage(SCENARIO_MIXED_CHANGES, 5)
            
        case (SCENARIO_NEW_FILES)
            baseline_data = generate_baseline_coverage_data()
            current_data = generate_realistic_project_coverage(SCENARIO_BASIC_IMPROVEMENT, 5)
            
        case (SCENARIO_REMOVED_FILES)
            baseline_data = generate_realistic_project_coverage(SCENARIO_BASIC_IMPROVEMENT, 5)
            current_data = generate_baseline_coverage_data()
            
        case (SCENARIO_IDENTICAL)
            baseline_data = generate_identical_coverage_data()
            current_data = generate_identical_coverage_data()
            
        case (SCENARIO_LARGE_PROJECT)
            baseline_data = generate_large_project_coverage(20, 100)
            current_data = generate_large_project_coverage(20, 100)
            
        case default
            baseline_data = generate_baseline_coverage_data()
            current_data = generate_current_coverage_data()
        end select
        
        ! When: Creating JSON files for scenario
        call generate_baseline_json_file(baseline_file, baseline_data)
        call generate_current_json_file(current_file, current_data)
    end subroutine create_diff_test_scenario

    function validate_generated_data(coverage_data) result(is_valid)
        type(coverage_data_t), intent(in) :: coverage_data
        logical :: is_valid
        integer :: i, j
        
        is_valid = .true.
        
        ! Given: Generated coverage data
        ! When: Validating data integrity
        
        ! Check if files are allocated
        if (.not. allocated(coverage_data%files)) then
            is_valid = .false.
            return
        end if
        
        ! Validate each file
        do i = 1, size(coverage_data%files)
            ! Check filename is not empty
            if (len_trim(coverage_data%files(i)%filename) == 0) then
                is_valid = .false.
                return
            end if
            
            ! Check if lines are allocated
            if (.not. allocated(coverage_data%files(i)%lines)) then
                is_valid = .false.
                return
            end if
            
            ! Validate each line
            do j = 1, size(coverage_data%files(i)%lines)
                ! Check line number is positive
                if (coverage_data%files(i)%lines(j)%line_number <= 0) then
                    is_valid = .false.
                    return
                end if
                
                ! Check execution count is non-negative
                if (coverage_data%files(i)%lines(j)%execution_count < 0) then
                    is_valid = .false.
                    return
                end if
                
                ! Check filename consistency
                if (coverage_data%files(i)%lines(j)%filename /= &
                    coverage_data%files(i)%filename) then
                    is_valid = .false.
                    return
                end if
            end do
        end do
        
        ! Then: Data should be valid
    end function validate_generated_data

end module test_diff_data_generation