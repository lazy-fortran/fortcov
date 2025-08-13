module coverage_statistics
    use coverage_model
    use string_utils
    implicit none
    private
    
    ! Public types
    public :: coverage_stats_t
    public :: module_stats_t
    
    ! Public procedures
    public :: calculate_line_coverage
    public :: calculate_branch_coverage
    public :: calculate_function_coverage
    public :: calculate_module_coverage
    
    ! Statistics result type
    type :: coverage_stats_t
        real(8) :: percentage = 0.0_8
        integer :: covered_count = 0
        integer :: total_count = 0
        character(len=:), allocatable :: missing_ranges
    contains
        procedure :: init => stats_init
    end type coverage_stats_t
    
    ! Module-specific statistics type
    type :: module_stats_t
        character(len=:), allocatable :: module_name
        real(8) :: line_percentage = 0.0_8
        real(8) :: function_percentage = 0.0_8
        real(8) :: branch_percentage = 0.0_8
        integer :: total_lines = 0
        integer :: covered_lines = 0
        integer :: total_functions = 0
        integer :: covered_functions = 0
        integer :: total_branches = 0
        integer :: covered_branches = 0
    contains
        procedure :: init => module_stats_init
    end type module_stats_t
    
contains

    ! Initialize coverage statistics
    subroutine stats_init(this, percentage, covered_count, total_count, &
                         missing_ranges)
        class(coverage_stats_t), intent(inout) :: this
        real(8), intent(in) :: percentage
        integer, intent(in) :: covered_count
        integer, intent(in) :: total_count
        character(len=*), intent(in), optional :: missing_ranges
        
        this%percentage = percentage
        this%covered_count = covered_count
        this%total_count = total_count
        
        if (present(missing_ranges)) then
            this%missing_ranges = missing_ranges
        else
            this%missing_ranges = ""
        end if
    end subroutine stats_init

    ! Initialize module statistics
    subroutine module_stats_init(this, module_name, line_percentage, &
                                function_percentage, branch_percentage, &
                                total_lines, covered_lines, &
                                total_functions, covered_functions, &
                                total_branches, covered_branches)
        class(module_stats_t), intent(inout) :: this
        character(len=*), intent(in) :: module_name
        real(8), intent(in) :: line_percentage
        real(8), intent(in) :: function_percentage
        real(8), intent(in) :: branch_percentage
        integer, intent(in) :: total_lines, covered_lines
        integer, intent(in) :: total_functions, covered_functions
        integer, intent(in) :: total_branches, covered_branches
        
        this%module_name = module_name
        this%line_percentage = line_percentage
        this%function_percentage = function_percentage
        this%branch_percentage = branch_percentage
        this%total_lines = total_lines
        this%covered_lines = covered_lines
        this%total_functions = total_functions
        this%covered_functions = covered_functions
        this%total_branches = total_branches
        this%covered_branches = covered_branches
    end subroutine module_stats_init

    ! Calculate line coverage percentage across all files
    function calculate_line_coverage(coverage_data) result(stats)
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_stats_t) :: stats
        integer :: file_idx, line_idx
        integer :: total_executable, total_covered
        integer, allocatable :: uncovered_lines(:)
        integer :: uncovered_count
        
        total_executable = 0
        total_covered = 0
        uncovered_count = 0
        
        ! First pass: count totals and identify uncovered lines
        do file_idx = 1, size(coverage_data%files)
            do line_idx = 1, size(coverage_data%files(file_idx)%lines)
                associate(line => coverage_data%files(file_idx)%lines(line_idx))
                    if (line%is_executable) then
                        total_executable = total_executable + 1
                        if (line%is_covered()) then
                            total_covered = total_covered + 1
                        else
                            uncovered_count = uncovered_count + 1
                        end if
                    end if
                end associate
            end do
        end do
        
        ! Second pass: collect uncovered line numbers
        if (uncovered_count > 0) then
            allocate(uncovered_lines(uncovered_count))
            uncovered_count = 0  ! Reset for second pass
            do file_idx = 1, size(coverage_data%files)
                do line_idx = 1, size(coverage_data%files(file_idx)%lines)
                    associate(line => coverage_data%files(file_idx)%lines(line_idx))
                        if (line%is_executable .and. .not. line%is_covered()) then
                            uncovered_count = uncovered_count + 1
                            uncovered_lines(uncovered_count) = line%line_number
                        end if
                    end associate
                end do
            end do
        else
            allocate(uncovered_lines(0))
        end if
        
        ! Calculate percentage
        if (total_executable == 0) then
            call stats%init(100.0_8, 0, 0, "")
        else
            call stats%init((real(total_covered, 8) / real(total_executable, 8)) * 100.0_8, &
                           total_covered, total_executable, &
                           compress_ranges(uncovered_lines))
        end if
        
        deallocate(uncovered_lines)
    end function calculate_line_coverage

    ! Calculate branch coverage percentage
    function calculate_branch_coverage(coverage_data) result(stats)
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_stats_t) :: stats
        integer :: file_idx, func_idx, branch_idx
        integer :: total_branches, fully_covered_branches
        integer, allocatable :: uncovered_branches(:)
        integer :: uncovered_count
        
        total_branches = 0
        fully_covered_branches = 0
        uncovered_count = 0
        
        ! Count branches and coverage
        do file_idx = 1, size(coverage_data%files)
            if (allocated(coverage_data%files(file_idx)%functions)) then
                do func_idx = 1, size(coverage_data%files(file_idx)%functions)
                    if (allocated(coverage_data%files(file_idx)%functions(func_idx)%branches)) then
                        do branch_idx = 1, size(coverage_data%files(file_idx)%functions(func_idx)%branches)
                            associate(branch => coverage_data%files(file_idx)%functions(func_idx)%branches(branch_idx))
                                total_branches = total_branches + 1
                                if (branch%is_fully_covered()) then
                                    fully_covered_branches = fully_covered_branches + 1
                                else
                                    uncovered_count = uncovered_count + 1
                                end if
                            end associate
                        end do
                    end if
                end do
            end if
        end do
        
        ! Collect uncovered branch line numbers
        if (uncovered_count > 0) then
            allocate(uncovered_branches(uncovered_count))
            uncovered_count = 0  ! Reset for collection
            do file_idx = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(file_idx)%functions)) then
                    do func_idx = 1, size(coverage_data%files(file_idx)%functions)
                        if (allocated(coverage_data%files(file_idx)%functions(func_idx)%branches)) then
                            do branch_idx = 1, size(coverage_data%files(file_idx)%functions(func_idx)%branches)
                                associate(branch => coverage_data%files(file_idx)%functions(func_idx)%branches(branch_idx))
                                    if (.not. branch%is_fully_covered()) then
                                        uncovered_count = uncovered_count + 1
                                        uncovered_branches(uncovered_count) = branch%line_number
                                    end if
                                end associate
                            end do
                        end if
                    end do
                end if
            end do
        else
            allocate(uncovered_branches(0))
        end if
        
        ! Calculate percentage
        if (total_branches == 0) then
            call stats%init(100.0_8, 0, 0, "")
        else
            call stats%init((real(fully_covered_branches, 8) / real(total_branches, 8)) * 100.0_8, &
                           fully_covered_branches, total_branches, &
                           compress_ranges(uncovered_branches))
        end if
        
        deallocate(uncovered_branches)
    end function calculate_branch_coverage

    ! Calculate function coverage percentage
    function calculate_function_coverage(coverage_data) result(stats)
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_stats_t) :: stats
        integer :: file_idx, func_idx
        integer :: total_functions, covered_functions
        integer, allocatable :: uncovered_functions(:)
        integer :: uncovered_count
        
        total_functions = 0
        covered_functions = 0
        uncovered_count = 0
        
        ! Count functions and coverage
        do file_idx = 1, size(coverage_data%files)
            if (allocated(coverage_data%files(file_idx)%functions)) then
                do func_idx = 1, size(coverage_data%files(file_idx)%functions)
                    associate(func => coverage_data%files(file_idx)%functions(func_idx))
                        total_functions = total_functions + 1
                        if (func%execution_count > 0) then
                            covered_functions = covered_functions + 1
                        else
                            uncovered_count = uncovered_count + 1
                        end if
                    end associate
                end do
            end if
        end do
        
        ! Collect uncovered function line numbers
        if (uncovered_count > 0) then
            allocate(uncovered_functions(uncovered_count))
            uncovered_count = 0  ! Reset for collection
            do file_idx = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(file_idx)%functions)) then
                    do func_idx = 1, size(coverage_data%files(file_idx)%functions)
                        associate(func => coverage_data%files(file_idx)%functions(func_idx))
                            if (func%execution_count == 0) then
                                uncovered_count = uncovered_count + 1
                                uncovered_functions(uncovered_count) = func%line_number
                            end if
                        end associate
                    end do
                end if
            end do
        else
            allocate(uncovered_functions(0))
        end if
        
        ! Calculate percentage
        if (total_functions == 0) then
            call stats%init(100.0_8, 0, 0, "")
        else
            call stats%init((real(covered_functions, 8) / real(total_functions, 8)) * 100.0_8, &
                           covered_functions, total_functions, &
                           compress_ranges(uncovered_functions))
        end if
        
        deallocate(uncovered_functions)
    end function calculate_function_coverage

    ! Calculate module-level statistics
    function calculate_module_coverage(coverage_data, module_name) result(stats)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: module_name
        type(module_stats_t) :: stats
        integer :: file_idx, func_idx, line_idx, branch_idx
        integer :: total_lines, covered_lines
        integer :: total_functions, covered_functions
        integer :: total_branches, covered_branches
        
        ! Initialize counters
        total_lines = 0
        covered_lines = 0
        total_functions = 0
        covered_functions = 0
        total_branches = 0
        covered_branches = 0
        
        ! Count module-specific statistics
        do file_idx = 1, size(coverage_data%files)
            ! Count functions belonging to this module
            if (allocated(coverage_data%files(file_idx)%functions)) then
                do func_idx = 1, size(coverage_data%files(file_idx)%functions)
                    associate(func => coverage_data%files(file_idx)%functions(func_idx))
                        if (trim(func%parent_module) == trim(module_name)) then
                            total_functions = total_functions + 1
                            if (func%execution_count > 0) then
                                covered_functions = covered_functions + 1
                            end if
                            
                            ! Count lines within this function
                            if (allocated(func%lines)) then
                                do line_idx = 1, size(func%lines)
                                    if (func%lines(line_idx)%is_executable) then
                                        total_lines = total_lines + 1
                                        if (func%lines(line_idx)%is_covered()) then
                                            covered_lines = covered_lines + 1
                                        end if
                                    end if
                                end do
                            end if
                            
                            ! Count branches within this function
                            if (allocated(func%branches)) then
                                do branch_idx = 1, size(func%branches)
                                    total_branches = total_branches + 1
                                    if (func%branches(branch_idx)%is_fully_covered()) then
                                        covered_branches = covered_branches + 1
                                    end if
                                end do
                            end if
                        end if
                    end associate
                end do
            end if
        end do
        
        ! Calculate percentages
        call stats%init(module_name, &
                       calculate_percentage(covered_lines, total_lines), &
                       calculate_percentage(covered_functions, total_functions), &
                       calculate_percentage(covered_branches, total_branches), &
                       total_lines, covered_lines, &
                       total_functions, covered_functions, &
                       total_branches, covered_branches)
    end function calculate_module_coverage
    
    ! Helper function to calculate percentage safely
    function calculate_percentage(covered, total) result(percentage)
        integer, intent(in) :: covered, total
        real(8) :: percentage
        
        if (total == 0) then
            percentage = 100.0_8  ! No items to cover = 100% coverage
        else
            percentage = (real(covered, 8) / real(total, 8)) * 100.0_8
        end if
    end function calculate_percentage

end module coverage_statistics