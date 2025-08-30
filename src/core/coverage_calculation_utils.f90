module coverage_calculation_utils
    !! Coverage Calculation Utilities Module
    !!
    !! Provides comprehensive utilities for calculating various coverage
    !! metrics from coverage data structures. Supports line coverage,
    !! branch coverage, and statistical analysis with memory safety.
    !!
    !! Key Features:
    !! - Line coverage rate calculations
    !! - Branch coverage rate calculations using actual branch data
    !! - Executable and covered line counting
    !! - Memory-safe processing with proper bounds checking

    use coverage_model_core
    implicit none
    private

    public :: calculate_coverage_rates
    public :: calculate_branch_coverage_rate
    public :: count_executable_lines
    public :: count_covered_lines

contains

    ! Calculate overall coverage rates from coverage data
    subroutine calculate_coverage_rates(coverage_data, line_rate, branch_rate)
        type(coverage_data_t), intent(in) :: coverage_data
        real, intent(out) :: line_rate, branch_rate

        integer :: total_lines, covered_lines

        total_lines = count_executable_lines(coverage_data)
        covered_lines = count_covered_lines(coverage_data)

        if (total_lines > 0) then
            line_rate = real(covered_lines) / real(total_lines)
        else
            line_rate = 0.0
        end if

        ! Branch coverage calculation - analyze conditional statements
        branch_rate = calculate_branch_coverage_rate(coverage_data)

    end subroutine calculate_coverage_rates

    ! Count total executable lines
    function count_executable_lines(coverage_data) result(total_lines)
        type(coverage_data_t), intent(in) :: coverage_data
        integer :: total_lines
        integer :: i, j

        total_lines = 0

        ! Memory safety: Check if files array is allocated
        if (.not. allocated(coverage_data%files)) return

        do i = 1, size(coverage_data%files)
            ! Memory safety: Check if lines array is allocated for this file
            if (.not. allocated(coverage_data%files(i)%lines)) cycle

            do j = 1, size(coverage_data%files(i)%lines)
                if (coverage_data%files(i)%lines(j)%is_executable) then
                    total_lines = total_lines + 1
                end if
            end do
        end do

    end function count_executable_lines

    ! Count total covered lines
    function count_covered_lines(coverage_data) result(covered_lines)
        type(coverage_data_t), intent(in) :: coverage_data
        integer :: covered_lines
        integer :: i, j

        covered_lines = 0

        ! Memory safety: Check if files array is allocated
        if (.not. allocated(coverage_data%files)) return

        do i = 1, size(coverage_data%files)
            ! Memory safety: Check if lines array is allocated for this file
            if (.not. allocated(coverage_data%files(i)%lines)) cycle

            do j = 1, size(coverage_data%files(i)%lines)
                if (coverage_data%files(i)%lines(j)%is_executable .and. &
                    coverage_data%files(i)%lines(j)%execution_count > 0) then
                    covered_lines = covered_lines + 1
                end if
            end do
        end do

    end function count_covered_lines

    ! Calculate branch coverage rate using proper branch data
    function calculate_branch_coverage_rate(coverage_data) result(branch_rate)
        type(coverage_data_t), intent(in) :: coverage_data
        real :: branch_rate

        integer :: total_branches, covered_branches
        integer :: file_idx, func_idx, branch_idx

        total_branches = 0
        covered_branches = 0

        ! Count actual branch coverage from gcov branch data
        if (allocated(coverage_data%files)) then
            do file_idx = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(file_idx)%functions)) then
                    do func_idx = 1, size(coverage_data%files(file_idx)%functions)
                        if (allocated(coverage_data%files(file_idx) &
                                          %functions(func_idx)%branches)) then
                            do branch_idx = 1, size(coverage_data%files(file_idx) &
                                                  %functions(func_idx) &
                                                  %branches)
                                total_branches = total_branches + 1
                                ! Branch is covered if taken path has been executed
                                if (coverage_data%files(file_idx)%functions(func_idx) &
                                    %branches(branch_idx)%taken_count > 0) then
                                    covered_branches = covered_branches + 1
                                end if
                            end do
                        end if
                    end do
                end if
            end do
        end if

        ! Use safe percentage calculation (returns 0.0 for 0/0 case)
        if (total_branches > 0) then
            branch_rate = real(covered_branches) / real(total_branches)
        else
            branch_rate = 0.0  ! No branches means 0% coverage (mathematical correctness)
        end if

    end function calculate_branch_coverage_rate

end module coverage_calculation_utils