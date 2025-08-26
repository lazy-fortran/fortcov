module coverage_reporter_utils
    !! Coverage Reporter Utilities
    !! 
    !! Extracted from coverage_reporter_impl for Issue #182 module size compliance.
    !! Contains shared utility functions for calculating coverage statistics.
    use coverage_model
    use coverage_statistics, only: stats_t => coverage_stats_t
    use string_utilities, only: int_to_string
    implicit none
    private
    
    public :: calculate_manual_line_stats
    public :: calculate_manual_branch_stats  
    public :: calculate_manual_function_stats
    public :: int_to_string

contains

    ! Manual calculation of line coverage statistics
    subroutine calculate_manual_line_stats(coverage_data, line_stats)
        type(coverage_data_t), intent(in) :: coverage_data
        type(stats_t), intent(out) :: line_stats
        integer :: total_lines, covered_lines, i
        real :: percentage
        
        total_lines = 0
        covered_lines = 0
        
        ! Sum across all files
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                total_lines = total_lines + coverage_data%files(i)%total_lines
                covered_lines = covered_lines + coverage_data%files(i)%covered_lines
            end do
        end if
        
        ! Calculate percentage
        if (total_lines > 0) then
            percentage = real(covered_lines) / real(total_lines) * 100.0
        else
            percentage = 100.0
        end if
        
        ! Initialize stats
        call line_stats%init(percentage, covered_lines, total_lines, "")
    end subroutine calculate_manual_line_stats

    ! Manual calculation of branch coverage statistics
    subroutine calculate_manual_branch_stats(coverage_data, branch_stats)
        type(coverage_data_t), intent(in) :: coverage_data
        type(stats_t), intent(out) :: branch_stats
        integer :: total_branches, covered_branches, i, j
        real :: percentage
        
        total_branches = 0
        covered_branches = 0
        
        ! Calculate from branch data in files
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%branches)) then
                    do j = 1, size(coverage_data%files(i)%branches)
                        total_branches = total_branches + 1
                        if (coverage_data%files(i)%branches(j)%taken_count > 0) then
                            covered_branches = covered_branches + 1
                        end if
                    end do
                end if
            end do
        end if
        
        ! Calculate percentage
        if (total_branches > 0) then
            percentage = real(covered_branches) / real(total_branches) * 100.0
        else
            percentage = 100.0
        end if
        
        ! Initialize stats
        call branch_stats%init(percentage, covered_branches, total_branches, "")
    end subroutine calculate_manual_branch_stats

    ! Manual calculation of function coverage statistics
    subroutine calculate_manual_function_stats(coverage_data, func_stats)
        type(coverage_data_t), intent(in) :: coverage_data
        type(stats_t), intent(out) :: func_stats
        integer :: total_functions, covered_functions, i, j
        real :: percentage
        
        total_functions = 0
        covered_functions = 0
        
        ! Calculate from function data in files
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%functions)) then
                    do j = 1, size(coverage_data%files(i)%functions)
                        total_functions = total_functions + 1
                        if (coverage_data%files(i)%functions(j)%execution_count > 0) then
                            covered_functions = covered_functions + 1
                        end if
                    end do
                end if
            end do
        end if
        
        ! Calculate percentage
        if (total_functions > 0) then
            percentage = real(covered_functions) / real(total_functions) * 100.0
        else
            percentage = 100.0
        end if
        
        ! Initialize stats
        call func_stats%init(percentage, covered_functions, total_functions, "")
    end subroutine calculate_manual_function_stats


end module coverage_reporter_utils