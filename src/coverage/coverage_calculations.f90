module coverage_calculations
    !! Coverage calculation operations extracted from coverage_operations_core
    !! 
    !! Focused solely on coverage statistics calculation and file-level coverage
    !! computations. Provides clean separation of calculation logic from
    !! data manipulation operations.
    use constants_core
    use foundation_utils
    use coverage_data_core
    use coverage_stats_core, only: extended_coverage_stats_t
    implicit none
    private
    
    public :: calculate_coverage_statistics
    public :: calculate_file_coverage
    public :: calculate_overall_coverage
    
contains
    
    subroutine calculate_coverage_statistics(coverage_data, stats)
        !! Calculates comprehensive coverage statistics
        type(coverage_data_t), intent(in) :: coverage_data
        type(extended_coverage_stats_t), intent(out) :: stats
        
        integer :: i, j
        
        ! Initialize statistics
        stats%total_lines = 0
        stats%covered_lines = 0
        stats%total_branches = 0
        stats%covered_branches = 0
        stats%total_functions = 0
        stats%covered_functions = 0
        stats%total_files = 0
        stats%covered_files = 0
        
        if (.not. allocated(coverage_data%files)) return
        
        stats%total_files = size(coverage_data%files)
        
        ! Calculate statistics for each file
        do i = 1, size(coverage_data%files)
            associate(file => coverage_data%files(i))
                call calculate_file_statistics(file, stats)
                
                ! Count covered files
                if (file%line_coverage > 0.0) then
                    stats%covered_files = stats%covered_files + 1
                end if
            end associate
        end do
        
        ! Calculate percentages
        if (stats%total_lines > 0) then
            stats%line_coverage = real(stats%covered_lines) / real(stats%total_lines) * 100.0
        end if
        
        if (stats%total_branches > 0) then
            stats%branch_coverage = real(stats%covered_branches) / real(stats%total_branches) * 100.0
        end if
        
        if (stats%total_functions > 0) then
            stats%function_coverage = real(stats%covered_functions) / real(stats%total_functions) * 100.0
        end if
        
    end subroutine calculate_coverage_statistics
    
    subroutine calculate_file_coverage(file)
        !! Calculates coverage statistics for a file
        type(coverage_file_t), intent(inout) :: file
        
        integer :: i, total_executable, covered_count
        
        if (.not. allocated(file%lines)) return
        
        total_executable = 0
        covered_count = 0
        
        do i = 1, size(file%lines)
            if (file%lines(i)%is_executable) then
                total_executable = total_executable + 1
                if (file%lines(i)%is_covered()) then
                    covered_count = covered_count + 1
                end if
            end if
        end do
        
        file%total_lines = total_executable
        file%covered_lines = covered_count
        
        if (total_executable > 0) then
            file%line_coverage = real(covered_count) / real(total_executable) * 100.0
        else
            file%line_coverage = 0.0
        end if
        
    end subroutine calculate_file_coverage
    
    subroutine calculate_overall_coverage(coverage_data)
        !! Calculates overall coverage for the entire dataset
        type(coverage_data_t), intent(inout) :: coverage_data
        
        integer :: i, total_lines, covered_lines
        
        if (.not. allocated(coverage_data%files)) return
        
        total_lines = 0
        covered_lines = 0
        
        do i = 1, size(coverage_data%files)
            call calculate_file_coverage(coverage_data%files(i))
            total_lines = total_lines + coverage_data%files(i)%total_lines
            covered_lines = covered_lines + coverage_data%files(i)%covered_lines
        end do
        
        coverage_data%total_files = size(coverage_data%files)
        coverage_data%total_lines = total_lines
        coverage_data%covered_lines = covered_lines
        
        if (total_lines > 0) then
            coverage_data%overall_coverage = real(covered_lines) / real(total_lines) * 100.0
        else
            coverage_data%overall_coverage = 0.0
        end if
        
    end subroutine calculate_overall_coverage
    
    ! Helper functions
    subroutine calculate_file_statistics(file, stats)
        !! Calculates statistics for a single file
        type(coverage_file_t), intent(in) :: file
        type(extended_coverage_stats_t), intent(inout) :: stats
        
        integer :: i
        
        if (allocated(file%lines)) then
            do i = 1, size(file%lines)
                if (file%lines(i)%is_executable) then
                    stats%total_lines = stats%total_lines + 1
                    if (file%lines(i)%is_covered()) then
                        stats%covered_lines = stats%covered_lines + 1
                    end if
                end if
            end do
        end if
        
        if (allocated(file%branches)) then
            stats%total_branches = stats%total_branches + size(file%branches)
            do i = 1, size(file%branches)
                if (file%branches(i)%is_fully_covered()) then
                    stats%covered_branches = stats%covered_branches + 1
                end if
            end do
        end if
        
        if (allocated(file%functions)) then
            stats%total_functions = stats%total_functions + size(file%functions)
            do i = 1, size(file%functions)
                if (file%functions(i)%is_covered()) then
                    stats%covered_functions = stats%covered_functions + 1
                end if
            end do
        end if
        
    end subroutine calculate_file_statistics
    
end module coverage_calculations