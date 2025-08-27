module coverage_summary_impl
    use coverage_model_core
    use data_transformer_types
    implicit none
    private
    
    ! Public procedures
    public :: generate_coverage_summary
    public :: generate_metadata_json
    
contains

    ! Generate coverage summary statistics
    subroutine generate_coverage_summary(coverage_data, summary, success)
        type(coverage_data_t), intent(in) :: coverage_data
        type(coverage_summary_t), intent(out) :: summary
        logical, intent(out) :: success
        
        integer :: i, total_lines, covered_lines
        
        call summary%init()
        success = .false.
        
        ! Check if coverage data is properly initialized
        if (.not. allocated(coverage_data%files)) then
            success = .true.  ! Empty data is valid
            return
        end if
        
        summary%total_files = size(coverage_data%files)
        total_lines = 0
        covered_lines = 0
        
        ! Calculate summary statistics
        do i = 1, size(coverage_data%files)
            if (allocated(coverage_data%files(i)%lines)) then
                total_lines = total_lines + size(coverage_data%files(i)%lines)
                covered_lines = covered_lines + &
                    count(coverage_data%files(i)%lines%execution_count > 0 .and. &
                          coverage_data%files(i)%lines%is_executable)
            end if
        end do
        
        summary%total_lines = total_lines
        summary%covered_lines = covered_lines
        
        ! Calculate percentage
        if (total_lines > 0) then
            summary%coverage_percentage = &
                real(covered_lines) / real(total_lines) * 100.0
        end if
        
        success = .true.
    end subroutine generate_coverage_summary
    
    ! Generate metadata JSON from summary
    subroutine generate_metadata_json(summary, metadata_json)
        type(coverage_summary_t), intent(in) :: summary
        character(len=:), allocatable, intent(out) :: metadata_json
        
        character(len=20) :: total_files_str, total_lines_str, covered_lines_str
        character(len=20) :: coverage_percentage_str
        
        ! Convert numbers to strings
        write(total_files_str, '(I0)') summary%total_files
        write(total_lines_str, '(I0)') summary%total_lines
        write(covered_lines_str, '(I0)') summary%covered_lines
        write(coverage_percentage_str, '(F0.1)') summary%coverage_percentage
        
        ! Build JSON
        metadata_json = '{' // &
            '"total_files": ' // trim(total_files_str) // ', ' // &
            '"total_lines": ' // trim(total_lines_str) // ', ' // &
            '"covered_lines": ' // trim(covered_lines_str) // ', ' // &
            '"coverage_percentage": ' // trim(coverage_percentage_str) // &
            '}'
    end subroutine generate_metadata_json

end module coverage_summary_impl