module coverage_annotator
    use coverage_model_core
    use data_transformer_types
    implicit none
    private
    
    ! Public procedures
    public :: annotate_coverage_file
    public :: extract_line_context
    
contains

    ! Annotate coverage data onto source file
    subroutine annotate_coverage_file(coverage_file, annotated_file, success)
        type(coverage_file_t), intent(in) :: coverage_file
        type(source_file_t), intent(out) :: annotated_file
        logical, intent(out) :: success
        
        integer :: i
        
        call annotated_file%init()
        annotated_file%filename = coverage_file%filename
        
        ! Simple implementation: just copy coverage data
        if (allocated(coverage_file%lines)) then
            allocate(annotated_file%lines(size(coverage_file%lines)))
            
            do i = 1, size(coverage_file%lines)
                call annotated_file%lines(i)%init()
                annotated_file%lines(i)%line_number = &
                    coverage_file%lines(i)%line_number
                annotated_file%lines(i)%execution_count = &
                    coverage_file%lines(i)%execution_count
                annotated_file%lines(i)%is_executable = &
                    coverage_file%lines(i)%is_executable
                annotated_file%lines(i)%is_covered = &
                    coverage_file%lines(i)%execution_count > 0
            end do
            
            ! Calculate coverage percentage
            annotated_file%coverage_percentage = &
                coverage_file%get_line_coverage()
        end if
        
        success = .true.
    end subroutine annotate_coverage_file
    
    ! Extract line context from coverage data and map to source file
    subroutine extract_line_context(coverage_file, source_file, success)
        type(coverage_file_t), intent(in) :: coverage_file
        type(source_file_t), intent(inout) :: source_file
        logical, intent(out) :: success
        
        integer :: i, j
        
        success = .false.
        
        ! Load source file first if not already loaded
        if (.not. allocated(source_file%lines)) then
            ! Need to load source file
            success = .false.
            return
        end if
        
        ! Map coverage data to source lines
        if (allocated(coverage_file%lines)) then
            do i = 1, size(coverage_file%lines)
                do j = 1, size(source_file%lines)
                    if (source_file%lines(j)%line_number == &
                        coverage_file%lines(i)%line_number) then
                        source_file%lines(j)%execution_count = &
                            coverage_file%lines(i)%execution_count
                        source_file%lines(j)%is_executable = &
                            coverage_file%lines(i)%is_executable
                        source_file%lines(j)%is_covered = &
                            coverage_file%lines(i)%execution_count > 0
                        exit
                    end if
                end do
            end do
        end if
        
        success = .true.
    end subroutine extract_line_context

end module coverage_annotator