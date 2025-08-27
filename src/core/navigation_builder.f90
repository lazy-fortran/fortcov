module navigation_builder
    use coverage_model_core
    use data_transformer_types
    implicit none
    private
    
    ! Public procedures
    public :: build_navigation_tree
    
contains

    ! Build navigation tree from coverage data
    subroutine build_navigation_tree(input_data, nav_tree, success)
        type(coverage_data_t), intent(in) :: input_data
        type(navigation_tree_t), intent(out) :: nav_tree
        logical, intent(out) :: success
        
        integer :: i
        
        ! Initialize navigation tree with error handling
        call nav_tree%init()
        success = .false.
        
        ! Defensive check for uninitialized input_data
        if (.not. allocated(input_data%files)) then
            success = .true.  ! Empty is not an error
            return
        end if
        
        if (size(input_data%files) == 0) then
            success = .true.  ! Empty is not an error
            return
        end if
        
        ! Simple implementation: flat structure for now
        allocate(nav_tree%nodes(size(input_data%files)))
        
        do i = 1, size(input_data%files)
            call nav_tree%nodes(i)%init()
            nav_tree%nodes(i)%name = input_data%files(i)%filename
            nav_tree%nodes(i)%path = input_data%files(i)%filename
            nav_tree%nodes(i)%is_directory = .false.
            nav_tree%nodes(i)%coverage_percentage = &
                input_data%files(i)%get_line_coverage()
        end do
        
        nav_tree%has_hierarchy = .true.
        success = .true.
    end subroutine build_navigation_tree

end module navigation_builder