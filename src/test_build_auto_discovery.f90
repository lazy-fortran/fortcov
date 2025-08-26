module test_build_auto_discovery
    !! Minimal stub module to fix missing dependency
    !! TODO: Implement full test build auto-discovery functionality
    
    implicit none
    private
    
    public :: test_build_result_t
    public :: auto_discover_test_build
    
    type :: test_build_result_t
        logical :: success = .false.
        character(len=512) :: error_message = ''
        character(len=256) :: test_command = ''
        logical :: has_coverage_support = .false.
    end type test_build_result_t
    
contains
    
    subroutine auto_discover_test_build(directory, config, result)
        !! Stub implementation for test build auto-discovery
        character(len=*), intent(in) :: directory
        type(*), intent(in) :: config  ! Using assumed type to avoid circular dependency
        type(test_build_result_t), intent(out) :: result
        
        ! Stub implementation - always returns failure for now
        result%success = .false.
        result%error_message = 'Test build auto-discovery not yet implemented'
        result%test_command = ''
        result%has_coverage_support = .false.
    end subroutine auto_discover_test_build
    
end module test_build_auto_discovery