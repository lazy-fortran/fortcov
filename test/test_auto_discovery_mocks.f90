module test_auto_discovery_mocks
    !! Mock implementations for auto-discovery testing
    !! Provides controlled test environments for discovery logic
    
    implicit none
    private
    
    public :: mock_discovery_result_t, create_mock_discovery_environment
    public :: cleanup_mock_discovery_environment
    
    type :: mock_discovery_result_t
        logical :: success = .false.
        character(len=256) :: message = ''
    end type mock_discovery_result_t
    
contains
    
    subroutine create_mock_discovery_environment()
        !! Create mock environment for testing
        call execute_command_line('mkdir -p mock_test_env')
        call execute_command_line('touch mock_test_env/test.gcov')
        call execute_command_line('touch mock_test_env/src.f90')
    end subroutine create_mock_discovery_environment
    
    subroutine cleanup_mock_discovery_environment()
        !! Clean up mock environment
        call execute_command_line('rm -rf mock_test_env')
    end subroutine cleanup_mock_discovery_environment
    
end module test_auto_discovery_mocks
