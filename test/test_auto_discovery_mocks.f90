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
        !! Create mock environment for testing using proper Fortran I/O
        integer :: unit, iostat
        logical :: exists
        
        ! Create directory using Fortran mkdir (if available) or inquire to check
        inquire(file='mock_test_env', exist=exists)
        if (.not. exists) then
            ! Create directory - use execute_command_line only for mkdir as it's the most portable
            call execute_command_line('mkdir -p mock_test_env')
        end if
        
        ! Create test.gcov using Fortran I/O
        open(newunit=unit, file='mock_test_env/test.gcov', status='unknown', &
             action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') '        -:    0:Source:test.f90'
            write(unit, '(A)') '        -:    0:Graph:test.gcno'  
            write(unit, '(A)') '        -:    0:Data:test.gcda'
            close(unit)
        end if
        
        ! Create src.f90 using Fortran I/O
        open(newunit=unit, file='mock_test_env/src.f90', status='unknown', &
             action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') 'program test'
            write(unit, '(A)') '    print *, "Hello World"'
            write(unit, '(A)') 'end program test'
            close(unit)
        end if
        
    end subroutine create_mock_discovery_environment
    
    subroutine cleanup_mock_discovery_environment()
        !! Clean up mock environment using proper Fortran I/O
        integer :: unit, iostat
        logical :: exists
        
        ! Remove test.gcov using Fortran I/O
        inquire(file='mock_test_env/test.gcov', exist=exists)
        if (exists) then
            open(newunit=unit, file='mock_test_env/test.gcov', status='old', iostat=iostat)
            if (iostat == 0) then
                close(unit, status='delete')
            end if
        end if
        
        ! Remove src.f90 using Fortran I/O
        inquire(file='mock_test_env/src.f90', exist=exists)
        if (exists) then
            open(newunit=unit, file='mock_test_env/src.f90', status='old', iostat=iostat)
            if (iostat == 0) then
                close(unit, status='delete')
            end if
        end if
        
        ! Remove directory - unfortunately Fortran doesn't have portable directory removal
        ! Keep execute_command_line for directory cleanup as it's the most reliable approach
        call execute_command_line('rmdir mock_test_env 2>/dev/null')
        
    end subroutine cleanup_mock_discovery_environment
    
end module test_auto_discovery_mocks
