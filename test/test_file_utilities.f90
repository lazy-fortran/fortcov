module test_file_utilities
    !! File creation and management utilities for tests
    !! Handles creation of test files and directories
    
    implicit none
    private
    
    public :: create_test_gcov_file, create_test_source_file, create_test_config_file
    
contains
    
    subroutine create_test_gcov_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') '        -:    0:Source:test_sample.f90'
        write(unit, '(A)') '        -:    0:Graph:test_sample.f90.gcno'
        write(unit, '(A)') '        -:    0:Data:test_sample.f90.gcda'
        write(unit, '(A)') '        1:    1:program test_sample'
        write(unit, '(A)') '        1:    2:  print *, "test"'
        write(unit, '(A)') '        1:    3:end program'
        close(unit)
    end subroutine create_test_gcov_file
    
    subroutine create_test_source_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') 'program test_sample'
        write(unit, '(A)') '  print *, "test"'
        write(unit, '(A)') 'end program'
        close(unit)
    end subroutine create_test_source_file
    
    subroutine create_test_config_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit
        
        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') '[output]'
        write(unit, '(A)') 'format = "html"'
        write(unit, '(A)') 'directory = "./coverage"'
        close(unit)
    end subroutine create_test_config_file
    
end module test_file_utilities