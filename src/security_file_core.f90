module security_file_core
    !! File location security assessment module
    !!
    !! Provides security checks for file locations including
    !! temporary file detection and readonly filesystem identification.
    !!
    !! SECURITY COMPLIANCE:
    !! - Detects temporary files requiring secure cleanup
    !! - Identifies readonly filesystem restrictions
    !! - Assesses concurrent access risks
    implicit none
    private
    
    public :: check_file_location
    public :: assess_concurrent_risk
    
contains
    
    ! Check file location permissions and characteristics - OPTIMIZED
    pure subroutine check_file_location(filename, is_temp, is_readonly)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: is_temp, is_readonly
        
        ! PERFORMANCE: Single scan with early exits
        integer :: pos
        
        is_temp = .false.
        is_readonly = .false.
        
        ! Check for temp patterns first (most common)
        pos = index(filename, '/tmp/')
        if (pos > 0) then
            is_temp = .true.
            return  ! Early exit - don't need to check readonly
        end if
        
        pos = index(filename, 'fortcov_secure_')
        if (pos > 0) then
            is_temp = .true.
            return  ! Early exit
        end if
        
        pos = index(filename, 'temp')
        if (pos > 0) then
            is_temp = .true.
            return  ! Early exit
        end if
        
        ! Check readonly patterns only if not temp
        pos = index(filename, '/usr/')
        if (pos > 0) then
            is_readonly = .true.
            return
        end if
        
        pos = index(filename, '/proc/')
        if (pos > 0) then
            is_readonly = .true.
            return
        end if
        
        pos = index(filename, '/sys/')
        if (pos > 0) then
            is_readonly = .true.
        end if
    end subroutine check_file_location
    
    ! Assess concurrent access risk for file
    pure function assess_concurrent_risk(filename, close_iostat) result(concurrent_risk)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: close_iostat
        logical :: concurrent_risk
        
        ! PERFORMANCE: Early exit for concurrent risk
        concurrent_risk = .false.
        if (close_iostat /= 0) then
            if (index(filename, 'fortcov') > 0) then
                concurrent_risk = .true.
            end if
        end if
    end function assess_concurrent_risk
    
end module security_file_core