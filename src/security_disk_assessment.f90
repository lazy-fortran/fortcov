module security_disk_assessment
    !! Disk space security assessment module
    !!
    !! Performs real-time disk space analysis to detect critical
    !! storage conditions that could impact security operations.
    !!
    !! SECURITY RATIONALE:
    !! Insufficient disk space can cause:
    !! - Incomplete temporary file cleanup (security policy violation)
    !! - Failed security logging (audit trail gaps)
    !! - System instability affecting security controls
    implicit none
    private
    
    public :: get_disk_space_risk
    public :: assess_disk_risk_for_file
    
contains
    
    !! THREAD-SAFE: Get disk space risk without shared state
    !!
    !! DISK SPACE SECURITY ASSESSMENT:
    !! This subroutine performs real-time disk space analysis to detect
    !! critical storage conditions that could impact security operations.
    !!
    !! IMPLEMENTATION APPROACH:
    !! - Uses shell command 'df' to check current directory usage
    !! - Detects 90%+ usage patterns that indicate critical conditions
    !! - Thread-safe: No shared state to avoid race conditions
    !! - Fresh check each time: Ensures real-time accuracy over caching
    !!
    !! PERFORMANCE vs SECURITY TRADE-OFF:
    !! This function prioritizes thread safety and security accuracy
    !! over performance caching to ensure reliable security assessments.
    subroutine get_disk_space_risk(disk_space_risk)
        logical, intent(out) :: disk_space_risk
        integer :: stat
        
        ! Perform fresh check each time for thread safety
        ! Performance impact is minimal compared to thread safety issues
        call execute_command_line("df . 2>/dev/null | grep -q ' 9[0-9]% '", &
            exitstat=stat)
        disk_space_risk = (stat == 0)
    end subroutine get_disk_space_risk
    
    ! Assess disk risk for specific file location
    subroutine assess_disk_risk_for_file(filename, file_existed, disk_risk)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: file_existed
        logical, intent(out) :: disk_risk
        integer :: stat
        
        ! THREAD-SAFE: Direct disk space check without shared cache
        if (file_existed) then
            call execute_command_line("df " // trim(filename) // &
                " 2>/dev/null | grep -q ' 9[0-9]% '", exitstat=stat)
            disk_risk = (stat == 0)
        else
            disk_risk = .false.
        end if
    end subroutine assess_disk_risk_for_file
    
end module security_disk_assessment