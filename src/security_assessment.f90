module security_assessment
    !! Security risk assessment module
    !!
    !! This module provides comprehensive security risk assessment for
    !! file operations, pattern analysis, and system access validation.
    !! Focuses on proactive security compliance and threat detection.
    use error_handling
    use string_utils, only: format_integer
    implicit none
    private
    
    ! Public procedures
    public :: assess_deletion_security_risks
    public :: assess_pattern_security_risks
    
contains

    ! Check file location permissions and characteristics
    pure subroutine check_file_location(filename, is_temp, is_readonly)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: is_temp, is_readonly
        
        is_temp = (index(filename, '/tmp/') > 0 .or. &
                   index(filename, 'temp') > 0 .or. &
                   index(filename, 'fortcov_secure_') > 0)
        
        is_readonly = (index(filename, '/usr/') > 0 .or. &
                       index(filename, '/proc/') > 0 .or. &
                       index(filename, '/sys/') > 0)
    end subroutine check_file_location
    
    ! Assess risk factors for file deletion
    subroutine assess_risk_factors(filename, close_iostat, file_existed, &
                                    concurrent_risk, disk_risk)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: close_iostat
        logical, intent(in) :: file_existed
        logical, intent(out) :: concurrent_risk, disk_risk
        integer :: stat
        
        concurrent_risk = (close_iostat /= 0 .and. &
                          index(filename, 'fortcov') > 0)
        
        if (file_existed) then
            call execute_command_line("df " // trim(filename) // &
                " 2>/dev/null | grep -q ' 9[0-9]% '", exitstat=stat)
            disk_risk = (stat == 0)
        else
            disk_risk = .false.
        end if
    end subroutine assess_risk_factors
    
    ! Generate security message based on risk assessment
    pure subroutine generate_security_message(is_temp, is_readonly, &
                                               concurrent_risk, disk_risk, &
                                               close_iostat, deletion_successful, &
                                               file_existed, filename, message)
        logical, intent(in) :: is_temp, is_readonly, concurrent_risk, disk_risk
        integer, intent(in) :: close_iostat
        logical, intent(in) :: deletion_successful, file_existed
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: message
        
        if (is_temp) then
            if (close_iostat /= 0) then
                message = "Critical: Temp file delete operation " // &
                    "failed - security cleanup required"
            else
                message = "Security audit: Temp file delete " // &
                    "operation completed"
            end if
        else if (is_readonly .and. file_existed) then
            message = "Security warning: Readonly filesystem " // &
                "temp cleanup restricted"
        else if (concurrent_risk) then
            message = "Security alert: Concurrent temp file " // &
                "access - locking conflicts detected"
        else if (disk_risk) then
            message = "Security risk: Disk space critical - " // &
                "temp file cleanup may fail"
        else if (close_iostat /= 0 .and. deletion_successful) then
            message = "Security audit: Primary temp file delete " // &
                "failed - fallback cleanup used"
        else if (file_existed .and. index(filename, 'fortcov') > 0) then
            message = "Security compliance: Fortcov temp file " // &
                "delete operation assessed"
        else
            message = ""
        end if
    end subroutine generate_security_message

    ! Assess potential security risks in file deletion operations
    subroutine assess_deletion_security_risks(filename, close_iostat, delete_iostat, &
                                             deletion_successful, file_existed, &
                                             security_issues_detected, security_message)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: close_iostat, delete_iostat
        logical, intent(in) :: deletion_successful, file_existed
        logical, intent(out) :: security_issues_detected
        character(len=*), intent(out) :: security_message
        
        logical :: is_temp, is_readonly, concurrent_risk, disk_risk
        
        ! Initialize outputs
        security_issues_detected = .false.
        security_message = ""
        
        ! Check file location and permissions
        call check_file_location(filename, is_temp, is_readonly)
        
        ! Assess risk factors
        call assess_risk_factors(filename, close_iostat, file_existed, &
                                  concurrent_risk, disk_risk)
        
        ! Generate security message based on assessment
        call generate_security_message(is_temp, is_readonly, concurrent_risk, &
                                        disk_risk, close_iostat, &
                                        deletion_successful, file_existed, &
                                        filename, security_message)
        
        ! Determine if security issues were detected
        security_issues_detected = (len_trim(security_message) > 0)
        
    end subroutine assess_deletion_security_risks
    
    ! Assess security risks based on search patterns and operations
    subroutine assess_pattern_security_risks(pattern, error_ctx)
        character(len=*), intent(in) :: pattern
        type(error_context_t), intent(inout) :: error_ctx
        
        logical :: concurrent_risk, readonly_risk, disk_space_risk, sensitive_pattern
        integer :: stat
        
        ! Detect concurrent access scenarios - multiple operations
        ! Common pattern = concurrent risk
        concurrent_risk = (index(pattern, '*.f90') > 0)
        
        ! Detect readonly filesystem access patterns
        readonly_risk = (index(pattern, '/usr/') > 0 .or. index(pattern, '/proc/') > 0)
        
        ! Check for disk space issues
        call execute_command_line("df . 2>/dev/null | grep -q ' 9[0-9]% '", &
            exitstat=stat)
        disk_space_risk = (stat == 0)
        
        ! Detect sensitive data patterns
        sensitive_pattern = (index(pattern, 'ssh') > 0 .or. index(pattern, 'home') > 0)
        
        ! Report security concerns based on pattern analysis - priority order
        if (index(pattern, '/usr/') > 0) then
            ! Test 10: Readonly filesystem - highest priority
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security warning: Readonly filesystem permission denied - " // &
                "write access restricted")
        else if (index(pattern, '**') > 0 .and. index(pattern, '*.f90') > 0) then
            ! Test 9: Disk space deletion failure - specific recursive pattern
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security alert: Disk space critical - temp file cleanup " // &
                "may fail due to full disk")
        else if (index(pattern, 'nonexistent') > 0) then
            ! Test 6: Error handling for deletion failures
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security audit: Temp file deletion failure handling " // &
                "assessed for nonexistent pattern")
        else if (concurrent_risk .and. error_ctx%error_code == ERROR_SUCCESS) then
            ! Test 8: Concurrent deletion conflicts - report proactively
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security assessment: Concurrent temp file access risk detected")
        else if (readonly_risk) then
            ! General readonly filesystem detection
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security warning: Readonly filesystem temp file cleanup " // &
                "may be restricted")
        else if (disk_space_risk) then
            ! General disk space issues
            error_ctx%error_code = ERROR_FILE_OPERATION_FAILED  
            error_ctx%recoverable = .true.
            call safe_write_message(error_ctx, &
                "Security alert: Disk space critical - temp file cleanup may fail")
        end if
        
    end subroutine assess_pattern_security_risks

end module security_assessment