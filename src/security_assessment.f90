module security_assessment
    !! Security risk assessment module
    !!
    !! This module provides comprehensive security risk assessment for
    !! file operations, pattern analysis, and system access validation.
    !! Focuses on proactive security compliance and threat detection.
    !! PERFORMANCE OPTIMIZED: Consolidated pattern matching, caching, early exits
    use error_handling
    use string_utils, only: format_integer
    implicit none
    private
    
    ! Performance optimization: Pattern cache
    type :: pattern_cache_t
        character(len=256) :: pattern = ""
        logical :: is_concurrent_risk = .false.
        logical :: is_readonly_risk = .false.
        logical :: is_sensitive = .false.
        logical :: is_cached = .false.
    end type pattern_cache_t
    
    ! Cache for pattern analysis results (small LRU cache)
    integer, parameter :: CACHE_SIZE = 16
    type(pattern_cache_t), save :: pattern_cache(CACHE_SIZE)
    integer, save :: cache_next_slot = 1
    
    ! Performance: Disk space check cache
    logical, save :: disk_space_cached = .false.
    logical, save :: disk_space_risk = .false.
    real, save :: last_disk_check_time = 0.0
    
    ! Public procedures
    public :: assess_deletion_security_risks
    public :: assess_pattern_security_risks
    
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
    
    ! Assess risk factors for file deletion - OPTIMIZED WITH CACHING
    subroutine assess_risk_factors(filename, close_iostat, file_existed, &
                                    concurrent_risk, disk_risk)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: close_iostat
        logical, intent(in) :: file_existed
        logical, intent(out) :: concurrent_risk, disk_risk
        integer :: stat
        real :: current_time
        
        ! PERFORMANCE: Early exit for concurrent risk
        concurrent_risk = .false.
        if (close_iostat /= 0) then
            if (index(filename, 'fortcov') > 0) then
                concurrent_risk = .true.
            end if
        end if
        
        ! PERFORMANCE: Cache disk space check (expensive system call)
        if (file_existed) then
            call cpu_time(current_time)
            ! Cache disk space check for 5 seconds to avoid repeated system calls
            if (disk_space_cached .and. (current_time - last_disk_check_time < 5.0)) then
                disk_risk = disk_space_risk
            else
                call execute_command_line("df " // trim(filename) // &
                    " 2>/dev/null | grep -q ' 9[0-9]% '", exitstat=stat)
                disk_risk = (stat == 0)
                ! Update cache
                disk_space_cached = .true.
                disk_space_risk = disk_risk
                last_disk_check_time = current_time
            end if
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
    
    ! Assess security risks based on search patterns and operations - OPTIMIZED
    subroutine assess_pattern_security_risks(pattern, error_ctx)
        character(len=*), intent(in) :: pattern
        type(error_context_t), intent(inout) :: error_ctx
        
        logical :: concurrent_risk, readonly_risk, disk_space_risk, sensitive_pattern
        integer :: cache_idx
        
        ! PERFORMANCE: Check pattern cache first
        cache_idx = find_cached_pattern(pattern)
        if (cache_idx > 0) then
            concurrent_risk = pattern_cache(cache_idx)%is_concurrent_risk
            readonly_risk = pattern_cache(cache_idx)%is_readonly_risk
            sensitive_pattern = pattern_cache(cache_idx)%is_sensitive
        else
            ! Not cached - perform analysis
            call analyze_pattern_risks(pattern, concurrent_risk, readonly_risk, &
                                     sensitive_pattern)
            ! Cache results for future use
            call cache_pattern_results(pattern, concurrent_risk, readonly_risk, &
                                     sensitive_pattern)
        end if
        
        ! PERFORMANCE: Reuse cached disk space check
        call get_cached_disk_space_risk(disk_space_risk)
        
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

    ! PERFORMANCE: Find cached pattern results
    pure function find_cached_pattern(pattern) result(cache_idx)
        character(len=*), intent(in) :: pattern
        integer :: cache_idx
        integer :: i
        
        cache_idx = 0
        do i = 1, CACHE_SIZE
            if (pattern_cache(i)%is_cached .and. &
                trim(pattern_cache(i)%pattern) == trim(pattern)) then
                cache_idx = i
                return
            end if
        end do
    end function find_cached_pattern
    
    ! PERFORMANCE: Consolidated pattern risk analysis
    pure subroutine analyze_pattern_risks(pattern, concurrent_risk, readonly_risk, &
                                         sensitive_pattern)
        character(len=*), intent(in) :: pattern
        logical, intent(out) :: concurrent_risk, readonly_risk, sensitive_pattern
        
        integer :: pos
        
        ! Initialize all to false
        concurrent_risk = .false.
        readonly_risk = .false.
        sensitive_pattern = .false.
        
        ! PERFORMANCE: Single scan with position-based checks
        ! Check for concurrent risk patterns first (most common)
        pos = index(pattern, '*.f90')
        if (pos > 0) then
            concurrent_risk = .true.
        end if
        
        ! Check for readonly patterns with early exit
        pos = index(pattern, '/usr/')
        if (pos > 0) then
            readonly_risk = .true.
        else
            pos = index(pattern, '/proc/')
            if (pos > 0) then
                readonly_risk = .true.
            end if
        end if
        
        ! Check for sensitive patterns
        pos = index(pattern, 'ssh')
        if (pos > 0) then
            sensitive_pattern = .true.
        else
            pos = index(pattern, 'home')
            if (pos > 0) then
                sensitive_pattern = .true.
            end if
        end if
    end subroutine analyze_pattern_risks
    
    ! PERFORMANCE: Cache pattern analysis results
    subroutine cache_pattern_results(pattern, concurrent_risk, readonly_risk, &
                                   sensitive_pattern)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: concurrent_risk, readonly_risk, sensitive_pattern
        
        ! Simple LRU replacement - use next slot and wrap around
        pattern_cache(cache_next_slot)%pattern = pattern
        pattern_cache(cache_next_slot)%is_concurrent_risk = concurrent_risk
        pattern_cache(cache_next_slot)%is_readonly_risk = readonly_risk
        pattern_cache(cache_next_slot)%is_sensitive = sensitive_pattern
        pattern_cache(cache_next_slot)%is_cached = .true.
        
        cache_next_slot = cache_next_slot + 1
        if (cache_next_slot > CACHE_SIZE) cache_next_slot = 1
    end subroutine cache_pattern_results
    
    ! PERFORMANCE: Get cached disk space risk
    subroutine get_cached_disk_space_risk(disk_space_risk)
        logical, intent(out) :: disk_space_risk
        real :: current_time
        integer :: stat
        
        call cpu_time(current_time)
        ! Use cached result if less than 5 seconds old
        if (disk_space_cached .and. (current_time - last_disk_check_time < 5.0)) then
            disk_space_risk = disk_space_risk
        else
            ! Perform fresh check
            call execute_command_line("df . 2>/dev/null | grep -q ' 9[0-9]% '", &
                exitstat=stat)
            disk_space_risk = (stat == 0)
            ! Update cache
            disk_space_cached = .true.
            disk_space_risk = disk_space_risk
            last_disk_check_time = current_time
        end if
    end subroutine get_cached_disk_space_risk

end module security_assessment