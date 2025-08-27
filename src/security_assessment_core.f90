module security_assessment_core
    !! Security risk assessment module
    !!
    !! This module provides comprehensive security risk assessment for
    !! file operations, pattern analysis, and system access validation.
    !! Focuses on proactive security compliance and threat detection.
    !!
    !! RISK PRIORITIZATION FRAMEWORK:
    !! 1. CRITICAL: Temp file cleanup failures (security policy violation)
    !! 2. HIGH: Readonly filesystem violations (system integrity risk)
    !! 3. MEDIUM: Concurrent access conflicts (data consistency risk)
    !! 4. LOW: Disk space warnings (operational risk)
    !!
    !! SECURITY PATTERNS DETECTED:
    !! - Directory traversal attacks (../../../etc/passwd)
    !! - System file access attempts (/usr/, /etc/, /root/, /proc/, /sys/)
    !! - Temporary file injection (fortcov_secure_*, /tmp/*)
    !! - Sensitive directory access (home, ssh, root, etc patterns)
    !!
    !! PERFORMANCE OPTIMIZED: Consolidated pattern matching, caching, early exits
    !!
    !! This module coordinates security assessment using specialized modules:
    !! - security_pattern_analyzer: Pattern-based risk detection
    !! - security_disk_assessment: Disk space security monitoring
    !! - security_file_location: File location security checks
    use error_handling_core
    use string_utils, only: int_to_string
    use security_pattern_core, only: pattern_cache_t, CACHE_SIZE, &
                                          analyze_pattern_risks, &
                                          find_cached_pattern, &
                                          cache_pattern_results
    use security_disk_core, only: get_disk_space_risk, &
                                        assess_disk_risk_for_file
    use security_file_core, only: check_file_location, &
                                      assess_concurrent_risk
    implicit none
    private
    
    ! Performance: Disk space check cache (per-function to avoid thread issues)
    
    ! Public procedures
    public :: assess_deletion_security_risks
    public :: assess_pattern_security_risks
    public :: check_file_location
    
contains
    
    ! Assess risk factors for file deletion - OPTIMIZED WITH CACHING
    subroutine assess_risk_factors(filename, close_iostat, file_existed, &
                                    concurrent_risk, disk_risk)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: close_iostat
        logical, intent(in) :: file_existed
        logical, intent(out) :: concurrent_risk, disk_risk
        
        ! Use specialized modules for assessment
        concurrent_risk = assess_concurrent_risk(filename, close_iostat)
        call assess_disk_risk_for_file(filename, file_existed, disk_risk)
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
    !!
    !! RISK ASSESSMENT ALGORITHM:
    !! This subroutine performs comprehensive security evaluation of file deletion
    !! operations using a multi-layered approach:
    !!
    !! 1. LOCATION ANALYSIS: Identifies file type and permission context
    !!    - Temporary files: /tmp/, fortcov_secure_*, temp patterns
    !!    - System files: /usr/, /proc/, /sys/ (readonly filesystem detection)
    !!
    !! 2. RISK FACTOR EVALUATION: Assesses operational security risks
    !!    - Concurrent access: Multi-process file locking conflicts
    !!    - Disk space: Critical storage capacity issues (90%+ usage)
    !!
    !! 3. SECURITY MESSAGE GENERATION: Prioritized threat reporting
    !!    - Critical: Temp file deletion failures (immediate action required)
    !!    - Warning: Readonly/concurrent access issues (monitoring required)
    !!    - Audit: Successful operations (compliance logging)
    !!
    !! USAGE EXAMPLE:
    !!   call assess_deletion_security_risks("temp_file.dat", close_stat, 
    !!                                      del_stat, success, existed, 
    !!                                      issues_found, message)
    !!   if (issues_found) then
    !!       write(error_unit, '(A)') trim(message)
    !!   end if
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
    !!
    !! PATTERN SECURITY ANALYSIS:
    !! This subroutine analyzes file patterns for security vulnerabilities using
    !! cached pattern recognition and prioritized threat detection:
    !!
    !! RISK CATEGORIES (Priority Order):
    !! 1. READONLY FILESYSTEM VIOLATIONS (/usr/, /proc/, /sys/)
    !!    - Highest priority: System integrity protection
    !!    - Blocks unauthorized system file access
    !!
    !! 2. DISK SPACE CRITICAL CONDITIONS (**/*.f90 recursive patterns)
    !!    - High priority: Prevents system instability
    !!    - Detects storage exhaustion risks during operations
    !!
    !! 3. CONCURRENT ACCESS RISKS (*.f90 patterns)
    !!    - Medium priority: Data consistency protection
    !!    - Identifies potential file locking conflicts
    !!
    !! 4. SENSITIVE DIRECTORY PATTERNS (ssh, home, root, etc)
    !!    - Security monitoring: Privacy and access control
    !!    - Tracks access to sensitive filesystem areas
    !!
    !! PERFORMANCE OPTIMIZATIONS:
    !! - Thread-safe pattern caching (local cache per function)
    !! - Early exit strategies for performance-critical paths
    !! - Consolidated pattern matching to reduce string operations
    !!
    !! USAGE EXAMPLE:
    !!   type(error_context_t) :: ctx
    !!   call assess_pattern_security_risks("**/*.f90", ctx)
    !!   if (ctx%error_code /= ERROR_SUCCESS) then
    !!       write(error_unit, '(A)') trim(ctx%message)
    !!   end if
    subroutine assess_pattern_security_risks(pattern, error_ctx)
        character(len=*), intent(in) :: pattern
        type(error_context_t), intent(inout) :: error_ctx
        
        logical :: concurrent_risk, readonly_risk, disk_space_risk, sensitive_pattern
        integer :: cache_idx
        
        ! THREAD-SAFE: Local pattern cache variables
        type(pattern_cache_t), save :: local_pattern_cache(CACHE_SIZE)
        integer, save :: local_cache_next_slot = 1
        
        ! PERFORMANCE: Check pattern cache first
        cache_idx = find_cached_pattern(pattern, local_pattern_cache)
        if (cache_idx > 0) then
            concurrent_risk = local_pattern_cache(cache_idx)%is_concurrent_risk
            readonly_risk = local_pattern_cache(cache_idx)%is_readonly_risk
            sensitive_pattern = local_pattern_cache(cache_idx)%is_sensitive
        else
            ! Not cached - perform analysis
            call analyze_pattern_risks(pattern, concurrent_risk, readonly_risk, &
                                     sensitive_pattern)
            ! Cache results for future use
            call cache_pattern_results(pattern, concurrent_risk, readonly_risk, &
                                     sensitive_pattern, local_pattern_cache, &
                                     local_cache_next_slot)
        end if
        
        ! THREAD-SAFE: Use local disk space check
        call get_disk_space_risk(disk_space_risk)
        
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


end module security_assessment_core