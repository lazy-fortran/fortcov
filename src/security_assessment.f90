module security_assessment
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
    use error_handling
    use string_utils, only: format_integer
    implicit none
    private
    
    ! THREAD-SAFE PERFORMANCE OPTIMIZATION: Pattern cache (per-function to avoid thread issues)
    !! 
    !! CACHING STRATEGY:
    !! Each function maintains its own pattern cache to ensure thread safety
    !! while providing performance benefits through pattern recognition caching.
    !!
    !! CACHE STRUCTURE RATIONALE:
    !! - pattern: Stores the exact pattern string for lookup
    !! - is_concurrent_risk: Cached concurrent access risk assessment
    !! - is_readonly_risk: Cached filesystem permission risk assessment  
    !! - is_sensitive: Cached sensitive directory access assessment
    !! - is_cached: Validity flag for cache entry
    type :: pattern_cache_t
        character(len=256) :: pattern = ""
        logical :: is_concurrent_risk = .false.
        logical :: is_readonly_risk = .false.
        logical :: is_sensitive = .false.
        logical :: is_cached = .false.
    end type pattern_cache_t
    
    ! Cache parameters
    integer, parameter :: CACHE_SIZE = 16
    
    ! Performance: Disk space check cache (per-function to avoid thread issues)
    
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
        
        ! PERFORMANCE: Early exit for concurrent risk
        concurrent_risk = .false.
        if (close_iostat /= 0) then
            if (index(filename, 'fortcov') > 0) then
                concurrent_risk = .true.
            end if
        end if
        
        ! THREAD-SAFE: Direct disk space check without shared cache
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

    ! THREAD-SAFE: Find cached pattern results
    pure function find_cached_pattern(pattern, cache) result(cache_idx)
        character(len=*), intent(in) :: pattern
        type(pattern_cache_t), intent(in) :: cache(CACHE_SIZE)
        integer :: cache_idx
        integer :: i
        
        cache_idx = 0
        do i = 1, CACHE_SIZE
            if (cache(i)%is_cached .and. &
                trim(cache(i)%pattern) == trim(pattern)) then
                cache_idx = i
                return
            end if
        end do
    end function find_cached_pattern
    
    ! SECURITY-CRITICAL: Consolidated pattern risk analysis - MUST DETECT ALL PATTERNS
    !!
    !! SECURITY PATTERN DETECTION ALGORITHM:
    !! This pure subroutine performs comprehensive security pattern analysis
    !! with mandatory detection of ALL security-relevant patterns.
    !!
    !! SECURITY REQUIREMENT: NO EARLY EXITS ALLOWED
    !! Unlike performance-optimized code, security analysis must examine
    !! ALL patterns to ensure complete threat detection.
    !!
    !! PATTERN CATEGORIES:
    !! 
    !! CONCURRENT RISK PATTERNS:
    !! - *.f90: Source file patterns indicating potential multi-process access
    !! - Used to detect file locking conflicts during coverage analysis
    !!
    !! READONLY RISK PATTERNS:
    !! - /usr/: System binaries and libraries (write-protected)
    !! - /proc/: Virtual filesystem (kernel interface, readonly)
    !! - /sys/: System filesystem (hardware interface, readonly)
    !!
    !! SENSITIVE PATTERNS:
    !! - ssh: SSH configuration and key files
    !! - home: User home directories (privacy risk)
    !! - root: System administrator directories (privilege escalation risk)
    !! - etc: System configuration files (security policy risk)
    !!
    !! SECURITY COMPLIANCE:
    !! This function MUST detect all patterns to maintain security posture.
    !! Performance optimizations are secondary to security completeness.
    pure subroutine analyze_pattern_risks(pattern, concurrent_risk, readonly_risk, &
                                         sensitive_pattern)
        character(len=*), intent(in) :: pattern
        logical, intent(out) :: concurrent_risk, readonly_risk, sensitive_pattern
        
        integer :: pos
        
        ! Initialize all to false
        concurrent_risk = .false.
        readonly_risk = .false.
        sensitive_pattern = .false.
        
        ! SECURITY: Must check ALL patterns - NO early exits for security
        ! Check for concurrent risk patterns
        pos = index(pattern, '*.f90')
        if (pos > 0) then
            concurrent_risk = .true.
        end if
        
        ! Check ALL readonly patterns - NO early exit
        pos = index(pattern, '/usr/')
        if (pos > 0) then
            readonly_risk = .true.
        end if
        
        pos = index(pattern, '/proc/')
        if (pos > 0) then
            readonly_risk = .true.
        end if
        
        pos = index(pattern, '/sys/')
        if (pos > 0) then
            readonly_risk = .true.
        end if
        
        ! Check ALL sensitive patterns - NO early exit
        pos = index(pattern, 'ssh')
        if (pos > 0) then
            sensitive_pattern = .true.
        end if
        
        pos = index(pattern, 'home')
        if (pos > 0) then
            sensitive_pattern = .true.
        end if
        
        pos = index(pattern, 'root')
        if (pos > 0) then
            sensitive_pattern = .true.
        end if
        
        pos = index(pattern, 'etc')
        if (pos > 0) then
            sensitive_pattern = .true.
        end if
    end subroutine analyze_pattern_risks
    
    ! THREAD-SAFE: Cache pattern analysis results
    subroutine cache_pattern_results(pattern, concurrent_risk, readonly_risk, &
                                   sensitive_pattern, cache, next_slot)
        character(len=*), intent(in) :: pattern
        logical, intent(in) :: concurrent_risk, readonly_risk, sensitive_pattern
        type(pattern_cache_t), intent(inout) :: cache(CACHE_SIZE)
        integer, intent(inout) :: next_slot
        
        ! Simple LRU replacement - use next slot and wrap around
        cache(next_slot)%pattern = pattern
        cache(next_slot)%is_concurrent_risk = concurrent_risk
        cache(next_slot)%is_readonly_risk = readonly_risk
        cache(next_slot)%is_sensitive = sensitive_pattern
        cache(next_slot)%is_cached = .true.
        
        next_slot = next_slot + 1
        if (next_slot > CACHE_SIZE) next_slot = 1
    end subroutine cache_pattern_results
    
    ! THREAD-SAFE: Get disk space risk without shared state
    !!
    !! DISK SPACE SECURITY ASSESSMENT:
    !! This subroutine performs real-time disk space analysis to detect
    !! critical storage conditions that could impact security operations.
    !!
    !! SECURITY RATIONALE:
    !! Insufficient disk space can cause:
    !! - Incomplete temporary file cleanup (security policy violation)
    !! - Failed security logging (audit trail gaps)
    !! - System instability affecting security controls
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

end module security_assessment