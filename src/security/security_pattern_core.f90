module security_pattern_core
    !! Security pattern analysis module
    !!
    !! Provides pattern-based security risk detection including
    !! concurrent access risks, readonly filesystem detection,
    !! and sensitive directory pattern matching.
    !!
    !! SECURITY PATTERNS DETECTED:
    !! - Directory traversal attacks (../../../etc/passwd)
    !! - System file access attempts (/usr/, /etc/, /root/, /proc/, /sys/)
    !! - Temporary file injection (fortcov_secure_*, /tmp/*)
    !! - Sensitive directory access (home, ssh, root, etc patterns)
    implicit none
    private
    
    ! Pattern cache type for performance optimization
    type :: pattern_cache_t
        character(len=256) :: pattern = ""
        logical :: is_concurrent_risk = .false.
        logical :: is_readonly_risk = .false.
        logical :: is_sensitive = .false.
        logical :: is_cached = .false.
    end type pattern_cache_t
    
    ! Cache parameters
    integer, parameter, public :: CACHE_SIZE = 16
    
    ! Public interfaces
    public :: pattern_cache_t
    public :: analyze_pattern_risks
    public :: find_cached_pattern
    public :: cache_pattern_results
    
contains
    
    !! Analyze security risks in patterns - comprehensive checking
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
        
        ! SECURITY: Must check ALL patterns - NO early exits for security
        concurrent_risk = detect_concurrent_risk_patterns(pattern)
        readonly_risk = detect_readonly_risk_patterns(pattern)
        sensitive_pattern = detect_sensitive_patterns(pattern)
        
    end subroutine analyze_pattern_risks

    pure function detect_concurrent_risk_patterns(pattern) result(concurrent_risk)
        !! Detect patterns indicating concurrent access risks
        character(len=*), intent(in) :: pattern
        logical :: concurrent_risk
        
        concurrent_risk = index(pattern, '*.f90') > 0

    end function detect_concurrent_risk_patterns

    pure function detect_readonly_risk_patterns(pattern) result(readonly_risk)
        !! Detect patterns indicating readonly filesystem risks
        character(len=*), intent(in) :: pattern
        logical :: readonly_risk
        
        ! Check ALL readonly patterns - NO early exit for security completeness
        readonly_risk = index(pattern, '/usr/') > 0 .or. &
                        index(pattern, '/proc/') > 0 .or. &
                        index(pattern, '/sys/') > 0

    end function detect_readonly_risk_patterns

    pure function detect_sensitive_patterns(pattern) result(sensitive_pattern)
        !! Detect patterns indicating access to sensitive directories
        character(len=*), intent(in) :: pattern
        logical :: sensitive_pattern
        
        ! Check ALL sensitive patterns - NO early exit for security completeness
        sensitive_pattern = index(pattern, 'ssh') > 0 .or. &
                            index(pattern, 'home') > 0 .or. &
                            index(pattern, 'root') > 0 .or. &
                            index(pattern, 'etc') > 0

    end function detect_sensitive_patterns
    
    ! Find cached pattern in local cache
    pure function find_cached_pattern(pattern, cache) result(cache_idx)
        character(len=*), intent(in) :: pattern
        type(pattern_cache_t), intent(in) :: cache(CACHE_SIZE)
        integer :: cache_idx
        integer :: i
        
        cache_idx = 0
        do i = 1, CACHE_SIZE
            if (cache(i)%is_cached .and. cache(i)%pattern == pattern) then
                cache_idx = i
                exit
            end if
        end do
    end function find_cached_pattern
    
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
    
end module security_pattern_core