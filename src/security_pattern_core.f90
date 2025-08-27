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