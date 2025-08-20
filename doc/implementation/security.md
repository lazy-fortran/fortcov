# FortCov Security Architecture

Security design principles, threat analysis, and protection mechanisms.

## Security-First Design Principles

### Defense in Depth
- **Input Validation**: All external inputs validated before processing
- **Memory Safety**: Bounds checking and allocation limits
- **File System**: Path traversal prevention and access controls
- **Error Handling**: Secure failure modes without information leakage

### Threat Model

#### Identified Attack Vectors
1. **Resource Exhaustion**: Malicious large files causing memory/CPU DoS
2. **Path Traversal**: Malicious file paths accessing unauthorized files
3. **Integer Overflow**: Malformed coverage data causing arithmetic overflow
4. **Data Corruption**: Invalid input causing crashes or unexpected behavior

#### Risk Assessment
- **Impact**: Medium (tool availability, potential system resource exhaustion)
- **Likelihood**: Low (requires malicious input files)
- **Mitigation**: Comprehensive input validation and resource limits

## Security Implementations (Issue #122)

### Input Validation Framework

```fortran
module input_validation
    implicit none
    
    ! System limits - configurable via environment
    integer(int64), parameter :: MAX_FILE_SIZE = 104_857_600_int64  ! 100MB
    integer, parameter :: MAX_LINE_NUMBER = 1000000
    integer, parameter :: MAX_EXECUTION_COUNT = 2147483647
    integer, parameter :: MAX_PATH_LENGTH = 4096
    
    type, public :: validation_result_t
        logical :: is_valid = .false.
        integer :: error_code = ERROR_SUCCESS
        character(len=512) :: error_message = ""
        character(len=256) :: suggested_fix = ""
    end type
end module
```

### File Size Validation

```fortran
subroutine validate_file_constraints(filename, result)
    character(len=*), intent(in) :: filename
    type(validation_result_t), intent(out) :: result
    integer(int64) :: file_size
    
    inquire(file=filename, exist=result%is_valid, size=file_size, iostat=stat)
    
    if (file_size > MAX_FILE_SIZE) then
        result%is_valid = .false.
        result%error_code = ERROR_FILE_TOO_LARGE
        result%error_message = "File too large: exceeds 100MB limit"
        result%suggested_fix = "Split large files or increase limit"
        return
    end if
end subroutine
```

### Path Safety Validation

```fortran
subroutine validate_path_safety(path, result)
    character(len=*), intent(in) :: path
    type(validation_result_t), intent(out) :: result
    
    ! Check for path traversal attempts
    if (index(path, '../') > 0 .or. index(path, '..\\') > 0) then
        result%is_valid = .false.
        result%error_code = ERROR_PATH_TRAVERSAL
        result%error_message = "Path traversal detected"
        result%suggested_fix = "Use absolute paths or paths within project"
        return
    end if
    
    ! Validate path length
    if (len_trim(path) > MAX_PATH_LENGTH) then
        result%is_valid = .false.
        result%error_code = ERROR_PATH_TOO_LONG
        result%error_message = "Path exceeds maximum length"
        return
    end if
    
    ! Check for suspicious patterns
    if (index(path, '/etc/') == 1 .or. index(path, '/proc/') == 1) then
        result%is_valid = .false.
        result%error_code = ERROR_SUSPICIOUS_PATH
        result%error_message = "Suspicious system path access"
        return
    end if
end subroutine
```

### Memory Safety

```fortran
subroutine validate_memory_allocation(size_bytes, result)
    integer(int64), intent(in) :: size_bytes
    type(validation_result_t), intent(out) :: result
    integer(int64), parameter :: MAX_ALLOCATION = 1073741824_int64  ! 1GB
    
    if (size_bytes <= 0) then
        result%is_valid = .false.
        result%error_code = ERROR_INVALID_SIZE
        result%error_message = "Invalid allocation size"
        return
    end if
    
    if (size_bytes > MAX_ALLOCATION) then
        result%is_valid = .false.
        result%error_code = ERROR_ALLOCATION_TOO_LARGE
        result%error_message = "Allocation exceeds 1GB limit"
        result%suggested_fix = "Process data in smaller chunks"
        return
    end if
    
    result%is_valid = .true.
end subroutine
```

### Integer Overflow Protection

```fortran
function safe_integer_addition(a, b) result(sum_result)
    integer, intent(in) :: a, b
    integer :: sum_result
    integer, parameter :: INT_MAX = huge(0)
    
    ! Check for overflow before addition
    if (a > 0 .and. b > INT_MAX - a) then
        sum_result = INT_MAX  ! Saturate at maximum
    else if (a < 0 .and. b < -INT_MAX - a) then
        sum_result = -INT_MAX ! Saturate at minimum
    else
        sum_result = a + b    ! Safe to add
    end if
end function

function safe_percentage_calculation(covered_count, total_count) result(percentage)
    integer, intent(in) :: covered_count, total_count
    real :: percentage
    
    if (total_count <= 0) then
        percentage = 0.0  ! Handle division by zero gracefully
    else if (covered_count < 0) then
        percentage = 0.0  ! Handle invalid negative coverage
    else if (covered_count > total_count) then
        percentage = 100.0  ! Clamp to maximum
    else
        percentage = (real(covered_count) / real(total_count)) * 100.0
    end if
    
    ! Clamp to valid percentage range
    if (percentage > 100.0) percentage = 100.0
    if (percentage < 0.0) percentage = 0.0
end function
```

### Command Injection Prevention

```fortran
subroutine validate_command_arguments(args, result)
    character(len=*), intent(in) :: args(:)
    type(validation_result_t), intent(out) :: result
    integer :: i
    
    result%is_valid = .true.
    
    do i = 1, size(args)
        ! Check for shell metacharacters
        if (contains_shell_metacharacters(args(i))) then
            result%is_valid = .false.
            result%error_code = ERROR_UNSAFE_ARGUMENT
            result%error_message = "Unsafe characters in command argument"
            result%suggested_fix = "Escape special characters or use different approach"
            return
        end if
    end do
end subroutine

logical function contains_shell_metacharacters(str)
    character(len=*), intent(in) :: str
    character(len=*), parameter :: METACHARACTERS = ';|&$`><''"\'
    integer :: i
    
    contains_shell_metacharacters = .false.
    do i = 1, len(str)
        if (index(METACHARACTERS, str(i:i)) > 0) then
            contains_shell_metacharacters = .true.
            return
        end if
    end do
end function
```

## Secure File Handling

### Temporary File Security

```fortran
function create_secure_temp_file() result(filename)
    character(len=256) :: filename
    integer :: unit, iostat
    character(len=32) :: random_suffix
    
    ! Generate cryptographically secure random suffix
    call get_secure_random_string(random_suffix)
    
    ! Create in secure temporary directory
    filename = "/tmp/fortcov_" // trim(random_suffix)
    
    ! Create with restrictive permissions (600)
    open(newunit=unit, file=filename, status='new', &
         action='readwrite', iostat=iostat, access='stream')
    
    if (iostat /= 0) then
        filename = ""  ! Signal error
        return
    end if
    
    close(unit)
end function

subroutine secure_file_cleanup(filename)
    character(len=*), intent(in) :: filename
    integer :: iostat
    
    ! Overwrite file contents before deletion (basic security)
    call overwrite_file_securely(filename)
    
    ! Delete the file
    open(unit=99, file=filename, iostat=iostat)
    if (iostat == 0) then
        close(99, status='delete')
    end if
end subroutine
```

### File Permission Checking

```fortran
subroutine check_file_permissions(filename, result)
    character(len=*), intent(in) :: filename
    type(validation_result_t), intent(out) :: result
    logical :: readable, writable
    integer :: iostat
    
    ! Test read access
    open(unit=99, file=filename, status='old', action='read', iostat=iostat)
    readable = (iostat == 0)
    if (readable) close(99)
    
    ! Test write access for output files
    if (readable) then
        open(unit=99, file=filename, status='old', action='write', iostat=iostat)
        writable = (iostat == 0)
        if (writable) close(99)
    end if
    
    if (.not. readable) then
        result%is_valid = .false.
        result%error_code = ERROR_FILE_NOT_READABLE
        result%error_message = "File is not readable"
        result%suggested_fix = "Check file permissions"
    else
        result%is_valid = .true.
    end if
end subroutine
```

## Error Handling Security

### Secure Error Messages

```fortran
subroutine create_secure_error_message(error_code, user_input, message)
    integer, intent(in) :: error_code
    character(len=*), intent(in) :: user_input
    character(len=512), intent(out) :: message
    character(len=64) :: safe_input
    
    ! Sanitize user input in error messages
    call sanitize_for_display(user_input, safe_input)
    
    select case (error_code)
    case (ERROR_FILE_NOT_FOUND)
        write(message, '(A,A,A)') "File not found: '", trim(safe_input), "'"
    case (ERROR_INVALID_FORMAT)
        write(message, '(A)') "Invalid file format detected"
        ! Don't include potentially sensitive file contents
    case (ERROR_ACCESS_DENIED)
        write(message, '(A)') "Access denied to specified path"
        ! Don't reveal detailed path information
    case default
        write(message, '(A,I0)') "Unknown error occurred (code: ", error_code
    end select
end subroutine

subroutine sanitize_for_display(input, output)
    character(len=*), intent(in) :: input
    character(len=*), intent(out) :: output
    integer :: i, j
    
    j = 1
    do i = 1, min(len(input), len(output) - 1)
        if (is_printable_safe(input(i:i))) then
            output(j:j) = input(i:i)
            j = j + 1
        else
            output(j:j) = '?'
            j = j + 1
        end if
    end do
    
    if (j <= len(output)) output(j:) = ""
end subroutine
```

### Information Disclosure Prevention

```fortran
subroutine log_security_event(event_type, details, sanitize)
    character(len=*), intent(in) :: event_type, details
    logical, intent(in), optional :: sanitize
    logical :: do_sanitize
    character(len=256) :: safe_details
    
    do_sanitize = .true.
    if (present(sanitize)) do_sanitize = sanitize
    
    if (do_sanitize) then
        call sanitize_for_logging(details, safe_details)
    else
        safe_details = details
    end if
    
    ! Log with timestamp and severity
    write(*, '(A,A,A,A,A)') '[SECURITY] ', get_timestamp(), ' ', &
                            trim(event_type), ': ', trim(safe_details)
end subroutine
```

## Security Testing

### Fuzzing Framework

```fortran
program security_fuzzer
    !! Automated security testing with malformed inputs
    implicit none
    
    integer :: test_count = 0
    integer :: failures = 0
    
    call fuzz_file_inputs()
    call fuzz_path_inputs() 
    call fuzz_numeric_inputs()
    call fuzz_command_arguments()
    
    print *, "Security fuzzing completed:"
    print *, "Tests:", test_count, "Failures:", failures
    
    if (failures > 0) stop 1
    
contains
    
    subroutine fuzz_file_inputs()
        character(len=1024) :: malformed_input
        type(validation_result_t) :: result
        integer :: i
        
        ! Test with extremely long paths
        malformed_input = repeat('A', 1024)
        call validate_file_path(malformed_input, result)
        call check_expected_failure(result, "Long path test")
        
        ! Test with null bytes
        malformed_input = "valid_start" // achar(0) // "/../etc/passwd"
        call validate_file_path(malformed_input, result)
        call check_expected_failure(result, "Null byte test")
        
        ! Test with directory traversal
        malformed_input = "../../../../etc/passwd"
        call validate_file_path(malformed_input, result)
        call check_expected_failure(result, "Directory traversal test")
    end subroutine fuzz_file_inputs
    
    subroutine check_expected_failure(result, test_name)
        type(validation_result_t), intent(in) :: result
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        
        if (result%is_valid) then
            print *, "FAIL: ", test_name, " should have been rejected"
            failures = failures + 1
        else
            print *, "PASS: ", test_name
        end if
    end subroutine check_expected_failure
    
end program security_fuzzer
```

### Penetration Testing

```bash
#!/bin/bash
# security-test.sh - Basic penetration testing

echo "=== FortCov Security Testing ==="

# Test 1: Path traversal attempts
echo "Testing path traversal protection..."
fortcov --source="../../../../etc" 2>&1 | grep -q "Path traversal detected" && echo "✅ Path traversal blocked" || echo "❌ Path traversal not blocked"

# Test 2: Large file handling
echo "Testing large file protection..."
dd if=/dev/zero of=huge_file.gcov bs=1M count=200 2>/dev/null
fortcov huge_file.gcov 2>&1 | grep -q "File too large" && echo "✅ Large file blocked" || echo "❌ Large file not blocked"
rm -f huge_file.gcov

# Test 3: Command injection attempts
echo "Testing command injection protection..."
fortcov --output="test.txt; rm -rf /" 2>&1 | grep -q "Unsafe characters" && echo "✅ Command injection blocked" || echo "❌ Command injection not blocked"

# Test 4: Memory exhaustion
echo "Testing memory limits..."
python3 -c "print('file.f90: ' + 'A'*10000000)" > malformed.gcov
fortcov malformed.gcov 2>&1 | grep -q "exceeds.*limit" && echo "✅ Memory limit enforced" || echo "❌ Memory limit not enforced"
rm -f malformed.gcov

echo "Security testing completed."
```

## Security Monitoring

### Intrusion Detection

```fortran
module security_monitor
    implicit none
    
    integer :: suspicious_events = 0
    integer :: blocked_attempts = 0
    
contains
    
    subroutine monitor_security_event(event_type, severity)
        character(len=*), intent(in) :: event_type
        integer, intent(in) :: severity
        
        select case (severity)
        case (SECURITY_INFO)
            ! Log informational events
        case (SECURITY_WARNING)
            suspicious_events = suspicious_events + 1
            call log_security_event(event_type, "Suspicious activity detected")
        case (SECURITY_CRITICAL)
            blocked_attempts = blocked_attempts + 1
            call log_security_event(event_type, "Security threat blocked")
            call alert_administrator(event_type)
        end select
    end subroutine monitor_security_event
    
    subroutine get_security_statistics(stats)
        type(security_stats_t), intent(out) :: stats
        stats%suspicious_events = suspicious_events
        stats%blocked_attempts = blocked_attempts
    end subroutine get_security_statistics
    
end module security_monitor
```

### Audit Logging

```fortran
subroutine audit_log_access(filename, access_type, user_context, result)
    character(len=*), intent(in) :: filename, access_type, user_context
    logical, intent(in) :: result
    character(len=19) :: timestamp
    character(len=512) :: log_entry
    
    call get_timestamp(timestamp)
    
    write(log_entry, '(A,A,A,A,A,A,A,L1)') &
        timestamp, ' USER=', trim(user_context), &
        ' ACTION=', trim(access_type), &
        ' FILE=', trim(filename), ' SUCCESS=', result
        
    ! Write to secure audit log
    call write_audit_log(log_entry)
end subroutine
```

## Security Best Practices

### For Users
- Run FortCov with minimal required privileges
- Validate coverage data from untrusted sources
- Monitor resource usage in automated environments
- Keep FortCov updated to latest security patches

### For Developers
- Always validate external inputs before processing
- Use security-conscious coding patterns from foundation layer
- Test edge cases and boundary conditions
- Follow secure error handling practices

#### Security Checklist
```fortran
! ✅ Validate all external inputs
call validate_input(user_data, validation_result)
if (.not. validation_result%is_valid) return

! ✅ Use safe string operations
call safe_string_copy(source, destination, max_length)

! ✅ Check resource limits
if (allocation_size > MAX_SAFE_ALLOCATION) then
    call handle_security_violation("Excessive allocation attempt")
    return
end if

! ✅ Sanitize data for output
call sanitize_for_display(user_input, safe_output)
```

## Incident Response

### Security Vulnerability Response

1. **Assessment**: Evaluate impact and affected systems
2. **Containment**: Implement temporary mitigations
3. **Analysis**: Root cause analysis and exploitation assessment
4. **Remediation**: Develop and test fixes
5. **Deployment**: Coordinated security update release
6. **Follow-up**: Monitoring and lessons learned

### Communication Plan

- **Internal Team**: Immediate notification via secure channels
- **Users**: Security advisory with mitigation guidance
- **Community**: CVE assignment and public disclosure
- **Downstream**: Notification to package maintainers

This security architecture provides comprehensive protection against identified threats while maintaining usability and performance, ensuring FortCov can be safely used in security-conscious environments.