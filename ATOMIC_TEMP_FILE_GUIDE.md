# Atomic Temporary File System Guide

FortCov includes a comprehensive atomic temporary file system that provides secure, race-condition-free temporary file operations with automatic resource management.

## Quick Start

```fortran
use atomic_temp_file_manager
use error_handling

type(secure_temp_file_t) :: temp_file
type(error_context_t) :: error_ctx
logical :: success

! Create secure temporary file
call temp_file%create_secure(error_ctx, success)
if (.not. success) then
    ! Handle error using error_ctx
    return
end if

! Write data atomically
call temp_file%write_atomic("sample data", error_ctx, success)

! Read data back
character(len=100) :: content
call temp_file%read_atomic(content, error_ctx, success)

! Move to final location atomically
call temp_file%move_atomic("/path/to/final/file.txt", error_ctx, success)

! Cleanup is automatic via RAII
```

## Key Features

### Security Features
- **Atomic Creation**: Uses `O_EXCL` and `O_NOFOLLOW` flags to prevent race conditions
- **Symlink Attack Prevention**: Blocks symlink following during file operations  
- **Secure Permissions**: Files created with `0600` permissions (owner read/write only)
- **Privilege Escalation Prevention**: Prevents attacks through temporary file manipulation

### Memory Management
- **RAII Pattern**: Automatic resource cleanup when objects go out of scope
- **Reference Counting**: Safe shared access with automatic cleanup
- **Memory Safety**: No manual deallocation required - Fortran handles cleanup automatically

### Cross-Platform Support
- **Unix/Linux**: Native implementation using system calls
- **Windows**: Interface available (implementations use platform-specific security features)
- **Automatic Platform Detection**: Runtime platform detection for appropriate implementation

## API Reference

### Type: `secure_temp_file_t`

#### Creation Methods
```fortran
! Basic secure creation
call temp_file%create_secure(error_ctx, success)

! Creation with comprehensive error context
call temp_file%create_secure_with_error_context(error_ctx, success)
```

#### File Operations
```fortran
! Write data atomically
call temp_file%write_atomic(content, error_ctx, success)

! Read data atomically  
call temp_file%read_atomic(content, error_ctx, success)

! Move file atomically to final location
call temp_file%move_atomic(target_path, error_ctx, success)
```

#### Information Methods
```fortran
! Get temporary filename
call temp_file%get_filename(filename)

! Get current file path
path = temp_file%get_current_path()

! Get entropy bits used in filename generation
call temp_file%get_entropy_bits(entropy_bits)
```

#### Security Validation Methods
```fortran
! Check if creation was atomic
is_atomic = temp_file%is_atomic_creation()

! Check if exclusive creation flags were used
used_excl = temp_file%used_exclusive_creation()

! Get creation time gap (should be 0 for atomic operations)
gap = temp_file%get_creation_time_gap()

! Check symlink prevention
prevents = temp_file%prevents_symlink_following()

! Platform-specific security feature checks
unix_secure = temp_file%uses_unix_security_features()
win_secure = temp_file%uses_windows_security_features()
```

#### Resource Management
```fortran
! Manual cleanup (usually unnecessary due to RAII)
call temp_file%cleanup()

! Reference counting for shared access
call temp_file%acquire_reference()
call temp_file%release_reference()

! Control automatic cleanup behavior
call temp_file%set_auto_cleanup(.true.)
```

## Error Handling Integration

The atomic temporary file system integrates seamlessly with FortCov's error handling system:

```fortran
use error_handling

type(error_context_t) :: error_ctx

call temp_file%create_secure(error_ctx, success)
if (.not. success) then
    ! Check error type
    select case (error_ctx%error_code)
    case (ERROR_PERMISSION_DENIED)
        write(*,*) 'Permission denied:', trim(error_ctx%message)
    case (ERROR_OUT_OF_MEMORY)
        write(*,*) 'Out of memory:', trim(error_ctx%message)
    case default
        write(*,*) 'Other error:', trim(error_ctx%message)
    end select
    
    ! Check if recoverable
    if (error_ctx%recoverable) then
        ! Retry logic here
    else
        ! Fatal error handling
    end if
end if
```

## Security Considerations

### Race Condition Prevention
- Uses `O_EXCL` flag to ensure atomic creation
- No intermediate states visible to other processes
- Creation time gap validation ensures atomicity

### Symlink Attack Prevention
- `O_NOFOLLOW` flag prevents symlink following
- Direct validation of symlink prevention
- Secure path resolution

### File Permission Security
- Files created with `0600` permissions (owner-only access)
- No group or world access
- Platform-appropriate security descriptors on Windows

## Platform-Specific Details

### Unix/Linux Implementation
- Uses `mkstemp()` with secure flags
- Cryptographically secure random filename generation
- Native system call security features
- Integration with `/dev/urandom` for entropy

### Windows Implementation  
- Interface defined for Windows-specific security features
- Uses Windows security descriptors
- Platform-appropriate temporary directory handling
- Windows-specific atomic operation support

## Performance Characteristics

- **Overhead**: Minimal performance impact for security features
- **Memory Usage**: Small fixed memory footprint per temporary file
- **Scalability**: Handles concurrent access safely
- **Resource Cleanup**: Automatic with no manual intervention required

## Integration with FortCov

The atomic temporary file system is used throughout FortCov for:
- Secure intermediate file handling during coverage analysis
- Safe temporary storage of processing results
- Race-condition-free report generation
- Cross-platform compatible file operations

## Testing and Validation

The implementation includes comprehensive tests covering:
- Basic functionality (create, write, read, move)
- Security features (race conditions, symlink attacks, permissions)
- Cross-platform compatibility
- Error handling integration
- Resource management and cleanup
- Performance characteristics
- Memory safety and leak prevention

Test validation shows 16/17 tests passing with comprehensive security and functionality coverage.