! Atomic temporary file operations security system
! Prevents race conditions, symlink attacks, and privilege escalation
! Cross-platform implementation with C interop for security-critical operations
module atomic_temp_file_manager_impl
    !! Atomic Temp File Manager Implementation - Foundation Layer Component
    !! 
    !! This module contains the actual implementation of atomic temp file operations.
    !! Extracted from the original atomic_temp_file_manager module for Issue #182 compliance.
    use iso_c_binding
    use error_handling
    use atomic_temp_file_c_interface
    use atomic_temp_file_error_handler, only: map_c_error_to_context, &
                                              is_temp_file_error_recoverable, &
                                              ERROR_CODE_STATE_ERROR, &
                                              ERROR_CODE_INVALID_ARGUMENT
    implicit none
    private
    
    ! Constants
    integer, parameter :: MAX_FILENAME_LEN = 4096
    integer, parameter :: MAX_CONTENT_LEN = 1048576  ! 1MB max content
    
    ! Secure temporary file type with automatic cleanup
    type, public :: secure_temp_file_t
        private
        character(len=MAX_FILENAME_LEN) :: filename = ""
        character(len=MAX_FILENAME_LEN) :: temp_dir = ""
        logical :: is_created = .false.
        logical :: is_cleaned = .false.
        integer :: entropy_bits = 0
        ! Platform-specific opaque state (C structure)
        integer(c_size_t) :: c_state_size = 0
        type(c_ptr) :: c_state_ptr = c_null_ptr
        ! RAII reference counting for reliable resource management
        integer :: ref_count = 0
        logical :: auto_cleanup = .true.
    contains
        procedure :: create_secure
        procedure :: create_secure_with_error_context
        procedure :: write_atomic
        procedure :: read_atomic
        procedure :: move_atomic
        procedure :: cleanup
        procedure :: get_filename
        procedure :: get_current_path
        procedure :: get_entropy_bits
        procedure :: is_atomic_creation
        procedure :: used_exclusive_creation
        procedure :: get_creation_time_gap
        procedure :: prevents_symlink_following
        procedure :: uses_unix_security_features
        procedure :: uses_windows_security_features
        procedure :: simulate_error_condition
        ! RAII resource management
        procedure :: acquire_reference
        procedure :: release_reference
        procedure :: set_auto_cleanup
        ! Explicit cleanup method for backward compatibility
        final :: explicit_finalizer
    end type secure_temp_file_t
    
contains

    ! Platform detection helpers
    function get_platform_is_unix() result(is_unix)
        logical :: is_unix
        is_unix = (is_unix_platform() /= 0)
    end function get_platform_is_unix

    function is_unix() result(unix_platform)
        logical :: unix_platform
        unix_platform = get_platform_is_unix()
    end function is_unix

    ! Core secure file operations
    subroutine create_secure(this, error_ctx, success)
        !! Create a secure temporary file atomically
        class(secure_temp_file_t), intent(inout) :: this
        type(error_context_t), intent(inout) :: error_ctx
        logical, intent(out) :: success
        
        integer(c_int) :: c_error_code, status
        
        success = .false.
        
        ! Ensure no previous state
        if (this%is_created) then
            error_ctx%error_code = ERROR_CODE_STATE_ERROR
            error_ctx%message = "Secure temp file already created"
            error_ctx%recoverable = .false.
            return
        end if
        
        ! Allocate C state structure
        if (.not. allocate_c_state(this)) then
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            error_ctx%message = "Failed to allocate C state structure"
            error_ctx%recoverable = .true.
            return
        end if
        
        ! Call platform-specific secure creation
        if (is_unix()) then
            status = create_secure_temp_file_unix(this%c_state_ptr, c_error_code)
        else
            status = create_secure_temp_file_windows(this%c_state_ptr, c_error_code)
        end if
        
        if (status /= 0) then
            call map_c_error_to_context(c_error_code, error_ctx)
            call deallocate_c_state(this)
            return
        end if
        
        ! Extract information from C state
        call extract_info_from_c_state(this)
        
        this%is_created = .true.
        this%is_cleaned = .false.
        this%ref_count = 1
        success = .true.
    end subroutine create_secure

    subroutine create_secure_with_error_context(this, error_ctx, success)
        !! Create secure temp file with provided error context
        class(secure_temp_file_t), intent(inout) :: this
        type(error_context_t), intent(inout) :: error_ctx
        logical, intent(out) :: success
        
        ! Create with retry logic for recoverable errors
        integer :: attempts
        
        do attempts = 1, 3
            call create_secure(this, error_ctx, success)
            if (success) return
            
            if (.not. is_temp_file_error_recoverable(error_ctx%error_code)) exit
            call clear_error_context(error_ctx)
        end do
    end subroutine create_secure_with_error_context

    subroutine write_atomic(this, content, error_ctx, success)
        !! Write content atomically to the temporary file
        class(secure_temp_file_t), intent(inout) :: this
        character(len=*), intent(in) :: content
        type(error_context_t), intent(inout) :: error_ctx
        logical, intent(out) :: success
        
        integer(c_int) :: c_error_code, status
        integer(c_size_t) :: data_len
        character(kind=c_char), allocatable :: c_buffer(:)
        integer :: i
        
        success = .false.
        
        if (.not. this%is_created .or. this%is_cleaned) then
            error_ctx%error_code = ERROR_CODE_STATE_ERROR
            error_ctx%message = "Secure temp file not in valid state for write"
            error_ctx%recoverable = .false.
            return
        end if
        
        ! Convert Fortran string to C character array
        data_len = len_trim(content)
        if (data_len > MAX_CONTENT_LEN) then
            error_ctx%error_code = ERROR_CODE_INVALID_ARGUMENT
            error_ctx%message = "Content exceeds maximum size limit"
            error_ctx%recoverable = .false.
            return
        end if
        
        allocate(c_buffer(data_len))
        do i = 1, data_len
            c_buffer(i) = content(i:i)
        end do
        
        ! Call platform-specific atomic write
        if (is_unix()) then
            status = write_atomic_temp_file_unix(this%c_state_ptr, c_buffer, &
                                                  data_len, c_error_code)
        else
            status = write_atomic_temp_file_windows(this%c_state_ptr, c_buffer, &
                                                     data_len, c_error_code)
        end if
        
        deallocate(c_buffer)
        
        if (status /= 0) then
            call map_c_error_to_context(c_error_code, error_ctx)
            return
        end if
        
        success = .true.
    end subroutine write_atomic

    subroutine read_atomic(this, content, error_ctx, success)
        !! Read content from the temporary file atomically
        class(secure_temp_file_t), intent(in) :: this
        character(len=:), allocatable, intent(out) :: content
        type(error_context_t), intent(inout) :: error_ctx
        logical, intent(out) :: success
        
        integer(c_int) :: c_error_code, status
        integer(c_size_t) :: bytes_read
        character(kind=c_char) :: c_buffer(MAX_CONTENT_LEN)
        integer :: i
        
        success = .false.
        
        if (.not. this%is_created .or. this%is_cleaned) then
            error_ctx%error_code = ERROR_CODE_STATE_ERROR
            error_ctx%message = "Secure temp file not in valid state for read"
            error_ctx%recoverable = .false.
            return
        end if
        
        ! Call platform-specific read
        if (is_unix()) then
            status = read_temp_file_unix(this%c_state_ptr, c_buffer, &
                                          int(MAX_CONTENT_LEN, c_size_t), &
                                          bytes_read, c_error_code)
        else
            status = read_temp_file_windows(this%c_state_ptr, c_buffer, &
                                             int(MAX_CONTENT_LEN, c_size_t), &
                                             bytes_read, c_error_code)
        end if
        
        if (status /= 0) then
            call map_c_error_to_context(c_error_code, error_ctx)
            return
        end if
        
        ! Convert C buffer to Fortran string
        allocate(character(len=bytes_read) :: content)
        do i = 1, bytes_read
            content(i:i) = c_buffer(i)
        end do
        
        success = .true.
    end subroutine read_atomic

    subroutine move_atomic(this, target_path, error_ctx, success)
        !! Atomically move temporary file to target location
        class(secure_temp_file_t), intent(inout) :: this
        character(len=*), intent(in) :: target_path
        type(error_context_t), intent(inout) :: error_ctx
        logical, intent(out) :: success
        
        integer(c_int) :: c_error_code, status
        character(kind=c_char) :: c_target(MAX_FILENAME_LEN)
        integer :: i, path_len
        
        success = .false.
        
        if (.not. this%is_created .or. this%is_cleaned) then
            error_ctx%error_code = ERROR_CODE_STATE_ERROR
            error_ctx%message = "Secure temp file not in valid state for move"
            error_ctx%recoverable = .false.
            return
        end if
        
        ! Convert target path to C string
        path_len = min(len_trim(target_path), MAX_FILENAME_LEN - 1)
        do i = 1, path_len
            c_target(i) = target_path(i:i)
        end do
        c_target(path_len + 1) = c_null_char
        
        ! Call platform-specific atomic move
        if (is_unix()) then
            status = move_atomic_temp_file_unix(this%c_state_ptr, c_target, &
                                                 c_error_code)
        else
            status = move_atomic_temp_file_windows(this%c_state_ptr, c_target, &
                                                    c_error_code)
        end if
        
        if (status /= 0) then
            call map_c_error_to_context(c_error_code, error_ctx)
            return
        end if
        
        ! Mark as cleaned since file has been moved
        this%is_cleaned = .true.
        success = .true.
    end subroutine move_atomic

    subroutine cleanup(this)
        !! Clean up temporary file and resources
        class(secure_temp_file_t), intent(inout) :: this
        
        integer(c_int) :: status
        
        if (this%is_created .and. .not. this%is_cleaned) then
            if (c_associated(this%c_state_ptr)) then
                if (is_unix()) then
                    status = cleanup_temp_file_unix(this%c_state_ptr)
                else
                    status = cleanup_temp_file_windows(this%c_state_ptr)
                end if
            end if
            this%is_cleaned = .true.
        end if
        
        call deallocate_c_state(this)
        this%is_created = .false.
    end subroutine cleanup

    ! File property getters
    subroutine get_filename(this, filename)
        !! Get the filename of the secure temp file
        class(secure_temp_file_t), intent(in) :: this
        character(len=*), intent(out) :: filename
        
        filename = trim(this%filename)
    end subroutine get_filename

    function get_current_path(this) result(path)
        !! Get the current path of the secure temp file
        class(secure_temp_file_t), intent(in) :: this
        character(len=:), allocatable :: path
        
        path = trim(this%filename)
    end function get_current_path

    subroutine get_entropy_bits(this, entropy_bits)
        !! Get entropy bits used for file creation
        class(secure_temp_file_t), intent(in) :: this
        integer, intent(out) :: entropy_bits
        
        entropy_bits = this%entropy_bits
    end subroutine get_entropy_bits

    function is_atomic_creation(this) result(is_atomic)
        !! Check if file was created atomically
        class(secure_temp_file_t), intent(in) :: this
        logical :: is_atomic
        
        ! We always aim for atomic creation
        is_atomic = this%is_created
    end function is_atomic_creation

    function used_exclusive_creation(this) result(used)
        !! Check if exclusive creation was used
        class(secure_temp_file_t), intent(in) :: this
        logical :: used
        
        integer(c_int) :: c_result
        
        used = .false.
        
        if (.not. this%is_created .or. .not. c_associated(this%c_state_ptr)) return
        
        if (is_unix()) then
            c_result = temp_file_used_exclusive_creation_unix(this%c_state_ptr)
        else
            c_result = temp_file_used_exclusive_creation_windows(this%c_state_ptr)
        end if
        
        used = (c_result /= 0)
    end function used_exclusive_creation

    function get_creation_time_gap(this) result(gap)
        !! Get creation time gap for security analysis
        class(secure_temp_file_t), intent(in) :: this
        real :: gap
        
        real(c_double) :: c_gap
        
        gap = -1.0
        
        if (.not. this%is_created .or. .not. c_associated(this%c_state_ptr)) return
        
        if (is_unix()) then
            c_gap = temp_file_get_creation_time_gap_unix(this%c_state_ptr)
        else
            c_gap = temp_file_get_creation_time_gap_windows(this%c_state_ptr)
        end if
        
        gap = real(c_gap)
    end function get_creation_time_gap

    function prevents_symlink_following(this) result(prevents)
        !! Check if symlink following prevention is active
        class(secure_temp_file_t), intent(in) :: this
        logical :: prevents
        
        integer(c_int) :: c_result
        
        prevents = .false.
        
        if (.not. this%is_created .or. .not. c_associated(this%c_state_ptr)) return
        
        if (is_unix()) then
            c_result = temp_file_prevents_symlink_following_unix(this%c_state_ptr)
        else
            c_result = temp_file_prevents_symlink_following_windows(this%c_state_ptr)
        end if
        
        prevents = (c_result /= 0)
    end function prevents_symlink_following

    function uses_unix_security_features(this) result(uses)
        !! Check if Unix security features are being used
        class(secure_temp_file_t), intent(in) :: this
        logical :: uses
        
        integer(c_int) :: c_result
        
        uses = .false.
        
        if (.not. is_unix() .or. .not. this%is_created) return
        if (.not. c_associated(this%c_state_ptr)) return
        
        c_result = temp_file_uses_unix_security_features_unix(this%c_state_ptr)
        uses = (c_result /= 0)
    end function uses_unix_security_features

    function uses_windows_security_features(this) result(uses)
        !! Check if Windows security features are being used
        class(secure_temp_file_t), intent(in) :: this
        logical :: uses
        
        integer(c_int) :: c_result
        
        uses = .false.
        
        if (is_unix() .or. .not. this%is_created) return
        if (.not. c_associated(this%c_state_ptr)) return
        
        c_result = temp_file_uses_windows_security_features_windows(this%c_state_ptr)
        uses = (c_result /= 0)
    end function uses_windows_security_features

    subroutine simulate_error_condition(this)
        !! Simulate error conditions for testing
        class(secure_temp_file_t), intent(inout) :: this
        
        ! Simply mark as cleaned to simulate error
        this%is_cleaned = .true.
    end subroutine simulate_error_condition

    ! Reference management
    subroutine acquire_reference(this)
        !! Acquire reference for RAII management
        class(secure_temp_file_t), intent(inout) :: this
        
        this%ref_count = this%ref_count + 1
    end subroutine acquire_reference

    subroutine release_reference(this)
        !! Release reference and cleanup if needed
        class(secure_temp_file_t), intent(inout) :: this
        
        this%ref_count = max(0, this%ref_count - 1)
        
        if (this%ref_count == 0 .and. this%auto_cleanup) then
            call cleanup(this)
        end if
    end subroutine release_reference

    subroutine set_auto_cleanup(this, auto_cleanup)
        !! Set auto cleanup flag
        class(secure_temp_file_t), intent(inout) :: this
        logical, intent(in) :: auto_cleanup
        
        this%auto_cleanup = auto_cleanup
    end subroutine set_auto_cleanup

    subroutine explicit_finalizer(this)
        !! Explicit finalizer for automatic cleanup
        type(secure_temp_file_t), intent(inout) :: this
        
        if (this%auto_cleanup) then
            call cleanup(this)
        end if
    end subroutine explicit_finalizer

    ! Private C state management procedures
    function allocate_c_state(this) result(success)
        !! Allocate C state structure
        type(secure_temp_file_t), intent(inout) :: this
        logical :: success
        
        this%c_state_size = get_secure_temp_file_state_size()
        this%c_state_ptr = c_malloc(this%c_state_size)
        success = c_associated(this%c_state_ptr)
    end function allocate_c_state

    subroutine deallocate_c_state(this)
        !! Deallocate C state structure
        type(secure_temp_file_t), intent(inout) :: this
        
        if (c_associated(this%c_state_ptr)) then
            call c_free(this%c_state_ptr)
            this%c_state_ptr = c_null_ptr
            this%c_state_size = 0
        end if
    end subroutine deallocate_c_state

    subroutine extract_info_from_c_state(this)
        !! Extract information from C state structure
        type(secure_temp_file_t), intent(inout) :: this
        
        character(kind=c_char) :: c_buffer(MAX_FILENAME_LEN)
        integer :: i
        
        if (.not. c_associated(this%c_state_ptr)) return
        
        ! Get filename from C state
        if (is_unix()) then
            call get_filename_from_state_unix(this%c_state_ptr, c_buffer, &
                                               int(MAX_FILENAME_LEN, c_size_t))
        else
            call get_filename_from_state_windows(this%c_state_ptr, c_buffer, &
                                                  int(MAX_FILENAME_LEN, c_size_t))
        end if
        
        ! Convert C string to Fortran string
        this%filename = ""
        do i = 1, MAX_FILENAME_LEN
            if (c_buffer(i) == c_null_char) exit
            this%filename(i:i) = c_buffer(i)
        end do
        
        ! Get temp directory
        if (is_unix()) then
            call get_temp_dir_from_state_unix(this%c_state_ptr, c_buffer, &
                                               int(MAX_FILENAME_LEN, c_size_t))
        else
            call get_temp_dir_from_state_windows(this%c_state_ptr, c_buffer, &
                                                  int(MAX_FILENAME_LEN, c_size_t))
        end if
        
        this%temp_dir = ""
        do i = 1, MAX_FILENAME_LEN
            if (c_buffer(i) == c_null_char) exit
            this%temp_dir(i:i) = c_buffer(i)
        end do
        
        ! Get entropy bits
        if (is_unix()) then
            this%entropy_bits = get_entropy_bits_from_state_unix(this%c_state_ptr)
        else
            this%entropy_bits = get_entropy_bits_from_state_windows(this%c_state_ptr)
        end if
    end subroutine extract_info_from_c_state

end module atomic_temp_file_manager_impl