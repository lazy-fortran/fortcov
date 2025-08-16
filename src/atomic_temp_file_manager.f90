! Atomic temporary file operations security system
! Prevents race conditions, symlink attacks, and privilege escalation
! Cross-platform implementation with C interop for security-critical operations
module atomic_temp_file_manager
    use iso_c_binding
    use error_handling
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
        final :: cleanup_finalizer
    end type secure_temp_file_t
    
    ! C interop interfaces for Unix/Linux
    interface
        function create_secure_temp_file_unix(state, error_code) &
                bind(c, name='create_secure_temp_file_unix') result(status)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int), intent(out) :: error_code
            integer(c_int) :: status
        end function create_secure_temp_file_unix
        
        function write_atomic_temp_file_unix(state, data, data_len, error_code) &
            bind(c, name='write_atomic_temp_file_unix') result(status)
            import :: c_ptr, c_char, c_size_t, c_int
            type(c_ptr), value :: state
            character(c_char), intent(in) :: data(*)
            integer(c_size_t), value :: data_len
            integer(c_int), intent(out) :: error_code
            integer(c_int) :: status
        end function write_atomic_temp_file_unix
        
        function read_temp_file_unix(state, buffer, buffer_size, bytes_read, &
            error_code) bind(c, name='read_temp_file_unix') result(status)
            import :: c_ptr, c_char, c_size_t, c_int
            type(c_ptr), value :: state
            character(c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
            integer(c_size_t), intent(out) :: bytes_read
            integer(c_int), intent(out) :: error_code
            integer(c_int) :: status
        end function read_temp_file_unix
        
        function move_atomic_temp_file_unix(state, target_path, error_code) &
            bind(c, name='move_atomic_temp_file_unix') result(status)
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: state
            character(c_char), intent(in) :: target_path(*)
            integer(c_int), intent(out) :: error_code
            integer(c_int) :: status
        end function move_atomic_temp_file_unix
        
        function cleanup_temp_file_unix(state) &
            bind(c, name='cleanup_temp_file_unix') result(status)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: status
        end function cleanup_temp_file_unix
        
        ! Security validation functions
        function temp_file_used_exclusive_creation_unix(state) &
                bind(c, name='temp_file_used_exclusive_creation_unix') &
                result(used)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: used
        end function temp_file_used_exclusive_creation_unix
        
        function temp_file_get_creation_time_gap_unix(state) &
            bind(c, name='temp_file_get_creation_time_gap_unix') result(gap)
            import :: c_ptr, c_long
            type(c_ptr), value :: state
            integer(c_long) :: gap
        end function temp_file_get_creation_time_gap_unix
        
        function temp_file_prevents_symlink_following_unix(state) &
            bind(c, name='temp_file_prevents_symlink_following_unix') &
            result(prevents)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: prevents
        end function temp_file_prevents_symlink_following_unix
        
        function temp_file_uses_unix_security_features_unix(state) &
            bind(c, name='temp_file_uses_unix_security_features_unix') &
            result(uses)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: uses
        end function temp_file_uses_unix_security_features_unix
        
        ! Utility functions
        subroutine create_baseline_temp_file_unix() &
            bind(c, name='create_baseline_temp_file_unix')
        end subroutine create_baseline_temp_file_unix
        
        ! State extraction functions
        subroutine get_filename_from_state_unix(state, buffer, buffer_size) &
            bind(c, name='get_filename_from_state_unix')
            import :: c_ptr, c_char, c_size_t
            type(c_ptr), value :: state
            character(c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
        end subroutine get_filename_from_state_unix
        
        subroutine get_temp_dir_from_state_unix(state, buffer, buffer_size) &
            bind(c, name='get_temp_dir_from_state_unix')
            import :: c_ptr, c_char, c_size_t
            type(c_ptr), value :: state
            character(c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
        end subroutine get_temp_dir_from_state_unix
        
        function get_entropy_bits_from_state_unix(state) &
            bind(c, name='get_entropy_bits_from_state_unix') result(entropy)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: entropy
        end function get_entropy_bits_from_state_unix
    end interface
    
    ! C interop interfaces for Windows
    interface
        function create_secure_temp_file_windows(state, error_code) &
            bind(c, name='create_secure_temp_file_windows') result(status)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int), intent(out) :: error_code
            integer(c_int) :: status
        end function create_secure_temp_file_windows
        
        function write_atomic_temp_file_windows(state, data, data_len, &
                error_code) bind(c, name='write_atomic_temp_file_windows') &
                result(status)
            import :: c_ptr, c_char, c_size_t, c_int
            type(c_ptr), value :: state
            character(c_char), intent(in) :: data(*)
            integer(c_size_t), value :: data_len
            integer(c_int), intent(out) :: error_code
            integer(c_int) :: status
        end function write_atomic_temp_file_windows
        
        function read_temp_file_windows(state, buffer, buffer_size, &
            bytes_read, error_code) &
            bind(c, name='read_temp_file_windows') result(status)
            import :: c_ptr, c_char, c_size_t, c_int
            type(c_ptr), value :: state
            character(c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
            integer(c_size_t), intent(out) :: bytes_read
            integer(c_int), intent(out) :: error_code
            integer(c_int) :: status
        end function read_temp_file_windows
        
        function move_atomic_temp_file_windows(state, target_path, &
                error_code) bind(c, name='move_atomic_temp_file_windows') &
                result(status)
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: state
            character(c_char), intent(in) :: target_path(*)
            integer(c_int), intent(out) :: error_code
            integer(c_int) :: status
        end function move_atomic_temp_file_windows
        
        function cleanup_temp_file_windows(state) &
            bind(c, name='cleanup_temp_file_windows') result(status)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: status
        end function cleanup_temp_file_windows
        
        ! Security validation functions
        function temp_file_used_exclusive_creation_windows(state) &
            bind(c, name='temp_file_used_exclusive_creation_windows') &
            result(used)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: used
        end function temp_file_used_exclusive_creation_windows
        
        function temp_file_get_creation_time_gap_windows(state) &
            bind(c, name='temp_file_get_creation_time_gap_windows') &
            result(gap)
            import :: c_ptr, c_long
            type(c_ptr), value :: state
            integer(c_long) :: gap
        end function temp_file_get_creation_time_gap_windows
        
        function temp_file_prevents_symlink_following_windows(state) &
                bind(c, name='temp_file_prevents_symlink_following_windows') &
                result(prevents)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: prevents
        end function temp_file_prevents_symlink_following_windows
        
        function temp_file_uses_windows_security_features_windows(state) &
                bind(c, name='temp_file_uses_windows_security_features_windows') &
                result(uses)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: uses
        end function temp_file_uses_windows_security_features_windows
        
        ! Utility functions
        subroutine create_baseline_temp_file_windows() &
            bind(c, name='create_baseline_temp_file_windows')
        end subroutine create_baseline_temp_file_windows
        
        ! State extraction functions
        subroutine get_filename_from_state_windows(state, buffer, buffer_size) &
            bind(c, name='get_filename_from_state_windows')
            import :: c_ptr, c_char, c_size_t
            type(c_ptr), value :: state
            character(c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
        end subroutine get_filename_from_state_windows
        
        subroutine get_temp_dir_from_state_windows(state, buffer, buffer_size) &
            bind(c, name='get_temp_dir_from_state_windows')
            import :: c_ptr, c_char, c_size_t
            type(c_ptr), value :: state
            character(c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
        end subroutine get_temp_dir_from_state_windows
        
        function get_entropy_bits_from_state_windows(state) &
            bind(c, name='get_entropy_bits_from_state_windows') result(entropy)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: entropy
        end function get_entropy_bits_from_state_windows
    end interface
    
    ! Public procedures
    public :: is_temp_file_error_recoverable, get_platform_is_unix
    
    ! C memory management interfaces
    interface
        function c_malloc(size) bind(c, name='malloc') result(ptr)
            import :: c_ptr, c_size_t
            integer(c_size_t), value :: size
            type(c_ptr) :: ptr
        end function c_malloc
        
        subroutine c_free(ptr) bind(c, name='free')
            import :: c_ptr
            type(c_ptr), value :: ptr
        end subroutine c_free
        
        ! Platform detection interface
        function is_unix_platform() bind(c, name='is_unix_platform') result(is_unix)
            import :: c_int
            integer(c_int) :: is_unix
        end function is_unix_platform
        
        ! Structure size interface
        function get_secure_temp_file_state_size() &
            bind(c, name='get_secure_temp_file_state_size') result(size)
            import :: c_size_t
            integer(c_size_t) :: size
        end function get_secure_temp_file_state_size
    end interface
    
contains

    ! Platform detection functions
    function get_platform_is_unix() result(is_unix)
        logical :: is_unix
        is_unix = (is_unix_platform() /= 0)
    end function get_platform_is_unix
    
    ! Helper function for internal use
    function is_unix() result(unix_platform)
        logical :: unix_platform
        unix_platform = (is_unix_platform() /= 0)
    end function is_unix

    ! Create secure temporary file with atomic operations
    subroutine create_secure(this, error_ctx, success)
        class(secure_temp_file_t), intent(inout) :: this
        type(error_context_t), intent(out) :: error_ctx
        logical, intent(out) :: success
        
        integer(c_int) :: c_error_code, status
        
        call clear_error_context(error_ctx)
        success = .false.
        
        ! Cleanup any previous state
        if (this%is_created) then
            call this%cleanup()
        end if
        
        ! Allocate C state structure
        if (.not. allocate_c_state(this)) then
            call handle_out_of_memory(int(this%c_state_size), error_ctx)
            return
        end if
        
        ! Create secure temp file using platform-specific implementation
        if (is_unix()) then
            status = create_secure_temp_file_unix(this%c_state_ptr, c_error_code)
        else
            status = create_secure_temp_file_windows(this%c_state_ptr, &
                c_error_code)
        end if
        
        if (status /= 0 .or. c_error_code /= ERROR_SUCCESS) then
            call map_c_error_to_context(c_error_code, error_ctx)
            call deallocate_c_state(this)
            return
        end if
        
        ! Extract information from C state
        call extract_info_from_c_state(this)
        
        this%is_created = .true.
        this%is_cleaned = .false.
        
        ! Set success status in error context
        error_ctx%error_code = ERROR_SUCCESS
        error_ctx%recoverable = .true.
        success = .true.
    end subroutine create_secure

    ! Create secure temporary file with comprehensive error context
    subroutine create_secure_with_error_context(this, error_ctx, success)
        class(secure_temp_file_t), intent(inout) :: this
        type(error_context_t), intent(out) :: error_ctx
        logical, intent(out) :: success
        
        ! Use same implementation as create_secure
        call this%create_secure(error_ctx, success)
        
        ! Add additional context for comprehensive error reporting
        if (.not. success) then
            call safe_write_context(error_ctx, "Atomic temp file creation")
            call safe_write_suggestion(error_ctx, &
                "Check temp directory permissions and available disk space")
        end if
    end subroutine create_secure_with_error_context

    ! Write data atomically to temporary file
    subroutine write_atomic(this, content, error_ctx, success)
        class(secure_temp_file_t), intent(inout) :: this
        character(len=*), intent(in) :: content
        type(error_context_t), intent(out) :: error_ctx
        logical, intent(out) :: success
        
        integer(c_int) :: c_error_code, status
        character(c_char), allocatable :: c_content(:)
        integer :: i, content_len
        
        call clear_error_context(error_ctx)
        success = .false.
        
        if (.not. this%is_created) then
            call safe_write_message(error_ctx, &
                "Cannot write to temp file: file not created")
            error_ctx%error_code = ERROR_FATAL
            return
        end if
        
        content_len = len_trim(content)
        if (content_len > MAX_CONTENT_LEN) then
            call handle_out_of_memory(content_len, error_ctx)
            return
        end if
        
        ! Convert Fortran string to C string
        allocate(c_content(content_len + 1))
        do i = 1, content_len
            c_content(i) = content(i:i)
        end do
        c_content(content_len + 1) = c_null_char
        
        ! Write atomically using platform-specific implementation
        if (is_unix()) then
            status = write_atomic_temp_file_unix(this%c_state_ptr, c_content, &
                int(content_len, c_size_t), c_error_code)
        else
            status = write_atomic_temp_file_windows(this%c_state_ptr, &
                c_content, int(content_len, c_size_t), c_error_code)
        end if
        
        deallocate(c_content)
        
        if (status /= 0 .or. c_error_code /= ERROR_SUCCESS) then
            call map_c_error_to_context(c_error_code, error_ctx)
            return
        end if
        
        success = .true.
    end subroutine write_atomic

    ! Read data from temporary file
    subroutine read_atomic(this, content, error_ctx, success)
        class(secure_temp_file_t), intent(inout) :: this
        character(len=*), intent(out) :: content
        type(error_context_t), intent(out) :: error_ctx
        logical, intent(out) :: success
        
        integer(c_int) :: c_error_code, status
        character(c_char), allocatable :: c_buffer(:)
        integer(c_size_t) :: bytes_read
        integer :: i, buffer_size
        
        call clear_error_context(error_ctx)
        success = .false.
        content = ""
        
        if (.not. this%is_created) then
            call safe_write_message(error_ctx, &
                "Cannot read from temp file: file not created")
            error_ctx%error_code = ERROR_FATAL
            return
        end if
        
        buffer_size = min(len(content), MAX_CONTENT_LEN)
        allocate(c_buffer(buffer_size + 1))
        
        ! Read using platform-specific implementation
        if (is_unix()) then
            status = read_temp_file_unix(this%c_state_ptr, c_buffer, &
                int(buffer_size + 1, c_size_t), bytes_read, c_error_code)
        else
            status = read_temp_file_windows(this%c_state_ptr, c_buffer, &
                int(buffer_size + 1, c_size_t), bytes_read, c_error_code)
        end if
        
        if (status == 0 .and. c_error_code == ERROR_SUCCESS) then
            ! Convert C string back to Fortran string
            do i = 1, min(int(bytes_read), len(content))
                content(i:i) = c_buffer(i)
            end do
            success = .true.
        else
            call map_c_error_to_context(c_error_code, error_ctx)
        end if
        
        deallocate(c_buffer)
    end subroutine read_atomic

    ! Move temporary file atomically to target location
    subroutine move_atomic(this, target_path, error_ctx, success)
        class(secure_temp_file_t), intent(inout) :: this
        character(len=*), intent(in) :: target_path
        type(error_context_t), intent(out) :: error_ctx
        logical, intent(out) :: success
        
        integer(c_int) :: c_error_code, status
        character(c_char), allocatable :: c_target_path(:)
        integer :: i, path_len
        
        call clear_error_context(error_ctx)
        success = .false.
        
        if (.not. this%is_created) then
            call safe_write_message(error_ctx, &
                "Cannot move temp file: file not created")
            error_ctx%error_code = ERROR_FATAL
            return
        end if
        
        path_len = len_trim(target_path)
        allocate(c_target_path(path_len + 1))
        
        ! Convert Fortran string to C string
        do i = 1, path_len
            c_target_path(i) = target_path(i:i)
        end do
        c_target_path(path_len + 1) = c_null_char
        
        ! Move atomically using platform-specific implementation
        if (is_unix()) then
            status = move_atomic_temp_file_unix(this%c_state_ptr, &
                c_target_path, c_error_code)
        else
            status = move_atomic_temp_file_windows(this%c_state_ptr, &
                c_target_path, c_error_code)
        end if
        
        deallocate(c_target_path)
        
        if (status /= 0 .or. c_error_code /= ERROR_SUCCESS) then
            call map_c_error_to_context(c_error_code, error_ctx)
            return
        end if
        
        ! Re-extract state information from C after successful move
        call extract_info_from_c_state(this)
        success = .true.
    end subroutine move_atomic

    ! Manual cleanup of temporary file
    subroutine cleanup(this)
        class(secure_temp_file_t), intent(inout) :: this
        
        if (.not. this%is_cleaned .and. this%is_created) then
            if (is_unix()) then
                associate(dummy => cleanup_temp_file_unix(this%c_state_ptr))
                end associate
            else
                associate(dummy => cleanup_temp_file_windows(this%c_state_ptr))
                end associate
            end if
            
            call deallocate_c_state(this)
            this%is_cleaned = .true.
            this%is_created = .false.
        end if
    end subroutine cleanup

    ! Get filename of temporary file
    subroutine get_filename(this, filename)
        class(secure_temp_file_t), intent(in) :: this
        character(len=*), intent(out) :: filename
        
        filename = trim(this%filename)
    end subroutine get_filename

    ! Get current path (same as filename for compatibility)
    function get_current_path(this) result(path)
        class(secure_temp_file_t), intent(in) :: this
        character(len=MAX_FILENAME_LEN) :: path
        
        path = trim(this%filename)
    end function get_current_path

    ! Get entropy bits used in filename generation
    subroutine get_entropy_bits(this, entropy_bits)
        class(secure_temp_file_t), intent(in) :: this
        integer, intent(out) :: entropy_bits
        
        entropy_bits = this%entropy_bits
    end subroutine get_entropy_bits

    ! Check if creation was atomic
    function is_atomic_creation(this) result(is_atomic)
        class(secure_temp_file_t), intent(in) :: this
        logical :: is_atomic
        
        is_atomic = this%used_exclusive_creation() .and. &
                   this%get_creation_time_gap() == 0
    end function is_atomic_creation

    ! Check if exclusive creation flag was used
    function used_exclusive_creation(this) result(used)
        class(secure_temp_file_t), intent(in) :: this
        logical :: used
        
        integer(c_int) :: c_result
        
        if (.not. this%is_created) then
            used = .false.
            return
        end if
        
        if (is_unix()) then
            c_result = temp_file_used_exclusive_creation_unix(this%c_state_ptr)
        else
            c_result = temp_file_used_exclusive_creation_windows( &
                    this%c_state_ptr)
        end if
        
        used = (c_result /= 0)
    end function used_exclusive_creation

    ! Get creation time gap (should be 0 for atomic operations)
    function get_creation_time_gap(this) result(gap)
        class(secure_temp_file_t), intent(in) :: this
        integer(c_long) :: gap
        
        if (.not. this%is_created) then
            gap = -1
            return
        end if
        
        if (is_unix()) then
            gap = temp_file_get_creation_time_gap_unix(this%c_state_ptr)
        else
            gap = temp_file_get_creation_time_gap_windows(this%c_state_ptr)
        end if
    end function get_creation_time_gap

    ! Check if symlink following is prevented
    function prevents_symlink_following(this) result(prevents)
        class(secure_temp_file_t), intent(in) :: this
        logical :: prevents
        
        integer(c_int) :: c_result
        
        if (.not. this%is_created) then
            prevents = .false.
            return
        end if
        
        if (is_unix()) then
            c_result = temp_file_prevents_symlink_following_unix( &
                this%c_state_ptr)
        else
            c_result = temp_file_prevents_symlink_following_windows( &
                this%c_state_ptr)
        end if
        
        prevents = (c_result /= 0)
    end function prevents_symlink_following

    ! Check Unix-specific security features
    function uses_unix_security_features(this) result(uses)
        class(secure_temp_file_t), intent(in) :: this
        logical :: uses
        
        integer(c_int) :: c_result
        
        if (.not. this%is_created .or. .not. is_unix()) then
            uses = .false.
            return
        end if
        
        c_result = temp_file_uses_unix_security_features_unix(this%c_state_ptr)
        uses = (c_result /= 0)
    end function uses_unix_security_features

    ! Check Windows-specific security features
    function uses_windows_security_features(this) result(uses)
        class(secure_temp_file_t), intent(in) :: this
        logical :: uses
        
        integer(c_int) :: c_result
        
        if (.not. this%is_created .or. is_unix()) then
            uses = .false.
            return
        end if
        
        c_result = temp_file_uses_windows_security_features_windows( &
                this%c_state_ptr)
        uses = (c_result /= 0)
    end function uses_windows_security_features

    ! Simulate error condition for testing
    subroutine simulate_error_condition(this)
        class(secure_temp_file_t), intent(inout) :: this
        
        ! Force cleanup to simulate error condition
        call this%cleanup()
    end subroutine simulate_error_condition

    ! Automatic cleanup on finalization
    subroutine cleanup_finalizer(this)
        type(secure_temp_file_t), intent(inout) :: this
        
        call this%cleanup()
    end subroutine cleanup_finalizer

    ! Helper functions

    ! Allocate C state structure for secure temp file operations
    function allocate_c_state(this) result(success)
        type(secure_temp_file_t), intent(inout) :: this
        logical :: success
        
        ! Get actual size of C structure and allocate memory
        this%c_state_size = get_secure_temp_file_state_size()
        this%c_state_ptr = c_malloc(this%c_state_size)
        
        success = c_associated(this%c_state_ptr)
    end function allocate_c_state

    ! Deallocate C state structure and free resources
    subroutine deallocate_c_state(this)
        type(secure_temp_file_t), intent(inout) :: this
        
        ! Free C memory if allocated
        if (c_associated(this%c_state_ptr)) then
            call c_free(this%c_state_ptr)
            this%c_state_ptr = c_null_ptr
        end if
        
        this%c_state_size = 0
    end subroutine deallocate_c_state

    ! Extract information from C state structure after successful creation
    subroutine extract_info_from_c_state(this)
        type(secure_temp_file_t), intent(inout) :: this
        character(c_char), allocatable :: c_buffer(:)
        integer :: i
        
        ! Allocate buffers for C string extraction
        allocate(c_buffer(MAX_FILENAME_LEN))
        
        ! Extract filename from C state
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
        
        ! Extract temp directory from C state
        if (is_unix()) then
            call get_temp_dir_from_state_unix(this%c_state_ptr, c_buffer, &
                int(MAX_FILENAME_LEN, c_size_t))
        else
            call get_temp_dir_from_state_windows(this%c_state_ptr, c_buffer, &
                int(MAX_FILENAME_LEN, c_size_t))
        end if
        
        ! Convert C string to Fortran string
        this%temp_dir = ""
        do i = 1, MAX_FILENAME_LEN
            if (c_buffer(i) == c_null_char) exit
            this%temp_dir(i:i) = c_buffer(i)
        end do
        
        ! Extract entropy bits from C state
        if (is_unix()) then
            this%entropy_bits = get_entropy_bits_from_state_unix(this%c_state_ptr)
        else
            this%entropy_bits = get_entropy_bits_from_state_windows( &
                this%c_state_ptr)
        end if
        
        deallocate(c_buffer)
    end subroutine extract_info_from_c_state

    ! Map C error codes to Fortran error context
    subroutine map_c_error_to_context(c_error_code, error_ctx)
        integer(c_int), intent(in) :: c_error_code
        type(error_context_t), intent(out) :: error_ctx
        
        error_ctx%error_code = c_error_code
        
        select case (c_error_code)
        case (ERROR_PERMISSION_DENIED)
            call safe_write_message(error_ctx, &
                "Permission denied creating secure temp file")
            call safe_write_suggestion(error_ctx, &
                "Check temp directory permissions")
            error_ctx%recoverable = .false.
            
        case (ERROR_OUT_OF_MEMORY)
            call safe_write_message(error_ctx, &
                "Out of memory during temp file operation")
            call safe_write_suggestion(error_ctx, &
                "Reduce data size or increase available memory")
            error_ctx%recoverable = .false.
            
        case default
            call safe_write_message(error_ctx, &
                "Unknown error during temp file operation")
            call safe_write_suggestion(error_ctx, &
                "Check system logs for details")
            error_ctx%recoverable = .false.
        end select
        
        call safe_write_context(error_ctx, "Atomic temp file operation")
    end subroutine map_c_error_to_context

    ! Check if temp file error is recoverable
    function is_temp_file_error_recoverable(error_code) result(recoverable)
        integer, intent(in) :: error_code
        logical :: recoverable
        
        select case (error_code)
        case (ERROR_SUCCESS)
            recoverable = .true.
        case (ERROR_OUT_OF_MEMORY, ERROR_PERMISSION_DENIED, ERROR_FATAL)
            recoverable = .false.
        case default
            recoverable = .false.
        end select
    end function is_temp_file_error_recoverable

end module atomic_temp_file_manager