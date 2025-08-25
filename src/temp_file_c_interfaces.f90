module temp_file_c_interfaces
    !! C Interop Interfaces for Temporary File Operations
    !!
    !! Contains all C binding interfaces for Unix and Windows platforms.
    !! Extracted from atomic_temp_file_manager_impl.f90 for SRP compliance.
    use iso_c_binding
    implicit none
    private

    ! Public interfaces
    public :: create_secure_temp_file_unix, write_atomic_temp_file_unix
    public :: read_temp_file_unix, move_atomic_temp_file_unix
    public :: cleanup_temp_file_unix
    public :: temp_file_used_exclusive_creation_unix
    public :: temp_file_get_creation_time_gap_unix
    public :: temp_file_prevents_symlink_following_unix
    public :: temp_file_uses_unix_security_features_unix
    public :: create_baseline_temp_file_unix
    public :: get_filename_from_state_unix, get_temp_dir_from_state_unix
    public :: get_entropy_bits_from_state_unix

    public :: create_secure_temp_file_windows, write_atomic_temp_file_windows
    public :: read_temp_file_windows, move_atomic_temp_file_windows
    public :: cleanup_temp_file_windows
    public :: temp_file_used_exclusive_creation_windows
    public :: temp_file_get_creation_time_gap_windows
    public :: temp_file_prevents_symlink_following_windows
    public :: temp_file_uses_windows_security_features_windows
    public :: create_baseline_temp_file_windows
    public :: get_filename_from_state_windows, get_temp_dir_from_state_windows
    public :: get_entropy_bits_from_state_windows

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
            bytes_read, error_code) bind(c, name='read_temp_file_windows') &
            result(status)
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

        ! Security validation functions for Windows
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

        ! Utility functions for Windows
        subroutine create_baseline_temp_file_windows() &
            bind(c, name='create_baseline_temp_file_windows')
        end subroutine create_baseline_temp_file_windows

        ! State extraction functions for Windows
        subroutine get_filename_from_state_windows(state, buffer, &
            buffer_size) bind(c, name='get_filename_from_state_windows')
            import :: c_ptr, c_char, c_size_t
            type(c_ptr), value :: state
            character(c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
        end subroutine get_filename_from_state_windows

        subroutine get_temp_dir_from_state_windows(state, buffer, &
            buffer_size) bind(c, name='get_temp_dir_from_state_windows')
            import :: c_ptr, c_char, c_size_t
            type(c_ptr), value :: state
            character(c_char), intent(out) :: buffer(*)
            integer(c_size_t), value :: buffer_size
        end subroutine get_temp_dir_from_state_windows

        function get_entropy_bits_from_state_windows(state) &
            bind(c, name='get_entropy_bits_from_state_windows') &
            result(entropy)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
            integer(c_int) :: entropy
        end function get_entropy_bits_from_state_windows
    end interface

end module temp_file_c_interfaces