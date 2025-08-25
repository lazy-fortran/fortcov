module secure_temp_file_types
    !! Secure Temporary File Type Definitions
    !!
    !! Contains the main secure temporary file type and constants.
    !! Extracted from atomic_temp_file_manager_impl.f90 for SRP compliance.
    use iso_c_binding
    implicit none
    private

    ! Public constants
    public :: MAX_FILENAME_LEN, MAX_CONTENT_LEN

    ! Public types
    public :: secure_temp_file_t

    ! Constants
    integer, parameter :: MAX_FILENAME_LEN = 4096
    integer, parameter :: MAX_CONTENT_LEN = 1048576  ! 1MB max content

    ! Secure temporary file type with automatic cleanup
    type :: secure_temp_file_t
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

    ! Interface definitions for procedures
    interface
        subroutine create_secure(this, error_ctx, success)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
            type(*), intent(inout) :: error_ctx
            logical, intent(out) :: success
        end subroutine create_secure

        subroutine create_secure_with_error_context(this, error_ctx, success)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
            type(*), intent(inout) :: error_ctx
            logical, intent(out) :: success
        end subroutine create_secure_with_error_context

        subroutine write_atomic(this, content, error_ctx, success)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
            character(len=*), intent(in) :: content
            type(*), intent(inout) :: error_ctx
            logical, intent(out) :: success
        end subroutine write_atomic

        subroutine read_atomic(this, content, error_ctx, success)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
            character(len=:), allocatable, intent(out) :: content
            type(*), intent(inout) :: error_ctx
            logical, intent(out) :: success
        end subroutine read_atomic

        subroutine move_atomic(this, target_path, error_ctx, success)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
            character(len=*), intent(in) :: target_path
            type(*), intent(inout) :: error_ctx
            logical, intent(out) :: success
        end subroutine move_atomic

        subroutine cleanup(this)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
        end subroutine cleanup

        subroutine get_filename(this, filename)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            character(len=:), allocatable, intent(out) :: filename
        end subroutine get_filename

        function get_current_path(this) result(path)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            character(len=:), allocatable :: path
        end function get_current_path

        subroutine get_entropy_bits(this, entropy_bits)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            integer, intent(out) :: entropy_bits
        end subroutine get_entropy_bits

        function is_atomic_creation(this) result(is_atomic)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            logical :: is_atomic
        end function is_atomic_creation

        function used_exclusive_creation(this) result(used)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            logical :: used
        end function used_exclusive_creation

        function get_creation_time_gap(this) result(gap)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            integer :: gap
        end function get_creation_time_gap

        function prevents_symlink_following(this) result(prevents)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            logical :: prevents
        end function prevents_symlink_following

        function uses_unix_security_features(this) result(uses)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            logical :: uses
        end function uses_unix_security_features

        function uses_windows_security_features(this) result(uses)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(in) :: this
            logical :: uses
        end function uses_windows_security_features

        subroutine simulate_error_condition(this)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
        end subroutine simulate_error_condition

        subroutine acquire_reference(this)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
        end subroutine acquire_reference

        subroutine release_reference(this)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
        end subroutine release_reference

        subroutine set_auto_cleanup(this, auto_cleanup)
            import :: secure_temp_file_t
            class(secure_temp_file_t), intent(inout) :: this
            logical, intent(in) :: auto_cleanup
        end subroutine set_auto_cleanup

        subroutine explicit_finalizer(this)
            import :: secure_temp_file_t
            type(secure_temp_file_t), intent(inout) :: this
        end subroutine explicit_finalizer
    end interface

end module secure_temp_file_types