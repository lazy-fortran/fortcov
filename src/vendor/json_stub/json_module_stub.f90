module json_module
    !! Minimal stub for json-fortran API to allow building without dependency.
    implicit none
    private

    type :: json_value
        integer :: dummy = 0
    end type json_value

    type :: json_core
        logical :: had_error = .false.
    contains
        procedure :: initialize
        procedure :: deserialize
        procedure :: failed
        procedure :: destroy
        procedure :: get_value_ptr
        procedure :: get_char
        procedure :: get_int
        generic :: get => get_value_ptr, get_char, get_int
        procedure :: info
        procedure :: get_child
        procedure :: create_object
        procedure :: create_array
        procedure :: add_char
        procedure :: add_int
        procedure :: add_real
        procedure :: add_obj
        generic :: add => add_char, add_int, add_real, add_obj
        ! procedure :: add_to_parent
        procedure :: print_to_string
        procedure :: check_for_errors
        procedure :: print_error_message
    end type json_core

    type :: json_file
    contains
        procedure :: get_path_char
        procedure :: get_path_int
        generic :: get => get_path_char, get_path_int
        procedure :: info_path
        generic :: info => info_path
    end type json_file

    ! No module-level generics; bound on json_core

    type(json_value), target, save :: json_singleton

    public :: json_core, json_value, json_file

contains

    subroutine initialize(this)
        class(json_core), intent(inout) :: this
        this%had_error = .false.
    end subroutine initialize

    subroutine deserialize(this, root, text)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(out) :: root
        character(len=*), intent(in) :: text
        ! Consider empty input as an error; otherwise OK
        if (len_trim(text) == 0) then
            this%had_error = .true.
            nullify(root)
        else
            root => json_singleton
        end if
    end subroutine deserialize

    logical function failed(this)
        class(json_core), intent(in) :: this
        failed = this%had_error
    end function failed

    subroutine destroy(this, node)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(inout) :: node
        nullify(node)
    end subroutine destroy

    subroutine get_value_ptr(this, node, key, out, found)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: node
        character(len=*), intent(in) :: key
        type(json_value), pointer, intent(out) :: out
        logical, intent(out) :: found
        out => json_singleton
        found = .false.
    end subroutine get_value_ptr

    subroutine get_char(this, node, key, out, found)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: node
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: found
        found = .false.
        if (allocated(out)) deallocate(out)
    end subroutine get_char

    subroutine get_int(this, node, key, out, found)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: node
        character(len=*), intent(in) :: key
        integer, intent(out) :: out
        logical, intent(out) :: found
        out = 0
        found = .false.
    end subroutine get_int

    subroutine info(this, node, n_children, found)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: node
        integer, intent(out), optional :: n_children
        logical, intent(out), optional :: found
        if (present(n_children)) n_children = 0
        if (present(found)) found = .false.
    end subroutine info

    subroutine get_child(this, node, index, child)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: node
        integer, intent(in) :: index
        type(json_value), pointer, intent(out) :: child
        nullify(child)
    end subroutine get_child

    subroutine create_object(this, obj, name)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(out) :: obj
        character(len=*), intent(in) :: name
        obj => json_singleton
    end subroutine create_object

    subroutine create_array(this, arr, name)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(out) :: arr
        character(len=*), intent(in) :: name
        arr => json_singleton
    end subroutine create_array

    subroutine add_char(this, parent, name, value)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: parent
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: value
        ! no-op
    end subroutine add_char

    subroutine add_int(this, parent, name, value)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: parent
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        ! no-op
    end subroutine add_int

    subroutine add_real(this, parent, name, value)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: parent
        character(len=*), intent(in) :: name
        real, intent(in) :: value
        ! no-op
    end subroutine add_real

    subroutine add_obj(this, parent, child)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer, intent(in) :: child
        ! no-op
    end subroutine add_obj

    ! Removed: add_to_parent (not needed in stub)

    subroutine print_to_string(this, root, text)
        class(json_core), intent(inout) :: this
        type(json_value), pointer, intent(in) :: root
        character(len=:), allocatable, intent(out) :: text
        if (allocated(text)) deallocate(text)
        text = '{}'
    end subroutine print_to_string

    subroutine check_for_errors(this, status_ok)
        class(json_core), intent(in) :: this
        logical, intent(out) :: status_ok
        status_ok = .true.
    end subroutine check_for_errors

    subroutine print_error_message(this)
        class(json_core), intent(in) :: this
        ! no-op placeholder
    end subroutine print_error_message

    ! json_file path-based helper stubs
    subroutine get_path_char(this, path, out, found)
        class(json_file), intent(inout) :: this
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: found
        found = .false.
        if (allocated(out)) deallocate(out)
    end subroutine get_path_char

    subroutine get_path_int(this, path, out, found)
        class(json_file), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer, intent(out) :: out
        logical, intent(out) :: found
        out = 0
        found = .false.
    end subroutine get_path_int

    subroutine info_path(this, path, n_children, found)
        class(json_file), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer, intent(out) :: n_children
        logical, intent(out) :: found
        n_children = 0
        found = .false.
    end subroutine info_path

end module json_module
