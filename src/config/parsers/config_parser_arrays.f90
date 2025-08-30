module config_parser_arrays
    !! Array management utilities for configuration data structures
    !!
    !! Provides dynamic array operations for strings and configuration arrays,
    !! including safe memory management with move_alloc.
    
    use config_types, only: config_t
    implicit none
    private
    
    public :: add_string_to_array
    public :: add_source_path
    public :: add_exclude_pattern
    public :: add_include_pattern
    public :: transfer_string_array
    
contains

    subroutine add_string_to_array(array, new_string)
        !! Add string to dynamically sized array
        character(len=:), allocatable, intent(inout) :: array(:)
        character(len=*), intent(in) :: new_string
        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size, i
        integer :: str_len

        str_len = max(len(new_string), 256)  ! Minimum length for safety
        
        if (.not. allocated(array)) then
            allocate(character(len=str_len) :: array(1))
            array(1) = new_string
        else
            current_size = size(array)
            new_size = current_size + 1
            
            ! Create temporary array with new size
            allocate(character(len=str_len) :: temp_array(new_size))
            
            ! Copy existing strings
            do i = 1, current_size
                temp_array(i) = array(i)
            end do
            
            ! Add new string
            temp_array(new_size) = new_string
            
            ! Replace array with temp array
            call move_alloc(temp_array, array)
        end if
    end subroutine add_string_to_array

    subroutine add_source_path(config, path)
        !! Add source path to configuration
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: path
        call add_string_to_array(config%source_paths, path)
    end subroutine add_source_path

    subroutine add_exclude_pattern(config, pattern)
        !! Add exclude pattern to configuration
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: pattern
        call add_string_to_array(config%exclude_patterns, pattern)
    end subroutine add_exclude_pattern

    subroutine add_include_pattern(config, pattern)
        !! Add include pattern to configuration
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: pattern
        call add_string_to_array(config%include_patterns, pattern)
    end subroutine add_include_pattern

    subroutine transfer_string_array(from_array, to_array)
        !! Transfer string array ownership using move_alloc
        character(len=:), allocatable, intent(inout) :: from_array(:)
        character(len=:), allocatable, intent(out) :: to_array(:)
        call move_alloc(from_array, to_array)
    end subroutine transfer_string_array

end module config_parser_arrays