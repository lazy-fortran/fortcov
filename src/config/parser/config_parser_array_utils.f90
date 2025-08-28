module config_parser_array_utils
    !! Array management utilities for configuration parsing
    !!
    !! This module provides specialized array operations for configuration
    !! data, including dynamic array growth and data transfer operations.

    use config_types, only: config_t, MAX_ARRAY_SIZE

    implicit none
    private

    ! Array management utilities
    public :: add_string_to_array
    public :: add_source_path
    public :: add_exclude_pattern
    public :: add_include_pattern
    public :: transfer_string_array

contains

    subroutine add_string_to_array(item, array, max_size, item_type, success, error_message)
        !! Add string to allocatable array with size checking
        character(len=*), intent(in) :: item
        character(len=:), allocatable, intent(inout) :: array(:)
        integer, intent(in) :: max_size
        character(len=*), intent(in) :: item_type
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size, i

        success = .true.
        error_message = ""

        if (.not. allocated(array)) then
            ! Initialize array with first item
            allocate(character(len=len(item)) :: array(1))
            array(1) = item
        else
            current_size = size(array)

            ! Check size limit
            if (current_size >= max_size) then
                success = .false.
                write(error_message, '(A,A,A,I0,A)') &
                    "Too many ", trim(item_type), " (max ", max_size, ")"
                return
            end if

            ! Allocate temporary array with increased size
            new_size = current_size + 1
            allocate(character(len=max(len(array), len(item))) :: temp_array(new_size))

            ! Copy existing items
            do i = 1, current_size
                temp_array(i) = array(i)
            end do

            ! Add new item
            temp_array(new_size) = item

            ! Replace array with temp_array
            call move_alloc(temp_array, array)
        end if

    end subroutine add_string_to_array

    subroutine add_source_path(path, config, success, error_message)
        !! Add source path to configuration
        character(len=*), intent(in) :: path
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(path, config%source_paths, MAX_ARRAY_SIZE, &
                                 "source paths", success, error_message)

    end subroutine add_source_path

    subroutine add_exclude_pattern(pattern, config, success, error_message)
        !! Add exclude pattern to configuration
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(pattern, config%exclude_patterns, MAX_ARRAY_SIZE, &
                                 "exclude patterns", success, error_message)

    end subroutine add_exclude_pattern

    subroutine add_include_pattern(pattern, config, success, error_message)
        !! Add include pattern to configuration
        character(len=*), intent(in) :: pattern
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        call add_string_to_array(pattern, config%include_patterns, MAX_ARRAY_SIZE, &
                                 "include patterns", success, error_message)

    end subroutine add_include_pattern

    subroutine transfer_string_array(input_array, output_array)
        !! Transfer non-empty strings from fixed array to allocatable array
        character(len=*), dimension(:), intent(in) :: input_array
        character(len=:), allocatable, dimension(:), intent(out) :: output_array
        
        integer :: i, count, max_len
        
        ! Count non-empty elements and find max length
        count = 0
        max_len = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
                max_len = max(max_len, len_trim(input_array(i)))
            end if
        end do
        
        if (count == 0) return
        
        ! Allocate output array
        allocate(character(len=max_len) :: output_array(count))
        
        ! Copy non-empty strings
        count = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
                output_array(count) = trim(input_array(i))
            end if
        end do
        
    end subroutine transfer_string_array

end module config_parser_array_utils