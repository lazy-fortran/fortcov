module source_location_types
    !! Source Location Type Definitions
    !!
    !! Contains types for representing source code locations.
    !! Extracted from coverage_types.f90 for SRP compliance.
    implicit none
    private

    ! Public types
    public :: source_location_t

    ! Source location type
    type :: source_location_t
        character(len=:), allocatable :: filename
        integer :: line_number
        integer :: column_start = 0
        integer :: column_end = 0
    contains
        procedure :: init => source_location_init
    end type source_location_t

contains

    ! Initialize source location
    subroutine source_location_init(this, filename, line_number, &
                                   column_start, column_end)
        class(source_location_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_number
        integer, intent(in), optional :: column_start, column_end

        this%filename = trim(filename)
        this%line_number = line_number
        
        if (present(column_start)) then
            this%column_start = column_start
        else
            this%column_start = 0
        end if
        
        if (present(column_end)) then
            this%column_end = column_end
        else
            this%column_end = 0
        end if
    end subroutine source_location_init

end module source_location_types