module string_utilities
    !! String Utility Functions
    !!
    !! Shared helper functions for string conversions and manipulations.
    !! Eliminates code duplication across modules per QADS DRY principle.
    implicit none
    private
    
    ! Public procedures
    public :: int_to_str
    public :: real_to_str
    public :: int_to_string
    public :: real_to_string
    public :: int64_to_string
    
contains

    ! Convert integer to string (primary interface)
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(I0)') num
        str = trim(temp_str)
    end function int_to_str
    
    ! Convert real to string (primary interface)
    function real_to_str(num) result(str)
        real, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(F0.1)') num
        str = trim(temp_str)
    end function real_to_str
    
    ! Convert integer to string (alternative interface)
    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') value
        str = trim(temp_str)
    end function int_to_string
    
    ! Convert real to string (alternative interface with higher precision)
    function real_to_string(value) result(str)
        real, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(F0.6)') value
        str = trim(temp_str)
    end function real_to_string
    
    ! Convert int64 to string
    function int64_to_string(int_val) result(str_val)
        use iso_fortran_env, only: int64
        integer(int64), intent(in) :: int_val
        character(len=:), allocatable :: str_val
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str_val = trim(temp_str)
    end function int64_to_string

end module string_utilities