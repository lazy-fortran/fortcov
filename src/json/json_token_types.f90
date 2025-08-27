module json_token_types
    !! JSON token type definitions
    implicit none
    private
    
    ! JSON token types
    integer, parameter, public :: JSON_NULL = 0
    integer, parameter, public :: JSON_STRING = 1
    integer, parameter, public :: JSON_NUMBER = 2
    integer, parameter, public :: JSON_OBJECT = 3
    integer, parameter, public :: JSON_ARRAY = 4
    integer, parameter, public :: JSON_BOOLEAN = 5
    
    ! JSON token type
    type, public :: json_token_t
        integer :: type = JSON_NULL
        character(len=:), allocatable :: value
        integer :: start_pos = 0
        integer :: end_pos = 0
    end type json_token_t
    
end module json_token_types