module json_kinds
    !! Minimal kinds to satisfy code without json-fortran
    implicit none
    public

    integer, parameter :: IK = selected_int_kind(9)
    integer, parameter :: RK = kind(1.0)

end module json_kinds

