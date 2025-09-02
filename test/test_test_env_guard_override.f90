program test_test_env_guard_override
    use iso_fortran_env, only: error_unit
    use iso_c_binding, only: c_int, c_char
    use test_env_guard, only: running_under_test_env
    implicit none

    interface
        function c_setenv(name, value, overwrite) bind(C, name="setenv") result(ret)
            import :: c_int, c_char
            character(kind=c_char), dimension(*) :: name
            character(kind=c_char), dimension(*) :: value
            integer(c_int), value :: overwrite
            integer(c_int) :: ret
        end function c_setenv
    end interface

    logical :: is_test

    if (c_setenv('FORTCOV_TEST_MODE' // char(0), '1' // char(0), 1_c_int) /= 0_c_int) then
        write(error_unit, '(A)') 'Failed to set FORTCOV_TEST_MODE'
        stop 1
    end if

    is_test = running_under_test_env()
    if (.not. is_test) then
        write(error_unit, '(A)') 'Expected test environment detection to be TRUE'
        stop 2
    end if

    if (c_setenv('FORTCOV_ALLOW_AUTOTEST' // char(0), '1' // char(0), 1_c_int) /= 0_c_int) then
        write(error_unit, '(A)') 'Failed to set FORTCOV_ALLOW_AUTOTEST'
        stop 3
    end if

    is_test = running_under_test_env()
    if (is_test) then
        write(error_unit, '(A)') 'Override did not bypass test environment guard'
        stop 4
    end if

    print *, 'OK: test_env_guard override bypasses test detection when enabled'
end program test_test_env_guard_override
