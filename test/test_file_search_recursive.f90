program test_file_search_recursive
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use file_search_secure, only: safe_find_files_with_glob, get_process_id
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_recursive_glob_nested_file()
    call report()

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//msg
        end if
    end subroutine assert

    subroutine test_recursive_glob_nested_file()
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: files(:)
        character(len=256) :: root, nested, path, cmd
        integer :: pid, unit, ios

        call get_process_id(pid)
        write (root, '("/tmp/fortcov-find-",i0)') pid
        nested = trim(root)//'/build/fo/obj'
        path = trim(nested)//'/sample.f90.gcov'

        cmd = 'rm -rf '//trim(root)//' && mkdir -p '//trim(nested)
        call execute_command_line(trim(cmd), exitstat=ios)
        call assert(ios == 0, 'creates nested fixture directory')

        open (newunit=unit, file=trim(path), status='replace', action='write', &
            iostat=ios)
        call assert(ios == 0, 'creates nested gcov file')
        if (ios == 0) then
            write (unit, '(a)') '        -:    0:Source:sample.f90'
            close (unit)
        end if

        call safe_find_files_with_glob(trim(root), '**/*.gcov', files, error_ctx)
        call assert(error_ctx%error_code == ERROR_SUCCESS, &
            'recursive glob succeeds')
        call assert(allocated(files), 'recursive glob allocates result')
        if (allocated(files)) then
            call assert(size(files) == 1, 'recursive glob finds nested file')
        end if

        call execute_command_line('rm -rf '//trim(root))
    end subroutine test_recursive_glob_nested_file

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'file_search_recursive: pass=', &
            n_pass, ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_file_search_recursive
