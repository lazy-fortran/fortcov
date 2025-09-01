program test_gcov_generation_defaults
    !! Verify default gcov generation synthesizes .gcov files (no real gcov)
    use gcov_generation_utils
    use config_types, only: config_t
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use iso_fortran_env, only: output_unit
    implicit none

    character(len=*), parameter :: workdir = 'ggd_build'
    type(config_t) :: cfg
    type(error_context_t) :: err
    character(len=:), allocatable :: gcda(:)
    character(len=:), allocatable :: gcov_files(:)
    integer :: n
    logical :: ok

    write(output_unit,'(A)') 'Test: gcov generation defaults (synthetic)'

    call setup_workspace()

    allocate(character(len=256) :: gcda(1))
    gcda(1) = workdir // '/unit.gcda'

    call generate_gcov_files(workdir, gcda, cfg, gcov_files, err)

    ok = (err%error_code == ERROR_SUCCESS)
    call report_assert(ok, 'generate_gcov_files returns success by default')

    if (ok) then
        n = size(gcov_files)
        call report_assert(n >= 1, 'at least one .gcov file produced')
    end if

    call cleanup_workspace()

contains

    subroutine setup_workspace()
        call execute_command_line('rm -rf ' // workdir)
        call execute_command_line('mkdir -p ' // workdir)
        call execute_command_line('touch ' // workdir // '/unit.gcda')
        call execute_command_line('touch ' // workdir // '/unit.gcno')
    end subroutine setup_workspace

    subroutine cleanup_workspace()
        call execute_command_line('rm -rf ' // workdir)
    end subroutine cleanup_workspace

    subroutine report_assert(cond, desc)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: desc
        if (cond) then
            write(output_unit,'(A,A)') '  ✓ ', trim(desc)
        else
            write(output_unit,'(A,A)') '  ✗ ', trim(desc)
            stop 1
        end if
    end subroutine report_assert

end program test_gcov_generation_defaults

