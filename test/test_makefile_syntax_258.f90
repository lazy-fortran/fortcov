program test_makefile_syntax_258
    !! Test Makefile syntax validation for issue 258.
    use, intrinsic :: iso_fortran_env, only: output_unit, int32
    use secure_command_execution, only: secure_execute_command
    implicit none

    character(len=:), allocatable :: repo_root
    character(len=:), allocatable :: make_dir
    character(len=:), allocatable :: makefile_path
    character(len=:), allocatable :: log_path
    character(len=16), dimension(7) :: targets
    character(len=16) :: target
    character(len=*), parameter :: missing_token = "missing separator"
    character(len=*), parameter :: expected_line = &
                                   "echo '<!DOCTYPE html>' > coverage.html"
    integer :: failures
    integer :: i
    integer(int32) :: exit_code
    logical :: has_missing
    logical :: has_expected

    failures = 0
    targets = [character(len=16) :: "all", "test", "coverage", "clean", &
               "clean-coverage", "distclean", "help"]

    call get_repo_root(repo_root)
    make_dir = trim(repo_root)// &
               "/examples/build_systems/make/basic_example"
    makefile_path = trim(make_dir)//"/Makefile"

    call check_path_exists(make_dir, "Makefile directory", failures)
    call check_path_exists(makefile_path, "Makefile", failures)

    do i = 1, size(targets)
        target = targets(i)
        call run_make_target(make_dir, trim(target), log_path, exit_code)
        if (exit_code == 0) then
            write (output_unit, '(A,A)') '  [PASS] make -n ', trim(target)
        else
            write (output_unit, '(A,A)') '  [FAIL] make -n ', trim(target)
            failures = failures + 1
        end if

        call log_contains(log_path, missing_token, .true., has_missing)
        if (has_missing) then
            write (output_unit, '(A,A)') '  [FAIL] missing separator in ', &
                trim(target)
            failures = failures + 1
        end if

        if (trim(target) == "coverage") then
            call log_contains(log_path, expected_line, .false., has_expected)
            if (has_expected) then
                write (output_unit, '(A)') '  [PASS] coverage recipe emits HTML'
            else
                write (output_unit, '(A)') '  [FAIL] coverage recipe missing HTML'
                failures = failures + 1
            end if
        end if
    end do

    if (failures > 0) then
        error stop 1
    end if

contains

    subroutine get_repo_root(root)
        character(len=:), allocatable, intent(out) :: root
        character(len=1024) :: exe_path
        character(len=1024) :: pwd
        integer :: length
        integer :: pos
        integer :: status

        exe_path = ""
        call get_command_argument(0, exe_path, length)
        if (length > 0) then
            pos = index(exe_path(1:length), "/build/")
            if (pos > 0) then
                root = exe_path(1:pos - 1)
                return
            end if
        end if

        pwd = ""
        call get_environment_variable("PWD", pwd, status=status)
        if (status == 0 .and. len_trim(pwd) > 0) then
            root = trim(pwd)
        else
            root = "."
        end if
    end subroutine get_repo_root

    subroutine check_path_exists(path, label, failures)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: label
        integer, intent(inout) :: failures
        logical :: exists

        inquire (file=trim(path), exist=exists)
        if (exists) then
            write (output_unit, '(A,A)') '  [PASS] found ', trim(label)
        else
            write (output_unit, '(A,A,A)') '  [FAIL] missing ', trim(label), ':', &
                trim(path)
            failures = failures + 1
        end if
    end subroutine check_path_exists

    subroutine run_make_target(make_dir, target, log_path, exit_code)
        character(len=*), intent(in) :: make_dir
        character(len=*), intent(in) :: target
        character(len=:), allocatable, intent(out) :: log_path
        integer(int32), intent(out) :: exit_code
        character(len=2048) :: command

        call make_temp_log(target, log_path)
        command = "cd "//trim(make_dir)//" && make -n "//trim(target)
        command = trim(command)//" > "//trim(log_path)//" 2>&1"
        call secure_execute_command(command, exit_code)
    end subroutine run_make_target

    subroutine make_temp_log(target, log_path)
        character(len=*), intent(in) :: target
        character(len=:), allocatable, intent(out) :: log_path
        character(len=512) :: path_buf
        integer :: count
        call system_clock(count)
        write (path_buf, '(A,A,A,I0,A)') "/tmp/fortcov_make_", trim(target), &
            "_", count, ".log"
        log_path = trim(path_buf)
    end subroutine make_temp_log

    subroutine log_contains(path, token, case_insensitive, found)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: token
        logical, intent(in) :: case_insensitive
        logical, intent(out) :: found
        character(len=4096) :: line
        character(len=4096) :: line_cmp
        character(len=256) :: token_cmp
        integer :: unit
        integer :: stat

        found = .false.
        token_cmp = token
        if (case_insensitive) then
            token_cmp = to_lower(token)
        end if

        open (newunit=unit, file=trim(path), status="old", action="read", &
              iostat=stat)
        if (stat /= 0) then
            return
        end if

        do
            read (unit, '(A)', iostat=stat) line
            if (stat /= 0) then
                exit
            end if
            if (case_insensitive) then
                line_cmp = to_lower(line)
            else
                line_cmp = line
            end if
            if (index(line_cmp, trim(token_cmp)) > 0) then
                found = .true.
                exit
            end if
        end do

        close (unit)
    end subroutine log_contains

    pure function to_lower(text) result(lowered)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lowered
        integer :: i
        integer :: code

        lowered = text
        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar("A") .and. code <= iachar("Z")) then
                lowered(i:i) = achar(code + 32)
            end if
        end do
    end function to_lower

end program test_makefile_syntax_258
