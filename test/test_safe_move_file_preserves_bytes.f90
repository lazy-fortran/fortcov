program test_safe_move_file_preserves_bytes
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use file_ops_secure, only: get_process_id, safe_move_file, safe_remove_file
    implicit none

    character(len=:), allocatable :: source_path
    character(len=:), allocatable :: target_path
    character(len=:), allocatable :: expected
    character(len=:), allocatable :: actual
    character(len=32) :: suffix
    logical :: all_tests_passed
    logical :: exists
    integer :: pid
    integer :: clock_count
    integer :: file_size
    integer :: unit
    integer :: ios
    type(error_context_t) :: err

    all_tests_passed = .true.

    call get_process_id(pid)
    call system_clock(count=clock_count)
    write (suffix, '(I0,A,I0)') pid, '_', clock_count
    source_path = "/tmp/fortcov-test-safe-move-"//trim(suffix)//".bin"
    target_path = "/tmp/fortcov-test-safe-move-"//trim(suffix)//".out"

    expected = repeat('A', 2048)//repeat(' ', 8)//achar(0)//'Z'//achar(10)//'TAIL  '

    open (newunit=unit, file=source_path, access='stream', form='unformatted', &
          action='write', status='replace', iostat=ios)
    if (ios /= 0) then
        call fail("Could not create source file: "//trim(source_path))
        call cleanup()
        stop 1
    end if
    write (unit, iostat=ios) expected
    close (unit)
    if (ios /= 0) then
        call fail("Could not write source bytes to: "//trim(source_path))
        call cleanup()
        stop 1
    end if

    call safe_move_file(source_path, target_path, err)
    if (err%error_code /= ERROR_SUCCESS) then
        call fail("safe_move_file failed: "//trim(err%message))
        call cleanup()
        stop 1
    end if

    inquire (file=source_path, exist=exists)
    if (exists) then
        call fail("Source file still exists after move: "//trim(source_path))
        call cleanup()
        stop 1
    end if

    inquire (file=target_path, exist=exists)
    if (.not. exists) then
        call fail("Target file missing after move: "//trim(target_path))
        call cleanup()
        stop 1
    end if

    inquire (file=target_path, size=file_size, iostat=ios)
    if (ios /= 0) then
        call fail("Could not inquire target file size: "//trim(target_path))
        call cleanup()
        stop 1
    end if

    allocate (character(len=file_size) :: actual)

    open (newunit=unit, file=target_path, access='stream', form='unformatted', &
          action='read', status='old', iostat=ios)
    if (ios /= 0) then
        call fail("Could not open target file: "//trim(target_path))
        call cleanup()
        stop 1
    end if
    read (unit, iostat=ios) actual
    close (unit)
    if (ios /= 0) then
        call fail("Could not read target bytes from: "//trim(target_path))
        call cleanup()
        stop 1
    end if

    if (len(actual) /= len(expected)) then
        write (output_unit, '(A,I0)') "Expected size: ", len(expected)
        write (output_unit, '(A,I0)') "Actual size:   ", len(actual)
        call fail("Moved file size mismatch")
        call cleanup()
        stop 1
    end if

    if (actual /= expected) then
        call fail("Moved file bytes do not match source bytes")
        call cleanup()
        stop 1
    end if

    call cleanup()

    if (all_tests_passed) then
        write (output_unit, '(A)') "SAFE MOVE FILE BYTES TEST PASSED"
        stop 0
    end if

    stop 1

contains

    subroutine fail(message)
        character(len=*), intent(in) :: message
        all_tests_passed = .false.
        write (error_unit, '(A)') "FAIL: "//trim(message)
    end subroutine fail

    subroutine cleanup()
        type(error_context_t) :: rm_err
        call safe_remove_file(source_path, rm_err)
        call safe_remove_file(target_path, rm_err)
    end subroutine cleanup

end program test_safe_move_file_preserves_bytes
