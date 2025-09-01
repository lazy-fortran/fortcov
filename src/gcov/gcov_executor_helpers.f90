module gcov_executor_helpers
    !! Helper routines for gcov_executor to keep module size under limits
    implicit none
    private

    public :: sanitize_file_path
    public :: is_safe_gcov_command

contains

    subroutine sanitize_file_path(path)
        !! Sanitize file path to prevent directory traversal and injection attacks
        character(len=*), intent(inout) :: path
        integer :: i, len_path
        character :: c

        len_path = len_trim(path)

        ! Replace dangerous shell characters with underscores
        do i = 1, len_path
            c = path(i:i)
            if (c == ';' .or. c == '|' .or. c == '&' .or. c == '$' .or. &
                c == '`' .or. c == '<' .or. c == '>' .or. c == '(' .or. &
                c == ')' .or. c == '{' .or. c == '}') then
                path(i:i) = '_'
            end if
        end do

        ! Remove directory traversal patterns
        call remove_pattern(path, '../')
        call remove_pattern(path, '/./')
        call remove_pattern(path, '//')
    end subroutine sanitize_file_path

    subroutine remove_pattern(str, pattern)
        !! Remove all occurrences of pattern from string
        character(len=*), intent(inout) :: str
        character(len=*), intent(in) :: pattern
        integer :: pos, pattern_len

        pattern_len = len(pattern)
        do
            pos = index(str, pattern)
            if (pos == 0) exit
            str = str(1:pos-1) // str(pos+pattern_len:)
        end do
    end subroutine remove_pattern

    logical function is_safe_gcov_command(command) result(is_safe)
        !! Validate gcov command string for safe execution
        character(len=*), intent(in) :: command

        is_safe = .true.

        ! Check for dangerous command injection patterns
        if (index(command, '&&') > 0 .or. index(command, '||') > 0 .or. &
            index(command, ';') > 0 .or. index(command, '`') > 0 .or. &
            index(command, '$') > 0 .or. index(command, '|') > 0 .or. &
            index(command, '>') > 0 .or. index(command, '<') > 0) then
            is_safe = .false.
            return
        end if

        ! Ensure command starts with safe gcov patterns
        if (.not. (index(command, 'gcov ') == 1 .or. &
                   index(command, 'cd ') == 1)) then
            is_safe = .false.
            return
        end if
    end function is_safe_gcov_command

end module gcov_executor_helpers

