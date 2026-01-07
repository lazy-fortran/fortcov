module gcov_generation_utils
    !! GCov Generation Utilities Module
    !!
    !! Provides utilities for generating .gcov files from .gcda files using
    !! the gcov executable. Handles gcov command execution, file validation,
    !! and generated file discovery with comprehensive error handling.
    !!
    !! Key Features:
    !! - Safe gcov command execution with configurable executable
    !! - .gcno file validation before gcov execution
    !! - Generated .gcov file discovery and validation
    !! - Comprehensive error handling and context reporting

    use error_handling_core, only: error_context_t, ERROR_SUCCESS, clear_error_context, &
                              safe_write_message, safe_write_suggestion, safe_write_context
    ! SECURITY FIX Issue #963: safe_execute_gcov removed - shell injection vulnerability
    use config_types, only: config_t
    use gcov_file_discovery, only: discover_gcov_files
    use secure_command_execution, only: secure_execute_command
    implicit none
    private

    public :: generate_gcov_files

contains

    subroutine generate_gcov_files(directory, gcda_files, config, gcov_files, error_ctx)
        !! Generate .gcov files from .gcda files using gcov executable
        character(len=*), intent(in) :: directory
        character(len=*), intent(in) :: gcda_files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        type(error_context_t), intent(out) :: error_ctx

        integer :: i
        character(len=256) :: gcno_file, gcda_base, gcov_exec
        logical :: gcno_exists
        integer :: last_slash, u, ios
        character(len=512) :: out_dir, base_name, gcov_path
        character(len=512) :: created_files(512)
        integer :: created_count
        character(len=16) :: env_val
        integer :: env_len, env_stat
        logical :: use_real_gcov
        character(len=:), allocatable :: cmd
        integer :: cmd_exit

        call clear_error_context(error_ctx)

        ! SECURITY: Use hardcoded 'gcov' command - no user configuration
        gcov_exec = 'gcov'

        ! Feature flag: allow forcing synthetic gcov output for tests.
        ! By default, use real gcov for meaningful coverage data.
        env_val  = ''
        env_len  = 0
        env_stat = 1
        use_real_gcov = .true.
        call get_environment_variable('FORTCOV_USE_REAL_GCOV', env_val, env_len, env_stat)
        if (env_stat == 0 .and. env_len > 0) then
            if (env_val(1:1) == '0' .or. env_val(1:1) == 'N' .or. &
                env_val(1:1) == 'n' .or. env_val(1:1) == 'F' .or. &
                env_val(1:1) == 'f') then
                use_real_gcov = .false.
            else if (env_val(1:1) == '1' .or. env_val(1:1) == 'Y' .or. &
                     env_val(1:1) == 'y' .or. env_val(1:1) == 'T' .or. &
                     env_val(1:1) == 't') then
                use_real_gcov = .true.
            end if
        end if

        if (use_real_gcov) then
            ! Attempt to invoke real gcov; do not synthesize files in this mode.
            created_count = 0
            do i = 1, size(gcda_files)
                gcda_base = gcda_files(i)(1:index(gcda_files(i), '.gcda') - 1)
                gcno_file = trim(gcda_base) // '.gcno'
                inquire(file=trim(gcno_file), exist=gcno_exists)
                if (.not. gcno_exists) then
                    call safe_write_message(error_ctx, 'Missing .gcno file for: ' // trim(gcda_files(i)))
                    call safe_write_suggestion(error_ctx, 'Ensure tests were compiled with coverage flags')
                    call safe_write_context(error_ctx, 'gcov file generation')
                    error_ctx%error_code = 1
                    return
                end if

                last_slash = index(gcda_files(i), '/', back=.true.)
                if (last_slash > 0) then
                    out_dir   = gcda_files(i)(1:last_slash-1)
                    base_name = gcda_files(i)(last_slash+1:len_trim(gcda_files(i)))
                else
                    out_dir   = '.'
                    base_name = gcda_files(i)(1:len_trim(gcda_files(i)))
                end if
                if (len_trim(base_name) > 5) then
                    base_name = base_name(1:len_trim(base_name)-5)
                end if

                ! Validate paths to avoid unsafe shell execution
                if (.not. is_safe_path(out_dir) .or. .not. is_safe_path(gcno_file)) then
                    call safe_write_message(error_ctx, 'Unsafe characters in paths; refusing to run gcov')
                    call safe_write_context(error_ctx, 'gcov file generation')
                    error_ctx%error_code = 1
                    return
                end if

                ! Run: gcov --object-directory=out_dir gcno_file
                cmd = trim(gcov_exec)//' --object-directory='//trim(out_dir)// &
                      ' ' // trim(gcno_file)
                call secure_execute_command(cmd, cmd_exit)
                if (cmd_exit /= 0) then
                    call safe_write_message(error_ctx, 'gcov execution failed for: ' // trim(gcno_file))
                    call safe_write_context(error_ctx, 'gcov file generation')
                    error_ctx%error_code = cmd_exit
                    return
                end if

                ! Best-effort capture of expected gcov file name alongside artifacts
                gcov_path = trim(out_dir) // '/' // trim(base_name) // '.f90.gcov'
                inquire(file=trim(gcov_path), exist=gcno_exists)
                if (gcno_exists) then
                    if (created_count < size(created_files)) then
                        created_count = created_count + 1
                        created_files(created_count) = gcov_path
                    end if
                end if
            end do

            if (created_count > 0) then
                allocate(character(len=512) :: gcov_files(created_count))
                gcov_files(1:created_count) = created_files(1:created_count)
                call clear_error_context(error_ctx)
                error_ctx%error_code = ERROR_SUCCESS
                return
            end if
            ! No .gcov detected; report failure rather than synthesizing
            call safe_write_message(error_ctx, 'gcov did not generate any .gcov files')
            call safe_write_context(error_ctx, 'gcov file generation')
            error_ctx%error_code = 1
            return
        end if

        created_count = 0
        ! Process each .gcda file
        do i = 1, size(gcda_files)
            ! Extract base name without extension
            gcda_base = gcda_files(i)(1:index(gcda_files(i), '.gcda') - 1)

            ! Check if corresponding .gcno file exists
            gcno_file = trim(gcda_base) // '.gcno'
            inquire(file=trim(gcno_file), exist=gcno_exists)

            if (.not. gcno_exists) then
                call safe_write_message(error_ctx, &
                    'Missing .gcno file for: ' // trim(gcda_files(i)))
                call safe_write_suggestion(error_ctx, &
                    'Ensure tests were compiled with coverage flags')
                call safe_write_context(error_ctx, 'gcov file generation')
                error_ctx%error_code = 1
                return
            end if

            ! In lieu of executing gcov (removed for security), synthesize a
            ! minimal .gcov file next to the coverage artifacts so downstream
            ! processing and tests can operate deterministically.
            last_slash = index(gcda_files(i), '/', back=.true.)
            if (last_slash > 0) then
                out_dir   = gcda_files(i)(1:last_slash-1)
                base_name = gcda_files(i)(last_slash+1:len_trim(gcda_files(i)))
            else
                out_dir   = '.'
                base_name = gcda_files(i)(1:len_trim(gcda_files(i)))
            end if
            ! Strip .gcda suffix from base_name
            if (len_trim(base_name) > 5) then
                base_name = base_name(1:len_trim(base_name)-5)
            end if
            gcov_path = trim(out_dir) // '/' // trim(base_name) // '.f90.gcov'
            open(newunit=u, file=gcov_path, status='replace', action='write', iostat=ios)
            if (ios == 0) then
                write(u,'(A)') '        -:    0:Source:' // trim(base_name) // '.f90'
                write(u,'(A)') '        -:    1:module ' // trim(base_name)
                write(u,'(A)') '        1:    2:  implicit none'
                write(u,'(A)') '        -:    3:end module'
                close(u)
                if (created_count < size(created_files)) then
                    created_count = created_count + 1
                    created_files(created_count) = gcov_path
                end if
            end if
        end do

        ! Prefer directly returning the files we created to avoid
        ! dependency on discovery in restricted environments.
        if (created_count > 0) then
            allocate(character(len=512) :: gcov_files(created_count))
            gcov_files(1:created_count) = created_files(1:created_count)
            call clear_error_context(error_ctx)
            error_ctx%error_code = ERROR_SUCCESS
            return
        end if

        ! Fallback: discover generated .gcov files in current directory and subdirectories
        call discover_gcov_files('.', gcov_files, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            ! Try searching more broadly
            call discover_gcov_files(directory, gcov_files, error_ctx)
        end if

        if (.not. allocated(gcov_files) .or. size(gcov_files) == 0) then
            call safe_write_message(error_ctx, &
                'No .gcov files generated from .gcda files')
            call safe_write_suggestion(error_ctx, &
                'Check gcov command output and file permissions')
            call safe_write_context(error_ctx, 'gcov file generation')
            error_ctx%error_code = 1
        end if
    end subroutine generate_gcov_files


    logical function is_safe_path(s) result(ok)
        !! Allow only sane path characters to mitigate shell interpretation.
        character(len=*), intent(in) :: s
        integer :: i, c
        ok = .true.
        do i = 1, len_trim(s)
            c = iachar(s(i:i))
            select case (c)
            case (48:57)          ! 0-9
            case (65:90)          ! A-Z
            case (97:122)         ! a-z
            case (47, 46, 95, 45) ! / . _ -
            case default
                ok = .false.
                return
            end select
        end do
    end function is_safe_path


end module gcov_generation_utils
