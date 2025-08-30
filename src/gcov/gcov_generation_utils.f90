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
    use config_types, only: config_t
    use gcov_file_discovery, only: discover_gcov_files
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

        integer :: i, gcov_command_result
        character(len=1024) :: gcov_command
        character(len=256) :: gcno_file, gcda_base, gcov_exec
        logical :: gcno_exists

        call clear_error_context(error_ctx)

        ! Initialize gcov executable with safe default
        if (allocated(config%gcov_executable)) then
            gcov_exec = trim(config%gcov_executable)
        else
            gcov_exec = 'gcov'  ! Use default gcov if not configured
        end if

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

            ! Run gcov on the .gcda file with full path
            write(gcov_command, '(A,1X,A)') trim(gcov_exec), &
                trim(gcda_files(i))
            call execute_command_line(trim(gcov_command), &
                exitstat=gcov_command_result)

            if (gcov_command_result /= 0) then
                call safe_write_message(error_ctx, &
                    'Gcov command failed for: ' // trim(gcda_files(i)))
                call safe_write_suggestion(error_ctx, &
                    'Check gcov executable path and .gcda/.gcno file integrity')
                call safe_write_context(error_ctx, 'gcov execution')
                error_ctx%error_code = 1
                return
            end if
        end do

        ! Discover generated .gcov files in current directory and subdirectories
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


end module gcov_generation_utils