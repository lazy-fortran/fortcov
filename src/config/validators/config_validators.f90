module config_validators
    use config_types, only: config_t
    use path_security, only: validate_executable_path, validate_path_security
    use error_handling_core, only: error_context_t, ERROR_SUCCESS
    use config_validators_format, only: is_file_readable, is_supported_output_format, &
                              is_valid_coverage_file_format, is_valid_import_file_format
    implicit none
    private

    ! Public procedures
    public :: validate_coverage_files
    public :: validate_output_settings
    public :: validate_threshold_settings
    public :: validate_diff_configuration
    public :: validate_import_configuration
    public :: validate_source_paths
    public :: validate_output_path
    public :: validate_import_file
    public :: validate_gcov_executable
    public :: validate_diff_files

contains

    subroutine validate_coverage_files(files, is_valid, error_message)
        !! Validate coverage file paths and formats
        character(len=*), intent(in) :: files(:)
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        integer :: i
        logical :: file_exists

        is_valid = .true.
        error_message = ""

        do i = 1, size(files)
            ! Check file exists
            inquire (file=trim(files(i)), exist=file_exists)
            if (.not. file_exists) then
                is_valid = .false.
                error_message = "Coverage file not found: "//trim(files(i))
                return
            end if

            ! Check file format
            if (.not. is_valid_coverage_file_format(files(i))) then
                is_valid = .false.
                error_message = "Invalid coverage file format: "//trim(files(i))// &
                                ". Expected .gcov"
                return
            end if

            ! Check file is readable
            if (.not. is_file_readable(files(i))) then
                is_valid = .false.
                error_message = "Cannot read coverage file: "//trim(files(i))
                return
            end if
        end do

    end subroutine validate_coverage_files

    subroutine validate_output_settings(config, is_valid, error_message)
        !! Validate output configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        is_valid = .true.
        error_message = ""

        ! Check output format is supported
        if (.not. is_supported_output_format(config%output_format)) then
            is_valid = .false.
            error_message = "Unsupported output format: "//trim(config%output_format)
            return
        end if

        ! Validate output path if specified
        if (len_trim(config%output_path) > 0) then
            call validate_output_path(config%output_path, is_valid, error_message)
            if (.not. is_valid) return
        end if

    end subroutine validate_output_settings

    subroutine validate_threshold_settings(config, is_valid, error_message)
        !! Validate threshold configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        is_valid = .true.
        error_message = ""

        ! Validate coverage threshold (0-100)
        if (config%fail_under_threshold < 0.0 .or. &
            config%fail_under_threshold > 100.0) then
            is_valid = .false.
            error_message = "Fail under threshold must be between 0 and 100"
            return
        end if

        ! Additional threshold validation if needed
        if (config%diff_threshold < 0.0 .or. config%diff_threshold > 100.0) then
            is_valid = .false.
            error_message = "Diff threshold must be between 0 and 100"
            return
        end if

    end subroutine validate_threshold_settings

    subroutine validate_diff_configuration(config, is_valid, error_message)
        !! Validate diff configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: file_exists

        is_valid = .true.
        error_message = ""

        ! Check baseline file exists and is readable
        inquire (file=trim(config%diff_baseline_file), exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Diff baseline file not found: "// &
                            trim(config%diff_baseline_file)
            return
        end if

        if (.not. is_file_readable(config%diff_baseline_file)) then
            is_valid = .false.
            error_message = "Cannot read diff baseline file: "// &
                            trim(config%diff_baseline_file)
            return
        end if

        ! Check current file exists and is readable
        inquire (file=trim(config%diff_current_file), exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Diff current file not found: "// &
                            trim(config%diff_current_file)
            return
        end if

        if (.not. is_file_readable(config%diff_current_file)) then
            is_valid = .false.
            error_message = "Cannot read diff current file: "// &
                            trim(config%diff_current_file)
            return
        end if

    end subroutine validate_diff_configuration

    subroutine validate_import_configuration(config, is_valid, error_message)
        !! Validate import configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        is_valid = .true.
        error_message = ""

        call validate_import_file(config%import_file, is_valid, error_message)

    end subroutine validate_import_configuration

    subroutine validate_source_paths(paths, is_valid, error_message)
        !! Validate source paths
        character(len=*), intent(in) :: paths(:)
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        integer :: i
        logical :: path_exists
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx

        is_valid = .true.
        error_message = ""

        do i = 1, size(paths)
            ! SECURITY: Validate path safety before existence checks
            call validate_path_security(paths(i), safe_path, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) then
                is_valid = .false.
                error_message = "Invalid source path: "//trim(error_ctx%message)
                return
            end if

            ! Check path exists (file or directory)
            inquire (file=trim(paths(i)), exist=path_exists)
            if (.not. path_exists) then
                is_valid = .false.
                error_message = "Source path not found: "//trim(paths(i))
                return
            end if
        end do

    end subroutine validate_source_paths

    subroutine validate_output_path(output_path, is_valid, error_message)
        !! Validate output path
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx

        is_valid = .true.
        error_message = ""

        ! Use path validation for security
        call validate_path_security(output_path, safe_path, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            is_valid = .false.
            error_message = "Invalid output path: "//trim(error_ctx%message)
        end if

    end subroutine validate_output_path

    subroutine validate_import_file(import_file, is_valid, error_message)
        !! Validate import file
        character(len=*), intent(in) :: import_file
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: file_exists

        is_valid = .true.
        error_message = ""

        ! Check file exists
        inquire (file=trim(import_file), exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Import file not found: "//trim(import_file)
            return
        end if

        ! Import of external coverage files (JSON/XML) is no longer supported
        if (.not. is_valid_import_file_format(import_file)) then
            is_valid = .false.
            error_message = "Unsupported import file: "//trim(import_file)// &
                            ". Only native .gcov inputs are supported"
            return
        end if

        ! Check file is readable
        if (.not. is_file_readable(import_file)) then
            is_valid = .false.
            error_message = "Cannot read import file: "//trim(import_file)
            return
        end if

    end subroutine validate_import_file

    subroutine validate_gcov_executable(gcov_path, is_valid, error_message)
        character(len=*), intent(in) :: gcov_path
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_exec
        character(len=:), allocatable :: exec_name

        is_valid = .true.
        error_message = ""

        if (len_trim(gcov_path) == 0) then
            return
        end if

        call validate_executable_path(trim(gcov_path), safe_exec, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            is_valid = .false.
            error_message = trim(error_ctx%message)
            return
        end if

        call extract_executable_name(trim(safe_exec), exec_name)
        if (.not. is_valid_executable_token(exec_name)) then
            is_valid = .false.
            error_message = "Invalid gcov executable name: "//trim(exec_name)
            return
        end if

        if (.not. contains_gcov_substring(exec_name)) then
            is_valid = .false.
            error_message = "Invalid gcov executable name: "//trim(exec_name)
            return
        end if

    end subroutine validate_gcov_executable

    pure subroutine extract_executable_name(path, name)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: name
        integer :: slash_pos, backslash_pos, last_sep, path_len, end_pos
        character(len=1), parameter :: backslash = achar(92)

        path_len = len_trim(path)
        if (path_len == 0) then
            name = ""
            return
        end if

        end_pos = path_len
        do while (end_pos > 0)
            if (path(end_pos:end_pos) == '/' .or. &
                path(end_pos:end_pos) == backslash) then
                end_pos = end_pos - 1
            else
                exit
            end if
        end do

        if (end_pos == 0) then
            name = ""
            return
        end if

        slash_pos = index(path(1:end_pos), '/', back=.true.)
        backslash_pos = index(path(1:end_pos), backslash, back=.true.)
        last_sep = max(slash_pos, backslash_pos)

        if (last_sep > 0 .and. last_sep < end_pos) then
            name = path(last_sep + 1:end_pos)
        else
            name = path(1:end_pos)
        end if

    end subroutine extract_executable_name

    pure logical function is_valid_executable_token(name) result(is_allowed)
        character(len=*), intent(in) :: name
        integer :: i, name_len

        name_len = len_trim(name)
        if (name_len == 0) then
            is_allowed = .false.
            return
        end if

        do i = 1, name_len
            select case (name(i:i))
            case ("a":"z", "A":"Z", "0":"9", "-", "_", ".")
                cycle
            case default
                is_allowed = .false.
                return
            end select
        end do

        is_allowed = .true.
    end function is_valid_executable_token

    pure logical function contains_gcov_substring(name) result(has_gcov)
        character(len=*), intent(in) :: name
        integer :: i, name_len

        has_gcov = .false.
        name_len = len_trim(name)
        if (name_len < 4) return

        do i = 1, name_len - 3
            if ((name(i:i) == 'g' .or. name(i:i) == 'G') .and. &
                (name(i + 1:i + 1) == 'c' .or. name(i + 1:i + 1) == 'C') .and. &
                (name(i + 2:i + 2) == 'o' .or. name(i + 2:i + 2) == 'O') .and. &
                (name(i + 3:i + 3) == 'v' .or. name(i + 3:i + 3) == 'V')) then
                has_gcov = .true.
                return
            end if
        end do

    end function contains_gcov_substring

    subroutine validate_diff_files(config, is_valid, error_message)
        !! Validate diff files - wrapper for diff configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        call validate_diff_configuration(config, is_valid, error_message)

    end subroutine validate_diff_files

end module config_validators
