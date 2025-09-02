module config_validators
    use config_types, only: config_t
    use path_security, only: validate_executable_path, validate_path_security
    use error_handling_core
    use input_validation_core
    use config_validators_format
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
            inquire(file=trim(files(i)), exist=file_exists)
            if (.not. file_exists) then
                is_valid = .false.
                error_message = "Coverage file not found: " // trim(files(i))
                return
            end if

            ! Check file format
            if (.not. is_valid_coverage_file_format(files(i))) then
                is_valid = .false.
                error_message = "Invalid coverage file format: " // trim(files(i)) // &
                                ". Expected .gcov"
                return
            end if

            ! Check file is readable
            if (.not. is_file_readable(files(i))) then
                is_valid = .false.
                error_message = "Cannot read coverage file: " // trim(files(i))
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
            error_message = "Unsupported output format: " // trim(config%output_format)
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
        if (config%fail_under_threshold < 0.0 .or. config%fail_under_threshold > 100.0) then
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
        inquire(file=trim(config%diff_baseline_file), exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Diff baseline file not found: " // trim(config%diff_baseline_file)
            return
        end if

        if (.not. is_file_readable(config%diff_baseline_file)) then
            is_valid = .false.
            error_message = "Cannot read diff baseline file: " // trim(config%diff_baseline_file)
            return
        end if

        ! Check current file exists and is readable
        inquire(file=trim(config%diff_current_file), exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Diff current file not found: " // trim(config%diff_current_file)
            return
        end if

        if (.not. is_file_readable(config%diff_current_file)) then
            is_valid = .false.
            error_message = "Cannot read diff current file: " // trim(config%diff_current_file)
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
                error_message = "Invalid source path: " // trim(error_ctx%message)
                return
            end if

            ! Check path exists (file or directory)
            inquire(file=trim(paths(i)), exist=path_exists)
            if (.not. path_exists) then
                is_valid = .false.
                error_message = "Source path not found: " // trim(paths(i))
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
            error_message = "Invalid output path: " // trim(error_ctx%message)
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
        inquire(file=trim(import_file), exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Import file not found: " // trim(import_file)
            return
        end if

        ! Import of external coverage files (JSON/XML) is no longer supported
        if (.not. is_valid_import_file_format(import_file)) then
            is_valid = .false.
            error_message = "Unsupported import file: " // trim(import_file) // &
                            ". Only native .gcov inputs are supported"
            return
        end if

        ! Check file is readable
        if (.not. is_file_readable(import_file)) then
            is_valid = .false.
            error_message = "Cannot read import file: " // trim(import_file)
            return
        end if

    end subroutine validate_import_file

    subroutine validate_gcov_executable(gcov_path, is_valid, error_message)
        !! SECURITY FIX Issue #963: gcov_executable validation DISABLED
        !! Native .gcov file parsing eliminates need for gcov executable validation
        character(len=*), intent(in) :: gcov_path
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        ! Always return valid - no gcov executable needed
        is_valid = .true.
        error_message = "SECURITY FIX: Native .gcov parsing - no gcov executable required"

    end subroutine validate_gcov_executable

    subroutine validate_diff_files(config, is_valid, error_message)
        !! Validate diff files - wrapper for diff configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        call validate_diff_configuration(config, is_valid, error_message)

    end subroutine validate_diff_files

end module config_validators
