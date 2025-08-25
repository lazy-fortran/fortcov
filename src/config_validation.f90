module config_validation
    !! Configuration validation routines
    !! 
    !! This module provides comprehensive validation for configuration settings,
    !! ensuring all parameters meet requirements before processing begins.

    use config_types, only: config_t, MAX_ARRAY_SIZE
    use foundation_constants
    use foundation_layer_utils
    use string_utils
    use file_utils
    use error_handling
    use input_validation
    use path_validation, only: validate_executable_path

    implicit none
    private

    public :: validate_complete_config
    public :: validate_input_sources
    public :: validate_coverage_files
    public :: validate_output_settings
    public :: validate_threshold_settings
    public :: validate_diff_configuration
    public :: validate_import_configuration
    public :: validate_source_paths
    public :: validate_output_path
    public :: validate_import_file
    public :: validate_gcov_executable
    public :: is_supported_output_format
    public :: is_valid_coverage_file_format
    public :: is_valid_import_file_format
    public :: is_file_readable

contains

    function validate_complete_config(config) result(is_valid)
        !! Validate entire configuration
        type(config_t), intent(in) :: config
        logical :: is_valid

        character(len=512) :: error_message
        logical :: partial_valid

        is_valid = .true.

        ! Skip validation for help/version modes
        if (config%show_help .or. config%show_version) then
            return
        end if

        ! Validate input sources
        call validate_input_sources(config, partial_valid, error_message)
        if (.not. partial_valid) then
            if (config%verbose) then
                print '(A)', "Validation error: " // trim(error_message)
            end if
            is_valid = .false.
            return
        end if

        ! Validate output settings
        call validate_output_settings(config, partial_valid, error_message)
        if (.not. partial_valid) then
            if (config%verbose) then
                print '(A)', "Validation error: " // trim(error_message)
            end if
            is_valid = .false.
            return
        end if

        ! Validate threshold settings
        call validate_threshold_settings(config, partial_valid, error_message)
        if (.not. partial_valid) then
            if (config%verbose) then
                print '(A)', "Validation error: " // trim(error_message)
            end if
            is_valid = .false.
            return
        end if

        ! Validate diff configuration if enabled
        if (config%enable_diff) then
            call validate_diff_configuration(config, partial_valid, error_message)
            if (.not. partial_valid) then
                if (config%verbose) then
                    print '(A)', "Validation error: " // trim(error_message)
                end if
                is_valid = .false.
                return
            end if
        end if

        ! Validate import configuration if specified
        if (len_trim(config%import_file) > 0) then
            call validate_import_configuration(config, partial_valid, error_message)
            if (.not. partial_valid) then
                if (config%verbose) then
                    print '(A)', "Validation error: " // trim(error_message)
                end if
                is_valid = .false.
                return
            end if
        end if

        ! Validate thread count
        if (config%threads < 1 .or. config%threads > 256) then
            if (config%verbose) then
                print '(A)', "Validation error: Thread count must be between 1 and 256"
            end if
            is_valid = .false.
        end if

    end function validate_complete_config

    subroutine validate_input_sources(config, is_valid, error_message)
        !! Validate input source configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: has_source_paths
        logical :: has_coverage_files
        logical :: has_import_file
        logical :: has_diff_files

        is_valid = .true.
        error_message = ""

        has_source_paths = allocated(config%source_paths) .and. &
                           size(config%source_paths) > 0
        has_coverage_files = allocated(config%coverage_files) .and. &
                             size(config%coverage_files) > 0
        has_import_file = len_trim(config%import_file) > 0
        has_diff_files = config%enable_diff .and. &
                         len_trim(config%diff_baseline_file) > 0 .and. &
                         len_trim(config%diff_current_file) > 0

        ! Must have at least one input source (including diff files)
        if (.not. has_source_paths .and. &
            .not. has_coverage_files .and. &
            .not. has_import_file .and. &
            .not. has_diff_files) then
            is_valid = .false.
            error_message = "No input sources specified. Provide source paths, coverage files, import file, or diff files"
            return
        end if

        ! Can't mix import with other sources
        if (has_import_file .and. (has_source_paths .or. has_coverage_files)) then
            is_valid = .false.
            error_message = "Cannot mix import file with source paths or coverage files"
            return
        end if

        ! Can't mix diff with other sources (diff mode is standalone)
        if (has_diff_files .and. (has_source_paths .or. has_coverage_files .or. has_import_file)) then
            is_valid = .false.
            error_message = "Cannot mix diff mode with source paths, coverage files, or import file"
            return
        end if

        ! Validate source paths if present
        if (has_source_paths) then
            call validate_source_paths(config%source_paths, is_valid, error_message)
            if (.not. is_valid) return
        end if

        ! Validate coverage files if present
        if (has_coverage_files) then
            call validate_coverage_files(config%coverage_files, is_valid, error_message)
            if (.not. is_valid) return
        end if

        ! Validate diff files if present
        if (has_diff_files) then
            call validate_diff_files(config, is_valid, error_message)
            if (.not. is_valid) return
        end if

        ! Validate gcov executable if using source paths
        if (has_source_paths) then
            call validate_gcov_executable(config%gcov_executable, is_valid, error_message)
            if (.not. is_valid) return
        end if

    end subroutine validate_input_sources

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
                                ". Expected .gcov, .json, or .xml"
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

        ! Check that HTML output has a path
        if (config%output_format == "html" .and. &
            len_trim(config%output_path) == 0) then
            is_valid = .false.
            error_message = "HTML output requires an output path"
            return
        end if

    end subroutine validate_output_settings

    subroutine validate_threshold_settings(config, is_valid, error_message)
        !! Validate coverage threshold settings
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        is_valid = .true.
        error_message = ""

        ! Check minimum coverage is valid percentage
        if (config%minimum_coverage < 0.0 .or. config%minimum_coverage > 100.0) then
            is_valid = .false.
            error_message = "Minimum coverage must be between 0 and 100"
            return
        end if

        ! Check fail threshold is valid percentage
        if (config%fail_under_threshold < 0.0 .or. config%fail_under_threshold > 100.0) then
            is_valid = .false.
            error_message = "Fail threshold must be between 0 and 100"
            return
        end if

        ! Check diff threshold is valid if diff mode is enabled
        if (config%enable_diff) then
            if (config%diff_threshold < -100.0 .or. config%diff_threshold > 100.0) then
                is_valid = .false.
                error_message = "Diff threshold must be between -100 and 100"
                return
            end if
        end if

    end subroutine validate_threshold_settings

    subroutine validate_diff_configuration(config, is_valid, error_message)
        !! Validate diff mode configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: baseline_exists, current_exists

        is_valid = .true.
        error_message = ""

        ! Check baseline file is specified
        if (len_trim(config%diff_baseline_file) == 0) then
            is_valid = .false.
            error_message = "Diff mode requires baseline file"
            return
        end if

        ! Check current file is specified
        if (len_trim(config%diff_current_file) == 0) then
            is_valid = .false.
            error_message = "Diff mode requires current file"
            return
        end if

        ! Check baseline file exists
        inquire(file=trim(config%diff_baseline_file), exist=baseline_exists)
        if (.not. baseline_exists) then
            is_valid = .false.
            error_message = "Baseline file not found: " // trim(config%diff_baseline_file)
            return
        end if

        ! Check current file exists
        inquire(file=trim(config%diff_current_file), exist=current_exists)
        if (.not. current_exists) then
            is_valid = .false.
            error_message = "Current file not found: " // trim(config%diff_current_file)
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

        ! Validate import file
        call validate_import_file(config%import_file, is_valid, error_message)
        if (.not. is_valid) return

        ! Check import file format
        if (.not. is_valid_import_file_format(config%import_file)) then
            is_valid = .false.
            error_message = "Invalid import file format. Expected JSON or XML"
            return
        end if

    end subroutine validate_import_configuration

    subroutine validate_source_paths(paths, is_valid, error_message)
        !! Validate source code paths
        character(len=*), intent(in) :: paths(:)
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        integer :: i
        logical :: path_exists

        is_valid = .true.
        error_message = ""

        if (size(paths) == 0) then
            is_valid = .false.
            error_message = "No source paths specified"
            return
        end if

        do i = 1, size(paths)
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

        character(len=512) :: dir_path
        integer :: last_slash
        logical :: dir_exists, is_writable

        is_valid = .true.
        error_message = ""

        ! Extract directory from path
        last_slash = index(output_path, '/', back=.true.)
        if (last_slash > 0) then
            dir_path = output_path(1:last_slash-1)
        else
            dir_path = "."
        end if

        ! Check directory exists
        inquire(file=trim(dir_path), exist=dir_exists)
        if (.not. dir_exists) then
            is_valid = .false.
            error_message = "Output directory does not exist: " // trim(dir_path)
            return
        end if

        ! Check directory is writable (simple check)
        ! More comprehensive check would require attempting to create a temp file
        is_writable = .true.  ! Assume writable for now

        if (.not. is_writable) then
            is_valid = .false.
            error_message = "Output directory is not writable: " // trim(dir_path)
            return
        end if

    end subroutine validate_output_path

    subroutine validate_import_file(import_file, is_valid, error_message)
        !! Validate import file path
        character(len=*), intent(in) :: import_file
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: file_exists

        is_valid = .true.
        error_message = ""

        inquire(file=trim(import_file), exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Import file not found: " // trim(import_file)
            return
        end if

        if (.not. is_file_readable(import_file)) then
            is_valid = .false.
            error_message = "Cannot read import file: " // trim(import_file)
            return
        end if

    end subroutine validate_import_file

    function is_supported_output_format(format) result(is_supported)
        !! Check if output format is supported
        character(len=*), intent(in) :: format
        logical :: is_supported

        select case (trim(adjustl(format)))
        case ("terminal", "json", "xml", "html", "lcov", "cobertura", "markdown")
            is_supported = .true.
        case default
            is_supported = .false.
        end select

    end function is_supported_output_format

    function is_valid_coverage_file_format(file_path) result(is_valid)
        !! Check if file has valid coverage format extension
        character(len=*), intent(in) :: file_path
        logical :: is_valid

        character(len=:), allocatable :: extension
        integer :: dot_pos

        is_valid = .false.

        ! Find last dot in filename
        dot_pos = index(file_path, '.', back=.true.)
        if (dot_pos == 0) return

        ! Extract extension
        extension = file_path(dot_pos+1:)

        ! Check against valid extensions
        select case (trim(adjustl(extension)))
        case ("gcov")
            is_valid = .true.
        case ("json")
            ! Could be JSON coverage format
            is_valid = .true.
        case ("xml")
            ! Could be Cobertura XML format
            is_valid = .true.
        case ("info")
            ! LCOV info format
            is_valid = .true.
        case default
            ! Also check for .gcov.json pattern
            if (index(file_path, ".gcov.json") > 0) then
                is_valid = .true.
            end if
        end select

    end function is_valid_coverage_file_format

    function is_valid_import_file_format(file_path) result(is_valid)
        !! Check if file has valid import format
        character(len=*), intent(in) :: file_path
        logical :: is_valid

        is_valid = index(file_path, ".json") > 0 .or. &
                   index(file_path, ".xml") > 0

    end function is_valid_import_file_format

    function is_file_readable(file_path) result(is_readable)
        !! Check if file is readable
        character(len=*), intent(in) :: file_path
        logical :: is_readable

        integer :: unit, iostat

        is_readable = .false.

        open(newunit=unit, file=trim(file_path), status='old', &
             action='read', iostat=iostat)
        if (iostat == 0) then
            is_readable = .true.
            close(unit)
        end if

    end function is_file_readable

    subroutine validate_gcov_executable(gcov_path, is_valid, error_message)
        !! Validate gcov executable path
        character(len=*), intent(in) :: gcov_path
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        character(len=:), allocatable :: validated_path
        type(error_context_t) :: error_ctx

        is_valid = .true.
        error_message = ""

        ! Use secure command executor for validation
        call validate_executable_path(gcov_path, validated_path, error_ctx)

        if (error_ctx%error_code /= ERROR_SUCCESS) then
            is_valid = .false.
            error_message = "Invalid or inaccessible gcov executable: " // trim(gcov_path)
        end if

    end subroutine validate_gcov_executable

    subroutine validate_diff_files(config, is_valid, error_message)
        !! Validate diff file configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message

        logical :: baseline_exists, current_exists

        is_valid = .true.
        error_message = ""

        ! Check if parsing failed during flag recognition
        if (config%diff_baseline_file == "PARSE_ERROR") then
            is_valid = .false.
            error_message = trim(config%diff_current_file)  ! Error message stored here
            return
        end if

        ! Validate baseline file exists
        inquire(file=config%diff_baseline_file, exist=baseline_exists)
        if (.not. baseline_exists) then
            is_valid = .false.
            error_message = "Diff baseline file not found: " // trim(config%diff_baseline_file)
            return
        end if

        ! Validate current file exists  
        inquire(file=config%diff_current_file, exist=current_exists)
        if (.not. current_exists) then
            is_valid = .false.
            error_message = "Diff current file not found: " // trim(config%diff_current_file)
            return
        end if

        ! Validate files are readable
        if (.not. is_file_readable(config%diff_baseline_file)) then
            is_valid = .false.
            error_message = "Cannot read diff baseline file: " // trim(config%diff_baseline_file)
            return
        end if

        if (.not. is_file_readable(config%diff_current_file)) then
            is_valid = .false.
            error_message = "Cannot read diff current file: " // trim(config%diff_current_file)
            return
        end if

    end subroutine validate_diff_files

end module config_validation