module input_validator_source
    use config_types, only: config_t
    use config_validators
    implicit none
    private
    
    ! Public procedures
    public :: validate_input_sources
    
contains

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
        ! Exception: zero-configuration mode handles input discovery automatically
        if (.not. has_source_paths .and. &
            .not. has_coverage_files .and. &
            .not. has_import_file .and. &
            .not. has_diff_files .and. &
            .not. config%zero_configuration_mode) then
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

        ! Validate gcov executable if using source paths (skip in zero-config mode)
        if (has_source_paths .and. .not. config%zero_configuration_mode) then
            call validate_gcov_executable(config%gcov_executable, is_valid, error_message)
            if (.not. is_valid) return
        end if

    end subroutine validate_input_sources

end module input_validator_source