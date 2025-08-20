module config_validator
    !! Configuration Validator (Decomposed from fortcov_config.f90)
    !! 
    !! Focused on validation of configuration parameters and settings.
    !! Separated from parsing and storage for better separation of concerns.
    use foundation_constants
    use foundation_layer_utils
    use config_parser, only: config_t
    use string_utils
    use file_utils
    use error_handling
    use input_validation
    implicit none
    private
    
    public :: validate_complete_config
    public :: validate_input_sources
    public :: validate_coverage_files
    public :: validate_output_settings
    public :: validate_threshold_settings
    public :: validate_diff_configuration
    public :: validate_import_configuration
    
contains
    
    function validate_complete_config(config) result(is_valid)
        !! Comprehensive configuration validation
        !! Extracted from original validate_config function
        type(config_t), intent(in) :: config
        logical :: is_valid
        
        character(len=LONG_STRING_LEN) :: error_message
        logical :: sources_valid, files_valid, output_valid
        logical :: threshold_valid, diff_valid, import_valid
        
        is_valid = .true.
        
        ! Validate input sources
        call validate_input_sources(config, sources_valid, error_message)
        if (.not. sources_valid) then
            print *, "❌ Input sources validation failed: " // trim(error_message)
            is_valid = .false.
        end if
        
        ! Validate coverage files if specified
        if (allocated(config%coverage_files)) then
            call validate_coverage_files(config%coverage_files, files_valid, error_message)
            if (.not. files_valid) then
                print *, "❌ Coverage files validation failed: " // trim(error_message)
                is_valid = .false.
            end if
        end if
        
        ! Validate output settings
        call validate_output_settings(config, output_valid, error_message)
        if (.not. output_valid) then
            print *, "❌ Output settings validation failed: " // trim(error_message)
            is_valid = .false.
        end if
        
        ! Validate threshold settings
        call validate_threshold_settings(config, threshold_valid, error_message)
        if (.not. threshold_valid) then
            print *, "❌ Threshold settings validation failed: " // trim(error_message)
            is_valid = .false.
        end if
        
        ! Validate diff configuration if enabled
        if (config%enable_diff) then
            call validate_diff_configuration(config, diff_valid, error_message)
            if (.not. diff_valid) then
                print *, "❌ Diff configuration validation failed: " // trim(error_message)
                is_valid = .false.
            end if
        end if
        
        ! Validate import configuration if specified
        if (allocated(config%import_file)) then
            call validate_import_configuration(config, import_valid, error_message)
            if (.not. import_valid) then
                print *, "❌ Import configuration validation failed: " // trim(error_message)
                is_valid = .false.
            end if
        end if
        
    end function validate_complete_config
    
    subroutine validate_input_sources(config, is_valid, error_message)
        !! Validates that input sources are provided
        !! Extracted from original validate_input_sources function
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        logical :: has_source_paths, has_coverage_files, has_import_file
        
        is_valid = .true.
        error_message = ""
        
        ! Check for various input source types
        has_source_paths = allocated(config%source_paths)
        has_coverage_files = allocated(config%coverage_files)
        has_import_file = allocated(config%import_file)
        
        ! Must have at least one input source
        if (.not. has_source_paths .and. .not. has_coverage_files .and. &
            .not. has_import_file) then
            is_valid = .false.
            error_message = "No input sources specified. Provide source paths, " // &
                          "coverage files, or import file."
            return
        end if
        
        ! Validate source paths if provided
        if (has_source_paths) then
            call validate_source_paths(config%source_paths, is_valid, error_message)
            if (.not. is_valid) return
        end if
        
        ! Validate coverage files if provided
        if (has_coverage_files) then
            call validate_coverage_files(config%coverage_files, is_valid, error_message)
            if (.not. is_valid) return
        end if
        
        ! Validate import file if provided
        if (has_import_file) then
            call validate_import_file(config%import_file, is_valid, error_message)
            if (.not. is_valid) return
        end if
        
    end subroutine validate_input_sources
    
    subroutine validate_coverage_files(files, is_valid, error_message)
        !! Validates coverage files existence and accessibility
        !! Extracted from original validate_coverage_files function
        character(len=*), intent(in) :: files(:)
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        integer :: i
        logical :: file_exists
        character(len=MEDIUM_STRING_LEN) :: file_path
        
        is_valid = .true.
        error_message = ""
        
        do i = 1, size(files)
            file_path = trim(files(i))
            
            ! Check file existence
            inquire(file=file_path, exist=file_exists)
            if (.not. file_exists) then
                is_valid = .false.
                error_message = "Coverage file not found: " // trim(file_path)
                return
            end if
            
            ! Validate file format
            if (.not. is_valid_coverage_file_format(file_path)) then
                is_valid = .false.
                error_message = "Invalid coverage file format: " // trim(file_path)
                return
            end if
            
            ! Check file readability
            if (.not. is_file_readable(file_path)) then
                is_valid = .false.
                error_message = "Coverage file not readable: " // trim(file_path)
                return
            end if
        end do
        
    end subroutine validate_coverage_files
    
    subroutine validate_output_settings(config, is_valid, error_message)
        !! Validates output format and path settings
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        is_valid = .true.
        error_message = ""
        
        ! Validate output format
        if (.not. is_supported_output_format(config%output_format)) then
            is_valid = .false.
            error_message = "Unsupported output format: " // trim(config%output_format) // &
                          ". Supported: markdown, json, xml, html"
            return
        end if
        
        ! Validate output path if specified
        if (allocated(config%output_path)) then
            call validate_output_path(config%output_path, is_valid, error_message)
            if (.not. is_valid) return
        end if
        
        ! Check for conflicting quiet/verbose flags
        if (config%quiet .and. config%verbose) then
            is_valid = .false.
            error_message = "Cannot specify both --quiet and --verbose flags"
            return
        end if
        
    end subroutine validate_output_settings
    
    subroutine validate_threshold_settings(config, is_valid, error_message)
        !! Validates coverage threshold settings
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        is_valid = .true.
        error_message = ""
        
        ! Validate threshold range
        if (config%minimum_coverage < MIN_VALID_COVERAGE .or. &
            config%minimum_coverage > MAX_VALID_COVERAGE) then
            is_valid = .false.
            write(error_message, '(A, F6.2, A)') &
                "Threshold must be between 0-100, got: ", config%minimum_coverage, "%"
            return
        end if
        
        ! Validate diff threshold if specified
        if (config%enable_diff) then
            if (config%diff_threshold < MIN_VALID_COVERAGE .or. &
                config%diff_threshold > MAX_VALID_COVERAGE) then
                is_valid = .false.
                write(error_message, '(A, F6.2, A)') &
                    "Diff threshold must be between 0-100, got: ", config%diff_threshold, "%"
                return
            end if
        end if
        
    end subroutine validate_threshold_settings
    
    subroutine validate_diff_configuration(config, is_valid, error_message)
        !! Validates diff analysis configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        logical :: baseline_exists, current_exists
        
        is_valid = .true.
        error_message = ""
        
        ! Validate baseline file
        if (.not. allocated(config%diff_baseline_file)) then
            is_valid = .false.
            error_message = "Diff mode requires baseline file (--diff-baseline)"
            return
        end if
        
        inquire(file=config%diff_baseline_file, exist=baseline_exists)
        if (.not. baseline_exists) then
            is_valid = .false.
            error_message = "Baseline file not found: " // trim(config%diff_baseline_file)
            return
        end if
        
        ! Validate current file if specified
        if (allocated(config%diff_current_file)) then
            inquire(file=config%diff_current_file, exist=current_exists)
            if (.not. current_exists) then
                is_valid = .false.
                error_message = "Current file not found: " // trim(config%diff_current_file)
                return
            end if
        end if
        
    end subroutine validate_diff_configuration
    
    subroutine validate_import_configuration(config, is_valid, error_message)
        !! Validates import file configuration
        type(config_t), intent(in) :: config
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        logical :: file_exists
        
        is_valid = .true.
        error_message = ""
        
        ! Check import file existence
        inquire(file=config%import_file, exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Import file not found: " // trim(config%import_file)
            return
        end if
        
        ! Validate import file format
        if (.not. is_valid_import_file_format(config%import_file)) then
            is_valid = .false.
            error_message = "Invalid import file format: " // trim(config%import_file)
            return
        end if
        
    end subroutine validate_import_configuration
    
    ! Helper validation functions
    subroutine validate_source_paths(paths, is_valid, error_message)
        !! Validates source paths
        character(len=*), intent(in) :: paths(:)
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        integer :: i
        logical :: path_exists
        character(len=MEDIUM_STRING_LEN) :: path
        
        is_valid = .true.
        error_message = ""
        
        do i = 1, size(paths)
            path = trim(paths(i))
            
            inquire(file=path, exist=path_exists)
            if (.not. path_exists) then
                is_valid = .false.
                error_message = "Source path not found: " // trim(path)
                return
            end if
        end do
        
    end subroutine validate_source_paths
    
    subroutine validate_output_path(output_path, is_valid, error_message)
        !! Validates output path accessibility
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        character(len=MEDIUM_STRING_LEN) :: directory_path
        integer :: last_slash
        logical :: dir_exists
        
        is_valid = .true.
        error_message = ""
        
        ! Extract directory path
        last_slash = index(output_path, '/', back=.true.)
        if (last_slash > 0) then
            directory_path = output_path(1:last_slash-1)
            
            inquire(file=directory_path, exist=dir_exists)
            if (.not. dir_exists) then
                is_valid = .false.
                error_message = "Output directory does not exist: " // trim(directory_path)
                return
            end if
        end if
        
    end subroutine validate_output_path
    
    subroutine validate_import_file(import_file, is_valid, error_message)
        !! Validates import file
        character(len=*), intent(in) :: import_file
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        logical :: file_exists
        
        inquire(file=import_file, exist=file_exists)
        if (.not. file_exists) then
            is_valid = .false.
            error_message = "Import file not found: " // trim(import_file)
        else
            is_valid = .true.
            error_message = ""
        end if
        
    end subroutine validate_import_file
    
    function is_supported_output_format(format) result(is_supported)
        !! Checks if output format is supported
        character(len=*), intent(in) :: format
        logical :: is_supported
        
        select case (trim(format))
        case ('markdown', 'md', 'json', 'xml', 'html')
            is_supported = .true.
        case default
            is_supported = .false.
        end select
        
    end function is_supported_output_format
    
    function is_valid_coverage_file_format(file_path) result(is_valid)
        !! Checks if coverage file has valid format
        character(len=*), intent(in) :: file_path
        logical :: is_valid
        
        ! Check file extension
        is_valid = (index(file_path, '.gcov') > 0) .or. &
                   (index(file_path, '.json') > 0) .or. &
                   (index(file_path, '.xml') > 0)
        
    end function is_valid_coverage_file_format
    
    function is_valid_import_file_format(file_path) result(is_valid)
        !! Checks if import file has valid format
        character(len=*), intent(in) :: file_path
        logical :: is_valid
        
        ! For now, accept JSON files for import
        is_valid = (index(file_path, '.json') > 0)
        
    end function is_valid_import_file_format
    
    function is_file_readable(file_path) result(is_readable)
        !! Checks if file is readable
        character(len=*), intent(in) :: file_path
        logical :: is_readable
        
        integer :: unit, iostat
        
        open(newunit=unit, file=file_path, status='old', action='read', iostat=iostat)
        is_readable = (iostat == 0)
        if (is_readable) then
            close(unit)
        end if
        
    end function is_file_readable
    
end module config_validator