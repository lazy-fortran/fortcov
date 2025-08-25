module config_file_handler
    !! Configuration File Handling Module
    !! 
    !! Focused on reading and processing configuration files.
    !! Extracted from config_parser.f90 to maintain SRP and size limits.
    use iso_fortran_env, only: error_unit
    use foundation_constants
    use foundation_layer_utils
    use fortcov_config, only: config_t
    use config_types, only: MAX_ARRAY_SIZE
    use error_handling
    implicit none
    private
    
    ! Using MAX_ARRAY_SIZE from config_types module
    
    public :: parse_config_file
    public :: load_config_file_with_merge
    public :: merge_config_with_cli_priority
    public :: parse_namelist_config_file
    public :: detect_config_format
    
contains
    
    subroutine parse_config_file(config, success, error_message)
        !! Configuration file parsing implementation with format detection
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        logical :: is_namelist
        logical :: file_exists
        
        success = .true.
        error_message = ""
        
        ! Check if config file is specified and exists
        if (.not. allocated(config%config_file)) then
            return  ! No config file specified, not an error
        end if
        
        inquire(file=config%config_file, exist=file_exists)
        if (.not. file_exists) then
            success = .false.
            error_message = "Config file not found: " // config%config_file
            return
        end if
        
        ! Detect config file format
        call detect_config_format(config%config_file, is_namelist, success, error_message)
        if (.not. success) return
        
        ! Parse based on detected format
        if (is_namelist) then
            call parse_namelist_config_file(config, success, error_message)
        else
            call parse_keyvalue_config_file(config, success, error_message)
        end if
        
    end subroutine parse_config_file
    
    subroutine detect_config_format(filename, is_namelist, success, error_message)
        !! Detect whether config file is namelist or key=value format
        character(len=*), intent(in) :: filename
        logical, intent(out) :: is_namelist
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: unit, iostat
        character(len=1024) :: line
        
        success = .true.
        error_message = ""
        is_namelist = .false.
        
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file for format detection"
            return
        end if
        
        ! Read through file looking for namelist markers
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = adjustl(line)
            
            ! Skip empty lines and comments
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '!' .or. line(1:1) == '#') cycle
            
            ! Check for namelist start marker
            if (line(1:1) == '&') then
                is_namelist = .true.
                exit
            end if
            
            ! If we find a key=value line without namelist marker, it's key=value format
            if (index(line, '=') > 0) then
                is_namelist = .false.
                exit
            end if
        end do
        
        close(unit)
        
    end subroutine detect_config_format
    
    subroutine parse_namelist_config_file(config, success, error_message)
        !! Parse configuration file in Fortran namelist format
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(namelist_config_data_t) :: namelist_data
        integer :: unit
        
        success = .true.
        error_message = ""
        
        ! Initialize namelist data with defaults
        call init_namelist_data(namelist_data)
        
        ! Read namelist from file
        call read_namelist_file(config%config_file, namelist_data, unit, success, error_message)
        if (.not. success) return
        
        ! Transfer data to config structure
        call transfer_data_to_config(namelist_data, config)
        
        ! Validate arrays and warn if near capacity
        call validate_namelist_arrays(namelist_data)
        
    end subroutine parse_namelist_config_file
    
    subroutine init_namelist_data(namelist_data)
        !! Initialize namelist data with default values
        type(namelist_config_data_t), intent(out) :: namelist_data
        
        namelist_data%input_format = ""
        namelist_data%output_format = ""
        namelist_data%output_path = ""
        namelist_data%source_paths = ""
        namelist_data%exclude_patterns = ""
        namelist_data%include_patterns = ""
        namelist_data%gcov_executable = ""
        namelist_data%gcov_args = ""
        namelist_data%minimum_coverage = -1.0
        namelist_data%fail_under_threshold = -1.0
        namelist_data%threads = -1
        namelist_data%verbose = .false.
        namelist_data%quiet = .false.
        namelist_data%tui_mode = .false.
        namelist_data%strict_mode = .false.
        namelist_data%enable_diff = .false.
        namelist_data%diff_baseline_file = ""
        namelist_data%include_unchanged = .false.
        namelist_data%diff_threshold = -1.0
        namelist_data%keep_gcov_files = .false.
        namelist_data%max_files = -1
        
    end subroutine init_namelist_data
    
    subroutine read_namelist_file(config_file, namelist_data, unit, success, error_message)
        !! Read namelist from configuration file
        character(len=*), intent(in) :: config_file
        type(namelist_config_data_t), intent(inout) :: namelist_data
        integer, intent(out) :: unit
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: iostat
        character(len=256) :: iomsg
        
        ! Namelist variables (required for namelist reading)
        character(len=256) :: input_format, output_format, output_path
        character(len=256), dimension(MAX_ARRAY_SIZE) :: source_paths, exclude_patterns, include_patterns
        character(len=256) :: gcov_executable, gcov_args, diff_baseline_file
        real :: minimum_coverage, fail_under_threshold, diff_threshold
        integer :: threads, max_files
        logical :: verbose, quiet, tui_mode, strict_mode, enable_diff, include_unchanged, keep_gcov_files
        
        namelist /fortcov_config/ input_format, output_format, output_path, source_paths, &
                                  exclude_patterns, include_patterns, gcov_executable, gcov_args, &
                                  minimum_coverage, fail_under_threshold, threads, verbose, quiet, &
                                  tui_mode, strict_mode, enable_diff, diff_baseline_file, &
                                  include_unchanged, diff_threshold, keep_gcov_files, max_files
        
        ! Copy from structure to local variables
        input_format = namelist_data%input_format
        output_format = namelist_data%output_format
        output_path = namelist_data%output_path
        source_paths = namelist_data%source_paths
        exclude_patterns = namelist_data%exclude_patterns
        include_patterns = namelist_data%include_patterns
        gcov_executable = namelist_data%gcov_executable
        gcov_args = namelist_data%gcov_args
        minimum_coverage = namelist_data%minimum_coverage
        fail_under_threshold = namelist_data%fail_under_threshold
        threads = namelist_data%threads
        verbose = namelist_data%verbose
        quiet = namelist_data%quiet
        tui_mode = namelist_data%tui_mode
        strict_mode = namelist_data%strict_mode
        enable_diff = namelist_data%enable_diff
        diff_baseline_file = namelist_data%diff_baseline_file
        include_unchanged = namelist_data%include_unchanged
        diff_threshold = namelist_data%diff_threshold
        keep_gcov_files = namelist_data%keep_gcov_files
        max_files = namelist_data%max_files
        
        ! Open and read file
        open(newunit=unit, file=config_file, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file: " // trim(iomsg)
            return
        end if
        
        read(unit, nml=fortcov_config, iostat=iostat, iomsg=iomsg)
        close(unit)
        
        if (iostat /= 0) then
            success = .false.
            call format_namelist_error(iostat, iomsg, error_message)
            return
        end if
        
        ! Copy back to structure
        namelist_data%input_format = input_format
        namelist_data%output_format = output_format
        namelist_data%output_path = output_path
        namelist_data%source_paths = source_paths
        namelist_data%exclude_patterns = exclude_patterns
        namelist_data%include_patterns = include_patterns
        namelist_data%gcov_executable = gcov_executable
        namelist_data%gcov_args = gcov_args
        namelist_data%minimum_coverage = minimum_coverage
        namelist_data%fail_under_threshold = fail_under_threshold
        namelist_data%threads = threads
        namelist_data%verbose = verbose
        namelist_data%quiet = quiet
        namelist_data%tui_mode = tui_mode
        namelist_data%strict_mode = strict_mode
        namelist_data%enable_diff = enable_diff
        namelist_data%diff_baseline_file = diff_baseline_file
        namelist_data%include_unchanged = include_unchanged
        namelist_data%diff_threshold = diff_threshold
        namelist_data%keep_gcov_files = keep_gcov_files
        namelist_data%max_files = max_files
        
        success = .true.
        error_message = ""
        
    end subroutine read_namelist_file
    
    subroutine transfer_data_to_config(namelist_data, config)
        !! Transfer namelist data to config structure
        type(namelist_config_data_t), intent(in) :: namelist_data
        type(config_t), intent(inout) :: config
        
        ! Transfer string values (only if not empty)
        if (len_trim(namelist_data%input_format) > 0) config%input_format = trim(namelist_data%input_format)
        if (len_trim(namelist_data%output_format) > 0) config%output_format = trim(namelist_data%output_format)
        if (len_trim(namelist_data%output_path) > 0) config%output_path = trim(namelist_data%output_path)
        if (len_trim(namelist_data%gcov_executable) > 0) config%gcov_executable = trim(namelist_data%gcov_executable)
        if (len_trim(namelist_data%gcov_args) > 0) config%gcov_args = trim(namelist_data%gcov_args)
        if (len_trim(namelist_data%diff_baseline_file) > 0) config%diff_baseline_file = trim(namelist_data%diff_baseline_file)
        
        ! Transfer numeric values (only if set)
        if (namelist_data%minimum_coverage >= 0.0) config%minimum_coverage = namelist_data%minimum_coverage
        if (namelist_data%fail_under_threshold >= 0.0) config%fail_under_threshold = namelist_data%fail_under_threshold
        if (namelist_data%threads > 0) config%threads = namelist_data%threads
        if (namelist_data%max_files > 0) config%max_files = namelist_data%max_files
        if (namelist_data%diff_threshold >= 0.0) config%diff_threshold = namelist_data%diff_threshold
        
        ! Transfer logical values
        config%verbose = namelist_data%verbose
        config%quiet = namelist_data%quiet
        config%tui_mode = namelist_data%tui_mode
        config%strict_mode = namelist_data%strict_mode
        config%enable_diff = namelist_data%enable_diff
        config%include_unchanged = namelist_data%include_unchanged
        config%keep_gcov_files = namelist_data%keep_gcov_files
        
        ! Transfer arrays with validation
        call transfer_string_array(namelist_data%source_paths, config%source_paths)
        call transfer_string_array(namelist_data%exclude_patterns, config%exclude_patterns)
        call transfer_string_array(namelist_data%include_patterns, config%include_patterns)
        
    end subroutine transfer_data_to_config
    
    subroutine validate_namelist_arrays(namelist_data)
        !! Validate namelist arrays and warn if near capacity
        type(namelist_config_data_t), intent(in) :: namelist_data
        
        call validate_array_bounds(namelist_data%source_paths, MAX_ARRAY_SIZE, "source_paths")
        call validate_array_bounds(namelist_data%exclude_patterns, MAX_ARRAY_SIZE, "exclude_patterns")
        call validate_array_bounds(namelist_data%include_patterns, MAX_ARRAY_SIZE, "include_patterns")
        
    end subroutine validate_namelist_arrays
    
    ! Type to hold all namelist variables
    type :: namelist_config_data_t
        character(len=256) :: input_format
        character(len=256) :: output_format
        character(len=256) :: output_path
        character(len=256), dimension(MAX_ARRAY_SIZE) :: source_paths
        character(len=256), dimension(MAX_ARRAY_SIZE) :: exclude_patterns
        character(len=256), dimension(MAX_ARRAY_SIZE) :: include_patterns
        character(len=256) :: gcov_executable
        character(len=256) :: gcov_args
        real :: minimum_coverage
        real :: fail_under_threshold
        integer :: threads
        logical :: verbose
        logical :: quiet
        logical :: tui_mode
        logical :: strict_mode
        logical :: enable_diff
        character(len=256) :: diff_baseline_file
        logical :: include_unchanged
        real :: diff_threshold
        logical :: keep_gcov_files
        integer :: max_files
    end type namelist_config_data_t
    
    ! Initialize namelist variables with default values
    subroutine initialize_namelist_variables(namelist_data)
        type(namelist_config_data_t), intent(out) :: namelist_data
        
        namelist_data%input_format = ""
        namelist_data%output_format = ""
        namelist_data%output_path = ""
        namelist_data%source_paths = ""
        namelist_data%exclude_patterns = ""
        namelist_data%include_patterns = ""
        namelist_data%gcov_executable = ""
        namelist_data%gcov_args = ""
        namelist_data%minimum_coverage = -1.0
        namelist_data%fail_under_threshold = -1.0
        namelist_data%threads = -1
        namelist_data%verbose = .false.
        namelist_data%quiet = .false.
        namelist_data%tui_mode = .false.
        namelist_data%strict_mode = .false.
        namelist_data%enable_diff = .false.
        namelist_data%diff_baseline_file = ""
        namelist_data%include_unchanged = .false.
        namelist_data%diff_threshold = -1.0
        namelist_data%keep_gcov_files = .false.
        namelist_data%max_files = -1
    end subroutine initialize_namelist_variables
    
    ! Read namelist from file
    subroutine read_namelist_from_file(config_file, namelist_data, success, error_message)
        character(len=*), intent(in) :: config_file
        type(namelist_config_data_t), intent(inout) :: namelist_data
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: unit, iostat
        character(len=256) :: iomsg
        
        ! Define namelist - need to use individual variables due to Fortran limitations
        character(len=256) :: input_format
        character(len=256) :: output_format
        character(len=256) :: output_path
        character(len=256), dimension(MAX_ARRAY_SIZE) :: source_paths
        character(len=256), dimension(MAX_ARRAY_SIZE) :: exclude_patterns
        character(len=256), dimension(MAX_ARRAY_SIZE) :: include_patterns
        character(len=256) :: gcov_executable
        character(len=256) :: gcov_args
        real :: minimum_coverage
        real :: fail_under_threshold
        integer :: threads
        logical :: verbose
        logical :: quiet
        logical :: tui_mode
        logical :: strict_mode
        logical :: enable_diff
        character(len=256) :: diff_baseline_file
        logical :: include_unchanged
        real :: diff_threshold
        logical :: keep_gcov_files
        integer :: max_files
        
        namelist /fortcov_config/ input_format, output_format, &
                                  output_path, source_paths, exclude_patterns, &
                                  include_patterns, gcov_executable, gcov_args, &
                                  minimum_coverage, fail_under_threshold, threads, &
                                  verbose, quiet, tui_mode, strict_mode, enable_diff, &
                                  diff_baseline_file, include_unchanged, diff_threshold, &
                                  keep_gcov_files, max_files
        
        ! Copy from structure to local variables
        input_format = namelist_data%input_format
        output_format = namelist_data%output_format
        output_path = namelist_data%output_path
        source_paths = namelist_data%source_paths
        exclude_patterns = namelist_data%exclude_patterns
        include_patterns = namelist_data%include_patterns
        gcov_executable = namelist_data%gcov_executable
        gcov_args = namelist_data%gcov_args
        minimum_coverage = namelist_data%minimum_coverage
        fail_under_threshold = namelist_data%fail_under_threshold
        threads = namelist_data%threads
        verbose = namelist_data%verbose
        quiet = namelist_data%quiet
        tui_mode = namelist_data%tui_mode
        strict_mode = namelist_data%strict_mode
        enable_diff = namelist_data%enable_diff
        diff_baseline_file = namelist_data%diff_baseline_file
        include_unchanged = namelist_data%include_unchanged
        diff_threshold = namelist_data%diff_threshold
        keep_gcov_files = namelist_data%keep_gcov_files
        max_files = namelist_data%max_files
        
        call open_namelist_file(config_file, unit, success, error_message)
        if (.not. success) return
        
        call read_namelist_data_from_unit(unit, success, error_message)
        close(unit)
        
        ! Copy back to structure
        namelist_data%input_format = input_format
        namelist_data%output_format = output_format
        namelist_data%output_path = output_path
        namelist_data%source_paths = source_paths
        namelist_data%exclude_patterns = exclude_patterns
        namelist_data%include_patterns = include_patterns
        namelist_data%gcov_executable = gcov_executable
        namelist_data%gcov_args = gcov_args
        namelist_data%minimum_coverage = minimum_coverage
        namelist_data%fail_under_threshold = fail_under_threshold
        namelist_data%threads = threads
        namelist_data%verbose = verbose
        namelist_data%quiet = quiet
        namelist_data%tui_mode = tui_mode
        namelist_data%strict_mode = strict_mode
        namelist_data%enable_diff = enable_diff
        namelist_data%diff_baseline_file = diff_baseline_file
        namelist_data%include_unchanged = include_unchanged
        namelist_data%diff_threshold = diff_threshold
        namelist_data%keep_gcov_files = keep_gcov_files
        namelist_data%max_files = max_files
        
        contains
        
        subroutine read_namelist_data_from_unit(unit, success, error_message)
            integer, intent(in) :: unit
            logical, intent(out) :: success
            character(len=*), intent(out) :: error_message
            
            integer :: iostat
            character(len=256) :: iomsg
            
            read(unit, nml=fortcov_config, iostat=iostat, iomsg=iomsg)
            
            if (iostat /= 0) then
                success = .false.
                call format_namelist_error(iostat, iomsg, error_message)
            else
                success = .true.
                error_message = ""
            end if
        end subroutine read_namelist_data_from_unit
        
    end subroutine read_namelist_from_file
    
    ! Open namelist file for reading
    subroutine open_namelist_file(config_file, unit, success, error_message)
        character(len=*), intent(in) :: config_file
        integer, intent(out) :: unit
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: iostat
        character(len=256) :: iomsg
        
        open(newunit=unit, file=config_file, status='old', &
             action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file: " // trim(iomsg)
        else
            success = .true.
            error_message = ""
        end if
    end subroutine open_namelist_file
    
    ! Format namelist reading errors
    subroutine format_namelist_error(iostat, iomsg, error_message)
        integer, intent(in) :: iostat
        character(len=*), intent(in) :: iomsg
        character(len=*), intent(out) :: error_message
        
        if (iostat == 5010) then
            error_message = "Invalid namelist format (end-of-record error). " // &
                           "Check array continuations and formatting."
        else
            write(error_message, '(A,I0,A,A)') &
                "Failed to parse namelist (iostat=", iostat, "): ", trim(iomsg)
        end if
    end subroutine format_namelist_error
    
    ! Transfer all namelist data to config structure
    subroutine transfer_namelist_to_config(namelist_data, config)
        type(namelist_config_data_t), intent(in) :: namelist_data
        type(config_t), intent(inout) :: config
        
        call transfer_string_values(namelist_data, config)
        call transfer_numeric_values(namelist_data, config)
        call transfer_logical_values(namelist_data, config)
        call transfer_array_values(namelist_data, config)
    end subroutine transfer_namelist_to_config
    
    ! Transfer string values from namelist to config
    subroutine transfer_string_values(namelist_data, config)
        type(namelist_config_data_t), intent(in) :: namelist_data
        type(config_t), intent(inout) :: config
        
        if (len_trim(namelist_data%input_format) > 0) &
            config%input_format = trim(namelist_data%input_format)
        if (len_trim(namelist_data%output_format) > 0) &
            config%output_format = trim(namelist_data%output_format)
        if (len_trim(namelist_data%output_path) > 0) &
            config%output_path = trim(namelist_data%output_path)
        if (len_trim(namelist_data%gcov_executable) > 0) &
            config%gcov_executable = trim(namelist_data%gcov_executable)
        if (len_trim(namelist_data%gcov_args) > 0) &
            config%gcov_args = trim(namelist_data%gcov_args)
        if (len_trim(namelist_data%diff_baseline_file) > 0) &
            config%diff_baseline_file = trim(namelist_data%diff_baseline_file)
    end subroutine transfer_string_values
    
    ! Transfer numeric values from namelist to config
    subroutine transfer_numeric_values(namelist_data, config)
        type(namelist_config_data_t), intent(in) :: namelist_data
        type(config_t), intent(inout) :: config
        
        if (namelist_data%minimum_coverage >= 0.0) &
            config%minimum_coverage = namelist_data%minimum_coverage
        if (namelist_data%fail_under_threshold >= 0.0) &
            config%fail_under_threshold = namelist_data%fail_under_threshold
        if (namelist_data%threads > 0) config%threads = namelist_data%threads
        if (namelist_data%max_files > 0) config%max_files = namelist_data%max_files
        if (namelist_data%diff_threshold >= 0.0) &
            config%diff_threshold = namelist_data%diff_threshold
    end subroutine transfer_numeric_values
    
    ! Transfer logical values from namelist to config
    subroutine transfer_logical_values(namelist_data, config)
        type(namelist_config_data_t), intent(in) :: namelist_data
        type(config_t), intent(inout) :: config
        
        config%verbose = namelist_data%verbose
        config%quiet = namelist_data%quiet
        config%tui_mode = namelist_data%tui_mode
        config%strict_mode = namelist_data%strict_mode
        config%enable_diff = namelist_data%enable_diff
        config%include_unchanged = namelist_data%include_unchanged
        config%keep_gcov_files = namelist_data%keep_gcov_files
    end subroutine transfer_logical_values
    
    ! Transfer array values from namelist to config
    subroutine transfer_array_values(namelist_data, config)
        type(namelist_config_data_t), intent(in) :: namelist_data
        type(config_t), intent(inout) :: config
        
        call transfer_string_array(namelist_data%source_paths, config%source_paths)
        call transfer_string_array(namelist_data%exclude_patterns, config%exclude_patterns)
        call transfer_string_array(namelist_data%include_patterns, config%include_patterns)
        
        ! Validate array bounds
        call validate_array_bounds(namelist_data%source_paths, MAX_ARRAY_SIZE, "source_paths")
        call validate_array_bounds(namelist_data%exclude_patterns, MAX_ARRAY_SIZE, "exclude_patterns")
        call validate_array_bounds(namelist_data%include_patterns, MAX_ARRAY_SIZE, "include_patterns")
    end subroutine transfer_array_values
    
    subroutine parse_keyvalue_config_file(config, success, error_message)
        !! Parse configuration file in simple key=value format
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=1024) :: line
        character(len=256) :: key, value
        integer :: iostat, equal_pos
        integer :: unit
        
        success = .true.
        error_message = ""
        
        ! Open and read config file
        open(newunit=unit, file=config%config_file, status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file: " // config%config_file
            return
        end if
        
        ! Parse file line by line
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            line = adjustl(line)
            if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
            
            equal_pos = index(line, '=')
            if (equal_pos == 0) cycle
            
            key = adjustl(line(1:equal_pos-1))
            value = adjustl(line(equal_pos+1:))
            
            call process_config_file_option(key, value, config, success, error_message)
            if (.not. success) then
                close(unit)
                return
            end if
        end do
        
        close(unit)
        
    end subroutine parse_keyvalue_config_file
    
    subroutine transfer_string_array(input_array, output_array)
        !! Transfer non-empty strings from fixed array to allocatable array
        character(len=*), dimension(:), intent(in) :: input_array
        character(len=:), allocatable, dimension(:), intent(out) :: output_array
        
        integer :: i, count, max_len
        
        ! Count non-empty elements and find max length
        count = 0
        max_len = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
                max_len = max(max_len, len_trim(input_array(i)))
            end if
        end do
        
        if (count == 0) return
        
        ! Allocate output array
        allocate(character(len=max_len) :: output_array(count))
        
        ! Copy non-empty strings
        count = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
                output_array(count) = trim(input_array(i))
            end if
        end do
        
    end subroutine transfer_string_array
    
    subroutine load_config_file_with_merge(config, success, error_message)
        !! Load config file and merge with existing config (CLI takes precedence)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(config_t) :: file_config
        
        ! Initialize file config with defaults
        call initialize_file_config_defaults(file_config)
        
        ! Set the config file path
        file_config%config_file = config%config_file
        
        ! Load settings from file
        call parse_config_file(file_config, success, error_message)
        if (.not. success) return
        
        ! Merge settings - CLI overrides file settings
        call merge_config_with_cli_priority(config, file_config)
        
    end subroutine load_config_file_with_merge
    
    subroutine merge_config_with_cli_priority(cli_config, file_config)
        !! Merge file config into CLI config, with CLI taking precedence
        type(config_t), intent(inout) :: cli_config
        type(config_t), intent(in) :: file_config
        
        ! Only apply file settings if CLI didn't set them
        if (.not. allocated(cli_config%output_path) .and. &
            allocated(file_config%output_path)) then
            cli_config%output_path = file_config%output_path
        end if
        
        if (.not. allocated(cli_config%output_format) .and. &
            allocated(file_config%output_format)) then
            cli_config%output_format = file_config%output_format
        end if
        
        ! For numeric values, check if they're still at default
        if (cli_config%minimum_coverage == 0.0 .and. &
            file_config%minimum_coverage > 0.0) then
            cli_config%minimum_coverage = file_config%minimum_coverage
        end if
        
        ! For boolean flags, only apply if not already set by CLI
        ! Note: This is tricky since we can't distinguish "not set" from "false"
        ! For now, we'll assume CLI always takes precedence for booleans
        
    end subroutine merge_config_with_cli_priority
    
    
    
    subroutine parse_config_real_value(str, value, success)
        !! Parses string to real value for config file
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
    end subroutine parse_config_real_value
    
    subroutine parse_config_integer_value(str, value, success)
        !! Parses string to integer value for config file
        character(len=*), intent(in) :: str
        integer, intent(out) :: value
        logical, intent(out) :: success
        
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
        
        ! Validate that value is positive
        if (success .and. value <= 0) then
            success = .false.
        end if
        
    end subroutine parse_config_integer_value
    
    subroutine initialize_file_config_defaults(config)
        !! Initializes configuration with default values for file loading
        type(config_t), intent(out) :: config
        
        config%input_format = "gcov"
        config%output_format = "markdown"
        config%minimum_coverage = 0.0
        config%fail_under_threshold = 0.0
        config%threads = 1
        config%verbose = .false.
        config%quiet = .false.
        config%show_help = .false.
        config%show_version = .false.
        config%validate_config_only = .false.
        config%enable_diff = .false.
        config%include_unchanged = .false.
        config%diff_threshold = 0.0
        config%keep_gcov_files = .false.
        config%tui_mode = .false.
        config%strict_mode = .false.
        config%zero_configuration_mode = .false.
        config%max_files = 10000  ! Default maximum files
        
        ! Check for FORTCOV_MAX_FILES environment variable
        call get_max_files_from_env(config%max_files)
        
    end subroutine initialize_file_config_defaults
    
    subroutine get_max_files_from_env(max_files)
        !! Check for FORTCOV_MAX_FILES environment variable and update max_files
        integer, intent(inout) :: max_files
        
        character(len=20) :: env_value
        integer :: env_max_files, iostat_var
        
        ! Try to get environment variable
        call get_environment_variable('FORTCOV_MAX_FILES', env_value)
        
        if (len_trim(env_value) > 0) then
            ! Parse the environment variable value
            read(env_value, *, iostat=iostat_var) env_max_files
            if (iostat_var == 0 .and. env_max_files > 0) then
                max_files = env_max_files
            end if
        end if
    end subroutine get_max_files_from_env
    
    subroutine validate_array_bounds(input_array, max_size, array_name)
        !! Validate array bounds and warn if array appears to be at capacity
        character(len=*), dimension(:), intent(in) :: input_array
        integer, intent(in) :: max_size
        character(len=*), intent(in) :: array_name
        
        integer :: i, count
        
        ! Count non-empty elements
        count = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
            end if
        end do
        
        ! Warn if array is at or near capacity
        if (count >= max_size - 5) then
            write(error_unit, '(A)') "WARNING: " // trim(array_name) // &
                " array is nearly full. Consider increasing MAX_ARRAY_SIZE parameter."
        end if
        
    end subroutine validate_array_bounds
    
end module config_file_handler