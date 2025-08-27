module config_parser_namelist_impl
    !! Namelist configuration file parser
    use iso_fortran_env, only: error_unit
    use config_types, only: config_t, MAX_ARRAY_SIZE
    implicit none
    private
    
    public :: parse_namelist_config_file
    public :: transfer_string_array
    public :: validate_array_bounds
    public :: get_max_files_from_env
    
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
    
contains

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
        
        ! Define the namelist
        namelist /fortcov_config/ &
            input_format, output_format, output_path, source_paths, &
            exclude_patterns, include_patterns, gcov_executable, gcov_args, &
            minimum_coverage, fail_under_threshold, threads, &
            verbose, quiet, tui_mode, strict_mode, enable_diff, &
            diff_baseline_file, include_unchanged, diff_threshold, &
            keep_gcov_files, max_files
        
        success = .true.
        error_message = ""
        
        ! Initialize local namelist variables
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
        
        ! Open the config file
        open(newunit=unit, file=config_file, status='old', action='read', iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to open config file: " // trim(iomsg)
            return
        end if
        
        ! Read the namelist
        read(unit, nml=fortcov_config, iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            success = .false.
            call format_namelist_error(iostat, iomsg, error_message)
            close(unit)
            return
        end if
        
        close(unit)
        
        ! Transfer back to namelist_data
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
        
    end subroutine read_namelist_file
    
    subroutine format_namelist_error(iostat, iomsg, error_message)
        !! Format namelist error message
        integer, intent(in) :: iostat
        character(len=*), intent(in) :: iomsg
        character(len=*), intent(out) :: error_message
        
        if (iostat > 0) then
            error_message = "Error reading namelist: " // trim(iomsg)
        else if (iostat < 0) then
            error_message = "End of file reached while reading namelist"
        else
            error_message = "Unknown error reading namelist"
        end if
    end subroutine format_namelist_error
    
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
        
        ! Transfer arrays
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
    
    subroutine validate_array_bounds(input_array, max_size, array_name)
        !! Validate array bounds and warn if near capacity
        character(len=*), dimension(:), intent(in) :: input_array
        integer, intent(in) :: max_size
        character(len=*), intent(in) :: array_name
        
        integer :: count, i
        real :: usage_ratio
        
        ! Count non-empty elements
        count = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
            end if
        end do
        
        usage_ratio = real(count) / real(max_size)
        
        if (usage_ratio >= 0.8) then
            write(error_unit, '(A,A,A,I0,A,I0,A)') &
                "WARNING: Array '", trim(array_name), "' is ", &
                nint(usage_ratio * 100), "% full (", count, " of ", max_size, " entries)"
        end if
        
    end subroutine validate_array_bounds
    
    subroutine get_max_files_from_env(max_files)
        !! Get MAX_FILES value from environment variable
        integer, intent(out) :: max_files
        
        character(len=256) :: env_value
        integer :: status
        
        max_files = 1000  ! Default value
        
        call get_environment_variable("FORTCOV_MAX_FILES", env_value, status=status)
        
        if (status == 0 .and. len_trim(env_value) > 0) then
            read(env_value, *, iostat=status) max_files
            if (status /= 0) then
                max_files = 1000  ! Fall back to default on parse error
            end if
        end if
        
    end subroutine get_max_files_from_env

end module config_parser_namelist_impl