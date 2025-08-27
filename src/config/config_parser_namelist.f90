module config_parser_namelist
    !! Namelist configuration file parsing
    !!
    !! This module handles parsing of namelist-format configuration files
    !! for fortcov configuration management.
    
    use config_types, only: config_t, MAX_ARRAY_SIZE
    use constants_core
    implicit none
    private

    ! Namelist data structure to match the format
    type :: namelist_config_data_t
        ! General settings
        character(len=256) :: output_path = ""
        character(len=64) :: output_format = "html"
        real :: minimum_coverage = 80.0
        real :: fail_under_threshold = 0.0
        logical :: verbose = .false.
        logical :: quiet = .false.
        
        ! Auto-discovery settings
        logical :: auto_discovery = .false.
        logical :: auto_test_execution = .false.
        integer :: test_timeout_seconds = 300
        
        ! Source paths and patterns
        character(len=256) :: source_paths(MAX_ARRAY_SIZE) = ""
        character(len=256) :: exclude_patterns(MAX_ARRAY_SIZE) = ""
        character(len=256) :: include_patterns(MAX_ARRAY_SIZE) = ""
        
        ! Coverage files
        character(len=256) :: coverage_files(MAX_ARRAY_SIZE) = ""
        character(len=256) :: import_file = ""
        
        ! Additional settings
        logical :: include_unchanged = .false.
        logical :: keep_gcov_files = .false.
        logical :: tui_mode = .false.
        logical :: strict_mode = .false.
        integer :: threads = 1
        
        ! Diff mode
        logical :: enable_diff = .false.
        character(len=256) :: diff_baseline_file = ""
        character(len=256) :: diff_current_file = ""
        real :: diff_threshold = 0.0
        
        ! gcov settings
        character(len=256) :: gcov_executable = "gcov"
        character(len=256) :: gcov_args = ""
    end type namelist_config_data_t

    public :: parse_namelist_config_file
    public :: detect_namelist_format
    public :: initialize_namelist_variables

contains

    subroutine parse_namelist_config_file(config, success, error_message)
        !! Parse a namelist-format configuration file
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        type(namelist_config_data_t) :: namelist_data
        
        success = .false.
        error_message = ""
        
        ! Initialize namelist data
        call init_namelist_data(namelist_data)
        
        ! Read the namelist file
        call read_namelist_file(config%config_file, namelist_data, 0, success, error_message)
        if (.not. success) return
        
        ! Transfer to config
        call transfer_data_to_config(namelist_data, config)
        
        success = .true.
    end subroutine parse_namelist_config_file

    function detect_namelist_format(filename) result(is_namelist)
        !! Detect if file is in namelist format
        character(len=*), intent(in) :: filename
        logical :: is_namelist
        
        integer :: unit, iostat
        character(len=256) :: line
        logical :: file_exists
        
        is_namelist = .false.
        
        ! Check file exists
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) return
        
        ! Open and check first non-comment line
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            
            ! Skip empty lines and comments
            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#' .or. line(1:1) == '!') cycle
            
            ! Check for namelist start
            if (index(line, '&fortcov_config') > 0 .or. &
                index(line, '&FORTCOV_CONFIG') > 0) then
                is_namelist = .true.
            end if
            exit
        end do
        
        close(unit)
    end function detect_namelist_format

    subroutine init_namelist_data(namelist_data)
        !! Initialize namelist data with defaults
        type(namelist_config_data_t), intent(out) :: namelist_data
        
        namelist_data%output_path = ""
        namelist_data%output_format = "html"
        namelist_data%minimum_coverage = 80.0
        namelist_data%fail_under_threshold = 0.0
        namelist_data%verbose = .false.
        namelist_data%quiet = .false.
        namelist_data%auto_discovery = .false.
        namelist_data%auto_test_execution = .false.
        namelist_data%test_timeout_seconds = 300
        namelist_data%source_paths = ""
        namelist_data%exclude_patterns = ""
        namelist_data%include_patterns = ""
        namelist_data%coverage_files = ""
        namelist_data%import_file = ""
        namelist_data%include_unchanged = .false.
        namelist_data%keep_gcov_files = .false.
        namelist_data%tui_mode = .false.
        namelist_data%strict_mode = .false.
        namelist_data%threads = 1
        namelist_data%enable_diff = .false.
        namelist_data%diff_baseline_file = ""
        namelist_data%diff_current_file = ""
        namelist_data%diff_threshold = 0.0
        namelist_data%gcov_executable = "gcov"
        namelist_data%gcov_args = ""
    end subroutine init_namelist_data

    subroutine read_namelist_file(config_file, namelist_data, unit_in, success, error_message)
        !! Read namelist from file
        character(len=*), intent(in) :: config_file
        type(namelist_config_data_t), intent(inout) :: namelist_data
        integer, intent(in) :: unit_in
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: unit, iostat
        character(len=1024) :: iomsg
        namelist /fortcov_config/ &
            namelist_data%output_path, &
            namelist_data%output_format, &
            namelist_data%minimum_coverage, &
            namelist_data%fail_under_threshold, &
            namelist_data%verbose, &
            namelist_data%quiet, &
            namelist_data%auto_discovery, &
            namelist_data%auto_test_execution, &
            namelist_data%test_timeout_seconds, &
            namelist_data%source_paths, &
            namelist_data%exclude_patterns, &
            namelist_data%include_patterns, &
            namelist_data%coverage_files, &
            namelist_data%import_file, &
            namelist_data%include_unchanged, &
            namelist_data%keep_gcov_files, &
            namelist_data%tui_mode, &
            namelist_data%strict_mode, &
            namelist_data%threads, &
            namelist_data%enable_diff, &
            namelist_data%diff_baseline_file, &
            namelist_data%diff_current_file, &
            namelist_data%diff_threshold, &
            namelist_data%gcov_executable, &
            namelist_data%gcov_args
        
        success = .false.
        error_message = ""
        
        if (unit_in /= 0) then
            unit = unit_in
        else
            open(newunit=unit, file=config_file, status='old', action='read', iostat=iostat)
            if (iostat /= 0) then
                error_message = "Cannot open config file: " // trim(config_file)
                return
            end if
        end if
        
        read(unit, nml=fortcov_config, iostat=iostat, iomsg=iomsg)
        
        if (unit_in == 0) close(unit)
        
        if (iostat /= 0) then
            call format_namelist_error(iostat, iomsg, error_message)
            return
        end if
        
        ! Validate arrays
        call validate_namelist_arrays(namelist_data)
        
        success = .true.
    end subroutine read_namelist_file

    subroutine transfer_data_to_config(namelist_data, config)
        !! Transfer namelist data to config structure
        type(namelist_config_data_t), intent(in) :: namelist_data
        type(config_t), intent(inout) :: config
        
        ! Transfer string values
        if (len_trim(namelist_data%output_path) > 0) &
            config%output_path = trim(namelist_data%output_path)
        if (len_trim(namelist_data%output_format) > 0) &
            config%output_format = trim(namelist_data%output_format)
        if (len_trim(namelist_data%import_file) > 0) &
            config%import_file = trim(namelist_data%import_file)
        if (len_trim(namelist_data%gcov_executable) > 0) &
            config%gcov_executable = trim(namelist_data%gcov_executable)
        if (len_trim(namelist_data%gcov_args) > 0) &
            config%gcov_args = trim(namelist_data%gcov_args)
        
        ! Transfer numeric values
        config%minimum_coverage = namelist_data%minimum_coverage
        config%fail_under_threshold = namelist_data%fail_under_threshold
        config%test_timeout_seconds = namelist_data%test_timeout_seconds
        config%threads = namelist_data%threads
        config%diff_threshold = namelist_data%diff_threshold
        
        ! Transfer logical values
        config%verbose = namelist_data%verbose
        config%quiet = namelist_data%quiet
        config%auto_discovery = namelist_data%auto_discovery
        config%auto_test_execution = namelist_data%auto_test_execution
        config%include_unchanged = namelist_data%include_unchanged
        config%keep_gcov_files = namelist_data%keep_gcov_files
        config%tui_mode = namelist_data%tui_mode
        config%strict_mode = namelist_data%strict_mode
        config%enable_diff = namelist_data%enable_diff
        
        ! Transfer arrays
        call transfer_string_array(namelist_data%source_paths, config%source_paths)
        call transfer_string_array(namelist_data%exclude_patterns, config%exclude_patterns)
        call transfer_string_array(namelist_data%include_patterns, config%include_patterns)
        call transfer_string_array(namelist_data%coverage_files, config%coverage_files)
    end subroutine transfer_data_to_config

    subroutine validate_namelist_arrays(namelist_data)
        !! Validate namelist arrays are properly sized
        type(namelist_config_data_t), intent(inout) :: namelist_data
        
        integer :: i
        
        ! Clean up empty array entries
        do i = 1, MAX_ARRAY_SIZE
            if (len_trim(namelist_data%source_paths(i)) == 0) then
                namelist_data%source_paths(i:) = ""
                exit
            end if
        end do
        
        do i = 1, MAX_ARRAY_SIZE
            if (len_trim(namelist_data%exclude_patterns(i)) == 0) then
                namelist_data%exclude_patterns(i:) = ""
                exit
            end if
        end do
        
        do i = 1, MAX_ARRAY_SIZE
            if (len_trim(namelist_data%include_patterns(i)) == 0) then
                namelist_data%include_patterns(i:) = ""
                exit
            end if
        end do
        
        do i = 1, MAX_ARRAY_SIZE
            if (len_trim(namelist_data%coverage_files(i)) == 0) then
                namelist_data%coverage_files(i:) = ""
                exit
            end if
        end do
    end subroutine validate_namelist_arrays

    subroutine initialize_namelist_variables(namelist_data)
        !! Initialize namelist variables to defaults
        type(namelist_config_data_t), intent(out) :: namelist_data
        
        call init_namelist_data(namelist_data)
    end subroutine initialize_namelist_variables

    subroutine format_namelist_error(iostat, iomsg, error_message)
        !! Format namelist error message
        integer, intent(in) :: iostat
        character(len=*), intent(in) :: iomsg
        character(len=*), intent(out) :: error_message
        
        if (iostat > 0) then
            error_message = "Error reading namelist config: " // trim(iomsg)
        else if (iostat < 0) then
            error_message = "Namelist config incomplete or malformed"
        else
            error_message = "Unknown error reading namelist config"
        end if
    end subroutine format_namelist_error

    subroutine transfer_string_array(input_array, output_array)
        !! Transfer string array from namelist to config
        character(len=*), intent(in) :: input_array(:)
        character(len=:), allocatable, intent(out) :: output_array(:)
        
        integer :: i, count
        
        ! Count non-empty entries
        count = 0
        do i = 1, size(input_array)
            if (len_trim(input_array(i)) > 0) then
                count = count + 1
            else
                exit
            end if
        end do
        
        ! Allocate and copy
        if (count > 0) then
            if (allocated(output_array)) deallocate(output_array)
            allocate(character(len=256) :: output_array(count))
            do i = 1, count
                output_array(i) = trim(input_array(i))
            end do
        end if
    end subroutine transfer_string_array

end module config_parser_namelist