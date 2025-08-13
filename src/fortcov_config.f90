module fortcov_config
    use string_utils
    use file_utils
    implicit none
    private
    
    ! Constants
    integer, parameter :: MAX_PATH_LENGTH = 256
    integer, parameter :: MAX_ARRAY_SIZE = 100
    real, parameter :: MIN_COVERAGE = 0.0
    real, parameter :: MAX_COVERAGE = 100.0
    
    ! Public types
    public :: config_t
    
    ! Public procedures
    public :: parse_config
    public :: show_help
    public :: show_version
    public :: initialize_config
    
    ! Configuration type
    type :: config_t
        character(len=:), allocatable :: input_format
        character(len=:), allocatable :: output_format
        character(len=:), allocatable :: output_path
        character(len=:), allocatable :: source_paths(:)
        character(len=:), allocatable :: exclude_patterns(:)
        real :: minimum_coverage
        logical :: verbose
        logical :: quiet
        logical :: show_help
        logical :: show_version
        character(len=:), allocatable :: config_file
    end type config_t

contains

    subroutine parse_config(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: i, argc
        character(len=:), allocatable :: arg
        character(len=:), allocatable :: key, value
        integer :: eq_pos
        character(len=:), allocatable :: temp_sources(:)
        character(len=:), allocatable :: temp_excludes(:)
        integer :: num_sources, num_excludes
        
        ! Initialize config with defaults
        call initialize_config(config)
        
        success = .true.
        error_message = ""
        
        ! Temporary arrays for dynamic sizing
        allocate(character(len=MAX_PATH_LENGTH) :: temp_sources(MAX_ARRAY_SIZE))
        allocate(character(len=MAX_PATH_LENGTH) :: temp_excludes(MAX_ARRAY_SIZE))
        num_sources = 0
        num_excludes = 0
        
        argc = size(args)
        
        do i = 1, argc
            arg = trim(args(i))
            
            if (len_trim(arg) == 0) cycle
            
            ! Check for help flag
            if (arg == "--help" .or. arg == "-h") then
                config%show_help = .true.
                success = .false.
                return
            end if
            
            ! Check for version flag
            if (arg == "--version" .or. arg == "-V") then
                config%show_version = .true.
                success = .false.
                return
            end if
            
            ! Check for verbose flag
            if (arg == "--verbose" .or. arg == "-v") then
                config%verbose = .true.
                cycle
            end if
            
            ! Check for quiet flag
            if (arg == "--quiet" .or. arg == "-q") then
                config%quiet = .true.
                cycle
            end if
            
            ! Parse key=value arguments
            eq_pos = index(arg, "=")
            if (eq_pos > 0) then
                key = arg(1:eq_pos-1)
                value = arg(eq_pos+1:)
                
                select case (trim(key))
                case ("--input-format")
                    config%input_format = trim(value)
                    
                case ("--output-format")
                    config%output_format = trim(value)
                    
                case ("--output")
                    config%output_path = trim(value)
                    
                case ("--source")
                    call add_to_array(trim(value), temp_sources, num_sources, &
                                     MAX_ARRAY_SIZE, "source paths")
                    
                case ("--exclude")
                    call add_to_array(trim(value), temp_excludes, num_excludes, &
                                     MAX_ARRAY_SIZE, "exclude patterns")
                    
                case ("--fail-under")
                    call parse_threshold(trim(value), config%minimum_coverage, &
                                        success, error_message)
                    if (.not. success) return
                    
                case ("--config")
                    config%config_file = trim(value)
                    call load_config_file(config, success, error_message)
                    if (.not. success) return
                    
                case default
                    success = .false.
                    error_message = "Unknown option: " // trim(key)
                    return
                end select
            else
                success = .false.
                error_message = "Unknown argument: " // trim(arg)
                return
            end if
        end do
        
        ! Finalize arrays
        call finalize_array(temp_sources, num_sources, config%source_paths)
        call finalize_array(temp_excludes, num_excludes, &
                           config%exclude_patterns)
    end subroutine parse_config

    subroutine parse_real(str, value, success)
        character(len=*), intent(in) :: str
        real, intent(out) :: value
        logical, intent(out) :: success
        integer :: iostat
        
        read(str, *, iostat=iostat) value
        success = (iostat == 0)
    end subroutine parse_real

    subroutine load_config_file(config, success, error_message)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        ! Namelist variables
        character(len=256) :: input_format
        character(len=256) :: output_format
        character(len=256) :: output_path
        character(len=256), dimension(MAX_ARRAY_SIZE) :: source_paths
        character(len=256), dimension(MAX_ARRAY_SIZE) :: exclude_patterns
        real :: minimum_coverage
        logical :: verbose
        logical :: quiet
        integer :: unit, iostat, i, count
        logical :: file_exists
        
        namelist /fortcov_config/ input_format, output_format, output_path, &
                                  source_paths, exclude_patterns, &
                                  minimum_coverage, verbose, quiet
        
        success = .false.
        error_message = ""
        
        ! Check if file exists
        inquire(file=config%config_file, exist=file_exists)
        if (.not. file_exists) then
            error_message = "Configuration file not found: " // config%config_file
            return
        end if
        
        ! Initialize namelist variables with defaults
        input_format = config%input_format
        output_format = config%output_format
        output_path = config%output_path
        source_paths = ''
        exclude_patterns = ''
        minimum_coverage = config%minimum_coverage
        verbose = config%verbose
        quiet = config%quiet
        
        ! Read namelist from file
        open(newunit=unit, file=config%config_file, status='old', &
             action='read', iostat=iostat)
        if (iostat /= 0) then
            error_message = "Failed to open config file: " // config%config_file
            return
        end if
        
        read(unit, nml=fortcov_config, iostat=iostat)
        close(unit)
        
        if (iostat /= 0) then
            error_message = "Invalid namelist format in config file"
            return
        end if
        
        ! Update config from namelist variables
        config%input_format = trim(adjustl(input_format))
        config%output_format = trim(adjustl(output_format))
        config%output_path = trim(adjustl(output_path))
        
        ! Process source paths array
        if (allocated(config%source_paths)) deallocate(config%source_paths)
        count = 0
        do i = 1, MAX_ARRAY_SIZE
            if (len_trim(source_paths(i)) > 0) then
                count = count + 1
            else
                exit
            end if
        end do
        if (count > 0) then
            allocate(character(len=MAX_PATH_LENGTH) :: config%source_paths(count))
            do i = 1, count
                config%source_paths(i) = trim(adjustl(source_paths(i)))
            end do
        end if
        
        ! Process exclude patterns array
        if (allocated(config%exclude_patterns)) deallocate(config%exclude_patterns)
        count = 0
        do i = 1, MAX_ARRAY_SIZE
            if (len_trim(exclude_patterns(i)) > 0) then
                count = count + 1
            else
                exit
            end if
        end do
        if (count > 0) then
            allocate(character(len=MAX_PATH_LENGTH) :: config%exclude_patterns(count))
            do i = 1, count
                config%exclude_patterns(i) = trim(adjustl(exclude_patterns(i)))
            end do
        end if
        
        ! Update other fields
        config%minimum_coverage = minimum_coverage
        config%verbose = verbose
        config%quiet = quiet
        
        ! Validate the loaded configuration
        if (config%minimum_coverage < MIN_COVERAGE .or. &
            config%minimum_coverage > MAX_COVERAGE) then
            error_message = "Invalid threshold in config file (must be 0-100)"
            return
        end if
        
        success = .true.
    end subroutine load_config_file

    subroutine show_help()
        print *, "Usage: fortcov [OPTIONS]"
        print *, ""
        print *, "Options:"
        print *, "  --input-format=FORMAT     Input format (gcov, lcov) [default: gcov]"
        print *, "  --output-format=FORMAT    Output format (markdown, json, xml) [default: markdown]"
        print *, "  --output=FILE             Output file path [default: stdout]"
        print *, "  --source=PATH             Source directory to include (can be repeated)"
        print *, "  --exclude=PATTERN         Pattern to exclude (can be repeated)"
        print *, "  --fail-under=THRESHOLD    Minimum coverage threshold (0-100)"
        print *, "  --config=FILE             Load configuration from namelist file"
        print *, "  --verbose, -v             Enable verbose output"
        print *, "  --quiet, -q               Suppress all non-error output"
        print *, "  --help, -h                Show this help message"
        print *, ""
        print *, "Examples:"
        print *, "  fortcov --output=coverage.md --source=src"
        print *, "  fortcov --fail-under=80 --exclude='*.mod' --verbose"
        print *, "  fortcov --config=fortcov.nml"
        print *, ""
        print *, "Configuration File Format (Fortran namelist):"
        print *, "  Create a file (e.g., fortcov.nml) with:"
        print *, ""
        print *, "  &fortcov_config"
        print *, "    input_format = 'gcov'"
        print *, "    output_format = 'markdown'"
        print *, "    output_path = 'coverage.md'"
        print *, "    source_paths = 'src/', 'lib/', 'app/'"
        print *, "    exclude_patterns = '*.mod', 'test/*', 'build/*'"
        print *, "    minimum_coverage = 80.0"
        print *, "    verbose = .true."
        print *, "    quiet = .false."
        print *, "  /"
    end subroutine show_help

    subroutine show_version()
        print *, "fortcov version 1.0.0"
        print *, "Coverage analysis tool for Fortran code"
        print *, "Copyright (c) 2025 fortcov contributors"
    end subroutine show_version

    subroutine initialize_config(config)
        type(config_t), intent(out) :: config
        
        config%input_format = "gcov"
        config%output_format = "markdown"
        config%output_path = "-"
        allocate(character(len=MAX_PATH_LENGTH) :: config%source_paths(0))
        allocate(character(len=MAX_PATH_LENGTH) :: config%exclude_patterns(0))
        config%minimum_coverage = MIN_COVERAGE
        config%verbose = .false.
        config%quiet = .false.
        config%show_help = .false.
        config%show_version = .false.
        config%config_file = ""
    end subroutine initialize_config

    subroutine add_to_array(value, array, count, max_size, type_name)
        character(len=*), intent(in) :: value
        character(len=*), intent(inout) :: array(:)
        integer, intent(inout) :: count
        integer, intent(in) :: max_size
        character(len=*), intent(in) :: type_name
        
        associate (unused_vars => [type_name])
        end associate
        
        count = count + 1
        if (count <= max_size) then
            array(count) = value
        end if
    end subroutine add_to_array

    subroutine parse_threshold(value_str, threshold, success, error_message)
        character(len=*), intent(in) :: value_str
        real, intent(out) :: threshold
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        call parse_real(value_str, threshold, success)
        if (.not. success) then
            error_message = "Invalid threshold value: " // value_str
            return
        end if
        
        if (threshold < MIN_COVERAGE .or. threshold > MAX_COVERAGE) then
            success = .false.
            error_message = "Threshold must be between 0 and 100"
            return
        end if
    end subroutine parse_threshold

    subroutine finalize_array(temp_array, count, final_array)
        character(len=*), intent(in) :: temp_array(:)
        integer, intent(in) :: count
        character(len=:), allocatable, intent(out) :: final_array(:)
        
        if (count > 0) then
            allocate(character(len=MAX_PATH_LENGTH) :: final_array(count))
            final_array(1:count) = temp_array(1:count)
        else
            allocate(character(len=MAX_PATH_LENGTH) :: final_array(0))
        end if
    end subroutine finalize_array

end module fortcov_config