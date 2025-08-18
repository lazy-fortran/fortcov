module fortcov_config
    use string_utils
    use file_utils
    use secure_command_executor
    use error_handling
    implicit none
    private
    
    ! Constants
    integer, parameter :: MAX_PATH_LENGTH = 256
    integer, parameter :: MAX_ARRAY_SIZE = 100
    real, parameter :: MIN_COVERAGE = 0.0
    real, parameter :: MAX_COVERAGE = 100.0
    
    ! Public constants
    public :: MAX_ARRAY_SIZE
    
    ! Public types
    public :: config_t
    
    ! Public procedures
    public :: parse_config
    public :: show_help
    public :: show_version
    public :: initialize_config
    public :: validate_config
    public :: load_config_file
    
    ! Configuration type
    type :: config_t
        character(len=:), allocatable :: input_format
        character(len=:), allocatable :: output_format
        character(len=:), allocatable :: output_path
        character(len=:), allocatable :: source_paths(:)
        character(len=:), allocatable :: exclude_patterns(:)
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: gcov_executable
        real :: minimum_coverage
        logical :: verbose
        logical :: quiet
        logical :: show_help
        logical :: show_version
        character(len=:), allocatable :: config_file
        ! Diff functionality fields
        logical :: enable_diff
        character(len=:), allocatable :: diff_baseline_file
        character(len=:), allocatable :: diff_current_file
        logical :: include_unchanged
        real :: diff_threshold
        ! Import functionality fields
        character(len=:), allocatable :: import_file
        ! Missing CLI flags (Issue #59)
        logical :: keep_gcov_files
        character(len=:), allocatable :: gcov_args
        ! TUI mode flag (Issue #106)
        logical :: tui_mode
        ! Strict mode flag (Issue #109)
        logical :: strict_mode
    end type config_t

contains

    subroutine parse_config(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(out) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: flags(:)
        character(len=:), allocatable :: positionals(:)
        integer :: flag_count, positional_count
        
        ! Initialize config with defaults
        call initialize_config(config)
        
        success = .true.
        error_message = ""
        
        ! Two-pass parsing: classify arguments first
        call classify_arguments(args, flags, flag_count, positionals, &
                               positional_count)
        
        ! Pass 1: Process flag arguments
        call process_flags(flags, flag_count, config, success, error_message)
        if (.not. success) return
        
        ! Pass 2: Process positional arguments as coverage files
        call process_positional_arguments(positionals, positional_count, &
                                        config, success, error_message)
        if (.not. success) return
        
        ! Pass 3: Validate that input sources are provided (Issue #102)
        ! Only enforce this when no arguments provided or only output flags without sources
        call validate_input_sources(args, config, success, error_message)
        if (.not. success) return
        
        ! Pass 4: Apply HTML default filename logic (Issue #104)
        call apply_html_default_filename(config)
    end subroutine parse_config
    
    ! Process flag arguments using existing logic
    subroutine process_flags(flags, flag_count, config, success, error_message)
        character(len=*), intent(in) :: flags(:)
        integer, intent(in) :: flag_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: i
        character(len=:), allocatable :: arg
        character(len=:), allocatable :: key, value
        integer :: eq_pos
        character(len=:), allocatable :: temp_sources(:)
        character(len=:), allocatable :: temp_excludes(:)
        integer :: num_sources, num_excludes
        
        success = .true.
        error_message = ""
        
        ! Temporary arrays for dynamic sizing
        allocate(character(len=MAX_PATH_LENGTH) :: temp_sources(MAX_ARRAY_SIZE))
        allocate(character(len=MAX_PATH_LENGTH) :: temp_excludes(MAX_ARRAY_SIZE))
        num_sources = 0
        num_excludes = 0
        
        do i = 1, flag_count
            arg = trim(flags(i))
            
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
            
            ! Check for include-unchanged flag
            if (arg == "--include-unchanged") then
                config%include_unchanged = .true.
                cycle
            end if
            
            ! Check for keep-gcov-files flag (Issue #59)
            if (arg == "--keep-gcov-files") then
                config%keep_gcov_files = .true.
                cycle
            end if
            
            ! Check for TUI flag (Issue #106)
            if (arg == "--tui") then
                config%tui_mode = .true.
                cycle
            end if
            
            ! Check for strict flag (Issue #109)
            if (arg == "--strict") then
                config%strict_mode = .true.
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
                    
                case ("--gcov")
                    config%gcov_executable = trim(value)
                    
                case ("--gcov-args")
                    config%gcov_args = trim(value)
                    
                case ("--diff")
                    ! Parse diff with two file arguments
                    config%enable_diff = .true.
                    if (index(value, ',') > 0) then
                        ! Format: --diff=baseline.json,current.json
                        config%diff_baseline_file = trim(value(1:index(value, ',')-1))
                        config%diff_current_file = trim(value(index(value, ',')+1:))
                    else
                        ! Single file format (assume baseline, current from next arg)
                        config%diff_baseline_file = trim(value)
                        ! Need to handle second file in next iteration or different way
                    end if
                    
                case ("--threshold")
                    ! Threshold is only valid in diff mode - check if diff enabled
                    if (.not. config%enable_diff) then
                        success = .false.
                        error_message = "Unknown option: " // trim(key)
                        return
                    end if
                    ! Parse threshold value for coverage diff reporting
                    call parse_real(value, config%diff_threshold, success)
                    if (.not. success) then
                        error_message = "Invalid threshold value: " // trim(value)
                        return
                    end if
                    
                case ("--import")
                    config%import_file = trim(value)
                    ! When importing JSON, set input format to json automatically
                    config%input_format = "json"
                    
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
        
        ! Finalize arrays - only override if config file didn't already populate them
        if (.not. allocated(config%source_paths) .or. size(config%source_paths) == 0) then
            call finalize_array(temp_sources, num_sources, config%source_paths)
        end if
        if (.not. allocated(config%exclude_patterns) .or. size(config%exclude_patterns) == 0) then
            call finalize_array(temp_excludes, num_excludes, config%exclude_patterns)
        end if
    end subroutine process_flags
    
    ! Process positional arguments as coverage files
    subroutine process_positional_arguments(positionals, positional_count, &
                                          config, success, error_message)
        character(len=*), intent(in) :: positionals(:)
        integer, intent(in) :: positional_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=MAX_PATH_LENGTH) :: filtered_positionals(MAX_ARRAY_SIZE)
        integer :: filtered_count
        integer :: i
        
        success = .true.
        error_message = ""
        filtered_count = 0
        
        if (positional_count > 0) then
            ! Filter out executable paths and keep only .gcov files
            do i = 1, positional_count
                if (is_executable_path(positionals(i))) then
                    ! Skip executable paths silently
                    cycle
                else
                    filtered_count = filtered_count + 1
                    filtered_positionals(filtered_count) = positionals(i)
                end if
            end do
            
            ! Only validate remaining files
            if (filtered_count > 0) then
                call validate_coverage_files(filtered_positionals(1:filtered_count), &
                                           success, error_message)
                if (.not. success) return
                
                ! Store coverage files (deallocate first if already allocated)
                if (allocated(config%coverage_files)) deallocate(config%coverage_files)
                allocate(character(len=MAX_PATH_LENGTH) :: &
                        config%coverage_files(filtered_count))
                config%coverage_files(1:filtered_count) = &
                    filtered_positionals(1:filtered_count)
            end if
        end if
    end subroutine process_positional_arguments

    ! Helper function to detect executable paths
    function is_executable_path(arg) result(is_executable)
        character(len=*), intent(in) :: arg
        logical :: is_executable
        
        ! Check if this looks like an executable path
        ! Executables typically contain "/", end with "fortcov", or are in build directories
        is_executable = (index(arg, "/") > 0 .and. &
                        (index(arg, "fortcov") > 0 .or. &
                         index(arg, "/build/") > 0 .or. &
                         index(arg, "/app/") > 0))
    end function is_executable_path

    ! Helper function to detect flag arguments
    function is_flag_argument(arg) result(is_flag)
        character(len=*), intent(in) :: arg
        logical :: is_flag
        
        ! Check for both -- and - flags
        is_flag = (len_trim(arg) > 1) .and. (arg(1:1) == "-")
    end function is_flag_argument
    
    ! Classify arguments into flags and positional arguments
    subroutine classify_arguments(args, flags, flag_count, positionals, &
                                 positional_count)
        character(len=*), intent(in) :: args(:)
        character(len=:), allocatable, intent(out) :: flags(:)
        integer, intent(out) :: flag_count
        character(len=:), allocatable, intent(out) :: positionals(:)
        integer, intent(out) :: positional_count
        
        integer :: i, argc
        character(len=MAX_PATH_LENGTH) :: temp_flags(MAX_ARRAY_SIZE)
        character(len=MAX_PATH_LENGTH) :: temp_positionals(MAX_ARRAY_SIZE)
        
        argc = size(args)
        flag_count = 0
        positional_count = 0
        
        ! Classify each argument
        do i = 1, argc
            if (len_trim(args(i)) == 0) cycle
            
            if (is_flag_argument(args(i))) then
                flag_count = flag_count + 1
                temp_flags(flag_count) = trim(args(i))
            else
                positional_count = positional_count + 1
                temp_positionals(positional_count) = trim(args(i))
            end if
        end do
        
        ! Allocate and copy flags
        if (flag_count > 0) then
            allocate(character(len=MAX_PATH_LENGTH) :: flags(flag_count))
            flags(1:flag_count) = temp_flags(1:flag_count)
        else
            allocate(character(len=MAX_PATH_LENGTH) :: flags(0))
        end if
        
        ! Allocate and copy positionals
        if (positional_count > 0) then
            allocate(character(len=MAX_PATH_LENGTH) :: positionals(positional_count))
            positionals(1:positional_count) = temp_positionals(1:positional_count)
        else
            allocate(character(len=MAX_PATH_LENGTH) :: positionals(0))
        end if
    end subroutine classify_arguments
    
    ! Validate coverage file extensions and security
    subroutine validate_coverage_files(files, success, error_message)
        character(len=*), intent(in) :: files(:)
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: i
        character(len=:), allocatable :: file_ext
        
        success = .true.
        error_message = ""
        
        do i = 1, size(files)
            if (len_trim(files(i)) == 0) cycle
            
            ! Check for .gcov extension
            if (len_trim(files(i)) < 5) then
                success = .false.
                error_message = "Invalid file name: " // trim(files(i))
                return
            end if
            
            file_ext = files(i)(len_trim(files(i))-4:len_trim(files(i)))
            if (file_ext /= ".gcov") then
                success = .false.
                error_message = "Invalid file extension: " // trim(files(i)) // &
                              " (expected .gcov)"
                return
            end if
        end do
    end subroutine validate_coverage_files

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
        
        ! Namelist variables - support both single string and array formats
        character(len=256) :: input_format
        character(len=256) :: output_format
        character(len=256) :: output_path
        character(len=256) :: source_paths(MAX_ARRAY_SIZE)  ! Array format
        character(len=256) :: exclude_patterns(MAX_ARRAY_SIZE)  ! Array format
        character(len=256) :: gcov_executable
        real :: minimum_coverage
        logical :: verbose
        logical :: quiet
        integer :: unit, iostat, i, count
        logical :: file_exists
        character(len=:), allocatable :: split_sources(:)
        character(len=:), allocatable :: split_excludes(:)
        
        namelist /fortcov_config/ input_format, output_format, output_path, &
                                  source_paths, exclude_patterns, &
                                  gcov_executable, minimum_coverage, verbose, quiet
        
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
        source_paths = ''           ! Initialize all array elements to empty
        exclude_patterns = ''       ! Initialize all array elements to empty
        gcov_executable = config%gcov_executable
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
            write(error_message, '(A,I0)') &
                "Invalid namelist format in config file (iostat=", iostat
            error_message = trim(error_message) // ")"
            return
        end if
        
        ! Update config from namelist variables
        config%input_format = trim(adjustl(input_format))
        config%output_format = trim(adjustl(output_format))
        config%output_path = trim(adjustl(output_path))
        config%gcov_executable = trim(adjustl(gcov_executable))
        
        ! Process source paths from array format (backward compatible)
        ! MEMORY SAFETY FIX: Safe deallocation with error handling
        if (allocated(config%source_paths)) then
            deallocate(config%source_paths)
        end if
        count = 0
        
        ! Check if first entry contains comma-separated values (legacy format)
        if (len_trim(source_paths(1)) > 0 .and. index(source_paths(1), ',') > 0) then
            ! Legacy format: single string with comma-separated values
            split_sources = split(trim(source_paths(1)), ',')
            
            ! MEMORY SAFETY FIX: Allocation error handling
            if (allocated(split_sources) .and. size(split_sources) > 0) then
                allocate(character(len=MAX_PATH_LENGTH) :: &
                        config%source_paths(size(split_sources)), stat=iostat)
                if (iostat /= 0) then
                    error_message = "Memory allocation failed for source_paths"
                    success = .false.
                    ! MEMORY SAFETY: Clean up split_sources before returning
                    if (allocated(split_sources)) deallocate(split_sources)
                    return
                end if
                
                do i = 1, size(split_sources)
                    config%source_paths(i) = trim(adjustl(split_sources(i)))
                end do
            else
                allocate(character(len=MAX_PATH_LENGTH) :: config%source_paths(0), &
                        stat=iostat)
                if (iostat /= 0) then
                    error_message = "Memory allocation failed for empty source_paths"
                    success = .false.
                    ! MEMORY SAFETY: Clean up split_sources before returning
                    if (allocated(split_sources)) deallocate(split_sources)
                    return
                end if
            end if
            
            ! MEMORY SAFETY FIX: Explicit cleanup of split result
            if (allocated(split_sources)) deallocate(split_sources)
        else
            ! New format: array entries
            ! Count non-empty entries
            do i = 1, MAX_ARRAY_SIZE
                if (len_trim(source_paths(i)) > 0) then
                    count = count + 1
                else
                    exit  ! Stop at first empty entry
                end if
            end do
            
            if (count > 0) then
                allocate(character(len=MAX_PATH_LENGTH) :: config%source_paths(count), &
                        stat=iostat)
                if (iostat /= 0) then
                    error_message = "Memory allocation failed for source_paths array"
                    success = .false.
                    return
                end if
                
                do i = 1, count
                    config%source_paths(i) = trim(adjustl(source_paths(i)))
                end do
            else
                allocate(character(len=MAX_PATH_LENGTH) :: config%source_paths(0), &
                        stat=iostat)
                if (iostat /= 0) then
                    error_message = "Memory allocation failed for empty source_paths array"
                    success = .false.
                    return
                end if
            end if
        end if
        
        ! Process exclude patterns from array format (backward compatible)
        ! MEMORY SAFETY FIX: Safe deallocation with error handling
        if (allocated(config%exclude_patterns)) then
            deallocate(config%exclude_patterns)
        end if
        count = 0
        
        ! Check if first entry contains comma-separated values (legacy format)
        if (len_trim(exclude_patterns(1)) > 0 .and. index(exclude_patterns(1), ',') > 0) then
            ! Legacy format: single string with comma-separated values
            split_excludes = split(trim(exclude_patterns(1)), ',')
            
            ! MEMORY SAFETY FIX: Allocation error handling
            if (allocated(split_excludes) .and. size(split_excludes) > 0) then
                allocate(character(len=MAX_PATH_LENGTH) :: &
                        config%exclude_patterns(size(split_excludes)), stat=iostat)
                if (iostat /= 0) then
                    error_message = "Memory allocation failed for exclude_patterns"
                    success = .false.
                    ! MEMORY SAFETY: Clean up split_excludes before returning
                    if (allocated(split_excludes)) deallocate(split_excludes)
                    return
                end if
                
                do i = 1, size(split_excludes)
                    config%exclude_patterns(i) = trim(adjustl(split_excludes(i)))
                end do
            else
                allocate(character(len=MAX_PATH_LENGTH) :: config%exclude_patterns(0), &
                        stat=iostat)
                if (iostat /= 0) then
                    error_message = "Memory allocation failed for empty exclude_patterns"
                    success = .false.
                    ! MEMORY SAFETY: Clean up split_excludes before returning
                    if (allocated(split_excludes)) deallocate(split_excludes)
                    return
                end if
            end if
            
            ! MEMORY SAFETY FIX: Explicit cleanup of split result
            if (allocated(split_excludes)) deallocate(split_excludes)
        else
            ! New format: array entries
            ! Count non-empty entries
            do i = 1, MAX_ARRAY_SIZE
                if (len_trim(exclude_patterns(i)) > 0) then
                    count = count + 1
                else
                    exit  ! Stop at first empty entry
                end if
            end do
            
            if (count > 0) then
                allocate(character(len=MAX_PATH_LENGTH) :: &
                        config%exclude_patterns(count), stat=iostat)
                if (iostat /= 0) then
                    error_message = "Memory allocation failed for exclude_patterns array"
                    success = .false.
                    return
                end if
                
                do i = 1, count
                    config%exclude_patterns(i) = trim(adjustl(exclude_patterns(i)))
                end do
            else
                allocate(character(len=MAX_PATH_LENGTH) :: config%exclude_patterns(0), &
                        stat=iostat)
                if (iostat /= 0) then
                    error_message = "Memory allocation failed for empty exclude_patterns array"
                    success = .false.
                    return
                end if
            end if
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
        print *, "FortCov - Coverage analysis tool for Fortran projects"
        print *, ""
        print *, "USAGE:"
        print *, "  fortcov --source=PATH --output=FILE [OPTIONS]"
        print *, ""
        print *, "QUICK START:"
        print *, "  fortcov --source=src --output=coverage.md        # Basic coverage report"
        print *, "  fortcov --source=src --fail-under=80 --quiet     # CI/CD usage"
        print *, "  fortcov --source=src --tui                       # Interactive browser"
        print *, ""
        print *, "COMMON OPTIONS:"
        print *, "  -s, --source=PATH         Source directory to analyze (required)"
        print *, "  -o, --output=FILE         Output file [default: stdout]"
        print *, "  -t, --fail-under=N        Minimum coverage threshold (0-100)"
        print *, "  -v, --verbose             Show detailed processing information"
        print *, "  -q, --quiet               Only show errors (perfect for CI/CD)"
        print *, "  -h, --help                Show this help message"
        print *, ""
        print *, "INPUT/OUTPUT OPTIONS:"
        print *, "  --input-format=FORMAT     Input: gcov, lcov, json [default: gcov]"
        print *, "  --output-format=FORMAT    Output: markdown, json, html [default: markdown]"
        print *, "  --import=FILE             Import existing coverage data (JSON format)"
        print *, ""
        print *, "FILTERING OPTIONS:"
        print *, "  --exclude=PATTERN         Skip files matching pattern (use multiple times)"
        print *, "                            Example: --exclude='*.mod' --exclude='test/*'"
        print *, ""
        print *, "ADVANCED OPTIONS:"
        print *, "  --config=FILE             Use configuration file (see fortcov.nml.example)"
        print *, "  --gcov=EXECUTABLE         Custom gcov path [default: gcov]"
        print *, "  --tui                     Interactive terminal interface"
        print *, "  --diff=BASE,CURRENT       Compare two coverage datasets"
        print *, "  --strict                  Exit with error if no coverage files found"
        print *, ""
        print *, "EXIT CODES:"
        print *, "  0   Success"
        print *, "  1   Error (missing files, invalid options, etc.)"
        print *, "  2   Coverage below --fail-under threshold"
        print *, ""
        print *, "EXAMPLES:"
        print *, ""
        print *, "  Basic usage:"
        print *, "    fortcov --source=src --output=coverage.md"
        print *, ""
        print *, "  CI/CD pipeline:"
        print *, "    fortcov --source=src --fail-under=80 --quiet"
        print *, ""
        print *, "  Exclude test files:"
        print *, "    fortcov --source=src --exclude='test/*' --exclude='*_test.f90'"
        print *, ""
        print *, "  Multiple output formats:"
        print *, "    fortcov --source=src --output-format=json --output=coverage.json"
        print *, "    fortcov --source=src --output-format=html --output=report.html"
        print *, ""
        print *, "  Interactive analysis:"
        print *, "    fortcov --source=src --tui"
        print *, ""
        print *, "  Compare coverage between versions:"
        print *, "    fortcov --diff=v1.0.json,v1.1.json --threshold=5.0"
        print *, ""
        print *, "GETTING STARTED:"
        print *, "  1. Build with coverage: fpm build --flag ""-fprofile-arcs -ftest-coverage"""
        print *, "  2. Run tests:           fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        print *, "  3. Generate data:       gcov src/*.f90"
        print *, "  4. Create report:       fortcov --source=src --output=coverage.md"
        print *, ""
        print *, "For detailed documentation see: https://github.com/krystophny/fortcov"
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
        config%gcov_executable = "gcov"  ! Default to system gcov
        allocate(character(len=MAX_PATH_LENGTH) :: config%source_paths(0))
        allocate(character(len=MAX_PATH_LENGTH) :: config%exclude_patterns(0))
        allocate(character(len=MAX_PATH_LENGTH) :: config%coverage_files(0))
        config%minimum_coverage = MIN_COVERAGE
        config%verbose = .false.
        config%quiet = .false.
        config%show_help = .false.
        config%show_version = .false.
        config%config_file = ""
        ! Diff functionality defaults
        config%enable_diff = .false.
        config%diff_baseline_file = ""
        config%diff_current_file = ""
        config%include_unchanged = .false.
        config%diff_threshold = 0.0
        ! Import functionality defaults
        config%import_file = ""
        ! Missing CLI flags defaults (Issue #59)
        config%keep_gcov_files = .false.
        config%gcov_args = ""
        ! TUI mode default (Issue #106)
        config%tui_mode = .false.
        ! Strict mode default (Issue #109)
        config%strict_mode = .false.
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

    ! Validate configuration for security and accessibility
    subroutine validate_config(config, error_ctx)
        type(config_t), intent(in) :: config
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: safe_path
        character(len=:), allocatable :: safe_executable
        integer :: i
        
        call clear_error_context(error_ctx)
        
        ! Validate gcov executable
        if (len_trim(config%gcov_executable) > 0) then
            call validate_executable_path(config%gcov_executable, &
                                        safe_executable, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) then
                call safe_write_context(error_ctx, "gcov executable validation")
                return
            end if
        end if
        
        ! Validate output path if it's not stdout
        if (config%output_path /= "-" .and. len_trim(config%output_path) > 0) then
            call validate_path_security(config%output_path, safe_path, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS) then
                call safe_write_context(error_ctx, "output path validation")
                return
            end if
        end if
        
        ! Validate source paths
        if (allocated(config%source_paths)) then
            do i = 1, size(config%source_paths)
                if (len_trim(config%source_paths(i)) > 0) then
                    call validate_path_security(config%source_paths(i), &
                                               safe_path, error_ctx)
                    if (error_ctx%error_code /= ERROR_SUCCESS) then
                        call safe_write_context(error_ctx, &
                            "source path validation: " // trim(config%source_paths(i)))
                        return
                    end if
                end if
            end do
        end if
        
        ! Validate exclude patterns (basic security check)
        if (allocated(config%exclude_patterns)) then
            do i = 1, size(config%exclude_patterns)
                if (len_trim(config%exclude_patterns(i)) > 0) then
                    call validate_path_security(config%exclude_patterns(i), &
                                               safe_path, error_ctx)
                    if (error_ctx%error_code /= ERROR_SUCCESS) then
                        call safe_write_context(error_ctx, &
                            "exclude pattern validation: " // trim(config%exclude_patterns(i)))
                        return
                    end if
                end if
            end do
        end if
        
        ! Validate coverage threshold
        if (config%minimum_coverage < MIN_COVERAGE .or. &
            config%minimum_coverage > MAX_COVERAGE) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Invalid coverage threshold (must be 0-100)")
            call safe_write_suggestion(error_ctx, &
                "Set threshold between 0 and 100 percent")
            call safe_write_context(error_ctx, "coverage threshold validation")
            return
        end if
        
        ! Validate input/output formats
        if (trim(config%input_format) /= "gcov" .and. &
            trim(config%input_format) /= "lcov" .and. &
            trim(config%input_format) /= "json") then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Unsupported input format: " // trim(config%input_format))
            call safe_write_suggestion(error_ctx, &
                "Use supported input formats: gcov, lcov, json")
            call safe_write_context(error_ctx, "input format validation")
            return
        end if
        
        if (trim(config%output_format) /= "markdown" .and. &
            trim(config%output_format) /= "md" .and. &
            trim(config%output_format) /= "json" .and. &
            trim(config%output_format) /= "xml" .and. &
            trim(config%output_format) /= "html") then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Unsupported output format: " // trim(config%output_format))
            call safe_write_suggestion(error_ctx, &
                "Use supported output formats: markdown, md, json, xml, html")
            call safe_write_context(error_ctx, "output format validation")
            return
        end if
    end subroutine validate_config

    ! Validate that input sources are provided (Issue #102)
    subroutine validate_input_sources(args, config, success, error_message)
        character(len=*), intent(in) :: args(:)
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        success = .true.
        error_message = ""
        
        ! Skip validation if help or version flags are already set
        if (config%show_help .or. config%show_version) then
            return
        end if
        
        ! Show help when no arguments provided at all (Issue #102)
        ! This addresses the main issue where running `fortcov` with no args
        ! should show help instead of attempting to run coverage analysis
        if (size(args) == 0) then
            config%show_help = .true.
            success = .false.
            error_message = "No arguments provided. " // &
                          "Use --help for usage information."
        end if
    end subroutine validate_input_sources

    ! Apply HTML default filename logic (Issue #104)
    ! When HTML format is specified without explicit output file,
    ! automatically set default filename to avoid stdout mixing
    subroutine apply_html_default_filename(config)
        type(config_t), intent(inout) :: config
        
        ! Only apply to HTML format without explicit output file
        if (trim(config%output_format) == "html" .and. &
            trim(config%output_path) == "-") then
            config%output_path = "coverage_report.html"
        end if
    end subroutine apply_html_default_filename

end module fortcov_config
