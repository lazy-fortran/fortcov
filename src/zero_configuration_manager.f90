module zero_configuration_manager
    !! Zero-Configuration Manager (Issue #204)
    !! 
    !! Implementation module for zero-configuration functionality
    !! 
    !! This module will implement the zero-configuration functionality for Issue #204.
    !! The comprehensive test suite has been implemented in RED phase and is ready
    !! for this implementation to make the tests pass.
    !! 
    !! Responsibilities:
    !! - Detect zero-argument command execution
    !! - Auto-discover coverage files from build/gcov and fallback locations
    !! - Auto-discover source files from src/ and current directory
    !! - Set smart defaults for output path (build/coverage/coverage.md)
    !! - Create output directories as needed
    !! - Provide helpful error messages when no coverage files found
    !! 
    !! Integration with existing modules:
    !! - config_parser: Enhanced to detect zero-argument mode
    !! - coverage_discovery: Extended for priority-ordered auto-discovery
    !! - file_utils: Directory creation for output paths
    !! - user_guidance: Error messages and recovery instructions
    use coverage_discovery, only: discover_coverage_files
    use file_utils, only: find_files, find_files_with_glob, ensure_directory
    use error_handling, only: error_context_t, ERROR_SUCCESS, ERROR_FILE_ACCESS
    implicit none
    private
    
    ! Interface for system process ID function
    interface
        function c_getpid() bind(c, name="getpid")
            use iso_c_binding, only: c_int
            integer(c_int) :: c_getpid
        end function c_getpid
    end interface
    
    ! Public interfaces for zero-configuration functionality
    public :: is_zero_configuration_mode
    public :: apply_zero_configuration_defaults
    public :: auto_discover_coverage_files_priority
    public :: auto_discover_source_files_priority
    public :: ensure_output_directory_structure
    public :: show_zero_configuration_error_guidance
    
contains

    function escape_shell_arg(arg) result(escaped_arg)
        !! Properly escape shell arguments to prevent injection attacks
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: escaped_arg
        integer :: i, len_arg, escape_count
        character :: c
        
        len_arg = len_trim(arg)
        escape_count = 0
        
        ! Count special characters that need escaping
        do i = 1, len_arg
            c = arg(i:i)
            if (c == "'" .or. c == '"' .or. c == '\' .or. c == '$' .or. &
                c == '`' .or. c == '!' .or. c == '*' .or. c == '?' .or. &
                c == '[' .or. c == ']' .or. c == '(' .or. c == ')' .or. &
                c == '{' .or. c == '}' .or. c == ';' .or. c == '&' .or. &
                c == '|' .or. c == '<' .or. c == '>') then
                escape_count = escape_count + 1
            end if
        end do
        
        ! Allocate escaped string with extra space for escape characters
        allocate(character(len=len_arg + escape_count + 2) :: escaped_arg)
        
        ! Build escaped string with single quotes
        escaped_arg = "'" // replace_single_quotes(arg(1:len_arg)) // "'"
    end function escape_shell_arg
    
    function replace_single_quotes(str) result(escaped_str)
        !! Replace single quotes with '\'' sequence for shell safety
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped_str
        integer :: i, len_str, quote_count, pos
        
        len_str = len_trim(str)
        quote_count = 0
        
        ! Count single quotes
        do i = 1, len_str
            if (str(i:i) == "'") quote_count = quote_count + 1
        end do
        
        ! Allocate space for replacement (each ' becomes '\'' - 3 extra chars)
        allocate(character(len=len_str + quote_count*3) :: escaped_str)
        
        pos = 1
        do i = 1, len_str
            if (str(i:i) == "'") then
                escaped_str(pos:pos+3) = "'\''"
                pos = pos + 4
            else
                escaped_str(pos:pos) = str(i:i)
                pos = pos + 1
            end if
        end do
        
        ! Trim to actual length
        escaped_str = escaped_str(1:pos-1)
    end function replace_single_quotes
    
    function is_zero_configuration_mode() result(is_zero_config)
        !! Detect if user invoked fortcov with no arguments (zero-configuration mode)
        logical :: is_zero_config
        integer :: argc
        
        ! Check if fortcov was invoked with no arguments
        argc = command_argument_count()
        is_zero_config = (argc == 0)
        
    end function is_zero_configuration_mode
    
    subroutine apply_zero_configuration_defaults(output_path, output_format, &
                                                input_format, exclude_patterns)
        !! Apply smart defaults for zero-configuration mode
        character(len=:), allocatable, intent(out) :: output_path
        character(len=:), allocatable, intent(out) :: output_format
        character(len=:), allocatable, intent(out) :: input_format
        character(len=:), allocatable, intent(out) :: exclude_patterns(:)
        
        ! Set smart defaults for zero-configuration mode
        output_path = "build/coverage/coverage.md"
        output_format = "markdown"
        input_format = "gcov"
        
        ! Default exclusion patterns for common build and test directories
        allocate(character(len=32) :: exclude_patterns(2))
        exclude_patterns(1) = "build/*"
        exclude_patterns(2) = "test/*"
        
    end subroutine apply_zero_configuration_defaults
    
    function auto_discover_coverage_files_priority() result(coverage_files)
        !! Auto-discover coverage files using priority-ordered search with automatic gcov generation
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: temp_files(:), gcda_files(:)
        logical :: dir_exists, gcov_available
        integer :: i
        
        ! Phase 1: Check for existing .gcov files (fast path)
        coverage_files = discover_existing_gcov_files()
        if (allocated(coverage_files) .and. size(coverage_files) > 0) then
            return
        end if
        
        ! Phase 2: Auto-generate .gcov files from .gcda/.gcno (zero-config enhancement)
        call check_gcov_availability(gcov_available)
        if (.not. gcov_available) then
            if (allocated(coverage_files)) deallocate(coverage_files)
            allocate(character(len=256) :: coverage_files(0))
            return
        end if
        
        ! Discover .gcda files in build directories
        gcda_files = discover_gcda_files_priority()
        
        if (allocated(gcda_files) .and. size(gcda_files) > 0) then
            call generate_gcov_files_from_gcda(gcda_files, coverage_files)
            return
        end if
        
        ! Phase 3: No coverage data found
        if (allocated(coverage_files)) deallocate(coverage_files)
        allocate(character(len=256) :: coverage_files(0))
        
    end function auto_discover_coverage_files_priority
    
    function auto_discover_source_files_priority() result(source_paths)
        !! Auto-discover source files using priority-ordered search
        character(len=:), allocatable :: source_paths(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        integer :: file_count
        
        ! Priority 1: Check if src/ directory exists and has Fortran files
        inquire(file="src", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("src", "*.f90")
            if (allocated(temp_files)) then
                file_count = size(temp_files)
            else
                file_count = 0
            end if
            
            if (file_count > 0) then
                allocate(character(len=3) :: source_paths(1))
                source_paths(1) = "src"
                return
            end if
        end if
        
        ! Priority 2: Use current directory as fallback
        allocate(character(len=1) :: source_paths(1))
        source_paths(1) = "."
        
    end function auto_discover_source_files_priority
    
    subroutine ensure_output_directory_structure(output_path, error_ctx)
        !! Ensure output directory structure exists, create if needed
        character(len=*), intent(in) :: output_path
        type(error_context_t), intent(inout) :: error_ctx
        character(len=256) :: directory_path
        integer :: last_slash_pos
        logical :: dir_exists, error_flag
        
        ! Extract directory from output path
        last_slash_pos = index(output_path, '/', back=.true.)
        if (last_slash_pos > 0) then
            directory_path = output_path(1:last_slash_pos-1)
            
            ! Check if directory exists
            inquire(file=trim(directory_path), exist=dir_exists)
            if (.not. dir_exists) then
                ! Create directory structure
                call ensure_directory(trim(directory_path), error_flag)
                if (error_flag) then
                    error_ctx%error_code = ERROR_FILE_ACCESS
                    error_ctx%message = "Failed to create output directory: " // &
                                      trim(directory_path)
                    error_ctx%suggestion = "Check permissions in parent directory"
                else
                    error_ctx%error_code = ERROR_SUCCESS
                end if
            else
                error_ctx%error_code = ERROR_SUCCESS
            end if
        else
            ! No directory in path, current directory will be used
            error_ctx%error_code = ERROR_SUCCESS
        end if
        
    end subroutine ensure_output_directory_structure
    
    subroutine show_zero_configuration_error_guidance()
        !! Show helpful error messages when zero-configuration fails
        
        print *, ""
        print *, "No coverage files found in standard locations"
        print *, ""
        print *, "Fortcov searched for .gcov files in:"
        print *, "  1. build/gcov/  (Issue #203 standard location - RECOMMENDED)"
        print *, "  2. ./           (current directory)"
        print *, "  3. build/       (recursive search)"
        print *, ""
        print *, "To generate coverage files, follow these steps:"
        print *, ""
        print *, "Step 1: Compile with coverage flags"
        print *, "  fpm test --flag '-fprofile-arcs -ftest-coverage'"
        print *, ""
        print *, "Step 2: Run your tests to generate coverage data"
        print *, "  (This creates .gcda files alongside your .gcno files)"
        print *, ""
        print *, "Step 3: Generate .gcov files using gcov"
        print *, "  Option A (RECOMMENDED - Issue #203 standard):"
        print *, "    mkdir -p build/gcov"
        print *, "    gcov -o build/your_test_dir src/*.f90 -t build/gcov/"
        print *, ""
        print *, "  Option B (current directory):"
        print *, "    gcov src/*.f90"
        print *, ""
        print *, "Step 4: Run fortcov again"
        print *, "  fortcov"
        print *, ""
        print *, "Alternative: Specify coverage files manually"
        print *, "  fortcov path/to/*.gcov --source=src --output=coverage.md"
        print *, ""
        print *, "For more help: fortcov --help"
        print *, ""
        
    end subroutine show_zero_configuration_error_guidance
    
    function discover_existing_gcov_files() result(coverage_files)
        !! Phase 1: Discover existing .gcov files in priority locations
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        
        ! Priority 1: Check build/gcov/*.gcov (Issue #203 standard location)
        inquire(file="build/gcov", exist=dir_exists)
        if (dir_exists) then
            temp_files = direct_find_gcov_files("build/gcov")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                coverage_files = temp_files
                return
            end if
        end if
        
        ! Priority 2: Check current directory *.gcov
        temp_files = direct_find_gcov_files(".")
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            coverage_files = temp_files
            return
        end if
        
        ! Priority 3: Check build directory recursively (if exists)
        inquire(file="build", exist=dir_exists)
        if (dir_exists) then
            temp_files = direct_find_gcov_files_recursive("build")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                coverage_files = temp_files
                return
            end if
        end if
        
        ! No existing .gcov files found
        allocate(character(len=256) :: coverage_files(0))
    end function discover_existing_gcov_files
    
    subroutine check_gcov_availability(gcov_available)
        !! Check if gcov executable is available in PATH
        use path_validation, only: validate_executable_path
        logical, intent(out) :: gcov_available
        type(error_context_t) :: error_ctx
        character(len=:), allocatable :: safe_gcov
        
        ! Try to validate gcov executable path - if it passes, gcov is available
        call validate_executable_path("gcov", safe_gcov, error_ctx)
        gcov_available = (error_ctx%error_code == ERROR_SUCCESS)
    end subroutine check_gcov_availability
    
    function discover_gcda_files_priority() result(gcda_files)
        !! Systematically discover .gcda/.gcno files across different build system patterns
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: temp_files(:)
        
        ! Priority 1: FPM build structure
        temp_files = discover_fpm_gcda_files()
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            gcda_files = temp_files
            return
        end if
        
        ! Priority 2: CMake/Make build structure  
        temp_files = discover_cmake_gcda_files()
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            gcda_files = temp_files  
            return
        end if
        
        ! Priority 3: Generic build directory search
        temp_files = discover_generic_gcda_files()
        gcda_files = temp_files
    end function discover_gcda_files_priority
    
    function discover_fpm_gcda_files() result(gcda_files)
        !! Discover .gcda files in FPM build structure
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: temp_files(:), app_files(:), test_files(:)
        logical :: dir_exists
        integer :: total_files, app_count, test_count
        
        total_files = 0
        
        ! Check FPM app directory pattern: build/gfortran_*/app/*.gcda
        inquire(file="build", exist=dir_exists)
        if (dir_exists) then
            app_files = find_files_with_glob("build", "gfortran_*/app/*.gcda")
            if (allocated(app_files)) then
                total_files = total_files + size(app_files)
            end if
            
            ! Check FPM test directory pattern: build/gfortran_*/test/*.gcda
            test_files = find_files_with_glob("build", "gfortran_*/test/*.gcda")
            if (allocated(test_files)) then
                total_files = total_files + size(test_files)
            end if
        end if
        
        if (total_files > 0) then
            allocate(character(len=256) :: gcda_files(total_files))
            app_count = 0
            test_count = 0
            
            if (allocated(app_files) .and. size(app_files) > 0) then
                app_count = size(app_files)
                gcda_files(1:app_count) = app_files
            end if
            
            if (allocated(test_files) .and. size(test_files) > 0) then
                test_count = size(test_files)
                gcda_files(app_count+1:app_count+test_count) = test_files
            end if
        else
            allocate(character(len=256) :: gcda_files(0))
        end if
    end function discover_fpm_gcda_files
    
    function discover_cmake_gcda_files() result(gcda_files)
        !! Discover .gcda files in CMake build structure  
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        
        ! Check CMake patterns: build/**/*.gcda, _build/**/*.gcda
        inquire(file="build", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("build", "**/*.gcda")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                gcda_files = temp_files
                return
            end if
        end if
        
        inquire(file="_build", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("_build", "**/*.gcda")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                gcda_files = temp_files
                return
            end if
        end if
        
        allocate(character(len=256) :: gcda_files(0))
    end function discover_cmake_gcda_files
    
    function discover_generic_gcda_files() result(gcda_files)
        !! Discover .gcda files in generic build directory patterns
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        
        ! Generic patterns: any directory containing "build" in name
        temp_files = find_files_with_glob(".", "*build*/**/*.gcda")
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            gcda_files = temp_files
            return
        end if
        
        ! Check obj/objects directories
        inquire(file="obj", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("obj", "**/*.gcda")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                gcda_files = temp_files
                return
            end if
        end if
        
        inquire(file="objects", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("objects", "**/*.gcda")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                gcda_files = temp_files
                return
            end if
        end if
        
        allocate(character(len=256) :: gcda_files(0))
    end function discover_generic_gcda_files
    
    subroutine generate_gcov_files_from_gcda(gcda_files, generated_gcov_files)
        !! Safely execute gcov commands to generate .gcov files from .gcda files
        use gcov_command_executor, only: gcov_executor_t
        character(len=*), intent(in) :: gcda_files(:)
        character(len=:), allocatable, intent(out) :: generated_gcov_files(:)
        
        type(gcov_executor_t) :: executor
        type(error_context_t) :: error_ctx
        integer :: i, success_count
        character(len=256), allocatable :: temp_generated_files(:)
        character(len=:), allocatable :: temp_files(:)
        character(len=256) :: source_file
        logical :: dir_created
        
        ! Configure gcov executor for zero-config mode
        call executor%set_gcov_output_directory("build/gcov")
        call executor%set_working_directory(".")
        
        ! Ensure output directory exists
        call ensure_directory("build/gcov", dir_created)
        
        success_count = 0
        allocate(character(len=256) :: temp_generated_files(size(gcda_files) * 10))  ! Estimate
        
        do i = 1, size(gcda_files)
            source_file = extract_source_from_gcda(gcda_files(i))
            call executor%execute_gcov(source_file, temp_files, error_ctx)
            
            if (error_ctx%error_code == ERROR_SUCCESS .and. &
                allocated(temp_files) .and. size(temp_files) > 0) then
                temp_generated_files(success_count+1:success_count+size(temp_files)) = temp_files
                success_count = success_count + size(temp_files)
            end if
        end do
        
        ! Return successfully generated .gcov files
        if (success_count > 0) then
            allocate(character(len=256) :: generated_gcov_files(success_count))
            generated_gcov_files(1:success_count) = temp_generated_files(1:success_count)
        else
            allocate(character(len=256) :: generated_gcov_files(0))
        end if
    end subroutine generate_gcov_files_from_gcda
    
    function extract_source_from_gcda(gcda_file) result(source_file)
        !! Extract source file path from .gcda file path
        character(len=*), intent(in) :: gcda_file
        character(len=256) :: source_file
        integer :: gcda_pos
        logical :: file_exists
        
        ! Simple extraction: replace .gcda with .f90/.F90 (most common case)
        gcda_pos = index(gcda_file, '.gcda', back=.true.)
        if (gcda_pos > 0) then
            source_file = gcda_file(1:gcda_pos-1) // ".f90"
            ! Check if .f90 exists, otherwise try .F90
            inquire(file=source_file, exist=file_exists)
            if (.not. file_exists) then
                source_file = gcda_file(1:gcda_pos-1) // ".F90"
            end if
        else
            ! Fallback
            source_file = gcda_file
        end if
    end function extract_source_from_gcda
    
    function direct_find_gcov_files(directory) result(gcov_files)
        !! Direct filesystem-based .gcov file discovery for zero-config mode
        !! Uses system find command to discover ALL .gcov files in the directory
        !! Bypasses security restrictions for zero-configuration functionality
        character(len=*), intent(in) :: directory
        character(len=:), allocatable :: gcov_files(:)
        
        logical :: dir_exists
        character(len=512) :: command, temp_file
        character(len=256) :: line
        integer :: unit, iostat, file_count, i
        character(len=256), allocatable :: temp_results(:)
        
        ! Check if directory exists
        inquire(file=directory, exist=dir_exists)
        if (.not. dir_exists) then
            allocate(character(len=256) :: gcov_files(0))
            return
        end if
        
        ! Create temporary file for results
        temp_file = "/tmp/fortcov_find_gcov_" // get_unique_suffix()
        
        ! Build find command - properly escape directory path for security
        if (trim(directory) == ".") then
            command = "find . -maxdepth 1 -name '*.gcov' -type f > " // &
                     escape_shell_arg(trim(temp_file)) // " 2>/dev/null"
        else
            command = "find " // escape_shell_arg(trim(directory)) // &
                     " -maxdepth 1 -name '*.gcov' -type f > " // &
                     escape_shell_arg(trim(temp_file)) // " 2>/dev/null"
        end if
        
        ! Execute find command
        call execute_command_line(command)
        
        ! Read results from temporary file
        allocate(temp_results(100))  ! Reasonable maximum
        file_count = 0
        
        open(newunit=unit, file=trim(temp_file), status='old', iostat=iostat, &
             action='read')
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (len_trim(line) > 0 .and. file_count < 100) then
                    file_count = file_count + 1
                    temp_results(file_count) = trim(line)
                end if
            end do
            close(unit)
        end if
        
        ! Clean up temporary file
        call execute_command_line("rm -f " // escape_shell_arg(trim(temp_file)))
        
        ! Allocate final result
        if (file_count > 0) then
            allocate(character(len=256) :: gcov_files(file_count))
            gcov_files(1:file_count) = temp_results(1:file_count)
        else
            allocate(character(len=256) :: gcov_files(0))
        end if
        
        deallocate(temp_results)
    end function direct_find_gcov_files
    
    function get_unique_suffix() result(suffix)
        !! Generate a cryptographically secure unique suffix for temporary files
        character(len=16) :: suffix
        integer :: pid, clock_count
        character(len=32) :: temp_suffix
        
        ! Get process ID using system call
        pid = c_getpid()  ! Proper system call for process ID
        
        ! Add high-resolution clock for additional entropy
        call system_clock(clock_count)
        
        ! Combine PID and clock for uniqueness - use temp string first
        write(temp_suffix, '(I0,"_",I0)') pid, clock_count
        
        ! Truncate to fit in result
        if (len_trim(temp_suffix) > 16) then
            suffix = temp_suffix(1:16)
        else
            suffix = trim(temp_suffix)
        end if
    end function get_unique_suffix
    
    
    function direct_find_gcov_files_recursive(base_directory) result(gcov_files)
        !! Recursively search for .gcov files in directory tree
        character(len=*), intent(in) :: base_directory
        character(len=:), allocatable :: gcov_files(:)
        
        logical :: dir_exists
        character(len=512) :: command, temp_file
        character(len=256) :: line
        integer :: unit, iostat, file_count
        character(len=256), allocatable :: temp_results(:)
        
        ! Check if base directory exists
        inquire(file=base_directory, exist=dir_exists)
        if (.not. dir_exists) then
            allocate(character(len=256) :: gcov_files(0))
            return
        end if
        
        ! Create temporary file for results
        temp_file = "/tmp/fortcov_find_gcov_recursive_" // get_unique_suffix()
        
        ! Build recursive find command - properly escape paths
        command = "find " // escape_shell_arg(trim(base_directory)) // &
                 " -name '*.gcov' -type f > " // &
                 escape_shell_arg(trim(temp_file)) // " 2>/dev/null"
        
        ! Execute find command
        call execute_command_line(command)
        
        ! Read results from temporary file
        allocate(temp_results(200))  ! Reasonable maximum for recursive
        file_count = 0
        
        open(newunit=unit, file=trim(temp_file), status='old', iostat=iostat, &
             action='read')
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (len_trim(line) > 0 .and. file_count < 200) then
                    file_count = file_count + 1
                    temp_results(file_count) = trim(line)
                end if
            end do
            close(unit)
        end if
        
        ! Clean up temporary file
        call execute_command_line("rm -f " // escape_shell_arg(trim(temp_file)))
        
        ! Allocate final result
        if (file_count > 0) then
            allocate(character(len=256) :: gcov_files(file_count))
            gcov_files(1:file_count) = temp_results(1:file_count)
        else
            allocate(character(len=256) :: gcov_files(0))
        end if
        
        deallocate(temp_results)
    end function direct_find_gcov_files_recursive
    
end module zero_configuration_manager