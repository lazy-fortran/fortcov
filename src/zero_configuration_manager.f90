module zero_configuration_manager
    !! Zero-Configuration Manager (Issue #204)
    !! 
    !! PLACEHOLDER MODULE - Ready for GREEN phase implementation
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
    
    ! Public interfaces for zero-configuration functionality
    public :: is_zero_configuration_mode
    public :: apply_zero_configuration_defaults
    public :: auto_discover_coverage_files_priority
    public :: auto_discover_source_files_priority
    public :: ensure_output_directory_structure
    public :: show_zero_configuration_error_guidance
    
contains
    
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
        !! Auto-discover coverage files using priority-ordered search
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        integer :: i
        
        ! Priority 1: Check build/gcov/*.gcov (Issue #203 standard location)
        inquire(file="build/gcov", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("build/gcov", "*.gcov")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                coverage_files = temp_files
                return
            end if
        end if
        
        ! Priority 2: Check current directory *.gcov
        temp_files = find_files_with_glob(".", "*.gcov")
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            coverage_files = temp_files
            return
        end if
        
        ! Priority 3: Check build directory recursively (if exists)
        inquire(file="build", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("build", "*.gcov")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                coverage_files = temp_files
                return
            end if
        end if
        
        ! No coverage files found
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
    
end module zero_configuration_manager