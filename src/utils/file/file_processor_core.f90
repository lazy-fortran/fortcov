module file_processor_core
    !! File processing utilities extracted from zero_configuration_manager
    !! 
    !! This module provides file processing, directory management, and user
    !! guidance functionality for zero-configuration operations.
    !!
    !! Responsibilities:
    !! - Output directory structure creation
    !! - Zero-configuration error guidance display
    !! - Configuration default application
    use file_utils_core, only: ensure_directory
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, ERROR_FILE_ACCESS
    implicit none
    private
    
    public :: ensure_output_directory_structure
    public :: show_zero_configuration_error_guidance
    public :: apply_zero_configuration_defaults

contains

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
    
    subroutine apply_zero_configuration_defaults(output_path, output_format, &
                                                input_format, exclude_patterns)
        !! Apply smart defaults for zero-configuration mode
        character(len=:), allocatable, intent(out) :: output_path
        character(len=:), allocatable, intent(out) :: output_format
        character(len=:), allocatable, intent(out) :: input_format
        character(len=:), allocatable, intent(out) :: exclude_patterns(:)
        
        integer :: stat
        character(len=512) :: errmsg
        
        ! Set smart defaults for zero-configuration mode
        output_path = "coverage.md"
        output_format = "markdown"
        input_format = "gcov"
        
        ! Default exclusion patterns for common build and test directories
        allocate(character(len=32) :: exclude_patterns(2), &
            stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Memory allocation failed for exclude_patterns: " // &
                trim(errmsg)
            return
        end if
        exclude_patterns(1) = "build/*"
        exclude_patterns(2) = "test/*"
        
    end subroutine apply_zero_configuration_defaults

end module file_processor_core