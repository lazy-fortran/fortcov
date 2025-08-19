module config_storage
    !! Configuration Storage and Management (Decomposed from fortcov_config.f90)
    !! 
    !! Focused on configuration storage, help display, and utility functions.
    !! Separated from parsing and validation for better separation of concerns.
    use foundation_constants
    use foundation_layer_utils
    use config_parser, only: config_t
    implicit none
    private
    
    public :: show_help_information
    public :: show_version_information
    public :: display_config_summary
    public :: add_to_string_array
    public :: merge_configurations
    public :: export_config_to_file
    
contains
    
    subroutine show_help_information()
        !! Displays comprehensive help information
        !! Extracted from original show_help function
        
        print *, "fortcov - Fortran Coverage Analysis Tool"
        print *, ""
        print *, "USAGE:"
        print *, "    fortcov [OPTIONS] [COVERAGE_FILES...]"
        print *, ""
        print *, "OPTIONS:"
        print *, "    -h, --help              Show this help message"
        print *, "    -V, --version           Show version information"
        print *, "    -v, --verbose           Enable verbose output"
        print *, "    -q, --quiet             Suppress output messages"
        print *, ""
        print *, "OUTPUT OPTIONS:"
        print *, "    -f, --format FORMAT     Output format [markdown|json|xml|html]"
        print *, "    -o, --output PATH       Output file path"
        print *, ""
        print *, "ANALYSIS OPTIONS:"
        print *, "    -t, --threshold PCT     Coverage threshold percentage (0-100)"
        print *, "    -s, --source PATHS      Source directory paths (comma-separated)"
        print *, "    -e, --exclude PATTERNS  Exclude patterns (comma-separated)"
        print *, ""
        print *, "ADVANCED OPTIONS:"
        print *, "    --config FILE           Configuration file path"
        print *, "    --tui                   Launch Terminal User Interface"
        print *, "    --strict                Enable strict mode validation"
        print *, "    --keep-gcov             Keep generated .gcov files"
        print *, ""
        print *, "DIFF ANALYSIS:"
        print *, "    --diff                  Enable coverage diff analysis"
        print *, "    --diff-baseline FILE    Baseline coverage file for diff"
        print *, "    --diff-current FILE     Current coverage file for diff"
        print *, "    --include-unchanged     Include unchanged files in diff"
        print *, ""
        print *, "IMPORT/EXPORT:"
        print *, "    --import FILE           Import coverage data from JSON file"
        print *, ""
        print *, "GCOV OPTIONS:"
        print *, "    --gcov-executable PATH  Custom gcov executable path"
        print *, "    --gcov-args ARGS        Additional gcov arguments"
        print *, ""
        print *, "EXAMPLES:"
        print *, "    fortcov *.gcov                    # Analyze all .gcov files"
        print *, "    fortcov --format=json --output=coverage.json"
        print *, "    fortcov --threshold=80 --source=src/"
        print *, "    fortcov --diff --diff-baseline=old.json --diff-current=new.json"
        print *, "    fortcov --import=coverage.json --format=html"
        print *, "    fortcov --tui                     # Launch interactive mode"
        print *, ""
        print *, "For more information, visit: https://github.com/fortran-lang/fortcov"
        
    end subroutine show_help_information
    
    subroutine show_version_information()
        !! Displays version and build information
        !! Extracted from original show_version function
        
        print *, "fortcov version 0.4.0"
        print *, "Fortran Coverage Analysis Tool"
        print *, ""
        print *, "Built with:"
        print *, "  - Fortran compiler support"
        print *, "  - gcov integration"
        print *, "  - JSON/XML/HTML output formats"
        print *, "  - Terminal User Interface (TUI)"
        print *, "  - Coverage diff analysis"
        print *, ""
        print *, "Copyright (c) 2024 Fortran Language Community"
        print *, "Licensed under MIT License"
        print *, ""
        print *, "Report bugs at: https://github.com/fortran-lang/fortcov/issues"
        
    end subroutine show_version_information
    
    subroutine display_config_summary(config)
        !! Displays configuration summary for debugging
        type(config_t), intent(in) :: config
        
        print *, "=== Configuration Summary ==="
        print *, "Input format:     " // trim(config%input_format)
        print *, "Output format:    " // trim(config%output_format)
        
        if (allocated(config%output_path)) then
            print *, "Output path:      " // trim(config%output_path)
        end if
        
        write(*, '(A, F6.2, A)') "Threshold:        ", config%minimum_coverage, "%"
        
        if (config%verbose) print *, "Verbose:          enabled"
        if (config%quiet) print *, "Quiet:            enabled"
        if (config%tui_mode) print *, "TUI mode:         enabled"
        if (config%strict_mode) print *, "Strict mode:      enabled"
        if (config%keep_gcov_files) print *, "Keep gcov files:  enabled"
        
        if (allocated(config%source_paths)) then
            print *, "Source paths:     " // join_string_array(config%source_paths, ", ")
        end if
        
        if (allocated(config%exclude_patterns)) then
            print *, "Exclude patterns: " // join_string_array(config%exclude_patterns, ", ")
        end if
        
        if (allocated(config%coverage_files)) then
            print *, "Coverage files:   " // join_string_array(config%coverage_files, ", ")
        end if
        
        if (config%enable_diff) then
            print *, "Diff mode:        enabled"
            if (allocated(config%diff_baseline_file)) then
                print *, "  Baseline:       " // trim(config%diff_baseline_file)
            end if
            if (allocated(config%diff_current_file)) then
                print *, "  Current:        " // trim(config%diff_current_file)
            end if
        end if
        
        if (allocated(config%import_file)) then
            print *, "Import file:      " // trim(config%import_file)
        end if
        
        print *, "============================="
        
    end subroutine display_config_summary
    
    subroutine add_to_string_array(value, array, type_name, success, error_message)
        !! Adds a value to a string array, reallocating as needed
        !! Extracted from original add_to_array function
        character(len=*), intent(in) :: value
        character(len=:), allocatable, intent(inout) :: array(:)
        character(len=*), intent(in) :: type_name
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        character(len=:), allocatable :: temp_array(:)
        integer :: current_size, new_size
        
        success = .true.
        error_message = ""
        
        if (allocated(array)) then
            current_size = size(array)
            
            ! Check size limits
            if (current_size >= MAX_ARRAY_SIZE) then
                success = .false.
                write(error_message, '(A, I0, A)') &
                    "Maximum " // trim(type_name) // " count exceeded (", MAX_ARRAY_SIZE, ")"
                return
            end if
            
            ! Reallocate with increased size
            new_size = current_size + 1
            allocate(character(len=len(array)) :: temp_array(new_size))
            temp_array(1:current_size) = array
            temp_array(new_size) = value
            call move_alloc(temp_array, array)
        else
            ! Initial allocation
            allocate(character(len=len(value)) :: array(1))
            array(1) = value
        end if
        
    end subroutine add_to_string_array
    
    subroutine merge_configurations(base_config, override_config, merged_config)
        !! Merges two configurations with override taking precedence
        type(config_t), intent(in) :: base_config
        type(config_t), intent(in) :: override_config
        type(config_t), intent(out) :: merged_config
        
        ! Start with base configuration
        merged_config = base_config
        
        ! Override with non-default values from override_config
        if (allocated(override_config%input_format)) then
            merged_config%input_format = override_config%input_format
        end if
        
        if (allocated(override_config%output_format)) then
            merged_config%output_format = override_config%output_format
        end if
        
        if (allocated(override_config%output_path)) then
            merged_config%output_path = override_config%output_path
        end if
        
        if (allocated(override_config%source_paths)) then
            merged_config%source_paths = override_config%source_paths
        end if
        
        if (allocated(override_config%exclude_patterns)) then
            merged_config%exclude_patterns = override_config%exclude_patterns
        end if
        
        if (allocated(override_config%coverage_files)) then
            merged_config%coverage_files = override_config%coverage_files
        end if
        
        if (allocated(override_config%gcov_executable)) then
            merged_config%gcov_executable = override_config%gcov_executable
        end if
        
        if (override_config%minimum_coverage /= base_config%minimum_coverage) then
            merged_config%minimum_coverage = override_config%minimum_coverage
        end if
        
        ! Boolean flags - override takes precedence
        if (override_config%verbose .neqv. base_config%verbose) then
            merged_config%verbose = override_config%verbose
        end if
        
        if (override_config%quiet .neqv. base_config%quiet) then
            merged_config%quiet = override_config%quiet
        end if
        
        if (override_config%tui_mode .neqv. base_config%tui_mode) then
            merged_config%tui_mode = override_config%tui_mode
        end if
        
        if (override_config%strict_mode .neqv. base_config%strict_mode) then
            merged_config%strict_mode = override_config%strict_mode
        end if
        
    end subroutine merge_configurations
    
    subroutine export_config_to_file(config, file_path, success, error_message)
        !! Exports configuration to a file
        type(config_t), intent(in) :: config
        character(len=*), intent(in) :: file_path
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        integer :: unit, iostat
        
        success = .true.
        error_message = ""
        
        open(newunit=unit, file=file_path, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            success = .false.
            error_message = "Failed to create config file: " // trim(file_path)
            return
        end if
        
        ! Write configuration in key=value format
        write(unit, '(A)') "# fortcov configuration file"
        write(unit, '(A)') "# Generated automatically"
        write(unit, '(A)') ""
        
        write(unit, '(A)') "output_format=" // trim(config%output_format)
        
        if (allocated(config%output_path)) then
            write(unit, '(A)') "output_path=" // trim(config%output_path)
        end if
        
        write(unit, '(A, F6.2)') "threshold=", config%minimum_coverage
        
        if (config%verbose) then
            write(unit, '(A)') "verbose=true"
        end if
        
        if (config%quiet) then
            write(unit, '(A)') "quiet=true"
        end if
        
        if (config%tui_mode) then
            write(unit, '(A)') "tui_mode=true"
        end if
        
        if (config%strict_mode) then
            write(unit, '(A)') "strict_mode=true"
        end if
        
        close(unit)
        
    end subroutine export_config_to_file
    
    function join_string_array(array, separator) result(joined)
        !! Joins string array elements with separator
        character(len=*), intent(in) :: array(:)
        character(len=*), intent(in) :: separator
        character(len=:), allocatable :: joined
        
        integer :: i, total_length
        
        if (size(array) == 0) then
            joined = ""
            return
        end if
        
        ! Calculate total length needed
        total_length = len(array(1))
        do i = 2, size(array)
            total_length = total_length + len(separator) + len(array(i))
        end do
        
        ! Allocate and build joined string
        allocate(character(len=total_length) :: joined)
        joined = array(1)
        
        do i = 2, size(array)
            joined = joined // separator // array(i)
        end do
        
    end function join_string_array
    
end module config_storage