module coverage_workflows_discovery
    !! Coverage file discovery and filtering workflows
    !!
    !! Handles coverage file discovery from various sources including
    !! auto-discovery, explicit file lists, and gcov-based discovery.
    !! Provides comprehensive filtering and validation capabilities.
    
    use constants_core
    use config_core
    use coverage_processor_gcov, only: discover_gcov_files
    use coverage_workflows_patterns, only: prepare_pattern_paths, check_include_patterns, &
                                           check_exclude_patterns, check_test_file_exclusion
    implicit none
    private
    
    public :: discover_coverage_files
    public :: filter_coverage_files_by_patterns
    public :: evaluate_exclude_patterns
    
contains

    function discover_coverage_files(config) result(files)
        !! Coverage file discovery implementation
        !! Extracted from original find_coverage_files function
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: files(:)
        
        character(len=:), allocatable :: all_files(:)
        character(len=:), allocatable :: filtered_files(:)
        
        ! Find coverage files based on configuration
        call determine_coverage_files_source(config, all_files)
        
        ! Apply filtering if needed
        call apply_coverage_file_filtering(all_files, config, filtered_files)
        
        ! Validate and limit files
        call validate_and_limit_files(filtered_files, config, files)
        
    end function discover_coverage_files
    
    function evaluate_exclude_patterns(filepath, config) result(should_exclude)
        !! Pattern evaluation implementation for both include and exclude patterns
        !! Enhanced from original check_exclude_patterns function
        character(len=*), intent(in) :: filepath
        type(config_t), intent(in) :: config
        logical :: should_exclude
        
        character(len=:), allocatable :: normalized_path, basename
        logical :: matches_include
        
        should_exclude = .false.
        
        ! Prepare normalized paths for pattern matching
        call prepare_pattern_paths(filepath, normalized_path, basename)
        
        ! Check include patterns first
        call check_include_patterns(normalized_path, basename, config, matches_include)
        if (.not. matches_include) then
            should_exclude = .true.
            return
        end if
        
        ! Check exclude patterns
        call check_exclude_patterns(normalized_path, basename, config, should_exclude)
        if (should_exclude) return
        
        ! Check test file exclusion patterns
        call check_test_file_exclusion(normalized_path, config, should_exclude)
        
    end function evaluate_exclude_patterns
    
    function filter_coverage_files_by_patterns(files, config) result(filtered)
        !! File filtering by patterns implementation
        !! Extracted from original filter_files_by_patterns function
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable :: filtered(:)
        
        character(len=:), allocatable :: temp_files(:)
        integer :: i, filtered_count
        logical :: should_exclude
        
        ! Count files that pass filtering
        filtered_count = 0
        do i = 1, size(files)
            should_exclude = evaluate_exclude_patterns(files(i), config)
            if (.not. should_exclude) then
                filtered_count = filtered_count + 1
            end if
        end do
        
        ! Allocate filtered array
        if (filtered_count > 0) then
            allocate(character(len=len(files(1))) :: filtered(filtered_count))
            
            ! Copy non-excluded files
            filtered_count = 0
            do i = 1, size(files)
                should_exclude = evaluate_exclude_patterns(files(i), config)
                if (.not. should_exclude) then
                    filtered_count = filtered_count + 1
                    filtered(filtered_count) = files(i)
                end if
            end do
        end if
        
    end function filter_coverage_files_by_patterns
    
    ! Internal helper routines
    
    subroutine determine_coverage_files_source(config, files)
        !! Determine source of coverage files based on configuration
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: files(:)
        
        if (allocated(config%coverage_files)) then
            ! Use explicitly specified coverage files
            files = config%coverage_files
        else
            ! Delegate to gcov processor for discovery (single supported path)
            call discover_gcov_files(config, files)
        end if
    end subroutine determine_coverage_files_source
    
    subroutine apply_coverage_file_filtering(all_files, config, filtered_files)
        !! Apply filtering to coverage files if needed
        !! Issue #884: Exclude patterns must NOT filter .gcov discovery
        !!             Filtering is applied later to source paths within
        !!             the parsed coverage data, not to the coverage files
        !!             themselves. Here we simply pass through discovered
        !!             coverage files unchanged.
        character(len=:), allocatable, intent(in) :: all_files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: filtered_files(:)

        if (.not. allocated(all_files)) then
            return
        end if

        if (size(all_files) == 0) then
            allocate(character(len=256) :: filtered_files(0))
        else
            allocate(character(len=len(all_files(1))) :: filtered_files(size(all_files)))
            filtered_files = all_files
        end if
    end subroutine apply_coverage_file_filtering
    
    subroutine validate_and_limit_files(input_files, config, output_files)
        !! Validate file existence and apply max_files limit
        character(len=:), allocatable, intent(in) :: input_files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: output_files(:)
        
        integer :: i, file_count
        logical :: file_exists
        
        if (.not. allocated(input_files)) then
            return
        end if
        
        output_files = input_files
        
        ! Count existing files
        file_count = 0
        do i = 1, size(output_files)
            inquire(file=trim(output_files(i)), exist=file_exists)
            if (file_exists) then
                file_count = file_count + 1
            end if
        end do
        
        if (file_count == 0) then
            deallocate(output_files)
        else if (file_count > config%max_files) then
            call report_file_count_limit(config, file_count)
            call resize_file_array(output_files, config%max_files)
        end if
    end subroutine validate_and_limit_files
    
    subroutine report_file_count_limit(config, file_count)
        !! Report file count limitation
        type(config_t), intent(in) :: config
        integer, intent(in) :: file_count
        
        if (.not. config%quiet) then
            print *, "Limiting coverage files from", file_count, "to", config%max_files
        end if
    end subroutine report_file_count_limit
    
    subroutine resize_file_array(files, new_size)
        !! Resizes file array to specified size (truncates if necessary)
        character(len=:), allocatable, intent(inout) :: files(:)
        integer, intent(in) :: new_size
        
        character(len=:), allocatable :: temp_files(:)
        integer :: i, copy_size
        
        if (.not. allocated(files)) return
        if (new_size <= 0) then
            deallocate(files)
            return
        end if
        
        copy_size = min(size(files), new_size)
        allocate(character(len=len(files(1))) :: temp_files(new_size))
        
        do i = 1, copy_size
            temp_files(i) = files(i)
        end do
        
        call move_alloc(temp_files, files)
        
    end subroutine resize_file_array

end module coverage_workflows_discovery
