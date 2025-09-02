module file_discovery_core
    !! File Discovery and Source Determination
    !! 
    !! Handles discovery of coverage files from different sources.
    !! Extracted from coverage_workflows.f90 for SRP compliance (Issue #718).
    use config_core
    use coverage_processor_gcov
    implicit none
    private
    
    public :: discover_coverage_files, determine_coverage_files_source
    public :: apply_coverage_file_filtering, validate_and_limit_files

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
        !! CRITICAL: Exclude patterns should NOT filter coverage files in auto-discovery mode
        !! They should only filter source files within the coverage data
        use pattern_filtering_core, only: filter_coverage_files_by_patterns
        character(len=:), allocatable, intent(in) :: all_files(:)
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: filtered_files(:)
        
        ! Handle empty arrays
        if (.not. allocated(all_files) .or. size(all_files) == 0) then
            allocate(character(len=256) :: filtered_files(0))
            return
        end if
        
        ! CRITICAL: Exclude patterns should NEVER filter coverage files
        ! They should only filter source files within the coverage data
        ! Coverage files are discovered by auto-discovery or explicit specification
        ! Exclude patterns are for source file filtering only
        filtered_files = all_files
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
            print *, "⚠️  Limiting coverage files from", file_count, "to", config%max_files
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
        allocate(character(len=len(files)) :: temp_files(new_size))
        
        do i = 1, copy_size
            temp_files(i) = files(i)
        end do
        
        call move_alloc(temp_files, files)
        
    end subroutine resize_file_array

end module file_discovery_core
