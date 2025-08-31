module config_merger
    !! Configuration merging utilities
    use config_core, only: config_t
    implicit none
    private
    
    public :: merge_config_with_cli_priority
    public :: initialize_file_config_defaults
    
contains

    subroutine merge_config_with_cli_priority(cli_config, file_config)
        !! Merge file config into CLI config (CLI values take priority)
        type(config_t), intent(inout) :: cli_config
        type(config_t), intent(in) :: file_config
        
        ! Only update unset string values (allocated check means CLI didn't set)
        if (.not. allocated(cli_config%source_paths) .and. allocated(file_config%source_paths)) then
            cli_config%source_paths = file_config%source_paths
        end if
        
        if (.not. allocated(cli_config%output_path) .and. allocated(file_config%output_path)) then
            cli_config%output_path = file_config%output_path
        end if
        
        ! SECURITY FIX Issue #963: gcov_executable removed - shell injection vulnerability
        
        ! Update numeric values if not explicitly set (using sentinel values)
        if (cli_config%minimum_coverage < 0.0 .and. file_config%minimum_coverage >= 0.0) then
            cli_config%minimum_coverage = file_config%minimum_coverage
        end if
        
        if (cli_config%threads <= 0 .and. file_config%threads > 0) then
            cli_config%threads = file_config%threads
        end if
        
        ! Boolean flags - only update if file config explicitly sets them
        if (file_config%verbose) cli_config%verbose = .true.
        
        ! Merge arrays (append file values to CLI values)
        call merge_arrays(cli_config%exclude_patterns, file_config%exclude_patterns)
        call merge_arrays(cli_config%include_patterns, file_config%include_patterns)
        
    end subroutine merge_config_with_cli_priority
    
    subroutine initialize_file_config_defaults(config)
        !! Initialize config with default/sentinel values for merging
        type(config_t), intent(out) :: config
        
        ! Negative values indicate unset
        config%minimum_coverage = -1.0
        config%fail_under_threshold = -1.0
        config%threads = -1
        
        ! Booleans default to false
        config%verbose = .false.
        config%quiet = .false.
        config%show_help = .false.
        config%show_version = .false.
        config%validate_config_only = .false.
        config%enable_diff = .false.
        
    end subroutine initialize_file_config_defaults
    
    subroutine merge_arrays(target_array, source_array)
        !! Merge source array into target array (append)
        character(len=:), allocatable, intent(inout) :: target_array(:)
        character(len=:), allocatable, intent(in) :: source_array(:)
        
        character(len=:), allocatable :: temp_array(:)
        integer :: target_size, source_size, i
        
        if (.not. allocated(source_array)) return
        if (size(source_array) == 0) return
        
        if (.not. allocated(target_array)) then
            ! No target array, just copy source
            allocate(target_array, source=source_array)
        else
            ! Merge arrays
            target_size = size(target_array)
            source_size = size(source_array)
            
            allocate(character(len=256) :: temp_array(target_size + source_size))
            
            ! Copy existing values
            do i = 1, target_size
                temp_array(i) = target_array(i)
            end do
            
            ! Append new values
            do i = 1, source_size
                temp_array(target_size + i) = source_array(i)
            end do
            
            ! Replace target with merged array
            call move_alloc(temp_array, target_array)
        end if
        
    end subroutine merge_arrays

end module config_merger