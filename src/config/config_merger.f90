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
        if (.not. allocated(cli_config%source_dir) .and. allocated(file_config%source_dir)) then
            cli_config%source_dir = file_config%source_dir
        end if
        
        if (.not. allocated(cli_config%build_dir) .and. allocated(file_config%build_dir)) then
            cli_config%build_dir = file_config%build_dir
        end if
        
        if (.not. allocated(cli_config%output_dir) .and. allocated(file_config%output_dir)) then
            cli_config%output_dir = file_config%output_dir
        end if
        
        if (.not. allocated(cli_config%test_dir) .and. allocated(file_config%test_dir)) then
            cli_config%test_dir = file_config%test_dir
        end if
        
        ! Update numeric values if not explicitly set (using sentinel values)
        if (cli_config%coverage_threshold < 0.0 .and. file_config%coverage_threshold >= 0.0) then
            cli_config%coverage_threshold = file_config%coverage_threshold
        end if
        
        if (cli_config%max_processes <= 0 .and. file_config%max_processes > 0) then
            cli_config%max_processes = file_config%max_processes
        end if
        
        ! Boolean flags - only update if file config explicitly sets them
        if (file_config%generate_html) cli_config%generate_html = .true.
        if (file_config%verbose) cli_config%verbose = .true.
        if (file_config%parallel_execution) cli_config%parallel_execution = .true.
        
        ! Merge arrays (append file values to CLI values)
        call merge_arrays(cli_config%exclude_patterns, file_config%exclude_patterns)
        call merge_arrays(cli_config%include_patterns, file_config%include_patterns)
        
    end subroutine merge_config_with_cli_priority
    
    subroutine initialize_file_config_defaults(config)
        !! Initialize config with default/sentinel values for merging
        type(config_t), intent(out) :: config
        
        ! Negative values indicate unset
        config%coverage_threshold = -1.0
        config%max_processes = -1
        
        ! Booleans default to false
        config%generate_html = .false.
        config%verbose = .false.
        config%parallel_execution = .false.
        
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