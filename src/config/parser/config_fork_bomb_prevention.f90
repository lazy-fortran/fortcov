module config_fork_bomb_prevention
    !! Fork bomb prevention logic extracted from config_parser_command_line
    !! 
    !! Focused on detecting manual coverage file specification and disabling
    !! auto-test execution to prevent infinite recursion.
    use config_types, only: config_t
    implicit none
    private
    
    public :: prevent_fork_bomb_with_manual_files
    
contains

    subroutine prevent_fork_bomb_with_manual_files(config)
        !! Prevent fork bomb by disabling auto-test execution when manual coverage files are provided
        !! 
        !! Issue #395: When gcov files are provided as arguments, auto-test execution can cause
        !! infinite recursion if the test suite itself calls fortcov. This function detects
        !! manual coverage file specification and disables auto-test execution to prevent the fork bomb.
        
        type(config_t), intent(inout) :: config
        logical :: has_manual_coverage_files
        
        has_manual_coverage_files = .false.
        
        ! Check for manually specified coverage files (positional arguments)
        if (allocated(config%coverage_files) .and. size(config%coverage_files) > 0) then
            has_manual_coverage_files = .true.
        end if
        
        ! Check for import file specification
        if (allocated(config%import_file) .and. len_trim(config%import_file) > 0) then
            has_manual_coverage_files = .true.
        end if
        
        ! Check for manually specified source paths (also indicates manual mode)
        if (allocated(config%source_paths) .and. size(config%source_paths) > 0) then
            has_manual_coverage_files = .true.
        end if
        
        ! If manual coverage files detected, disable auto-test execution to prevent fork bomb
        if (has_manual_coverage_files) then
            config%auto_test_execution = .false.
        end if
        
    end subroutine prevent_fork_bomb_with_manual_files

end module config_fork_bomb_prevention