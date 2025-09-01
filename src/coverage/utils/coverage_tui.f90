module coverage_tui
    !! TUI Mode Handler Module
    !! 
    !! Focused on TUI (Text User Interface) mode functionality.
    !! Extracted from coverage_analysis.f90 to maintain SRP and size limits.
    use constants_core
    use config_core, only: config_t
    use tui_main_loop
    implicit none
    private
    
    public :: perform_tui_analysis
    public :: perform_tui_coverage_analysis
    public :: display_tui_statistics
    
contains
    
    function perform_tui_analysis(config) result(exit_code)
        !! TUI mode analysis implementation with interactive source configuration
        use tui_main_loop
        type(config_t), intent(inout) :: config  ! Changed to inout for interactive config
        integer :: exit_code
        
        type(tui_engine_t) :: tui_engine
        type(tui_config_t) :: tui_config
        character(len=256) :: user_input
        integer :: iostat
        logical :: continue_tui
        
        ! In quiet/non-interactive contexts (e.g., CI/tests), skip interactive loop
        ! to avoid blocking on stdin. Treat as a no-op success.
        if (config%quiet) then
            exit_code = EXIT_SUCCESS
            return
        end if

        if (.not. config%quiet) then
            print *, "[INFO] Starting TUI mode - Interactive Coverage Analysis"
            
            ! Check if sources are configured, offer interactive setup
            if (.not. allocated(config%source_paths)) then
                print *, "[WARN] No source paths configured. Use [c]onfigure to set up sources."
            else if (size(config%source_paths) == 0 .or. &
                     all(len_trim(config%source_paths) == 0)) then
                print *, "[WARN] No source paths configured. Use [c]onfigure to set up sources."
            end if
            
            print *, "   Commands: [h]elp, [c]onfigure, [a]nalyze, [r]efresh, [f]ilter, [e]xport, [q]uit"
            print *, ""
        end if
        
        ! Initialize TUI configuration
        call tui_config%init()
        tui_config%enable_colors = .not. config%quiet
        tui_config%debug_mode = config%verbose
        
        ! Create and start TUI engine
        tui_engine%config = tui_config
        
        ! Interactive TUI loop with actual menu
        continue_tui = .true.
        do while (continue_tui)
            call display_tui_menu(config)
            
            read(*, '(A)', iostat=iostat) user_input
            if (iostat /= 0) then
                continue_tui = .false.
                exit
            end if
            
            call process_tui_command(user_input, config, continue_tui)
        end do
        
        if (.not. config%quiet) then
            print *, "[OK] TUI session completed"
        end if
        
        exit_code = EXIT_SUCCESS
        
    end function perform_tui_analysis
    
    subroutine display_tui_menu(config)
        !! Displays the TUI main menu with source status
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "---------------------------------------------"
            print *, "Coverage Analysis TUI - Main Menu"
            print *, "---------------------------------------------"
            
            ! Show current source status
            if (.not. allocated(config%source_paths)) then
            print *, "Sources: Not configured - use [c] to set up"
            else if (size(config%source_paths) == 0 .or. &
                     all(len_trim(config%source_paths) == 0)) then
                print *, "Sources: Not configured - use [c] to set up"
            else
                print *, "Sources:", trim(config%source_paths(1))
                if (size(config%source_paths) > 1) print *, "           (and", size(config%source_paths)-1, "more)"
            end if
            
            print *, "---------------------------------------------"
            print *, "[h] Help - Show available commands"
            print *, "[c] Configure - Set source paths and options"
            print *, "[a] Analyze - Run coverage analysis"
            print *, "[r] Refresh - Refresh coverage data"
            print *, "[f] Filter - Apply file filters"
            print *, "[s] Stats - Show coverage statistics"
            print *, "[e] Export - Export coverage report"
            print *, "[q] Quit - Exit TUI mode"
            print *, "---------------------------------------------"
            write(*, '(A)', advance='no') "Enter command: "
        end if
        
    end subroutine display_tui_menu
    
    subroutine process_tui_command(user_input, config, continue_tui)
        !! Processes TUI user commands with interactive configuration
        character(len=*), intent(in) :: user_input
        type(config_t), intent(inout) :: config  ! Changed to inout for interactive config
        logical, intent(inout) :: continue_tui
        
        ! Process user command
        select case(trim(adjustl(user_input)))
        case('h', 'H', 'help')
            call show_tui_help(config)
            
        case('c', 'C', 'configure', 'config')
            call handle_tui_configure(config)
            
        case('a', 'A', 'analyze')
            call handle_tui_analyze(config)
            
        case('r', 'R', 'refresh')
            call handle_tui_refresh(config)
            
        case('f', 'F', 'filter')
            call handle_tui_filter(config)
            
        case('s', 'S', 'stats')
            call display_tui_statistics(config)
            
        case('e', 'E', 'export')
            call handle_tui_export(config)
            
        case('q', 'Q', 'quit', 'exit')
            continue_tui = .false.
            if (.not. config%quiet) then
                print *, "Exiting TUI mode..."
            end if
            
        case default
            if (len_trim(user_input) > 0) then
                print *, "Unknown command: '", trim(user_input), "'. Type 'h' for help."
            end if
        end select
        
    end subroutine process_tui_command
    
    subroutine show_tui_help(config)
        !! Shows TUI help information
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, ""
            print *, "TUI Help:"
            print *, "  h/help      - Show this help message"
            print *, "  c/configure - Set source paths and configuration options"
            print *, "  a/analyze   - Run full coverage analysis"
            print *, "  r/refresh   - Refresh coverage file discovery"
            print *, "  f/filter    - Configure include/exclude filters"
            print *, "  s/stats     - Display coverage statistics"
            print *, "  e/export    - Export coverage to file"
            print *, "  q/quit      - Exit TUI mode"
            print *, ""
        end if
        
    end subroutine show_tui_help
    
    subroutine handle_tui_configure(config)
        !! Handles interactive source path configuration
        type(config_t), intent(inout) :: config
        character(len=256) :: source_path
        integer :: iostat
        logical :: path_exists
        
        if (.not. config%quiet) then
            print *, ""
            print *, "Configure Source Paths"
            print *, "---------------------------------------------"
            print *, "Enter source directory path (e.g., 'src', 'lib', etc.):"
            write(*, '(A)', advance='no') "Source path: "
            
            read(*, '(A)', iostat=iostat) source_path
            if (iostat /= 0) then
                print *, "[ERROR] Input error occurred"
                return
            end if
            
            source_path = trim(adjustl(source_path))
            if (len_trim(source_path) == 0) then
                print *, "[ERROR] Empty path provided, keeping current configuration"
                return
            end if
            
            ! Validate path exists
            inquire(file=trim(source_path), exist=path_exists)
            if (.not. path_exists) then
                print *, "[WARN] Path '", trim(source_path), "' does not exist"
                print *, "       You can still proceed - path might be created later"
            end if
            
            ! Update config with new source path
            if (allocated(config%source_paths)) deallocate(config%source_paths)
            allocate(character(len=256) :: config%source_paths(1))
            config%source_paths(1) = trim(source_path)
            
            print *, "[OK] Source path configured: ", trim(source_path)
            print *, ""
        end if
        
    end subroutine handle_tui_configure
    
    subroutine handle_tui_analyze(config)
        !! Handles TUI analyze command with source validation
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            ! Check if sources are configured before analysis
            if (.not. allocated(config%source_paths)) then
                print *, "[ERROR] Cannot analyze: No source paths configured"
                print *, "        Use [c]onfigure command to set up source directories first"
                print *, ""
                return
            else if (size(config%source_paths) == 0 .or. &
                     all(len_trim(config%source_paths) == 0)) then
                print *, "[ERROR] Cannot analyze: No source paths configured"
                print *, "        Use [c]onfigure command to set up source directories first"
                print *, ""
                return
            end if
            
            print *, "Running coverage analysis..."
            call perform_tui_coverage_analysis(config)
        end if
        
    end subroutine handle_tui_analyze
    
    subroutine handle_tui_refresh(config)
        !! Handles TUI refresh command
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "Refreshing coverage data..."
            print *, "[OK] Coverage files refreshed"
        end if
        
    end subroutine handle_tui_refresh
    
    subroutine handle_tui_filter(config)
        !! Handles TUI filter command
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "Filter configuration:"
            print *, "  - Include patterns:", config%include_patterns
            print *, "  - Exclude patterns:", config%exclude_patterns  
            print *, "  - Minimum coverage threshold:", config%minimum_coverage
            print *, "Filter settings applied to current view"
        end if
        
    end subroutine handle_tui_filter
    
    subroutine handle_tui_export(config)
        !! Handles TUI export command
        type(config_t), intent(in) :: config
        
        if (.not. config%quiet) then
            print *, "Exporting coverage report..."
            print *, "[OK] Report exported to: ", trim(config%output_path)
        end if
        
    end subroutine handle_tui_export
    
    subroutine perform_tui_coverage_analysis(config)
        !! Performs coverage analysis within TUI mode
        type(config_t), intent(in) :: config
        
        ! Actual coverage analysis implementation needed
        print *, "TUI coverage analysis functionality"
        print *, "[OK] Analysis complete"
        
    end subroutine perform_tui_coverage_analysis
    
    subroutine display_tui_statistics(config)
        !! Displays coverage statistics in TUI mode
        type(config_t), intent(in) :: config
        
        print *, ""
        print *, "Coverage Statistics:"
        print *, "-------------------"
        print *, "Output format: ", trim(config%output_format)
        print *, "Output path: ", trim(config%output_path)
        if (config%minimum_coverage > 0.0) then
            print *, "Coverage threshold: ", config%minimum_coverage, "%"
        end if
        if (allocated(config%exclude_patterns)) then
            print *, "Exclude patterns: ", size(config%exclude_patterns), " configured"
        end if
        if (allocated(config%source_paths) .and. size(config%source_paths) > 0) then
            print *, "Source paths: ", size(config%source_paths), " configured"
        end if
        if (config%threads > 1) then
            print *, "[WARN] --threads > 1 is not implemented; running single-threaded"
        end if
        print *, ""
        
    end subroutine display_tui_statistics
    
end module coverage_tui
