module tui_manager_core
    !! Terminal UI Management
    !!
    !! Handles terminal display and interactive TUI operations.
    !! Extracted from report_engine_impl.f90 for SRP compliance.
    use iso_fortran_env, only: real64
    use data_transformer_core
    use data_transformer_types
    use theme_manager_core
    use tui_main_loop
    use report_config_core
    use string_utils, only: int_to_str, real_to_str
    implicit none
    private

    ! Public procedures
    public :: generate_terminal_display
    public :: start_interactive_tui

contains

    ! Generate terminal-friendly display output
    subroutine generate_terminal_display(data, theme, output)
        type(transformed_data_t), intent(in) :: data
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable, intent(out) :: output

        output = char(27)//'[1;32m' // 'Coverage Report' // char(27)//'[0m' // &
                new_line('a') // &
                '================================' // new_line('a') // &
                'Theme: ' // theme%name // new_line('a') // &
                'Files: ' // int_to_str(data%summary%total_files) // new_line('a') // &
                'Coverage: ' // real_to_str(data%summary%coverage_percentage) // &
                '%' // new_line('a')
    end subroutine generate_terminal_display

    ! Start interactive TUI with proper main loop
    subroutine start_interactive_tui(session, display_content, success, error_msg, &
                                     max_runtime_seconds)
        type(terminal_session_t), intent(inout) :: session
        character(len=*), intent(in) :: display_content
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        real(real64), intent(in), optional :: max_runtime_seconds

        type(tui_engine_t) :: tui
        type(tui_config_t) :: tui_config
        logical :: tui_initialized
        integer :: ios
        logical :: terminal_ok

        success = .false.
        error_msg = ""
        tui_initialized = .false.
        terminal_ok = .true.

        ! Verify session state before proceeding
        if (.not. session%is_active) then
            error_msg = "TUI session is not active; aborting startup"
            return
        end if

        ! Configure TUI engine
        call tui_config%init()
        tui_config%enable_colors = session%colors_enabled
        tui_config%input_timeout_ms = 50  ! 20 FPS for responsive UI
        tui_config%max_iterations = 5000  ! Safety limit
        tui_config%debug_mode = .false.

        ! Initialize TUI engine
        call tui%init(tui_config)
        tui_initialized = .true.

        ! Set display content
        tui%display_buffer = display_content

        ! Display initial content
        if (session%colors_enabled) then
            write(*,'(A)',iostat=ios) char(27)//'[2J'//char(27)//'[H'
            if (ios /= 0) then
                terminal_ok = .false.
            end if
        end if

        ! Always attempt to print primary content; fail gracefully on error
        write(*,'(A)',iostat=ios) trim(display_content)
        if (ios /= 0) then
            error_msg = "Terminal output failed; unable to render TUI content"
            goto 100
        end if

        write(*,'(A)',iostat=ios) ""
        write(*,'(A)',iostat=ios) "Interactive TUI Mode - Press Ctrl+C to exit"
        write(*,'(A)',iostat=ios) "Frame rate limited to prevent system issues"

        ! Run the TUI main loop with proper controls
        if (present(max_runtime_seconds)) then
            call tui%run_main_loop(.true., max_runtime_seconds)
        else
            call tui%run_main_loop(.true.)  ! Use engine default safety limit (5s)
        end if

        ! Clean up
100     continue
        if (session%colors_enabled .and. terminal_ok) then
            write(*,'(A)',iostat=ios) char(27)//'[0m'
        end if

        if (tui_initialized) then
            call tui%cleanup()
        end if

        success = (len_trim(error_msg) == 0)
    end subroutine start_interactive_tui

end module tui_manager_core
