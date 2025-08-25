module terminal_ui_manager
    !! Terminal UI Management
    !!
    !! Handles terminal display and interactive TUI operations.
    !! Extracted from report_engine_impl.f90 for SRP compliance.
    use iso_fortran_env, only: real64
    use data_transformer
    use theme_manager
    use tui_main_loop
    use report_configuration
    use string_utilities, only: int_to_str, real_to_str
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
    subroutine start_interactive_tui(session, display_content, success, error_msg)
        type(terminal_session_t), intent(inout) :: session
        character(len=*), intent(in) :: display_content
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg

        type(tui_engine_t) :: tui
        type(tui_config_t) :: tui_config

        success = .false.
        error_msg = ""

        ! Configure TUI engine
        call tui_config%init()
        tui_config%enable_colors = session%colors_enabled
        tui_config%input_timeout_ms = 50  ! 20 FPS for responsive UI
        tui_config%max_iterations = 5000  ! Safety limit
        tui_config%debug_mode = .false.

        ! Initialize TUI engine
        call tui%init(tui_config)

        ! Set display content
        tui%display_buffer = display_content

        ! Display initial content
        if (session%colors_enabled) then
            print *, char(27)//'[2J'//char(27)//'[H'  ! Clear screen and home cursor
        end if
        print *, trim(display_content)
        print *, ""
        print *, "Interactive TUI Mode - Press Ctrl+C to exit"
        print *, "Frame rate limited to prevent system issues"

        ! Run the TUI main loop with proper controls
        call tui%run_main_loop(.true., 30.0_real64)  ! Interactive, 30 second max

        ! Clean up
        if (session%colors_enabled) then
            print *, char(27)//'[0m'  ! Reset terminal colors
        end if

        call tui%cleanup()
        success = .true.
    end subroutine start_interactive_tui

end module terminal_ui_manager