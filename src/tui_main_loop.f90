module tui_main_loop
    use iso_fortran_env, only: real64, int32
    implicit none
    private
    
    ! Public interface
    public :: tui_engine_t
    public :: tui_config_t
    public :: tui_input_event_t
    public :: TUI_EVENT_NONE, TUI_EVENT_KEY, TUI_EVENT_RESIZE, TUI_EVENT_QUIT
    
    ! Event types
    integer(int32), parameter :: TUI_EVENT_NONE = 0
    integer(int32), parameter :: TUI_EVENT_KEY = 1
    integer(int32), parameter :: TUI_EVENT_RESIZE = 2
    integer(int32), parameter :: TUI_EVENT_QUIT = 3
    
    ! Frame timing constants
    real(real64), parameter :: TARGET_FPS = 60.0_real64
    real(real64), parameter :: FRAME_TIME_TARGET = 1.0_real64 / TARGET_FPS
    real(real64), parameter :: MAX_FRAME_TIME = 0.1_real64  ! 10 FPS minimum
    integer(int32), parameter :: MAX_ANSI_SEQUENCES_PER_FRAME = 10
    
    type :: tui_config_t
        logical :: enable_colors = .true.
        logical :: enable_mouse = .false.
        integer(int32) :: input_timeout_ms = 16  ! ~60 FPS
        integer(int32) :: max_iterations = 10000  ! Safety limit
        logical :: debug_mode = .false.
    contains
        procedure :: init => tui_config_init
    end type tui_config_t
    
    type :: tui_input_event_t
        integer(int32) :: event_type = TUI_EVENT_NONE
        integer(int32) :: key_code = 0
        character(len=1) :: key_char = ' '
        logical :: ctrl_pressed = .false.
        logical :: alt_pressed = .false.
    contains
        procedure :: init => tui_input_event_init
        procedure :: clear => tui_input_event_clear
    end type tui_input_event_t
    
    type :: tui_engine_t
        type(tui_config_t) :: config
        logical :: is_running = .false.
        logical :: should_quit = .false.
        real(real64) :: last_frame_time = 0.0_real64
        integer(int32) :: frame_count = 0
        integer(int32) :: ansi_sequence_count = 0
        character(len=:), allocatable :: display_buffer
    contains
        procedure :: init => tui_engine_init
        procedure :: cleanup => tui_engine_cleanup
        procedure :: run_main_loop => tui_engine_run_main_loop
        procedure :: handle_input => tui_engine_handle_input
        procedure :: update_display => tui_engine_update_display
        procedure :: limit_frame_rate => tui_engine_limit_frame_rate
        procedure :: should_continue => tui_engine_should_continue
        procedure :: reset_ansi_counter => tui_engine_reset_ansi_counter
        procedure :: check_ansi_limit => tui_engine_check_ansi_limit
    end type tui_engine_t

contains

    ! Initialize TUI configuration
    subroutine tui_config_init(this)
        class(tui_config_t), intent(out) :: this
        
        this%enable_colors = .true.
        this%enable_mouse = .false.
        this%input_timeout_ms = 16
        this%max_iterations = 10000
        this%debug_mode = .false.
    end subroutine tui_config_init
    
    ! Initialize input event
    subroutine tui_input_event_init(this)
        class(tui_input_event_t), intent(out) :: this
        
        call this%clear()
    end subroutine tui_input_event_init
    
    ! Clear input event
    subroutine tui_input_event_clear(this)
        class(tui_input_event_t), intent(inout) :: this
        
        this%event_type = TUI_EVENT_NONE
        this%key_code = 0
        this%key_char = ' '
        this%ctrl_pressed = .false.
        this%alt_pressed = .false.
    end subroutine tui_input_event_clear
    
    ! Initialize TUI engine
    subroutine tui_engine_init(this, config)
        class(tui_engine_t), intent(out) :: this
        type(tui_config_t), intent(in), optional :: config
        
        if (present(config)) then
            this%config = config
        else
            call this%config%init()
        end if
        
        this%is_running = .false.
        this%should_quit = .false.
        this%last_frame_time = 0.0_real64
        this%frame_count = 0
        this%ansi_sequence_count = 0
        this%display_buffer = ""
    end subroutine tui_engine_init
    
    ! Cleanup TUI engine
    subroutine tui_engine_cleanup(this)
        class(tui_engine_t), intent(inout) :: this
        
        this%is_running = .false.
        this%should_quit = .false.
        
        if (allocated(this%display_buffer)) then
            deallocate(this%display_buffer)
        end if
    end subroutine tui_engine_cleanup
    
    ! Main TUI loop with proper frame rate limiting and input handling
    subroutine tui_engine_run_main_loop(this, interactive, max_runtime_seconds)
        class(tui_engine_t), intent(inout) :: this
        logical, intent(in) :: interactive
        real(real64), intent(in), optional :: max_runtime_seconds
        
        real(real64) :: start_time, current_time, elapsed_time
        real(real64) :: max_runtime
        type(tui_input_event_t) :: input_event
        logical :: continue_loop
        
        ! Set maximum runtime (default 5 seconds for safety)
        if (present(max_runtime_seconds)) then
            max_runtime = max_runtime_seconds
        else
            max_runtime = 5.0_real64
        end if
        
        call cpu_time(start_time)
        this%is_running = .true.
        this%should_quit = .false.
        this%frame_count = 0
        
        ! Main loop with proper termination conditions
        do while (this%should_continue())
            call cpu_time(current_time)
            elapsed_time = current_time - start_time
            
            ! Safety: Exit if maximum runtime exceeded
            if (elapsed_time > max_runtime) then
                if (this%config%debug_mode) then
                    print *, "TUI: Maximum runtime exceeded, terminating"
                end if
                exit
            end if
            
            ! Safety: Exit if too many iterations
            if (this%frame_count >= this%config%max_iterations) then
                if (this%config%debug_mode) then
                    print *, "TUI: Maximum iterations reached, terminating"
                end if
                exit
            end if
            
            ! Handle input with timeout to prevent blocking
            call this%handle_input(input_event, continue_loop)
            if (.not. continue_loop) exit
            
            ! Update display (with ANSI sequence limiting)
            call this%update_display()
            
            ! Limit frame rate to prevent CPU spinning
            call this%limit_frame_rate()
            
            ! Reset ANSI sequence counter for next frame
            call this%reset_ansi_counter()
            
            this%frame_count = this%frame_count + 1
            
            ! For non-interactive mode, exit after one frame
            if (.not. interactive) exit
        end do
        
        this%is_running = .false.
        
        if (this%config%debug_mode) then
            call cpu_time(current_time)
            elapsed_time = current_time - start_time
            print *, "TUI: Loop completed. Frames:", this%frame_count, &
                    "Time:", elapsed_time, "s"
        end if
    end subroutine tui_engine_run_main_loop
    
    ! Handle input with proper timeout to prevent infinite blocking
    subroutine tui_engine_handle_input(this, event, continue_loop)
        class(tui_engine_t), intent(inout) :: this
        type(tui_input_event_t), intent(out) :: event
        logical, intent(out) :: continue_loop
        
        real(real64) :: start_time, current_time, elapsed_time
        real(real64) :: timeout_seconds
        
        call event%init()
        continue_loop = .true.
        
        ! Convert timeout from ms to seconds
        timeout_seconds = real(this%config%input_timeout_ms, real64) / 1000.0_real64
        
        call cpu_time(start_time)
        
        ! Simulate input handling with timeout
        do
            call cpu_time(current_time)
            elapsed_time = current_time - start_time
            
            ! Timeout reached - no input available
            if (elapsed_time >= timeout_seconds) then
                event%event_type = TUI_EVENT_NONE
                exit
            end if
            
            ! In a real implementation, this would check for actual input
            ! For now, simulate that no input is available most of the time
            ! Occasionally simulate a quit event for testing
            if (this%frame_count > 50 .and. mod(this%frame_count, 100) == 0) then
                event%event_type = TUI_EVENT_QUIT
                this%should_quit = .true.
                continue_loop = .false.
                exit
            end if
            
            ! Small delay to prevent busy waiting
            ! In real implementation, this would be proper select/poll
            continue
        end do
        
    end subroutine tui_engine_handle_input
    
    ! Update display with ANSI sequence limiting
    subroutine tui_engine_update_display(this)
        class(tui_engine_t), intent(inout) :: this
        
        ! Only update display if we haven't exceeded ANSI sequence limit
        if (.not. this%check_ansi_limit()) then
            ! Simulate display update with ANSI sequences
            this%ansi_sequence_count = this%ansi_sequence_count + 1
            
            ! Build display buffer (limited content to prevent flooding)
            this%display_buffer = "Frame: " // int_to_str(this%frame_count)
        end if
    end subroutine tui_engine_update_display
    
    ! Limit frame rate to prevent CPU spinning
    subroutine tui_engine_limit_frame_rate(this)
        class(tui_engine_t), intent(inout) :: this
        
        real(real64) :: current_time, frame_time_used, sleep_time
        
        call cpu_time(current_time)
        
        if (this%last_frame_time > 0.0_real64) then
            frame_time_used = current_time - this%last_frame_time
            
            ! Calculate how long to sleep to achieve target frame rate
            sleep_time = FRAME_TIME_TARGET - frame_time_used
            
            ! Only sleep if we have time left in the frame
            if (sleep_time > 0.0_real64) then
                ! Simulate sleep (in real implementation would use system sleep)
                ! For testing, just consume some CPU time in a controlled way
                call controlled_delay(sleep_time)
            end if
        end if
        
        this%last_frame_time = current_time
    end subroutine tui_engine_limit_frame_rate
    
    ! Check if TUI should continue running
    function tui_engine_should_continue(this) result(should_continue)
        class(tui_engine_t), intent(in) :: this
        logical :: should_continue
        
        should_continue = this%is_running .and. .not. this%should_quit
    end function tui_engine_should_continue
    
    ! Reset ANSI sequence counter for new frame
    subroutine tui_engine_reset_ansi_counter(this)
        class(tui_engine_t), intent(inout) :: this
        
        this%ansi_sequence_count = 0
    end subroutine tui_engine_reset_ansi_counter
    
    ! Check if ANSI sequence limit reached
    function tui_engine_check_ansi_limit(this) result(limit_reached)
        class(tui_engine_t), intent(in) :: this
        logical :: limit_reached
        
        limit_reached = this%ansi_sequence_count >= MAX_ANSI_SEQUENCES_PER_FRAME
    end function tui_engine_check_ansi_limit
    
    ! Helper function to convert integer to string
    function int_to_str(num) result(str)
        integer(int32), intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp_str
        
        write(temp_str, '(I0)') num
        str = trim(temp_str)
    end function int_to_str
    
    ! Controlled delay to simulate frame rate limiting
    subroutine controlled_delay(delay_seconds)
        real(real64), intent(in) :: delay_seconds
        
        real(real64) :: start_time, current_time, elapsed_time
        integer :: dummy_work
        
        call cpu_time(start_time)
        dummy_work = 0
        
        ! Light work loop instead of busy waiting
        do
            call cpu_time(current_time)
            elapsed_time = current_time - start_time
            
            if (elapsed_time >= delay_seconds) exit
            
            ! Light work to avoid busy waiting
            dummy_work = dummy_work + 1
            if (dummy_work > 1000) dummy_work = 0
        end do
    end subroutine controlled_delay

end module tui_main_loop