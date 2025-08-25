module report_configuration
    !! Report Configuration Management
    !!
    !! Handles all configuration types and initialization for the report engine.
    !! Extracted from report_engine_impl.f90 for SRP compliance.
    implicit none
    private

    ! Public types
    public :: report_config_t
    public :: terminal_session_t
    public :: filter_criteria_t

    type :: report_config_t
        character(len=:), allocatable :: output_format
        character(len=:), allocatable :: theme_name
        logical :: enable_syntax_highlighting
        logical :: enable_coverage_annotation
        logical :: terminal_colors_enabled
        integer :: max_memory_mb
        real :: startup_timeout_seconds
    contains
        procedure :: init => report_config_init
    end type report_config_t

    type :: terminal_session_t
        logical :: is_active = .false.
        logical :: colors_enabled = .false.
        integer :: terminal_width = 80
        integer :: terminal_height = 24
        character(len=:), allocatable :: display_buffer
    contains
        procedure :: init => terminal_session_init
        procedure :: cleanup => terminal_session_cleanup
    end type terminal_session_t

    type :: filter_criteria_t
        real :: min_coverage_threshold = 0.0
        character(len=:), allocatable :: include_patterns(:)
        character(len=:), allocatable :: exclude_patterns(:)
        logical :: show_only_uncovered = .false.
    contains
        procedure :: init => filter_criteria_init
    end type filter_criteria_t

contains

    ! Initialize report configuration
    subroutine report_config_init(this)
        class(report_config_t), intent(out) :: this

        this%output_format = "html"
        this%theme_name = "cyberpunk"
        this%enable_syntax_highlighting = .true.
        this%enable_coverage_annotation = .true.
        this%terminal_colors_enabled = .true.
        this%max_memory_mb = 100
        this%startup_timeout_seconds = 2.0
    end subroutine report_config_init

    ! Initialize terminal session
    subroutine terminal_session_init(this)
        class(terminal_session_t), intent(out) :: this

        this%is_active = .false.
        this%colors_enabled = .false.
        this%terminal_width = 80
        this%terminal_height = 24
        this%display_buffer = ""
    end subroutine terminal_session_init

    ! Cleanup terminal session
    subroutine terminal_session_cleanup(this)
        class(terminal_session_t), intent(inout) :: this

        this%is_active = .false.
        this%colors_enabled = .false.
        if (allocated(this%display_buffer)) deallocate(this%display_buffer)
    end subroutine terminal_session_cleanup

    ! Initialize filter criteria
    subroutine filter_criteria_init(this)
        class(filter_criteria_t), intent(out) :: this

        this%min_coverage_threshold = 0.0
        this%show_only_uncovered = .false.

        ! Allocate empty arrays
        allocate(character(len=100) :: this%include_patterns(0))
        allocate(character(len=100) :: this%exclude_patterns(0))
    end subroutine filter_criteria_init

end module report_configuration