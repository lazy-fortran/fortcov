module theme_manager
    implicit none
    private
    
    ! Public types
    public :: theme_manager_t, color_scheme_t
    
    ! Color scheme type for theme functionality
    type :: color_scheme_t
        character(len=32) :: name = "default"
        character(len=32) :: primary_color = "#0066cc"
        character(len=32) :: secondary_color = "#333333"
        character(len=32) :: background_color = "#ffffff"
        character(len=32) :: text_color = "#000000"
        character(len=32) :: accent_color = "#ff6600"
    end type color_scheme_t
    
    ! Theme manager type with complete theme management functionality
    type :: theme_manager_t
        type(color_scheme_t), private :: current_theme
        logical, private :: initialized = .false.
    contains
        procedure :: init => theme_manager_init
        procedure :: load_theme => theme_manager_load_theme
        procedure :: load_cyberpunk_theme => theme_manager_load_cyberpunk_theme
        procedure :: load_default_theme => theme_manager_load_default_theme
        procedure :: load_high_contrast_theme => theme_manager_load_high_contrast_theme
        procedure :: get_current_theme => theme_manager_get_current_theme
        procedure :: generate_css_variables => theme_manager_generate_css_variables
        procedure :: is_theme_available => theme_manager_is_theme_available
    end type theme_manager_t

contains

    ! Initialize theme manager with default theme
    subroutine theme_manager_init(this)
        class(theme_manager_t), intent(inout) :: this
        logical :: success
        character(len=256) :: error_msg
        
        ! Load default theme on initialization
        call this%load_default_theme(this%current_theme, success, error_msg)
        this%initialized = success
    end subroutine theme_manager_init
    
    ! Load theme by name
    subroutine theme_manager_load_theme(this, theme_name, theme, success, error_msg)
        class(theme_manager_t), intent(inout) :: this
        character(len=*), intent(in) :: theme_name
        type(color_scheme_t), intent(out) :: theme
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        
        select case (trim(adjustl(theme_name)))
        case ("default")
            call this%load_default_theme(theme, success, error_msg)
        case ("cyberpunk")
            call this%load_cyberpunk_theme(theme, success, error_msg)
        case ("high-contrast")
            call this%load_high_contrast_theme(theme, success, error_msg)
        case default
            success = .false.
            error_msg = "Unknown theme: " // trim(theme_name)
            return
        end select
        
        if (success) then
            this%current_theme = theme
        end if
    end subroutine theme_manager_load_theme
    
    ! Load default theme
    subroutine theme_manager_load_default_theme(this, theme, success, error_msg)
        class(theme_manager_t), intent(inout) :: this
        type(color_scheme_t), intent(out) :: theme
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        
        theme%name = "default"
        theme%primary_color = "#0066cc"
        theme%secondary_color = "#333333"
        theme%background_color = "#ffffff"
        theme%text_color = "#000000"
        theme%accent_color = "#ff6600"
        
        success = .true.
        error_msg = ""
    end subroutine theme_manager_load_default_theme

    ! Load cyberpunk theme
    subroutine theme_manager_load_cyberpunk_theme(this, theme, success, error_msg)
        class(theme_manager_t), intent(inout) :: this
        type(color_scheme_t), intent(out) :: theme
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        
        theme%name = "cyberpunk"
        theme%primary_color = "#00ffff"
        theme%secondary_color = "#ff00ff"
        theme%background_color = "#000011"
        theme%text_color = "#00ff00"
        theme%accent_color = "#ffff00"
        
        success = .true.
        error_msg = ""
    end subroutine theme_manager_load_cyberpunk_theme
    
    ! Load high contrast theme for accessibility
    subroutine theme_manager_load_high_contrast_theme(this, theme, success, error_msg)
        class(theme_manager_t), intent(inout) :: this
        type(color_scheme_t), intent(out) :: theme
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        
        theme%name = "high-contrast"
        theme%primary_color = "#ffffff"
        theme%secondary_color = "#000000"
        theme%background_color = "#000000"
        theme%text_color = "#ffffff"
        theme%accent_color = "#ffff00"
        
        success = .true.
        error_msg = ""
    end subroutine theme_manager_load_high_contrast_theme
    
    ! Get current active theme
    function theme_manager_get_current_theme(this) result(theme)
        class(theme_manager_t), intent(in) :: this
        type(color_scheme_t) :: theme
        
        theme = this%current_theme
    end function theme_manager_get_current_theme
    
    ! Check if a theme is available
    function theme_manager_is_theme_available(this, theme_name) result(available)
        class(theme_manager_t), intent(in) :: this
        character(len=*), intent(in) :: theme_name
        logical :: available
        
        select case (trim(adjustl(theme_name)))
        case ("default", "cyberpunk", "high-contrast")
            available = .true.
        case default
            available = .false.
        end select
    end function theme_manager_is_theme_available
    
    ! Generate CSS variables from theme
    subroutine theme_manager_generate_css_variables(this, theme, css_variables)
        class(theme_manager_t), intent(inout) :: this
        type(color_scheme_t), intent(in) :: theme
        character(len=:), allocatable, intent(out) :: css_variables
        
        ! Generate comprehensive CSS variables
        css_variables = ":root { " // &
            "--primary-color: " // trim(theme%primary_color) // "; " // &
            "--secondary-color: " // trim(theme%secondary_color) // "; " // &
            "--background-color: " // trim(theme%background_color) // "; " // &
            "--text-color: " // trim(theme%text_color) // "; " // &
            "--accent-color: " // trim(theme%accent_color) // "; " // &
            "--theme-name: '" // trim(theme%name) // "'; }"
    end subroutine theme_manager_generate_css_variables

end module theme_manager
