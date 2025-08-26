module zero_config_auto_discovery_integration
    use zero_config_core
    implicit none
    private
    
    ! Re-export procedures for backward compatibility
    public :: enhance_zero_config_with_auto_discovery
    public :: configure_auto_discovery_defaults
    public :: integrate_build_system_detection
    public :: setup_auto_test_execution
    public :: provide_zero_config_user_feedback
    public :: execute_zero_config_complete_workflow

end module zero_config_auto_discovery_integration