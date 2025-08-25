module fortcov
  use fortcov_config, only: config_t, parse_config, show_help, show_version
  use coverage_engine, only: analyze_coverage, EXIT_SUCCESS, EXIT_FAILURE, &
                             EXIT_THRESHOLD_NOT_MET, EXIT_NO_COVERAGE_DATA
  use gcov_command_executor, only: gcov_executor_t
  use atomic_temp_file_manager, only: secure_temp_file_t
  use system_diff_converter
  use unicode_secure_validator
  use command_timeout_manager
  use error_handling
  use file_utils
  use input_validation
  use data_transformer
  use theme_manager
  use tui_main_loop
  use coverage_discovery
  use coverage_diff
  use report_engine
  use secure_command_executor, only: safe_execute_gcov
  use secure_file_operations, only: safe_find_files, safe_mkdir
  use path_validation, only: validate_path_security, validate_executable_path
  use shell_utils, only: escape_shell_argument
  implicit none
  private

  public :: run_coverage_analysis
  public :: config_t, parse_config, show_help, show_version
  public :: gcov_executor_t
  public :: secure_temp_file_t
  public :: EXIT_SUCCESS, EXIT_FAILURE, EXIT_THRESHOLD_NOT_MET, &
           EXIT_NO_COVERAGE_DATA
contains
  
  ! Main coverage analysis routine for CLI
  function run_coverage_analysis(config) result(exit_code)
    type(config_t), intent(inout) :: config
    integer :: exit_code
    
    exit_code = analyze_coverage(config)
  end function run_coverage_analysis
  
end module fortcov
