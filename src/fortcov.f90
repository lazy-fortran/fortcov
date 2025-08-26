module fortcov
  use fortcov_config, only: config_t, parse_config, show_help, show_version
  use coverage_engine, only: analyze_coverage, EXIT_SUCCESS, EXIT_FAILURE, &
                             EXIT_THRESHOLD_NOT_MET, EXIT_NO_COVERAGE_DATA
  implicit none
  private

  public :: run_coverage_analysis
  public :: config_t, parse_config, show_help, show_version
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
