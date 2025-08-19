program test
use fortcov_config
type(config_t) :: config
character(len=6) :: args(1) = ["--help"]
character(len=256) :: error_msg
logical :: success
call parse_config(args, config, success, error_msg)
print *, "success=", success, "show_help=", config%show_help
end
