module coverage_testing_helpers
    !! Helper utilities for coverage testing
    use file_ops_secure, only: safe_remove_file
    use error_handling_core, only: error_context_t
    implicit none
    private
    public :: delete_marker_if_exists

contains

    subroutine delete_marker_if_exists()
        type(error_context_t) :: e
        call safe_remove_file('.fortcov_execution_marker', e)
    end subroutine delete_marker_if_exists

end module coverage_testing_helpers

