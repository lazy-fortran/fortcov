module test_build_gcov_auto_discovery
    !! Test Build and Gcov Auto-Discovery Integration Module (Issue #277)
    !!
    !! This module provides a unified interface to the auto-discovery ecosystem
    !! by integrating the specialized modules:
    !! - test_build_auto_discovery: Test build detection and configuration
    !! - gcov_auto_processor: Secure gcov file processing and execution  
    !! - auto_discovery_utilities: Complete workflow orchestration
    !!
    !! This integration module maintains backward compatibility while providing
    !! the benefits of modular, focused implementation.

    use test_build_auto_discovery, only: test_build_result_t, &
                                        auto_discover_test_build
    use gcov_auto_processor, only: gcov_result_t, source_mapping_t, &
                                  auto_process_gcov_files
    use auto_discovery_utilities, only: complete_workflow_result_t, &
                                       execute_complete_auto_workflow
    implicit none
    private

    ! Re-export public interface for backward compatibility
    public :: auto_discover_test_build
    public :: auto_process_gcov_files  
    public :: execute_complete_auto_workflow

    ! Re-export result types for backward compatibility
    public :: test_build_result_t
    public :: gcov_result_t
    public :: source_mapping_t
    public :: complete_workflow_result_t

contains

    ! No additional implementation needed - all functions are re-exported
    ! from the specialized modules for backward compatibility

end module test_build_gcov_auto_discovery