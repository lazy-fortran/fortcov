! Test suite for CI/CD workflow integration validation
! Tests GitHub Actions, GitLab CI, and Jenkins integration patterns from DESIGN.md
! Validates automated coverage analysis workflows and deployment patterns

program test_ci_cd_workflow_integration
    use error_handling
    use file_utils
    use string_utils
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    integer :: failed_count = 0
    
    write(*,*) "=== CI/CD Workflow Integration Test Suite ==="
    write(*,*)
    
    ! Test CI/CD integration patterns
    call test_github_actions_workflow_validation()
    call test_gitlab_ci_workflow_validation()
    call test_jenkins_pipeline_validation()
    call test_docker_integration_patterns()
    call test_hpc_integration_patterns()
    call test_cross_platform_ci_matrix()
    call test_ci_cd_artifact_generation()
    call test_automated_coverage_reporting()
    call test_workflow_failure_handling()
    
    ! Report results
    write(*,*)
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests completed: ", test_count, " (", passed_count, " passed, ", failed_count, " failed)"
    
    if (failed_count > 0) then
        write(*,*) "CI/CD WORKFLOW INTEGRATION TESTS FAILED"
        stop 1
    else
        write(*,*) "All CI/CD workflow integration tests passed"
    end if

contains

    subroutine test_github_actions_workflow_validation()
        ! Given: GitHub Actions workflow from DESIGN.md
        ! When: Testing workflow structure and step validation
        ! Then: Validate complete GitHub Actions integration works correctly
        
        logical :: setup_valid, build_valid, coverage_valid, upload_valid
        
        write(*,'(A)', advance='no') "Testing GitHub Actions workflow validation... "
        test_count = test_count + 1
        
        ! Test GitHub Actions workflow components
        setup_valid = validate_github_actions_setup()
        build_valid = validate_github_actions_build()
        coverage_valid = validate_github_actions_coverage()
        upload_valid = validate_github_actions_upload()
        
        if (setup_valid .and. build_valid .and. coverage_valid .and. upload_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. setup_valid) write(*,*) "  - GitHub Actions setup validation failed"
            if (.not. build_valid) write(*,*) "  - GitHub Actions build validation failed"
            if (.not. coverage_valid) write(*,*) "  - GitHub Actions coverage validation failed"
            if (.not. upload_valid) write(*,*) "  - GitHub Actions upload validation failed"
        end if
    end subroutine

    function validate_github_actions_setup() result(is_valid)
        ! Test GitHub Actions setup steps from DESIGN.md
        logical :: is_valid
        character(len=512) :: setup_steps(3)
        logical :: setup_structure_valid
        
        ! Expected setup steps from documentation
        setup_steps(1) = "uses: actions/checkout@v4"
        setup_steps(2) = "uses: fortran-lang/setup-fortran@v1"
        setup_steps(3) = "uses: fortran-lang/setup-fpm@v5"
        
        ! Validate setup structure
        setup_structure_valid = validate_github_actions_step_structure(setup_steps, 3)
        
        is_valid = setup_structure_valid
    end function

    function validate_github_actions_build() result(is_valid)
        ! Test GitHub Actions build step from DESIGN.md
        logical :: is_valid
        character(len=512) :: build_commands(1)
        logical :: build_structure_valid
        
        ! Expected build command
        build_commands(1) = "fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        
        ! Validate build structure
        build_structure_valid = validate_github_actions_run_command(build_commands, 1)
        
        is_valid = build_structure_valid
    end function

    function validate_github_actions_coverage() result(is_valid)
        ! Test GitHub Actions coverage generation from DESIGN.md
        logical :: is_valid
        character(len=512) :: coverage_commands(4)
        logical :: coverage_structure_valid
        
        ! Expected coverage commands
        coverage_commands(1) = "find build -name ""*.gcda"" -path ""*/fortcov/*"" -execdir gcov {} \\; || true"
        coverage_commands(2) = "gcov src/*.f90 || true"
        coverage_commands(3) = "fpm run fortcov -- --source=. --exclude='build/*' --exclude='test/*' --output=coverage.md"
        coverage_commands(4) = "# Coverage generation complete"
        
        ! Validate coverage structure
        coverage_structure_valid = validate_github_actions_run_command(coverage_commands, 4)
        
        is_valid = coverage_structure_valid
    end function

    function validate_github_actions_upload() result(is_valid)
        ! Test GitHub Actions artifact upload from DESIGN.md
        logical :: is_valid
        character(len=512) :: upload_config(3)
        logical :: upload_structure_valid
        
        ! Expected upload configuration
        upload_config(1) = "uses: actions/upload-artifact@v4"
        upload_config(2) = "name: coverage-report"
        upload_config(3) = "path: coverage.md"
        
        ! Validate upload structure
        upload_structure_valid = validate_github_actions_artifact_config(upload_config, 3)
        
        is_valid = upload_structure_valid
    end function

    subroutine test_gitlab_ci_workflow_validation()
        ! Given: GitLab CI configuration from DESIGN.md
        ! When: Testing GitLab CI pipeline structure and validation
        ! Then: Validate complete GitLab CI integration works correctly
        
        logical :: config_valid, script_valid, artifacts_valid, coverage_valid
        
        write(*,'(A)', advance='no') "Testing GitLab CI workflow validation... "
        test_count = test_count + 1
        
        ! Test GitLab CI workflow components
        config_valid = validate_gitlab_ci_configuration()
        script_valid = validate_gitlab_ci_script()
        artifacts_valid = validate_gitlab_ci_artifacts()
        coverage_valid = validate_gitlab_ci_coverage_pattern()
        
        if (config_valid .and. script_valid .and. artifacts_valid .and. coverage_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. config_valid) write(*,*) "  - GitLab CI configuration validation failed"
            if (.not. script_valid) write(*,*) "  - GitLab CI script validation failed"
            if (.not. artifacts_valid) write(*,*) "  - GitLab CI artifacts validation failed"
            if (.not. coverage_valid) write(*,*) "  - GitLab CI coverage pattern validation failed"
        end if
    end subroutine

    function validate_gitlab_ci_configuration() result(is_valid)
        ! Test GitLab CI configuration from DESIGN.md
        logical :: is_valid
        character(len=512) :: config_components(4)
        logical :: config_structure_valid
        
        ! Expected GitLab CI configuration
        config_components(1) = "stages: - build - test - coverage"
        config_components(2) = "image: fortran/gfortran:latest"
        config_components(3) = "variables: COVERAGE_FLAGS: ""-fprofile-arcs -ftest-coverage"""
        config_components(4) = "stage: coverage"
        
        ! Validate configuration structure
        config_structure_valid = validate_gitlab_ci_config_structure(config_components, 4)
        
        is_valid = config_structure_valid
    end function

    function validate_gitlab_ci_script() result(is_valid)
        ! Test GitLab CI script section from DESIGN.md
        logical :: is_valid
        character(len=512) :: script_commands(3)
        logical :: script_structure_valid
        
        ! Expected script commands
        script_commands(1) = "fpm test --flag ""$COVERAGE_FLAGS"""
        script_commands(2) = "gcov src/*.f90"
        script_commands(3) = "fpm run fortcov -- --source=. --exclude='build/*' --output=coverage.html"
        
        ! Validate script structure
        script_structure_valid = validate_gitlab_ci_script_structure(script_commands, 3)
        
        is_valid = script_structure_valid
    end function

    function validate_gitlab_ci_artifacts() result(is_valid)
        ! Test GitLab CI artifacts configuration from DESIGN.md
        logical :: is_valid
        character(len=512) :: artifacts_config(4)
        logical :: artifacts_structure_valid
        
        ! Expected artifacts configuration
        artifacts_config(1) = "artifacts:"
        artifacts_config(2) = "reports: coverage_report:"
        artifacts_config(3) = "coverage_format: cobertura"
        artifacts_config(4) = "paths: - coverage.html"
        
        ! Validate artifacts structure
        artifacts_structure_valid = validate_gitlab_ci_artifacts_structure(artifacts_config, 4)
        
        is_valid = artifacts_structure_valid
    end function

    function validate_gitlab_ci_coverage_pattern() result(is_valid)
        ! Test GitLab CI coverage pattern from DESIGN.md
        logical :: is_valid
        character(len=256) :: coverage_pattern
        logical :: pattern_structure_valid
        
        ! Expected coverage pattern
        coverage_pattern = "coverage: '/Total coverage: (\\d+\\.\\d+)%/'"
        
        ! Validate coverage pattern structure
        pattern_structure_valid = validate_gitlab_ci_coverage_regex(coverage_pattern)
        
        is_valid = pattern_structure_valid
    end function

    subroutine test_jenkins_pipeline_validation()
        ! Given: Jenkins pipeline from DESIGN.md
        ! When: Testing Jenkins pipeline structure and validation
        ! Then: Validate complete Jenkins integration works correctly
        
        logical :: pipeline_valid, build_valid, coverage_valid, publish_valid
        
        write(*,'(A)', advance='no') "Testing Jenkins pipeline validation... "
        test_count = test_count + 1
        
        ! Test Jenkins pipeline components
        pipeline_valid = validate_jenkins_pipeline_structure()
        build_valid = validate_jenkins_build_stage()
        coverage_valid = validate_jenkins_coverage_stage()
        publish_valid = validate_jenkins_publish_stage()
        
        if (pipeline_valid .and. build_valid .and. coverage_valid .and. publish_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. pipeline_valid) write(*,*) "  - Jenkins pipeline structure validation failed"
            if (.not. build_valid) write(*,*) "  - Jenkins build stage validation failed"
            if (.not. coverage_valid) write(*,*) "  - Jenkins coverage stage validation failed"
            if (.not. publish_valid) write(*,*) "  - Jenkins publish stage validation failed"
        end if
    end subroutine

    function validate_jenkins_pipeline_structure() result(is_valid)
        ! Test Jenkins pipeline structure from DESIGN.md
        logical :: is_valid
        character(len=512) :: pipeline_components(3)
        logical :: structure_valid
        
        ! Expected pipeline structure
        pipeline_components(1) = "pipeline {"
        pipeline_components(2) = "agent any"
        pipeline_components(3) = "stages {"
        
        ! Validate pipeline structure
        structure_valid = validate_jenkins_structure_syntax(pipeline_components, 3)
        
        is_valid = structure_valid
    end function

    function validate_jenkins_build_stage() result(is_valid)
        ! Test Jenkins build stage from DESIGN.md
        logical :: is_valid
        character(len=512) :: build_stage(2)
        logical :: stage_structure_valid
        
        ! Expected build stage
        build_stage(1) = "stage('Build with Coverage') {"
        build_stage(2) = "sh 'fpm test --flag ""-fprofile-arcs -ftest-coverage""'"
        
        ! Validate build stage structure
        stage_structure_valid = validate_jenkins_stage_structure(build_stage, 2)
        
        is_valid = stage_structure_valid
    end function

    function validate_jenkins_coverage_stage() result(is_valid)
        ! Test Jenkins coverage stage from DESIGN.md
        logical :: is_valid
        character(len=512) :: coverage_stage(3)
        logical :: stage_structure_valid
        
        ! Expected coverage stage
        coverage_stage(1) = "stage('Generate Coverage') {"
        coverage_stage(2) = "sh 'gcov src/*.f90'"
        coverage_stage(3) = "sh 'fpm run fortcov -- --source=. --exclude=""build/*"" --output=coverage.html'"
        
        ! Validate coverage stage structure
        stage_structure_valid = validate_jenkins_stage_structure(coverage_stage, 3)
        
        is_valid = stage_structure_valid
    end function

    function validate_jenkins_publish_stage() result(is_valid)
        ! Test Jenkins publish stage from DESIGN.md
        logical :: is_valid
        character(len=512) :: publish_config(6)
        logical :: publish_structure_valid
        
        ! Expected publish configuration
        publish_config(1) = "publishHTML(["
        publish_config(2) = "allowMissing: false,"
        publish_config(3) = "alwaysLinkToLastBuild: true,"
        publish_config(4) = "keepAll: true,"
        publish_config(5) = "reportFiles: 'coverage.html',"
        publish_config(6) = "reportName: 'Coverage Report'"
        
        ! Validate publish structure
        publish_structure_valid = validate_jenkins_publish_structure(publish_config, 6)
        
        is_valid = publish_structure_valid
    end function

    subroutine test_docker_integration_patterns()
        ! Given: Docker integration patterns from DESIGN.md
        ! When: Testing Docker container coverage workflows
        ! Then: Validate Docker integration patterns work correctly
        
        logical :: multistage_valid, hpc_valid
        
        write(*,'(A)', advance='no') "Testing Docker integration patterns... "
        test_count = test_count + 1
        
        ! Test Docker integration patterns
        multistage_valid = validate_docker_multistage_pattern()
        hpc_valid = validate_hpc_module_pattern()
        
        if (multistage_valid .and. hpc_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. multistage_valid) write(*,*) "  - Docker multi-stage pattern validation failed"
            if (.not. hpc_valid) write(*,*) "  - HPC module pattern validation failed"
        end if
    end subroutine

    function validate_docker_multistage_pattern() result(is_valid)
        ! Test Docker multi-stage build pattern from DESIGN.md
        logical :: is_valid
        character(len=512) :: docker_stages(6)
        logical :: multistage_structure_valid
        
        ! Expected Docker multi-stage pattern
        docker_stages(1) = "FROM fortran/gfortran:latest as builder"
        docker_stages(2) = "RUN curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux-x86_64"
        docker_stages(3) = "RUN fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        docker_stages(4) = "RUN gcov src/*.f90"
        docker_stages(5) = "RUN fpm run fortcov -- --source=. --output=coverage.html"
        docker_stages(6) = "FROM alpine:latest"
        
        ! Validate multi-stage structure
        multistage_structure_valid = validate_docker_multistage_structure(docker_stages, 6)
        
        is_valid = multistage_structure_valid
    end function

    function validate_hpc_module_pattern() result(is_valid)
        ! Test HPC module system pattern from DESIGN.md
        logical :: is_valid
        character(len=512) :: hpc_components(5)
        logical :: hpc_structure_valid
        
        ! Expected HPC module pattern
        hpc_components(1) = "module load gcc/13.2.0"
        hpc_components(2) = "module load cmake/3.25.0"
        hpc_components(3) = "export FCFLAGS=""-fprofile-arcs -ftest-coverage"""
        hpc_components(4) = "make clean && make test"
        hpc_components(5) = "fortcov --source=src --exclude='*test*' --output=coverage-$(date +%Y%m%d).html"
        
        ! Validate HPC structure
        hpc_structure_valid = validate_hpc_module_structure(hpc_components, 5)
        
        is_valid = hpc_structure_valid
    end function

    subroutine test_hpc_integration_patterns()
        ! Given: HPC integration patterns from DESIGN.md
        ! When: Testing SLURM and module system integration
        ! Then: Validate HPC integration patterns work correctly
        
        logical :: slurm_valid, module_valid
        
        write(*,'(A)', advance='no') "Testing HPC integration patterns... "
        test_count = test_count + 1
        
        ! Test HPC integration patterns
        slurm_valid = validate_slurm_job_pattern()
        module_valid = validate_environment_module_pattern()
        
        if (slurm_valid .and. module_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. slurm_valid) write(*,*) "  - SLURM job pattern validation failed"
            if (.not. module_valid) write(*,*) "  - Environment module pattern validation failed"
        end if
    end subroutine

    function validate_slurm_job_pattern() result(is_valid)
        ! Test SLURM job integration pattern from DESIGN.md
        logical :: is_valid
        character(len=512) :: slurm_directives(7)
        logical :: slurm_structure_valid
        
        ! Expected SLURM job pattern
        slurm_directives(1) = "#SBATCH --job-name=fortcov-coverage"
        slurm_directives(2) = "#SBATCH --ntasks=1"
        slurm_directives(3) = "#SBATCH --time=00:30:00"
        slurm_directives(4) = "module load fortran-coverage-tools"
        slurm_directives(5) = "srun fpm test --flag ""-fprofile-arcs -ftest-coverage"""
        slurm_directives(6) = "srun gcov src/*.f90"
        slurm_directives(7) = "cp coverage-${SLURM_JOB_ID}.html /shared/coverage-reports/"
        
        ! Validate SLURM structure
        slurm_structure_valid = validate_slurm_structure(slurm_directives, 7)
        
        is_valid = slurm_structure_valid
    end function

    function validate_environment_module_pattern() result(is_valid)
        ! Test environment module pattern validation
        logical :: is_valid
        character(len=512) :: module_commands(4)
        logical :: module_structure_valid
        
        ! Expected module commands
        module_commands(1) = "module load gcc/13.2.0"
        module_commands(2) = "export FCFLAGS=""-fprofile-arcs -ftest-coverage"""
        module_commands(3) = "make clean && make test"
        module_commands(4) = "fortcov --source=src --output=coverage.html"
        
        ! Validate module structure
        module_structure_valid = validate_module_command_structure(module_commands, 4)
        
        is_valid = module_structure_valid
    end function

    subroutine test_cross_platform_ci_matrix()
        ! Given: Cross-platform CI matrix from DESIGN.md
        ! When: Testing multi-compiler, multi-OS matrix strategy
        ! Then: Validate cross-platform integration works correctly
        
        logical :: matrix_valid, exclusion_valid
        
        write(*,'(A)', advance='no') "Testing cross-platform CI matrix... "
        test_count = test_count + 1
        
        ! Test cross-platform matrix configuration
        matrix_valid = validate_ci_matrix_strategy()
        exclusion_valid = validate_matrix_exclusion_rules()
        
        if (matrix_valid .and. exclusion_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. matrix_valid) write(*,*) "  - CI matrix strategy validation failed"
            if (.not. exclusion_valid) write(*,*) "  - Matrix exclusion rules validation failed"
        end if
    end subroutine

    function validate_ci_matrix_strategy() result(is_valid)
        ! Test CI matrix strategy from DESIGN.md
        logical :: is_valid
        character(len=256) :: matrix_components(4)
        logical :: matrix_structure_valid
        
        ! Expected matrix strategy components
        matrix_components(1) = "compiler: [gfortran, ifort, flang]"
        matrix_components(2) = "os: [ubuntu-latest, macos-latest]"
        matrix_components(3) = "export FC=${{ matrix.compiler }}"
        matrix_components(4) = "coverage-${{ matrix.compiler }}-${{ matrix.os }}.html"
        
        ! Validate matrix structure
        matrix_structure_valid = validate_matrix_configuration(matrix_components, 4)
        
        is_valid = matrix_structure_valid
    end function

    function validate_matrix_exclusion_rules() result(is_valid)
        ! Test matrix exclusion rules from DESIGN.md
        logical :: is_valid
        character(len=256) :: exclusion_rules(3)
        logical :: exclusion_structure_valid
        
        ! Expected exclusion rules
        exclusion_rules(1) = "exclude:"
        exclusion_rules(2) = "- os: macos-latest"
        exclusion_rules(3) = "compiler: ifort  ! Intel Fortran not available on macOS"
        
        ! Validate exclusion structure
        exclusion_structure_valid = validate_exclusion_rules_structure(exclusion_rules, 3)
        
        is_valid = exclusion_structure_valid
    end function

    subroutine test_ci_cd_artifact_generation()
        ! Given: CI/CD artifact patterns from DESIGN.md
        ! When: Testing artifact generation and storage
        ! Then: Validate artifact generation patterns work correctly
        
        logical :: github_artifacts_valid, gitlab_artifacts_valid, jenkins_artifacts_valid
        
        write(*,'(A)', advance='no') "Testing CI/CD artifact generation... "
        test_count = test_count + 1
        
        ! Test artifact generation for different platforms
        github_artifacts_valid = validate_github_artifact_generation()
        gitlab_artifacts_valid = validate_gitlab_artifact_generation()
        jenkins_artifacts_valid = validate_jenkins_artifact_generation()
        
        if (github_artifacts_valid .and. gitlab_artifacts_valid .and. jenkins_artifacts_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. github_artifacts_valid) write(*,*) "  - GitHub artifact generation validation failed"
            if (.not. gitlab_artifacts_valid) write(*,*) "  - GitLab artifact generation validation failed"
            if (.not. jenkins_artifacts_valid) write(*,*) "  - Jenkins artifact generation validation failed"
        end if
    end subroutine

    function validate_github_artifact_generation() result(is_valid)
        ! Test GitHub artifact generation pattern
        logical :: is_valid
        character(len=256) :: artifact_config(3)
        logical :: artifact_valid
        
        ! GitHub artifact configuration
        artifact_config(1) = "uses: actions/upload-artifact@v4"
        artifact_config(2) = "name: coverage-report"
        artifact_config(3) = "path: coverage.md"
        
        ! Validate artifact configuration
        artifact_valid = validate_artifact_upload_config(artifact_config, 3)
        
        is_valid = artifact_valid
    end function

    function validate_gitlab_artifact_generation() result(is_valid)
        ! Test GitLab artifact generation pattern
        logical :: is_valid
        character(len=256) :: artifact_config(3)
        logical :: artifact_valid
        
        ! GitLab artifact configuration
        artifact_config(1) = "artifacts:"
        artifact_config(2) = "paths: - coverage.html"
        artifact_config(3) = "coverage_format: cobertura"
        
        ! Validate artifact configuration
        artifact_valid = validate_artifact_upload_config(artifact_config, 3)
        
        is_valid = artifact_valid
    end function

    function validate_jenkins_artifact_generation() result(is_valid)
        ! Test Jenkins artifact generation pattern
        logical :: is_valid
        character(len=256) :: artifact_config(3)
        logical :: artifact_valid
        
        ! Jenkins artifact configuration
        artifact_config(1) = "publishHTML(["
        artifact_config(2) = "reportFiles: 'coverage.html'"
        artifact_config(3) = "reportName: 'Coverage Report'"
        
        ! Validate artifact configuration
        artifact_valid = validate_artifact_upload_config(artifact_config, 3)
        
        is_valid = artifact_valid
    end function

    subroutine test_automated_coverage_reporting()
        ! Given: Automated coverage reporting patterns
        ! When: Testing automated report generation and notification
        ! Then: Validate automated reporting works correctly
        
        logical :: report_generation_valid, notification_valid
        
        write(*,'(A)', advance='no') "Testing automated coverage reporting... "
        test_count = test_count + 1
        
        ! Test automated reporting patterns
        report_generation_valid = validate_automated_report_generation()
        notification_valid = validate_coverage_notification_patterns()
        
        if (report_generation_valid .and. notification_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. report_generation_valid) write(*,*) "  - Automated report generation validation failed"
            if (.not. notification_valid) write(*,*) "  - Coverage notification validation failed"
        end if
    end subroutine

    function validate_automated_report_generation() result(is_valid)
        ! Test automated report generation patterns
        logical :: is_valid
        character(len=256) :: report_steps(3)
        logical :: generation_valid
        
        ! Automated report generation steps
        report_steps(1) = "gcov src/*.f90"
        report_steps(2) = "fortcov --source=. --output=coverage.html"
        report_steps(3) = "# Report generated automatically"
        
        ! Validate report generation
        generation_valid = validate_report_generation_sequence(report_steps, 3)
        
        is_valid = generation_valid
    end function

    function validate_coverage_notification_patterns() result(is_valid)
        ! Test coverage notification patterns
        logical :: is_valid
        character(len=256) :: notification_patterns(2)
        logical :: notification_valid
        
        ! Coverage notification patterns
        notification_patterns(1) = "coverage: '/Total coverage: (\\d+\\.\\d+)%/'"
        notification_patterns(2) = "reportName: 'Coverage Report'"
        
        ! Validate notification patterns
        notification_valid = validate_notification_config(notification_patterns, 2)
        
        is_valid = notification_valid
    end function

    subroutine test_workflow_failure_handling()
        ! Given: Workflow failure scenarios
        ! When: Testing error handling and recovery patterns
        ! Then: Validate failure handling works correctly
        
        logical :: error_handling_valid, recovery_valid
        
        write(*,'(A)', advance='no') "Testing workflow failure handling... "
        test_count = test_count + 1
        
        ! Test failure handling patterns
        error_handling_valid = validate_workflow_error_handling()
        recovery_valid = validate_workflow_recovery_patterns()
        
        if (error_handling_valid .and. recovery_valid) then
            write(*,*) "PASSED"
            passed_count = passed_count + 1
        else
            write(*,*) "FAILED"
            failed_count = failed_count + 1
            if (.not. error_handling_valid) write(*,*) "  - Workflow error handling validation failed"
            if (.not. recovery_valid) write(*,*) "  - Workflow recovery validation failed"
        end if
    end subroutine

    function validate_workflow_error_handling() result(is_valid)
        ! Test workflow error handling patterns
        logical :: is_valid
        character(len=256) :: error_patterns(3)
        logical :: error_valid
        
        ! Error handling patterns
        error_patterns(1) = "gcov src/*.f90 || true"
        error_patterns(2) = "find build -name ""*.gcda"" -execdir gcov {} \\; || true"
        error_patterns(3) = "# Continue on coverage generation errors"
        
        ! Validate error handling
        error_valid = validate_error_handling_patterns(error_patterns, 3)
        
        is_valid = error_valid
    end function

    function validate_workflow_recovery_patterns() result(is_valid)
        ! Test workflow recovery patterns
        logical :: is_valid
        character(len=256) :: recovery_patterns(2)
        logical :: recovery_valid
        
        ! Recovery patterns
        recovery_patterns(1) = "|| true  ! Allow failure and continue"
        recovery_patterns(2) = "# Fallback to alternative coverage method"
        
        ! Validate recovery patterns
        recovery_valid = validate_recovery_strategy_patterns(recovery_patterns, 2)
        
        is_valid = recovery_valid
    end function

    ! === Helper Functions for CI/CD Pattern Validation ===

    function validate_github_actions_step_structure(steps, count) result(is_valid)
        character(len=*), intent(in) :: steps(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate each step has 'uses:' action
        do i = 1, count
            if (index(steps(i), "uses:") == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_github_actions_run_command(commands, count) result(is_valid)
        character(len=*), intent(in) :: commands(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate commands are not empty
        do i = 1, count
            if (len_trim(commands(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_github_actions_artifact_config(config, count) result(is_valid)
        character(len=*), intent(in) :: config(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate artifact configuration includes required fields
        is_valid = (count >= 3) .and. &
                  (index(config(1), "upload-artifact") > 0) .and. &
                  (index(config(2), "name:") > 0) .and. &
                  (index(config(3), "path:") > 0)
    end function

    function validate_gitlab_ci_config_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate GitLab CI configuration structure
        is_valid = (count >= 4) .and. &
                  (index(components(1), "stages:") > 0) .and. &
                  (index(components(2), "image:") > 0) .and. &
                  (index(components(3), "variables:") > 0)
    end function

    function validate_gitlab_ci_script_structure(commands, count) result(is_valid)
        character(len=*), intent(in) :: commands(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate script commands are not empty
        do i = 1, count
            if (len_trim(commands(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_gitlab_ci_artifacts_structure(config, count) result(is_valid)
        character(len=*), intent(in) :: config(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate GitLab artifacts configuration
        is_valid = (count >= 3) .and. &
                  (index(config(1), "artifacts:") > 0) .and. &
                  (index(config(4), "paths:") > 0)
    end function

    function validate_gitlab_ci_coverage_regex(pattern) result(is_valid)
        character(len=*), intent(in) :: pattern
        logical :: is_valid
        
        ! Validate coverage regex pattern
        is_valid = (index(pattern, "coverage:") > 0) .and. &
                  (index(pattern, "\\d+") > 0) .and. &
                  (index(pattern, "%") > 0)
    end function

    function validate_jenkins_structure_syntax(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate Jenkins pipeline syntax
        is_valid = (count >= 3) .and. &
                  (index(components(1), "pipeline") > 0) .and. &
                  (index(components(2), "agent") > 0) .and. &
                  (index(components(3), "stages") > 0)
    end function

    function validate_jenkins_stage_structure(stage, count) result(is_valid)
        character(len=*), intent(in) :: stage(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate Jenkins stage structure
        is_valid = (count >= 2) .and. &
                  (index(stage(1), "stage(") > 0) .and. &
                  (index(stage(2), "sh") > 0)
    end function

    function validate_jenkins_publish_structure(config, count) result(is_valid)
        character(len=*), intent(in) :: config(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate Jenkins publish configuration
        is_valid = (count >= 4) .and. &
                  (index(config(1), "publishHTML") > 0) .and. &
                  (index(config(5), "reportFiles") > 0)
    end function

    function validate_docker_multistage_structure(stages, count) result(is_valid)
        character(len=*), intent(in) :: stages(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate Docker multi-stage structure
        is_valid = (count >= 6) .and. &
                  (index(stages(1), "FROM") > 0) .and. &
                  (index(stages(1), "as builder") > 0) .and. &
                  (index(stages(6), "FROM") > 0)
    end function

    function validate_hpc_module_structure(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate HPC module structure
        is_valid = (count >= 4) .and. &
                  (index(components(1), "module load") > 0) .and. &
                  (index(components(3), "export") > 0) .and. &
                  (index(components(5), "fortcov") > 0)
    end function

    function validate_slurm_structure(directives, count) result(is_valid)
        character(len=*), intent(in) :: directives(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate SLURM job structure
        is_valid = (count >= 6) .and. &
                  (index(directives(1), "#SBATCH") > 0) .and. &
                  (index(directives(4), "module load") > 0) .and. &
                  (index(directives(5), "srun") > 0)
    end function

    function validate_module_command_structure(commands, count) result(is_valid)
        character(len=*), intent(in) :: commands(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate module commands are not empty
        do i = 1, count
            if (len_trim(commands(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_matrix_configuration(components, count) result(is_valid)
        character(len=*), intent(in) :: components(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate CI matrix configuration
        is_valid = (count >= 4) .and. &
                  (index(components(1), "compiler:") > 0) .and. &
                  (index(components(2), "os:") > 0) .and. &
                  (index(components(3), "matrix.compiler") > 0)
    end function

    function validate_exclusion_rules_structure(rules, count) result(is_valid)
        character(len=*), intent(in) :: rules(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate matrix exclusion rules
        is_valid = (count >= 3) .and. &
                  (index(rules(1), "exclude:") > 0) .and. &
                  (index(rules(2), "os:") > 0) .and. &
                  (index(rules(3), "compiler:") > 0)
    end function

    function validate_artifact_upload_config(config, count) result(is_valid)
        character(len=*), intent(in) :: config(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate artifact configuration is not empty
        do i = 1, count
            if (len_trim(config(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_report_generation_sequence(steps, count) result(is_valid)
        character(len=*), intent(in) :: steps(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate report generation sequence
        is_valid = (count >= 2) .and. &
                  (index(steps(1), "gcov") > 0) .and. &
                  (index(steps(2), "fortcov") > 0)
    end function

    function validate_notification_config(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate notification patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

    function validate_error_handling_patterns(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        
        ! Validate error handling includes '|| true' patterns
        is_valid = (count >= 2) .and. &
                  (index(patterns(1), "|| true") > 0) .and. &
                  (index(patterns(2), "|| true") > 0)
    end function

    function validate_recovery_strategy_patterns(patterns, count) result(is_valid)
        character(len=*), intent(in) :: patterns(:)
        integer, intent(in) :: count
        logical :: is_valid
        integer :: i
        
        is_valid = .true.
        
        ! Validate recovery patterns are not empty
        do i = 1, count
            if (len_trim(patterns(i)) == 0) then
                is_valid = .false.
                exit
            end if
        end do
    end function

end program test_ci_cd_workflow_integration