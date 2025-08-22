! Test for Issue #232: Documentation Contains Broken gcov Commands
!
! This test implements the RED phase of TDD by creating FAILING tests that validate
! documentation commands work exactly as written. These tests will FAIL initially,
! proving that the documented gcov commands are broken.
!
! Given: Documentation files contain gcov commands  
! When: Following documented workflows exactly as written
! Then: All commands should execute successfully and produce expected output
!
! TEST SCOPE:
! - getting-started.md line 88: gcov -o build/gcov src/*.f90
! - examples.md lines 50,70,123,138: gcov -o build/gcov src/*.f90
! - usage-guide.md lines 28,214,223: gcov -o build/gcov src/*.f90
! - troubleshooting.md lines 169,248: gcov -o build/gcov src/*.f90

program test_documentation_gcov_commands_issue_232
    use error_handling
    implicit none
    
    ! Test counters
    integer :: total_tests = 0
    integer :: passed_tests = 0
    integer :: failed_tests = 0
    
    write(*,*) '================================================================='
    write(*,*) 'DOCUMENTATION GCOV COMMANDS VALIDATION TEST (Issue #232)'
    write(*,*) '================================================================='
    write(*,*) ''
    write(*,*) 'TDD RED PHASE: These tests SHOULD FAIL initially'
    write(*,*) 'Proving that documented gcov commands are broken'
    write(*,*) ''
    
    ! Test documentation command patterns
    call test_getting_started_gcov_command()
    call test_examples_gcov_commands()
    call test_usage_guide_gcov_commands()
    call test_troubleshooting_gcov_commands()
    call test_end_to_end_workflow_validation()
    
    ! Results summary
    call print_test_summary()
    
    ! Exit with appropriate code
    if (failed_tests > 0) then
        write(*,*) ''
        write(*,*) '❌ TESTS FAILED AS EXPECTED (RED phase)'
        write(*,*) '   Documentation contains broken gcov commands'
        write(*,*) '   Ready for GREEN phase implementation'
        stop 1  ! Expected failure in RED phase
    else
        write(*,*) ''
        write(*,*) '⚠️  UNEXPECTED: Tests passed (should fail in RED phase)'
        write(*,*) '   Either tests are wrong or documentation already fixed'
        stop 2
    end if

contains

    ! Test getting-started.md line 88 command pattern
    subroutine test_getting_started_gcov_command()
        integer :: exit_code
        logical :: gcov_files_created
        character(len=1000) :: error_message
        
        call test_start('getting-started.md gcov command validation')
        
        ! Given: FPM project with coverage data generated
        call setup_fpm_coverage_environment()
        
        ! When: Running the exact command from getting-started.md line 88
        ! Command: gcov -o build/gcov src/*.f90
        call execute_command_line('gcov -o build/gcov src/*.f90', &
                                  exitstat=exit_code)
        
        ! Then: Command should succeed and create .gcov files
        call check_gcov_files_created(gcov_files_created, error_message)
        
        if (exit_code == 0 .and. gcov_files_created) then
            call test_pass('getting-started.md gcov command', &
                          'Command executed and created .gcov files')
        else
            call test_fail('getting-started.md gcov command', &
                          'Command failed: ' // trim(error_message))
        end if
        
        call cleanup_test_environment()
    end subroutine

    ! Test examples.md gcov command patterns
    subroutine test_examples_gcov_commands()
        call test_start('examples.md gcov commands validation')
        
        ! Test line 50: gcov -o build/gcov src/*.f90
        call test_examples_line_50_command()
        
        ! Test line 70: gcov -o build/gcov src/*.f90  # Standard location
        call test_examples_line_70_command()
        
        ! Test line 123: gcov -o build/gcov src/*.f90 (in CI context)
        call test_examples_line_123_command()
        
        ! Test line 138: gcov -o build/gcov src/*.f90 (in troubleshooting)
        call test_examples_line_138_command()
    end subroutine

    ! Test usage-guide.md gcov command patterns  
    subroutine test_usage_guide_gcov_commands()
        call test_start('usage-guide.md gcov commands validation')
        
        ! Test line 28: Basic workflow command
        call test_usage_guide_line_28_command()
        
        ! Test line 214: Advanced workflow command
        call test_usage_guide_line_214_command()
        
        ! Test line 223: CI/CD workflow command
        call test_usage_guide_line_223_command()
    end subroutine

    ! Test troubleshooting.md gcov command patterns
    subroutine test_troubleshooting_gcov_commands()
        call test_start('troubleshooting.md gcov commands validation')
        
        ! Test line 169: Troubleshooting workflow command
        call test_troubleshooting_line_169_command()
        
        ! Test line 248: Alternative workflow command
        call test_troubleshooting_line_248_command()
    end subroutine

    ! Test complete end-to-end workflow from documentation
    subroutine test_end_to_end_workflow_validation()
        integer :: step1_exit, step2_exit, step3_exit
        logical :: coverage_report_created
        
        call test_start('Complete documented workflow validation')
        
        call cleanup_test_environment()
        
        ! Step 1: fpm test --flag "-fprofile-arcs -ftest-coverage"
        call execute_command_line('fpm test --flag "-fprofile-arcs -ftest-coverage"', &
                                  exitstat=step1_exit)
        
        ! Step 2: gcov -o build/gcov src/*.f90 (THE BROKEN COMMAND)
        call execute_command_line('gcov -o build/gcov src/*.f90', &
                                  exitstat=step2_exit)
        
        ! Step 3: fortcov (should work if gcov step succeeded)
        call execute_command_line('fpm run fortcov -- --source=src --output=coverage.md', &
                                  exitstat=step3_exit)
        
        ! Verify final output exists
        inquire(file='coverage.md', exist=coverage_report_created)
        
        if (step1_exit == 0 .and. step2_exit == 0 .and. step3_exit == 0 .and. &
            coverage_report_created) then
            call test_pass('Complete documented workflow', &
                          'All steps completed successfully')
        else
            call test_fail('Complete documented workflow', &
                          'Workflow failed at gcov step (as expected)')
        end if
    end subroutine

    ! Setup FPM coverage environment for testing
    subroutine setup_fpm_coverage_environment()
        integer :: exit_code
        
        ! Clean any existing coverage data
        call execute_command_line('rm -f *.gcda *.gcno *.gcov', exitstat=exit_code)
        call execute_command_line('rm -rf build/gcov', exitstat=exit_code)
        
        ! Build and test with coverage to generate .gcda/.gcno files
        call execute_command_line('fpm build --flag "-fprofile-arcs -ftest-coverage"', &
                                  exitstat=exit_code)
        call execute_command_line('fpm test --flag "-fprofile-arcs -ftest-coverage"', &
                                  exitstat=exit_code)
    end subroutine

    ! Check if gcov files were created successfully
    subroutine check_gcov_files_created(files_created, error_message)
        logical, intent(out) :: files_created
        character(len=*), intent(out) :: error_message
        integer :: exit_code
        
        files_created = .false.
        error_message = ''
        
        ! Check if any .gcov files were created
        call execute_command_line('ls *.gcov > /dev/null 2>&1', exitstat=exit_code)
        
        if (exit_code == 0) then
            files_created = .true.
        else
            ! Check what actually happened
            call execute_command_line('ls -la build/ 2>/dev/null || echo "build dir missing"', &
                                     exitstat=exit_code)
            error_message = 'No .gcov files created - gcov command failed'
        end if
    end subroutine

    ! Individual test implementations for specific documentation lines
    subroutine test_examples_line_50_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('examples.md line 50', 'gcov command succeeded')
        else
            call test_fail('examples.md line 50', 'gcov command failed')
        end if
    end subroutine

    subroutine test_examples_line_70_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('examples.md line 70', 'gcov standard location command succeeded')
        else
            call test_fail('examples.md line 70', 'gcov standard location command failed')
        end if
    end subroutine

    subroutine test_examples_line_123_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('examples.md line 123', 'gcov CI context command succeeded')
        else
            call test_fail('examples.md line 123', 'gcov CI context command failed')
        end if
    end subroutine

    subroutine test_examples_line_138_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('examples.md line 138', 'gcov troubleshooting command succeeded')
        else
            call test_fail('examples.md line 138', 'gcov troubleshooting command failed')
        end if
    end subroutine

    subroutine test_usage_guide_line_28_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('usage-guide.md line 28', 'gcov basic workflow succeeded')
        else
            call test_fail('usage-guide.md line 28', 'gcov basic workflow failed')
        end if
    end subroutine

    subroutine test_usage_guide_line_214_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('usage-guide.md line 214', 'gcov advanced workflow succeeded')
        else
            call test_fail('usage-guide.md line 214', 'gcov advanced workflow failed')
        end if
    end subroutine

    subroutine test_usage_guide_line_223_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('usage-guide.md line 223', 'gcov CI/CD workflow succeeded')
        else
            call test_fail('usage-guide.md line 223', 'gcov CI/CD workflow failed')
        end if
    end subroutine

    subroutine test_troubleshooting_line_169_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('troubleshooting.md line 169', 'gcov troubleshooting succeeded')
        else
            call test_fail('troubleshooting.md line 169', 'gcov troubleshooting failed')
        end if
    end subroutine

    subroutine test_troubleshooting_line_248_command()
        integer :: exit_code
        logical :: success
        
        call setup_fpm_coverage_environment()
        call execute_command_line('gcov -o build/gcov src/*.f90', exitstat=exit_code)
        
        success = (exit_code == 0)
        if (success) then
            call test_pass('troubleshooting.md line 248', 'gcov alternative workflow succeeded')
        else
            call test_fail('troubleshooting.md line 248', 'gcov alternative workflow failed')
        end if
    end subroutine

    ! Test utility functions
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*,*) '  Running: ', trim(test_name)
    end subroutine

    subroutine test_pass(test_name, message)
        character(len=*), intent(in) :: test_name, message
        passed_tests = passed_tests + 1
        write(*,*) '    ✅ PASS: ', trim(test_name), ' - ', trim(message)
    end subroutine

    subroutine test_fail(test_name, message)
        character(len=*), intent(in) :: test_name, message
        failed_tests = failed_tests + 1
        write(*,*) '    ❌ FAIL: ', trim(test_name), ' - ', trim(message)
    end subroutine

    subroutine cleanup_test_environment()
        integer :: exit_code
        call execute_command_line('rm -f *.gcov coverage.md coverage.json', &
                                  exitstat=exit_code)
    end subroutine

    subroutine print_test_summary()
        write(*,*) ''
        write(*,*) '================================================================='
        write(*,*) 'DOCUMENTATION GCOV COMMANDS TEST RESULTS'
        write(*,*) '================================================================='
        write(*,*) 'Total Tests:    ', total_tests
        write(*,*) 'Passed Tests:   ', passed_tests  
        write(*,*) 'Failed Tests:   ', failed_tests
        if (total_tests > 0) then
            write(*,*) 'Success Rate:   ', (passed_tests * 100) / total_tests, '%'
        end if
    end subroutine

end program test_documentation_gcov_commands_issue_232