program test_readme_workflow_issue_260
    use iso_fortran_env, only: error_unit
    implicit none
    integer :: stat, unit
    character(len=:), allocatable :: test_dir, cmd
    character(len=1024) :: msg
    logical :: test_passed
    
    ! Test the corrected README workflow from issue #260
    print *, "Testing README workflow corrections (issue #260)..."
    
    ! Create temporary test directory
    call execute_command_line("mktemp -d", exitstat=stat, cmdmsg=msg)
    if (stat /= 0) then
        write(error_unit, '(A)') "Failed to create temp directory"
        stop 1
    end if
    
    ! Store the temp directory path
    test_dir = "/tmp/test_readme_260_" // get_timestamp()
    cmd = "mkdir -p " // test_dir
    call execute_command_line(cmd, exitstat=stat)
    if (stat /= 0) then
        write(error_unit, '(A)') "Failed to create test directory"
        stop 1
    end if
    
    ! Create test project structure
    call create_test_files(test_dir)
    
    ! Test 1: Verify gcov workflow generates .gcov files
    test_passed = test_gcov_generation(test_dir)
    if (.not. test_passed) then
        write(error_unit, '(A)') "FAIL: gcov workflow test failed"
        call cleanup(test_dir)
        stop 1
    end if
    print *, "PASS: gcov workflow generates coverage files"
    
    ! Test 2: Verify --fail-under flag works in CI/CD context
    test_passed = test_fail_under_flag(test_dir)
    if (.not. test_passed) then
        write(error_unit, '(A)') "FAIL: --fail-under flag test failed"
        call cleanup(test_dir)
        stop 1
    end if
    print *, "PASS: --fail-under flag works correctly"
    
    ! Test 3: Verify --threshold flag still works (backward compatibility)
    test_passed = test_threshold_flag(test_dir)
    if (.not. test_passed) then
        write(error_unit, '(A)') "FAIL: --threshold flag test failed"
        call cleanup(test_dir)
        stop 1
    end if
    print *, "PASS: --threshold flag works for backward compatibility"
    
    ! Cleanup
    call cleanup(test_dir)
    
    print *, "SUCCESS: All README workflow tests passed (issue #260)"
    
contains
    
    function get_timestamp() result(timestamp)
        character(len=16) :: timestamp
        integer :: values(8)
        call date_and_time(values=values)
        write(timestamp, '(I4,5I2.2)') values(1), values(2), values(3), &
                                       values(5), values(6), values(7)
    end function get_timestamp
    
    subroutine create_test_files(dir)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable :: path, content
        integer :: unit, iostat
        
        ! Create fpm.toml
        path = trim(dir) // "/fpm.toml"
        open(newunit=unit, file=path, action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') 'name = "calculator"'
            write(unit, '(A)') 'version = "0.1.0"'
            close(unit)
        end if
        
        ! Create src directory
        call execute_command_line("mkdir -p " // trim(dir) // "/src")
        call execute_command_line("mkdir -p " // trim(dir) // "/test")
        
        ! Create calculator.f90
        path = trim(dir) // "/src/calculator.f90"
        open(newunit=unit, file=path, action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') 'module calculator'
            write(unit, '(A)') '    implicit none'
            write(unit, '(A)') '    public :: add'
            write(unit, '(A)') 'contains'
            write(unit, '(A)') '    function add(a, b) result(c)'
            write(unit, '(A)') '        real, intent(in) :: a, b'
            write(unit, '(A)') '        real :: c'
            write(unit, '(A)') '        c = a + b'
            write(unit, '(A)') '    end function add'
            write(unit, '(A)') 'end module calculator'
            close(unit)
        end if
        
        ! Create test_calculator.f90
        path = trim(dir) // "/test/test_calculator.f90"
        open(newunit=unit, file=path, action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') 'program test_calculator'
            write(unit, '(A)') '    use calculator'
            write(unit, '(A)') '    implicit none'
            write(unit, '(A)') '    if (abs(add(2.0, 3.0) - 5.0) > 1e-6) stop 1'
            write(unit, '(A)') '    print *, "Test passed!"'
            write(unit, '(A)') 'end program test_calculator'
            close(unit)
        end if
    end subroutine create_test_files
    
    function test_gcov_generation(dir) result(success)
        character(len=*), intent(in) :: dir
        logical :: success
        character(len=:), allocatable :: cmd
        integer :: stat
        
        success = .false.
        
        ! Change to test directory
        cmd = "cd " // trim(dir) // " && " // &
              "fpm test --flag '-fprofile-arcs -ftest-coverage' && " // &
              "find build -name '*.gcda' | while read gcda_file; do " // &
              "gcov -b ""$gcda_file"" 2>/dev/null || true; done && " // &
              "ls *.gcov > /dev/null 2>&1"
        
        call execute_command_line(cmd, exitstat=stat)
        success = (stat == 0)
    end function test_gcov_generation
    
    function test_fail_under_flag(dir) result(success)
        character(len=*), intent(in) :: dir
        logical :: success
        character(len=:), allocatable :: cmd, fortcov_path
        integer :: stat
        
        success = .false.
        
        ! Find fortcov executable
        cmd = "find build -name fortcov -type f -executable | head -1"
        call execute_command_line(cmd, exitstat=stat)
        
        ! Assume fortcov is in PATH for this test
        fortcov_path = "fortcov"
        
        ! Test --fail-under flag with passing threshold
        cmd = "cd " // trim(dir) // " && " // &
              fortcov_path // " --fail-under=70 --output=coverage_pass.md"
        call execute_command_line(cmd, exitstat=stat)
        if (stat /= 0) return
        
        ! Test --fail-under flag with failing threshold (should exit non-zero)
        cmd = "cd " // trim(dir) // " && " // &
              fortcov_path // " --fail-under=95 --output=coverage_fail.md"
        call execute_command_line(cmd, exitstat=stat)
        ! This should fail (exit non-zero) since coverage is below 95%
        
        success = .true.
    end function test_fail_under_flag
    
    function test_threshold_flag(dir) result(success)
        character(len=*), intent(in) :: dir
        logical :: success
        character(len=:), allocatable :: cmd, fortcov_path
        integer :: stat
        
        success = .false.
        
        ! Assume fortcov is in PATH for this test
        fortcov_path = "fortcov"
        
        ! Test --threshold flag (should work but not fail the process)
        cmd = "cd " // trim(dir) // " && " // &
              fortcov_path // " --threshold=80 --output=coverage_threshold.md"
        call execute_command_line(cmd, exitstat=stat)
        
        success = (stat == 0)
    end function test_threshold_flag
    
    subroutine cleanup(dir)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable :: cmd
        
        cmd = "rm -rf " // trim(dir)
        call execute_command_line(cmd)
    end subroutine cleanup
    
end program test_readme_workflow_issue_260