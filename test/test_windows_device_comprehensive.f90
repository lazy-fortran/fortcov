program test_windows_device_comprehensive
    !! Comprehensive Windows Device Names Validation Test Suite for Issue #309
    !! 
    !! GIVEN: Windows system with reserved device names that need complete validation
    !! WHEN: Various Windows device names are tested with different formats
    !! THEN: All Windows device names should be properly detected and blocked
    !!
    !! This test exposes gaps in current Windows device validation:
    !! - Missing COM3-COM9, LPT2-LPT9 validation
    !! - Incomplete extension handling (.txt, .exe, etc.)
    !! - Case sensitivity issues (con vs CON)
    !! - Path component validation (/path/to/CON)
    !!
    use file_ops_secure, only: safe_find_files, safe_mkdir
    use path_security_core, only: validate_path_security, validate_executable_path
    use error_handling_core
    implicit none
    
    integer :: test_count = 0
    integer :: failed_count = 0
    
    print *, "=================================================================="
    print *, "COMPREHENSIVE: Windows Device Names Security Test (Issue #309)"
    print *, "=================================================================="
    print *, ""
    print *, "Testing complete Windows reserved device names validation"
    print *, "This test exposes gaps in current validation implementation"
    print *, ""
    
    ! Test all base device names
    call test_base_device_names()
    
    ! Test all COM devices (COM1-COM9)
    call test_com_device_names()
    
    ! Test all LPT devices (LPT1-LPT9)
    call test_lpt_device_names()
    
    ! Test case variations
    call test_case_variations()
    
    ! Test with extensions
    call test_device_names_with_extensions()
    
    ! Test as path components
    call test_device_names_in_paths()
    
    ! Test edge cases
    call test_edge_cases()
    
    ! Report results
    print *, ""
    print *, "=================================================================="
    print *, "Windows Device Names Test Results"
    print *, "=================================================================="
    write(*, '(A, I0, A, I0, A)') "Tests run: ", test_count, ", Failed: ", failed_count, &
        " (failures indicate incomplete device validation)"
    
    if (failed_count > 0) then
        print *, ""
        print *, "🚨 WINDOWS DEVICE VALIDATION GAPS FOUND"
        print *, "   Current implementation has incomplete Windows device name coverage"
        print *, ""
        write(*, '(A, I0, A)') "   ", failed_count, " validation gaps detected in Windows device names"
        print *, ""
        print *, "Required fixes:"
        print *, "• Add complete COM1-COM9 validation"
        print *, "• Add complete LPT1-LPT9 validation" 
        print *, "• Handle all case variations"
        print *, "• Validate device names with extensions"
        print *, "• Check device names in path components"
    else
        print *, ""
        print *, "✅ COMPLETE WINDOWS DEVICE VALIDATION"
        print *, "   All Windows device names properly detected and blocked"
    end if
    
contains

    subroutine test_base_device_names()
        character(len=32) :: base_devices(4)
        integer :: i
        logical :: all_detected
        
        call start_test("Base Windows Device Names (CON, PRN, AUX, NUL)")
        
        base_devices(1) = "CON"
        base_devices(2) = "PRN"
        base_devices(3) = "AUX" 
        base_devices(4) = "NUL"
        
        all_detected = .true.
        
        do i = 1, 4
            if (.not. is_windows_device_blocked(base_devices(i))) then
                all_detected = .false.
                write(*, '(A, A, A)') "   Missing base device: ", trim(base_devices(i))
            end if
        end do
        
        if (all_detected) then
            call pass_test("All base device names properly detected")
        else
            call fail_test("VALIDATION GAP: Some base device names not detected")
        end if
    end subroutine test_base_device_names
    
    subroutine test_com_device_names()
        character(len=32) :: com_devices(9)
        integer :: i
        logical :: all_detected
        
        call start_test("COM Device Names (COM1-COM9)")
        
        ! Generate COM1 through COM9
        do i = 1, 9
            write(com_devices(i), '(A, I0)') "COM", i
        end do
        
        all_detected = .true.
        
        do i = 1, 9
            if (.not. is_windows_device_blocked(com_devices(i))) then
                all_detected = .false.
                write(*, '(A, A, A)') "   Missing COM device: ", trim(com_devices(i))
            end if
        end do
        
        if (all_detected) then
            call pass_test("All COM devices (COM1-COM9) properly detected")
        else
            call fail_test("VALIDATION GAP: Some COM devices not detected")
        end if
    end subroutine test_com_device_names
    
    subroutine test_lpt_device_names()
        character(len=32) :: lpt_devices(9)
        integer :: i
        logical :: all_detected
        
        call start_test("LPT Device Names (LPT1-LPT9)")
        
        ! Generate LPT1 through LPT9
        do i = 1, 9
            write(lpt_devices(i), '(A, I0)') "LPT", i
        end do
        
        all_detected = .true.
        
        do i = 1, 9
            if (.not. is_windows_device_blocked(lpt_devices(i))) then
                all_detected = .false.
                write(*, '(A, A, A)') "   Missing LPT device: ", trim(lpt_devices(i))
            end if
        end do
        
        if (all_detected) then
            call pass_test("All LPT devices (LPT1-LPT9) properly detected")
        else
            call fail_test("VALIDATION GAP: Some LPT devices not detected")
        end if
    end subroutine test_lpt_device_names
    
    subroutine test_case_variations()
        character(len=32) :: case_variants(12)
        integer :: i
        logical :: all_detected
        
        call start_test("Case Variations (con, Con, CON, prn, Prn, PRN)")
        
        case_variants(1) = "con"
        case_variants(2) = "Con"
        case_variants(3) = "CON"
        case_variants(4) = "prn"
        case_variants(5) = "Prn"
        case_variants(6) = "PRN"
        case_variants(7) = "nul"
        case_variants(8) = "Nul"
        case_variants(9) = "NUL"
        case_variants(10) = "aux"
        case_variants(11) = "Aux"
        case_variants(12) = "AUX"
        
        all_detected = .true.
        
        do i = 1, 12
            if (.not. is_windows_device_blocked(case_variants(i))) then
                all_detected = .false.
                write(*, '(A, A, A)') "   Missing case variant: ", trim(case_variants(i))
            end if
        end do
        
        if (all_detected) then
            call pass_test("All case variations properly detected")
        else
            call fail_test("VALIDATION GAP: Some case variations not detected")
        end if
    end subroutine test_case_variations
    
    subroutine test_device_names_with_extensions()
        character(len=64) :: device_extensions(8)
        integer :: i
        logical :: all_detected
        
        call start_test("Device Names with Extensions (CON.txt, prn.exe, etc.)")
        
        device_extensions(1) = "CON.txt"
        device_extensions(2) = "prn.exe"
        device_extensions(3) = "NUL.log"
        device_extensions(4) = "aux.dat"
        device_extensions(5) = "COM1.cfg"
        device_extensions(6) = "com5.ini"
        device_extensions(7) = "LPT3.tmp"
        device_extensions(8) = "lpt7.bak"
        
        all_detected = .true.
        
        do i = 1, 8
            if (.not. is_windows_device_blocked(device_extensions(i))) then
                all_detected = .false.
                write(*, '(A, A, A)') "   Missing extension variant: ", &
                    trim(device_extensions(i))
            end if
        end do
        
        if (all_detected) then
            call pass_test("All device names with extensions properly detected")
        else
            call fail_test("VALIDATION GAP: Some extension variants not detected")
        end if
    end subroutine test_device_names_with_extensions
    
    subroutine test_device_names_in_paths()
        character(len=64) :: device_paths(6)
        integer :: i
        logical :: all_detected
        
        call start_test("Device Names in Path Components")
        
        device_paths(1) = "path/CON/file"
        device_paths(2) = "dir/prn.txt"
        device_paths(3) = "folder/NUL"
        device_paths(4) = "test/COM9/data"
        device_paths(5) = "output/LPT5.log"
        device_paths(6) = "temp/aux.tmp"
        
        all_detected = .true.
        
        do i = 1, 6
            if (.not. is_windows_device_blocked(device_paths(i))) then
                all_detected = .false.
                write(*, '(A, A, A)') "   Missing path component: ", &
                    trim(device_paths(i))
            end if
        end do
        
        if (all_detected) then
            call pass_test("All device names in paths properly detected")
        else
            call fail_test("VALIDATION GAP: Some device names in paths not detected")
        end if
    end subroutine test_device_names_in_paths
    
    subroutine test_edge_cases()
        character(len=64) :: edge_cases(4)
        integer :: i
        logical :: all_handled
        
        call start_test("Edge Cases (COM0, LPT0, trailing spaces)")
        
        edge_cases(1) = "COM0"    ! Invalid COM port (should not be blocked)
        edge_cases(2) = "LPT0"    ! Invalid LPT port (should not be blocked)
        edge_cases(3) = "CON  "   ! Trailing spaces (should be blocked)
        edge_cases(4) = "CONN"    ! Similar but not device name (should not be blocked)
        
        all_handled = .true.
        
        ! COM0 and LPT0 should NOT be blocked (invalid device numbers)
        if (is_windows_device_blocked(edge_cases(1)) .or. &
            is_windows_device_blocked(edge_cases(2))) then
            all_handled = .false.
            write(*, '(A)') "   Over-blocking invalid device numbers (COM0/LPT0)"
        end if
        
        ! CON with spaces should be blocked
        if (.not. is_windows_device_blocked(edge_cases(3))) then
            all_handled = .false.
            write(*, '(A)') "   Missing trailing spaces handling"
        end if
        
        ! CONN should NOT be blocked (not a device name)
        if (is_windows_device_blocked(edge_cases(4))) then
            all_handled = .false.
            write(*, '(A)') "   Over-blocking similar names (CONN)"
        end if
        
        if (all_handled) then
            call pass_test("All edge cases properly handled")
        else
            call fail_test("VALIDATION GAP: Some edge cases not handled correctly")
        end if
    end subroutine test_edge_cases
    
    ! Test helper to check if a Windows device name is blocked
    function is_windows_device_blocked(device_name) result(is_blocked)
        character(len=*), intent(in) :: device_name
        logical :: is_blocked
        
        character(len=:), allocatable :: safe_path
        type(error_context_t) :: error_ctx
        
        call validate_path_security(device_name, safe_path, error_ctx)
        is_blocked = (error_ctx%error_code /= ERROR_SUCCESS)
        
        ! Clean up if allocated
        if (allocated(safe_path)) deallocate(safe_path)
    end function is_windows_device_blocked
    
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') "Test ", test_count, ": ", test_name
    end subroutine start_test
    
    subroutine pass_test(message)
        character(len=*), intent(in) :: message
        print *, "   ✅ PASS: " // trim(message)
        print *, ""
    end subroutine pass_test
    
    subroutine fail_test(message)
        character(len=*), intent(in) :: message
        failed_count = failed_count + 1
        print *, "   🚨 FAIL: " // trim(message)
        print *, ""
    end subroutine fail_test

end program test_windows_device_comprehensive