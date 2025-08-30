program test_security_attack_vectors
    !! Security attack vector tests
    !! Tests various attack patterns and injection techniques
    
    use test_utilities, only: test_runner_t, assert_false
    implicit none
    
    type(test_runner_t) :: runner
    
    call runner%init("Security Attack Vector Tests")
    
    ! Attack vector tests
    call test_url_encoded_attacks()
    call test_windows_device_attacks()
    call test_redirection_attacks()
    call test_system_file_attacks()
    
    call runner%print_summary()
    
    if (abs(runner%get_pass_rate() - 100.0) < epsilon(1.0)) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_url_encoded_attacks()
        !! Test prevention of URL-encoded attack vectors
        logical :: passed, is_safe
        character(len=256) :: test_path, decoded_path
        
        passed = .true.
        
        ! Test URL-encoded traversal (%2e%2e = ..)
        test_path = "%2e%2e%2f%2e%2e%2fetc%2fpasswd"
        call simple_url_decode(test_path, decoded_path)
        is_safe = validate_safe_path(decoded_path)
        call assert_false(is_safe, &
            "URL-encoded traversal should be blocked", passed)
        
        ! Test encoded semicolon (%3b = ;)
        test_path = "file%3brm%20-rf%20%2f"
        call simple_url_decode(test_path, decoded_path)
        is_safe = validate_safe_path(decoded_path)
        call assert_false(is_safe, &
            "URL-encoded command should be blocked", passed)
        
        call runner%run_test("url_encoded_attacks", passed)
    end subroutine test_url_encoded_attacks
    
    subroutine test_windows_device_attacks()
        !! Test prevention of Windows device name attacks
        logical :: passed, is_safe
        character(len=256) :: test_path
        
        passed = .true.
        
        ! Test Windows device names
        test_path = "CON"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "CON device should be blocked", passed)
        
        test_path = "PRN.txt"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "PRN device should be blocked", passed)
        
        test_path = "NUL"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "NUL device should be blocked", passed)
        
        test_path = "COM1"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "COM1 device should be blocked", passed)
        
        call runner%run_test("windows_device_attacks", passed)
    end subroutine test_windows_device_attacks
    
    subroutine test_redirection_attacks()
        !! Test prevention of output redirection attacks
        logical :: passed, is_safe
        character(len=256) :: test_path
        
        passed = .true.
        
        ! Test output redirection
        test_path = "file.txt > /etc/passwd"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Output redirection should be blocked", passed)
        
        ! Test input redirection
        test_path = "file.txt < /etc/passwd"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Input redirection should be blocked", passed)
        
        ! Test append redirection
        test_path = "file.txt >> /etc/passwd"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Append redirection should be blocked", passed)
        
        call runner%run_test("redirection_attacks", passed)
    end subroutine test_redirection_attacks
    
    subroutine test_system_file_attacks()
        !! Test prevention of system file access
        logical :: passed, is_safe
        character(len=256) :: test_path
        
        passed = .true.
        
        ! Test Unix system files
        test_path = "/etc/passwd"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Direct /etc/passwd access should be blocked", passed)
        
        test_path = "/etc/shadow"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Direct /etc/shadow access should be blocked", passed)
        
        ! Test Windows system files
        test_path = "C:\Windows\System32\config\SAM"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Windows SAM access should be blocked", passed)
        
        call runner%run_test("system_file_attacks", passed)
    end subroutine test_system_file_attacks
    
    ! Helper functions
    
    subroutine simple_url_decode(encoded_path, decoded_path)
        !! Simple URL decoder for testing
        character(len=*), intent(in) :: encoded_path
        character(len=*), intent(out) :: decoded_path
        integer :: i, j, hex_val, io_stat
        character(len=2) :: hex_str
        
        decoded_path = ""
        i = 1
        j = 1
        
        do while (i <= len_trim(encoded_path))
            if (encoded_path(i:i) == "%") then
                if (i + 2 <= len_trim(encoded_path)) then
                    hex_str = encoded_path(i+1:i+2)
                    read(hex_str, '(Z2)', iostat=io_stat) hex_val
                    if (io_stat == 0) then
                        decoded_path(j:j) = char(hex_val)
                        j = j + 1
                    end if
                    i = i + 3
                else
                    i = i + 1
                end if
            else
                decoded_path(j:j) = encoded_path(i:i)
                i = i + 1
                j = j + 1
            end if
        end do
    end subroutine simple_url_decode
    
    function validate_safe_path(path) result(is_safe)
        !! Path validator including Windows device checks
        character(len=*), intent(in) :: path
        logical :: is_safe
        character(len=256) :: upper_path
        
        is_safe = .true.
        
        ! Check for dangerous characters
        if (index(path, ";") > 0 .or. &
            index(path, "|") > 0 .or. &
            index(path, "&") > 0 .or. &
            index(path, "`") > 0 .or. &
            index(path, "$") > 0 .or. &
            index(path, "(") > 0 .or. &
            index(path, ")") > 0 .or. &
            index(path, "<") > 0 .or. &
            index(path, ">") > 0) then
            is_safe = .false.
            return
        end if
        
        ! Check for path traversal
        if (index(path, "..") > 0) then
            is_safe = .false.
            return
        end if
        
        ! Check for system file paths
        if (index(path, "/etc/") > 0 .or. &
            index(path, "\Windows\") > 0) then
            is_safe = .false.
            return
        end if
        
        ! Check Windows device names
        call to_upper(path, upper_path)
        if (trim(upper_path) == "CON" .or. &
            trim(upper_path) == "PRN" .or. &
            trim(upper_path) == "AUX" .or. &
            trim(upper_path) == "NUL" .or. &
            index(upper_path, "COM") == 1 .or. &
            index(upper_path, "LPT") == 1 .or. &
            index(upper_path, "PRN.") == 1) then
            is_safe = .false.
            return
        end if
        
    end function validate_safe_path
    
    subroutine to_upper(input, output)
        !! Convert string to uppercase
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer :: i
        
        output = input
        do i = 1, len_trim(input)
            if (iachar(input(i:i)) >= iachar('a') .and. &
                iachar(input(i:i)) <= iachar('z')) then
                output(i:i) = achar(iachar(input(i:i)) - 32)
            end if
        end do
    end subroutine to_upper
    
end program test_security_attack_vectors
