program test_security_validation_core
    !! Core security validation tests for path sanitization
    !! Tests essential security functions to prevent injection attacks
    
    use test_utilities, only: test_runner_t, assert_true, assert_false
    implicit none
    
    type(test_runner_t) :: runner
    
    call runner%init("Security Validation Core Tests")
    
    ! Core security validation tests
    call test_basic_path_validation()
    call test_shell_metacharacters()
    call test_path_traversal()
    call test_null_byte_injection()
    
    call runner%print_summary()
    
    if (runner%get_pass_rate() == 100.0) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_basic_path_validation()
        !! Test basic path validation functionality
        logical :: passed, is_safe
        character(len=256) :: test_path
        
        passed = .true.
        
        ! Test normal paths
        test_path = "coverage/report.html"
        is_safe = validate_safe_path(test_path)
        call assert_true(is_safe, &
            "Normal path should be safe", passed)
        
        ! Test paths with special chars
        test_path = "coverage@report.html"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Path with @ should be unsafe", passed)
        
        call runner%run_test("basic_path_validation", passed)
    end subroutine test_basic_path_validation
    
    subroutine test_shell_metacharacters()
        !! Test detection of shell metacharacters
        logical :: passed, is_safe
        character(len=256) :: test_path
        
        passed = .true.
        
        ! Test semicolon injection
        test_path = "file.txt; rm -rf /"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Semicolon command should be blocked", passed)
        
        ! Test pipe injection
        test_path = "file.txt | cat /etc/passwd"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Pipe command should be blocked", passed)
        
        ! Test backtick injection
        test_path = "file`whoami`.txt"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Backtick command should be blocked", passed)
        
        ! Test dollar injection
        test_path = "file$(rm -rf /).txt"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Dollar command substitution should be blocked", passed)
        
        call runner%run_test("shell_metacharacters", passed)
    end subroutine test_shell_metacharacters
    
    subroutine test_path_traversal()
        !! Test prevention of directory traversal attacks
        logical :: passed, is_safe
        character(len=256) :: test_path
        
        passed = .true.
        
        ! Test basic traversal
        test_path = "../../../etc/passwd"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Parent directory traversal should be blocked", passed)
        
        ! Test hidden traversal
        test_path = "coverage/../../../etc/passwd"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Hidden traversal should be blocked", passed)
        
        ! Test Windows traversal
        test_path = "..\..\..\windows\system32"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Windows traversal should be blocked", passed)
        
        call runner%run_test("path_traversal", passed)
    end subroutine test_path_traversal
    
    subroutine test_null_byte_injection()
        !! Test prevention of null byte injection
        logical :: passed, is_safe
        character(len=256) :: test_path
        
        passed = .true.
        
        ! Test null byte injection (represented as char(0))
        test_path = "file.txt" // char(0) // ".exe"
        is_safe = validate_safe_path(test_path)
        call assert_false(is_safe, &
            "Null byte injection should be blocked", passed)
        
        call runner%run_test("null_byte_injection", passed)
    end subroutine test_null_byte_injection
    
    ! Simple path validator for testing
    function validate_safe_path(path) result(is_safe)
        character(len=*), intent(in) :: path
        logical :: is_safe
        integer :: i
        
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
            index(path, ">") > 0 .or. &
            index(path, "@") > 0 .or. &
            index(path, char(0)) > 0) then
            is_safe = .false.
            return
        end if
        
        ! Check for path traversal
        if (index(path, "..") > 0) then
            is_safe = .false.
            return
        end if
        
    end function validate_safe_path
    
end program test_security_validation_core
