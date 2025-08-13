program test_config_edge
    use fortcov_config
    implicit none
    
    print *, "Testing config edge cases..."
    
    if (test_empty_string()) print *, "  Empty string: PASS"
    if (test_empty_value()) print *, "  Empty value: PASS"
    if (test_multiple_equals()) print *, "  Multiple equals: PASS"
    
contains

    function test_empty_string() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        allocate(character(len=20) :: args(1))
        args(1) = ""
        call parse_config(args, config, success, error_message)
        passed = success  ! Empty string should be skipped
    end function
    
    function test_empty_value() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        allocate(character(len=20) :: args(1))
        args(1) = "--output="
        call parse_config(args, config, success, error_message)
        passed = success .and. (config%output_path == "")
    end function
    
    function test_multiple_equals() result(passed)
        logical :: passed
        type(config_t) :: config
        character(len=256) :: error_message
        logical :: success
        character(len=:), allocatable :: args(:)
        
        allocate(character(len=30) :: args(1))
        args(1) = "--output=file=test.md"
        call parse_config(args, config, success, error_message)
        passed = success .and. (config%output_path == "file=test.md")
    end function

end program test_config_edge
