program test_edge_cases
    use fortcov_config
    implicit none
    
    type(config_t) :: config
    character(len=256) :: error_message
    logical :: success
    character(len=:), allocatable :: args(:)
    
    ! Test with empty string argument
    print *, "Testing empty string argument..."
    allocate(character(len=20) :: args(1))
    args(1) = ""
    call parse_config(args, config, success, error_message)
    print *, "  Empty string: success=", success
    
    ! Test with equals sign but no value
    print *, "Testing equals with no value..."
    deallocate(args)
    allocate(character(len=20) :: args(1))
    args(1) = "--output="
    call parse_config(args, config, success, error_message)
    print *, "  Empty value: success=", success, " output='", config%output_path, "'"
    
    ! Test with multiple equals signs
    print *, "Testing multiple equals signs..."
    deallocate(args)
    allocate(character(len=30) :: args(1))
    args(1) = "--output=file=test.md"
    call parse_config(args, config, success, error_message)
    print *, "  Multiple equals: success=", success, " output='", config%output_path, "'"
    
end program test_edge_cases
