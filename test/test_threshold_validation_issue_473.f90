program test_threshold_validation_issue_473
    !! Comprehensive test for issue #473 - Input validation for all threshold parameters
    !! 
    !! Tests that all percentage thresholds validate range (0.0-100.0):
    !! - --minimum (and aliases -m, --threshold)
    !! - --fail-under
    !! - --diff-threshold
    use config_types, only: config_t
    use config_core, only: parse_config
    use test_framework_utilities
    implicit none
    
    type(test_counter_t) :: counter
    
    print *, "========================================="
    print *, "Test: Threshold Validation Issue #473"
    print *, "========================================="
    print *, ""
    
    call init_test_counter(counter)
    
    ! Test --minimum threshold
    print *, "Testing --minimum threshold:"
    print *, "---------------------------------"
    call test_threshold_parameter(counter, "--minimum", "minimum coverage")
    call test_threshold_parameter(counter, "-m", "minimum coverage (short)")
    call test_threshold_parameter(counter, "--threshold", &
                                  "minimum coverage (alias)")
    
    ! Test --fail-under threshold
    print *, ""
    print *, "Testing --fail-under threshold:"
    print *, "---------------------------------"
    call test_threshold_parameter(counter, "--fail-under", "fail threshold")
    
    ! Test --diff-threshold
    print *, ""
    print *, "Testing --diff-threshold:"
    print *, "---------------------------------"
    call test_threshold_parameter(counter, "--diff-threshold", &
                                  "diff threshold")
    
    call print_test_summary(counter, "Threshold Validation Issue #473")
    
    if (counter%failed > 0) then
        stop 1
    else
        stop 0
    end if
    
contains
    
    subroutine test_threshold_parameter(counter, flag, description)
        type(test_counter_t), intent(inout) :: counter
        character(len=*), intent(in) :: flag, description
        
        ! Test valid values
        call test_single_value(counter, flag, "50.0", .true., &
                               trim(description) // " - valid mid-range")
        call test_single_value(counter, flag, "0.0", .true., &
                               trim(description) // " - lower boundary")
        call test_single_value(counter, flag, "100.0", .true., &
                               trim(description) // " - upper boundary")
        
        ! Test invalid values (should be rejected)
        call test_single_value(counter, flag, "-1", .false., &
                               trim(description) // " - negative")
        call test_single_value(counter, flag, "101", .false., &
                               trim(description) // " - above 100")
        call test_single_value(counter, flag, "999999", .false., &
                               trim(description) // " - extreme value")
    end subroutine test_threshold_parameter
    
    subroutine test_single_value(counter, flag, value, should_succeed, test_desc)
        type(test_counter_t), intent(inout) :: counter
        character(len=*), intent(in) :: flag, value, test_desc
        logical, intent(in) :: should_succeed
        type(config_t) :: config
        character(len=512) :: error_message
        logical :: success
        character(len=256), allocatable :: args(:)
        
        allocate(args(2))
        args(1) = flag
        args(2) = value
        
        call parse_config(args, config, success, error_message)
        
        if (should_succeed) then
            if (success) then
                call increment_pass(counter)
                print *, "  ✅ PASS: ", trim(test_desc), " (", &
                        trim(value), ")"
            else
                call increment_fail(counter)
                print *, "  ❌ FAIL: ", trim(test_desc), " (", &
                        trim(value), ")"
                print *, "    Expected success but got error: ", &
                        trim(error_message)
            end if
        else
            if (.not. success) then
                call increment_pass(counter)
                print *, "  ✅ PASS: ", trim(test_desc), " (", &
                        trim(value), ") correctly rejected"
                print *, "    Error: ", trim(error_message)
            else
                call increment_fail(counter)
                print *, "  ❌ FAIL: ", trim(test_desc), " (", &
                        trim(value), ")"
                print *, "    Expected rejection but value was accepted"
            end if
        end if
        
        deallocate(args)
    end subroutine test_single_value
    
end program test_threshold_validation_issue_473
