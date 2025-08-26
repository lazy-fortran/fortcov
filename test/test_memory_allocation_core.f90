program test_memory_allocation_core
    !! Core memory allocation tests
    !! Tests allocation safety and guard patterns
    
    use test_utilities, only: test_runner_t, assert_true, assert_false
    use coverage_types, only: coverage_data_t
    implicit none
    
    type(test_runner_t) :: runner
    
    call runner%init("Memory Allocation Core Tests")
    
    ! Core allocation tests
    call test_double_allocation_prevention()
    call test_allocation_guards()
    call test_deallocation_safety()
    call test_reallocation_patterns()
    
    call runner%print_summary()
    
    if (runner%get_pass_rate() == 100.0) then
        call exit(0)
    else
        call exit(1)
    end if
    
contains
    
    subroutine test_double_allocation_prevention()
        !! Test prevention of double allocation
        type(coverage_data_t) :: coverage
        logical :: passed, allocation_safe
        
        passed = .true.
        
        ! First allocation
        call coverage%initialize()
        allocation_safe = coverage%is_initialized()
        call assert_true(allocation_safe, &
            "First allocation should succeed", passed)
        
        ! Attempt second allocation without deallocation
        call coverage%initialize()  ! Should handle gracefully
        allocation_safe = coverage%is_initialized()
        call assert_true(allocation_safe, &
            "Double allocation should be handled safely", passed)
        
        call runner%run_test("double_allocation_prevention", passed)
    end subroutine test_double_allocation_prevention
    
    subroutine test_allocation_guards()
        !! Test allocation guard patterns
        type(coverage_data_t), allocatable :: coverages(:)
        logical :: passed
        integer :: i
        
        passed = .true.
        
        ! Test array allocation guards
        if (.not. allocated(coverages)) then
            allocate(coverages(10))
            call assert_true(allocated(coverages), &
                "Guarded allocation should succeed", passed)
        end if
        
        ! Initialize array elements
        do i = 1, 10
            call coverages(i)%initialize()
        end do
        
        ! Clean up
        if (allocated(coverages)) then
            deallocate(coverages)
            call assert_false(allocated(coverages), &
                "Deallocation should succeed", passed)
        end if
        
        call runner%run_test("allocation_guards", passed)
    end subroutine test_allocation_guards
    
    subroutine test_deallocation_safety()
        !! Test safe deallocation patterns
        type(coverage_data_t), allocatable :: coverage
        logical :: passed
        
        passed = .true.
        
        ! Allocate
        allocate(coverage)
        call coverage%initialize()
        
        ! Deallocate safely
        if (allocated(coverage)) then
            deallocate(coverage)
        end if
        
        ! Verify deallocated
        call assert_false(allocated(coverage), &
            "Coverage should be deallocated", passed)
        
        ! Test double deallocation safety
        if (allocated(coverage)) then
            deallocate(coverage)  ! Should not execute
        end if
        
        call runner%run_test("deallocation_safety", passed)
    end subroutine test_deallocation_safety
    
    subroutine test_reallocation_patterns()
        !! Test reallocation patterns
        character(len=:), allocatable :: dynamic_string
        logical :: passed
        
        passed = .true.
        
        ! Initial allocation
        dynamic_string = "initial"
        call assert_true(allocated(dynamic_string), &
            "Initial allocation should succeed", passed)
        
        ! Reallocation with different size
        dynamic_string = "much longer string now"
        call assert_true(allocated(dynamic_string), &
            "Reallocation should succeed", passed)
        
        ! Deallocate
        if (allocated(dynamic_string)) then
            deallocate(dynamic_string)
        end if
        
        call runner%run_test("reallocation_patterns", passed)
    end subroutine test_reallocation_patterns
    
end program test_memory_allocation_core