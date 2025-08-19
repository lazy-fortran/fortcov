! Test for Issue #178: Double allocation crash in coverage_model.f90:453
! 
! **Given**: CATASTROPHIC double allocation bug causing 100% crash rate
! **When**: Processing ANY number of coverage files (even 1 file)  
! **Then**: fortcov crashes with "Attempting to allocate already allocated variable 'this'"
!
! This test directly reproduces and validates the fix for the double allocation
! crash at line 453 in coverage_data_init subroutine.

program test_issue_178_double_allocation_crash
    use coverage_model
    implicit none
    
    logical :: all_tests_passed
    
    print *, "Testing Issue #178: Double allocation crash fix..."
    
    all_tests_passed = .true.
    
    ! Test 1: Direct double allocation detection in coverage_data_init
    all_tests_passed = all_tests_passed .and. test_double_allocation_prevention()
    
    ! Test 2: Multiple init calls on same object (real-world crash scenario)
    all_tests_passed = all_tests_passed .and. test_multiple_init_calls()
    
    ! Test 3: Coverage file processing memory lifecycle
    all_tests_passed = all_tests_passed .and. test_coverage_processing_lifecycle()
    
    ! Test 4: Memory safety during repeated file processing
    all_tests_passed = all_tests_passed .and. test_repeated_file_processing()
    
    if (all_tests_passed) then
        print *, "All Issue #178 double allocation tests PASSED"
        call exit(0)
    else
        print *, "Issue #178 double allocation tests FAILED"
        call exit(1)
    end if

contains

    ! Test direct double allocation prevention in coverage_data_init
    function test_double_allocation_prevention() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t), allocatable :: test_files(:)
        type(coverage_line_t), allocatable :: test_lines(:)
        
        print *, "  Test 1: Direct double allocation prevention (line 453 bug)"
        
        ! **Given**: Create initial coverage data with files allocated
        allocate(test_lines(2))
        call test_lines(1)%init("test.f90", 1, 5, .true.)
        call test_lines(2)%init("test.f90", 2, 0, .true.)
        
        allocate(test_files(1))
        call test_files(1)%init("test.f90", test_lines)
        
        ! Initialize coverage data - this allocates this%files
        call coverage_data%init(test_files)
        
        ! **When**: Call init again on same object (double allocation scenario)
        ! This should NOT crash - previously caused "Attempting to allocate already allocated variable"
        
        ! **Then**: Second init call should handle already allocated this%files safely
        call coverage_data%init(test_files)
        
        ! Verify the data is still accessible and valid
        passed = allocated(coverage_data%files) .and. &
                 size(coverage_data%files) == 1 .and. &
                 coverage_data%files(1)%filename == "test.f90"
        
        if (.not. passed) then
            print *, "    FAILED: Double allocation not handled safely"
        else
            print *, "    PASSED: Double allocation handled without crash"
        end if
    end function test_double_allocation_prevention

    ! Test multiple init calls on same object (real-world crash scenario)
    function test_multiple_init_calls() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t), allocatable :: files_batch1(:), files_batch2(:), files_batch3(:)
        type(coverage_line_t), allocatable :: lines1(:), lines2(:), lines3(:)
        integer :: i
        
        print *, "  Test 2: Multiple init calls (real-world crash scenario)"
        
        ! **Given**: Simulate real-world usage where coverage_data might be re-initialized
        ! This happens when processing multiple .gcov files sequentially
        
        ! First batch of files
        allocate(lines1(3))
        do i = 1, 3
            call lines1(i)%init("batch1.f90", i, i, .true.)
        end do
        allocate(files_batch1(1))
        call files_batch1(1)%init("batch1.f90", lines1)
        
        ! Second batch of files
        allocate(lines2(2))
        do i = 1, 2
            call lines2(i)%init("batch2.f90", i, i*2, .true.)
        end do
        allocate(files_batch2(1))
        call files_batch2(1)%init("batch2.f90", lines2)
        
        ! Third batch of files
        allocate(lines3(4))
        do i = 1, 4
            call lines3(i)%init("batch3.f90", i, i*3, .true.)
        end do
        allocate(files_batch3(1))
        call files_batch3(1)%init("batch3.f90", lines3)
        
        ! **When**: Initialize coverage_data multiple times (simulates issue reproduction)
        call coverage_data%init(files_batch1)  ! First allocation
        call coverage_data%init(files_batch2)  ! Second allocation (double allocation)
        call coverage_data%init(files_batch3)  ! Third allocation (triple allocation)
        
        ! **Then**: All calls should succeed without memory crashes
        passed = allocated(coverage_data%files) .and. &
                 size(coverage_data%files) == 1 .and. &
                 coverage_data%files(1)%filename == "batch3.f90" .and. &
                 size(coverage_data%files(1)%lines) == 4
        
        if (.not. passed) then
            print *, "    FAILED: Multiple init calls caused issues"
        else
            print *, "    PASSED: Multiple init calls handled safely"
        end if
    end function test_multiple_init_calls

    ! Test coverage file processing memory lifecycle
    function test_coverage_processing_lifecycle() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t), allocatable :: files_set(:)
        type(coverage_line_t), allocatable :: lines_set(:)
        character(len=:), allocatable :: serialized
        integer :: i, j
        
        print *, "  Test 3: Coverage file processing memory lifecycle"
        
        ! **Given**: Simulate full coverage processing workflow
        
        ! Create multiple files with varying line counts (stress test)
        allocate(files_set(3))
        
        ! File 1: Small file
        allocate(lines_set(5))
        do i = 1, 5
            call lines_set(i)%init("small.f90", i, i*2, .true.)
        end do
        call files_set(1)%init("small.f90", lines_set)
        deallocate(lines_set)
        
        ! File 2: Medium file
        allocate(lines_set(15))
        do i = 1, 15
            call lines_set(i)%init("medium.f90", i, mod(i,3), .true.)
        end do
        call files_set(2)%init("medium.f90", lines_set)
        deallocate(lines_set)
        
        ! File 3: Large file
        allocate(lines_set(50))
        do i = 1, 50
            call lines_set(i)%init("large.f90", i, mod(i,5), .true.)
        end do
        call files_set(3)%init("large.f90", lines_set)
        
        ! **When**: Process through complete workflow (init -> serialize -> re-init)
        call coverage_data%init(files_set)
        serialized = coverage_data%serialize()
        call coverage_data%init()  ! Re-initialize with empty data
        call coverage_data%init(files_set)  ! Re-initialize with data again
        
        ! **Then**: Memory should be stable throughout lifecycle
        passed = allocated(coverage_data%files) .and. &
                 size(coverage_data%files) == 3 .and. &
                 len(serialized) > 100
        
        ! Validate all files are intact
        do i = 1, size(coverage_data%files)
            if (.not. allocated(coverage_data%files(i)%lines)) then
                passed = .false.
                exit
            end if
        end do
        
        if (.not. passed) then
            print *, "    FAILED: Memory lifecycle issues detected"
        else
            print *, "    PASSED: Memory lifecycle stable"
        end if
    end function test_coverage_processing_lifecycle

    ! Test memory safety during repeated file processing
    function test_repeated_file_processing() result(passed)
        logical :: passed
        type(coverage_data_t) :: coverage_data
        type(coverage_file_t), allocatable :: temp_files(:)
        type(coverage_line_t), allocatable :: temp_lines(:)
        integer :: iteration, line_count
        character(len=20) :: filename
        
        print *, "  Test 4: Repeated file processing (stress test)"
        
        ! **Given**: Simulate processing many .gcov files in sequence
        ! This is the exact scenario where Issue #178 manifests
        
        passed = .true.
        
        ! **When**: Process files repeatedly with different sizes
        do iteration = 1, 10
            line_count = iteration * 2  ! Varying file sizes
            
            allocate(temp_lines(line_count))
            do line_count = 1, size(temp_lines)
                call temp_lines(line_count)%init("test.f90", line_count, iteration, .true.)
            end do
            
            write(filename, '(A,I0,A)') "file_", iteration, ".f90"
            allocate(temp_files(1))
            call temp_files(1)%init(trim(filename), temp_lines)
            
            ! This init call should not crash (Issue #178 reproduction)
            call coverage_data%init(temp_files)
            
            ! Verify state after each iteration
            if (.not. allocated(coverage_data%files) .or. &
                size(coverage_data%files) /= 1) then
                passed = .false.
                print *, "    FAILED: Iteration", iteration, "caused memory issues"
                exit
            end if
            
            deallocate(temp_files)
            deallocate(temp_lines)
        end do
        
        ! **Then**: All iterations should complete without crashes
        if (passed) then
            print *, "    PASSED: Repeated processing handled safely"
        end if
    end function test_repeated_file_processing

end program test_issue_178_double_allocation_crash