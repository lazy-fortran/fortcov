! Memory safety test for atomic temp file manager
! Tests for malloc corruption and memory leaks
program test_memory_safety_atomic
    use atomic_temp_file_manager
    use error_handling
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_tests_passed = .true.
    
    call run_memory_safety_tests()
    
    if (all_tests_passed) then
        write(*, '(A,I0,A,I0,A)') 'PASSED: ', passed_count, '/', test_count, &
            ' memory safety tests passed'
        stop 0
    else
        write(*, '(A,I0,A,I0,A)') 'FAILED: ', passed_count, '/', &
            test_count, ' memory safety tests passed'
        stop 1
    end if

contains

    subroutine run_memory_safety_tests()
        call test_rapid_creation_cleanup()
        call test_finalizer_memory_safety()
        call test_error_path_memory_safety()
        call test_large_content_memory_safety()
    end subroutine run_memory_safety_tests

    subroutine test_rapid_creation_cleanup()
        integer, parameter :: ITERATIONS = 200
        integer :: i
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        
        call start_test('test_rapid_creation_cleanup')
        
        ! Rapidly create and cleanup to stress memory management
        do i = 1, ITERATIONS
            call temp_file%create_secure(error_ctx, success)
            call assert(success, 'Failed to create temp file in rapid test')
            
            call temp_file%write_atomic('test content', error_ctx, success)
            call assert(success, 'Failed to write in rapid test')
            
            call temp_file%cleanup()
        end do
        
        call end_test()
    end subroutine test_rapid_creation_cleanup

    subroutine test_finalizer_memory_safety()
        integer, parameter :: ITERATIONS = 100
        integer :: i
        
        call start_test('test_finalizer_memory_safety')
        
        ! Create temp files in local scope to test finalizer
        do i = 1, ITERATIONS
            block
                type(secure_temp_file_t) :: temp_file
                type(error_context_t) :: error_ctx
                logical :: success
                
                call temp_file%create_secure(error_ctx, success)
                call assert(success, 'Failed to create temp file for finalizer test')
                
                call temp_file%write_atomic('finalizer test data', error_ctx, success)
                call assert(success, 'Failed to write for finalizer test')
                
                ! Let finalizer handle cleanup automatically
            end block
        end do
        
        call end_test()
    end subroutine test_finalizer_memory_safety

    subroutine test_error_path_memory_safety()
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        
        call start_test('test_error_path_memory_safety')
        
        call temp_file%create_secure(error_ctx, success)
        call assert(success, 'Failed to create temp file for error path test')
        
        ! Test double cleanup (should be safe)
        call temp_file%cleanup()
        call temp_file%cleanup()  ! This should not crash
        
        ! Test operations on cleaned up file (should fail gracefully)
        call temp_file%write_atomic('should fail', error_ctx, success)
        call assert(.not. success, 'Write should fail on cleaned up file')
        
        call end_test()
    end subroutine test_error_path_memory_safety

    subroutine test_large_content_memory_safety()
        type(secure_temp_file_t) :: temp_file
        type(error_context_t) :: error_ctx
        logical :: success
        character(len=10000) :: large_content
        integer :: i
        
        call start_test('test_large_content_memory_safety')
        
        ! Create large content string
        do i = 1, len(large_content)
            large_content(i:i) = char(mod(i, 26) + 65)  ! A-Z pattern
        end do
        
        call temp_file%create_secure(error_ctx, success)
        call assert(success, 'Failed to create temp file for large content test')
        
        call temp_file%write_atomic(large_content, error_ctx, success)
        call assert(success, 'Failed to write large content')
        
        call temp_file%cleanup()
        
        call end_test()
    end subroutine test_large_content_memory_safety

    ! Test framework helpers
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A,A)', advance='no') 'Running ', test_name, '... '
    end subroutine start_test

    subroutine end_test()
        passed_count = passed_count + 1
        write(*, '(A)') 'PASSED'
    end subroutine end_test

    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        
        if (.not. condition) then
            write(*, '(A)') 'FAILED'
            write(*, '(A,A)') 'Assertion failed: ', message
            all_tests_passed = .false.
        end if
    end subroutine assert

end program test_memory_safety_atomic