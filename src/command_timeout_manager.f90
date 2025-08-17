module command_timeout_manager
    !! Command execution timeout protection module
    !! 
    !! This module provides timeout protection for command execution to prevent
    !! DoS attacks and resource exhaustion. It implements configurable timeouts
    !! with cross-platform process management abstractions.
    use iso_c_binding
    use error_handling
    implicit none
    private
    
    ! Status enumeration
    integer, parameter, public :: STATUS_PENDING = 0
    integer, parameter, public :: STATUS_RUNNING = 1
    integer, parameter, public :: STATUS_COMPLETED = 2
    integer, parameter, public :: STATUS_TIMEOUT = 3
    integer, parameter, public :: STATUS_ERROR = 4
    integer, parameter, public :: STATUS_TERMINATED = 5
    
    ! Core timeout management type
    type, public :: timeout_command_executor_t
        character(len=:), allocatable :: command
        integer :: timeout_seconds = 30
        integer(c_int) :: process_id = -1
        real(8) :: start_time = 0.0_8
        integer :: status = STATUS_PENDING
        logical :: is_background = .false.
        character(len=:), allocatable :: working_directory
        type(c_ptr) :: process_handle = c_null_ptr
    end type timeout_command_executor_t
    
    ! C interface bindings (Unix-focused implementation)
    interface
        function create_process_with_timeout_c(command, working_dir, &
                                              timeout_seconds, handle) &
                                              bind(c, &
                                              name='create_process_with_timeout')
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: command(*)
            character(kind=c_char), intent(in) :: working_dir(*)
            integer(c_int), value, intent(in) :: timeout_seconds
            type(c_ptr), value :: handle
            integer(c_int) :: create_process_with_timeout_c
        end function create_process_with_timeout_c
        
        function monitor_process_timeout_c(handle, timed_out) &
                                          bind(c, name='monitor_process_timeout')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int), intent(out) :: timed_out
            integer(c_int) :: monitor_process_timeout_c
        end function monitor_process_timeout_c
        
        function terminate_process_tree_c(handle, graceful) &
                                         bind(c, name='terminate_process_tree')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int), value, intent(in) :: graceful
            integer(c_int) :: terminate_process_tree_c
        end function terminate_process_tree_c
        
        function cleanup_process_resources_c(handle) &
                                            bind(c, name='cleanup_process_resources')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int) :: cleanup_process_resources_c
        end function cleanup_process_resources_c
    end interface
    
    ! C memory allocation interfaces
    interface
        function c_malloc(size) bind(c, name='malloc')
            import :: c_ptr, c_size_t
            integer(c_size_t), value :: size
            type(c_ptr) :: c_malloc
        end function c_malloc
        
        subroutine c_free(ptr) bind(c, name='free')
            import :: c_ptr
            type(c_ptr), value :: ptr
        end subroutine c_free
        
        function get_process_handle_size() bind(c, name='get_process_handle_size')
            import :: c_size_t
            integer(c_size_t) :: get_process_handle_size
        end function get_process_handle_size
    end interface
    
    ! Public interfaces
    public :: execute_with_timeout
    public :: monitor_timeout
    public :: terminate_process_tree
    public :: cleanup_process_resources
    public :: create_timeout_executor
    public :: destroy_timeout_executor

contains

    ! Create timeout executor with validation
    subroutine create_timeout_executor(executor, timeout_seconds, error_ctx)
        type(timeout_command_executor_t), intent(out) :: executor
        integer, intent(in) :: timeout_seconds
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        ! Validate timeout value
        if (timeout_seconds <= 0 .or. timeout_seconds > 86400) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Invalid timeout value: must be between 1 and 86400 seconds")
            call safe_write_suggestion(error_ctx, &
                "Use a reasonable timeout value (1-86400 seconds)")
            call safe_write_context(error_ctx, "Timeout executor creation")
            return
        end if
        
        ! Initialize executor
        executor%timeout_seconds = timeout_seconds
        executor%process_id = -1
        executor%start_time = 0.0_8
        executor%status = STATUS_PENDING
        executor%is_background = .false.
        executor%process_handle = c_null_ptr
        
        ! Allocate working directory to current by default
        executor%working_directory = "."
    end subroutine create_timeout_executor
    
    ! Execute command with timeout protection
    subroutine execute_with_timeout(executor, command, error_ctx)
        type(timeout_command_executor_t), intent(inout) :: executor
        character(len=*), intent(in) :: command
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: result
        integer(c_int) :: timed_out
        real(8) :: elapsed_time
        
        call clear_error_context(error_ctx)
        
        ! Validate inputs
        if (len_trim(command) == 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, "Empty command provided")
            call safe_write_suggestion(error_ctx, "Provide a valid command")
            call safe_write_context(error_ctx, "Command execution")
            executor%status = STATUS_ERROR
            return
        end if
        
        ! Security check - prevent shell injection
        if (contains_dangerous_chars(command)) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Command contains potentially dangerous characters")
            call safe_write_suggestion(error_ctx, &
                "Use safe command syntax without shell metacharacters")
            call safe_write_context(error_ctx, "Security validation")
            executor%status = STATUS_ERROR
            return
        end if
        
        ! Store command and update status
        executor%command = trim(command)
        executor%status = STATUS_RUNNING
        executor%start_time = get_wall_time()
        
        ! Allocate C handle structure using proper size - SECURITY FIX
        executor%process_handle = c_malloc(get_process_handle_size())
        if (.not. c_associated(executor%process_handle)) then
            error_ctx%error_code = ERROR_OUT_OF_MEMORY
            call safe_write_message(error_ctx, "Failed to allocate process handle")
            executor%status = STATUS_ERROR
            return
        end if
        
        ! Create process with timeout
        result = create_process_with_timeout_c( &
            c_char_array_from_string(executor%command), &
            c_char_array_from_string(executor%working_directory), &
            int(executor%timeout_seconds, c_int), &
            executor%process_handle)
        
        if (result /= 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Failed to create process for command: " // executor%command)
            call safe_write_suggestion(error_ctx, &
                "Check command syntax and system resources")
            call safe_write_context(error_ctx, "Process creation")
            executor%status = STATUS_ERROR
            call c_free(executor%process_handle)
            executor%process_handle = c_null_ptr
            return
        end if
        
        ! Monitor process until completion or timeout - EVENT-DRIVEN, NO BUSY WAIT
        ! Single call to C function which handles all waiting with select()
        result = monitor_process_timeout_c(executor%process_handle, timed_out)
        if (result /= 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, "Process monitoring failed")
            executor%status = STATUS_ERROR
        else if (timed_out == 1) then
            ! Timeout occurred - terminate process
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Command timed out after " // &
                trim(int_to_string(executor%timeout_seconds)) // " seconds")
            executor%status = STATUS_TIMEOUT
            
            ! Attempt graceful termination
            result = terminate_process_tree_c(executor%process_handle, 1)
        else
            ! Process completed successfully
            executor%status = STATUS_COMPLETED
        end if
        
        ! Cleanup process resources
        if (c_associated(executor%process_handle)) then
            result = cleanup_process_resources_c(executor%process_handle)
            call c_free(executor%process_handle)
            executor%process_handle = c_null_ptr
        end if
    end subroutine execute_with_timeout
    
    ! Monitor timeout for background processes
    subroutine monitor_timeout(executor, error_ctx)
        type(timeout_command_executor_t), intent(inout) :: executor
        type(error_context_t), intent(out) :: error_ctx
        
        integer(c_int) :: timed_out
        integer :: result
        
        call clear_error_context(error_ctx)
        
        if (.not. c_associated(executor%process_handle)) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, "No active process to monitor")
            return
        end if
        
        result = monitor_process_timeout_c(executor%process_handle, timed_out)
        if (result /= 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, "Process monitoring failed")
            return
        end if
        
        if (timed_out == 1) then
            executor%status = STATUS_TIMEOUT
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, &
                "Command timed out after " // &
                trim(int_to_string(executor%timeout_seconds)) // " seconds")
        end if
    end subroutine monitor_timeout
    
    ! Terminate process tree
    subroutine terminate_process_tree(executor, error_ctx)
        type(timeout_command_executor_t), intent(inout) :: executor
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: result
        
        call clear_error_context(error_ctx)
        
        if (.not. c_associated(executor%process_handle)) then
            return  ! No process to terminate
        end if
        
        result = terminate_process_tree_c(executor%process_handle, 1)
        if (result /= 0) then
            error_ctx%error_code = ERROR_INVALID_CONFIG
            call safe_write_message(error_ctx, "Failed to terminate process tree")
            call safe_write_suggestion(error_ctx, &
                "Check system permissions and process status")
            call safe_write_context(error_ctx, "Process termination")
        else
            executor%status = STATUS_TERMINATED
        end if
    end subroutine terminate_process_tree
    
    ! Clean up process resources
    subroutine cleanup_process_resources(executor, error_ctx)
        type(timeout_command_executor_t), intent(inout) :: executor
        type(error_context_t), intent(out) :: error_ctx
        
        integer :: result
        
        call clear_error_context(error_ctx)
        
        if (c_associated(executor%process_handle)) then
            result = cleanup_process_resources_c(executor%process_handle)
            if (result /= 0) then
                error_ctx%error_code = ERROR_INVALID_CONFIG
                call safe_write_message(error_ctx, &
                    "Failed to cleanup process resources")
            end if
            call c_free(executor%process_handle)
            executor%process_handle = c_null_ptr
        end if
        
        ! Reset executor state
        executor%process_id = -1
        executor%status = STATUS_PENDING
        executor%start_time = 0.0_8
    end subroutine cleanup_process_resources
    
    ! Destroy timeout executor
    subroutine destroy_timeout_executor(executor, error_ctx)
        type(timeout_command_executor_t), intent(inout) :: executor
        type(error_context_t), intent(out) :: error_ctx
        
        call clear_error_context(error_ctx)
        
        ! Cleanup any active process
        call cleanup_process_resources(executor, error_ctx)
        
        ! Deallocate strings
        if (allocated(executor%command)) deallocate(executor%command)
        if (allocated(executor%working_directory)) then
            deallocate(executor%working_directory)
        end if
    end subroutine destroy_timeout_executor
    
    ! Helper functions
    
    ! Get wall clock time in seconds
    function get_wall_time() result(time_seconds)
        real(8) :: time_seconds
        integer :: count, count_rate
        
        call system_clock(count, count_rate)
        time_seconds = real(count, 8) / real(count_rate, 8)
    end function get_wall_time
    
    ! Sleep function removed - was causing DoS vulnerability
    ! Event-driven monitoring in C eliminates need for sleep delays
    
    ! Convert Fortran string to C char array
    function c_char_array_from_string(f_string) result(c_array)
        character(len=*), intent(in) :: f_string
        character(kind=c_char), allocatable :: c_array(:)
        integer :: i, n
        
        n = len_trim(f_string)
        allocate(c_array(n+1))
        
        do i = 1, n
            c_array(i) = f_string(i:i)
        end do
        c_array(n+1) = c_null_char
    end function c_char_array_from_string
    
    ! Check for dangerous characters in command
    function contains_dangerous_chars(command) result(dangerous)
        character(len=*), intent(in) :: command
        logical :: dangerous
        integer :: i, consecutive_count, cmd_len
        character(len=1) :: prev_char, curr_char
        
        dangerous = .false.
        cmd_len = len_trim(command)
        
        ! SECURITY FIX: Check command length (max 256 characters)
        if (cmd_len > 256) then
            dangerous = .true.
            return
        end if
        
        ! SECURITY FIX: Check for repetitive patterns (>20 consecutive chars)
        consecutive_count = 1
        if (cmd_len > 1) then
            prev_char = command(1:1)
            do i = 2, cmd_len
                curr_char = command(i:i)
                if (curr_char == prev_char) then
                    consecutive_count = consecutive_count + 1
                    if (consecutive_count > 20) then
                        dangerous = .true.
                        return
                    end if
                else
                    consecutive_count = 1
                    prev_char = curr_char
                end if
            end do
        end if
        
        ! Check for shell injection patterns
        if (index(command, ';') > 0 .or. index(command, '&&') > 0 .or. &
            index(command, '||') > 0 .or. index(command, '`') > 0 .or. &
            index(command, '$()') > 0 .or. index(command, '${') > 0) then
            dangerous = .true.
        end if
        
        ! SECURITY FIX Issue #148: Check for escape sequence injection
        if (index(command, '\n') > 0 .or. index(command, '\t') > 0 .or. &
            index(command, '\r') > 0 .or. index(command, '\v') > 0 .or. &
            index(command, '\f') > 0 .or. index(command, '\b') > 0 .or. &
            index(command, '\a') > 0 .or. index(command, '\\') > 0) then
            dangerous = .true.
        end if
        
        ! Check for suspicious redirection attempts
        if (index(command, '> /') > 0 .or. index(command, '>> /') > 0 .or. &
            index(command, '| rm') > 0 .or. index(command, '| sudo') > 0) then
            dangerous = .true.
        end if
    end function contains_dangerous_chars
    
    ! Simple integer to string conversion
    function int_to_string(int_val) result(str_val)
        integer, intent(in) :: int_val
        character(len=:), allocatable :: str_val
        character(len=32) :: temp_str
        
        write(temp_str, '(I0)') int_val
        str_val = trim(temp_str)
    end function int_to_string

end module command_timeout_manager
