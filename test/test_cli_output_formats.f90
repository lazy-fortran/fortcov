program test_cli_output_formats
    use fortcov_config
    use coverage_reporter
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running CLI output format tests..."
    
    ! Test 1: Validate config accepts advertised formats
    call test_validate_config_accepts_json()
    call test_validate_config_accepts_xml()
    call test_validate_config_accepts_markdown()
    call test_validate_config_accepts_md()
    
    ! Test 2: Create reporter now works for all advertised formats
    call test_create_reporter_json_works()
    call test_create_reporter_xml_works()
    call test_create_reporter_markdown_works()
    call test_create_reporter_md_works()
    
    ! Test 3: Help text consistency
    call test_help_text_matches_implementation()
    
    ! Report results
    write(*,*) ""
    write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
               pass_count, " (", (pass_count * 100) / test_count, "%)"
    
    if (pass_count /= test_count) then
        stop 1  ! Exit with error code
    end if
    
contains
    
    subroutine assert(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: expected, actual
        
        test_count = test_count + 1
        if (condition) then
            write(*,'(A,A)') "PASS: ", test_name
            pass_count = pass_count + 1
        else
            write(*,'(A,A)') "FAIL: ", test_name
            write(*,'(A,A)') "  Expected: ", expected
            write(*,'(A,A)') "  Actual:   ", actual
        end if
    end subroutine assert
    
    ! Test validate_config accepts json format (Issue #85)
    ! Given: config with output_format = "json"
    ! When: Calling validate_config()
    ! Then: Should not raise validation error
    subroutine test_validate_config_accepts_json()
        use error_handling
        type(config_t) :: config
        type(error_context_t) :: error_ctx
        
        ! Given: Config with json output format
        call initialize_config(config)
        config%output_format = "json"
        
        ! When: Validate config
        call validate_config(config, error_ctx)
        
        ! Then: Should not have validation error
        call assert(error_ctx%error_code == ERROR_SUCCESS, &
                   "validate_config accepts json", "ERROR_SUCCESS", &
                   "validation error")
    end subroutine test_validate_config_accepts_json
    
    ! Test validate_config accepts xml format (Issue #85)
    ! Given: config with output_format = "xml"
    ! When: Calling validate_config()
    ! Then: Should not raise validation error
    subroutine test_validate_config_accepts_xml()
        use error_handling
        type(config_t) :: config
        type(error_context_t) :: error_ctx
        
        ! Given: Config with xml output format
        call initialize_config(config)
        config%output_format = "xml"
        
        ! When: Validate config
        call validate_config(config, error_ctx)
        
        ! Then: Should not have validation error
        call assert(error_ctx%error_code == ERROR_SUCCESS, &
                   "validate_config accepts xml", "ERROR_SUCCESS", &
                   "validation error")
    end subroutine test_validate_config_accepts_xml
    
    ! Test validate_config accepts markdown format
    ! Given: config with output_format = "markdown"
    ! When: Calling validate_config()
    ! Then: Should not raise validation error
    subroutine test_validate_config_accepts_markdown()
        use error_handling
        type(config_t) :: config
        type(error_context_t) :: error_ctx
        
        ! Given: Config with markdown output format
        call initialize_config(config)
        config%output_format = "markdown"
        
        ! When: Validate config
        call validate_config(config, error_ctx)
        
        ! Then: Should not have validation error
        call assert(error_ctx%error_code == ERROR_SUCCESS, &
                   "validate_config accepts markdown", "ERROR_SUCCESS", &
                   "validation error")
    end subroutine test_validate_config_accepts_markdown
    
    ! Test validate_config accepts md format
    ! Given: config with output_format = "md"
    ! When: Calling validate_config()
    ! Then: Should not raise validation error
    subroutine test_validate_config_accepts_md()
        use error_handling
        type(config_t) :: config
        type(error_context_t) :: error_ctx
        
        ! Given: Config with md output format
        call initialize_config(config)
        config%output_format = "md"
        
        ! When: Validate config
        call validate_config(config, error_ctx)
        
        ! Then: Should not have validation error
        call assert(error_ctx%error_code == ERROR_SUCCESS, &
                   "validate_config accepts md", "ERROR_SUCCESS", &
                   "validation error")
    end subroutine test_validate_config_accepts_md
    
    ! Test create_reporter works for json (Issue #85 - now fixed)
    ! Given: format = "json"
    ! When: Calling create_reporter()
    ! Then: Should set error_flag = .false.
    subroutine test_create_reporter_json_works()
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag
        
        ! Given: json format
        ! When: Create reporter
        call create_reporter("json", reporter, error_flag)
        
        ! Then: Should succeed (Issue #85 fixed)
        call assert(error_flag .eqv. .false., &
                   "create_reporter json works", ".false.", &
                   merge(".true. ", ".false.", error_flag))
    end subroutine test_create_reporter_json_works
    
    ! Test create_reporter works for xml (Issue #85 - now fixed)
    ! Given: format = "xml"
    ! When: Calling create_reporter()
    ! Then: Should set error_flag = .false.
    subroutine test_create_reporter_xml_works()
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag
        
        ! Given: xml format
        ! When: Create reporter
        call create_reporter("xml", reporter, error_flag)
        
        ! Then: Should succeed (Issue #85 fixed)
        call assert(error_flag .eqv. .false., &
                   "create_reporter xml works", ".false.", &
                   merge(".true. ", ".false.", error_flag))
    end subroutine test_create_reporter_xml_works
    
    ! Test create_reporter works for markdown
    ! Given: format = "markdown"
    ! When: Calling create_reporter()
    ! Then: Should set error_flag = .false.
    subroutine test_create_reporter_markdown_works()
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag
        
        ! Given: markdown format
        ! When: Create reporter
        call create_reporter("markdown", reporter, error_flag)
        
        ! Then: Should succeed
        call assert(error_flag .eqv. .false., &
                   "create_reporter markdown works", ".false.", &
                   merge(".true. ", ".false.", error_flag))
    end subroutine test_create_reporter_markdown_works
    
    ! Test create_reporter works for md
    ! Given: format = "md"
    ! When: Calling create_reporter()
    ! Then: Should set error_flag = .false.
    subroutine test_create_reporter_md_works()
        class(coverage_reporter_t), allocatable :: reporter
        logical :: error_flag
        
        ! Given: md format
        ! When: Create reporter
        call create_reporter("md", reporter, error_flag)
        
        ! Then: Should succeed
        call assert(error_flag .eqv. .false., &
                   "create_reporter md works", ".false.", &
                   merge(".true. ", ".false.", error_flag))
    end subroutine test_create_reporter_md_works
    
    ! Test help text vs implementation consistency (Issue #85 - now fixed)
    ! Given: Help text advertises json, xml, markdown, md
    ! When: Implementation supports all advertised formats
    ! Then: All formats should work correctly
    subroutine test_help_text_matches_implementation()
        class(coverage_reporter_t), allocatable :: reporter
        logical :: json_error, xml_error, md_error, markdown_error
        
        ! Test what create_reporter actually supports
        call create_reporter("json", reporter, json_error)
        call create_reporter("xml", reporter, xml_error)
        call create_reporter("markdown", reporter, markdown_error)
        call create_reporter("md", reporter, md_error)
        
        ! All advertised formats should work (Issue #85 fixed)
        call assert(.not. json_error .and. .not. xml_error .and. &
                   .not. markdown_error .and. .not. md_error, &
                   "help text matches implementation", &
                   "all formats work", &
                   "all formats implemented")
    end subroutine test_help_text_matches_implementation
    
end program test_cli_output_formats