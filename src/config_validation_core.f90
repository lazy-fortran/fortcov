module config_validation_core
    use config_types, only: config_t
    use config_validators_impl
    implicit none
    private
    
    ! Public procedures
    public :: validate_complete_config
    
contains

    function validate_complete_config(config) result(is_valid)
        !! Validate entire configuration
        type(config_t), intent(in) :: config
        logical :: is_valid

        character(len=512) :: error_message
        logical :: partial_valid

        is_valid = .true.

        ! Skip validation for help/version modes
        if (config%show_help .or. config%show_version) then
            return
        end if

        ! Validate input sources
        call validate_input_sources(config, partial_valid, error_message)
        if (.not. partial_valid) then
            if (config%verbose) then
                print '(A)', "Validation error: " // trim(error_message)
            end if
            is_valid = .false.
            return
        end if

        ! Validate output settings
        call validate_output_settings(config, partial_valid, error_message)
        if (.not. partial_valid) then
            if (config%verbose) then
                print '(A)', "Validation error: " // trim(error_message)
            end if
            is_valid = .false.
            return
        end if

        ! Validate threshold settings
        call validate_threshold_settings(config, partial_valid, error_message)
        if (.not. partial_valid) then
            if (config%verbose) then
                print '(A)', "Validation error: " // trim(error_message)
            end if
            is_valid = .false.
            return
        end if

        ! Validate diff configuration if enabled
        if (config%enable_diff) then
            call validate_diff_configuration(config, partial_valid, error_message)
            if (.not. partial_valid) then
                if (config%verbose) then
                    print '(A)', "Validation error: " // trim(error_message)
                end if
                is_valid = .false.
                return
            end if
        end if

        ! Validate import configuration if specified
        if (len_trim(config%import_file) > 0) then
            call validate_import_configuration(config, partial_valid, error_message)
            if (.not. partial_valid) then
                if (config%verbose) then
                    print '(A)', "Validation error: " // trim(error_message)
                end if
                is_valid = .false.
                return
            end if
        end if

        ! Validate thread count
        if (config%threads < 1 .or. config%threads > 256) then
            if (config%verbose) then
                print '(A)', "Validation error: Thread count must be between 1 and 256"
            end if
            is_valid = .false.
        end if

    end function validate_complete_config

end module config_validation_core