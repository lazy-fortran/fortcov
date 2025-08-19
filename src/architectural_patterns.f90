module architectural_patterns
    !! Architectural Patterns for Clean Module Decomposition
    !! 
    !! Provides common design patterns and interfaces to support
    !! consistent architectural decomposition across modules.
    use foundation_constants
    use foundation_layer_utils
    implicit none
    private
    
    ! Abstract interface patterns for decomposed modules
    public :: processor_interface_t
    public :: validator_interface_t
    public :: parser_interface_t
    public :: generator_interface_t
    public :: orchestrator_interface_t
    
    ! Factory patterns for module creation
    public :: create_processor
    public :: create_validator
    public :: create_parser
    public :: create_generator
    
    ! Common architectural patterns
    public :: command_pattern_t
    public :: strategy_pattern_t
    public :: observer_pattern_t
    public :: factory_pattern_t
    
    ! Abstract processor interface for data transformation
    type, abstract :: processor_interface_t
    contains
        procedure(process_interface), deferred :: process
        procedure(validate_input_interface), deferred :: validate_input
        procedure(cleanup_interface), deferred :: cleanup
    end type processor_interface_t
    
    ! Abstract validator interface for input validation
    type, abstract :: validator_interface_t
    contains
        procedure(validate_interface), deferred :: validate
        procedure(get_error_message_interface), deferred :: get_error_message
    end type validator_interface_t
    
    ! Abstract parser interface for data parsing
    type, abstract :: parser_interface_t
    contains
        procedure(parse_interface), deferred :: parse
        procedure(is_valid_format_interface), deferred :: is_valid_format
        procedure(get_parsed_data_interface), deferred :: get_parsed_data
    end type parser_interface_t
    
    ! Abstract generator interface for output generation
    type, abstract :: generator_interface_t
    contains
        procedure(generate_interface), deferred :: generate
        procedure(set_template_interface), deferred :: set_template
        procedure(validate_output_interface), deferred :: validate_output
    end type generator_interface_t
    
    ! Abstract orchestrator interface for workflow coordination
    type, abstract :: orchestrator_interface_t
    contains
        procedure(orchestrate_interface), deferred :: orchestrate
        procedure(setup_workflow_interface), deferred :: setup_workflow
        procedure(teardown_workflow_interface), deferred :: teardown_workflow
    end type orchestrator_interface_t
    
    ! Command pattern for encapsulating operations
    type :: command_pattern_t
        character(len=:), allocatable :: command_name
        character(len=:), allocatable :: parameters(:)
        logical :: executed = .false.
        type(status_result_t) :: result
    contains
        procedure :: execute => execute_command
        procedure :: undo => undo_command
        procedure :: can_undo => command_can_undo
    end type command_pattern_t
    
    ! Strategy pattern for algorithmic flexibility
    type :: strategy_pattern_t
        character(len=:), allocatable :: strategy_name
        character(len=:), allocatable :: algorithm_type
        type(status_result_t) :: execution_result
    contains
        procedure :: execute_strategy
        procedure :: set_algorithm => set_strategy_algorithm
    end type strategy_pattern_t
    
    ! Observer pattern for event notification
    type :: observer_pattern_t
        character(len=:), allocatable :: observer_name
        character(len=:), allocatable :: events_subscribed(:)
        logical :: active = .true.
    contains
        procedure :: notify => notify_observer
        procedure :: subscribe => subscribe_to_event
        procedure :: unsubscribe => unsubscribe_from_event
    end type observer_pattern_t
    
    ! Factory pattern for object creation
    type :: factory_pattern_t
        character(len=:), allocatable :: factory_type
        character(len=:), allocatable :: supported_types(:)
    contains
        procedure :: create_instance => factory_create_instance
        procedure :: supports_type => factory_supports_type
    end type factory_pattern_t
    
    ! Abstract interfaces for deferred procedures
    abstract interface
        function process_interface(this, input_data) result(output_data)
            import :: processor_interface_t
            class(processor_interface_t), intent(inout) :: this
            class(*), intent(in) :: input_data
            class(*), allocatable :: output_data
        end function process_interface
        
        function validate_input_interface(this, input_data) result(is_valid)
            import :: processor_interface_t
            class(processor_interface_t), intent(in) :: this
            class(*), intent(in) :: input_data
            logical :: is_valid
        end function validate_input_interface
        
        subroutine cleanup_interface(this)
            import :: processor_interface_t
            class(processor_interface_t), intent(inout) :: this
        end subroutine cleanup_interface
        
        function validate_interface(this, data) result(is_valid)
            import :: validator_interface_t
            class(validator_interface_t), intent(in) :: this
            class(*), intent(in) :: data
            logical :: is_valid
        end function validate_interface
        
        function get_error_message_interface(this) result(message)
            import :: validator_interface_t
            class(validator_interface_t), intent(in) :: this
            character(len=:), allocatable :: message
        end function get_error_message_interface
        
        function parse_interface(this, input) result(success)
            import :: parser_interface_t
            class(parser_interface_t), intent(inout) :: this
            character(len=*), intent(in) :: input
            logical :: success
        end function parse_interface
        
        function is_valid_format_interface(this, input) result(is_valid)
            import :: parser_interface_t
            class(parser_interface_t), intent(in) :: this
            character(len=*), intent(in) :: input
            logical :: is_valid
        end function is_valid_format_interface
        
        function get_parsed_data_interface(this) result(data)
            import :: parser_interface_t
            class(parser_interface_t), intent(in) :: this
            class(*), allocatable :: data
        end function get_parsed_data_interface
        
        function generate_interface(this, input_data) result(output)
            import :: generator_interface_t
            class(generator_interface_t), intent(inout) :: this
            class(*), intent(in) :: input_data
            character(len=:), allocatable :: output
        end function generate_interface
        
        subroutine set_template_interface(this, template)
            import :: generator_interface_t
            class(generator_interface_t), intent(inout) :: this
            character(len=*), intent(in) :: template
        end subroutine set_template_interface
        
        function validate_output_interface(this, output) result(is_valid)
            import :: generator_interface_t
            class(generator_interface_t), intent(in) :: this
            character(len=*), intent(in) :: output
            logical :: is_valid
        end function validate_output_interface
        
        function orchestrate_interface(this, components) result(success)
            import :: orchestrator_interface_t
            class(orchestrator_interface_t), intent(inout) :: this
            class(*), intent(in) :: components
            logical :: success
        end function orchestrate_interface
        
        subroutine setup_workflow_interface(this)
            import :: orchestrator_interface_t
            class(orchestrator_interface_t), intent(inout) :: this
        end subroutine setup_workflow_interface
        
        subroutine teardown_workflow_interface(this)
            import :: orchestrator_interface_t
            class(orchestrator_interface_t), intent(inout) :: this
        end subroutine teardown_workflow_interface
    end interface
    
contains
    
    ! Factory functions for creating pattern instances
    function create_processor(processor_type) result(processor)
        character(len=*), intent(in) :: processor_type
        class(processor_interface_t), allocatable :: processor
        
        ! This would be implemented with concrete processor types
        ! For now, just validate the pattern
        if (len_trim(processor_type) > 0) then
            ! Processor creation logic would go here
        end if
    end function create_processor
    
    function create_validator(validator_type) result(validator)
        character(len=*), intent(in) :: validator_type
        class(validator_interface_t), allocatable :: validator
        
        ! This would be implemented with concrete validator types
        if (len_trim(validator_type) > 0) then
            ! Validator creation logic would go here
        end if
    end function create_validator
    
    function create_parser(parser_type) result(parser)
        character(len=*), intent(in) :: parser_type
        class(parser_interface_t), allocatable :: parser
        
        ! This would be implemented with concrete parser types
        if (len_trim(parser_type) > 0) then
            ! Parser creation logic would go here
        end if
    end function create_parser
    
    function create_generator(generator_type) result(generator)
        character(len=*), intent(in) :: generator_type
        class(generator_interface_t), allocatable :: generator
        
        ! This would be implemented with concrete generator types
        if (len_trim(generator_type) > 0) then
            ! Generator creation logic would go here
        end if
    end function create_generator
    
    ! Command pattern implementation
    subroutine execute_command(this)
        class(command_pattern_t), intent(inout) :: this
        
        if (.not. this%executed) then
            this%executed = .true.
            this%result%success = .true.
            this%result%error_code = FOUNDATION_SUCCESS
        end if
    end subroutine execute_command
    
    subroutine undo_command(this)
        class(command_pattern_t), intent(inout) :: this
        
        if (this%executed) then
            this%executed = .false.
        end if
    end subroutine undo_command
    
    function command_can_undo(this) result(can_undo)
        class(command_pattern_t), intent(in) :: this
        logical :: can_undo
        
        can_undo = this%executed
    end function command_can_undo
    
    ! Strategy pattern implementation
    subroutine execute_strategy(this)
        class(strategy_pattern_t), intent(inout) :: this
        
        this%execution_result%success = .true.
        this%execution_result%error_code = FOUNDATION_SUCCESS
    end subroutine execute_strategy
    
    subroutine set_strategy_algorithm(this, algorithm)
        class(strategy_pattern_t), intent(inout) :: this
        character(len=*), intent(in) :: algorithm
        
        this%algorithm_type = algorithm
    end subroutine set_strategy_algorithm
    
    ! Observer pattern implementation
    subroutine notify_observer(this, event, data)
        class(observer_pattern_t), intent(inout) :: this
        character(len=*), intent(in) :: event
        class(*), intent(in), optional :: data
        
        if (this%active .and. allocated(this%events_subscribed)) then
            if (any(this%events_subscribed == event)) then
                ! Handle notification
            end if
        end if
    end subroutine notify_observer
    
    subroutine subscribe_to_event(this, event)
        class(observer_pattern_t), intent(inout) :: this
        character(len=*), intent(in) :: event
        character(len=:), allocatable :: temp_events(:)
        integer :: n
        
        if (allocated(this%events_subscribed)) then
            n = size(this%events_subscribed)
            allocate(character(len=len(event)) :: temp_events(n + 1))
            temp_events(1:n) = this%events_subscribed
            temp_events(n + 1) = event
            call move_alloc(temp_events, this%events_subscribed)
        else
            allocate(character(len=len(event)) :: this%events_subscribed(1))
            this%events_subscribed(1) = event
        end if
    end subroutine subscribe_to_event
    
    subroutine unsubscribe_from_event(this, event)
        class(observer_pattern_t), intent(inout) :: this
        character(len=*), intent(in) :: event
        character(len=:), allocatable :: temp_events(:)
        integer :: i, j, n
        
        if (allocated(this%events_subscribed)) then
            n = size(this%events_subscribed)
            if (n > 1) then
                allocate(character(len=len(this%events_subscribed(1))) :: temp_events(n - 1))
                j = 0
                do i = 1, n
                    if (this%events_subscribed(i) /= event) then
                        j = j + 1
                        if (j <= n - 1) then
                            temp_events(j) = this%events_subscribed(i)
                        end if
                    end if
                end do
                call move_alloc(temp_events, this%events_subscribed)
            else
                deallocate(this%events_subscribed)
            end if
        end if
    end subroutine unsubscribe_from_event
    
    ! Factory pattern implementation
    function factory_create_instance(this, instance_type) result(success)
        class(factory_pattern_t), intent(in) :: this
        character(len=*), intent(in) :: instance_type
        logical :: success
        
        success = this%supports_type(instance_type)
    end function factory_create_instance
    
    function factory_supports_type(this, instance_type) result(supports)
        class(factory_pattern_t), intent(in) :: this
        character(len=*), intent(in) :: instance_type
        logical :: supports
        
        supports = .false.
        if (allocated(this%supported_types)) then
            supports = any(this%supported_types == instance_type)
        end if
    end function factory_supports_type
    
end module architectural_patterns