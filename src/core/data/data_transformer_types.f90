module data_transformer_types
    implicit none
    private
    
    ! Public types
    public :: transformed_data_t
    public :: source_file_t
    public :: annotated_line_t
    public :: navigation_tree_t
    public :: large_file_streamer_t
    public :: coverage_summary_t
    public :: navigation_node_t
    public :: file_cache_entry_t
    
    ! Type definitions for data transformation
    type :: coverage_summary_t
        integer :: total_lines = 0
        integer :: covered_lines = 0
        integer :: total_files = 0
        real :: coverage_percentage = 0.0
    contains
        procedure :: init => coverage_summary_init
    end type coverage_summary_t
    
    type :: annotated_line_t
        integer :: line_number = 0
        integer :: execution_count = 0
        logical :: is_covered = .false.
        logical :: is_executable = .false.
        character(len=:), allocatable :: content
        character(len=:), allocatable :: css_class
        character(len=:), allocatable :: ansi_sequence
    contains
        procedure :: init => annotated_line_init
    end type annotated_line_t
    
    type :: source_file_t
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: content
        type(annotated_line_t), allocatable :: lines(:)
        real :: coverage_percentage = 0.0
    contains
        procedure :: init => source_file_init
    end type source_file_t
    
    type :: navigation_node_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: path
        logical :: is_directory = .false.
        real :: coverage_percentage = 0.0
    contains
        procedure :: init => navigation_node_init
    end type navigation_node_t
    
    type :: navigation_tree_t
        type(navigation_node_t), allocatable :: nodes(:)
        logical :: has_hierarchy = .false.
    contains
        procedure :: init => navigation_tree_init
    end type navigation_tree_t
    
    type :: transformed_data_t
        type(source_file_t), allocatable :: files(:)
        type(coverage_summary_t) :: summary
        type(navigation_tree_t) :: file_tree
        character(len=:), allocatable :: metadata_json
    contains
        procedure :: init => transformed_data_init
    end type transformed_data_t
    
    type :: file_cache_entry_t
        character(len=:), allocatable :: filepath
        character(len=:), allocatable :: content
        logical :: is_valid = .false.
    end type file_cache_entry_t
    
    type :: large_file_streamer_t
        character(len=:), allocatable :: filepath
        integer :: chunk_size_lines = 100
        integer :: current_chunk = 0
        integer :: total_chunks = 0
        logical :: has_more = .false.
    contains
        procedure :: has_more_chunks => streamer_has_more_chunks
        procedure :: load_next_chunk => streamer_load_next_chunk
        procedure :: init => streamer_init
    end type large_file_streamer_t

contains

    ! Initialize annotated line
    subroutine annotated_line_init(this)
        class(annotated_line_t), intent(out) :: this
        
        this%line_number = 0
        this%execution_count = 0
        this%is_covered = .false.
        this%is_executable = .false.
        this%content = ""
        this%css_class = ""
        this%ansi_sequence = ""
    end subroutine annotated_line_init
    
    ! Initialize source file
    subroutine source_file_init(this)
        class(source_file_t), intent(out) :: this
        
        this%filename = ""
        this%content = ""
        this%coverage_percentage = 0.0
    end subroutine source_file_init
    
    ! Initialize navigation node
    subroutine navigation_node_init(this)
        class(navigation_node_t), intent(out) :: this
        
        this%name = ""
        this%path = ""
        this%is_directory = .false.
        this%coverage_percentage = 0.0
    end subroutine navigation_node_init
    
    ! Initialize navigation tree
    subroutine navigation_tree_init(this)
        class(navigation_tree_t), intent(out) :: this
        
        this%has_hierarchy = .false.
    end subroutine navigation_tree_init
    
    ! Initialize transformed data
    subroutine transformed_data_init(this)
        class(transformed_data_t), intent(out) :: this
        
        call this%summary%init()
        call this%file_tree%init()
        this%metadata_json = ""
    end subroutine transformed_data_init
    
    ! Initialize coverage summary
    subroutine coverage_summary_init(this)
        class(coverage_summary_t), intent(out) :: this
        
        this%total_lines = 0
        this%covered_lines = 0
        this%total_files = 0
        this%coverage_percentage = 0.0
    end subroutine coverage_summary_init
    
    ! Initialize file streamer
    subroutine streamer_init(this)
        class(large_file_streamer_t), intent(out) :: this
        
        this%filepath = ""
        this%chunk_size_lines = 100
        this%current_chunk = 0
        this%total_chunks = 0
        this%has_more = .false.
    end subroutine streamer_init
    
    ! Check if streamer has more chunks
    function streamer_has_more_chunks(this) result(has_more)
        class(large_file_streamer_t), intent(in) :: this
        logical :: has_more
        
        has_more = this%has_more
    end function streamer_has_more_chunks
    
    ! Load next chunk from streamer
    subroutine streamer_load_next_chunk(this, success)
        class(large_file_streamer_t), intent(inout) :: this
        logical, intent(out) :: success
        
        success = .false.
        
        if (.not. this%has_more) return
        
        this%current_chunk = this%current_chunk + 1
        
        ! Simple implementation - just advance chunk counter
        if (this%current_chunk >= this%total_chunks) then
            this%has_more = .false.
        end if
        
        success = .true.
    end subroutine streamer_load_next_chunk

end module data_transformer_types