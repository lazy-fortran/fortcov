module data_transformer
    use coverage_model
    implicit none
    private
    
    ! Public types
    public :: data_transformer_t
    public :: transformed_data_t
    public :: source_file_t
    public :: annotated_line_t
    public :: navigation_tree_t
    public :: large_file_streamer_t
    public :: coverage_summary_t
    public :: navigation_node_t
    
    ! Public procedures
    public :: transform_coverage_data
    
    ! Type definitions based on architecture specification
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
        ! Note: Removed recursive children component to avoid module &
        ! interface corruption
        ! Hierarchical structure can be handled externally if needed
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
    
    type :: data_transformer_t
        integer :: max_cache_size_mb = 100
        integer :: max_memory_mb = 100
        integer :: chunk_size_lines = 100
        integer :: current_memory_usage_mb = 0
        type(file_cache_entry_t), allocatable :: cache(:)
        integer :: cache_count = 0
    contains
        procedure :: init => transformer_init
        procedure :: init_with_cache => transformer_init_with_cache
        procedure :: init_with_limits => transformer_init_with_limits
        procedure :: transform_data => transformer_transform_data
        procedure :: load_source_file => transformer_load_source_file
        procedure :: extract_line_context => transformer_extract_line_context
        procedure :: annotate_coverage => transformer_annotate_coverage
        procedure :: build_navigation_tree => transformer_build_navigation_tree
        procedure :: init_file_streaming => transformer_init_file_streaming
    end type data_transformer_t

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
    
    ! Initialize transformer
    subroutine transformer_init(this)
        class(data_transformer_t), intent(out) :: this
        
        this%max_cache_size_mb = 100
        this%max_memory_mb = 100
        this%chunk_size_lines = 100
        this%current_memory_usage_mb = 0
        this%cache_count = 0
        
        ! Initialize cache
        allocate(this%cache(100))  ! Start with reasonable cache size
    end subroutine transformer_init
    
    ! Initialize transformer with cache settings
    subroutine transformer_init_with_cache(this, max_cache_size_mb)
        class(data_transformer_t), intent(out) :: this
        integer, intent(in) :: max_cache_size_mb
        
        call this%init()
        this%max_cache_size_mb = max_cache_size_mb
    end subroutine transformer_init_with_cache
    
    ! Initialize transformer with memory limits
    subroutine transformer_init_with_limits(this, max_memory_mb, &
                                            chunk_size_lines)
        class(data_transformer_t), intent(out) :: this
        integer, intent(in) :: max_memory_mb
        integer, intent(in) :: chunk_size_lines
        
        call this%init()
        this%max_memory_mb = max_memory_mb
        this%chunk_size_lines = chunk_size_lines
    end subroutine transformer_init_with_limits
    
    ! Transform coverage data to enriched format
    subroutine transformer_transform_data(this, input_data, output_data, &
                                           success, error_msg)
        class(data_transformer_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: input_data
        type(transformed_data_t), intent(out) :: output_data
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        integer :: i, total_lines, covered_lines
        
        success = .false.
        error_msg = ""
        
        call output_data%init()
        
        ! Check if input data has files
        if (.not. allocated(input_data%files)) then
            error_msg = "No files in input coverage data"
            return
        end if
        
        if (size(input_data%files) == 0) then
            error_msg = "Empty file list in input coverage data"
            return
        end if
        
        ! Allocate output files
        allocate(output_data%files(size(input_data%files)))
        
        ! Transform each file
        total_lines = 0
        covered_lines = 0
        
        do i = 1, size(input_data%files)
            call transform_single_file(this, input_data%files(i), &
                                       output_data%files(i))
            
            ! Accumulate statistics
            if (allocated(input_data%files(i)%lines)) then
                total_lines = total_lines + size(input_data%files(i)%lines)
                covered_lines = covered_lines + &
                    count(input_data%files(i)%lines%execution_count > 0 .and. &
                          input_data%files(i)%lines%is_executable)
            end if
        end do
        
        ! Update summary
        output_data%summary%total_files = size(input_data%files)
        output_data%summary%total_lines = total_lines
        output_data%summary%covered_lines = covered_lines
        
        if (total_lines > 0) then
            output_data%summary%coverage_percentage = &
                real(covered_lines) / real(total_lines) * 100.0
        end if
        
        ! Generate metadata JSON
        call generate_metadata_json(this, output_data%summary, &
                                     output_data%metadata_json)
        
        success = .true.
    end subroutine transformer_transform_data
    
    ! Transform a single file
    subroutine transform_single_file(this, input_file, output_file)
        class(data_transformer_t), intent(inout) :: this
        type(coverage_file_t), intent(in) :: input_file
        type(source_file_t), intent(out) :: output_file
        
        integer :: i, executable_lines, covered_lines
        
        call output_file%init()
        output_file%filename = input_file%filename
        
        ! Transform lines if they exist
        if (allocated(input_file%lines)) then
            allocate(output_file%lines(size(input_file%lines)))
            
            executable_lines = 0
            covered_lines = 0
            
            do i = 1, size(input_file%lines)
                call output_file%lines(i)%init()
                output_file%lines(i)%line_number = &
                    input_file%lines(i)%line_number
                output_file%lines(i)%execution_count = &
                    input_file%lines(i)%execution_count
                output_file%lines(i)%is_executable = &
                    input_file%lines(i)%is_executable
                output_file%lines(i)%is_covered = &
                    input_file%lines(i)%execution_count > 0
                
                if (input_file%lines(i)%is_executable) then
                    executable_lines = executable_lines + 1
                    if (input_file%lines(i)%execution_count > 0) then
                        covered_lines = covered_lines + 1
                    end if
                end if
            end do
            
            ! Calculate file coverage percentage
            if (executable_lines > 0) then
                output_file%coverage_percentage = &
                    real(covered_lines) / real(executable_lines) * 100.0
            end if
        end if
    end subroutine transform_single_file
    
    ! Load source file with caching
    subroutine transformer_load_source_file(this, file_path, source_file, &
                                             success, error_msg)
        class(data_transformer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        type(source_file_t), intent(out) :: source_file
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        logical :: file_exists
        integer :: unit, iostat, line_count, i
        character(len=1000) :: line_buffer
        character(len=:), allocatable :: content
        
        success = .false.
        error_msg = ""
        
        call source_file%init()
        
        ! Check if file exists
        inquire(file=file_path, exist=file_exists)
        if (.not. file_exists) then
            error_msg = "Source file not found: " // file_path
            return
        end if
        
        source_file%filename = file_path
        
        ! Read file content
        open(newunit=unit, file=file_path, status='old', action='read', &
             iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Failed to open source file: " // file_path
            return
        end if
        
        ! First pass: count lines
        line_count = 0
        do
            read(unit, '(A)', iostat=iostat) line_buffer
            if (iostat /= 0) exit
            line_count = line_count + 1
        end do
        
        ! Reset file position
        rewind(unit)
        
        ! Allocate lines array
        allocate(source_file%lines(line_count))
        
        ! Second pass: read content and populate lines
        content = ""
        do i = 1, line_count
            read(unit, '(A)', iostat=iostat) line_buffer
            if (iostat /= 0) exit
            
            call source_file%lines(i)%init()
            source_file%lines(i)%line_number = i
            source_file%lines(i)%content = trim(line_buffer)
            
            ! Accumulate content
            if (i == 1) then
                content = trim(line_buffer)
            else
                content = content // new_line('a') // trim(line_buffer)
            end if
        end do
        
        close(unit)
        source_file%content = content
        
        success = .true.
    end subroutine transformer_load_source_file
    
    ! Extract line context from coverage data
    subroutine transformer_extract_line_context(this, coverage_file, &
                                                 source_file, success)
        class(data_transformer_t), intent(inout) :: this
        type(coverage_file_t), intent(in) :: coverage_file
        type(source_file_t), intent(inout) :: source_file
        logical, intent(out) :: success
        
        integer :: i, j
        
        success = .false.
        
        ! Load source file first if not already loaded
        if (.not. allocated(source_file%lines)) then
            ! Need to load source file
            success = .false.
            return
        end if
        
        ! Map coverage data to source lines
        if (allocated(coverage_file%lines)) then
            do i = 1, size(coverage_file%lines)
                do j = 1, size(source_file%lines)
                    if (source_file%lines(j)%line_number == &
                        coverage_file%lines(i)%line_number) then
                        source_file%lines(j)%execution_count = &
                            coverage_file%lines(i)%execution_count
                        source_file%lines(j)%is_executable = &
                            coverage_file%lines(i)%is_executable
                        source_file%lines(j)%is_covered = &
                            coverage_file%lines(i)%execution_count > 0
                        exit
                    end if
                end do
            end do
        end if
        
        success = .true.
    end subroutine transformer_extract_line_context
    
    ! Annotate coverage data onto source file
    subroutine transformer_annotate_coverage(this, coverage_file, &
                                              annotated_file, success)
        class(data_transformer_t), intent(inout) :: this
        type(coverage_file_t), intent(in) :: coverage_file
        type(source_file_t), intent(out) :: annotated_file
        logical, intent(out) :: success
        
        integer :: i
        
        call annotated_file%init()
        annotated_file%filename = coverage_file%filename
        
        ! Simple implementation: just copy coverage data
        if (allocated(coverage_file%lines)) then
            allocate(annotated_file%lines(size(coverage_file%lines)))
            
            do i = 1, size(coverage_file%lines)
                call annotated_file%lines(i)%init()
                annotated_file%lines(i)%line_number = &
                    coverage_file%lines(i)%line_number
                annotated_file%lines(i)%execution_count = &
                    coverage_file%lines(i)%execution_count
                annotated_file%lines(i)%is_executable = &
                    coverage_file%lines(i)%is_executable
                annotated_file%lines(i)%is_covered = &
                    coverage_file%lines(i)%execution_count > 0
            end do
            
            ! Calculate coverage percentage
            annotated_file%coverage_percentage = &
                coverage_file%get_line_coverage_percentage()
        end if
        
        success = .true.
    end subroutine transformer_annotate_coverage
    
    ! Build navigation tree from coverage data
    subroutine transformer_build_navigation_tree(this, input_data, &
                                                  nav_tree, success)
        class(data_transformer_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: input_data
        type(navigation_tree_t), intent(out) :: nav_tree
        logical, intent(out) :: success
        
        integer :: i
        
        ! Initialize navigation tree with error handling
        call nav_tree%init()
        success = .false.
        
        ! Defensive check for uninitialized input_data
        if (.not. allocated(input_data%files)) then
            success = .true.  ! Empty is not an error
            return
        end if
        
        if (size(input_data%files) == 0) then
            success = .true.  ! Empty is not an error
            return
        end if
        
        ! Simple implementation: flat structure for now
        allocate(nav_tree%nodes(size(input_data%files)))
        
        do i = 1, size(input_data%files)
            call nav_tree%nodes(i)%init()
            nav_tree%nodes(i)%name = input_data%files(i)%filename
            nav_tree%nodes(i)%path = input_data%files(i)%filename
            nav_tree%nodes(i)%is_directory = .false.
            nav_tree%nodes(i)%coverage_percentage = &
                input_data%files(i)%get_line_coverage_percentage()
        end do
        
        nav_tree%has_hierarchy = .true.
        success = .true.
    end subroutine transformer_build_navigation_tree
    
    ! Initialize file streaming for large files
    subroutine transformer_init_file_streaming(this, file_path, &
                                               streamer, success)
        class(data_transformer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        type(large_file_streamer_t), intent(out) :: streamer
        logical, intent(out) :: success
        
        logical :: file_exists
        integer :: unit, iostat, line_count
        character(len=1000) :: line_buffer
        
        call streamer%init()
        success = .false.
        
        ! Check if file exists
        inquire(file=file_path, exist=file_exists)
        if (.not. file_exists) then
            return
        end if
        
        streamer%filepath = file_path
        streamer%chunk_size_lines = this%chunk_size_lines
        
        ! Count total lines to determine chunks
        open(newunit=unit, file=file_path, status='old', action='read', &
             iostat=iostat)
        if (iostat /= 0) then
            return
        end if
        
        line_count = 0
        do
            read(unit, '(A)', iostat=iostat) line_buffer
            if (iostat /= 0) exit
            line_count = line_count + 1
        end do
        close(unit)
        
        streamer%total_chunks = (line_count + streamer%chunk_size_lines - 1) / &
                               streamer%chunk_size_lines
        streamer%current_chunk = 0
        streamer%has_more = streamer%total_chunks > 0
        
        success = .true.
    end subroutine transformer_init_file_streaming
    
    ! Generate metadata JSON
    subroutine generate_metadata_json(this, summary, metadata_json)
        class(data_transformer_t), intent(in) :: this
        type(coverage_summary_t), intent(in) :: summary
        character(len=:), allocatable, intent(out) :: metadata_json
        
        character(len=20) :: total_files_str, total_lines_str, covered_lines_str
        character(len=20) :: coverage_percentage_str
        
        ! Convert numbers to strings
        write(total_files_str, '(I0)') summary%total_files
        write(total_lines_str, '(I0)') summary%total_lines
        write(covered_lines_str, '(I0)') summary%covered_lines
        write(coverage_percentage_str, '(F0.1)') summary%coverage_percentage
        
        ! Build JSON
        metadata_json = '{' // &
            '"total_files": ' // trim(total_files_str) // ', ' // &
            '"total_lines": ' // trim(total_lines_str) // ', ' // &
            '"covered_lines": ' // trim(covered_lines_str) // ', ' // &
            '"coverage_percentage": ' // trim(coverage_percentage_str) // &
            '}'
    end subroutine generate_metadata_json

    ! Main transformation procedure for coverage data
    subroutine transform_coverage_data(coverage_data, transformed_data, &
                                       success, error_msg)
        type(coverage_data_t), intent(in) :: coverage_data
        type(transformed_data_t), intent(out) :: transformed_data
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(data_transformer_t) :: transformer
        integer :: i
        
        success = .true.
        error_msg = ""
        
        ! Initialize transformer
        call transformer%init()
        
        ! Initialize transformed data
        call transformed_data%init()
        
        ! Transform each coverage file
        if (allocated(coverage_data%files)) then
            allocate(transformed_data%files(size(coverage_data%files)))
            
            do i = 1, size(coverage_data%files)
                call transformer%annotate_coverage(coverage_data%files(i), &
                                                  transformed_data%files(i), &
                                                  success)
                if (.not. success) then
                    error_msg = "Failed to transform coverage file: " // &
                               coverage_data%files(i)%filename
                    return
                end if
            end do
        end if
        
        ! Build navigation tree with safety checks
        ! Check if transformed_data is properly initialized
        if (.not. allocated(coverage_data%files)) then
            error_msg = "Coverage data not properly initialized"
            return
        end if
        
        call transformer%build_navigation_tree(coverage_data, &
                                              transformed_data%file_tree, &
                                              success)
        if (.not. success) then
            error_msg = "Failed to build navigation tree"
            return
        end if
        
        ! Calculate overall summary
        ! Memory safety: Check if files array is allocated
        if (allocated(coverage_data%files)) then
            transformed_data%summary%total_files = size(coverage_data%files)
            
            ! Sum up line counts and coverage
            do i = 1, size(coverage_data%files)
                ! Memory safety: Check if lines array is allocated for this file
                if (allocated(coverage_data%files(i)%lines)) then
                    transformed_data%summary%total_lines = &
                        transformed_data%summary%total_lines + &
                        size(coverage_data%files(i)%lines)
                    transformed_data%summary%covered_lines = &
                        transformed_data%summary%covered_lines + &
                        count(coverage_data%files(i)%lines%execution_count > 0 .and. &
                              coverage_data%files(i)%lines%is_executable)
                end if
            end do
        else
            transformed_data%summary%total_files = 0
        end if
        
        ! Calculate percentage
        if (transformed_data%summary%total_lines > 0) then
            transformed_data%summary%coverage_percentage = &
                (real(transformed_data%summary%covered_lines) / &
                 real(transformed_data%summary%total_lines)) * 100.0
        end if
        
    end subroutine transform_coverage_data

end module data_transformer
