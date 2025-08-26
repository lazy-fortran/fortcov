module data_transformer_core
    use coverage_model
    use data_transformer_types
    use coverage_file_loader
    use coverage_annotator
    use navigation_tree_builder
    use coverage_summary_generator
    implicit none
    private
    
    ! Public types
    public :: data_transformer_t
    
    ! Public procedures
    public :: transform_coverage_data
    
    ! Data transformer type with caching and memory management
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
    subroutine transformer_init_with_limits(this, max_memory_mb, chunk_size_lines)
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
        
        integer :: i
        
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
        do i = 1, size(input_data%files)
            call transform_single_file(this, input_data%files(i), &
                                       output_data%files(i))
        end do
        
        ! Generate summary
        call generate_coverage_summary(input_data, output_data%summary, success)
        if (.not. success) then
            error_msg = "Failed to generate coverage summary"
            return
        end if
        
        ! Build navigation tree
        call build_navigation_tree(input_data, output_data%file_tree, success)
        if (.not. success) then
            error_msg = "Failed to build navigation tree"
            return
        end if
        
        ! Generate metadata JSON
        call generate_metadata_json(output_data%summary, output_data%metadata_json)
        
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
    
    ! Load source file - delegate to module procedure
    subroutine transformer_load_source_file(this, file_path, source_file, &
                                             success, error_msg)
        class(data_transformer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        type(source_file_t), intent(out) :: source_file
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        call load_source_file(file_path, source_file, success, error_msg)
    end subroutine transformer_load_source_file
    
    ! Extract line context - delegate to annotator module
    subroutine transformer_extract_line_context(this, coverage_file, &
                                                 source_file, success)
        class(data_transformer_t), intent(inout) :: this
        type(coverage_file_t), intent(in) :: coverage_file
        type(source_file_t), intent(inout) :: source_file
        logical, intent(out) :: success
        
        call extract_line_context(coverage_file, source_file, success)
    end subroutine transformer_extract_line_context
    
    ! Annotate coverage - delegate to annotator module
    subroutine transformer_annotate_coverage(this, coverage_file, &
                                              annotated_file, success)
        class(data_transformer_t), intent(inout) :: this
        type(coverage_file_t), intent(in) :: coverage_file
        type(source_file_t), intent(out) :: annotated_file
        logical, intent(out) :: success
        
        call annotate_coverage_file(coverage_file, annotated_file, success)
    end subroutine transformer_annotate_coverage
    
    ! Build navigation tree - delegate to builder module
    subroutine transformer_build_navigation_tree(this, input_data, &
                                                  nav_tree, success)
        class(data_transformer_t), intent(inout) :: this
        type(coverage_data_t), intent(in) :: input_data
        type(navigation_tree_t), intent(out) :: nav_tree
        logical, intent(out) :: success
        
        call build_navigation_tree(input_data, nav_tree, success)
    end subroutine transformer_build_navigation_tree
    
    ! Initialize file streaming - delegate to loader module
    subroutine transformer_init_file_streaming(this, file_path, &
                                               streamer, success)
        class(data_transformer_t), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        type(large_file_streamer_t), intent(out) :: streamer
        logical, intent(out) :: success
        
        call init_file_streaming(file_path, this%chunk_size_lines, &
                                streamer, success)
    end subroutine transformer_init_file_streaming

    ! Main transformation procedure for coverage data
    subroutine transform_coverage_data(coverage_data, transformed_data, &
                                       success, error_msg)
        type(coverage_data_t), intent(in) :: coverage_data
        type(transformed_data_t), intent(out) :: transformed_data
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg
        
        type(data_transformer_t) :: transformer
        
        success = .true.
        error_msg = ""
        
        ! Initialize transformer
        call transformer%init()
        
        ! Delegate to transformer
        call transformer%transform_data(coverage_data, transformed_data, &
                                       success, error_msg)
    end subroutine transform_coverage_data

end module data_transformer_core