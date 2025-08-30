module coverage_file_loader
    use data_transformer_types
    implicit none
    private
    
    ! Public procedures
    public :: load_source_file
    public :: init_file_streaming
    
contains

    ! Load source file with error handling
    subroutine load_source_file(file_path, source_file, success, error_msg)
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
    end subroutine load_source_file
    
    ! Initialize file streaming for large files
    subroutine init_file_streaming(file_path, chunk_size_lines, streamer, success)
        character(len=*), intent(in) :: file_path
        integer, intent(in) :: chunk_size_lines
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
        streamer%chunk_size_lines = chunk_size_lines
        
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
    end subroutine init_file_streaming

end module coverage_file_loader