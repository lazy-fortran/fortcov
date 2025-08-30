module coverage_processor_file
    !! Coverage File Processing Functions
    !! 
    !! File discovery and parsing functions extracted from coverage_analysis.f90 
    !! for maintainability and adherence to size limits (Issue #333 - Patrick's review).
    use coverage_model_core, only: coverage_data_t, coverage_file_t
    use config_core, only: config_t
    use config_types, only: MAX_ARRAY_SIZE
    use coverage_parser_factory, only: coverage_parser_t, create_parser
    use file_utils_core, only: read_file_content, file_exists
    use coverage_workflows, only: execute_auto_test_workflow
    
    implicit none
    private
    
    ! Use constant for array sizing
    integer, parameter :: MAX_FILE_PATH_LENGTH = 1024
    
    public :: find_and_filter_coverage_files
    public :: parse_coverage_files
    
contains

    subroutine find_and_filter_coverage_files(config, coverage_files, filtered_files)
        !! Find and filter coverage files based on configuration
        ! Import at procedure level to avoid circular dependency (Issue #281)
        use coverage_workflows, only: discover_coverage_files
        type(config_t), intent(in) :: config
        character(len=MAX_FILE_PATH_LENGTH), allocatable, &
            intent(out) :: coverage_files(:)
        character(len=MAX_FILE_PATH_LENGTH), allocatable, &
            intent(out) :: filtered_files(:)
        
        character(len=MAX_FILE_PATH_LENGTH) :: temp_files(MAX_ARRAY_SIZE)
        integer :: num_files, i, filtered_count, stat
        character(len=512) :: errmsg
        logical :: include_file
        
        ! Initialize arrays
        if (allocated(coverage_files)) deallocate(coverage_files)
        if (allocated(filtered_files)) deallocate(filtered_files)
        
        ! Discover coverage files
        coverage_files = discover_coverage_files(config)
        
        if (.not. allocated(coverage_files) .or. size(coverage_files) == 0) then
            ! No files found - allocate empty arrays
            allocate(character(len=MAX_FILE_PATH_LENGTH) :: filtered_files(0), &
                stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Memory allocation failed for filtered_files: " // &
                    trim(errmsg)
                return
            end if
            return
        end if
        
        ! No filtering of coverage files - exclude patterns apply to source files within the data
        ! Issue #884 fix: Don't filter coverage files, only filter source files in reports
        allocate(character(len=MAX_FILE_PATH_LENGTH) :: &
            filtered_files(size(coverage_files)), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Memory allocation failed for filtered_files: " // &
                trim(errmsg)
            return
        end if
        filtered_files = coverage_files
        
    end subroutine find_and_filter_coverage_files

    subroutine parse_coverage_files(files, config, merged_coverage, parse_error)
        !! Parse coverage files and merge into single coverage model
        use gcov_file_processor, only: process_gcov_file
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        type(coverage_data_t), intent(out) :: merged_coverage
        logical, intent(out) :: parse_error
        
        integer :: i, j, parse_count, file_count, stat
        character(len=512) :: errmsg
        type(coverage_data_t) :: file_coverage
        type(coverage_file_t), allocatable :: all_files(:), temp_files(:)
        logical :: file_parse_error
        
        parse_error = .false.
        parse_count = 0
        file_count = 0
        
        ! Initialize merged coverage
        call merged_coverage%init()
        
        ! Allocate initial storage for files
        allocate(all_files(size(files)), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Memory allocation failed for all_files: " // &
                trim(errmsg)
            parse_error = .true.
            return
        end if
        
        ! Parse each coverage file
        do i = 1, size(files)
            if (len_trim(files(i)) == 0) cycle
            
            if (.not. file_exists(files(i))) then
                if (.not. config%quiet) then
                    write(*,'(A)') "Warning: Coverage file not found: " // &
                        trim(files(i))
                end if
                cycle
            end if
            
            if (.not. config%quiet) then
                write(*,'(A,I0,A)') "Found coverage file ", i, ": " // trim(files(i))
            end if
            
            ! Parse this coverage file
            call process_gcov_file(files(i), file_coverage, file_parse_error)
            
            if (file_parse_error) then
                if (.not. config%quiet) then
                    write(*,'(A)') "Warning: Failed to parse: " // trim(files(i))
                end if
                cycle
            end if
            
            parse_count = parse_count + 1
            
            ! Merge files from this coverage data into our collection
            if (allocated(file_coverage%files)) then
                do j = 1, size(file_coverage%files)
                    file_count = file_count + 1
                    
                    ! Expand all_files array if needed
                    if (file_count > size(all_files)) then
                        allocate(temp_files(file_count * 2), stat=stat, errmsg=errmsg)
                        if (stat /= 0) then
                            write(*, '(A)') "Error: Memory allocation failed for temp_files: " // &
                                trim(errmsg)
                            parse_error = .true.
                            return
                        end if
                        temp_files(1:size(all_files)) = all_files
                        call move_alloc(temp_files, all_files)
                    end if
                    
                    ! Copy file data
                    all_files(file_count) = file_coverage%files(j)
                end do
            end if
        end do
        
        ! Check if any files were successfully parsed
        if (parse_count == 0 .or. file_count == 0) then
            if (.not. config%quiet) then
                write(*,'(A)') "Error: No valid coverage files found"
            end if
            parse_error = .true.
            return
        end if
        
        ! Set merged coverage files
        if (file_count > 0) then
            ! Deallocate first since init() already allocated empty array
            if (allocated(merged_coverage%files)) deallocate(merged_coverage%files)
            allocate(merged_coverage%files(file_count), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Memory allocation failed for merged_coverage files: " // &
                    trim(errmsg)
                parse_error = .true.
                return
            end if
            merged_coverage%files = all_files(1:file_count)
        end if
        
        if (.not. config%quiet) then
            write(*,'(A,I0,A)') "Found ", parse_count, " coverage files"
        end if
        
    end subroutine parse_coverage_files

end module coverage_processor_file