module coverage_file_processor
    !! Coverage File Processing Functions
    !! 
    !! File discovery and parsing functions extracted from coverage_analysis.f90 
    !! for maintainability and adherence to size limits (Issue #333 - Patrick's review).
    use foundation_constants
    use coverage_model
    use fortcov_config
    use config_types, only: MAX_ARRAY_SIZE
    use coverage_parser, only: coverage_parser_t, create_parser
    use error_handling
    use file_utils, only: read_file_content, file_exists
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
        integer :: num_files, i, filtered_count
        logical :: include_file
        
        ! Initialize arrays
        if (allocated(coverage_files)) deallocate(coverage_files)
        if (allocated(filtered_files)) deallocate(filtered_files)
        
        ! Discover coverage files
        coverage_files = discover_coverage_files(config)
        
        if (.not. allocated(coverage_files) .or. size(coverage_files) == 0) then
            ! No files found - allocate empty arrays
            allocate(character(len=MAX_FILE_PATH_LENGTH) :: filtered_files(0))
            return
        end if
        
        ! Apply filtering if exclude patterns are specified
        if (allocated(config%exclude_patterns) .and. &
            size(config%exclude_patterns) > 0) then
            
            filtered_count = 0
            do i = 1, size(coverage_files)
                include_file = should_include_file(coverage_files(i), config)
                if (include_file .and. filtered_count < MAX_ARRAY_SIZE) then
                    filtered_count = filtered_count + 1
                    temp_files(filtered_count) = coverage_files(i)
                end if
            end do
            
            ! Allocate and copy filtered results
            allocate(character(len=MAX_FILE_PATH_LENGTH) :: &
                filtered_files(filtered_count))
            if (filtered_count > 0) then
                filtered_files(1:filtered_count) = temp_files(1:filtered_count)
            end if
        else
            ! No filtering needed - copy all files
            allocate(character(len=MAX_FILE_PATH_LENGTH) :: &
                filtered_files(size(coverage_files)))
            filtered_files = coverage_files
        end if
        
    end subroutine find_and_filter_coverage_files

    subroutine parse_coverage_files(files, config, merged_coverage, parse_error)
        !! Parse coverage files and merge into single coverage model
        use gcov_file_processor, only: process_gcov_file
        character(len=*), intent(in) :: files(:)
        type(config_t), intent(in) :: config
        type(coverage_data_t), intent(out) :: merged_coverage
        logical, intent(out) :: parse_error
        
        integer :: i, j, parse_count, file_count
        type(coverage_data_t) :: file_coverage
        type(coverage_file_t), allocatable :: all_files(:), temp_files(:)
        logical :: file_parse_error
        
        parse_error = .false.
        parse_count = 0
        file_count = 0
        
        ! Initialize merged coverage
        call merged_coverage%init()
        
        ! Allocate initial storage for files
        allocate(all_files(size(files)))
        
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
                        allocate(temp_files(file_count * 2))
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
            allocate(merged_coverage%files(file_count))
            merged_coverage%files = all_files(1:file_count)
        end if
        
        if (.not. config%quiet) then
            write(*,'(A,I0,A)') "Found ", parse_count, " coverage files"
        end if
        
    end subroutine parse_coverage_files

    function should_include_file(filename, config) result(include_file)
        !! Check if file should be included based on exclude patterns
        use string_utils, only: matches_pattern
        character(len=*), intent(in) :: filename
        type(config_t), intent(in) :: config
        logical :: include_file
        
        integer :: i
        
        include_file = .true.
        
        if (.not. allocated(config%exclude_patterns)) return
        
        do i = 1, size(config%exclude_patterns)
            if (matches_pattern(filename, config%exclude_patterns(i))) then
                include_file = .false.
                return
            end if
        end do
        
    end function should_include_file

end module coverage_file_processor