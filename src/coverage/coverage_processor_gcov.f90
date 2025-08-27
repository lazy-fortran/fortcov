module coverage_processor_gcov
    !! GCov processing workflows extracted from coverage_workflows
    !! 
    !! Focused on .gcov file discovery, generation, and processing.
    !! Provides specialized gcov operations separated from other workflow
    !! management functionality.
    use constants_core
    use config_core
    use file_utils_core
    use shell_utils_core, only: escape_shell_argument
    implicit none
    private

    public :: discover_gcov_files
    public :: auto_generate_gcov_files

contains

    subroutine discover_gcov_files(config, files)
        !! Discovers .gcov files in configured source paths with automatic gcov generation
        !! Implements "sane default mode" - auto-discovers coverage files and runs gcov
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: files(:)
        
        character(len=:), allocatable :: search_paths(:)
        character(len=:), allocatable :: found_files(:)
        character(len=:), allocatable :: generated_files(:)
        
        ! Determine search paths based on configuration
        call determine_gcov_search_paths(config, search_paths, found_files)
        
        ! Search for existing .gcov files if needed
        call search_existing_gcov_files(search_paths, found_files)
        
        ! Generate gcov files if none found and in auto-discovery mode
        call attempt_gcov_generation(config, found_files, generated_files)
        
        ! Set final result
        call finalize_gcov_file_result(found_files, generated_files, files)
        
    end subroutine discover_gcov_files
    
    subroutine auto_generate_gcov_files(config, generated_files)
        !! Automatically discovers and generates .gcov files from build directories
        !! Implements the core "sane default mode" functionality for Issue #196
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: generated_files(:)
        
        character(len=:), allocatable :: build_dirs(:)
        character(len=:), allocatable :: all_gcov_files(:)
        logical :: success
        
        ! Find build directories with coverage data
        call find_coverage_build_directories(build_dirs)
        
        if (.not. allocated(build_dirs) .or. size(build_dirs) == 0) then
            return ! No build directories found
        end if
        
        ! Generate gcov files from build directories
        call generate_gcov_from_build_dirs(config, build_dirs, success)
        
        if (success) then
            ! Copy generated .gcov files to project root and collect them
            call collect_generated_gcov_files(build_dirs, all_gcov_files)
            if (allocated(all_gcov_files)) then
                generated_files = all_gcov_files
            end if
        end if
        
    end subroutine auto_generate_gcov_files
    
    subroutine find_coverage_build_directories(build_dirs)
        !! Finds build directories containing coverage data files
        character(len=:), allocatable, intent(out) :: build_dirs(:)
        
        character(len=:), allocatable :: gcda_files(:)
        character(len=500) :: dir_path
        integer :: i
        
        ! Find all .gcda files (indicates executed coverage data)
        gcda_files = find_files("build/**/fortcov/*.gcda")
        
        if (allocated(gcda_files) .and. size(gcda_files) > 0) then
            ! Use first .gcda file to determine build directory structure
            ! For simplicity, assume all build dirs follow same pattern
            dir_path = gcda_files(1)
            
            ! Extract directory path (remove filename)
            i = index(dir_path, '/', back=.true.)
            if (i > 0) then
                dir_path = dir_path(1:i-1)
                
                ! Allocate and set single build directory
                allocate(character(len=500) :: build_dirs(1))
                build_dirs(1) = trim(dir_path)
            end if
        end if
        
    end subroutine find_coverage_build_directories
    
    subroutine collect_generated_gcov_files(build_dirs, gcov_files)
        !! Collects generated .gcov files and copies them to build/gcov directory
        character(len=*), intent(in) :: build_dirs(:)
        character(len=:), allocatable, intent(out) :: gcov_files(:)
        
        character(len=:), allocatable :: found_gcov_files(:)
        character(len=300) :: command
        character(len=256), parameter :: GCOV_OUTPUT_DIR = "build/gcov"
        integer :: i, stat
        logical :: dir_exists
        
        ! Ensure the gcov output directory exists
        inquire(file=GCOV_OUTPUT_DIR, exist=dir_exists)
        if (.not. dir_exists) then
            call execute_command_line("mkdir -p " // escape_shell_argument(GCOV_OUTPUT_DIR), exitstat=stat)
        end if
        
        ! Copy all .gcov files from build directories to build/gcov
        do i = 1, size(build_dirs)
            write(command, '(A)') 'find ' // escape_shell_argument(trim(build_dirs(i))) // &
                ' -name "*.gcov" -exec cp {} ' // escape_shell_argument(GCOV_OUTPUT_DIR) // '/ \; 2>/dev/null'
            call execute_command_line(command)
        end do
        
        ! Now find the .gcov files in the gcov output directory
        found_gcov_files = find_files(GCOV_OUTPUT_DIR // "/*.gcov")
        
        if (allocated(found_gcov_files)) then
            gcov_files = found_gcov_files
        end if
        
    end subroutine collect_generated_gcov_files

    subroutine determine_gcov_search_paths(config, search_paths, found_files)
        !! Determine search paths for gcov files based on configuration
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(out) :: search_paths(:)
        character(len=:), allocatable, intent(out) :: found_files(:)
        
        character(len=256), parameter :: GCOV_OUTPUT_DIR = "build/gcov"
        logical :: dir_exists
        
        ! If explicit source paths are provided, respect them exclusively
        if (allocated(config%source_paths)) then
            search_paths = config%source_paths
        else
            ! Only check build/gcov directory when no explicit source paths provided
            inquire(file=GCOV_OUTPUT_DIR, exist=dir_exists)
            if (dir_exists) then
                found_files = find_files(GCOV_OUTPUT_DIR // "/*" // GCOV_EXTENSION)
            end if
            
            ! If no .gcov files found in build/gcov, default to current directory
            if (.not. allocated(found_files) .or. size(found_files) == 0) then
                allocate(character(len=1) :: search_paths(1))
                search_paths(1) = "."
            end if
        end if
    end subroutine determine_gcov_search_paths
    
    subroutine search_existing_gcov_files(search_paths, found_files)
        !! Search for existing .gcov files in configured search paths
        character(len=:), allocatable, intent(in) :: search_paths(:)
        character(len=:), allocatable, intent(inout) :: found_files(:)
        
        ! Search for existing .gcov files in configured search paths
        if ((.not. allocated(found_files) .or. size(found_files) == 0) .and. &
            allocated(search_paths)) then
            call search_gcov_files_in_paths(search_paths, found_files)
        end if
    end subroutine search_existing_gcov_files
    
    subroutine attempt_gcov_generation(config, found_files, generated_files)
        !! Attempt automatic generation only in auto-discovery mode
        type(config_t), intent(in) :: config
        character(len=:), allocatable, intent(in) :: found_files(:)
        character(len=:), allocatable, intent(out) :: generated_files(:)
        
        ! Attempt automatic generation only in auto-discovery mode
        ! When explicit source paths are specified, respect user intent
        if ((.not. allocated(found_files) .or. size(found_files) == 0) .and. &
            .not. allocated(config%source_paths)) then
            call auto_generate_gcov_files(config, generated_files)
        end if
    end subroutine attempt_gcov_generation
    
    subroutine finalize_gcov_file_result(found_files, generated_files, files)
        !! Set final result for gcov file discovery
        character(len=:), allocatable, intent(in) :: found_files(:)
        character(len=:), allocatable, intent(in) :: generated_files(:)
        character(len=:), allocatable, intent(out) :: files(:)
        
        if (allocated(generated_files)) then
            files = generated_files
        else if (allocated(found_files)) then
            files = found_files
        else
            ! CRITICAL FIX: Ensure files is always allocated to prevent memory issues
            ! If no files found and no files generated, allocate empty array
            allocate(character(len=256) :: files(0))
        end if
    end subroutine finalize_gcov_file_result
    
    subroutine generate_gcov_from_build_dirs(config, build_dirs, success)
        !! Generate gcov files from build directories
        type(config_t), intent(in) :: config
        character(len=*), intent(in) :: build_dirs(:)
        logical, intent(out) :: success
        
        character(len=300) :: command
        character(len=1000) :: build_path
        character(len=:), allocatable :: gcov_exe
        integer :: i, exit_status
        
        success = .false.
        
        ! Use custom gcov executable if specified, otherwise default to 'gcov'
        if (allocated(config%gcov_executable)) then
            gcov_exe = trim(config%gcov_executable)
            ! If gcov_executable is empty or only whitespace, use default
            if (len_trim(gcov_exe) == 0) then
                gcov_exe = "gcov"
            end if
        else
            gcov_exe = "gcov"
        end if
        
        ! Process each build directory
        do i = 1, size(build_dirs)
            build_path = trim(build_dirs(i))
            
            ! Generate gcov files for this directory (check if .gcno files exist)
            write(command, '(A)') 'find ' // escape_shell_argument(trim(build_path)) // ' -name "src_*.f90.gcno" -execdir ' // &
                                  escape_shell_argument(trim(gcov_exe)) // ' {} \; 2>/dev/null'
            call execute_command_line(command, exitstat=exit_status)
            
            if (exit_status == 0) then
                success = .true.
            end if
        end do
    end subroutine generate_gcov_from_build_dirs

    subroutine search_gcov_files_in_paths(search_paths, found_files)
        !! Searches for .gcov files in specified search paths
        character(len=*), intent(in) :: search_paths(:)
        character(len=:), allocatable, intent(out) :: found_files(:)
        
        character(len=:), allocatable :: path_files(:)
        character(len=:), allocatable :: all_files(:)
        integer :: i, total_files, current_size
        logical :: path_exists
        
        ! Initialize to empty
        allocate(character(len=256) :: all_files(0))
        total_files = 0
        
        ! Search each path for .gcov files
        do i = 1, size(search_paths)
            ! Check if path exists before searching
            inquire(file=trim(search_paths(i)), exist=path_exists)
            if (path_exists) then
                ! Search for .gcov files in this path
                path_files = find_files(trim(search_paths(i)) // "/*" // GCOV_EXTENSION)
                
                ! Merge with existing files
                if (allocated(path_files) .and. size(path_files) > 0) then
                    current_size = size(all_files)
                    call expand_file_array(all_files, path_files)
                    total_files = total_files + size(path_files)
                end if
            end if
        end do
        
        ! Return the found files
        if (total_files > 0) then
            found_files = all_files
        else
            ! Return empty array
            allocate(character(len=256) :: found_files(0))
        end if
        
    end subroutine search_gcov_files_in_paths
    
    subroutine expand_file_array(all_files, new_files)
        !! Expands file array to include new files
        character(len=:), allocatable, intent(inout) :: all_files(:)
        character(len=*), intent(in) :: new_files(:)
        
        character(len=:), allocatable :: temp_files(:)
        integer :: old_size, new_size, i
        
        old_size = size(all_files)
        new_size = old_size + size(new_files)
        
        ! Create temporary array with combined size
        allocate(character(len=max(len(all_files), len(new_files))) :: temp_files(new_size))
        
        ! Copy existing files
        do i = 1, old_size
            temp_files(i) = all_files(i)
        end do
        
        ! Copy new files
        do i = 1, size(new_files)
            temp_files(old_size + i) = new_files(i)
        end do
        
        ! Replace all_files with expanded array
        call move_alloc(temp_files, all_files)
        
    end subroutine expand_file_array

end module coverage_processor_gcov