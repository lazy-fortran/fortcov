module gcov_auto_processor
    !! GCOV Auto-Processing Module (Issue #277 - Part 2)
    !!
    !! Handles automatic processing of gcov files with build system context
    !! and source mapping discovery. Provides secure file operations and
    !! comprehensive error handling.
    !!
    !! Key Features:
    !! - Auto-processing of gcov files with build system context
    !! - Secure file finding and command execution
    !! - Source file mapping discovery from gcov files
    !! - Build directory extraction and processing

    use config_types, only: config_t
    use secure_command_executor, only: safe_execute_gcov
    use secure_file_operations, only: safe_find_files
    use path_validation, only: validate_executable_path
    use error_handling, only: error_context_t, ERROR_SUCCESS, &
                              clear_error_context
    use file_utils, only: file_exists, find_files
    use string_utils, only: trim_string
    use foundation_constants, only: EXIT_SUCCESS, EXIT_FAILURE
    implicit none
    private

    ! Public interface
    public :: auto_process_gcov_files
    public :: gcov_result_t
    public :: source_mapping_t

    ! Result types
    type :: source_mapping_t
        character(len=256) :: source_file = ''
        character(len=256) :: gcov_file = ''
        character(len=256) :: build_path = ''
    end type source_mapping_t

    type :: gcov_result_t
        logical :: success = .false.
        character(len=:), allocatable :: gcov_files(:)
        type(source_mapping_t), allocatable :: source_mappings(:)
        logical :: used_build_context = .false.
        character(len=256) :: error_message = ''
        character(len=512) :: guidance_message = ''
    end type gcov_result_t

    ! Constants
    integer, parameter :: MAX_GCOV_FILES = 1000
    integer, parameter :: MAX_SOURCE_MAPPINGS = 1000
    character(len=*), parameter :: GCDA_PATTERN = '**/*.gcda'
    character(len=*), parameter :: GCNO_PATTERN = '**/*.gcno'
    character(len=*), parameter :: GCOV_PATTERN = '*.gcov'

contains

    subroutine auto_process_gcov_files(project_path, config, result)
        !! Auto-process gcov files with build system context
        !!
        !! Finds .gcda files automatically after test execution and
        !! processes them using gcov with appropriate build system context.
        !! Auto-discovers source file mappings and provides comprehensive
        !! error handling.
        !!
        !! Args:
        !!   project_path: Project root directory
        !!   config: Configuration with gcov settings
        !!   result: Populated with processing results and file mappings

        character(len=*), intent(in) :: project_path  
        type(config_t), intent(in) :: config
        type(gcov_result_t), intent(out) :: result

        ! Initialize and validate inputs
        call initialize_gcov_result(result)
        
        if (.not. config%auto_discovery) then
            result%guidance_message = 'Auto-discovery disabled in configuration'
            return
        end if

        ! Execute main processing workflow
        call execute_gcov_processing_workflow(project_path, config, result)

    end subroutine auto_process_gcov_files

    subroutine initialize_gcov_result(result)
        !! Initialize gcov result structure
        type(gcov_result_t), intent(out) :: result
        
        result%success = .false.
        result%used_build_context = .false.
        result%error_message = ''
        result%guidance_message = ''
        
        ! Always allocate source_mappings array to avoid unallocated access
        allocate(result%source_mappings(0))
        ! Note: gcov_files is allocated in process_gcov_in_build_dirs when needed
    end subroutine initialize_gcov_result

    subroutine execute_gcov_processing_workflow(project_path, config, result)
        !! Execute the main gcov processing workflow
        character(len=*), intent(in) :: project_path
        type(config_t), intent(in) :: config
        type(gcov_result_t), intent(inout) :: result

        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: build_dirs(:)
        logical :: path_valid

        ! Validate project path
        call validate_project_path(project_path, path_valid, result%error_message)
        if (.not. path_valid) return

        ! Find and process gcda files
        call find_gcda_files(project_path, gcda_files)
        
        if (.not. allocated(gcda_files) .or. size(gcda_files) == 0) then
            call handle_no_gcda_files(result)
            return
        end if

        ! Extract and process build directories
        call extract_build_directories(gcda_files, build_dirs)
        
        if (allocated(build_dirs)) then
            call process_gcov_in_build_dirs(build_dirs, config, result)
        else
            ! No build directories found
            result%error_message = 'No build directories found in .gcda file paths'
            result%guidance_message = 'Verify build system generated coverage data correctly'
            allocate(character(len=0) :: result%gcov_files(0))
        end if

        if (result%success) then
            ! Discover source file mappings
            if (allocated(result%gcov_files)) then
                call discover_source_mappings(result%gcov_files, result%source_mappings)
            else
                ! Ensure source_mappings is allocated even if empty
                allocate(result%source_mappings(0))
            end if
            result%used_build_context = allocated(build_dirs) .and. size(build_dirs) > 0
        end if
    end subroutine execute_gcov_processing_workflow

    subroutine validate_project_path(project_path, valid, error_message)
        !! Validate that project path exists and is accessible
        character(len=*), intent(in) :: project_path
        logical, intent(out) :: valid
        character(len=*), intent(out) :: error_message

        character(len=256) :: resolved_path

        valid = .false.
        error_message = ''

        resolved_path = trim(project_path)
        if (len_trim(resolved_path) == 0) then
            resolved_path = '.'
        end if

        inquire(file=trim(resolved_path), exist=valid)
        if (.not. valid) then
            write(error_message, '(A,A,A)') &
                'Project path does not exist: ', trim(resolved_path), &
                '. Please verify the path is correct and accessible.'
        end if

    end subroutine validate_project_path

    subroutine find_gcda_files(project_path, gcda_files)
        !! Securely find .gcda files in project directory and subdirectories
        character(len=*), intent(in) :: project_path
        character(len=:), allocatable, intent(out) :: gcda_files(:)

        character(len=512) :: search_pattern
        type(error_context_t) :: error_ctx

        ! Build search pattern for .gcda files
        search_pattern = trim(project_path) // '/' // GCDA_PATTERN

        ! Use secure file finding
        call safe_find_files(search_pattern, gcda_files, error_ctx)
        
        ! Only deallocate files on actual failure, not recoverable issues
        if (error_ctx%error_code /= ERROR_SUCCESS .and. .not. error_ctx%recoverable) then
            if (allocated(gcda_files)) deallocate(gcda_files)
        end if
    end subroutine find_gcda_files

    subroutine handle_no_gcda_files(result)
        !! Handle case when no .gcda files are found
        type(gcov_result_t), intent(inout) :: result
        
        result%success = .false.
        result%error_message = 'No .gcda coverage data files found'
        result%guidance_message = &
            'Execute tests with coverage flags to generate .gcda files. ' // &
            'Run tests with --profile-arcs --test-coverage flags for GCC, ' // &
            'or use build system coverage targets (e.g., fpm test --flag ' // &
            '"-fprofile-arcs -ftest-coverage")'
        
        ! Ensure arrays are allocated even when no files found
        if (.not. allocated(result%gcov_files)) then
            allocate(character(len=0) :: result%gcov_files(0))
        end if
        
    end subroutine handle_no_gcda_files

    subroutine extract_build_directories(gcda_files, build_dirs)
        !! Extract unique build directories from .gcda file paths
        character(len=*), intent(in) :: gcda_files(:)
        character(len=:), allocatable, intent(out) :: build_dirs(:)

        character(len=256) :: temp_dirs(size(gcda_files))
        character(len=256) :: dir_path
        integer :: i, last_slash, num_unique
        logical :: found_duplicate
        integer :: j

        num_unique = 0

        ! Extract directory from each .gcda file path
        do i = 1, size(gcda_files)
            last_slash = index(gcda_files(i), '/', back=.true.)
            if (last_slash > 0) then
                dir_path = gcda_files(i)(1:last_slash-1)
            else
                dir_path = '.'
            end if

            ! Check if directory already in list
            found_duplicate = .false.
            do j = 1, num_unique
                if (trim(temp_dirs(j)) == trim(dir_path)) then
                    found_duplicate = .true.
                    exit
                end if
            end do

            ! Add unique directory
            if (.not. found_duplicate) then
                num_unique = num_unique + 1
                temp_dirs(num_unique) = trim(dir_path)
            end if
        end do

        ! Allocate result with unique directories
        if (num_unique > 0) then
            allocate(character(len=256) :: build_dirs(num_unique))
            do i = 1, num_unique
                build_dirs(i) = trim(temp_dirs(i))
            end do
        end if

    end subroutine extract_build_directories

    subroutine process_gcov_in_build_dirs(build_dirs, config, result)
        !! Process gcov files in each build directory
        character(len=*), intent(in) :: build_dirs(:)
        type(config_t), intent(in) :: config
        type(gcov_result_t), intent(out) :: result

        character(len=:), allocatable :: all_generated_files(:)
        character(len=:), allocatable :: dir_generated_files(:)
        integer :: i, exit_status
        character(len=256) :: gcov_exe

        ! Initialize result
        result%success = .false.
        result%error_message = ''

        ! Get gcov executable (default to 'gcov')
        gcov_exe = 'gcov'
        if (len_trim(config%gcov_executable) > 0) then
            gcov_exe = trim(config%gcov_executable)
        end if

        ! Process each build directory
        do i = 1, size(build_dirs)
            call process_single_build_dir(build_dirs(i), gcov_exe, exit_status, &
                                         dir_generated_files)
            
            if (exit_status /= EXIT_SUCCESS) then
                write(result%error_message, '(A,A)') &
                    'Failed to process gcov files in directory: ', trim(build_dirs(i))
                result%guidance_message = &
                    'Ensure gcov executable is available in PATH ' // &
                    'and gcov executable is compatible with coverage data format.'
                return
            end if

            if (allocated(dir_generated_files)) then
                call append_generated_files(all_generated_files, dir_generated_files)
            end if
        end do

        if (allocated(all_generated_files) .and. size(all_generated_files) > 0) then
            result%success = .true.
            result%gcov_files = all_generated_files
        else
            result%error_message = 'No gcov files generated from processing'
            result%guidance_message = &
                'Verify that .gcno files exist in build directories ' // &
                'and gcov executable is compatible with coverage data format.'
            ! Allocate empty array to avoid unallocated access
            allocate(character(len=0) :: result%gcov_files(0))
        end if
    end subroutine process_gcov_in_build_dirs

    subroutine process_single_build_dir(build_dir, gcov_exe, exit_status, &
                                       generated_files)
        !! Process gcov in single build directory using secure methods
        character(len=*), intent(in) :: build_dir, gcov_exe
        integer, intent(out) :: exit_status
        character(len=:), allocatable, intent(out) :: generated_files(:)

        character(len=:), allocatable :: gcno_files(:)
        character(len=256) :: gcov_pattern
        type(error_context_t) :: error_ctx
        integer :: i

        exit_status = EXIT_FAILURE

        ! Securely find .gcno files in build directory
        call safe_find_gcno_files(build_dir, gcno_files, error_ctx)
        if (error_ctx%error_code /= ERROR_SUCCESS .and. .not. error_ctx%recoverable) then
            exit_status = EXIT_FAILURE
            return
        end if

        ! Process each .gcno file with secure gcov execution
        if (allocated(gcno_files)) then
            do i = 1, size(gcno_files)
                call safe_execute_gcov_on_file(gcov_exe, gcno_files(i), &
                                               build_dir, error_ctx)
                if (error_ctx%error_code /= ERROR_SUCCESS .and. .not. error_ctx%recoverable) then
                    exit_status = EXIT_FAILURE
                    return
                end if
            end do
            exit_status = EXIT_SUCCESS
        else
            ! No .gcno files found - still report success if directory exists
            ! (it may have been processed already)
            exit_status = EXIT_SUCCESS
        end if

        if (exit_status == EXIT_SUCCESS) then
            ! Securely find generated .gcov files in build directory
            ! Ensure build_dir is valid before using it
            if (len_trim(build_dir) == 0) then
                exit_status = EXIT_FAILURE
                return
            end if
            
            ! Build pattern for finding gcov files
            gcov_pattern = trim(adjustl(build_dir)) // '/' // GCOV_PATTERN
            
            call safe_find_files(trim(gcov_pattern), generated_files, error_ctx)
            if (error_ctx%error_code /= ERROR_SUCCESS .and. .not. error_ctx%recoverable) then
                exit_status = EXIT_FAILURE
            end if
        end if
    end subroutine process_single_build_dir

    subroutine safe_find_gcno_files(directory, gcno_files, error_ctx)
        !! Securely find .gcno files in directory
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: gcno_files(:)
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=512) :: pattern
        
        call clear_error_context(error_ctx)
        
        ! Build secure pattern for .gcno files
        pattern = trim(directory) // '/*.gcno'
        
        ! Use secure file finding
        call safe_find_files(pattern, gcno_files, error_ctx)
    end subroutine safe_find_gcno_files

    subroutine safe_execute_gcov_on_file(gcov_exe, gcno_file, working_dir, error_ctx)
        !! Securely execute gcov on a single .gcno file
        character(len=*), intent(in) :: gcov_exe, gcno_file, working_dir
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=256) :: output_file
        
        ! Generate output file name
        output_file = ''  ! Let gcov use default output
        
        ! Use secure gcov execution  
        call safe_execute_gcov(gcov_exe, gcno_file, working_dir, &
                               .false., output_file, error_ctx)
    end subroutine safe_execute_gcov_on_file

    subroutine append_generated_files(all_files, new_files)
        !! Append new files to all_files array
        character(len=:), allocatable, intent(inout) :: all_files(:)
        character(len=:), allocatable, intent(in) :: new_files(:)

        character(len=:), allocatable :: temp_files(:)
        integer :: old_size, new_size, i

        if (.not. allocated(all_files)) then
            all_files = new_files
            return
        end if

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
    end subroutine append_generated_files

    subroutine discover_source_mappings(gcov_files, source_mappings)
        !! Discover source file mappings from gcov files
        character(len=*), intent(in) :: gcov_files(:)
        type(source_mapping_t), allocatable, intent(out) :: source_mappings(:)

        integer :: i, num_mappings
        character(len=256) :: source_file
        
        num_mappings = size(gcov_files)
        
        ! Always allocate source_mappings, even if empty
        if (allocated(source_mappings)) deallocate(source_mappings)
        allocate(source_mappings(num_mappings))

        ! For each gcov file, extract source file mapping
        do i = 1, num_mappings
            call extract_source_from_gcov(gcov_files(i), source_file)
            
            source_mappings(i)%gcov_file = trim(gcov_files(i))
            source_mappings(i)%source_file = trim(source_file)
            source_mappings(i)%build_path = extract_build_path(gcov_files(i))
        end do
    end subroutine discover_source_mappings

    subroutine extract_source_from_gcov(gcov_file, source_file)
        !! Extract source file name from gcov file name
        character(len=*), intent(in) :: gcov_file
        character(len=*), intent(out) :: source_file

        integer :: last_slash, last_dot
        character(len=256) :: basename

        ! Extract basename from path
        last_slash = index(gcov_file, '/', back=.true.)
        if (last_slash > 0) then
            basename = gcov_file(last_slash+1:)
        else
            basename = gcov_file
        end if

        ! Remove .gcov extension
        last_dot = index(basename, '.gcov', back=.true.)
        if (last_dot > 0) then
            source_file = basename(1:last_dot-1)
        else
            source_file = basename
        end if
    end subroutine extract_source_from_gcov

    function extract_build_path(gcov_file) result(build_path)
        !! Extract build directory path from gcov file path
        character(len=*), intent(in) :: gcov_file
        character(len=256) :: build_path

        integer :: last_slash

        last_slash = index(gcov_file, '/', back=.true.)
        if (last_slash > 0) then
            build_path = gcov_file(1:last_slash-1)
        else
            build_path = '.'
        end if
    end function extract_build_path

end module gcov_auto_processor