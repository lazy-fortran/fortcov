module config_positional_args
    !! Positional argument processing extracted from config_parser_command_line
    !! 
    !! Focused on processing coverage files and source paths from command line.
    !! Provides clean separation of positional argument logic.
    use config_types, only: config_t, MAX_ARRAY_SIZE
    use config_parser_utils, only: add_string_to_array, add_source_path
    use file_utilities
    implicit none
    private
    
    public :: process_positional_arguments
    public :: classify_positional_argument
    
    ! Private helper functions
    private :: check_if_executable_path
    
contains

    subroutine process_positional_arguments(positionals, positional_count, &
                                             config, success, error_message)
        !! Process positional arguments (coverage files)
        character(len=*), intent(in) :: positionals(:)
        integer, intent(in) :: positional_count
        type(config_t), intent(inout) :: config
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        integer :: i
        logical :: is_valid_coverage_file
        logical :: is_source_path
        logical :: is_executable_path

        success = .true.
        error_message = ""

        ! Only process if there are positional arguments
        if (positional_count == 0) return

        ! Allocate coverage files array only when needed
        if (allocated(config%coverage_files)) deallocate(config%coverage_files)
        allocate(character(len=0) :: config%coverage_files(0))

        do i = 1, positional_count
            call classify_positional_argument(positionals(i), is_valid_coverage_file, &
                                               is_source_path)

            if (is_valid_coverage_file) then
                ! Add to coverage files
                call add_string_to_array(config%coverage_files, positionals(i))

            else if (is_source_path) then
                ! Add to source paths
                call add_source_path(config, positionals(i))

            else
                ! Check if this is an executable path (silently ignored)
                call check_if_executable_path(positionals(i), is_executable_path)
                
                if (.not. is_executable_path) then
                    ! Unknown positional argument (not an executable)
                    if (config%strict_mode) then
                        success = .false.
                        error_message = "Unknown positional argument: " // trim(positionals(i))
                        return
                    else if (config%verbose) then
                        print '(A)', "Warning: Ignoring unknown argument: " // trim(positionals(i))
                    end if
                end if
                ! Note: executable paths are silently ignored (no warning needed)
            end if
        end do

        ! Deallocate if no coverage files were found - allows auto-discovery
        if (allocated(config%coverage_files) .and. size(config%coverage_files) == 0) then
            deallocate(config%coverage_files)
        end if

    end subroutine process_positional_arguments

    subroutine classify_positional_argument(arg, is_valid_coverage_file, is_source_path)
        !! Classify a positional argument
        character(len=*), intent(in) :: arg
        logical, intent(out) :: is_valid_coverage_file
        logical, intent(out) :: is_source_path

        logical :: file_exists, is_directory, is_executable_path
        character(len=:), allocatable :: extension
        integer :: dot_pos

        is_valid_coverage_file = .false.
        is_source_path = .false.
        is_executable_path = .false.

        ! First check file extension to recognize coverage files by pattern
        ! even if they do not exist yet
        dot_pos = index(arg, '.', back=.true.)
        if (dot_pos > 0) then
            extension = arg(dot_pos+1:)

            select case (trim(adjustl(extension)))
            case ("gcov")
                is_valid_coverage_file = .true.
            case ("json")
                ! Could be coverage JSON
                if (index(arg, ".gcov.json") > 0 .or. &
                    index(arg, "coverage") > 0) then
                    is_valid_coverage_file = .true.
                end if
            case ("xml")
                ! Could be Cobertura XML
                if (index(arg, "coverage") > 0 .or. &
                    index(arg, "cobertura") > 0) then
                    is_valid_coverage_file = .true.
                end if
            case ("info")
                ! LCOV info file
                is_valid_coverage_file = .true.
            case ("f90", "f95", "f03", "f08", "f", "for")
                ! Fortran source file
                is_source_path = .true.
            end select
        end if

        ! Check if this is an executable path before other classifications
        ! This prevents spurious warnings when shell expansion includes multiple executables
        call check_if_executable_path(arg, is_executable_path)
        
        ! If it is an executable path, silently ignore (no classification needed)
        if (is_executable_path) then
            return
        end if

        ! If not recognized by extension, check if it is an existing file/directory
        if (.not. is_valid_coverage_file .and. .not. is_source_path) then
            inquire(file=trim(arg), exist=file_exists)
            if (file_exists) then
                ! Check if it is a directory
                call check_if_directory(arg, is_directory)
                if (is_directory) then
                    is_source_path = .true.
                end if
            end if
        end if

    contains

        subroutine check_if_directory(path, is_dir)
            !! Check if path is a directory
            character(len=*), intent(in) :: path
            logical, intent(out) :: is_dir

            integer :: unit, iostat

            is_dir = .false.

            ! Try to open as directory (will fail for files)
            open(newunit=unit, file=trim(path)//'/..', status='old', &
                 action='read', iostat=iostat)
            if (iostat == 0) then
                is_dir = .true.
                close(unit)
            end if

        end subroutine check_if_directory

    end subroutine classify_positional_argument
    
    subroutine check_if_executable_path(path, is_executable)
        !! Check if path is an executable file (fix for issue #918)
        !! This prevents spurious warnings when shell expansion includes multiple fortcov executables
        character(len=*), intent(in) :: path
        logical, intent(out) :: is_executable

        logical :: file_exists
        integer :: unit, iostat

        is_executable = .false.

        ! Check if file exists
        inquire(file=trim(path), exist=file_exists)
        if (.not. file_exists) return

        ! Check if path contains fortcov and is in a build directory structure
        if (index(path, 'fortcov') > 0 .and. &
            (index(path, 'build/') > 0 .or. index(path, '/app/') > 0)) then
            
            ! Additional verification: try to open for read as binary executable
            ! This is a heuristic - if the file can be opened and contains fortcov
            ! in the path with build structure, it is likely an executable
            open(newunit=unit, file=trim(path), status='old', &
                 action='read', form='unformatted', access='stream', iostat=iostat)
            if (iostat == 0) then
                close(unit)
                is_executable = .true.
            end if
        end if

    end subroutine check_if_executable_path

end module config_positional_args
