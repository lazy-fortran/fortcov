module config_positional_args
    !! Positional argument processing extracted from config_parser_command_line
    !!
    !! Focused on processing coverage files and source paths from command line.
    !! Provides clean separation of positional argument logic.
    use config_types, only: config_t
    implicit none
    private

    public :: add_source_path
    public :: add_string_to_array
    public :: process_positional_arguments
    public :: classify_positional_argument

    ! Private helper functions
    private :: check_if_directory
    private :: check_if_executable_path
    private :: classify_by_existing_path
    private :: classify_by_extension
    private :: finalize_coverage_files_list
    private :: handle_unknown_positional
    private :: initialize_coverage_files_list

contains

    subroutine add_string_to_array(array, new_string)
        character(len=:), allocatable, intent(inout) :: array(:)
        character(len=*), intent(in) :: new_string

        character(len=:), allocatable :: temp_array(:)
        integer :: current_size
        integer :: new_size
        integer :: i
        integer :: new_len

        if (.not. allocated(array)) then
            new_len = max(256, len_trim(new_string))
            allocate (character(len=new_len) :: array(1))
            array(1) = new_string
            return
        end if

        current_size = size(array)
        new_size = current_size + 1
        new_len = max(256, len(array), len_trim(new_string))

        allocate (character(len=new_len) :: temp_array(new_size))
        do i = 1, current_size
            temp_array(i) = array(i)
        end do
        temp_array(new_size) = new_string

        call move_alloc(temp_array, array)

    end subroutine add_string_to_array

    subroutine add_source_path(config, path)
        type(config_t), intent(inout) :: config
        character(len=*), intent(in) :: path

        call add_string_to_array(config%source_paths, path)

    end subroutine add_source_path

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

        call initialize_coverage_files_list(config)

        do i = 1, positional_count
            call classify_positional_argument(positionals(i), is_valid_coverage_file, &
                                              is_source_path)

            if (is_valid_coverage_file) then
                call add_string_to_array(config%coverage_files, positionals(i))
            else if (is_source_path) then
                call add_source_path(config, positionals(i))
            else
                call handle_unknown_positional(config, positionals(i), success, &
                                               error_message)
                if (.not. success) return
            end if
        end do

        call finalize_coverage_files_list(config)

    end subroutine process_positional_arguments

    subroutine classify_positional_argument(arg, is_valid_coverage_file, is_source_path)
        !! Classify a positional argument
        character(len=*), intent(in) :: arg
        logical, intent(out) :: is_valid_coverage_file
        logical, intent(out) :: is_source_path

        logical :: is_executable_path

        is_valid_coverage_file = .false.
        is_source_path = .false.
        is_executable_path = .false.

        call classify_by_extension(arg, is_valid_coverage_file, is_source_path)

        ! Check if this is an executable path before other classifications.
        ! This prevents spurious warnings when shell expansion includes multiple
        ! executables.
        call check_if_executable_path(arg, is_executable_path)

        ! If it is an executable path, silently ignore (no classification needed)
        if (is_executable_path) then
            return
        end if

        ! If not recognized by extension, check if it is an existing file/directory
        if (.not. is_valid_coverage_file .and. .not. is_source_path) then
            call classify_by_existing_path(arg, is_source_path)
        end if

    end subroutine classify_positional_argument

    subroutine initialize_coverage_files_list(config)
        type(config_t), intent(inout) :: config

        if (allocated(config%coverage_files)) deallocate (config%coverage_files)
        allocate (character(len=0) :: config%coverage_files(0))

    end subroutine initialize_coverage_files_list

    subroutine finalize_coverage_files_list(config)
        type(config_t), intent(inout) :: config

        if (allocated(config%coverage_files)) then
            if (size(config%coverage_files) == 0) then
                deallocate (config%coverage_files)
            end if
        end if

    end subroutine finalize_coverage_files_list

    subroutine handle_unknown_positional(config, arg, success, error_message)
        type(config_t), intent(in) :: config
        character(len=*), intent(in) :: arg
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message

        logical :: is_executable_path

        success = .true.
        error_message = ""

        call check_if_executable_path(arg, is_executable_path)
        if (is_executable_path) return

        if (config%strict_mode) then
            success = .false.
            error_message = "Unknown positional argument: "//trim(arg)
            return
        end if

        if (config%verbose) then
            print '(A)', "Warning: Ignoring unknown argument: "//trim(arg)
        end if

    end subroutine handle_unknown_positional

    subroutine classify_by_extension(arg, is_valid_coverage_file, is_source_path)
        character(len=*), intent(in) :: arg
        logical, intent(out) :: is_valid_coverage_file
        logical, intent(out) :: is_source_path

        character(len=:), allocatable :: extension
        integer :: dot_pos

        is_valid_coverage_file = .false.
        is_source_path = .false.

        dot_pos = index(arg, '.', back=.true.)
        if (dot_pos <= 0) return

        extension = arg(dot_pos + 1:)
        select case (trim(adjustl(extension)))
        case ("gcov", "info")
            is_valid_coverage_file = .true.
        case ("json")
            if (index(arg, ".gcov.json") > 0 .or. index(arg, "coverage") > 0) then
                is_valid_coverage_file = .true.
            end if
        case ("xml")
            if (index(arg, "coverage") > 0 .or. index(arg, "cobertura") > 0) then
                is_valid_coverage_file = .true.
            end if
        case ("f90", "f95", "f03", "f08", "f", "for")
            is_source_path = .true.
        end select

    end subroutine classify_by_extension

    subroutine classify_by_existing_path(arg, is_source_path)
        character(len=*), intent(in) :: arg
        logical, intent(inout) :: is_source_path

        logical :: file_exists, is_directory

        inquire (file=trim(arg), exist=file_exists)
        if (.not. file_exists) return

        call check_if_directory(arg, is_directory)
        if (is_directory) is_source_path = .true.

    end subroutine classify_by_existing_path

    subroutine check_if_directory(path, is_dir)
        character(len=*), intent(in) :: path
        logical, intent(out) :: is_dir

        integer :: unit, iostat

        is_dir = .false.
        open (newunit=unit, file=trim(path)//'/..', status='old', action='read', &
              iostat=iostat)
        if (iostat == 0) then
            is_dir = .true.
            close (unit)
        end if

    end subroutine check_if_directory

    subroutine check_if_executable_path(path, is_executable)
        !! Check if path is an executable file (fix for issue #918)
        !! This prevents spurious warnings when shell expansion includes multiple
        !! fortcov executables.
        character(len=*), intent(in) :: path
        logical, intent(out) :: is_executable

        logical :: file_exists
        integer :: unit, iostat

        is_executable = .false.

        ! Check if file exists
        inquire (file=trim(path), exist=file_exists)
        if (.not. file_exists) return

        ! Check if path contains fortcov and is in a build directory structure
        if (index(path, 'fortcov') > 0 .and. &
            (index(path, 'build/') > 0 .or. index(path, '/app/') > 0)) then

            ! Additional verification: try to open for read as binary executable
            ! This is a heuristic - if the file can be opened and contains fortcov
            ! in the path with build structure, it is likely an executable
            open (newunit=unit, file=trim(path), status='old', &
                  action='read', form='unformatted', access='stream', iostat=iostat)
            if (iostat == 0) then
                close (unit)
                is_executable = .true.
            end if
        end if

    end subroutine check_if_executable_path

end module config_positional_args
