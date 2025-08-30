module config_positional_args
    !! Positional argument processing extracted from config_parser_command_line
    !! 
    !! Focused on processing coverage files and source paths from command line.
    !! Provides clean separation of positional argument logic.
    use config_types, only: config_t, MAX_ARRAY_SIZE
    use config_parser_utils, only: add_string_to_array, add_source_path
    use file_utils_core
    implicit none
    private
    
    public :: process_positional_arguments
    public :: classify_positional_argument
    
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

        success = .true.
        error_message = ""

        ! Allocate coverage files array
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
                ! Unknown positional argument
                if (config%strict_mode) then
                    success = .false.
                    error_message = "Unknown positional argument: " // trim(positionals(i))
                    return
                else if (config%verbose) then
                    print '(A)', "Warning: Ignoring unknown argument: " // trim(positionals(i))
                end if
            end if
        end do

    end subroutine process_positional_arguments

    subroutine classify_positional_argument(arg, is_valid_coverage_file, is_source_path)
        !! Classify a positional argument
        character(len=*), intent(in) :: arg
        logical, intent(out) :: is_valid_coverage_file
        logical, intent(out) :: is_source_path

        logical :: file_exists, is_directory
        character(len=:), allocatable :: extension
        integer :: dot_pos

        is_valid_coverage_file = .false.
        is_source_path = .false.

        ! First check file extension to recognize coverage files by pattern
        ! (even if they don't exist yet - Issue #395 fork bomb prevention)
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

        ! If not recognized by extension, check if it's an existing file/directory
        if (.not. is_valid_coverage_file .and. .not. is_source_path) then
            inquire(file=trim(arg), exist=file_exists)
            if (file_exists) then
                ! Check if it's a directory
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

end module config_positional_args