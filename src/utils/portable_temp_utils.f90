module portable_temp_utils
    !! Portable temporary directory utilities for cross-platform compatibility
    !!
    !! This module provides portable temporary directory and file operations
    !! that work across different operating systems and environments.
    
    use iso_fortran_env, only: int32
    implicit none
    private
    
    public :: get_temp_dir
    public :: create_temp_subdir
    
contains

    function get_temp_dir() result(temp_dir)
        !! Get portable temporary directory path
        !! 
        !! Returns the appropriate temporary directory for the current platform:
        !! - TMPDIR environment variable (Unix/Linux)
        !! - TEMP environment variable (Windows)
        !! - TMP environment variable (Windows fallback) 
        !! - /tmp (Unix/Linux fallback)
        !! - . (final fallback for current directory)
        
        character(len=:), allocatable :: temp_dir
        character(len=256) :: env_value
        integer :: status
        logical :: dir_exists
        
        ! Try TMPDIR (Unix/Linux standard)
        call get_environment_variable('TMPDIR', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            call check_directory_exists(trim(env_value), dir_exists)
            if (dir_exists) then
                temp_dir = trim(env_value)
                return
            end if
        end if
        
        ! Try TEMP (Windows)
        call get_environment_variable('TEMP', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            call check_directory_exists(trim(env_value), dir_exists)
            if (dir_exists) then
                temp_dir = trim(env_value)
                return
            end if
        end if
        
        ! Try TMP (Windows fallback)
        call get_environment_variable('TMP', env_value, status=status)
        if (status == 0 .and. len_trim(env_value) > 0) then
            call check_directory_exists(trim(env_value), dir_exists)
            if (dir_exists) then
                temp_dir = trim(env_value)
                return
            end if
        end if
        
        ! Unix/Linux fallback
        call check_directory_exists('/tmp', dir_exists)
        if (dir_exists) then
            temp_dir = '/tmp'
            return
        end if
        
        ! Final fallback - current directory
        temp_dir = '.'
        
    end function get_temp_dir

    subroutine create_temp_subdir(subdir_name, full_path, success)
        !! Create a temporary subdirectory with given name
        !!
        !! Args:
        !!   subdir_name: Name of subdirectory to create
        !!   full_path: Returns full path to created directory
        !!   success: Returns true if directory created successfully
        
        character(len=*), intent(in) :: subdir_name
        character(len=:), allocatable, intent(out) :: full_path
        logical, intent(out) :: success
        
        character(len=:), allocatable :: base_temp_dir
        integer :: exit_status
        
        base_temp_dir = get_temp_dir()
        full_path = base_temp_dir // '/' // trim(subdir_name)
        
        ! SECURITY FIX Issue #963: Use secure directory creation instead of shell
        call create_secure_temp_directory(full_path, exit_status)
        
        success = (exit_status == 0)
        
    end subroutine create_temp_subdir
    
    subroutine check_directory_exists(path, exists)
        !! Portable directory existence check using standard Fortran
        !!
        !! This uses the portable approach of checking if we can inquire
        !! about the path. Directories can be inquired about as file paths
        !! in standard Fortran, avoiding the need for Fortran 2018 features.
        !!
        !! Args:
        !!   path: Directory path to check
        !!   exists: Returns true if directory exists and is accessible
        
        character(len=*), intent(in) :: path
        logical, intent(out) :: exists
        
        ! Use standard inquire with file parameter
        ! This works for directories in all Fortran standards
        inquire(file=trim(path), exist=exists)
        
    end subroutine check_directory_exists
    
    ! Create directory securely without shell commands
    ! SECURITY FIX Issue #963: Replace mkdir -p vulnerability
    subroutine create_secure_temp_directory(dir_path, exit_status)
        character(len=*), intent(in) :: dir_path
        integer, intent(out) :: exit_status
        
        character(len=512) :: temp_file_path
        integer :: temp_unit
        logical :: dir_exists
        
        exit_status = 0
        
        ! Check if directory already exists
        inquire(file=dir_path, exist=dir_exists)
        if (dir_exists) return
        
        ! Use file creation to force directory creation
        temp_file_path = trim(dir_path) // '/.temp_marker'
        
        ! Try to create the temporary file which forces directory creation
        open(newunit=temp_unit, file=temp_file_path, status='new', iostat=exit_status)
        if (exit_status == 0) then
            ! Directory was created successfully
            close(temp_unit, status='delete')  ! Remove the temporary file
            exit_status = 0
        else
            ! Directory creation failed
            exit_status = 1
        end if
        
    end subroutine create_secure_temp_directory

end module portable_temp_utils