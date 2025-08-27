module path_resolver_impl
    implicit none
    private
    
    ! Public procedures
    public :: resolve_path
    public :: basename
    public :: file_exists
    
contains

    function resolve_path(path) result(resolved)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: resolved
        character(len=256) :: cwd
        integer :: stat
        
        if (path(1:1) == "/") then
            ! Already absolute path
            resolved = trim(path)
        else if (path(1:2) == "./") then
            ! Relative to current directory
            call getcwd(cwd, stat)
            if (stat == 0) then
                resolved = trim(cwd) // "/" // path(3:)
            else
                resolved = path
            end if
        else
            ! Relative path without ./
            call getcwd(cwd, stat)
            if (stat == 0) then
                resolved = trim(cwd) // "/" // trim(path)
            else
                resolved = path
            end if
        end if
    end function resolve_path

    ! Helper function - basename functionality
    function basename(filepath) result(name)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable :: name
        integer :: pos
        
        pos = index(trim(filepath), "/", back=.true.)
        if (pos > 0) then
            name = trim(filepath(pos+1:))
        else
            name = trim(filepath)
        end if
    end function basename

    ! Check if file exists
    function file_exists(filename) result(exists)
        character(len=*), intent(in) :: filename
        logical :: exists
        
        inquire(file=filename, exist=exists)
    end function file_exists

end module path_resolver_impl