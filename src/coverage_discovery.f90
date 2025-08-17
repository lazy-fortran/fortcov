module coverage_discovery
    !! Coverage Discovery Module
    !!
    !! This module provides functionality to discover and validate coverage files
    !! for automated gcov workflow. It can recursively search directories for
    !! .gcda/.gcno pairs and validate their consistency.
    use iso_fortran_env, only: error_unit
    use file_utils
    use string_utils
    implicit none
    private
    
    ! Public procedures
    public :: discover_coverage_files
    public :: detect_input_type
    public :: validate_coverage_pairs
    
    ! Constants
    integer, parameter :: MAX_FILES = 1000
    integer, parameter :: MAX_PATH_LENGTH = 256
    
contains

    subroutine discover_coverage_files(directory_path, coverage_pairs, pair_count)
        !! Find .gcda/.gcno pairs recursively in given directory
        character(len=*), intent(in) :: directory_path
        character(len=:), allocatable, intent(out) :: coverage_pairs(:)
        integer, intent(out) :: pair_count
        
        character(len=MAX_PATH_LENGTH) :: temp_pairs(MAX_FILES)
        character(len=:), allocatable :: gcda_files(:)
        logical :: dir_exists
        integer :: i
        
        pair_count = 0
        
        ! Check if directory exists
        inquire(file=directory_path, exist=dir_exists)
        if (.not. dir_exists) then
            allocate(character(len=MAX_PATH_LENGTH) :: coverage_pairs(0))
            return
        end if
        
        ! Find all .gcda files recursively
        gcda_files = find_files_recursive(directory_path, "*.gcda")
        
        ! For each .gcda file, check if matching .gcno exists
        do i = 1, size(gcda_files)
            if (has_matching_gcno(gcda_files(i))) then
                pair_count = pair_count + 1
                if (pair_count <= MAX_FILES) then
                    temp_pairs(pair_count) = trim(gcda_files(i))
                end if
            end if
        end do
        
        ! Allocate result array
        if (pair_count > 0) then
            allocate(character(len=MAX_PATH_LENGTH) :: coverage_pairs(pair_count))
            coverage_pairs(1:pair_count) = temp_pairs(1:pair_count)
        else
            allocate(character(len=MAX_PATH_LENGTH) :: coverage_pairs(0))
        end if
    end subroutine discover_coverage_files
    
    subroutine detect_input_type(input_path, input_type)
        !! Detect whether input is a directory or .gcov file
        character(len=*), intent(in) :: input_path
        character(len=*), intent(out) :: input_type
        
        logical :: is_dir, path_exists
        integer :: path_len
        
        ! Check if path exists and is directory
        inquire(file=input_path, exist=path_exists)
        
        if (.not. path_exists) then
            input_type = "nonexistent"
            return
        end if
        
        ! Simple heuristic: if it ends with .gcov, it's a file
        path_len = len_trim(input_path)
        if (path_len >= 5) then
            if (input_path(path_len-4:path_len) == ".gcov") then
                input_type = "gcov_file"
                return
            end if
        end if
        
        ! Check if it's a directory by trying to access as directory
        is_dir = is_directory(input_path)
        if (is_dir) then
            input_type = "directory"
        else
            input_type = "unknown_file"
        end if
    end subroutine detect_input_type
    
    subroutine validate_coverage_pairs(gcda_files, file_count, validation_result, &
                                     missing_files, missing_count)
        !! Validate that each .gcda has a matching .gcno file
        character(len=*), intent(in) :: gcda_files(:)
        integer, intent(in) :: file_count
        logical, intent(out) :: validation_result
        character(len=*), intent(out) :: missing_files(:)
        integer, intent(out) :: missing_count
        
        character(len=MAX_PATH_LENGTH) :: gcno_file
        logical :: gcno_exists
        integer :: i
        
        validation_result = .true.
        missing_count = 0
        
        do i = 1, min(file_count, size(gcda_files))
            if (len_trim(gcda_files(i)) == 0) cycle
            
            ! Generate corresponding .gcno filename
            gcno_file = get_gcno_from_gcda(gcda_files(i))
            
            ! Check if .gcno file exists
            inquire(file=gcno_file, exist=gcno_exists)
            if (.not. gcno_exists) then
                validation_result = .false.
                missing_count = missing_count + 1
                if (missing_count <= size(missing_files)) then
                    missing_files(missing_count) = gcno_file
                end if
            end if
        end do
    end subroutine validate_coverage_pairs
    
    ! Helper functions
    
    function find_files_recursive(directory_path, pattern) result(files)
        !! Find files matching pattern recursively
        character(len=*), intent(in) :: directory_path
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: files(:)
        
        character(len=MAX_PATH_LENGTH) :: search_path
        
        ! Search in given directory with pattern
        search_path = trim(directory_path) // "/" // trim(pattern)
        files = find_files(search_path)
        
        ! TODO: Add true recursive search in subdirectories
        ! For now, just search the given directory
    end function find_files_recursive
    
    function has_matching_gcno(gcda_file) result(has_match)
        !! Check if .gcda file has matching .gcno
        character(len=*), intent(in) :: gcda_file
        logical :: has_match
        
        character(len=MAX_PATH_LENGTH) :: gcno_file
        
        gcno_file = get_gcno_from_gcda(gcda_file)
        inquire(file=gcno_file, exist=has_match)
    end function has_matching_gcno
    
    function get_gcno_from_gcda(gcda_file) result(gcno_file)
        !! Convert .gcda filename to corresponding .gcno filename
        character(len=*), intent(in) :: gcda_file
        character(len=:), allocatable :: gcno_file
        
        integer :: gcda_pos
        
        gcda_pos = index(gcda_file, ".gcda", back=.true.)
        if (gcda_pos > 0) then
            gcno_file = gcda_file(1:gcda_pos-1) // ".gcno"
        else
            gcno_file = trim(gcda_file) // ".gcno"  ! Fallback
        end if
    end function get_gcno_from_gcda
    
    function is_directory(path) result(is_dir)
        !! Simple check if path is a directory
        character(len=*), intent(in) :: path
        logical :: is_dir
        
        logical :: path_exists
        character(len=:), allocatable :: test_files(:)
        
        ! Check if path exists first
        inquire(file=path, exist=path_exists)
        if (.not. path_exists) then
            is_dir = .false.
            return
        end if
        
        ! Try to list files in the path - if this works, it's a directory
        test_files = find_files(trim(path) // "/*")
        if (size(test_files) >= 0) then
            is_dir = .true.
            return
        end if
        
        ! Fallback heuristic: if it doesn't have a file extension, assume directory
        is_dir = (index(path, ".", back=.true.) <= index(path, "/", back=.true.))
        
        ! Additional check: if it ends with .gcov, it's definitely a file
        if (len_trim(path) >= 5) then
            if (path(len_trim(path)-4:len_trim(path)) == ".gcov") then
                is_dir = .false.
            end if
        end if
    end function is_directory

end module coverage_discovery
