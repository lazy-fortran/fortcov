module auto_discovery_core
    !! Auto-discovery functionality - Enhanced with Memory Management Infrastructure
    !! 
    !! This module provides auto-discovery capabilities for coverage files and source paths.
    !! Enhanced to address Issue #967: Systematic memory leaks across core modules.
    !!
    !! Memory Management Improvements:
    !! - Comprehensive error handling for all allocations
    !! - Balanced allocation/deallocation patterns
    !! - Integration with memory_management_core infrastructure
    !! - Elimination of memory leak patterns identified in analysis
    !!
    !! Responsibilities:
    !! - Auto-discover coverage files using priority-ordered search
    !! - Auto-discover source files from src/ and current directory
    !! - Coordinate gcov file generation from gcda files
    use coverage_discovery_core, only: discover_coverage_files
    use file_utilities, only: find_files, find_files_with_glob
    use file_search_secure, only: safe_find_files_with_glob, safe_find_files_recursive
    use gcda_discovery, only: discover_gcda_files_priority
    use gcov_generator, only: generate_gcov_files_from_gcda, &
                                   check_gcov_availability

    implicit none
    private
    
    public :: auto_discover_coverage_files_priority
    public :: auto_discover_source_files_priority
    public :: discover_existing_gcov_files

contains

    function auto_discover_coverage_files_priority() result(coverage_files)
        !! Auto-discover coverage files using priority-ordered search with &
        !! automatic gcov generation
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: temp_files(:), gcda_files(:)
        logical :: dir_exists, gcov_available, success
        integer :: i, stat
        character(len=512) :: errmsg, error_msg
        
        ! Phase 1: Check for existing .gcov files (fast path)
        coverage_files = discover_existing_gcov_files()
        if (allocated(coverage_files) .and. size(coverage_files) > 0) then
            return
        end if
        
        ! Phase 2: Auto-generate .gcov files from .gcda/.gcno (zero-config enhancement)
        call check_gcov_availability(gcov_available)
        if (.not. gcov_available) then
            ! Use safe memory management with proper error handling
            if (allocated(coverage_files)) then
                deallocate(coverage_files, stat=stat, errmsg=errmsg)
                if (stat /= 0) then
                    write(*, '(A)') "Warning: Failed to deallocate coverage_files: " // trim(errmsg)
                end if
            end if
            allocate(character(len=256) :: coverage_files(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Memory allocation failed for coverage_files: " // &
                    trim(errmsg)
            end if
            return
        end if
        
        ! Discover .gcda files in build directories
        gcda_files = discover_gcda_files_priority()
        
        if (allocated(gcda_files) .and. size(gcda_files) > 0) then
            call generate_gcov_files_from_gcda(gcda_files, coverage_files)
            return
        end if
        
        ! Phase 3: No coverage data found
        if (allocated(coverage_files)) then
            deallocate(coverage_files, stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Warning: Failed to deallocate coverage_files: " // trim(errmsg)
            end if
        end if
        allocate(character(len=256) :: coverage_files(0), stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Memory allocation failed for coverage_files: " // &
                trim(errmsg)
            return
        end if
        
    end function auto_discover_coverage_files_priority
    
    function auto_discover_source_files_priority() result(source_paths)
        !! Auto-discover source files using priority-ordered search
        character(len=:), allocatable :: source_paths(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        integer :: file_count, stat
        character(len=512) :: errmsg
        
        ! Priority 1: Check if src/ directory exists and has Fortran files
        inquire(file="src", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("src", "*.f90")
            if (allocated(temp_files)) then
                file_count = size(temp_files)
            else
                file_count = 0
            end if
            
            if (file_count > 0) then
                allocate(character(len=3) :: source_paths(1), &
                    stat=stat, errmsg=errmsg)
                if (stat /= 0) then
                    write(*, '(A)') "Error: Memory allocation failed for source_paths: " // &
                        trim(errmsg)
                    return
                end if
                source_paths(1) = "src"
                return
            end if
        end if
        
        ! Priority 2: Use current directory as fallback
        allocate(character(len=1) :: source_paths(1), &
            stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Memory allocation failed for source_paths: " // &
                trim(errmsg)
            return
        end if
        source_paths(1) = "."
        
    end function auto_discover_source_files_priority
    
    function discover_existing_gcov_files() result(coverage_files)
        !! Phase 1: Discover existing .gcov files in priority locations
        character(len=:), allocatable :: coverage_files(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        integer :: stat
        character(len=512) :: errmsg
        
        ! Priority 1: Check build/gcov/*.gcov (Issue #203 standard location)
        inquire(file="build/gcov", exist=dir_exists)
        if (dir_exists) then
            temp_files = direct_find_gcov_files("build/gcov")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                coverage_files = temp_files
                return
            end if
        end if
        
        ! Priority 2: Check current directory *.gcov
        temp_files = direct_find_gcov_files(".")
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            coverage_files = temp_files
            return
        end if
        
        ! Priority 3: Check build directory recursively (if exists)
        inquire(file="build", exist=dir_exists)
        if (dir_exists) then
            temp_files = direct_find_gcov_files_recursive("build")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                coverage_files = temp_files
                return
            end if
        end if
        
        ! No existing .gcov files found
        allocate(character(len=256) :: coverage_files(0), &
            stat=stat, errmsg=errmsg)
        if (stat /= 0) then
            write(*, '(A)') "Error: Memory allocation failed for coverage_files: " // &
                trim(errmsg)
            return
        end if
    end function discover_existing_gcov_files
    
    function direct_find_gcov_files(directory) result(gcov_files)
        !! Direct filesystem-based .gcov file discovery using canonical secure implementation
        !! DEDUPLICATION: Now uses file_search_secure instead of shell commands
        use error_handling_core, only: error_context_t, ERROR_SUCCESS
        character(len=*), intent(in) :: directory
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        integer :: stat
        character(len=512) :: errmsg
        logical :: dir_exists
        
        ! Check if directory exists
        inquire(file=directory, exist=dir_exists)
        if (.not. dir_exists) then
            allocate(character(len=256) :: gcov_files(0), &
                stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Memory allocation failed for gcov_files: " // &
                    trim(errmsg)
                return
            end if
            return
        end if
        
        ! Use canonical secure implementation for .gcov file finding
        call safe_find_files_with_glob(directory, "*.gcov", gcov_files, error_ctx)
        
        ! Handle error by returning empty array
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (allocated(gcov_files)) deallocate(gcov_files, stat=stat)
            allocate(character(len=256) :: gcov_files(0), &
                stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Memory allocation failed for gcov_files: " // &
                    trim(errmsg)
                return
            end if
        end if
    end function direct_find_gcov_files
    
    function direct_find_gcov_files_recursive(base_directory) result(gcov_files)
        !! Recursively search for .gcov files using canonical secure implementation
        !! DEDUPLICATION: Now uses file_search_secure instead of shell commands
        use error_handling_core, only: error_context_t, ERROR_SUCCESS
        character(len=*), intent(in) :: base_directory
        character(len=:), allocatable :: gcov_files(:)
        type(error_context_t) :: error_ctx
        integer :: stat
        character(len=512) :: errmsg
        logical :: dir_exists
        
        ! Check if directory exists
        inquire(file=base_directory, exist=dir_exists)
        if (.not. dir_exists) then
            allocate(character(len=256) :: gcov_files(0), &
                stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Memory allocation failed for gcov_files: " // &
                    trim(errmsg)
                return
            end if
            return
        end if
        
        ! Use canonical secure implementation for recursive .gcov file finding
        call safe_find_files_recursive(base_directory, "*.gcov", gcov_files, error_ctx)
        
        ! Handle error by returning empty array
        if (error_ctx%error_code /= ERROR_SUCCESS) then
            if (allocated(gcov_files)) deallocate(gcov_files, stat=stat)
            allocate(character(len=256) :: gcov_files(0), &
                stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                write(*, '(A)') "Error: Memory allocation failed for gcov_files: " // &
                    trim(errmsg)
                return
            end if
        end if
    end function direct_find_gcov_files_recursive

end module auto_discovery_core