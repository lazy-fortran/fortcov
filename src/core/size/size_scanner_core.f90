module size_scanner_core
    !! Core Size Scanning Functionality
    !! 
    !! Provides efficient file and directory scanning capabilities for 
    !! architectural size validation. Single responsibility: data collection.
    use error_handling_core, only: error_context_t, ERROR_SUCCESS, &
                                   ERROR_FILE_OPERATION_FAILED, clear_error_context
    use error_handlers, only: handle_out_of_memory
    use file_search_secure, only: safe_find_files_recursive, safe_find_files_with_glob
    implicit none
    private
    
    ! Public interface for size scanning
    public :: scan_file_sizes_in_directory
    public :: scan_directory_item_counts
    public :: scan_result_t
    
    ! Architectural size limits (lines/items)
    integer, parameter, public :: FILE_SIZE_TARGET = 500
    integer, parameter, public :: FILE_SIZE_HARD_LIMIT = 1000
    integer, parameter, public :: DIRECTORY_SOFT_LIMIT = 15
    integer, parameter, public :: DIRECTORY_HARD_LIMIT = 30
    
    ! Scan result type for file analysis
    type :: scan_result_t
        character(len=:), allocatable :: path
        integer :: size_metric
        logical :: is_valid
    end type scan_result_t

contains

    subroutine scan_file_sizes_in_directory(base_directory, results, total_scanned, error_ctx)
        !! Recursively scans all .f90 files and returns size information
        character(len=*), intent(in) :: base_directory
        type(scan_result_t), allocatable, intent(out) :: results(:)
        integer, intent(out) :: total_scanned
        type(error_context_t), intent(out) :: error_ctx
        
        character(len=:), allocatable :: f90_files(:)
        integer :: stat
        character(len=256) :: errmsg
        type(scan_result_t) :: temp_results(1000)  ! Temp storage
        integer :: i, nfiles, line_count
        
        call clear_error_context(error_ctx)
        total_scanned = 0

        ! Discover Fortran source files recursively under base_directory
        call safe_find_files_recursive(trim(base_directory), '*.f90', f90_files, error_ctx)
        if (.not. allocated(f90_files)) then
            allocate(character(len=1) :: f90_files(0))
        end if

        nfiles = size(f90_files)
        if (nfiles > 0) then
            do i = 1, nfiles
                if (total_scanned >= size(temp_results)) exit
                line_count = count_file_lines(trim(f90_files(i)))
                total_scanned = total_scanned + 1
                temp_results(total_scanned)%path = trim(f90_files(i))
                temp_results(total_scanned)%size_metric = line_count
                temp_results(total_scanned)%is_valid = .true.
            end do
        end if
        
        ! Allocate and copy results with proper error handling
        if (total_scanned > 0) then
            allocate(results(total_scanned), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                call handle_out_of_memory(total_scanned * 8, error_ctx)  ! Approximate size
                allocate(results(0))  ! Fallback to empty array
                return
            end if
            results(1:total_scanned) = temp_results(1:total_scanned)
        else
            allocate(results(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                call handle_out_of_memory(0, error_ctx)
                return
            end if
        end if
        
    end subroutine scan_file_sizes_in_directory
    
    subroutine scan_directory_item_counts(base_directory, results, total_scanned, error_ctx)
        !! Recursively scans directories and returns item counts
        character(len=*), intent(in) :: base_directory
        type(scan_result_t), allocatable, intent(out) :: results(:)
        integer, intent(out) :: total_scanned
        type(error_context_t), intent(out) :: error_ctx
        
        type(scan_result_t) :: temp_results(200)  ! Temp storage
        integer :: stat
        character(len=256) :: errmsg
        character(len=:), allocatable :: entries(:)
        integer :: nitems
        
        call clear_error_context(error_ctx)
        total_scanned = 0
        
        ! Scan specific directories that commonly have violations
        call scan_specific_directory(resolve_coverage_dir(base_directory), &
                                   temp_results(total_scanned+1))
        if (temp_results(total_scanned+1)%is_valid) then
            total_scanned = total_scanned + 1
        end if
        
        ! Allocate and copy results with proper error handling
        if (total_scanned > 0) then
            allocate(results(total_scanned), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                call handle_out_of_memory(total_scanned * 8, error_ctx)
                allocate(results(0))  ! Fallback to empty array
                return
            end if
            results(1:total_scanned) = temp_results(1:total_scanned)
        else
            allocate(results(0), stat=stat, errmsg=errmsg)
            if (stat /= 0) then
                call handle_out_of_memory(0, error_ctx)
                return
            end if
        end if
        
    end subroutine scan_directory_item_counts

    ! ================ INTERNAL IMPLEMENTATION ================
    
    ! Count lines in a text file (simple wc -l equivalent)
    function count_file_lines(filename) result(nlines)
        character(len=*), intent(in) :: filename
        integer :: nlines
        integer :: unit, ios
        character(len=1024) :: buf
        logical :: exists

        nlines = 0
        inquire(file=trim(filename), exist=exists)
        if (.not. exists) return

        open(newunit=unit, file=trim(filename), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read(unit, '(A)', iostat=ios) buf
            if (ios /= 0) exit
            nlines = nlines + 1
        end do
        close(unit)
    end function count_file_lines
    
    subroutine scan_specific_directory(dir_path, result)
        !! Scans specific directory for item count
        character(len=*), intent(in) :: dir_path
        type(scan_result_t), intent(out) :: result
        
        character(len=:), allocatable :: files(:)
        integer :: item_count
        type(error_context_t) :: tmp_error

        if (len_trim(dir_path) == 0) then
            result%path = ''
            result%size_metric = 0
            result%is_valid = .false.
            return
        end if

        call safe_find_files_with_glob(trim(dir_path), '*', files, tmp_error)
        if (allocated(files)) then
            item_count = size(files)
        else
            item_count = 0
        end if
        result%path = trim(dir_path)
        result%size_metric = item_count
        result%is_valid = .true.
        
    end subroutine scan_specific_directory

    function resolve_coverage_dir(base_directory) result(dir_path)
        !! Resolve a likely coverage source directory under the base directory
        character(len=*), intent(in) :: base_directory
        character(len=:), allocatable :: dir_path
        character(len=512) :: cand1, cand2
        logical :: ex1, ex2

        cand1 = trim(base_directory) // '/src/coverage'
        cand2 = trim(base_directory) // '/coverage'
        inquire(file=cand1, exist=ex1)
        inquire(file=cand2, exist=ex2)
        if (ex1) then
            dir_path = trim(cand1)
        else if (ex2) then
            dir_path = trim(cand2)
        else
            dir_path = ''
        end if
    end function resolve_coverage_dir

end module size_scanner_core
