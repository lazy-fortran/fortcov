module gcda_discovery
    !! GCDA File Discovery Module (extracted from zero_configuration_manager)
    !! 
    !! Responsibilities:
    !! - Discover .gcda files across different build system patterns (FPM, CMake, generic)
    !! - Priority-based search across multiple build directory structures
    !! - Support for various build system conventions
    
    use file_utilities, only: find_files_with_glob
    implicit none
    private
    
    ! Public interfaces for GCDA file discovery
    public :: discover_gcda_files_priority
    public :: discover_fpm_gcda_files
    public :: discover_cmake_gcda_files
    public :: discover_generic_gcda_files
    
contains

    function discover_gcda_files_priority() result(gcda_files)
        !! Systematically discover .gcda/.gcno files across different &
        !! build system patterns
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: temp_files(:)
        
        ! Priority 1: FPM build structure
        temp_files = discover_fpm_gcda_files()
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            gcda_files = temp_files
            return
        end if
        
        ! Priority 2: CMake/Make build structure  
        temp_files = discover_cmake_gcda_files()
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            gcda_files = temp_files  
            return
        end if
        
        ! Priority 3: Generic build directory search
        temp_files = discover_generic_gcda_files()
        gcda_files = temp_files
    end function discover_gcda_files_priority
    
    function discover_fpm_gcda_files() result(gcda_files)
        !! Discover .gcda files in FPM build structure
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: temp_files(:), app_files(:), test_files(:)
        logical :: dir_exists
        integer :: total_files, app_count, test_count, stat
        character(len=512) :: errmsg
        
        total_files = 0
        
        ! Check FPM app directory pattern: build/gfortran_*/app/*.gcda
        inquire(file="build", exist=dir_exists)
        if (dir_exists) then
            app_files = find_files_with_glob("build", "gfortran_*/app/*.gcda")
            if (allocated(app_files)) then
                total_files = total_files + size(app_files)
            end if
            
            ! Check FPM test directory pattern: build/gfortran_*/test/*.gcda
            test_files = find_files_with_glob("build", "gfortran_*/test/*.gcda")
            if (allocated(test_files)) then
                total_files = total_files + size(test_files)
            end if
        end if
        
        if (total_files > 0) then
            allocate(character(len=256) :: gcda_files(total_files))
            app_count = 0
            test_count = 0
            
            if (allocated(app_files) .and. size(app_files) > 0) then
                app_count = size(app_files)
                gcda_files(1:app_count) = app_files
            end if
            
            if (allocated(test_files) .and. size(test_files) > 0) then
                test_count = size(test_files)
                gcda_files(app_count+1:app_count+test_count) = test_files
            end if
        else
            allocate(character(len=256) :: gcda_files(0))
        end if
    end function discover_fpm_gcda_files
    
    function discover_cmake_gcda_files() result(gcda_files)
        !! Discover .gcda files in CMake build structure  
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        
        ! Check CMake patterns: build/**/*.gcda, _build/**/*.gcda
        inquire(file="build", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("build", "**/*.gcda")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                gcda_files = temp_files
                return
            end if
        end if
        
        inquire(file="_build", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("_build", "**/*.gcda")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                gcda_files = temp_files
                return
            end if
        end if
        
        allocate(character(len=256) :: gcda_files(0))
    end function discover_cmake_gcda_files
    
    function discover_generic_gcda_files() result(gcda_files)
        !! Discover .gcda files in generic build directory patterns
        character(len=:), allocatable :: gcda_files(:)
        character(len=:), allocatable :: temp_files(:)
        logical :: dir_exists
        
        ! Generic patterns: any directory containing "build" in name
        temp_files = find_files_with_glob(".", "*build*/**/*.gcda")
        if (allocated(temp_files) .and. size(temp_files) > 0) then
            gcda_files = temp_files
            return
        end if
        
        ! Check obj/objects directories
        inquire(file="obj", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("obj", "**/*.gcda")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                gcda_files = temp_files
                return
            end if
        end if
        
        inquire(file="objects", exist=dir_exists)
        if (dir_exists) then
            temp_files = find_files_with_glob("objects", "**/*.gcda")
            if (allocated(temp_files) .and. size(temp_files) > 0) then
                gcda_files = temp_files
                return
            end if
        end if
        
        allocate(character(len=256) :: gcda_files(0))
    end function discover_generic_gcda_files

end module gcda_discovery