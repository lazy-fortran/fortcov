module coverage_processor_gcov
    !! GCov processing workflows extracted from coverage_workflows
    !!
    !! Focused on .gcov file discovery, generation, and processing.
    !! Provides specialized gcov operations separated from other workflow
    !! management functionality.
   use constants_core, only: GCOV_EXTENSION
   use config_core, only: config_t
   use file_utilities, only: find_files, find_files_with_glob, basename, resolve_path
   implicit none
   private

   public :: discover_gcov_files
   public :: auto_generate_gcov_files

contains

   subroutine discover_gcov_files(config, files)
        !! Discovers .gcov files in configured source paths with automatic gcov
        !! generation in sane default mode
      type(config_t), intent(in) :: config
      character(len=:), allocatable, intent(out) :: files(:)

      character(len=:), allocatable :: search_paths(:)
      character(len=:), allocatable :: found_files(:)
      character(len=:), allocatable :: generated_files(:)

      ! Determine search paths based on configuration
      call determine_gcov_search_paths(config, search_paths, found_files)

      ! Search for existing .gcov files if needed
      call search_existing_gcov_files(search_paths, found_files)

      ! Generate gcov files if none found and in auto-discovery mode
      call attempt_gcov_generation(config, found_files, generated_files)

      ! Set final result
      call finalize_gcov_file_result(found_files, generated_files, files)

   end subroutine discover_gcov_files

   subroutine auto_generate_gcov_files(config, generated_files)
        !! Automatically discovers and generates .gcov files from build directories
        !! Implements the core sane default mode functionality for Issue #196
      type(config_t), intent(in) :: config
      character(len=:), allocatable, intent(out) :: generated_files(:)

      character(len=:), allocatable :: build_dirs(:)
      character(len=:), allocatable :: all_gcov_files(:)
      character(len=:), allocatable :: test_gcda(:)
      character(len=:), allocatable :: synthesized(:)
      logical :: success

      ! Find build directories with coverage data
      call find_coverage_build_directories(build_dirs)

      if (.not. allocated(build_dirs) .or. size(build_dirs) == 0) then
         ! Test-friendly path: directly synthesize .gcov from test_build/*.gcda
         test_gcda = find_files('test_build/*.gcda')
         if (allocated(test_gcda) .and. size(test_gcda) > 0) then
            block
               use error_handling_core, only: error_context_t, ERROR_SUCCESS
               use gcov_generation_utils, only: generate_gcov_files
               type(error_context_t) :: ectx
               call generate_gcov_files('test_build', test_gcda, config, &
                                        synthesized, ectx)
               if (ectx%error_code == ERROR_SUCCESS .and. &
                   allocated(synthesized)) then
                  generated_files = synthesized
               end if
            end block
         end if
         return
      end if

      ! Generate gcov files from build directories
      call generate_gcov_from_build_dirs(config, build_dirs, success)

      if (success) then
         ! Copy generated .gcov files to project root and collect them
         call collect_generated_gcov_files(config, all_gcov_files)
         if (allocated(all_gcov_files)) then
            generated_files = all_gcov_files
         end if
      end if

   end subroutine auto_generate_gcov_files

   subroutine find_coverage_build_directories(build_dirs)
        !! Finds build directories containing coverage data files
      character(len=:), allocatable, intent(out) :: build_dirs(:)

      character(len=:), allocatable :: gcda_files(:)
      integer :: i

      ! Find all .gcda files (indicates executed coverage data)
      ! Use secure recursive discovery to handle various build layouts reliably
      block
         use gcov_file_discovery, only: discover_gcda_files
         use error_handling_core, only: error_context_t, ERROR_SUCCESS
         type(error_context_t) :: ectx
         call discover_gcda_files('.', gcda_files, ectx)
         if (ectx%error_code /= ERROR_SUCCESS) then
            ! Leave gcda_files unallocated; fallback logic below will try test_build
         end if
      end block

      ! Test-friendly fallback: also look in a conventional test directory
      if (.not. allocated(gcda_files) .or. size(gcda_files) == 0) then
         block
            character(len=:), allocatable :: test_gcda(:)
            test_gcda = find_files("test_build/*.gcda")
            if (allocated(test_gcda) .and. size(test_gcda) > 0) then
               gcda_files = test_gcda
            end if
         end block
      end if

      if (allocated(gcda_files) .and. size(gcda_files) > 0) then
         call extract_build_directories(gcda_files, build_dirs)
      end if

   end subroutine find_coverage_build_directories

   subroutine extract_build_directories(gcda_files, build_dirs)
      character(len=:), allocatable, intent(in) :: gcda_files(:)
      character(len=:), allocatable, intent(out) :: build_dirs(:)

      character(len=:), allocatable :: raw_dirs(:)
      integer :: i, slash_pos, dir_count, max_len

      max_len = 1
      do i = 1, size(gcda_files)
         max_len = max(max_len, len_trim(gcda_files(i)))
      end do

      allocate (character(len=max_len) :: raw_dirs(size(gcda_files)))
      dir_count = 0

      do i = 1, size(gcda_files)
         slash_pos = index(gcda_files(i), '/', back=.true.)
         if (slash_pos > 0) then
            dir_count = dir_count + 1
            raw_dirs(dir_count) = gcda_files(i) (1:slash_pos - 1)
         end if
      end do

      if (dir_count == 0) then
         allocate (character(len=1) :: build_dirs(0))
         return
      end if

      call deduplicate_directories(raw_dirs(1:dir_count), build_dirs)
   end subroutine extract_build_directories

   subroutine deduplicate_directories(input_dirs, output_dirs)
      character(len=*), intent(in) :: input_dirs(:)
      character(len=:), allocatable, intent(out) :: output_dirs(:)

      character(len=:), allocatable :: resolved(:)
      logical, allocatable :: is_unique(:)
      integer :: i, j, unique_count, dir_count

      dir_count = size(input_dirs)
      if (dir_count == 0) then
         allocate (character(len=1) :: output_dirs(0))
         return
      end if

      allocate (character(len=4096) :: resolved(dir_count))
      allocate (is_unique(dir_count))
      is_unique = .true.

      do i = 1, dir_count
         resolved(i) = resolve_path(trim(input_dirs(i)))
      end do

      do i = 2, dir_count
         do j = 1, i - 1
            if (is_unique(i) .and. &
                trim(resolved(i)) == trim(resolved(j))) then
               is_unique(i) = .false.
               exit
            end if
         end do
      end do

      unique_count = count(is_unique)
      if (unique_count == 0) then
         allocate (character(len=1) :: output_dirs(0))
         return
      end if

      allocate (character(len=len(input_dirs(1))) :: output_dirs(unique_count))
      j = 0
      do i = 1, dir_count
         if (is_unique(i)) then
            j = j + 1
            output_dirs(j) = input_dirs(i)
         end if
      end do
   end subroutine deduplicate_directories

   subroutine collect_generated_gcov_files(config, gcov_files)
        !! Collects generated .gcov files from configured output directory
      type(config_t), intent(in) :: config
      character(len=:), allocatable, intent(out) :: gcov_files(:)

      character(len=:), allocatable :: found_gcov_files(:)
      character(len=256) :: output_dir
      logical :: dir_exists

      ! Find the .gcov files in the gcov output directory if present
      output_dir = trim(config%gcov_output_dir)
      if (len_trim(output_dir) == 0) then
         output_dir = "build/gcov"
      end if
      inquire (file=trim(output_dir), exist=dir_exists)
      if (dir_exists) then
         found_gcov_files = find_files(trim(output_dir)//"/*.gcov")
      end if

      if (allocated(found_gcov_files)) then
         gcov_files = found_gcov_files
      else
         allocate (character(len=1) :: gcov_files(0))
      end if

   end subroutine collect_generated_gcov_files

   subroutine determine_gcov_search_paths(config, search_paths, found_files)
        !! Determine search paths for gcov files based on configuration
      use gcov_file_discovery, only: discover_gcov_files_impl => discover_gcov_files
      use error_handling_core, only: error_context_t, ERROR_SUCCESS
      type(config_t), intent(in) :: config
      character(len=:), allocatable, intent(out) :: search_paths(:)
      character(len=:), allocatable, intent(out) :: found_files(:)

      type(error_context_t) :: ectx
      character(len=256) :: output_dir

      ! Use the secure recursive discovery for gcov files
      call discover_gcov_files_impl(".", found_files, ectx)

      ! If .gcov files found, use them
      if (ectx%error_code == ERROR_SUCCESS .and. &
          allocated(found_files) .and. size(found_files) > 0) then
         return
      end if

      output_dir = trim(config%gcov_output_dir)
      if (len_trim(output_dir) == 0) then
         output_dir = "build/gcov"
      end if

      ! Also check configured gcov output directory
      call discover_gcov_files_impl(trim(output_dir), found_files, ectx)
      if (ectx%error_code == ERROR_SUCCESS .and. &
          allocated(found_files) .and. size(found_files) > 0) then
         return
      end if

      ! Use configured source paths as search locations
      if (allocated(config%source_paths)) then
         search_paths = config%source_paths
      else
         ! Default to current directory
         allocate (character(len=1) :: search_paths(1))
         search_paths(1) = "."
      end if
   end subroutine determine_gcov_search_paths

   subroutine search_existing_gcov_files(search_paths, found_files)
        !! Search for existing .gcov files in configured search paths
      character(len=:), allocatable, intent(in) :: search_paths(:)
      character(len=:), allocatable, intent(inout) :: found_files(:)

      ! Search for existing .gcov files in configured search paths
      if ((.not. allocated(found_files) .or. size(found_files) == 0) .and. &
          allocated(search_paths)) then
         call search_gcov_files_in_paths(search_paths, found_files)
      end if
   end subroutine search_existing_gcov_files

   subroutine attempt_gcov_generation(config, found_files, generated_files)
        !! Attempt automatic gcov generation when no .gcov files found
      type(config_t), intent(in) :: config
      character(len=:), allocatable, intent(in) :: found_files(:)
      character(len=:), allocatable, intent(out) :: generated_files(:)

      ! Only attempt generation if no .gcov files were discovered
      if (.not. allocated(found_files) .or. size(found_files) == 0) then
         call auto_generate_gcov_files(config, generated_files)
      end if
   end subroutine attempt_gcov_generation

   subroutine finalize_gcov_file_result(found_files, generated_files, files)
        !! Set final result for gcov file discovery
      character(len=:), allocatable, intent(in) :: found_files(:)
      character(len=:), allocatable, intent(in) :: generated_files(:)
      character(len=:), allocatable, intent(out) :: files(:)

      if (allocated(generated_files)) then
         files = generated_files
      else if (allocated(found_files)) then
         files = found_files
      else
         ! CRITICAL FIX: Ensure files is always allocated to prevent memory issues
         ! If no files found and no files generated, allocate empty array
         allocate (character(len=1) :: files(0))
      end if
   end subroutine finalize_gcov_file_result

   subroutine generate_gcov_from_build_dirs(config, build_dirs, success)
        !! Generate gcov files from build directories with gcno/gcda compatibility
        !! checking
      type(config_t), intent(in) :: config
      character(len=*), intent(in) :: build_dirs(:)
      logical, intent(out) :: success

      character(len=300) :: command
      character(len=:), allocatable :: gcov_exe
      integer :: i, exit_status, gcno_count, gcda_count
      logical :: has_compatible_files

      success = .false.

      ! SECURITY FIX Issue #963: Use hardcoded 'gcov' command - no user configuration
      gcov_exe = "gcov"

      ! Process each build directory
      do i = 1, size(build_dirs)
         ! Check for gcno/gcda file compatibility before running gcov
         call check_coverage_file_compatibility(trim(build_dirs(i)), &
                                                has_compatible_files, gcno_count, &
                                                gcda_count)

         if (has_compatible_files) then
            ! SECURITY FIX Issue #963: Generate gcov files using secure execution
            call generate_gcov_files_secure(trim(build_dirs(i)), trim(gcov_exe), &
                                            config%gcov_output_dir, exit_status)

            if (exit_status == 0) then
               success = .true.
            end if
         else
            ! Report incompatibility issue for debugging
            if (.not. config%quiet) then
               print *, "Warning: Skipping directory due to gcno/gcda mismatch:", &
                  trim(build_dirs(i))
               print *, "   .gcno files:", gcno_count, ", .gcda files:", gcda_count
            end if
         end if
      end do
   end subroutine generate_gcov_from_build_dirs

   subroutine search_gcov_files_in_paths(search_paths, found_files)
        !! Searches for .gcov files in specified search paths
      character(len=*), intent(in) :: search_paths(:)
      character(len=:), allocatable, intent(out) :: found_files(:)

      character(len=:), allocatable :: path_files(:)
      character(len=:), allocatable :: all_files(:)
      integer :: i, total_files, current_size
      logical :: path_exists

      ! Initialize to empty
      allocate (character(len=1) :: all_files(0))
      total_files = 0

      ! Search each path for .gcov files
      do i = 1, size(search_paths)
         ! Check if path exists before searching
         inquire (file=trim(search_paths(i)), exist=path_exists)
         if (path_exists) then
            ! Search for .gcov files in this path
            path_files = find_files(trim(search_paths(i))//"/*"//GCOV_EXTENSION)

            ! Merge with existing files
            if (allocated(path_files) .and. size(path_files) > 0) then
               current_size = size(all_files)
               call expand_file_array(all_files, path_files)
               total_files = total_files + size(path_files)
            end if
         end if
      end do

      ! Return the found files
      if (total_files > 0) then
         found_files = all_files
      else
         ! Return empty array
         allocate (character(len=1) :: found_files(0))
      end if

   end subroutine search_gcov_files_in_paths

   subroutine expand_file_array(all_files, new_files)
        !! Expands file array to include new files
      character(len=:), allocatable, intent(inout) :: all_files(:)
      character(len=*), intent(in) :: new_files(:)

      character(len=:), allocatable :: temp_files(:)
      integer :: old_size, new_size, i

      old_size = size(all_files)
      new_size = old_size + size(new_files)

      ! Create temporary array with combined size
      allocate (character(len=max(len(all_files), len(new_files))) &
                :: temp_files(new_size))

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

   end subroutine expand_file_array

   subroutine check_coverage_file_compatibility(build_path, has_compatible_files, &
                                                gcno_count, gcda_count)
        !! Check if gcno and gcda files are present and compatible in build directory
      character(len=*), intent(in) :: build_path
      logical, intent(out) :: has_compatible_files
      integer, intent(out) :: gcno_count, gcda_count

      character(len=:), allocatable :: gcno_files(:), gcda_files(:)

      has_compatible_files = .false.
      gcno_count = 0
      gcda_count = 0

      ! Find .gcno files in build directory
      gcno_files = find_files(trim(build_path)//"/*.gcno")
      if (allocated(gcno_files)) then
         gcno_count = size(gcno_files)
      end if

      ! Find .gcda files in build directory
      gcda_files = find_files(trim(build_path)//"/*.gcda")
      if (allocated(gcda_files)) then
         gcda_count = size(gcda_files)
      end if

      ! Files are compatible if both types exist
      ! Note: We do not require exact count matching since some files might not
      ! be executed
      has_compatible_files = (gcno_count > 0 .and. gcda_count > 0)

   end subroutine check_coverage_file_compatibility

   ! Secure directory creation for gcov output
   ! SECURITY FIX Issue #963: Replace mkdir -p shell vulnerability
   subroutine create_gcov_output_directory(dir_path, exit_status)
      use directory_operations, only: ensure_directory_safe
      use error_handling_core, only: error_context_t, ERROR_SUCCESS
      character(len=*), intent(in) :: dir_path
      integer, intent(out) :: exit_status
      type(error_context_t) :: error_ctx

      call ensure_directory_safe(trim(dir_path), error_ctx)
      if (error_ctx%error_code == ERROR_SUCCESS) then
         exit_status = 0
      else
         exit_status = 1
      end if

   end subroutine create_gcov_output_directory

   ! Secure file copying without shell commands
   ! SECURITY FIX Issue #963: Replace find -exec cp shell vulnerability
   subroutine copy_gcov_files_secure(source_dir, target_dir)
      use file_ops_secure, only: safe_move_file
      use error_handling_core, only: error_context_t
      character(len=*), intent(in) :: source_dir, target_dir
      character(len=:), allocatable :: files(:)
      character(len=256) :: dst
      type(error_context_t) :: err
      integer :: i

      files = find_files_with_glob(trim(source_dir), '*.gcov')
      if (.not. allocated(files) .or. size(files) == 0) return

      do i = 1, size(files)
         dst = trim(target_dir)//'/'//trim(basename(files(i)))
         call safe_move_file(trim(files(i)), trim(dst), err)
      end do

   end subroutine copy_gcov_files_secure

   ! Secure gcov generation without shell cd && commands
   ! SECURITY FIX Issue #963: Replace cd && gcov shell vulnerability
   subroutine generate_gcov_files_secure(build_path, gcov_exe, gcov_output_dir, &
                                         exit_status)
      use gcov_generator, only: generate_gcov_files_from_gcda
      use file_utilities, only: find_files
      character(len=*), intent(in) :: build_path, gcov_exe
      character(len=*), intent(in) :: gcov_output_dir
      integer, intent(out) :: exit_status
      character(len=:), allocatable :: gcda(:)
      character(len=:), allocatable :: generated(:)

      exit_status = 1

      gcda = find_files(trim(build_path)//'/*.gcda')
      if (.not. allocated(gcda) .or. size(gcda) == 0) return

      call generate_gcov_files_from_gcda(gcda, gcov_output_dir, generated)
      if (allocated(generated) .and. size(generated) > 0) then
         exit_status = 0
      end if

   end subroutine generate_gcov_files_secure

end module coverage_processor_gcov
