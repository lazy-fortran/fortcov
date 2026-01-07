module gcov_generator
    !! GCOV File Generation Module (extracted from zero_configuration_manager)
    !!
    !! Responsibilities:
    !! - Generate .gcov files from .gcda files using gcov command
    !! - Handle gcov command execution safely
    !! - Extract source file paths from .gcda files
    !! - Check gcov executable availability

   use gcov_executor, only: gcov_executor_t
   use path_security, only: validate_executable_path
   use error_handling_core, only: error_context_t, ERROR_SUCCESS
   use file_utilities, only: ensure_directory
   implicit none
   private

   ! Public interfaces for gcov file generation
   public :: generate_gcov_files_from_gcda
   public :: check_gcov_availability
   public :: extract_source_from_gcda

contains

   subroutine check_gcov_availability(gcov_available)
        !! Check if gcov executable is available in PATH
      logical, intent(out) :: gcov_available
      type(error_context_t) :: error_ctx
      character(len=:), allocatable :: safe_gcov

      ! Try to validate gcov executable path - if it passes, gcov is available
      call validate_executable_path("gcov", safe_gcov, error_ctx)
      gcov_available = (error_ctx%error_code == ERROR_SUCCESS)
   end subroutine check_gcov_availability

   subroutine generate_gcov_files_from_gcda(gcda_files, gcov_output_dir, &
                                            generated_gcov_files)
        !! Safely execute gcov commands to generate .gcov files from .gcda files
      character(len=*), intent(in) :: gcda_files(:)
      character(len=*), intent(in) :: gcov_output_dir
      character(len=:), allocatable, intent(out) :: generated_gcov_files(:)

      type(gcov_executor_t) :: executor
      type(error_context_t) :: error_ctx
      integer :: i, success_count, stat
      character(len=512) :: errmsg
      character(len=256), allocatable :: temp_generated_files(:)
      character(len=:), allocatable :: temp_files(:)
      logical :: dir_created
      character(len=256) :: output_dir

      ! Configure gcov executor for zero-config mode
      output_dir = trim(gcov_output_dir)
      if (len_trim(output_dir) == 0) then
         output_dir = "build/gcov"
      end if
      call executor%set_gcov_output_directory(output_dir)
      call executor%set_working_directory(".")

      ! Ensure output directory exists
      call ensure_directory(trim(output_dir), dir_created)

      success_count = 0
      allocate (character(len=256) :: &
                temp_generated_files(size(gcda_files)*10), &
                stat=stat, errmsg=errmsg)  ! Estimate
      if (stat /= 0) then
         write (*, '(A)') "Error: Memory allocation failed for temp_generated_files: "// &
            trim(errmsg)
         allocate (character(len=256) :: generated_gcov_files(0))
         return
      end if

      do i = 1, size(gcda_files)
         ! Pass the gcda file path directly - gcov can determine the source
         ! from notes stored in the gcda/gcno files.
         call executor%execute_gcov(gcda_files(i), temp_files, error_ctx)

         if (error_ctx%error_code == ERROR_SUCCESS) then
            if (allocated(temp_files)) then
               if (size(temp_files) > 0) then
                  temp_generated_files(success_count + 1:success_count + &
                                       size(temp_files)) = temp_files
                  success_count = success_count + size(temp_files)
               end if
            end if
         end if
      end do

      ! Return successfully generated .gcov files
      if (success_count > 0) then
         allocate (character(len=256) :: generated_gcov_files(success_count), &
                   stat=stat, errmsg=errmsg)
         if (stat /= 0) then
            write (*, '(A)') "Error: Memory allocation failed for generated_gcov_files: "// &
               trim(errmsg)
            allocate (character(len=256) :: generated_gcov_files(0))
            return
         end if
         generated_gcov_files(1:success_count) = &
            temp_generated_files(1:success_count)
      else
         allocate (character(len=256) :: generated_gcov_files(0), &
                   stat=stat, errmsg=errmsg)
         if (stat /= 0) then
            write (*, '(A)') "Error: Memory allocation failed for generated_gcov_files: "// &
               trim(errmsg)
            return
         end if
      end if
   end subroutine generate_gcov_files_from_gcda

   function extract_source_from_gcda(gcda_file) result(source_file)
        !! Extract source file path from .gcda file path
      character(len=*), intent(in) :: gcda_file
      character(len=256) :: source_file
      integer :: gcda_pos
      logical :: file_exists

      ! Simple extraction: replace .gcda with .f90/.F90 (most common case)
      gcda_pos = index(gcda_file, '.gcda', back=.true.)
      if (gcda_pos > 0) then
         source_file = gcda_file(1:gcda_pos - 1)//".f90"
         ! Check if .f90 exists, otherwise try .F90
         inquire (file=source_file, exist=file_exists)
         if (.not. file_exists) then
            source_file = gcda_file(1:gcda_pos - 1)//".F90"
         end if
      else
         ! Fallback
         source_file = gcda_file
      end if
   end function extract_source_from_gcda

end module gcov_generator
