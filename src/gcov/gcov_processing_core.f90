module gcov_processing_core
    !! GCov Processing Core Module
    !!
    !! Provides core functionality for processing discovered .gcov files
    !! and collecting detailed statistics about coverage processing results.
    !! Handles individual file processing and result aggregation.
    !!
    !! Key Features:
    !! - Individual .gcov file processing with error handling
    !! - Statistics collection for processed files
    !! - Line counting and coverage analysis
    !! - Result finalization and summary generation

    use gcov_file_processor, only: process_gcov_file
    use coverage_model_core, only: coverage_data_t

    implicit none
    private

    ! Result summary for individual file processing
    type, public :: gcov_file_summary_t
        character(len=256) :: filename = ''
        logical :: processed_successfully = .false.
        character(len=256) :: error_message = ''
        integer :: lines_processed = 0
    end type gcov_file_summary_t

    ! Comprehensive result type for gcov processing
    type, public :: gcov_result_t
        logical :: success = .false.
        character(len=512) :: error_message = ''
        integer :: files_processed = 0
        integer :: files_discovered = 0
        integer :: successful_files = 0
        integer :: failed_files = 0
        integer :: total_lines_processed = 0
        type(gcov_file_summary_t), allocatable :: file_summaries(:)
    end type gcov_result_t

    public :: initialize_processing_result
    public :: process_discovered_files
    public :: process_single_gcov_file
    public :: finalize_processing_results
    public :: count_coverage_lines

contains

    subroutine initialize_processing_result(result)
        !! Initialize result structure with default values
        type(gcov_result_t), intent(out) :: result

        result%success = .false.
        result%error_message = ''
        result%files_processed = 0
        result%files_discovered = 0
        result%successful_files = 0
        result%failed_files = 0
        result%total_lines_processed = 0

        if (allocated(result%file_summaries)) deallocate(result%file_summaries)
    end subroutine initialize_processing_result

    subroutine process_discovered_files(gcov_files, result)
        !! Process each discovered .gcov file and collect statistics
        character(len=*), intent(in) :: gcov_files(:)
        type(gcov_result_t), intent(inout) :: result

        integer :: i, file_count
        type(coverage_data_t) :: coverage_data
        logical :: error_flag

        file_count = size(gcov_files)
        allocate(result%file_summaries(file_count))

        ! Process each file
        do i = 1, file_count
            call process_single_gcov_file(gcov_files(i), result%file_summaries(i), &
                                        coverage_data, error_flag)

            result%files_processed = result%files_processed + 1

            if (.not. error_flag) then
                result%successful_files = result%successful_files + 1
                result%total_lines_processed = result%total_lines_processed + &
                                              result%file_summaries(i)%lines_processed
            else
                result%failed_files = result%failed_files + 1
            end if
        end do
    end subroutine process_discovered_files

    subroutine process_single_gcov_file(gcov_file, file_summary, coverage_data, error_flag)
        !! Process a single .gcov file and collect detailed statistics
        character(len=*), intent(in) :: gcov_file
        type(gcov_file_summary_t), intent(out) :: file_summary
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: error_flag

        file_summary%filename = trim(gcov_file)
        file_summary%processed_successfully = .false.
        file_summary%error_message = ''
        file_summary%lines_processed = 0

        ! Process the gcov file using existing processor
        call process_gcov_file(gcov_file, coverage_data, error_flag)

        if (.not. error_flag) then
            file_summary%processed_successfully = .true.
            file_summary%lines_processed = count_coverage_lines(coverage_data)
        else
            file_summary%error_message = 'Failed to process gcov file: ' // trim(gcov_file)
        end if
    end subroutine process_single_gcov_file

    function count_coverage_lines(coverage_data) result(line_count)
        !! Count the total number of coverage lines in processed data
        type(coverage_data_t), intent(in) :: coverage_data
        integer :: line_count

        integer :: i

        line_count = 0

        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                if (allocated(coverage_data%files(i)%lines)) then
                    line_count = line_count + size(coverage_data%files(i)%lines)
                end if
            end do
        end if
    end function count_coverage_lines

    subroutine finalize_processing_results(result)
        !! Determine final success status and generate summary message
        type(gcov_result_t), intent(inout) :: result

        ! Determine overall success - succeed if we processed at least some files
        result%success = (result%files_discovered > 0) .and. &
                        (result%successful_files > 0)

        ! Generate summary message
        if (result%success) then
            write(result%error_message, '(A,I0,A,I0,A)') &
                'Successfully processed ', result%successful_files, &
                ' gcov files with ', result%total_lines_processed, ' coverage lines'
        else if (result%files_discovered == 0) then
            result%error_message = 'No .gcda files found in target directory'
        else if (result%failed_files > 0) then
            write(result%error_message, '(A,I0,A,I0,A)') &
                'Processing completed with ', result%failed_files, &
                ' failures out of ', result%files_processed, ' files'
        else
            result%error_message = 'GCov auto-processing completed with unknown status'
        end if
    end subroutine finalize_processing_results

end module gcov_processing_core