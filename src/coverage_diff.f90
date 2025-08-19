module coverage_diff
    use coverage_model
    implicit none
    
    private
    
    ! Diff type constants
    integer, parameter :: DIFF_UNCHANGED = 0
    integer, parameter :: DIFF_CHANGED = 1
    integer, parameter :: DIFF_ADDED = 2
    integer, parameter :: DIFF_REMOVED = 3
    public :: compute_coverage_diff
    public :: compute_file_diff
    public :: compute_line_diff
    ! Commented out: public :: compute_coverage_diff_with_thresholds
    ! Commented out: public :: compute_enhanced_file_diff
    
contains

    ! Compute coverage difference between baseline and current coverage data
    function compute_coverage_diff(baseline, current, include_unchanged, &
                                   & threshold) result(diff)
        type(coverage_data_t), intent(in) :: baseline
        type(coverage_data_t), intent(in) :: current
        logical, intent(in), optional :: include_unchanged
        real, intent(in), optional :: threshold
        type(coverage_diff_t) :: diff
        
        type(file_diff_t), allocatable :: file_diffs_temp(:)
        integer :: baseline_file_count, current_file_count, max_files
        integer :: i, j, file_diff_count
        logical :: found
        logical :: include_unchanged_flag
        real :: threshold_val
        
        ! Set default parameter values
        include_unchanged_flag = .false.
        if (present(include_unchanged)) include_unchanged_flag = include_unchanged
        
        threshold_val = 0.0
        if (present(threshold)) threshold_val = threshold
        
        ! Get file counts
        baseline_file_count = size(baseline%files)
        current_file_count = size(current%files)
        max_files = baseline_file_count + current_file_count
        
        ! Allocate temporary array for file diffs
        allocate(file_diffs_temp(max_files))
        file_diff_count = 0
        
        ! Process files present in current coverage
        do i = 1, current_file_count
            found = .false.
            do j = 1, baseline_file_count
                if (current%files(i)%filename == baseline%files(j)%filename) then
                    file_diff_count = file_diff_count + 1
                    file_diffs_temp(file_diff_count) = compute_file_diff( &
                        baseline%files(j), current%files(i))
                    found = .true.
                    exit
                end if
            end do
            
            ! File exists in current but not in baseline (new file)
            if (.not. found) then
                file_diff_count = file_diff_count + 1
                file_diffs_temp(file_diff_count) = compute_file_diff_new( &
                    current%files(i))
            end if
        end do
        
        ! Process files present in baseline but not in current (removed files)
        do i = 1, baseline_file_count
            found = .false.
            do j = 1, current_file_count
                if (baseline%files(i)%filename == current%files(j)%filename) then
                    found = .true.
                    exit
                end if
            end do
            
            ! File exists in baseline but not in current (removed file)
            if (.not. found) then
                file_diff_count = file_diff_count + 1
                file_diffs_temp(file_diff_count) = compute_file_diff_removed( &
                    baseline%files(i))
            end if
        end do
        
        ! Create final coverage diff
        diff%file_diffs = file_diffs_temp(1:file_diff_count)
        
        ! Apply threshold filtering if needed
        ! TODO: Implement threshold filtering
        ! if (threshold_val > 0.0) then
        !     call diff%filter_by_threshold()
        ! end if
        
    end function compute_coverage_diff

    ! Compute file difference between baseline and current file
    function compute_file_diff(baseline_file, current_file) result(file_diff)
        type(coverage_file_t), intent(in) :: baseline_file
        type(coverage_file_t), intent(in) :: current_file
        type(file_diff_t) :: file_diff
        
        type(line_diff_t), allocatable :: line_diffs_temp(:)
        integer :: baseline_line_count, current_line_count, max_lines
        integer :: i, j, line_diff_count
        logical :: found
        
        ! Get line counts
        baseline_line_count = size(baseline_file%lines)
        current_line_count = size(current_file%lines)
        max_lines = baseline_line_count + current_line_count
        
        ! Allocate temporary array for line diffs
        allocate(line_diffs_temp(max_lines))
        line_diff_count = 0
        
        ! Process lines present in current file
        do i = 1, current_line_count
            found = .false.
            do j = 1, baseline_line_count
                if (current_file%lines(i)%line_number == &
                    baseline_file%lines(j)%line_number) then
                    line_diff_count = line_diff_count + 1
                    line_diffs_temp(line_diff_count) = compute_line_diff( &
                        baseline_file%lines(j), current_file%lines(i))
                    found = .true.
                    exit
                end if
            end do
            
            ! Line exists in current but not in baseline (new line)
            if (.not. found) then
                line_diff_count = line_diff_count + 1
                line_diffs_temp(line_diff_count) = compute_line_diff_new( &
                    current_file%lines(i))
            end if
        end do
        
        ! Process lines present in baseline but not in current (removed lines)
        do i = 1, baseline_line_count
            found = .false.
            do j = 1, current_line_count
                if (baseline_file%lines(i)%line_number == &
                    current_file%lines(j)%line_number) then
                    found = .true.
                    exit
                end if
            end do
            
            ! Line exists in baseline but not in current (removed line)
            if (.not. found) then
                line_diff_count = line_diff_count + 1
                line_diffs_temp(line_diff_count) = compute_line_diff_removed( &
                    baseline_file%lines(i))
            end if
        end do
        
        ! Create file diff - direct assignment
        file_diff%filename = current_file%filename
        file_diff%line_diffs = line_diffs_temp(1:line_diff_count)
        
    end function compute_file_diff
    
    ! Compute line difference between baseline and current line
    function compute_line_diff(baseline_line, current_line) result(line_diff)
        type(coverage_line_t), intent(in) :: baseline_line
        type(coverage_line_t), intent(in) :: current_line
        type(line_diff_t) :: line_diff
        
        integer :: diff_type
        
        ! Determine diff type based on execution count changes
        if (baseline_line%execution_count == current_line%execution_count) then
            diff_type = DIFF_UNCHANGED
        else
            diff_type = DIFF_CHANGED
        end if
        
        ! Direct assignment - map to correct fields
        line_diff%line_number = baseline_line%line_number
        line_diff%filename = baseline_line%filename
        line_diff%old_count = baseline_line%execution_count
        line_diff%new_count = current_line%execution_count
        if (diff_type == DIFF_UNCHANGED) then
            line_diff%status = '='
        else
            line_diff%status = '~'
        end if
    end function compute_line_diff
    
    ! Compute line diff for new line (added)
    function compute_line_diff_new(current_line) result(line_diff)
        type(coverage_line_t), intent(in) :: current_line
        type(line_diff_t) :: line_diff
        
        type(coverage_line_t) :: empty_baseline
        
        ! Create empty baseline line
        call empty_baseline%init(current_line%filename, current_line%line_number, &
                                0, .false.)
        
        ! Direct assignment - map to correct fields
        line_diff%line_number = current_line%line_number
        line_diff%filename = current_line%filename
        line_diff%old_count = 0
        line_diff%new_count = current_line%execution_count
        line_diff%status = '+'
    end function compute_line_diff_new
    
    ! Compute line diff for removed line
    function compute_line_diff_removed(baseline_line) result(line_diff)
        type(coverage_line_t), intent(in) :: baseline_line
        type(line_diff_t) :: line_diff
        
        type(coverage_line_t) :: empty_current
        
        ! Create empty current line
        call empty_current%init(baseline_line%filename, baseline_line%line_number, &
                               0, .false.)
        
        ! Direct assignment - map to correct fields
        line_diff%line_number = baseline_line%line_number
        line_diff%filename = baseline_line%filename
        line_diff%old_count = baseline_line%execution_count
        line_diff%new_count = 0
        line_diff%status = '-'
    end function compute_line_diff_removed
    
    ! Compute file diff for new file (added)
    function compute_file_diff_new(current_file) result(file_diff)
        type(coverage_file_t), intent(in) :: current_file
        type(file_diff_t) :: file_diff
        
        type(line_diff_t), allocatable :: line_diffs(:)
        integer :: i
        
        allocate(line_diffs(size(current_file%lines)))
        
        do i = 1, size(current_file%lines)
            line_diffs(i) = compute_line_diff_new(current_file%lines(i))
        end do
        
        ! Direct assignment
        file_diff%filename = current_file%filename
        file_diff%line_diffs = line_diffs
    end function compute_file_diff_new
    
    ! Compute file diff for removed file
    function compute_file_diff_removed(baseline_file) result(file_diff)
        type(coverage_file_t), intent(in) :: baseline_file
        type(file_diff_t) :: file_diff
        
        type(line_diff_t), allocatable :: line_diffs(:)
        integer :: i
        
        allocate(line_diffs(size(baseline_file%lines)))
        
        do i = 1, size(baseline_file%lines)
            line_diffs(i) = compute_line_diff_removed(baseline_file%lines(i))
        end do
        
        ! Direct assignment
        file_diff%filename = baseline_file%filename
        file_diff%line_diffs = line_diffs
    end function compute_file_diff_removed

    ! Enhanced threshold-based diff computation with sophisticated analysis
    ! TODO: Implement threshold type and functions - commented out for compilation
    ! function compute_coverage_diff_with_thresholds(baseline, current, thresholds, &
    !                                               include_unchanged) result(diff)
    !    type(coverage_data_t), intent(in) :: baseline, current
    !    type(diff_thresholds_t), intent(in) :: thresholds
    !    logical, intent(in), optional :: include_unchanged
    !    type(coverage_diff_t) :: diff
!        
!        type(file_diff_t), allocatable :: file_diffs_temp(:)
!        integer :: baseline_file_count, current_file_count, max_files
!        integer :: i, j, file_diff_count
!        logical :: found, include_unchanged_flag
!        
!        ! Set default parameter values
!        include_unchanged_flag = .false.
!        if (present(include_unchanged)) include_unchanged_flag = include_unchanged
!        
!        ! Get file counts
!        baseline_file_count = size(baseline%files)
!        current_file_count = size(current%files)
!        max_files = baseline_file_count + current_file_count
!        
!        ! Allocate temporary array for file diffs
!        allocate(file_diffs_temp(max_files))
!        file_diff_count = 0
!        
!        ! Process files present in current coverage
!        do i = 1, current_file_count
!            found = .false.
!            do j = 1, baseline_file_count
!                if (current%files(i)%filename == baseline%files(j)%filename) then
!                    file_diff_count = file_diff_count + 1
!                    file_diffs_temp(file_diff_count) = compute_enhanced_file_diff( &
!                        baseline%files(j), current%files(i), thresholds)
!                    found = .true.
!                    exit
!                end if
!            end do
!            
!            ! File exists in current but not in baseline (new file)
!            if (.not. found) then
!                file_diff_count = file_diff_count + 1
!                file_diffs_temp(file_diff_count) = compute_enhanced_file_diff_new( &
!                    current%files(i), thresholds)
!            end if
!        end do
!        
!        ! Process files present in baseline but not in current (removed files)
!        do i = 1, baseline_file_count
!            found = .false.
!            do j = 1, current_file_count
!                if (baseline%files(i)%filename == current%files(j)%filename) then
!                    found = .true.
!                    exit
!                end if
!            end do
!            
!            ! File exists in baseline but not in current (removed file)
!            if (.not. found) then
!                file_diff_count = file_diff_count + 1
!                file_diffs_temp(file_diff_count) = compute_enhanced_file_diff_removed( &
!                    baseline%files(i), thresholds)
!            end if
!        end do
!        
!        ! Create final coverage diff with enhanced analysis
!        ! Direct assignment
!        diff%file_diffs = file_diffs_temp(1:file_diff_count)
!        
!        ! Apply sophisticated threshold filtering based on statistical significance
!        if (.not. include_unchanged_flag) then
!            call filter_by_enhanced_thresholds(diff, thresholds)
!        end if
!    end function compute_coverage_diff_with_thresholds
!
!    ! Enhanced file diff with threshold-based analysis
!    function compute_enhanced_file_diff(baseline_file, current_file, thresholds) &
!             & result(file_diff)
!        type(coverage_file_t), intent(in) :: baseline_file, current_file
!        type(diff_thresholds_t), intent(in) :: thresholds
!        type(file_diff_t) :: file_diff
!        
!        ! First compute basic file diff
!        file_diff = compute_file_diff(baseline_file, current_file)
!        
!        ! Apply enhanced threshold-based analysis
!        call file_diff%apply_threshold_analysis(thresholds)
!    end function compute_enhanced_file_diff
!
!    ! Enhanced file diff for new file with threshold analysis
!    function compute_enhanced_file_diff_new(current_file, thresholds) result(file_diff)
!        type(coverage_file_t), intent(in) :: current_file
!        type(diff_thresholds_t), intent(in) :: thresholds
!        type(file_diff_t) :: file_diff
!        
!        ! First compute basic new file diff
!        file_diff = compute_file_diff_new(current_file)
!        
!        ! Apply enhanced threshold-based analysis
!        call file_diff%apply_threshold_analysis(thresholds)
!    end function compute_enhanced_file_diff_new
!
!    ! Enhanced file diff for removed file with threshold analysis
!    function compute_enhanced_file_diff_removed(baseline_file, thresholds) &
!             & result(file_diff)
!        type(coverage_file_t), intent(in) :: baseline_file
!        type(diff_thresholds_t), intent(in) :: thresholds
!        type(file_diff_t) :: file_diff
!        
!        ! First compute basic removed file diff
!        file_diff = compute_file_diff_removed(baseline_file)
!        
!        ! Apply enhanced threshold-based analysis
!        call file_diff%apply_threshold_analysis(thresholds)
!    end function compute_enhanced_file_diff_removed
!
!    ! Enhanced threshold filtering with statistical significance
!    subroutine filter_by_enhanced_thresholds(diff, thresholds)
!        type(coverage_diff_t), intent(inout) :: diff
!        type(diff_thresholds_t), intent(in) :: thresholds
!        
!        type(file_diff_t), allocatable :: filtered_diffs(:)
!        integer :: i, filtered_count
!        
!        ! Count files that meet enhanced significance criteria
!        filtered_count = 0
!        do i = 1, size(diff%file_diffs)
!            if (is_statistically_significant(diff%file_diffs(i), thresholds)) then
!                filtered_count = filtered_count + 1
!            end if
!        end do
!        
!        ! Create filtered array with significant changes only
!        allocate(filtered_diffs(filtered_count))
!        filtered_count = 0
!        do i = 1, size(diff%file_diffs)
!            if (is_statistically_significant(diff%file_diffs(i), thresholds)) then
!                filtered_count = filtered_count + 1
!                filtered_diffs(filtered_count) = diff%file_diffs(i)
!            end if
!        end do
!        
!        ! Replace original array
!        deallocate(diff%file_diffs)
!        allocate(diff%file_diffs, source=filtered_diffs)
!        deallocate(filtered_diffs)
!        
!        ! Recalculate totals
!        call diff%calculate_totals()
!    end subroutine filter_by_enhanced_thresholds
!
!    ! Check if file diff is statistically significant
!    function is_statistically_significant(file_diff, thresholds) result(significant)
!        type(file_diff_t), intent(in) :: file_diff
!        type(diff_thresholds_t), intent(in) :: thresholds
!        logical :: significant
!        
!        ! Consider multiple factors for statistical significance:
!        ! 1. Absolute change magnitude
!        ! 2. Statistical confidence
!        ! 3. Classification level
!        
!        significant = .false.
!        
!        ! Always include critical changes regardless of confidence
!        if (abs(file_diff%overall_significance_classification) == 3) then
!            significant = .true.
!            return
!        end if
!        
!        ! Include major changes with reasonable confidence
!        if (abs(file_diff%overall_significance_classification) >= 2 .and. &
!            file_diff%statistical_confidence >= 0.5) then
!            significant = .true.
!            return
!        end if
!        
!        ! Include minor changes with high confidence
!        if (abs(file_diff%overall_significance_classification) >= 1 .and. &
!            file_diff%statistical_confidence >= 0.8) then
!            significant = .true.
!            return
!        end if
!        
!        ! Include new/lost coverage
!        if (abs(file_diff%overall_significance_classification) == 10) then
!            significant = .true.
!            return
!        end if
!        
!        ! Fallback to basic threshold check
!        if (abs(file_diff%coverage_percentage_delta) >= thresholds%significance_threshold) then
!            significant = .true.
!        end if
!    end function is_statistically_significant
!
end module coverage_diff
