module coverage_diff
    use coverage_model
    implicit none
    
    private
    public :: compute_coverage_diff
    public :: compute_file_diff
    public :: compute_line_diff
    
contains

    ! Compute coverage difference between baseline and current coverage data
    function compute_coverage_diff(baseline, current, include_unchanged, threshold) result(diff)
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
        call diff%init(file_diffs_temp(1:file_diff_count), &
                      include_unchanged_flag, threshold_val)
        
        ! Apply threshold filtering if needed
        if (threshold_val > 0.0) then
            call diff%filter_by_threshold()
        end if
        
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
        
        ! Create file diff
        call file_diff%init(current_file%filename, &
                           line_diffs_temp(1:line_diff_count))
        
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
        
        call line_diff%init(baseline_line, current_line, diff_type)
    end function compute_line_diff
    
    ! Compute line diff for new line (added)
    function compute_line_diff_new(current_line) result(line_diff)
        type(coverage_line_t), intent(in) :: current_line
        type(line_diff_t) :: line_diff
        
        type(coverage_line_t) :: empty_baseline
        
        ! Create empty baseline line
        call empty_baseline%init(0, current_line%line_number, &
                                current_line%filename, .false.)
        
        call line_diff%init(empty_baseline, current_line, DIFF_ADDED)
    end function compute_line_diff_new
    
    ! Compute line diff for removed line
    function compute_line_diff_removed(baseline_line) result(line_diff)
        type(coverage_line_t), intent(in) :: baseline_line
        type(line_diff_t) :: line_diff
        
        type(coverage_line_t) :: empty_current
        
        ! Create empty current line
        call empty_current%init(0, baseline_line%line_number, &
                               baseline_line%filename, .false.)
        
        call line_diff%init(baseline_line, empty_current, DIFF_REMOVED)
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
        
        call file_diff%init(current_file%filename, line_diffs)
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
        
        call file_diff%init(baseline_file%filename, line_diffs)
    end function compute_file_diff_removed

end module coverage_diff