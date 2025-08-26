module coverage_metrics_calculator
    !! Coverage Metrics Calculation
    !!
    !! Handles computation and management of coverage statistics.
    !! Extracted from report_engine_impl.f90 for SRP compliance.
    use coverage_model
    use string_utils, only: real_to_str
    implicit none
    private

    ! Public types
    public :: coverage_metrics_t
    public :: calculate_metrics_for_data
    public :: generate_diff_output

    type :: coverage_metrics_t
        integer :: total_lines = 0
        integer :: covered_lines = 0
        integer :: total_files = 0
        real :: line_coverage_percentage = 0.0
        real :: branch_coverage_percentage = 0.0
        real :: function_coverage_percentage = 0.0
    contains
        procedure :: init => coverage_metrics_init
    end type coverage_metrics_t

contains

    ! Initialize coverage metrics
    subroutine coverage_metrics_init(this)
        class(coverage_metrics_t), intent(out) :: this

        this%total_lines = 0
        this%covered_lines = 0
        this%total_files = 0
        this%line_coverage_percentage = 0.0
        this%branch_coverage_percentage = 0.0
        this%function_coverage_percentage = 0.0
    end subroutine coverage_metrics_init

    ! Calculate metrics from coverage data
    subroutine calculate_metrics_for_data(data, metrics)
        type(coverage_data_t), intent(in) :: data
        type(coverage_metrics_t), intent(out) :: metrics

        integer :: i, j, total_executable, total_covered

        call metrics%init()

        if (.not. allocated(data%files)) return

        metrics%total_files = size(data%files)
        total_executable = 0
        total_covered = 0

        do i = 1, size(data%files)
            if (allocated(data%files(i)%lines)) then
                do j = 1, size(data%files(i)%lines)
                    if (data%files(i)%lines(j)%is_executable) then
                        total_executable = total_executable + 1
                        if (data%files(i)%lines(j)%execution_count > 0) then
                            total_covered = total_covered + 1
                        end if
                    end if
                end do
            end if
        end do

        metrics%total_lines = total_executable
        metrics%covered_lines = total_covered

        if (total_executable > 0) then
            metrics%line_coverage_percentage = &
                real(total_covered) / real(total_executable) * 100.0
        end if
    end subroutine calculate_metrics_for_data

    ! Generate diff output between two metrics
    subroutine generate_diff_output(baseline, current, output)
        type(coverage_metrics_t), intent(in) :: baseline, current
        character(len=:), allocatable, intent(out) :: output

        real :: coverage_delta
        character(len=:), allocatable :: status

        coverage_delta = current%line_coverage_percentage - &
                        baseline%line_coverage_percentage

        if (coverage_delta > 0.1) then
            status = "improved"
        else if (coverage_delta < -0.1) then
            status = "degraded"
        else
            status = "unchanged"
        end if

        output = 'Coverage Diff Report' // new_line('a') // &
                '===================' // new_line('a') // &
                'Baseline: ' // real_to_str(baseline%line_coverage_percentage) // &
                '%' // new_line('a') // &
                'Current:  ' // real_to_str(current%line_coverage_percentage) // &
                '%' // new_line('a') // &
                'Delta:    ' // real_to_str(coverage_delta) // '%' // new_line('a') // &
                'Status:   ' // status // new_line('a')
    end subroutine generate_diff_output

end module coverage_metrics_calculator