module coverage_data_comparator
    !! Coverage Data Comparison Module
    !!
    !! Provides comprehensive utilities for comparing and validating
    !! coverage data structures. Supports exact comparison, structural
    !! validation, and numerical tolerance checking for robust testing.
    !!
    !! Key Features:
    !! - Complete coverage data structure comparison
    !! - Structural equivalence validation (ignoring execution counts)
    !! - Numerical tolerance checking for floating-point values
    !! - Memory-safe comparison with proper bounds checking

    use coverage_model_core
    implicit none
    private

    public :: compare_coverage_data
    public :: validate_structural_equivalence
    public :: check_numerical_tolerance

contains

    ! Compare two coverage data structures for equality
    subroutine compare_coverage_data(data1, data2, data_matches)
        type(coverage_data_t), intent(in) :: data1, data2
        logical, intent(out) :: data_matches

        integer :: i, j

        data_matches = .false.

        ! Memory safety: Check if both files arrays are allocated
        if (.not. allocated(data1%files) .or. .not. allocated(data2%files)) return

        ! Check file count
        if (size(data1%files) /= size(data2%files)) return

        ! Check each file
        do i = 1, size(data1%files)
            if (data1%files(i)%filename /= data2%files(i)%filename) return

            ! Memory safety: Check if both lines arrays are allocated
            if (.not. allocated(data1%files(i)%lines) .or. &
                .not. allocated(data2%files(i)%lines)) return

            ! Check line count
            if (size(data1%files(i)%lines) /= size(data2%files(i)%lines)) return

            ! Check each line
            do j = 1, size(data1%files(i)%lines)
                if (data1%files(i)%lines(j)%line_number /= &
                    data2%files(i)%lines(j)%line_number) return
                if (data1%files(i)%lines(j)%execution_count /= &
                    data2%files(i)%lines(j)%execution_count) return
                if (data1%files(i)%lines(j)%is_executable .neqv. &
                    data2%files(i)%lines(j)%is_executable) return
            end do
        end do

        data_matches = .true.

    end subroutine compare_coverage_data

    ! Validate structural equivalence between two coverage datasets
    subroutine validate_structural_equivalence(data1, data2, structures_match)
        type(coverage_data_t), intent(in) :: data1, data2
        logical, intent(out) :: structures_match

        integer :: i, j

        structures_match = .false.

        ! Memory safety: Check if both files arrays are allocated
        if (.not. allocated(data1%files) .or. .not. allocated(data2%files)) return

        ! Check file count and names
        if (size(data1%files) /= size(data2%files)) return

        do i = 1, size(data1%files)
            if (data1%files(i)%filename /= data2%files(i)%filename) return

            ! Memory safety: Check if both lines arrays are allocated
            if (.not. allocated(data1%files(i)%lines) .or. &
                .not. allocated(data2%files(i)%lines)) return

            ! Check line count and numbers (ignore execution counts)
            if (size(data1%files(i)%lines) /= size(data2%files(i)%lines)) return

            do j = 1, size(data1%files(i)%lines)
                if (data1%files(i)%lines(j)%line_number /= &
                    data2%files(i)%lines(j)%line_number) return
            end do
        end do

        structures_match = .true.

    end subroutine validate_structural_equivalence

    ! Check if two numerical values are within tolerance
    subroutine check_numerical_tolerance(value1, value2, tolerance, within_tolerance)
        real, intent(in) :: value1, value2, tolerance
        logical, intent(out) :: within_tolerance

        real :: difference

        difference = abs(value1 - value2)
        within_tolerance = (difference <= tolerance)

    end subroutine check_numerical_tolerance

end module coverage_data_comparator