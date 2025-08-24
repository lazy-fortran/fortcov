module coverage_data_model
    !! Coverage Data Model - Coordinated Public Interface
    !! 
    !! Provides backward-compatible public interface while decomposing
    !! implementation across multiple modules to maintain QADS size limits.
    !! Acts as coordination layer and ensures existing imports continue to work.
    
    ! Re-export all public types from coverage_types
    use coverage_types, only: &
        source_location_t, &
        coverage_line_t, &
        coverage_branch_t, &
        coverage_function_t, &
        coverage_file_t, &
        coverage_data_t, &
        coverage_diff_t, &
        line_diff_t, &
        file_diff_t, &
        line_coverage_t, &
        file_coverage_t
    
    ! Re-export initialization procedures from coverage_constructors  
    use coverage_constructors, only: &
        initialize_source_location, &
        initialize_coverage_line, &
        initialize_coverage_branch, &
        initialize_coverage_function, &
        initialize_coverage_file, &
        initialize_coverage_data, &
        file_calculate_coverage_impl, &
        data_calculate_overall_coverage_impl, &
        data_serialize_impl, &
        data_deserialize_impl, &
        file_get_line_coverage_percentage_impl, &
        file_get_executable_line_count_impl, &
        file_get_covered_line_count_impl, &
        file_get_branch_coverage_impl, &
        file_get_function_coverage_impl
    
    use foundation_constants
    implicit none
    private
    
    ! Public type re-exports for backward compatibility
    public :: source_location_t
    public :: coverage_line_t
    public :: coverage_branch_t
    public :: coverage_function_t
    public :: coverage_file_t
    public :: coverage_data_t
    public :: coverage_diff_t
    public :: line_diff_t
    public :: file_diff_t
    public :: line_coverage_t
    public :: file_coverage_t
    
    ! Public initialization procedures for backward compatibility
    public :: initialize_source_location
    public :: initialize_coverage_line
    public :: initialize_coverage_branch
    public :: initialize_coverage_function
    public :: initialize_coverage_file
    public :: initialize_coverage_data
    
    ! Public enhanced procedures that maintain original interface
    public :: file_init_simple, file_init_with_lines
    public :: data_init_simple, data_init_with_files
    
    ! Enhanced wrapper procedures that delegate to constructors module
    public :: enhanced_file_calculate_coverage
    public :: enhanced_data_calculate_overall_coverage
    public :: enhanced_data_serialize
    public :: enhanced_data_deserialize
    public :: enhanced_file_get_line_coverage_percentage
    public :: enhanced_file_get_executable_line_count
    public :: enhanced_file_get_covered_line_count
    public :: enhanced_file_get_branch_coverage
    public :: enhanced_file_get_function_coverage
    
contains

    ! Wrapper procedures that delegate to implementation modules
    ! These maintain the original interface while using decomposed implementation
    
    subroutine enhanced_file_calculate_coverage(file)
        type(coverage_file_t), intent(inout) :: file
        call file_calculate_coverage_impl(file)
    end subroutine enhanced_file_calculate_coverage
    
    subroutine enhanced_data_calculate_overall_coverage(data)
        type(coverage_data_t), intent(inout) :: data
        call data_calculate_overall_coverage_impl(data)
    end subroutine enhanced_data_calculate_overall_coverage
    
    function enhanced_data_serialize(data) result(serialized)
        type(coverage_data_t), intent(in) :: data
        character(len=:), allocatable :: serialized
        serialized = data_serialize_impl(data)
    end function enhanced_data_serialize
    
    subroutine enhanced_data_deserialize(data, serialized)
        type(coverage_data_t), intent(inout) :: data
        character(len=*), intent(in) :: serialized
        call data_deserialize_impl(data, serialized)
    end subroutine enhanced_data_deserialize
    
    function enhanced_file_get_line_coverage_percentage(file) result(percentage)
        type(coverage_file_t), intent(in) :: file
        real :: percentage
        percentage = file_get_line_coverage_percentage_impl(file)
    end function enhanced_file_get_line_coverage_percentage
    
    function enhanced_file_get_executable_line_count(file) result(count)
        type(coverage_file_t), intent(in) :: file
        integer :: count
        count = file_get_executable_line_count_impl(file)
    end function enhanced_file_get_executable_line_count
    
    function enhanced_file_get_covered_line_count(file) result(count)
        type(coverage_file_t), intent(in) :: file
        integer :: count
        count = file_get_covered_line_count_impl(file)
    end function enhanced_file_get_covered_line_count
    
    function enhanced_file_get_branch_coverage(file) result(coverage)
        type(coverage_file_t), intent(in) :: file
        real :: coverage
        coverage = file_get_branch_coverage_impl(file)
    end function enhanced_file_get_branch_coverage
    
    function enhanced_file_get_function_coverage(file) result(coverage)
        type(coverage_file_t), intent(in) :: file
        real :: coverage
        coverage = file_get_function_coverage_impl(file)
    end function enhanced_file_get_function_coverage
    
    ! Simple delegation procedures that maintain original method signatures
    subroutine file_init_simple(file, filename)
        type(coverage_file_t), intent(out) :: file
        character(len=*), intent(in) :: filename
        call file%init(filename)
    end subroutine file_init_simple
    
    subroutine file_init_with_lines(file, filename, lines)
        type(coverage_file_t), intent(out) :: file
        character(len=*), intent(in) :: filename
        type(coverage_line_t), intent(in) :: lines(:)
        call file%init(filename, lines)
    end subroutine file_init_with_lines
    
    subroutine data_init_simple(data)
        type(coverage_data_t), intent(out) :: data
        call data%init()
    end subroutine data_init_simple
    
    subroutine data_init_with_files(data, files)
        type(coverage_data_t), intent(out) :: data
        type(coverage_file_t), intent(in) :: files(:)
        call data%init(files)
    end subroutine data_init_with_files

end module coverage_data_model