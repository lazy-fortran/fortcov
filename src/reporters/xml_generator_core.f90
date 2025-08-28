module xml_generator_core
    !! XML Generation Functions
    !! 
    !! Handles generation of XML sections for coverage reports.
    !! Extracted from xml_utils.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    use string_utils, only: int_to_string, real_to_string
    use xml_utility_helpers, only: get_directory_path, get_base_name
    implicit none
    private
    
    public :: generate_sources_section, generate_packages_section
    public :: calculate_file_line_rate

contains

    ! Generate sources section of XML
    function generate_sources_section(coverage_data) result(sources_xml)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: sources_xml
        integer :: i
        character(len=:), allocatable :: source_path
        
        sources_xml = '<sources>' // new_line('')
        
        ! Memory safety: Check if files array is allocated
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                ! Extract directory from filename for source path
                source_path = get_directory_path(coverage_data%files(i)%filename)
                sources_xml = sources_xml // '  <source>' // &
                             trim(source_path) // '</source>' // new_line('')
            end do
        end if
        
        sources_xml = sources_xml // '</sources>'
        
    end function generate_sources_section
    
    ! Generate packages section of XML
    function generate_packages_section(coverage_data) result(packages_xml)
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=:), allocatable :: packages_xml
        integer :: i, j
        real :: file_line_rate
        
        packages_xml = '<packages>' // new_line('') // &
                      '  <package name="fortcov-coverage">' // new_line('') // &
                      '    <classes>' // new_line('')
        
        ! Memory safety: Check if files array is allocated
        if (allocated(coverage_data%files)) then
            do i = 1, size(coverage_data%files)
                ! Calculate line rate for this file
                call calculate_file_line_rate(coverage_data%files(i), file_line_rate)
                
                packages_xml = packages_xml // &
                              '      <class filename="' // &
                              trim(coverage_data%files(i)%filename) // &
                              '" name="' // &
                              get_base_name(coverage_data%files(i)%filename) // &
                              '" line-rate="' // real_to_string(file_line_rate) // &
                              '" branch-rate="' // real_to_string(file_line_rate) // &
                              '" complexity="0.0">' // new_line('') // &
                              '        <lines>' // new_line('')
                
                ! Memory safety: Check if lines array is allocated for this file
                if (allocated(coverage_data%files(i)%lines)) then
                    ! Add lines for this file
                    do j = 1, size(coverage_data%files(i)%lines)
                        if (coverage_data%files(i)%lines(j)%is_executable) then
                            packages_xml = packages_xml // &
                                          '          <line number="' // &
                                          int_to_string( &
                                            coverage_data%files(i)%lines(j)%line_number) &
                                          // &
                                          '" hits="' // &
                                          int_to_string( &
                                            coverage_data%files(i)%lines(j)%execution_count) &
                                          // &
                                          '" branch="false"/>' // new_line('')
                        end if
                    end do
                end if
                
                packages_xml = packages_xml // &
                              '        </lines>' // new_line('') // &
                              '      </class>' // new_line('')
            end do
        end if
        
        packages_xml = packages_xml // &
                      '    </classes>' // new_line('') // &
                      '  </package>' // new_line('') // &
                      '</packages>'
        
    end function generate_packages_section
    
    ! Calculate line rate for a single file
    subroutine calculate_file_line_rate(file_data, line_rate)
        type(coverage_file_t), intent(in) :: file_data
        real, intent(out) :: line_rate
        
        integer :: total_lines, covered_lines, i
        
        total_lines = 0
        covered_lines = 0
        
        ! Memory safety: Check if lines array is allocated
        if (allocated(file_data%lines)) then
            do i = 1, size(file_data%lines)
                if (file_data%lines(i)%is_executable) then
                    total_lines = total_lines + 1
                    if (file_data%lines(i)%execution_count > 0) then
                        covered_lines = covered_lines + 1
                    end if
                end if
            end do
        end if
        
        if (total_lines > 0) then
            line_rate = real(covered_lines) / real(total_lines)
        else
            line_rate = 0.0
        end if
        
    end subroutine calculate_file_line_rate
    
end module xml_generator_core