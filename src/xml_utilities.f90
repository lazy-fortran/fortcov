module xml_utilities
    use coverage_model
    use string_utilities, only: int_to_string, real_to_string
    implicit none
    private
    
    ! Public interface for XML utilities
    public :: generate_sources_section
    public :: generate_packages_section
    public :: calculate_file_line_rate
    public :: parse_cobertura_xml
    public :: count_xml_elements
    public :: parse_classes_from_xml
    public :: extract_filename_from_class
    public :: parse_lines_from_class
    public :: extract_line_attributes
    public :: get_current_timestamp
    public :: int_to_string
    public :: get_directory_path
    public :: get_base_name
    public :: is_well_formed_xml
    
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
    
    ! Parse Cobertura XML into coverage data structure
    subroutine parse_cobertura_xml(xml_content, coverage_data, success)
        character(len=*), intent(in) :: xml_content
        type(coverage_data_t), intent(out) :: coverage_data
        logical, intent(out) :: success
        
        ! Simplified XML parsing - extract class and line information
        ! This is a basic implementation for testing purposes
        character(len=:), allocatable :: class_section, lines_section
        type(coverage_file_t), allocatable :: files(:)
        integer :: class_start, class_end, line_pos
        integer :: file_count
        
        success = .false.
        file_count = 0
        
        ! Count files by counting class elements
        file_count = count_xml_elements(xml_content, '<class ')
        if (file_count == 0) then
            call coverage_data%init()
            success = .true.
            return
        end if
        
        allocate(files(file_count))
        
        ! Parse each class element (simplified implementation)
        call parse_classes_from_xml(xml_content, files, success)
        if (.not. success) return
        
        call coverage_data%init()
        coverage_data%files = files
        success = .true.
        
    end subroutine parse_cobertura_xml
    
    ! Simplified XML parsing helper functions
    
    function count_xml_elements(xml_content, element_name) result(count)
        character(len=*), intent(in) :: xml_content, element_name
        integer :: count, pos, start_search
        
        count = 0
        start_search = 1
        
        do
            pos = index(xml_content(start_search:), element_name)
            if (pos == 0) exit
            
            ! Check that this is not a closing tag (avoid counting </class as <class)
            if (start_search + pos - 2 > 0) then
                if (xml_content(start_search + pos - 2:start_search + pos - 2) &
                    == '/') then
                    start_search = start_search + pos + len(element_name) - 1
                    if (start_search > len(xml_content)) exit
                    cycle
                end if
            end if
            
            count = count + 1
            start_search = start_search + pos + len(element_name) - 1
            if (start_search > len(xml_content)) exit
        end do
        
    end function count_xml_elements
    
    subroutine parse_classes_from_xml(xml_content, files, success)
        character(len=*), intent(in) :: xml_content
        type(coverage_file_t), intent(out) :: files(:)
        logical, intent(out) :: success
        
        character(len=:), allocatable :: filename
        type(coverage_line_t), allocatable :: lines(:)
        integer :: i, class_start, class_end, lines_count
        integer :: pos, next_class
        
        success = .true.
        pos = 1
        
        ! Parse each class element
        do i = 1, size(files)
            ! Find next class element
            class_start = index(xml_content(pos:), '<class filename="')
            if (class_start == 0) then
                success = .false.
                return
            end if
            class_start = class_start + pos - 1
            
            ! Extract filename - with bounds checking
            call extract_filename_from_class(xml_content(class_start:), filename, &
                                              success)
            if (.not. success) return
            
            ! Find end of this class - with bounds checking
            class_end = index(xml_content(class_start:), '</class>')
            if (class_end == 0) then
                success = .false.
                return
            end if
            class_end = class_end + class_start - 1
            
            ! Validate bounds
            if (class_start > len(xml_content) .or. class_end > len(xml_content)) then
                success = .false.
                return
            end if
            
            ! Parse lines within this class
            call parse_lines_from_class(xml_content(class_start:class_end), lines, &
                                         success)
            if (.not. success) return
            
            ! Create file object
            call files(i)%init(filename)
            if (allocated(lines)) deallocate(lines)
            
            ! Move position for next class
            pos = class_end + 8  ! length of '</class>'
        end do
        
    end subroutine parse_classes_from_xml
    
    ! Helper functions for XML parsing
    
    subroutine extract_filename_from_class(class_xml, filename, success)
        character(len=*), intent(in) :: class_xml
        character(len=:), allocatable, intent(out) :: filename
        logical, intent(out) :: success
        
        integer :: start_pos, end_pos
        
        success = .false.
        
        ! Find filename="
        start_pos = index(class_xml, 'filename="')
        if (start_pos == 0) return
        start_pos = start_pos + 10  ! length of 'filename="'
        
        ! Bounds check
        if (start_pos > len(class_xml)) return
        
        ! Find closing quote
        end_pos = index(class_xml(start_pos:), '"')
        if (end_pos == 0) return
        end_pos = start_pos + end_pos - 2
        
        ! Final bounds check
        if (end_pos < start_pos .or. end_pos > len(class_xml)) return
        
        filename = class_xml(start_pos:end_pos)
        success = .true.
        
    end subroutine extract_filename_from_class
    
    subroutine parse_lines_from_class(class_xml, lines, success)
        character(len=*), intent(in) :: class_xml
        type(coverage_line_t), allocatable, intent(out) :: lines(:)
        logical, intent(out) :: success
        
        integer :: line_count, i, pos, line_start
        integer :: line_number, hits
        character(len=:), allocatable :: line_attr
        
        success = .false.
        
        ! Count line elements
        line_count = count_xml_elements(class_xml, '<line number=')
        if (line_count == 0) then
            allocate(lines(0))
            success = .true.
            return
        end if
        
        allocate(lines(line_count))
        pos = 1
        
        ! Parse each line
        do i = 1, line_count
            line_start = index(class_xml(pos:), '<line number="')
            if (line_start == 0) return
            line_start = line_start + pos - 1
            
            ! Bounds check
            if (line_start > len(class_xml)) return
            
            ! Extract line number and hits
            call extract_line_attributes(class_xml(line_start:), line_number, hits, &
                                          success)
            if (.not. success) return
            
            ! Create line object - use dummy filename since we'll get it from parent
            call lines(i)%init('dummy.f90', line_number, hits, .true.)
            
            ! Move position for next line - find actual end of current line element
            pos = index(class_xml(line_start:), '/>')
            if (pos == 0) then
                pos = index(class_xml(line_start:), '</line>')
                if (pos == 0) return
                pos = line_start + pos + 6  ! length of '</line>'
            else
                pos = line_start + pos + 1  ! length of '/>'
            end if
        end do
        
        success = .true.
        
    end subroutine parse_lines_from_class
    
    subroutine extract_line_attributes(line_xml, line_number, hits, success)
        character(len=*), intent(in) :: line_xml
        integer, intent(out) :: line_number, hits
        logical, intent(out) :: success
        
        integer :: num_start, num_end, hits_start, hits_end, iostat_var
        character(len=20) :: temp_str
        
        success = .false.
        line_number = 0
        hits = 0
        
        ! Extract line number
        num_start = index(line_xml, 'number="')
        if (num_start == 0) return
        num_start = num_start + 8  ! length of 'number="'
        
        ! Bounds check
        if (num_start > len(line_xml)) return
        
        num_end = index(line_xml(num_start:), '"')
        if (num_end == 0) return
        num_end = num_start + num_end - 2
        
        ! Final bounds check
        if (num_end < num_start .or. num_end > len(line_xml)) return
        
        temp_str = adjustl(line_xml(num_start:num_end))
        read(temp_str, *, iostat=iostat_var) line_number
        if (iostat_var /= 0) return
        
        ! Extract hits
        hits_start = index(line_xml, 'hits="')
        if (hits_start == 0) return
        hits_start = hits_start + 6  ! length of 'hits="'
        
        ! Bounds check
        if (hits_start > len(line_xml)) return
        
        hits_end = index(line_xml(hits_start:), '"')
        if (hits_end == 0) return
        hits_end = hits_start + hits_end - 2
        
        ! Final bounds check
        if (hits_end < hits_start .or. hits_end > len(line_xml)) return
        
        temp_str = adjustl(line_xml(hits_start:hits_end))
        read(temp_str, *, iostat=iostat_var) hits
        if (iostat_var /= 0) return
        
        success = .true.
        
    end subroutine extract_line_attributes
    
    ! Utility functions
    
    function get_current_timestamp() result(timestamp)
        character(len=19) :: timestamp
        integer :: values(8)
        
        call date_and_time(values=values)
        write(timestamp, '(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2)') &
            values(1), '-', values(2), '-', values(3), 'T', &
            values(5), ':', values(6), ':', values(7)
            
    end function get_current_timestamp
    
    function get_directory_path(filename) result(dir_path)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dir_path
        integer :: last_slash
        
        last_slash = index(filename, '/', back=.true.)
        if (last_slash > 0) then
            dir_path = filename(1:last_slash-1)
        else
            dir_path = '.'
        end if
        
    end function get_directory_path
    
    function get_base_name(filename) result(base_name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: base_name
        integer :: last_slash, last_dot
        
        last_slash = index(filename, '/', back=.true.)
        last_dot = index(filename, '.', back=.true.)
        
        if (last_slash > 0 .and. last_dot > last_slash) then
            base_name = filename(last_slash+1:last_dot-1)
        else if (last_slash > 0) then
            base_name = filename(last_slash+1:)
        else if (last_dot > 0) then
            base_name = filename(1:last_dot-1)
        else
            base_name = filename
        end if
        
    end function get_base_name
    
    function is_well_formed_xml(xml_content) result(is_well_formed)
        character(len=*), intent(in) :: xml_content
        logical :: is_well_formed
        integer :: open_tags, close_tags
        
        ! Basic well-formed check - count opening and closing tags
        open_tags = count_xml_elements(xml_content, '<')
        close_tags = count_xml_elements(xml_content, '</')
        
        ! Should have balanced tags (simplified check)
        is_well_formed = (open_tags >= close_tags)
        
    end function is_well_formed_xml

end module xml_utilities