module xml_parser_core
    !! XML Parsing and Document Processing
    !! 
    !! Handles parsing of XML documents into coverage data structures.
    !! Extracted from xml_utils.f90 for SRP compliance (Issue #718).
    use coverage_model_core
    implicit none
    private
    
    public :: parse_cobertura_xml, count_xml_elements, parse_classes_from_xml
    public :: is_well_formed_xml

contains

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
            
            ! Extract filename - inline implementation to avoid circular dependency
            call extract_filename_inline(xml_content(class_start:), filename, &
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
            
            ! Parse lines within this class - inline implementation to avoid circular dependency
            call parse_lines_inline(xml_content(class_start:class_end), lines, &
                                         success)
            if (.not. success) return
            
            ! Create file object
            call files(i)%init(filename)
            if (allocated(lines)) deallocate(lines)
            
            ! Move position for next class
            pos = class_end + 8  ! length of '</class>'
        end do
        
    end subroutine parse_classes_from_xml
    
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
    
    ! Inline implementations to avoid circular dependency with xml_attribute_parser
    subroutine extract_filename_inline(class_xml, filename, success)
        character(len=*), intent(in) :: class_xml
        character(len=:), allocatable, intent(out) :: filename
        logical, intent(out) :: success
        
        integer :: start_pos, end_pos
        
        success = .false.
        
        ! Find filename attribute
        start_pos = index(class_xml, 'filename="')
        if (start_pos == 0) return
        
        start_pos = start_pos + 10  ! Length of 'filename="'
        
        ! Find end quote
        end_pos = index(class_xml(start_pos:), '"')
        if (end_pos == 0) return
        
        end_pos = start_pos + end_pos - 2
        
        ! Bounds check
        if (end_pos < start_pos .or. end_pos > len(class_xml)) return
        
        filename = class_xml(start_pos:end_pos)
        success = .true.
        
    end subroutine extract_filename_inline
    
    subroutine parse_lines_inline(class_xml, lines, success)
        character(len=*), intent(in) :: class_xml
        type(coverage_line_t), allocatable, intent(out) :: lines(:)
        logical, intent(out) :: success
        
        integer :: line_count, i, pos, line_start
        integer :: line_number, hits
        logical :: line_success
        
        success = .false.
        
        ! Count lines
        line_count = count_xml_elements(class_xml, '<line number="')
        if (line_count == 0) then
            allocate(lines(0))
            success = .true.
            return
        end if
        
        allocate(lines(line_count))
        pos = 1
        
        ! Parse each line element
        do i = 1, line_count
            ! Find line element
            line_start = index(class_xml(pos:), '<line number="')
            if (line_start == 0) return
            
            line_start = line_start + pos - 1
            
            ! Extract line attributes
            call extract_line_attributes_inline(class_xml(line_start:), &
                                        line_number, hits, line_success)
            if (.not. line_success) return
            
            ! Initialize line with filename placeholder
            call lines(i)%init("parsed.f90", line_number, hits, .true.)
            
            pos = line_start + 14  ! Move past '<line number="'
        end do
        
        success = .true.
        
    end subroutine parse_lines_inline
    
    subroutine extract_line_attributes_inline(line_xml, line_number, hits, success)
        character(len=*), intent(in) :: line_xml
        integer, intent(out) :: line_number, hits
        logical, intent(out) :: success
        
        integer :: num_start, num_end, hits_start, hits_end
        character(len=:), allocatable :: temp_string
        integer :: iostat
        
        success = .false.
        
        ! Extract line number
        num_start = index(line_xml, 'number="')
        if (num_start == 0) return
        num_start = num_start + 8
        
        num_end = index(line_xml(num_start:), '"')
        if (num_end == 0) return
        num_end = num_start + num_end - 2
        
        temp_string = line_xml(num_start:num_end)
        read(temp_string, *, iostat=iostat) line_number
        if (iostat /= 0) return
        
        ! Extract hits
        hits_start = index(line_xml, 'hits="')
        if (hits_start == 0) return
        hits_start = hits_start + 6
        
        hits_end = index(line_xml(hits_start:), '"')
        if (hits_end == 0) return
        hits_end = hits_start + hits_end - 2
        
        temp_string = line_xml(hits_start:hits_end)
        read(temp_string, *, iostat=iostat) hits
        if (iostat /= 0) return
        
        success = .true.
        
    end subroutine extract_line_attributes_inline

end module xml_parser_core