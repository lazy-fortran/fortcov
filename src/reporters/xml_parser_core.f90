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
        use xml_attribute_parser, only: extract_filename_from_class, parse_lines_from_class
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

end module xml_parser_core