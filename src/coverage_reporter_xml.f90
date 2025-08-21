module coverage_reporter_xml
    !! XML/Cobertura Coverage Reporter Implementation
    !! 
    !! Generates Cobertura-compatible XML format for CI/CD integration.
    !! Supports standard package/class structure mapping and line coverage.
    use coverage_model
    use coverage_statistics, only: stats_t => coverage_stats_t
    use coverage_reporter_base
    use coverage_reporter_utils
    implicit none
    private
    
    public :: xml_reporter_t
    
    ! XML/Cobertura reporter implementation  
    type, extends(coverage_reporter_t) :: xml_reporter_t
        character(len=:), allocatable :: last_error
    contains
        procedure :: generate_report => xml_generate_report
        procedure :: get_format_name => xml_get_format_name
        procedure :: supports_diff => xml_supports_diff
    end type xml_reporter_t

contains

    subroutine xml_generate_report(this, coverage_data, output_path, &
                                 & success, error_message, &
                                 & diff_data, threshold)
        use zero_configuration_manager, only: ensure_output_directory_structure
        use error_handling, only: error_context_t
        class(xml_reporter_t), intent(in) :: this
        type(coverage_data_t), intent(in) :: coverage_data
        character(len=*), intent(in) :: output_path
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_message
        type(coverage_diff_t), intent(in), optional :: diff_data
        real, intent(in), optional :: threshold
        
        integer :: unit, stat
        logical :: use_stdout
        type(stats_t) :: line_stats, branch_stats, func_stats
        type(error_context_t) :: error_ctx
        
        success = .true.
        use_stdout = (trim(output_path) == "-")
        
        ! Ensure output directory exists for file output (Issue #204 zero-configuration support)
        if (.not. use_stdout) then
            call ensure_output_directory_structure(output_path, error_ctx)
            if (error_ctx%error_code /= 0) then
                success = .false.
                error_message = trim(error_ctx%message)
                return
            end if
        end if
        
        ! Calculate coverage statistics
        call calculate_manual_line_stats(coverage_data, line_stats)
        call calculate_manual_branch_stats(coverage_data, branch_stats)
        call calculate_manual_function_stats(coverage_data, func_stats)
        
        ! Apply threshold if provided
        if (present(threshold)) then
            if (line_stats%percentage < threshold) then
                success = .false.
                error_message = "Coverage below threshold"
                return
            end if
        end if
        
        ! Open output stream
        if (use_stdout) then
            unit = 6  ! Standard output
        else
            open(newunit=unit, file=output_path, status='replace', &
                 form='formatted', access='sequential', iostat=stat)
            if (stat /= 0) then
                success = .false.
                error_message = "Cannot write to output file '" // &
                              trim(output_path) // "'"
                return
            end if
        end if
        
        ! Generate Cobertura XML
        call write_cobertura_xml(unit, coverage_data, line_stats, &
                                 branch_stats, func_stats)
        
        if (.not. use_stdout) close(unit)
        
        ! Suppress unused parameter warnings
        call suppress_unused_warning(this, diff_data)
        
    end subroutine xml_generate_report

    function xml_get_format_name(this) result(format_name)
        class(xml_reporter_t), intent(in) :: this
        character(len=:), allocatable :: format_name
        
        call suppress_unused_warning_simple(this)
        format_name = "xml"
        
    end function xml_get_format_name

    function xml_supports_diff(this) result(supported)
        class(xml_reporter_t), intent(in) :: this
        logical :: supported
        
        call suppress_unused_warning_simple(this)
        supported = .false.
        
    end function xml_supports_diff

    subroutine write_cobertura_xml(unit, coverage_data, line_stats, &
                                  branch_stats, func_stats)
        integer, intent(in) :: unit
        type(coverage_data_t), intent(in) :: coverage_data
        type(stats_t), intent(in) :: line_stats, branch_stats, func_stats
        
        character(len=32) :: timestamp
        real :: line_rate, branch_rate
        
        ! Calculate coverage rates (0.0-1.0 for Cobertura)
        line_rate = line_stats%percentage / 100.0
        branch_rate = branch_stats%percentage / 100.0
        
        ! Generate ISO timestamp
        call get_iso_timestamp(timestamp)
        
        ! Write XML header and root coverage element
        write(unit, '(A)') '<?xml version="1.0" encoding="UTF-8"?>'
        write(unit, '(A)') '<!DOCTYPE coverage SYSTEM "http://cobertura.sourceforge.net/xml/coverage-04.dtd">'
        
        write(unit, '(A,A,A)', advance='no') '<coverage timestamp="', trim(timestamp), '"'
        write(unit, '(A,F0.4,A)', advance='no') ' line-rate="', line_rate, '"'
        write(unit, '(A,F0.4,A)', advance='no') ' branch-rate="', branch_rate, '"'
        write(unit, '(A,I0,A)', advance='no') ' lines-covered="', line_stats%covered_count, '"'
        write(unit, '(A,I0,A)', advance='no') ' lines-valid="', line_stats%total_count, '"'
        write(unit, '(A,I0,A)', advance='no') ' branches-covered="', branch_stats%covered_count, '"'
        write(unit, '(A,I0,A)', advance='no') ' branches-valid="', branch_stats%total_count, '"'
        write(unit, '(A)') ' complexity="0" version="1.9">'
        
        ! Write sources
        write(unit, '(A)') '  <sources>'
        write(unit, '(A)') '    <source>.</source>'
        write(unit, '(A)') '  </sources>'
        
        ! Write packages (map to source files)
        write(unit, '(A)') '  <packages>'
        call write_packages_xml(unit, coverage_data)
        write(unit, '(A)') '  </packages>'
        
        write(unit, '(A)') '</coverage>'
        
    end subroutine write_cobertura_xml

    subroutine write_packages_xml(unit, coverage_data)
        integer, intent(in) :: unit
        type(coverage_data_t), intent(in) :: coverage_data
        integer :: i
        real :: file_line_rate, file_branch_rate
        integer :: file_lines_covered, file_lines_valid
        integer :: file_branches_covered, file_branches_valid
        character(len=:), allocatable :: package_name, class_name
        
        if (.not. allocated(coverage_data%files)) return
        
        do i = 1, size(coverage_data%files)
            ! Extract package name from file path (directory structure)
            call extract_package_name(coverage_data%files(i)%filename, package_name)
            call extract_class_name(coverage_data%files(i)%filename, class_name)
            
            ! Calculate file-level statistics
            call calculate_file_stats(coverage_data%files(i), &
                                     file_line_rate, file_branch_rate, &
                                     file_lines_covered, file_lines_valid, &
                                     file_branches_covered, file_branches_valid)
            
            ! Write package for this file
            write(unit, '(A)', advance='no') '    <package name="'
            write(unit, '(A)', advance='no') trim(package_name)
            write(unit, '(A,F0.4,A)', advance='no') '" line-rate="', file_line_rate, '"'
            write(unit, '(A,F0.4,A)', advance='no') ' branch-rate="', file_branch_rate, '"'
            write(unit, '(A)') '>'
            
            write(unit, '(A)') '      <classes>'
            
            ! Write class for this file
            write(unit, '(A)', advance='no') '        <class name="'
            write(unit, '(A)', advance='no') trim(class_name)
            write(unit, '(A)', advance='no') '" filename="'
            write(unit, '(A)', advance='no') trim(coverage_data%files(i)%filename)
            write(unit, '(A,F0.4,A)', advance='no') '" line-rate="', file_line_rate, '"'
            write(unit, '(A,F0.4,A)', advance='no') ' branch-rate="', file_branch_rate, '"'
            write(unit, '(A)') '>'
            
            ! Write methods (functions) if available
            write(unit, '(A)') '          <methods/>'
            
            ! Write lines
            write(unit, '(A)') '          <lines>'
            call write_lines_xml(unit, coverage_data%files(i))
            write(unit, '(A)') '          </lines>'
            
            write(unit, '(A)') '        </class>'
            write(unit, '(A)') '      </classes>'
            write(unit, '(A)') '    </package>'
        end do
        
    end subroutine write_packages_xml

    subroutine write_lines_xml(unit, file_data)
        integer, intent(in) :: unit
        type(coverage_file_t), intent(in) :: file_data
        integer :: j
        logical :: is_covered
        
        if (.not. allocated(file_data%lines)) return
        
        do j = 1, size(file_data%lines)
            is_covered = file_data%lines(j)%execution_count > 0
            
            write(unit, '(A,I0)', advance='no') '            <line number="', &
                file_data%lines(j)%line_number
            write(unit, '(A,I0)', advance='no') '" hits="', &
                file_data%lines(j)%execution_count
            write(unit, '(A,L1)', advance='no') '" branch="false"/>'
            write(unit, '(A)') ''
        end do
        
    end subroutine write_lines_xml

    subroutine extract_package_name(filename, package_name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: package_name
        integer :: last_slash
        
        ! Extract directory path as package name
        last_slash = index(filename, '/', .true.)
        if (last_slash > 0) then
            package_name = filename(1:last_slash-1)
            ! Replace slashes with dots for package hierarchy
            call replace_char(package_name, '/', '.')
        else
            package_name = "default"
        end if
        
    end subroutine extract_package_name

    subroutine extract_class_name(filename, class_name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: class_name
        integer :: last_slash, last_dot
        
        ! Extract filename without extension as class name
        last_slash = index(filename, '/', .true.)
        last_dot = index(filename, '.', .true.)
        
        if (last_slash > 0) then
            if (last_dot > last_slash) then
                class_name = filename(last_slash+1:last_dot-1)
            else
                class_name = filename(last_slash+1:)
            end if
        else
            if (last_dot > 0) then
                class_name = filename(1:last_dot-1)
            else
                class_name = filename
            end if
        end if
        
    end subroutine extract_class_name

    subroutine replace_char(str, old_char, new_char)
        character(len=:), allocatable, intent(inout) :: str
        character, intent(in) :: old_char, new_char
        integer :: i
        
        do i = 1, len(str)
            if (str(i:i) == old_char) then
                str(i:i) = new_char
            end if
        end do
        
    end subroutine replace_char

    subroutine calculate_file_stats(file_data, line_rate, branch_rate, &
                                   lines_covered, lines_valid, &
                                   branches_covered, branches_valid)
        type(coverage_file_t), intent(in) :: file_data
        real, intent(out) :: line_rate, branch_rate
        integer, intent(out) :: lines_covered, lines_valid
        integer, intent(out) :: branches_covered, branches_valid
        integer :: i
        
        lines_covered = 0
        lines_valid = 0
        branches_covered = 0
        branches_valid = 0
        
        ! Calculate line coverage for this file
        if (allocated(file_data%lines)) then
            lines_valid = size(file_data%lines)
            do i = 1, size(file_data%lines)
                if (file_data%lines(i)%execution_count > 0) then
                    lines_covered = lines_covered + 1
                end if
            end do
        end if
        
        ! Calculate branch coverage for this file
        if (allocated(file_data%branches)) then
            branches_valid = size(file_data%branches)
            do i = 1, size(file_data%branches)
                if (file_data%branches(i)%taken_count > 0) then
                    branches_covered = branches_covered + 1
                end if
            end do
        end if
        
        ! Calculate rates (0.0-1.0)
        if (lines_valid > 0) then
            line_rate = real(lines_covered) / real(lines_valid)
        else
            line_rate = 0.0
        end if
        
        if (branches_valid > 0) then
            branch_rate = real(branches_covered) / real(branches_valid)
        else
            branch_rate = 0.0
        end if
        
    end subroutine calculate_file_stats

    subroutine get_iso_timestamp(timestamp)
        character(len=32), intent(out) :: timestamp
        integer :: date_time(8)
        
        call date_and_time(values=date_time)
        
        write(timestamp, '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,"Z")') &
            date_time(1), date_time(2), date_time(3), &
            date_time(5), date_time(6), date_time(7)
        
    end subroutine get_iso_timestamp

    ! Helper subroutines for unused parameter warnings
    subroutine suppress_unused_warning(reporter, diff_data)
        class(xml_reporter_t), intent(in) :: reporter
        type(coverage_diff_t), intent(in), optional :: diff_data
        associate(r => reporter); end associate
        if (present(diff_data)) then
            associate(d => diff_data); end associate
        end if
    end subroutine suppress_unused_warning
    
    subroutine suppress_unused_warning_simple(reporter)
        class(xml_reporter_t), intent(in) :: reporter
        associate(r => reporter); end associate
    end subroutine suppress_unused_warning_simple

end module coverage_reporter_xml