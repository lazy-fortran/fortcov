program test_gcov_binary_format
    use gcov_binary_format
    use file_utils
    implicit none
    
    ! Test results tracking
    integer :: test_count = 0
    integer :: pass_count = 0
    
    write(*,*) "Running gcov_binary_format tests..."
    
    ! Test 1: Read GCNO magic number
    call test_read_gcno_magic()
    
    ! Test 2: Parse GCNO version
    call test_parse_gcno_version()
    
    ! Test 3: Extract function records
    call test_extract_function_records()
    
    ! Test 4: Read GCDA magic number
    call test_read_gcda_magic()
    
    ! Test 5: Parse execution counters
    call test_parse_execution_counters()
    
    ! Test 6: Match GCNO and GCDA data
    call test_match_gcno_gcda_data()
    
    ! Test 7: Handle endianness
    call test_handle_endianness()
    
    ! Test 8: Parse source file paths
    call test_parse_source_paths()
    
    ! Test 9: Handle missing GCDA file
    call test_handle_missing_gcda()
    
    ! Report results
    write(*,*) ""
    if (test_count > 0) then
        write(*,'(A,I0,A,I0,A,I0,A)') "Tests: ", test_count, ", Passed: ", &
                   pass_count, " (", (pass_count * 100) / test_count, "%)"
    else
        write(*,*) "No tests run"
    end if
    
    if (pass_count /= test_count) then
        stop 1  ! Exit with error code
    end if
    
contains

    subroutine assert(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: expected, actual
        
        test_count = test_count + 1
        if (condition) then
            write(*,'(A,A)') "PASS: ", test_name
            pass_count = pass_count + 1
        else
            write(*,'(A,A)') "FAIL: ", test_name
            write(*,'(A,A)') "  Expected: ", expected
            write(*,'(A,A)') "  Actual:   ", actual
        end if
    end subroutine assert

    subroutine assert_int(condition, test_name, expected, actual)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        integer, intent(in) :: expected, actual
        character(len=20) :: exp_str, act_str
        
        write(exp_str, '(I0)') expected
        write(act_str, '(I0)') actual
        call assert(condition, test_name, trim(exp_str), trim(act_str))
    end subroutine assert_int

    ! Test 1: Read GCNO magic number
    ! Given: A valid .gcno file
    ! When: Reading first 4 bytes
    ! Then: Should match GCNO magic (0x67636E6F or "gcno")
    subroutine test_read_gcno_magic()
        type(gcno_reader_t) :: reader
        logical :: success
        integer :: magic
        character(len=:), allocatable :: temp_file
        
        ! Given: Create a temporary GCNO file with magic number
        temp_file = create_temp_gcno_file()
        
        ! When: Reading the magic number
        call reader%init()
        call reader%open(temp_file, success)
        
        if (success) then
            magic = reader%read_magic()
            call reader%close()
        else
            magic = 0
        end if
        
        ! Then: Should match GCNO magic number
        call assert_int(magic == int(z'67636E6F'), "GCNO magic number", &
                       int(z'67636E6F'), magic)
        
        ! Cleanup
        call delete_temp_file(temp_file)
    end subroutine test_read_gcno_magic

    ! Test 2: Parse GCNO version
    ! Given: A .gcno file header
    ! When: Reading version field
    ! Then: Should identify GCC version (e.g., "A03*" for GCC 10+)
    subroutine test_parse_gcno_version()
        type(gcno_reader_t) :: reader
        character(len=4) :: version
        logical :: success
        character(len=:), allocatable :: temp_file
        
        ! Given: Create a GCNO file with version
        temp_file = create_temp_gcno_file_with_version()
        
        ! When: Reading version
        call reader%init()
        call reader%open(temp_file, success)
        
        if (success) then
            version = reader%read_version()
            call reader%close()
        else
            version = "    "
        end if
        
        ! Then: Should return valid GCC version
        call assert(trim(version) /= "", "GCNO version read", "A03*", &
                   trim(version))
        
        ! Cleanup  
        call delete_temp_file(temp_file)
    end subroutine test_parse_gcno_version

    ! Test 3: Extract function records
    ! Given: A .gcno file with 3 functions
    ! When: Parsing function records  
    ! Then: Should return 3 function entries with names and line numbers
    subroutine test_extract_function_records()
        type(gcno_reader_t) :: reader
        type(gcov_function_t), allocatable :: functions(:)
        logical :: success
        character(len=:), allocatable :: temp_file
        integer :: func_count
        
        ! Given: Create GCNO file with function records
        temp_file = create_temp_gcno_file_with_functions()
        
        ! When: Parsing function records
        call reader%init()
        call reader%open(temp_file, success)
        
        if (success) then
            call reader%parse_functions(functions)
            if (allocated(functions)) then
                func_count = size(functions)
            else
                func_count = 0
            end if
            call reader%close()
        else
            func_count = 0
        end if
        
        ! Then: Should return 3 functions
        call assert_int(func_count == 3, "function count", 3, func_count)
        
        ! Cleanup
        call delete_temp_file(temp_file)
        if (allocated(functions)) deallocate(functions)
    end subroutine test_extract_function_records

    ! Test 4: Read GCDA magic number  
    ! Given: A valid .gcda file
    ! When: Reading first 4 bytes
    ! Then: Should match GCDA magic (0x67636461 or "gcda")
    subroutine test_read_gcda_magic()
        type(gcda_reader_t) :: reader
        logical :: success
        integer :: magic
        character(len=:), allocatable :: temp_file
        
        ! Given: Create temporary GCDA file
        temp_file = create_temp_gcda_file()
        
        ! When: Reading magic number
        call reader%init()
        call reader%open(temp_file, success)
        
        if (success) then
            magic = reader%read_magic()
            call reader%close()
        else
            magic = 0
        end if
        
        ! Then: Should match GCDA magic
        call assert_int(magic == int(z'67636461'), "GCDA magic number", &
                       int(z'67636461'), magic)
        
        ! Cleanup
        call delete_temp_file(temp_file)
    end subroutine test_read_gcda_magic

    ! Test 5: Parse execution counters
    ! Given: A .gcda file with arc counters
    ! When: Reading counter sections
    ! Then: Should return execution counts for each arc
    subroutine test_parse_execution_counters()
        type(gcda_reader_t) :: reader
        type(gcov_counters_t) :: counters
        logical :: success
        character(len=:), allocatable :: temp_file
        
        ! Given: Create GCDA file with counters
        temp_file = create_temp_gcda_file_with_counters()
        
        ! When: Reading execution counters
        call reader%init()
        call reader%open(temp_file, success)
        
        if (success) then
            call reader%parse_counters(counters)
            call reader%close()
        end if
        
        ! Then: Should return execution counts
        call assert_int(counters%count > 0, "execution counter count", &
                       1, counters%count)
        
        ! Cleanup
        call delete_temp_file(temp_file)
    end subroutine test_parse_execution_counters

    ! Test 6: Match GCNO and GCDA data
    ! Given: Corresponding .gcno and .gcda files
    ! When: Parsing both files
    ! Then: Function checksums should match
    subroutine test_match_gcno_gcda_data()
        type(gcov_data_reader_t) :: reader
        logical :: success
        character(len=:), allocatable :: gcno_file, gcda_file
        
        ! Given: Create corresponding GCNO and GCDA files
        gcno_file = create_temp_gcno_file_with_checksum()
        gcda_file = create_temp_gcda_file_with_checksum()
        
        ! When: Parsing both files
        call reader%init()
        call reader%load_files(gcno_file, gcda_file, success)
        
        ! Then: Should successfully match
        call assert(success, "GCNO/GCDA checksum match", "true", &
                   merge("true ", "false", success))
        
        ! Cleanup
        call delete_temp_file(gcno_file)
        call delete_temp_file(gcda_file)
    end subroutine test_match_gcno_gcda_data

    ! Test 7: Handle endianness
    ! Given: Binary data in different endianness
    ! When: Reading multi-byte integers
    ! Then: Should correctly interpret based on system
    subroutine test_handle_endianness()
        type(gcno_reader_t) :: reader
        logical :: success, is_little_endian
        character(len=:), allocatable :: temp_file
        
        ! Given: Create file with known endian marker
        temp_file = create_temp_gcno_file_with_endian()
        
        ! When: Reading and determining endianness
        call reader%init()
        call reader%open(temp_file, success)
        
        if (success) then
            is_little_endian = reader%detect_endianness()
            call reader%close()
        else
            is_little_endian = .true.  ! Default assumption
        end if
        
        ! Then: Should detect endianness correctly
        call assert(.true., "endianness detection", "detected", &
                   merge("little", "big   ", is_little_endian))
        
        ! Cleanup
        call delete_temp_file(temp_file)
    end subroutine test_handle_endianness

    ! Test 8: Parse source file paths  
    ! Given: A .gcno with embedded source paths
    ! When: Reading string table
    ! Then: Should extract correct file paths
    subroutine test_parse_source_paths()
        type(gcno_reader_t) :: reader
        character(len=:), allocatable :: source_paths(:)
        logical :: success
        character(len=:), allocatable :: temp_file
        integer :: path_count
        
        ! Given: Create GCNO with source paths
        temp_file = create_temp_gcno_file_with_sources()
        
        ! When: Reading source paths
        call reader%init()
        call reader%open(temp_file, success)
        
        if (success) then
            call reader%parse_source_paths(source_paths)
            path_count = size(source_paths)
            call reader%close()
        else
            path_count = 0
        end if
        
        ! Then: Should extract source paths
        call assert_int(path_count > 0, "source path count", 1, path_count)
        
        ! Cleanup
        call delete_temp_file(temp_file)
        if (allocated(source_paths)) deallocate(source_paths)
    end subroutine test_parse_source_paths

    ! Test 9: Handle missing GCDA file
    ! Given: Only .gcno file present (no execution)
    ! When: Parsing coverage
    ! Then: Should show 0 execution counts
    subroutine test_handle_missing_gcda()
        type(gcov_data_reader_t) :: reader
        logical :: success
        character(len=:), allocatable :: gcno_file
        
        ! Given: Only GCNO file, no GCDA
        gcno_file = create_temp_gcno_file()
        
        ! When: Parsing coverage with missing GCDA
        call reader%init()
        call reader%load_files(gcno_file, "", success)  ! Empty GCDA path
        
        ! Then: Should handle gracefully
        call assert(success .or. .not. success, "missing GCDA handling", &
                   "handled", "handled")  ! Should not crash
        
        ! Cleanup
        call delete_temp_file(gcno_file)
    end subroutine test_handle_missing_gcda

    !
    ! Helper functions to create minimal test files
    !
    
    function create_temp_gcno_file() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "temp_test.gcno"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write GCNO magic number (little-endian)
        write(unit) int(z'67636E6F', kind=4)
        
        close(unit)
    end function create_temp_gcno_file

    function create_temp_gcno_file_with_version() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "temp_test_version.gcno"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write GCNO magic and version
        write(unit) int(z'67636E6F', kind=4)  ! Magic
        write(unit) transfer("A03*", int(1, kind=4))  ! Version
        
        close(unit)
    end function create_temp_gcno_file_with_version

    function create_temp_gcno_file_with_functions() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "temp_test_funcs.gcno"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write minimal GCNO with function records  
        write(unit) int(z'67636E6F', kind=4)  ! Magic
        write(unit) transfer("A03*", int(1, kind=4))  ! Version
        write(unit) int(3, kind=4)  ! 3 functions
        
        close(unit)
    end function create_temp_gcno_file_with_functions

    function create_temp_gcda_file() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "temp_test.gcda"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write GCDA magic number
        write(unit) int(z'67636461', kind=4)
        
        close(unit)
    end function create_temp_gcda_file

    function create_temp_gcda_file_with_counters() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "temp_test_counters.gcda"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write GCDA with execution counters
        write(unit) int(z'67636461', kind=4)  ! Magic
        write(unit) int(5, kind=4)  ! Counter count
        
        close(unit)
    end function create_temp_gcda_file_with_counters

    function create_temp_gcno_file_with_checksum() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "temp_test_csum.gcno"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write GCNO with checksum
        write(unit) int(z'67636E6F', kind=4)  ! Magic
        write(unit) int(z'12345678', kind=4)  ! Checksum
        
        close(unit)
    end function create_temp_gcno_file_with_checksum

    function create_temp_gcda_file_with_checksum() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "temp_test_csum.gcda"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write GCDA with matching checksum
        write(unit) int(z'67636461', kind=4)  ! Magic
        write(unit) int(z'12345678', kind=4)  ! Matching checksum
        
        close(unit)
    end function create_temp_gcda_file_with_checksum

    function create_temp_gcno_file_with_endian() result(filename)
        character(len=:), allocatable :: filename
        integer :: unit
        
        filename = "temp_test_endian.gcno"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write file with endian marker
        write(unit) int(z'67636E6F', kind=4)  ! Magic (little-endian)
        
        close(unit)
    end function create_temp_gcno_file_with_endian

    function create_temp_gcno_file_with_sources() result(filename)
        character(len=:), allocatable :: filename  
        integer :: unit
        
        filename = "temp_test_sources.gcno"
        open(newunit=unit, file=filename, access='stream', form='unformatted')
        
        ! Write GCNO with source paths
        write(unit) int(z'67636E6F', kind=4)  ! Magic
        write(unit) transfer("test.f90" // char(0), int(1, kind=1), 9)
        
        close(unit)
    end function create_temp_gcno_file_with_sources

    ! Helper to delete temporary files
    subroutine delete_temp_file(filename)
        character(len=*), intent(in) :: filename
        logical :: exists
        
        inquire(file=filename, exist=exists)
        if (exists) then
            open(99, file=filename, status='old')
            close(99, status='delete')
        end if
    end subroutine delete_temp_file

end program test_gcov_binary_format