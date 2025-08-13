module gcov_binary_format
    implicit none
    private
    
    ! Public types
    public :: gcov_data_reader_t
    public :: gcov_function_t
    public :: gcov_counters_t
    public :: gcov_line_data_t
    public :: gcov_line_mapping_t
    
    ! Binary format constants
    integer, parameter :: GCNO_MAGIC = int(Z'67636E6F')  ! "oncg" little-endian  
    integer, parameter :: GCDA_MAGIC = int(Z'67636461')  ! "adcg" little-endian
    integer, parameter :: FUNCTION_TAG = int(Z'01000000')
    integer, parameter :: LINES_TAG = int(Z'01450000')
    integer, parameter :: PROGRAM_TAG = int(Z'00A10000')
    integer, parameter :: ARCS_TAG = int(Z'01430000')
    integer, parameter :: OBJECT_TAG = int(Z'00000000')
    
    ! Line mapping from GCNO LINES_TAG records
    type :: gcov_line_mapping_t
        integer :: function_id
        integer :: source_file_id
        integer, allocatable :: line_numbers(:)
    contains
        procedure :: init => gcov_line_mapping_init
    end type gcov_line_mapping_t

    ! Coverage line data from binary parsing
    type :: gcov_line_data_t
        integer :: line_number
        integer :: execution_count
        logical :: is_executable
        character(len=:), allocatable :: line_content
    contains
        procedure :: init => gcov_line_data_init
    end type gcov_line_data_t
    
    ! Function information from gcno file
    type :: gcov_function_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: source_file
        integer :: line_number = 0
        integer :: function_id = 0
        integer :: lineno_checksum = 0
        integer :: cfg_checksum = 0
        logical :: is_valid = .false.
    contains
        procedure :: init => gcov_function_init
    end type gcov_function_t
    
    ! Execution counters from gcda file
    type :: gcov_counters_t
        integer :: count = 0
        integer(8), allocatable :: values(:)
    contains
        procedure :: init => gcov_counters_init
    end type gcov_counters_t
    
    ! Main binary data reader
    type :: gcov_data_reader_t
        type(gcov_function_t), allocatable :: functions(:)
        type(gcov_counters_t) :: counters
        type(gcov_line_data_t), allocatable :: lines(:)
        type(gcov_line_mapping_t), allocatable :: line_mappings(:)
        logical :: has_gcda = .false.
        logical :: checksums_match = .false.
        logical :: is_big_endian = .false.
        character(len=:), allocatable :: error_message
        character(len=:), allocatable :: source_file_path
        integer, allocatable :: line_to_count_map(:,:)  ! (line_number, count)
    contains
        procedure :: init => gcov_data_reader_init
        procedure :: load_files => gcov_data_reader_load_files
        procedure :: parse_gcno_file => parse_gcno_binary
        procedure :: parse_gcda_file => parse_gcda_binary
        procedure :: validate_data_integrity => gcov_data_validate_integrity
        procedure :: generate_line_data => create_line_data_from_source
        procedure :: parse_gcov_text => parse_gcov_text_output
        procedure :: run_gcov_command => run_gcov_command_on_files
    end type gcov_data_reader_t

contains

    ! Helper function to convert integer to string
    function int_to_string(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=20) :: temp
        write(temp, '(I0)') value
        str = trim(temp)
    end function int_to_string

    ! Binary reading utility functions
    
    ! Read 4-byte word from binary stream
    function read_gcno_word(unit, iostat) result(word)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        integer :: word
        character(len=4) :: bytes
        
        read(unit, iostat=iostat) bytes
        if (iostat /= 0) then
            word = 0
            return
        end if
        
        ! Convert bytes to integer (little-endian)
        word = ichar(bytes(1:1)) + &
               ishft(ichar(bytes(2:2)), 8) + &
               ishft(ichar(bytes(3:3)), 16) + &
               ishft(ichar(bytes(4:4)), 24)
    end function read_gcno_word
    
    ! Read 32-bit value with endianness handling
    function read_gcno_value(unit, big_endian, iostat) result(value)
        integer, intent(in) :: unit
        logical, intent(in) :: big_endian
        integer, intent(out) :: iostat
        integer :: value
        character(len=4) :: bytes
        
        read(unit, iostat=iostat) bytes
        if (iostat /= 0) then
            value = 0
            return
        end if
        
        if (big_endian) then
            ! Big-endian conversion
            value = ishft(ichar(bytes(1:1)), 24) + &
                    ishft(ichar(bytes(2:2)), 16) + &
                    ishft(ichar(bytes(3:3)), 8) + &
                    ichar(bytes(4:4))
        else
            ! Little-endian conversion
            value = ichar(bytes(1:1)) + &
                    ishft(ichar(bytes(2:2)), 8) + &
                    ishft(ichar(bytes(3:3)), 16) + &
                    ishft(ichar(bytes(4:4)), 24)
        end if
    end function read_gcno_value
    
    ! Read 64-bit value with endianness handling
    function read_gcno_value64(unit, big_endian, iostat) result(value)
        integer, intent(in) :: unit
        logical, intent(in) :: big_endian
        integer, intent(out) :: iostat
        integer(8) :: value
        integer :: low, high
        
        low = read_gcno_value(unit, big_endian, iostat)
        if (iostat /= 0) then
            value = 0_8
            return
        end if
        
        high = read_gcno_value(unit, big_endian, iostat)
        if (iostat /= 0) then
            value = 0_8
            return
        end if
        
        if (big_endian) then
            value = ishft(int(high, 8), 32) + int(low, 8)   ! high:low for big-endian
        else
            value = ishft(int(low, 8), 32) + int(high, 8)   ! low:high for little-endian
        end if
    end function read_gcno_value64
    
    ! Read null-terminated string
    function read_gcno_string(unit, big_endian, iostat) result(string)
        integer, intent(in) :: unit
        logical, intent(in) :: big_endian
        integer, intent(out) :: iostat
        character(len=:), allocatable :: string
        integer :: length, i, padding
        character :: char
        character(len=1000) :: buffer
        
        ! Read string length
        length = read_gcno_value(unit, big_endian, iostat)
        if (iostat /= 0 .or. length <= 0) then
            string = ""
            return
        end if
        
        ! String length is in words, so multiply by 4 for bytes
        length = length * 4
        
        if (length > 1000) then
            iostat = -1
            string = ""
            return
        end if
        
        ! Read the string characters
        do i = 1, length
            read(unit, iostat=iostat) char
            if (iostat /= 0) then
                string = ""
                return
            end if
            if (ichar(char) == 0) exit  ! Null terminator
            buffer(i:i) = char
        end do
        
        string = trim(buffer(1:i-1))
    end function read_gcno_string

    ! Initialize line mapping
    subroutine gcov_line_mapping_init(this, function_id, source_file_id, &
                                      line_numbers)
        class(gcov_line_mapping_t), intent(inout) :: this
        integer, intent(in) :: function_id, source_file_id
        integer, intent(in) :: line_numbers(:)
        
        this%function_id = function_id
        this%source_file_id = source_file_id
        if (allocated(this%line_numbers)) deallocate(this%line_numbers)
        allocate(this%line_numbers, source=line_numbers)
    end subroutine gcov_line_mapping_init

    ! Initialize line data
    subroutine gcov_line_data_init(this, line_number, execution_count, &
                                   is_executable, line_content)
        class(gcov_line_data_t), intent(inout) :: this
        integer, intent(in) :: line_number, execution_count
        logical, intent(in) :: is_executable
        character(len=*), intent(in) :: line_content
        
        this%line_number = line_number
        this%execution_count = execution_count
        this%is_executable = is_executable
        this%line_content = trim(line_content)
    end subroutine gcov_line_data_init

    ! Initialize GCov function
    subroutine gcov_function_init(this, name, source_file, line_number)
        class(gcov_function_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source_file
        integer, intent(in) :: line_number
        
        this%name = trim(name)
        this%source_file = trim(source_file)
        this%line_number = line_number
        this%function_id = 0
        this%lineno_checksum = 0
        this%cfg_checksum = 0
        this%is_valid = .true.
    end subroutine gcov_function_init

    ! Initialize counters
    subroutine gcov_counters_init(this, values)
        class(gcov_counters_t), intent(inout) :: this
        integer(8), intent(in) :: values(:)
        
        if (allocated(this%values)) deallocate(this%values)
        this%count = size(values)
        allocate(this%values, source=values)
    end subroutine gcov_counters_init

    ! Initialize data reader
    subroutine gcov_data_reader_init(this)
        class(gcov_data_reader_t), intent(inout) :: this
        
        if (allocated(this%functions)) deallocate(this%functions)
        if (allocated(this%lines)) deallocate(this%lines)
        if (allocated(this%line_mappings)) deallocate(this%line_mappings)
        if (allocated(this%line_to_count_map)) deallocate(this%line_to_count_map)
        call this%counters%init([integer(8) ::])
        this%has_gcda = .false.
        this%checksums_match = .false.
        this%is_big_endian = .false.
        if (allocated(this%error_message)) deallocate(this%error_message)
        if (allocated(this%source_file_path)) deallocate(this%source_file_path)
    end subroutine gcov_data_reader_init

    ! Load and process gcov files using binary parsing
    subroutine gcov_data_reader_load_files(this, gcno_path, gcda_path, success)
        class(gcov_data_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: gcno_path, gcda_path
        logical, intent(out) :: success
        logical :: gcno_exists, gcda_exists, source_exists
        character(len=:), allocatable :: source_path, base_name
        integer :: dot_pos
        
        success = .false.
        call this%init()
        
        ! Check if required files exist
        inquire(file=gcno_path, exist=gcno_exists)
        if (.not. gcno_exists) then
            this%error_message = "GCNO file not found: " // trim(gcno_path)
            return
        end if
        
        ! Determine source file path from gcno path
        dot_pos = index(gcno_path, ".gcno", back=.true.)
        if (dot_pos > 0) then
            base_name = gcno_path(1:dot_pos-1)
        else
            this%error_message = "Invalid GCNO file path: " // trim(gcno_path)
            return
        end if
        
        source_path = base_name // ".f90"
        inquire(file=source_path, exist=source_exists)
        if (.not. source_exists) then
            this%error_message = "Source file not found: " // trim(source_path)
            return
        end if
        
        this%source_file_path = source_path
        
        ! Parse GCNO file (contains graph structure and line info)
        call this%parse_gcno_file(gcno_path, success)
        if (.not. success) return
        
        ! Check for GCDA file and parse if exists
        inquire(file=gcda_path, exist=gcda_exists)
        this%has_gcda = gcda_exists
        
        if (gcda_exists) then
            call this%parse_gcda_file(gcda_path, success)
            if (.not. success) return
        end if
        
        ! Generate line data from source file with execution counts
        call this%generate_line_data(source_path, success)
        if (.not. success) return
        
        this%checksums_match = .true.
        success = .true.
    end subroutine gcov_data_reader_load_files

    ! Parse GCNO binary file
    subroutine parse_gcno_binary(this, gcno_path, success)
        class(gcov_data_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: gcno_path
        logical, intent(out) :: success
        integer :: unit, iostat, magic, version, timestamp
        integer :: tag, length, function_count, mapping_count, i
        type(gcov_function_t), allocatable :: temp_functions(:)
        type(gcov_line_mapping_t), allocatable :: temp_mappings(:)
        character(len=:), allocatable :: func_name, source_file
        integer :: function_id, lineno_checksum, cfg_checksum
        
        success = .false.
        
        ! Open GCNO file for binary reading
        open(newunit=unit, file=gcno_path, status='old', &
             access='stream', form='unformatted', iostat=iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to open GCNO file '" // trim(gcno_path) // &
                                "' (error code " // int_to_string(iostat) // ")"
            return
        end if
        
        ! Read header
        magic = read_gcno_word(unit, iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to read GCNO magic number from '" // &
                                trim(gcno_path) // "'"
            close(unit)
            return
        end if
        
        ! Detect endianness and validate magic
        if (magic == GCNO_MAGIC) then
            this%is_big_endian = .false.  ! Little-endian
        else if (magic == int(Z'6F6E6367')) then  ! Big-endian "gcno"
            this%is_big_endian = .true.
        else
            this%error_message = "Invalid GCNO magic number in '" // &
                                trim(gcno_path) // "' (got " // &
                                int_to_string(magic) // ")"
            close(unit)
            return
        end if
        
        ! Read version and timestamp
        version = read_gcno_value(unit, this%is_big_endian, iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to read GCNO version from '" // &
                                trim(gcno_path) // "'"
            close(unit)
            return
        end if
        
        timestamp = read_gcno_value(unit, this%is_big_endian, iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to read GCNO timestamp from '" // &
                                trim(gcno_path) // "'"
            close(unit)
            return
        end if
        
        ! Skip flags if version >= 8.0.0 (version format varies)
        ! For now assume we don't need to handle this
        
        ! Parse records
        function_count = 0
        mapping_count = 0
        allocate(temp_functions(10))  ! Start with space for 10 functions
        allocate(temp_mappings(10))   ! Start with space for 10 mappings
        
        do
            tag = read_gcno_value(unit, this%is_big_endian, iostat)
            if (iostat /= 0) exit  ! End of file
            
            length = read_gcno_value(unit, this%is_big_endian, iostat)
            if (iostat /= 0) then
                this%error_message = "Failed to read record length in '" // &
                                    trim(gcno_path) // "' (tag: " // &
                                    int_to_string(tag) // ")"
                close(unit)
                return
            end if
            
            if (tag == FUNCTION_TAG) then
                ! Parse function record
                function_id = read_gcno_value(unit, this%is_big_endian, iostat)
                if (iostat /= 0) exit
                
                lineno_checksum = read_gcno_value(unit, this%is_big_endian, iostat)
                if (iostat /= 0) exit
                
                cfg_checksum = read_gcno_value(unit, this%is_big_endian, iostat)
                if (iostat /= 0) exit
                
                func_name = read_gcno_string(unit, this%is_big_endian, iostat)
                if (iostat /= 0) exit
                
                source_file = read_gcno_string(unit, this%is_big_endian, iostat)
                if (iostat /= 0) exit
                
                ! Store function data
                function_count = function_count + 1
                if (function_count > size(temp_functions)) then
                    ! Resize array
                    temp_functions = [temp_functions, temp_functions]
                end if
                
                call temp_functions(function_count)%init(func_name, source_file, 1)
                temp_functions(function_count)%function_id = function_id
                temp_functions(function_count)%lineno_checksum = lineno_checksum
                temp_functions(function_count)%cfg_checksum = cfg_checksum
                
            else if (tag == LINES_TAG) then
                ! Lines record - parse line numbers for function
                call parse_lines_record(unit, length, this%is_big_endian, iostat, &
                                       temp_mappings, mapping_count)
                if (iostat /= 0) then
                    this%error_message = "Failed to parse LINES record in " // &
                                        trim(gcno_path)
                    close(unit)
                    return
                end if
                
            else
                ! Skip unknown record types
                call skip_record_data(unit, length, this%is_big_endian, iostat)
                if (iostat /= 0) exit
            end if
        end do
        
        close(unit)
        
        ! Store functions
        if (function_count > 0) then
            allocate(this%functions(function_count))
            this%functions(1:function_count) = temp_functions(1:function_count)
        else
            ! Create a default function if none found
            allocate(this%functions(1))
            call this%functions(1)%init("main", this%source_file_path, 1)
        end if
        
        ! Store line mappings
        if (mapping_count > 0) then
            allocate(this%line_mappings(mapping_count))
            this%line_mappings(1:mapping_count) = temp_mappings(1:mapping_count)
        end if
        
        deallocate(temp_functions, temp_mappings)
        success = .true.
    end subroutine parse_gcno_binary
    
    ! Skip record data
    subroutine skip_record_data(unit, length_words, big_endian, iostat)
        integer, intent(in) :: unit, length_words
        logical, intent(in) :: big_endian
        integer, intent(out) :: iostat
        integer :: i, dummy
        
        do i = 1, length_words
            dummy = read_gcno_value(unit, big_endian, iostat)
            if (iostat /= 0) return
        end do
    end subroutine skip_record_data
    
    ! Parse LINES record from GCNO file
    subroutine parse_lines_record(unit, length_words, big_endian, iostat, &
                                 temp_mappings, mapping_count)
        integer, intent(in) :: unit, length_words
        logical, intent(in) :: big_endian
        integer, intent(out) :: iostat
        type(gcov_line_mapping_t), intent(inout) :: temp_mappings(:)
        integer, intent(inout) :: mapping_count
        integer :: function_id, source_file_id, line_count, i, line_num
        integer, allocatable :: line_numbers(:), temp_lines(:)
        integer :: words_read
        
        iostat = 0
        words_read = 0
        
        ! Read function ID (first word)
        if (words_read >= length_words) return
        function_id = read_gcno_value(unit, big_endian, iostat)
        if (iostat /= 0) return
        words_read = words_read + 1
        
        ! The LINES record can contain multiple file sections
        ! Each file section: source_file_id, line_numbers..., 0
        ! Continue until all words are consumed
        
        allocate(line_numbers(length_words))  ! Maximum possible size
        line_count = 0
        
        do while (words_read < length_words)
            ! Read source file ID or line number
            line_num = read_gcno_value(unit, big_endian, iostat)
            if (iostat /= 0) then
                deallocate(line_numbers)
                return
            end if
            words_read = words_read + 1
            
            if (line_num == 0) then
                ! End of current file section - create mapping if we have lines
                if (line_count > 0) then
                    mapping_count = mapping_count + 1
                    if (mapping_count <= size(temp_mappings)) then
                        call temp_mappings(mapping_count)%init(function_id, source_file_id, &
                                                              line_numbers(1:line_count))
                    else
                        ! Array is full, cannot store more mappings
                        mapping_count = mapping_count - 1
                    end if
                end if
                
                ! Reset for next file section
                line_count = 0
                ! Next word should be source_file_id for new section (if any)
                
            else if (words_read == 2 .or. line_count == 0) then
                ! This is likely a source_file_id (first after function_id or after 0)
                source_file_id = line_num
                
            else
                ! This is a line number
                line_count = line_count + 1
                line_numbers(line_count) = line_num
            end if
        end do
        
        ! Handle case where record doesn't end with 0
        if (line_count > 0) then
            mapping_count = mapping_count + 1
            if (mapping_count <= size(temp_mappings)) then
                call temp_mappings(mapping_count)%init(function_id, source_file_id, &
                                                      line_numbers(1:line_count))
            end if
        end if
        
        deallocate(line_numbers)
    end subroutine parse_lines_record

    ! Parse GCDA binary file for execution counts
    subroutine parse_gcda_binary(this, gcda_path, success)
        class(gcov_data_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: gcda_path
        logical, intent(out) :: success
        integer :: unit, iostat, magic, version, timestamp
        integer :: tag, length, counter_count, i, func_id
        integer(8), allocatable :: temp_counters(:)
        integer, allocatable :: temp_map(:,:)
        integer :: map_count
        
        success = .false.
        
        ! Open GCDA file for binary reading
        open(newunit=unit, file=gcda_path, status='old', &
             access='stream', form='unformatted', iostat=iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to open GCDA file '" // trim(gcda_path) // &
                                "' (error code " // int_to_string(iostat) // ")"
            return
        end if
        
        ! Read header
        magic = read_gcno_word(unit, iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to read GCDA magic number from '" // &
                                trim(gcda_path) // "'"
            close(unit)
            return
        end if
        
        ! Validate magic (should match GCNO endianness)
        if (magic /= GCDA_MAGIC .and. magic /= int(Z'61646367')) then
            this%error_message = "Invalid GCDA magic number in '" // &
                                trim(gcda_path) // "' (got " // &
                                int_to_string(magic) // ")"
            close(unit)
            return
        end if
        
        ! Read version and timestamp
        version = read_gcno_value(unit, this%is_big_endian, iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to read GCDA version from '" // &
                                trim(gcda_path) // "'"
            close(unit)
            return
        end if
        
        timestamp = read_gcno_value(unit, this%is_big_endian, iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to read GCDA timestamp from '" // &
                                trim(gcda_path) // "'"
            close(unit)
            return
        end if
        
        ! Parse counter records
        counter_count = 0
        map_count = 0
        allocate(temp_counters(100))  ! Start with space for 100 counters
        allocate(temp_map(2, 100))    ! Map line numbers to counter indices
        
        do
            tag = read_gcno_value(unit, this%is_big_endian, iostat)
            if (iostat /= 0) exit  ! End of file
            
            length = read_gcno_value(unit, this%is_big_endian, iostat)
            if (iostat /= 0) then
                this%error_message = "Failed to read GCDA record length in '" // &
                                    trim(gcda_path) // "' (tag: " // &
                                    int_to_string(tag) // ")"
                close(unit)
                return
            end if
            
            if (tag == OBJECT_TAG) then
                ! Object summary - contains function checksum
                func_id = read_gcno_value(unit, this%is_big_endian, iostat)
                if (iostat /= 0) exit
                
                ! Skip checksum data
                call skip_record_data(unit, length - 1, this%is_big_endian, iostat)
                if (iostat /= 0) exit
                
            else if (tag == PROGRAM_TAG) then
                ! Program data - contains execution counters
                do i = 1, length / 2  ! Each counter is 64-bit (2 words)
                    counter_count = counter_count + 1
                    if (counter_count > size(temp_counters)) then
                        ! Resize arrays
                        temp_counters = [temp_counters, temp_counters]
                        temp_map = reshape([temp_map, temp_map], [2, size(temp_counters)])
                    end if
                    
                    temp_counters(counter_count) = &
                        read_gcno_value64(unit, this%is_big_endian, iostat)
                    if (iostat /= 0) exit
                    
                    ! Create mapping using proper GCNO/GCDA correlation via function IDs
                    if (allocated(this%line_mappings) .and. allocated(this%functions)) then
                        ! Find matching function by ID and correlate with line mapping
                        block
                            integer :: mapping_idx, func_idx, line_idx
                            logical :: found_mapping
                            
                            found_mapping = .false.
                            
                            ! Find function matching current func_id from OBJECT_TAG
                            do func_idx = 1, size(this%functions)
                                if (this%functions(func_idx)%function_id == func_id) then
                                    ! Find line mapping for this function
                                    do mapping_idx = 1, size(this%line_mappings)
                                        if (this%line_mappings(mapping_idx)%function_id == func_id) then
                                            ! Map counter index to line number
                                            line_idx = counter_count
                                            if (line_idx <= size(this%line_mappings(mapping_idx)%line_numbers)) then
                                                map_count = map_count + 1
                                                temp_map(1, map_count) = &
                                                    this%line_mappings(mapping_idx)%line_numbers(line_idx)
                                                temp_map(2, map_count) = int(temp_counters(counter_count))
                                                found_mapping = .true.
                                            end if
                                            exit
                                        end if
                                    end do
                                    exit
                                end if
                            end do
                            
                            ! If no proper correlation found, do not create mapping
                        end block
                    end if
                end do
                
            else
                ! Skip unknown record types
                call skip_record_data(unit, length, this%is_big_endian, iostat)
                if (iostat /= 0) exit
            end if
        end do
        
        close(unit)
        
        ! Store counters
        if (counter_count > 0) then
            call this%counters%init(temp_counters(1:counter_count))
            
            ! Store line mapping
            if (map_count > 0) then
                allocate(this%line_to_count_map(2, map_count))
                this%line_to_count_map = temp_map(:, 1:map_count)
            end if
        else
            call this%counters%init([integer(8) ::])
        end if
        
        deallocate(temp_counters, temp_map)
        success = .true.
    end subroutine parse_gcda_binary

    ! Generate line data from source file with execution counts
    subroutine create_line_data_from_source(this, source_path, success)
        class(gcov_data_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: source_path
        logical, intent(out) :: success
        integer :: unit, iostat, line_count, i, execution_count
        character(len=1000) :: line_buffer
        type(gcov_line_data_t), allocatable :: temp_lines(:)
        logical :: is_executable
        
        success = .false.
        
        ! First pass: count lines
        open(newunit=unit, file=source_path, status='old', iostat=iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to open source file: " // trim(source_path)
            return
        end if
        
        line_count = 0
        do
            read(unit, '(A)', iostat=iostat) line_buffer
            if (iostat /= 0) exit
            line_count = line_count + 1
        end do
        close(unit)
        
        if (line_count == 0) then
            this%error_message = "Source file is empty"
            return
        end if
        
        ! Second pass: read lines and assign execution counts
        allocate(temp_lines(line_count))
        
        open(newunit=unit, file=source_path, status='old')
        
        do i = 1, line_count
            read(unit, '(A)', iostat=iostat) line_buffer
            if (iostat /= 0) exit
            
            ! Determine if line is executable (simplified heuristic)
            is_executable = is_line_executable(trim(line_buffer))
            
            ! Get execution count for this line
            execution_count = get_line_execution_count(this, i)
            
            call temp_lines(i)%init(i, execution_count, is_executable, &
                                   trim(line_buffer))
        end do
        
        close(unit)
        
        ! Store line data
        allocate(this%lines(line_count))
        this%lines = temp_lines
        deallocate(temp_lines)
        
        success = .true.
    end subroutine create_line_data_from_source
    
    ! Heuristic to determine if a line is executable
    function is_line_executable(line_content) result(executable)
        character(len=*), intent(in) :: line_content
        logical :: executable
        character(len=:), allocatable :: trimmed_line
        
        trimmed_line = trim(adjustl(line_content))
        executable = .false.
        
        ! Skip empty lines
        if (len(trimmed_line) == 0) return
        
        ! Skip comments
        if (trimmed_line(1:1) == '!') return
        
        ! Skip some declarations and keywords
        if (index(trimmed_line, 'implicit') == 1) return
        if (index(trimmed_line, 'integer') == 1) return
        if (index(trimmed_line, 'logical') == 1) return
        if (index(trimmed_line, 'character') == 1) return
        if (index(trimmed_line, 'real') == 1) return
        if (index(trimmed_line, 'program') == 1) return
        if (index(trimmed_line, 'end program') == 1) return
        if (index(trimmed_line, 'end if') == 1) return
        if (index(trimmed_line, 'end do') == 1) return
        if (index(trimmed_line, 'else') == 1) return
        
        ! Consider it executable if it contains assignment or call
        if (index(trimmed_line, '=') > 0) then
            executable = .true.
            return
        end if
        
        if (index(trimmed_line, 'print') == 1) then
            executable = .true.
            return
        end if
        
        if (index(trimmed_line, 'call') == 1) then
            executable = .true.
            return
        end if
        
        if (index(trimmed_line, 'if') == 1) then
            executable = .true.
            return
        end if
        
        if (index(trimmed_line, 'do') == 1) then
            executable = .true.
            return
        end if
    end function is_line_executable
    
    ! Get execution count for a specific line number
    function get_line_execution_count(this, line_number) result(count)
        class(gcov_data_reader_t), intent(in) :: this
        integer, intent(in) :: line_number
        integer :: count
        integer :: i
        
        count = -1  ! Default: non-executable
        
        ! If we have coverage data, look up the count
        if (allocated(this%line_to_count_map)) then
            do i = 1, size(this%line_to_count_map, 2)
                if (this%line_to_count_map(1, i) == line_number) then
                    count = this%line_to_count_map(2, i)
                    return
                end if
            end do
        end if
        
        ! Check if line is executable using GCNO line mappings
        if (allocated(this%line_mappings)) then
            block
                integer :: i, j
                logical :: found_executable
                found_executable = .false.
                
                do i = 1, size(this%line_mappings)
                    if (allocated(this%line_mappings(i)%line_numbers)) then
                        do j = 1, size(this%line_mappings(i)%line_numbers)
                            if (this%line_mappings(i)%line_numbers(j) == line_number) then
                                found_executable = .true.
                                exit
                            end if
                        end do
                    end if
                    if (found_executable) exit
                end do
                
                if (found_executable) then
                    if (this%has_gcda) then
                        count = 0  ! Executable but not covered
                    else
                        count = -1  ! No coverage data available
                    end if
                end if
            end block
        end if
    end function get_line_execution_count

    ! Run gcov command on source file
    subroutine run_gcov_command_on_files(this, source_path, has_gcda, success)
        class(gcov_data_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: source_path
        logical, intent(in) :: has_gcda
        logical, intent(out) :: success
        character(len=:), allocatable :: command, current_dir
        character(len=:), allocatable :: source_dir, source_filename
        integer :: last_slash, exit_status
        
        success = .false.
        
        ! Extract directory and filename from source path
        last_slash = index(source_path, "/", back=.true.)
        if (last_slash > 0) then
            source_dir = source_path(1:last_slash-1)
            source_filename = source_path(last_slash+1:)
        else
            source_dir = "."
            source_filename = source_path
        end if
        
        ! Store current directory
        call get_environment_variable("PWD", current_dir)
        if (.not. allocated(current_dir)) current_dir = "."
        
        ! Change to source directory and run gcov
        if (has_gcda) then
            command = "cd " // source_dir // " && gcov " // source_filename
        else
            command = "cd " // source_dir // " && gcov -n " // source_filename
        end if
        
        call execute_command_line(command, exitstat=exit_status)
        
        if (exit_status /= 0) then
            this%error_message = "gcov command failed with exit status: " // &
                                int_to_string(exit_status)
            return
        end if
        
        success = .true.
    end subroutine run_gcov_command_on_files

    ! Parse gcov text output file
    subroutine parse_gcov_text_output(this, gcov_file_path, success)
        class(gcov_data_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: gcov_file_path
        logical, intent(out) :: success
        integer :: unit, iostat, line_count, i
        character(len=1000) :: line_buffer
        character(len=20) :: exec_count_str, line_num_str
        character(len=:), allocatable :: line_content
        integer :: colon1, colon2, execution_count, line_number
        logical :: is_executable, file_exists
        type(gcov_line_data_t), allocatable :: temp_lines(:)
        integer(8), allocatable :: temp_counters(:)
        integer :: counter_count
        
        success = .false.
        
        ! Check if gcov output file exists
        inquire(file=gcov_file_path, exist=file_exists)
        if (.not. file_exists) then
            this%error_message = "GCOV output file not found: " // &
                                trim(gcov_file_path)
            return
        end if
        
        ! First pass: count lines
        open(newunit=unit, file=gcov_file_path, status='old', iostat=iostat)
        if (iostat /= 0) then
            this%error_message = "Failed to open gcov file: " // &
                                trim(gcov_file_path)
            return
        end if
        
        line_count = 0
        do
            read(unit, '(A)', iostat=iostat) line_buffer
            if (iostat /= 0) exit
            ! Skip header lines (start with spaces and line number 0)
            if (index(line_buffer, ":    0:") == 0) then
                line_count = line_count + 1
            end if
        end do
        close(unit)
        
        if (line_count == 0) then
            this%error_message = "No coverage data found in gcov file"
            return
        end if
        
        ! Second pass: parse coverage data
        allocate(temp_lines(line_count))
        allocate(temp_counters(line_count))
        
        open(newunit=unit, file=gcov_file_path, status='old')
        
        i = 0
        counter_count = 0
        
        do
            read(unit, '(A)', iostat=iostat) line_buffer
            if (iostat /= 0) exit
            
            ! Skip header lines (line number 0)
            if (index(line_buffer, ":    0:") > 0) cycle
            
            ! Parse gcov line format: "exec_count:line_num:content"
            colon1 = index(line_buffer, ":")
            if (colon1 == 0) cycle
            
            colon2 = index(line_buffer(colon1+1:), ":") + colon1
            if (colon2 <= colon1) cycle
            
            ! Extract execution count string
            exec_count_str = trim(adjustl(line_buffer(1:colon1-1)))
            
            ! Extract line number string  
            line_num_str = trim(adjustl(line_buffer(colon1+1:colon2-1)))
            
            ! Extract line content
            line_content = trim(line_buffer(colon2+1:))
            
            ! Parse line number
            read(line_num_str, *, iostat=iostat) line_number
            if (iostat /= 0) cycle
            
            ! Parse execution count
            if (trim(exec_count_str) == "-") then
                execution_count = -1  ! Non-executable line
                is_executable = .false.
            else if (trim(exec_count_str) == "#####") then
                execution_count = 0   ! Unexecuted line
                is_executable = .true.
            else
                read(exec_count_str, *, iostat=iostat) execution_count
                if (iostat /= 0) then
                    execution_count = -1
                    is_executable = .false.
                else
                    is_executable = .true.
                end if
            end if
            
            i = i + 1
            if (i <= line_count) then
                call temp_lines(i)%init(line_number, execution_count, &
                                       is_executable, line_content)
                
                ! Store execution counts for executable lines
                if (is_executable) then
                    counter_count = counter_count + 1
                    if (counter_count <= size(temp_counters)) then
                        temp_counters(counter_count) = int(execution_count, 8)
                    end if
                end if
            end if
        end do
        
        close(unit)
        
        ! Store results
        if (i > 0) then
            allocate(this%lines(i))
            this%lines(1:i) = temp_lines(1:i)
        end if
        
        if (counter_count > 0) then
            call this%counters%init(temp_counters(1:counter_count))
        else
            call this%counters%init([integer(8) ::])
        end if
        
        deallocate(temp_lines, temp_counters)
        success = .true.
    end subroutine parse_gcov_text_output

    ! Validate data integrity
    function gcov_data_validate_integrity(this) result(valid)
        class(gcov_data_reader_t), intent(in) :: this
        logical :: valid
        
        valid = .false.
        
        ! Check that we have valid functions
        if (.not. allocated(this%functions) .or. size(this%functions) == 0) return
        
        ! Check that we have line data
        if (.not. allocated(this%lines) .or. size(this%lines) == 0) return
        
        ! Validate first function
        if (.not. this%functions(1)%is_valid) return
        
        valid = .true.
    end function gcov_data_validate_integrity

end module gcov_binary_format