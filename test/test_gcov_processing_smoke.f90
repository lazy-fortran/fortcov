program test_gcov_processing_smoke
    use gcov_file_processor, only: process_gcov_file
    use coverage_data_core, only: coverage_data_t
    implicit none

    character(len=*), parameter :: path = 'tmp_minimal.gcov'
    type(coverage_data_t) :: data
    logical :: err
    integer :: unit

    open(newunit=unit, file=path, status='replace')
    write(unit,'(A)') '        -:    0:Source:src/sample.f90'
    write(unit,'(A)') '        -:    0:Graph: some'
    write(unit,'(A)') '        -:    0:Data: some'
    write(unit,'(A)') '    1:    1: program sample'
    write(unit,'(A)') '    -:    2: ! comment'
    write(unit,'(A)') '#####:    3: print *, "x"'
    write(unit,'(A)') '    1:    4: end program'
    close(unit)

    call process_gcov_file(path, data, err)
    if (err) then
        print *, 'gcov processing reported error'
        stop 1
    end if

    if (data%total_files /= 1) then
        print *, 'expected 1 file, got ', data%total_files
        stop 2
    end if

    if (data%total_lines /= 3) then
        print *, 'expected 3 executable lines, got ', data%total_lines
        stop 3
    end if

    if (data%covered_lines /= 2) then
        print *, 'expected 2 covered lines, got ', data%covered_lines
        stop 4
    end if

    ! cleanup
    open(newunit=unit, file=path, status='old')
    close(unit, status='delete')

    print *, 'OK: gcov processing smoke'
end program test_gcov_processing_smoke

