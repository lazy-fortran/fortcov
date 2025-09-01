program test_size_violation_counting
    !! Verifies counting helpers for size violations
    !! Scope: Ensure deduplicated helpers behave correctly

    use size_violation_analyzer, only: file_size_violation_t, &
                                       directory_size_violation_t
    use size_report_generator,   only: count_file_violations_by_severity, &
                                       count_directory_violations_by_severity
    implicit none

    type(file_size_violation_t) :: fviol(4)
    type(directory_size_violation_t) :: dviol(4)
    integer :: c

    ! Initialize file violations with a spread of severities
    fviol(1)%severity_level = "VIOLATION"
    fviol(2)%severity_level = "WARNING"
    fviol(3)%severity_level = "CRITICAL"
    fviol(4)%severity_level = ""

    ! Initialize directory violations similarly
    dviol(1)%severity_level = "VIOLATION"
    dviol(2)%severity_level = "WARNING"
    dviol(3)%severity_level = ""
    dviol(4)%severity_level = "CRITICAL"

    ! File counts
    c = count_file_violations_by_severity(fviol, "VIOLATION")
    if (c /= 1) then
        print *, "Expected 1 file VIOLATION, got:", c
        stop 1
    end if

    c = count_file_violations_by_severity(fviol, "WARNING")
    if (c /= 1) then
        print *, "Expected 1 file WARNING, got:", c
        stop 1
    end if

    c = count_file_violations_by_severity(fviol, "CRITICAL")
    if (c /= 1) then
        print *, "Expected 1 file CRITICAL, got:", c
        stop 1
    end if

    c = count_file_violations_by_severity(fviol, "INFO")
    if (c /= 0) then
        print *, "Expected 0 file INFO, got:", c
        stop 1
    end if

    ! Directory counts
    c = count_directory_violations_by_severity(dviol, "VIOLATION")
    if (c /= 1) then
        print *, "Expected 1 dir VIOLATION, got:", c
        stop 1
    end if

    c = count_directory_violations_by_severity(dviol, "WARNING")
    if (c /= 1) then
        print *, "Expected 1 dir WARNING, got:", c
        stop 1
    end if

    c = count_directory_violations_by_severity(dviol, "CRITICAL")
    if (c /= 1) then
        print *, "Expected 1 dir CRITICAL, got:", c
        stop 1
    end if

    c = count_directory_violations_by_severity(dviol, "")
    if (c /= 1) then
        print *, "Expected 1 dir empty severity, got:", c
        stop 1
    end if

    print *, "✅ Size violation counting tests passed"

end program test_size_violation_counting

