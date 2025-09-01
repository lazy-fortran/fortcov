program test_string_utils_matches_pattern_issue_1106
    use string_utils, only: matches_pattern
    use test_utils_core, only: assert_test, reset_test_counters, &
        print_test_header, print_test_summary
    implicit none

    logical :: ok

    call reset_test_counters()
    call print_test_header("string_utils.matches_pattern issue #1106")

    ! Expect true: exactly one directory between src/ and def.*
    ok = matches_pattern("src/abc/def.f90", "src/*/def.*")
    call assert_test(ok, &
        "matches_pattern true for src/*/def.* with one dir", &
        "Expected true for src/abc/def.f90 vs src/*/def.*")

    ! Expect false: two directories should not match single-segment wildcard
    ok = matches_pattern("src/x/y/def.f90", "src/*/def.*")
    call assert_test(.not. ok, &
        "matches_pattern false for src/*/def.* with two dirs", &
        "Expected false for src/x/y/def.f90 vs src/*/def.*")

    call print_test_summary("string_utils.matches_pattern issue #1106", .true.)
end program test_string_utils_matches_pattern_issue_1106

