program test_get_base_name_dotfiles
    !! Validate get_base_name behavior for hidden dotfiles and regular files

    use iso_fortran_env, only: output_unit
    use test_utils_core, only: assert_test, reset_test_counters, &
         print_test_header, print_test_summary
    use xml_utils_core, only: xml_utils_get_base_name => get_base_name
    use xml_utility_helpers, only: xml_helpers_get_base_name => get_base_name
    implicit none

    character(len=:), allocatable :: result

    call reset_test_counters()
    call print_test_header("get_base_name dotfiles")

    ! Hidden dotfile without extension: should remain intact
    result = xml_utils_get_base_name('.bashrc')
    call assert_test(trim(result) == '.bashrc', &
        "xml_utils_core: .bashrc stays .bashrc", &
        'Expected .bashrc, got: ' // trim(result))

    result = xml_helpers_get_base_name('.bashrc')
    call assert_test(trim(result) == '.bashrc', &
        "xml_utility_helpers: .bashrc stays .bashrc", &
        'Expected .bashrc, got: ' // trim(result))

    ! Hidden file in directory
    result = xml_utils_get_base_name('dir/.hidden')
    call assert_test(trim(result) == '.hidden', &
        "xml_utils_core: dir/.hidden -> .hidden", &
        'Expected .hidden, got: ' // trim(result))

    result = xml_helpers_get_base_name('dir/.hidden')
    call assert_test(trim(result) == '.hidden', &
        "xml_utility_helpers: dir/.hidden -> .hidden", &
        'Expected .hidden, got: ' // trim(result))

    ! Hidden dotfile with extension: strip extension but keep leading dot
    result = xml_utils_get_base_name('dir/.config.json')
    call assert_test(trim(result) == '.config', &
        "xml_utils_core: dir/.config.json -> .config", &
        'Expected .config, got: ' // trim(result))

    result = xml_helpers_get_base_name('dir/.config.json')
    call assert_test(trim(result) == '.config', &
        "xml_utility_helpers: dir/.config.json -> .config", &
        'Expected .config, got: ' // trim(result))

    ! Regular names remain unchanged in behavior
    result = xml_utils_get_base_name('file.f90')
    call assert_test(trim(result) == 'file', &
        "xml_utils_core: file.f90 -> file", &
        'Expected file, got: ' // trim(result))

    result = xml_helpers_get_base_name('file.f90')
    call assert_test(trim(result) == 'file', &
        "xml_utility_helpers: file.f90 -> file", &
        'Expected file, got: ' // trim(result))

    result = xml_utils_get_base_name('path/to/file')
    call assert_test(trim(result) == 'file', &
        "xml_utils_core: path/to/file -> file", &
        'Expected file, got: ' // trim(result))

    result = xml_helpers_get_base_name('path/to/file')
    call assert_test(trim(result) == 'file', &
        "xml_utility_helpers: path/to/file -> file", &
        'Expected file, got: ' // trim(result))

    call print_test_summary("GET_BASE_NAME DOTFILES", .true.)

end program test_get_base_name_dotfiles
