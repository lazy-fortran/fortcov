program test_exclude
    use string_utils
    implicit none
    
    logical :: result
    
    ! Test pattern matching
    result = matches_pattern("test_main.f90.gcov", "test_*")
    print *, "Pattern 'test_*' matches 'test_main.f90.gcov': ", result
    
    result = matches_pattern("./test_main.f90.gcov", "test_*")
    print *, "Pattern 'test_*' matches './test_main.f90.gcov': ", result
    
    result = matches_pattern("module.f90.gcov", "test_*")
    print *, "Pattern 'test_*' matches 'module.f90.gcov': ", result
    
end program test_exclude
