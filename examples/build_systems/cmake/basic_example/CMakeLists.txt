cmake_minimum_required(VERSION 3.16)
project(fortcov_cmake_demo LANGUAGES Fortran)

# Set Fortran standards and flags
set(CMAKE_Fortran_STANDARD 2008)

# Coverage configuration based on DESIGN.md patterns
option(ENABLE_COVERAGE "Enable coverage analysis" OFF)

# Configure build types with coverage support
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS_TESTING "-g -O0 -fprofile-arcs -ftest-coverage")

# Find codecov package (optional)
find_package(codecov QUIET)

# Define source files
set(DEMO_SOURCES
    src/demo_math.f90
)

# Create library
add_library(demo_math ${DEMO_SOURCES})

# Create executable
add_executable(demo_app app/main.f90)
target_link_libraries(demo_app demo_math)

# Create test executable
add_executable(test_demo test/test_demo.f90)
target_link_libraries(test_demo demo_math)

# Enable testing
enable_testing()
add_test(NAME demo_tests COMMAND test_demo)

# Coverage configuration
if(ENABLE_COVERAGE)
    # Add coverage flags to targets
    target_compile_options(demo_math PRIVATE -fprofile-arcs -ftest-coverage)
    target_link_options(demo_math PRIVATE -lgcov)
    
    target_compile_options(demo_app PRIVATE -fprofile-arcs -ftest-coverage)
    target_link_options(demo_app PRIVATE -lgcov)
    
    target_compile_options(test_demo PRIVATE -fprofile-arcs -ftest-coverage)
    target_link_options(test_demo PRIVATE -lgcov)
    
    # Mark targets for coverage (if codecov is available)
    if(codecov_FOUND)
        add_coverage(demo_math)
        add_coverage(demo_app)
        add_coverage(test_demo)
    endif()
endif()

# Custom target for fortcov integration (from DESIGN.md)
add_custom_target(fortcov_report
    COMMAND ${CMAKE_COMMAND} -E echo "Generating coverage data..."
    COMMAND gcov ${CMAKE_BINARY_DIR}/CMakeFiles/demo_math.dir/src/*.gcno || true
    COMMAND ${CMAKE_COMMAND} -E echo "Running fortcov analysis..."
    COMMAND ${CMAKE_COMMAND} -E echo "fortcov --source=${CMAKE_SOURCE_DIR} --output=coverage.html"
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    DEPENDS demo_math demo_app test_demo
    COMMENT "Generating fortcov coverage report"
)

# Clean coverage target
add_custom_target(clean_coverage
    COMMAND ${CMAKE_COMMAND} -E remove -f *.gcov *.gcda *.gcno
    COMMAND ${CMAKE_COMMAND} -E remove -f coverage.html coverage.xml
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Cleaning coverage files"
)