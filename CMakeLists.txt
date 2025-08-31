cmake_minimum_required(VERSION 3.16)
project(fortcov LANGUAGES Fortran)

# ============================================================================
# FortCov - Modern Fortran Code Coverage Analysis Tool
# ============================================================================
# 
# PRIMARY BUILD SYSTEM: FPM (Fortran Package Manager)
# 
# This CMakeLists.txt provides basic CMake integration for projects that 
# require CMake support, but FPM is the recommended and primary build system.
#
# For FPM-based development (RECOMMENDED):
#   fpm build
#   fpm test --flag "-fprofile-arcs -ftest-coverage"
#   gcov src/*.f90
#   fortcov --source=src *.gcov
#
# For complete CMake integration examples, see:
#   examples/build_systems/cmake/basic_example/
#   doc/developer/build-integration.md
# ============================================================================

# Set Fortran standards and flags
set(CMAKE_Fortran_STANDARD 2008)

# Coverage configuration
option(ENABLE_COVERAGE "Enable coverage analysis" OFF)

# Configure build types
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS_TESTING "-g -O0 -fprofile-arcs -ftest-coverage")

# Find json-fortran dependency
find_package(PkgConfig REQUIRED)
pkg_check_modules(JSONFORTRAN IMPORTED_TARGET json-fortran)

# Define source files
file(GLOB_RECURSE FORTCOV_SOURCES
    "src/*.f90"
)

# Create fortcov library
add_library(fortcov_lib ${FORTCOV_SOURCES})

# Handle json-fortran dependency
if(JSONFORTRAN_FOUND)
    target_link_libraries(fortcov_lib PkgConfig::JSONFORTRAN)
else()
    message(WARNING "json-fortran not found. Install via: FPM (recommended), pkg-config, or manual compilation")
    message(STATUS "For FPM-based development: fpm build (handles dependencies automatically)")
endif()

# Create fortcov executable
add_executable(fortcov app/main.f90)
target_link_libraries(fortcov fortcov_lib)

# Coverage support
if(ENABLE_COVERAGE)
    target_compile_options(fortcov_lib PRIVATE -fprofile-arcs -ftest-coverage)
    target_link_options(fortcov_lib PRIVATE -lgcov)
    
    target_compile_options(fortcov PRIVATE -fprofile-arcs -ftest-coverage) 
    target_link_options(fortcov PRIVATE -lgcov)
endif()

# Custom target for coverage analysis
add_custom_target(fortcov_coverage
    COMMAND ${CMAKE_COMMAND} -E echo "Running tests with coverage..."
    COMMAND ${CMAKE_COMMAND} -E echo "Note: Use 'fpm test --flag=\"-fprofile-arcs -ftest-coverage\"' for comprehensive testing"
    COMMAND ${CMAKE_COMMAND} -E echo "Generate coverage: gcov src/*.f90 && fortcov --source=src *.gcov"
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "FortCov coverage analysis (see examples/build_systems/cmake/ for complete integration)"
)

# Installation
install(TARGETS fortcov DESTINATION bin)

# Display configuration summary
message(STATUS "FortCov CMake Configuration:")
message(STATUS "  Primary build system: FPM (recommended)")
message(STATUS "  CMake integration: Basic support")
message(STATUS "  Coverage enabled: ${ENABLE_COVERAGE}")
message(STATUS "  Complete examples: examples/build_systems/cmake/")
message(STATUS "")
message(STATUS "RECOMMENDED WORKFLOW:")
message(STATUS "  fpm build && fmp test")
message(STATUS "  For coverage: fpm test --flag \"-fprofile-arcs -ftest-coverage\"")
message(STATUS "  Analysis: gcov src/*.f90 && fortcov --source=src *.gcov")
