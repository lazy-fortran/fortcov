# Meson build configuration with fortcov integration
# Demonstrates Meson integration patterns from DESIGN.md

project('fortcov_meson_demo', 'fortran',
  version : '0.1.0',
  default_options : [
    'warning_level=3',
    'fortran_std=legacy'  # Use legacy for broader compatibility
  ]
)

# Coverage configuration (from DESIGN.md pattern)
coverage_option = get_option('coverage')

# Initialize coverage flags
coverage_args = []
coverage_link_args = []

if coverage_option
  coverage_args = ['-fprofile-arcs', '-ftest-coverage']
  coverage_link_args = ['-lgcov']
  
  message('Coverage enabled: building with coverage instrumentation')
  
  # Add coverage flags to project
  add_project_arguments(coverage_args, language: 'fortran')
  add_project_link_arguments(coverage_link_args, language: 'fortran')
endif

# Source files
demo_sources = files('src/demo_io.f90')

# Create library
demo_lib = library('demo_io',
  demo_sources,
  install : false
)

# Create executable
demo_exe = executable('demo_app',
  'app/main.f90',
  link_with : demo_lib,
  install : false
)

# Create test executable
test_exe = executable('test_demo',
  'test/test_demo.f90', 
  link_with : demo_lib,
  install : false
)

# Add test
test('demo_tests', test_exe)

# Coverage target (from DESIGN.md - find fortcov and create custom target)
fortcov = find_program('fortcov', required: false)

if fortcov.found() and coverage_option
  # Custom target for coverage analysis
  run_target('fortcov_coverage',
    command: [
      find_program('bash'), '-c',
      'cd @0@ && find . -name "*.gcno" -exec gcov {} + && echo "Running fortcov analysis..." && echo "fortcov --source=@1@ --output=coverage.html"'.format(meson.project_build_root(), meson.project_source_root())
    ],
    depends: [demo_exe, test_exe]
  )
  
  # Create a demonstration coverage script
  coverage_script = configure_file(
    input: 'scripts/generate_coverage.sh.in',
    output: 'generate_coverage.sh',
    configuration: {
      'SOURCE_ROOT': meson.project_source_root(),
      'BUILD_ROOT': meson.project_build_root()
    }
  )
  
else
  # Fallback coverage target for demonstration
  run_target('fortcov_coverage',
    command: [
      find_program('bash'), '-c',
      'echo "Coverage analysis (demo mode)"; echo "Note: Install fortcov for full coverage analysis"'
    ]
  )
endif

# Summary
summary({
  'Coverage': coverage_option,
  'Fortcov found': fortcov.found(),
  'Build type': get_option('buildtype'),
}, section: 'Configuration')

# Install nothing by default (this is a demo)
message('Demo project - no installation targets')
