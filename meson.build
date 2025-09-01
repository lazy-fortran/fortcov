project('fortcov', 'fortran',
  version : '0.4.0',
  license : 'MIT', 
  default_options : [
    'warning_level=3',
    'fortran_std=legacy'
  ]
)

# ============================================================================
# FortCov - Modern Fortran Code Coverage Analysis Tool  
# ============================================================================
#
# PRIMARY BUILD SYSTEM: FPM (Fortran Package Manager)
#
# This meson.build provides basic Meson integration for projects that require
# Meson support, but FPM is the recommended and primary build system.
#
# For FPM-based development (RECOMMENDED):
#   fpm build
#   fpm test --flag "-fprofile-arcs -ftest-coverage"
#   gcov src/*.f90
#   fortcov --source=src *.gcov
#
# For complete Meson integration examples, see:
#   examples/build_systems/meson/basic_example/
#   doc/developer/build-integration.md
# ============================================================================

# Coverage configuration
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

# Find json-fortran dependency
jsonfortran_dep = dependency('json-fortran', required: false, method: 'pkg-config')

if not jsonfortran_dep.found()
  warning('json-fortran not found via pkg-config')
  message('For FPM-based development: fpm build (handles dependencies automatically)')
  message('Manual installation: see doc/developer/build-integration.md')
  
  # Create dummy dependency to allow compilation
  jsonfortran_dep = declare_dependency()
endif

# Collect source files
fortcov_sources = []
fortcov_sources += files(
  # TECHNICAL FIX: Corrected from non-existent 'src/config/args/arg_parser_core.f90'
  # Using actual argument parsing files that exist in codebase:
  'src/config/args/config_classifier_args.f90',
  'src/config/args/config_detector_args.f90', 
  'src/config/args/config_positional_args.f90',
  
  # Core config system 
  'src/config/core/config_core.f90',
  'src/config/core/config_types.f90',
  'src/config/core/config_defaults_core.f90',
  'src/config/parsers/config_parser_command_line.f90',
  'src/config/parsers/config_parser_special.f90',
  
  # VERIFIED: Zero-config CLI flag implementation (--zero-config)
  # Found in: config_parser_flags.f90, config_parser_special.f90, main.f90
  'src/zero_config/zero_config_core.f90',
  'src/zero_config/zero_config_manager.f90',
  
  # Core architecture and processing
  'src/coverage/coverage_data_builder.f90',
  'src/core/architecture/architectural_core.f90', 
  'src/utils/command/command_utilities.f90',
  'src/utils/file_ops/directory_operations_secure.f90',
  'src/utils/file_ops/file_ops_secure.f90',
  'src/utils/path_ops/path_utils_consolidated.f90',
  'src/json/json_core.f90',
  'src/gcov/gcov_executor.f90',
  'src/reporters/markdown_reporter.f90',
  'src/reporters/html_reporter.f90'
)

# CRITICAL LIMITATION: This is an INCOMPLETE subset for basic Meson support  
# 
# TECHNICAL ISSUE: Fortran module dependencies require ALL 179 source files
# for complete compilation due to complex inter-module dependency graph.
#
# RECOMMENDED SOLUTION: Use FPM (primary build system) which handles
# automatic dependency resolution and complete source file management.
#
# For complete Meson integration, see examples/build_systems/meson/ 
# which demonstrates full 179-file integration for production use.

# Create fortcov library
fortcov_lib = library('fortcov',
  fortcov_sources,
  dependencies : [jsonfortran_dep],
  install : false
)

# Create fortcov executable  
fortcov_exe = executable('fortcov',
  'app/main.f90',
  link_with : fortcov_lib,
  dependencies : [jsonfortran_dep],
  install : true
)

# Coverage target
fortcov = find_program('fortcov', required: false)

run_target('fortcov_coverage',
  command: [
    find_program('bash'), '-c', '''
    echo "FortCov Coverage Analysis"
    echo "========================"
    echo ""
    echo "RECOMMENDED: Use FPM for complete coverage workflow:"
    echo "  fpm test --flag \\"-fprofile-arcs -ftest-coverage\\""
    echo "  gcov src/*.f90"  
    echo "  fortcov --source=src *.gcov"
    echo ""
    echo "Complete Meson integration: examples/build_systems/meson/"
    '''
  ]
)

# Configuration summary  
summary({
  'Primary build system': 'FPM (recommended)',
  'Meson integration': 'Basic support',
  'Coverage enabled': coverage_option,
  'json-fortran found': jsonfortran_dep.found(),
  'Complete examples': 'examples/build_systems/meson/',
}, section: 'FortCov Configuration')

message('')
message('RECOMMENDED WORKFLOW:')
message('  fpm build && fpm test')
message('  Coverage: fpm test --flag "-fprofile-arcs -ftest-coverage"')
message('  Analysis: gcov src/*.f90 && fortcov --source=src *.gcov')
message('')
message('For complete Meson integration see: examples/build_systems/meson/')
