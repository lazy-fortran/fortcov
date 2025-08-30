# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Automated release pipeline infrastructure
- Semantic versioning compliance
- Release artifact validation

## [0.4.0] - 2024-08-30

### Added
- Memory safety audit and comprehensive fixes (#947, #959)
- Epic 2 architectural discipline enforcement with modular decomposition
- Core directory reorganization with enhanced architectural discipline (#957, #958)
- Comprehensive config directory reorganization with logical subdirectory structure
- Advanced security debt consolidation eliminating critical vulnerabilities (#854, #873)
- Improved EXECUTE_COMMAND_LINE runtime error handling in gcov processing
- CLI validation infrastructure with comprehensive improvements (#846)

### Fixed
- Critical memory allocation bugs causing runtime errors
- Memory safety vulnerabilities in secure file deletion (Issue #244)
- Threshold validation for coverage percentages (Issue #473)
- Shell metacharacter injection vulnerabilities
- Fork bomb prevention validation
- Path traversal attack protection
- Coverage calculation fraud in HTML output (#688, #690)
- Branch coverage mathematical accuracy (Issue #724)
- JSON functionality improvements (Issue #816)
- CLI flag parsing consistency (Issues #228, #231, #472)

### Changed
- Complete Epic 2 architectural decomposition with new utility modules
- Systematic codebase improvement consolidating technical debt (#859)
- Decomposed oversized modules to maintain <500 line architectural limits
- Enhanced build system integration and discovery workflows
- Improved error handling and validation across all modules

### Removed
- Eliminated 9 manual JSON modules (~2000 lines technical debt) (#798)
- Removed duplicate functionality across multiple modules
- Cleaned up unused imports and deprecated functions

## [0.3.0] - 2024-06-15

### Added
- Complete CLI application with comprehensive test suite (Issue #12, #14)
- Configuration module with advanced parsing capabilities (Issue #10, #12)
- Markdown reporter functionality (Issue #9)
- Coverage reporter abstraction layer
- String utilities module (Issue #2)
- Self-coverage functionality MVP (Issue #40, #41)
- XML-to-JSON conversion capabilities
- System test infrastructure for coverage diff validation (Issue #45)

### Fixed
- GitHub Actions CI workflow improvements
- XML-to-JSON conversion test failures (Issue #55, #60)
- CLI documentation mismatches implementing JSON and XML output formats (Issue #85, #92)
- Input validation comprehensive test coverage (Issue #94)
- Coverage statistics module fixes (Issue #131)
- Exit code integration (Issue #177, #186)

### Changed
- Comprehensive cleanup and documentation organization (#54)
- Enhanced user experience with improved documentation (#127, #160)
- Improved input validation patterns for production deployment

## [0.2.0] - 2024-04-20

### Added
- Core coverage analysis engine
- Basic gcov integration
- Initial CLI interface
- File processing capabilities
- Basic test infrastructure

### Fixed
- Core parsing and processing bugs
- Initial gcov compatibility issues
- Basic error handling improvements

## [0.1.0] - 2024-03-15

### Added
- Initial project structure
- Basic Fortran module framework
- FPM build system integration
- Foundation coverage analysis concepts
- Initial development toolchain

---

## Version History Notes

This changelog was created retroactively based on git commit history analysis. All versions prior to the introduction of this changelog represent the evolutionary development of FortCov from initial concept to production-ready coverage analysis tool.

### Versioning Strategy

- **MAJOR** version increments for incompatible API changes
- **MINOR** version increments for backwards-compatible functionality additions
- **PATCH** version increments for backwards-compatible bug fixes

### Release Automation

Starting with version 0.4.1, all releases will be automated through GitHub Actions with:
- Semantic versioning validation
- Automated changelog updates
- Release artifact generation
- Deployment verification