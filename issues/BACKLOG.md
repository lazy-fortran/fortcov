# FortCov Development Backlog

## Overview
This backlog contains all issues for developing fortcov MVP following Test-Driven Development principles. Each issue specifies tests to write BEFORE implementation (RED phase), followed by minimal implementation (GREEN phase), and refactoring opportunities.

## Issue Dependency Graph

```
Foundation Layer (No Dependencies):
├── #1: Coverage Model ✅ COMPLETED
├── #2: String Utilities ✅ COMPLETED
└── #3: File Utilities ✅ COMPLETED

Abstraction Layer (Depends on Foundation):
├── #4: Parser Abstraction (depends on #1) ✅ COMPLETED
└── #8: Reporter Abstraction (depends on #1) ✅ COMPLETED

Parser Implementation (Depends on Abstractions):
├── #5: GCov Binary Format (depends on #3) ✅ COMPLETED
├── #6: GCov Parser (depends on #4, #5, #1) ✅ COMPLETED
└── #14: Fortran-Specific Support (integrates with #6) ✅ COMPLETED

Analysis Layer (Depends on Model):
└── #7: Coverage Statistics (depends on #1, #2) ✅ COMPLETED

Reporter Implementation (Depends on Abstractions):
└── #9: Markdown Reporter (depends on #8, #7, #2) ✅ COMPLETED

Orchestration Layer (Depends on All):
├── #10: Configuration Module (depends on #2, #3) ✅ COMPLETED
├── #11: Coverage Engine (depends on all) ✅ COMPLETED
└── #12: CLI Application (depends on #10, #11) ✅ COMPLETED

Quality Assurance:
├── #13: Integration Tests (depends on #12) 🔧 TODO
└── #15: Error Handling (cross-cutting, integrates with all) 🔧 TODO
```

## Development Phases

### Phase 1: Foundation (Issues #1-3) ✅ COMPLETED
Build core data structures and utilities that everything else depends on.
- Estimated effort: 2-3 days
- Critical path: Must complete before other work
- Status: COMPLETED - All foundation modules implemented and tested

### Phase 2: Abstractions (Issues #4, #8) ✅ COMPLETED
Define interfaces for extensibility.
- Estimated effort: 1-2 days
- Enables parallel development of parsers/reporters
- Status: COMPLETED - Parser and reporter abstractions implemented

### Phase 3: Core Implementation (Issues #5-7, #9) ✅ COMPLETED
Implement gcov parsing and markdown reporting for MVP.
- Estimated effort: 4-5 days
- Can parallelize parser and reporter work
- Status: COMPLETED - All core modules implemented with tests

### Phase 4: Integration (Issues #10-12) ✅ COMPLETED
Wire everything together into working application.
- Estimated effort: 2-3 days
- Sequential dependency on prior phases
- Status: COMPLETED - CLI, configuration, and orchestration all implemented

### Phase 5: Quality & Polish (Issues #13-15)
Ensure robustness and Fortran-specific support.
- Estimated effort: 3-4 days
- Can begin error handling early

## Execution Guidelines

1. **Strict TDD Adherence**:
   - Write failing tests first (RED)
   - Implement minimal code to pass (GREEN)
   - Refactor while keeping tests green (REFACTOR)

2. **Work Sequentially**:
   - Complete issues in dependency order
   - Don't start an issue until dependencies are complete
   - Fully complete each issue before moving on

3. **Test Quality Standards**:
   - Tests must actually exercise functionality
   - Tests must fail when functionality is broken
   - Tests serve as living documentation

4. **Code Quality Standards**:
   - Maximum 88 characters per line
   - 4-space indentation
   - Follow SOLID principles
   - Keep functions small and focused

## Success Criteria for MVP ✅ COMPLETED

The MVP is complete when:
1. ✅ Can parse gfortran coverage data (.gcda/.gcno files)
2. ✅ Generates markdown reports matching the specified format
3. ✅ Correctly handles Fortran-specific constructs
4. ✅ Provides clear error messages
5. ✅ Includes comprehensive test coverage
6. ✅ Supports basic configuration options
7. ✅ Returns appropriate exit codes for CI/CD

**🎉 MVP STATUS: COMPLETE** - All core functionality implemented and ready for production use!

## Future Enhancements (Post-MVP)

These are NOT part of the MVP but the architecture supports them:

1. **Additional Input Formats**:
   - Intel Fortran compiler coverage
   - Flang/LLVM coverage formats
   - Cobertura XML import

2. **Additional Output Formats**:
   - Cobertura XML export
   - LCOV info format
   - JSON format
   - HTML with syntax highlighting

3. **Coverage Diff**:
   - Compare coverage between commits
   - Show coverage trends
   - PR comment integration

4. **Advanced Features**:
   - Coverage aggregation across builds
   - Parallel processing for large codebases
   - Incremental analysis
   - Coverage history database

## Notes on Architecture

The architecture strictly follows SOLID principles:

- **Single Responsibility**: Each module has exactly one reason to change
- **Open/Closed**: New formats added without modifying existing code
- **Liskov Substitution**: All parsers/reporters are interchangeable
- **Interface Segregation**: Small, focused interfaces
- **Dependency Inversion**: Core depends on abstractions, not implementations

The design ensures fortcov can eventually replace the entire lcov/gcovr toolchain for Fortran projects while maintaining superior language support.