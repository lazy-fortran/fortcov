# FortCov System Architecture

## System Overview

FortCov is a Fortran code coverage analysis tool providing gcov integration, reporting, and differential analysis. The system follows a modular architecture with strict separation of concerns.

## Core Architecture Principles

- **Security-First Design**: All external inputs validated before processing
- **Memory Safety**: Bounds checking, allocation limits, automatic deallocation
- **Performance**: O(1) validation checks, minimal overhead
- **Robustness**: Graceful degradation on invalid inputs
- **SOLID Principles**: Single responsibility, dependency injection
- **KISS**: Simplest solution that meets requirements

## Module Architecture

### Foundation Layer

**foundation_constants.f90**: Centralized constants and limits
**foundation_layer_utils.f90**: Core decomposition utilities  
**architectural_patterns.f90**: Abstract interfaces and design patterns

### Core Components

**coverage_engine.f90**: Main orchestration (97% size reduction)
**fortcov_config.f90**: Configuration management (92% size reduction)
**json_coverage_io.f90**: I/O operations (94% size reduction)
**coverage_model.f90**: Data structures (92% size reduction)

### Processing Pipeline

1. **Input Validation**: Comprehensive security-first validation
2. **Coverage Discovery**: O(n) file discovery with pattern matching
3. **Data Processing**: Streaming architecture for memory efficiency
4. **Output Generation**: Multiple format support with templating

## Design Patterns

### Modular Decomposition

Large modules decomposed into focused, single-responsibility modules:

- **Before**: coverage_engine.f90 (1,200+ lines)
- **After**: 5 focused modules (40-160 lines each)
- **Result**: 90%+ reduction in module sizes

### Security Architecture

Comprehensive input validation with early failure detection:
- Path traversal protection
- Resource exhaustion prevention
- Memory allocation limits
- Safe error handling

### Performance Optimization

O(n) algorithms throughout:
- Linear file discovery
- Streaming data processing
- Memory pool management
- Pre-allocation strategies

For detailed architectural information, see the complete DESIGN.md documentation.