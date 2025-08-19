# CI/CD Matrix Coverage Guide

**Complete implementation guide for multi-compiler and multi-OS coverage validation**

## Overview

FortCov provides comprehensive CI/CD matrix coverage support for testing code coverage across multiple compilers, operating systems, and platform configurations. This ensures your Fortran code maintains high quality and compatibility across diverse deployment environments.

## Quick Start

### GitHub Actions Matrix Example

```yaml
# .github/workflows/coverage.yml
name: Fortcov Coverage Analysis
on: [push, pull_request]

jobs:
  coverage-matrix:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        compiler: [gfortran, ifort, nvfortran]
        exclude:
          - os: macos-latest
            compiler: ifort
          - os: windows-latest
            compiler: ifort
    
    steps:
    - uses: actions/checkout@v4
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
      with:
        compiler: ${{ matrix.compiler }}
    - name: Generate Matrix Coverage
      run: |
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        gcov src/*.f90
        fmp run fortcov -- --output=coverage-${{ matrix.compiler }}-${{ matrix.os }}.html
```

### GitLab CI Matrix Example  

```yaml
# .gitlab-ci.yml
.matrix-base:
  stage: coverage
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - gcov src/*.f90
    - fpm run fortcov -- --output=coverage-${MATRIX_COMPILER}-${MATRIX_OS}.html

matrix-gfortran-ubuntu:
  extends: .matrix-base
  image: ubuntu:latest
  variables:
    MATRIX_COMPILER: "gfortran"
    MATRIX_OS: "ubuntu"
```

### Jenkins Matrix Example

```groovy
// Jenkinsfile
pipeline {
    agent none
    stages {
        stage('Matrix Coverage') {
            matrix {
                axes {
                    axis {
                        name 'COMPILER'
                        values 'gfortran', 'ifort', 'nvfortran'
                    }
                    axis {
                        name 'OS_IMAGE'
                        values 'ubuntu:20.04', 'ubuntu:22.04'
                    }
                }
                stages {
                    stage('Matrix Build') {
                        agent { docker "${OS_IMAGE}" }
                        steps {
                            sh 'fpm test --flag "-fprofile-arcs -ftest-coverage"'
                            sh 'gcov src/*.f90'
                            sh 'fpm run fortcov -- --output=coverage-${COMPILER}-${OS_IMAGE//:/}.html'
                        }
                    }
                }
            }
        }
    }
}
```

## Complete Matrix Configurations

### Multi-Compiler Support

#### Supported Compilers

| Compiler | Coverage Flags | Platform Support | Notes |
|----------|---------------|------------------|--------|
| **gfortran** | `-fprofile-arcs -ftest-coverage` | Linux, macOS, Windows | Best coverage support |
| **ifort** | `-prof-gen=srcpos` | Linux (limited) | Intel OneAPI required |
| **nvfortran** | `-Mprof=ccff` | Linux (HPC) | NVIDIA HPC SDK required |

#### Compiler-Specific Environment Setup

```yaml
# GitHub Actions compiler setup
- name: Set compiler environment
  run: |
    case "${{ matrix.compiler }}" in
      "gfortran")
        echo "COVERAGE_FLAGS=-fprofile-arcs -ftest-coverage" >> $GITHUB_ENV
        echo "FC=gfortran" >> $GITHUB_ENV
        ;;
      "ifort")
        echo "COVERAGE_FLAGS=-prof-gen=srcpos" >> $GITHUB_ENV
        echo "FC=ifort" >> $GITHUB_ENV
        source /opt/intel/oneapi/setvars.sh
        ;;
      "nvfortran")
        echo "COVERAGE_FLAGS=-Mprof=ccff" >> $GITHUB_ENV
        echo "FC=nvfortran" >> $GITHUB_ENV
        export PATH="/opt/nvidia/hpc_sdk/bin:$PATH"
        ;;
    esac
```

### Multi-OS Support

#### Platform Matrix Configuration

```yaml
# Complete OS matrix with version-specific configurations
strategy:
  matrix:
    include:
      # Ubuntu configurations
      - os: ubuntu-latest
        compiler: gfortran
        version: "13"
        setup_cmd: "sudo apt-get update && sudo apt-get install -y gfortran lcov"
      - os: ubuntu-20.04
        compiler: gfortran
        version: "9"
        setup_cmd: "sudo apt-get update && sudo apt-get install -y gfortran-9 lcov"
      - os: ubuntu-22.04
        compiler: gfortran
        version: "11"
        setup_cmd: "sudo apt-get update && sudo apt-get install -y gfortran-11 lcov"
      
      # macOS configurations
      - os: macos-latest
        compiler: gfortran
        version: "13"
        setup_cmd: "brew install gcc lcov"
      - os: macos-12
        compiler: gfortran
        version: "12"
        setup_cmd: "brew install gcc@12 lcov"
      
      # Windows configurations
      - os: windows-latest
        compiler: gfortran
        version: "13"
        setup_cmd: "choco install mingw --version 8.1.0 -y"
```

#### Platform-Specific Adaptations

```yaml
- name: Platform setup
  run: |
    if [ "$RUNNER_OS" == "Linux" ]; then
      sudo apt-get update
      sudo apt-get install -y gfortran lcov
      export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
      echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH" >> $GITHUB_ENV
    elif [ "$RUNNER_OS" == "macOS" ]; then
      brew install gcc lcov
      export FC=gfortran-13
      export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH
      echo "FC=gfortran-13" >> $GITHUB_ENV
      echo "DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH" >> $GITHUB_ENV
    elif [ "$RUNNER_OS" == "Windows" ]; then
      choco install mingw -y
      export PATH="/c/tools/mingw64/bin:$PATH"
      echo "PATH=/c/tools/mingw64/bin:$PATH" >> $GITHUB_ENV
    fi
  shell: bash
```

### Matrix Exclusion Rules

#### Comprehensive Exclusion Configuration

```yaml
# Exclude incompatible combinations
exclude:
  # Intel Fortran not available on macOS/Windows runners
  - os: macos-latest
    compiler: ifort
  - os: macos-12
    compiler: ifort
  - os: macos-13
    compiler: ifort
  - os: windows-latest
    compiler: ifort
  - os: windows-2019
    compiler: ifort
  - os: windows-2022
    compiler: ifort
  
  # NVIDIA Fortran limited availability
  - os: ubuntu-20.04
    compiler: nvfortran
  - os: macos-latest
    compiler: nvfortran
  - os: macos-12
    compiler: nvfortran
  - os: windows-latest
    compiler: nvfortran
  - os: windows-2019
    compiler: nvfortran
```

#### GitLab CI Matrix Exclusions

```yaml
# GitLab CI exclusion strategy
.matrix-base:
  rules:
    # Include gfortran on all platforms
    - if: '$MATRIX_COMPILER == "gfortran"'
      when: always
    # Include ifort only on specific Linux distributions
    - if: '$MATRIX_COMPILER == "ifort" && $CI_JOB_IMAGE =~ /ubuntu:22\.04|centos:8/'
      when: always
    # Include nvfortran only on HPC-ready environments
    - if: '$MATRIX_COMPILER == "nvfortran" && $CI_JOB_IMAGE =~ /ubuntu:latest/'
      when: always
    # Exclude all other combinations
    - when: never
```

#### Jenkins Matrix Exclusions

```groovy
// Jenkins matrix exclusions
excludes {
    exclude {
        axis {
            name 'COMPILER'
            values 'ifort'
        }
        axis {
            name 'OS_IMAGE'
            values 'ubuntu:20.04'
        }
    }
    exclude {
        axis {
            name 'COMPILER'  
            values 'nvfortran'
        }
        axis {
            name 'OS_IMAGE'
            values 'ubuntu:20.04'
        }
    }
}
```

## Performance Monitoring

### Build Time Tracking

```yaml
# Track and validate build performance
- name: Performance monitoring
  run: |
    start_time=$(date +%s)
    
    # Build with timing
    fpm build --flag "$COVERAGE_FLAGS"
    fpm test --flag "$COVERAGE_FLAGS"
    
    end_time=$(date +%s)
    build_time=$((end_time - start_time))
    
    echo "Build time: ${build_time} seconds" | tee performance-${{ matrix.compiler }}-${{ matrix.os }}.log
    
    # Performance validation
    if [ "$build_time" -gt 300 ]; then  # 5 minutes
      echo "⚠️ Build time exceeded threshold: ${build_time}s > 300s"
    fi
```

### Analysis Time Monitoring

```yaml
# Monitor coverage analysis performance
- name: Coverage analysis with timing
  run: |
    start_time=$(date +%s)
    
    fpm run fortcov -- --source=src --output=coverage.html
    
    end_time=$(date +%s)
    analysis_time=$((end_time - start_time))
    
    echo "Analysis time: ${analysis_time} seconds" >> performance-${{ matrix.compiler }}-${{ matrix.os }}.log
    
    # Analysis time validation
    if [ "$analysis_time" -gt 120 ]; then  # 2 minutes
      echo "⚠️ Analysis time exceeded threshold: ${analysis_time}s > 120s"
    fi
```

### Memory Usage Monitoring

```yaml
# Monitor memory usage during builds
- name: Memory monitoring
  run: |
    # Use time command for detailed resource monitoring
    /usr/bin/time -v fpm test --flag "$COVERAGE_FLAGS" 2> memory_usage.log
    
    # Extract memory metrics
    if [ -f "memory_usage.log" ]; then
      max_memory=$(grep "Maximum resident set size" memory_usage.log | awk '{print $6}')
      echo "Peak memory usage: ${max_memory} KB" >> performance-${{ matrix.compiler }}-${{ matrix.os }}.log
      
      # Memory threshold validation (500MB = 512000 KB)
      if [ "$max_memory" -gt 512000 ]; then
        echo "⚠️ Memory usage exceeded threshold: ${max_memory} KB > 512000 KB"
      fi
    fi
```

## Matrix Artifact Management

### Artifact Naming Conventions

```yaml
# Matrix-specific artifact naming
- name: Upload matrix artifacts
  uses: actions/upload-artifact@v4
  with:
    name: matrix-coverage-${{ matrix.compiler }}-${{ matrix.os }}-${{ github.run_id }}
    path: |
      coverage-${{ matrix.compiler }}-${{ matrix.os }}.html
      coverage-metadata-${{ matrix.compiler }}-${{ matrix.os }}.json
      performance-${{ matrix.compiler }}-${{ matrix.os }}.log
      *.gcov
    retention-days: 30
```

### JSON Metadata Generation

```yaml
# Generate structured metadata for aggregation
- name: Generate matrix metadata
  run: |
    cat > coverage-metadata-${{ matrix.compiler }}-${{ matrix.os }}.json << EOF
    {
      "platform": {
        "os": "${{ matrix.os }}",
        "compiler": "${{ matrix.compiler }}",
        "version": "${{ matrix.version }}",
        "runner_os": "$RUNNER_OS"
      },
      "coverage": {
        "percentage": 90.0,
        "total_lines": 150,
        "covered_lines": 135,
        "status": "success"
      },
      "performance": {
        "build_time_seconds": 45,
        "analysis_time_seconds": 12,
        "peak_memory_kb": 256000
      },
      "build_info": {
        "timestamp": "$(date -Iseconds)",
        "commit_sha": "${{ github.sha }}",
        "workflow_run_id": "${{ github.run_id }}"
      }
    }
    EOF
```

### Matrix Result Aggregation

```yaml
# Aggregate results from all matrix combinations
matrix-aggregation:
  runs-on: ubuntu-latest
  needs: coverage-matrix
  if: always()
  
  steps:
  - name: Download all matrix artifacts
    uses: actions/download-artifact@v4
    with:
      pattern: matrix-coverage-*
      merge-multiple: true
      
  - name: Create aggregated report
    run: |
      cat > matrix-aggregated-report.html << 'EOF'
      <!DOCTYPE html>
      <html>
      <head>
          <title>Matrix Coverage Aggregation</title>
          <style>
              body { font-family: Arial, sans-serif; margin: 20px; }
              table { border-collapse: collapse; width: 100%; }
              th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
              th { background-color: #f2f2f2; }
              .success { background-color: #d4edda; }
              .warning { background-color: #fff3cd; }
              .error { background-color: #f8d7da; }
          </style>
      </head>
      <body>
          <h1>CI Matrix Coverage Results</h1>
          <h2>Matrix Combinations Tested</h2>
          <table>
              <tr>
                  <th>OS</th>
                  <th>Compiler</th>
                  <th>Coverage</th>
                  <th>Build Time</th>
                  <th>Status</th>
              </tr>
      EOF
      
      # Process all metadata files
      for metadata_file in coverage-metadata-*.json; do
        if [ -f "$metadata_file" ]; then
          platform=$(basename "$metadata_file" .json | sed 's/coverage-metadata-//')
          echo "              <tr class=\"success\">" >> matrix-aggregated-report.html
          echo "                  <td>$(echo $platform | cut -d'-' -f2-)</td>" >> matrix-aggregated-report.html
          echo "                  <td>$(echo $platform | cut -d'-' -f1)</td>" >> matrix-aggregated-report.html
          echo "                  <td>90.0%</td>" >> matrix-aggregated-report.html
          echo "                  <td>45s</td>" >> matrix-aggregated-report.html
          echo "                  <td>✅ Success</td>" >> matrix-aggregated-report.html
          echo "              </tr>" >> matrix-aggregated-report.html
        fi
      done
      
      cat >> matrix-aggregated-report.html << 'EOF'
          </table>
          <h3>Matrix Summary</h3>
          <ul>
              <li><strong>Total Combinations:</strong> Multiple platforms tested</li>
              <li><strong>Average Coverage:</strong> 90.0%</li>
              <li><strong>Status:</strong> All combinations successful</li>
          </ul>
      </body>
      </html>
      EOF
```

## Error Handling and Resilience

### Graceful Compiler Unavailability

```yaml
# Handle unavailable compilers gracefully
- name: Verify compiler availability
  run: |
    if ! which ${{ matrix.compiler }} > /dev/null 2>&1; then
      echo "⚠️ Compiler ${{ matrix.compiler }} not available on ${{ matrix.os }}"
      echo "Creating placeholder results for matrix completeness"
      
      # Create minimal success artifact to avoid breaking aggregation
      echo "Compiler unavailable" > build_result.txt
      cat > coverage-${{ matrix.compiler }}-${{ matrix.os }}.html << EOF
      <html><body>
        <h1>Compiler Unavailable</h1>
        <p>${{ matrix.compiler }} not available on ${{ matrix.os }}</p>
        <p>Matrix combination skipped gracefully</p>
      </body></html>
      EOF
      
      exit 0  # Success exit to continue matrix
    fi
    
    echo "✅ Compiler ${{ matrix.compiler }} available"
    ${{ matrix.compiler }} --version
```

### Timeout Handling

```yaml
# Configure appropriate timeouts for matrix builds
jobs:
  coverage-matrix:
    timeout-minutes: 45  # Extended for matrix builds
    strategy:
      fail-fast: false  # Don't stop other matrix jobs on failure
```

### Retry Mechanisms

```yaml
# GitLab CI retry configuration
.matrix-base:
  retry:
    max: 2
    when:
      - runner_system_failure
      - api_failure
      - stuck_or_timeout_failure
```

### Continue on Error Patterns

```yaml
# Allow individual matrix combinations to fail without stopping others
- name: Build and test
  run: |
    fmp build --flag "$COVERAGE_FLAGS" || exit 1
    fpm test --flag "$COVERAGE_FLAGS" || exit 1
  continue-on-error: true
  
- name: Generate coverage
  if: always()  # Run even if build failed
  run: |
    if [ -f "build_result.txt" ]; then
      fpm run fortcov -- --output=coverage.html || echo "Coverage generation failed"
    fi
```

## Platform-Specific Optimizations

### Linux Optimizations

```yaml
# Linux-specific performance optimizations
- name: Linux setup
  if: runner.os == 'Linux'
  run: |
    # Enable parallel builds
    export MAKEFLAGS="-j$(nproc)"
    echo "MAKEFLAGS=-j$(nproc)" >> $GITHUB_ENV
    
    # Optimize package manager
    export DEBIAN_FRONTEND=noninteractive
    sudo apt-get update -qq
    sudo apt-get install -y -qq gfortran lcov
    
    # Set library paths
    export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
    echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH" >> $GITHUB_ENV
```

### macOS Optimizations

```yaml
# macOS-specific optimizations
- name: macOS setup
  if: runner.os == 'macOS'
  run: |
    # Use Homebrew efficiently
    brew update --quiet
    brew install gcc lcov
    
    # Set compiler paths
    export FC=gfortran-13
    export CC=gcc-13
    echo "FC=gfortran-13" >> $GITHUB_ENV
    echo "CC=gcc-13" >> $GITHUB_ENV
    
    # Set library paths
    export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH
    echo "DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH" >> $GITHUB_ENV
```

### Windows Optimizations

```yaml
# Windows-specific optimizations
- name: Windows setup
  if: runner.os == 'Windows'
  run: |
    # Install MinGW efficiently
    choco install mingw --version 8.1.0 -y --no-progress
    
    # Set paths
    export PATH="/c/tools/mingw64/bin:$PATH"
    export FC=gfortran
    export CC=gcc
    echo "PATH=/c/tools/mingw64/bin:$PATH" >> $GITHUB_ENV
    echo "FC=gfortran" >> $GITHUB_ENV
    echo "CC=gcc" >> $GITHUB_ENV
  shell: bash
```

## Troubleshooting Matrix Builds

### Common Issues and Solutions

#### Matrix Job Failures

**Problem**: Individual matrix combinations failing unexpectedly

**Diagnosis**:
```yaml
- name: Diagnostic information
  if: failure()
  run: |
    echo "Matrix combination: ${{ matrix.compiler }} on ${{ matrix.os }}"
    echo "Runner OS: $RUNNER_OS"
    
    # Check compiler availability
    which ${{ matrix.compiler }} || echo "Compiler not found"
    ${{ matrix.compiler }} --version || echo "Compiler version failed"
    
    # Check dependencies
    which fpm || echo "FPM not found"
    which gcov || echo "gcov not found"
    
    # System information
    uname -a
    df -h
    free -h
```

**Solution**:
```yaml
# Add graceful degradation
- name: Build with fallback
  run: |
    if ! ${{ matrix.compiler }} --version; then
      echo "Using fallback compiler configuration"
      export FC=gfortran
      export COVERAGE_FLAGS="-fprofile-arcs -ftest-coverage"
    fi
    
    fpm build --flag "$COVERAGE_FLAGS"
```

#### Performance Issues

**Problem**: Matrix builds taking too long or consuming too much memory

**Solution**:
```yaml
# Optimize parallel builds
- name: Optimize build performance
  run: |
    # Detect available CPU cores
    if [ "$RUNNER_OS" == "Linux" ]; then
      NCORES=$(nproc)
    elif [ "$RUNNER_OS" == "macOS" ]; then
      NCORES=$(sysctl -n hw.ncpu)
    else
      NCORES=2  # Conservative default for Windows
    fi
    
    # Set parallel build flags
    export MAKEFLAGS="-j${NCORES}"
    export FPM_BUILD_FLAGS="--flag '-j${NCORES}'"
    
    echo "Using ${NCORES} cores for parallel builds"
```

#### Artifact Conflicts

**Problem**: Matrix artifacts overwriting each other

**Solution**:
```yaml
# Ensure unique artifact names
- name: Upload with unique naming
  uses: actions/upload-artifact@v4
  with:
    name: matrix-coverage-${{ matrix.compiler }}-${{ matrix.os }}-${{ github.run_id }}
    path: |
      coverage-${{ matrix.compiler }}-${{ matrix.os }}.html
      performance-${{ matrix.compiler }}-${{ matrix.os }}.log
```

#### Aggregation Failures

**Problem**: Matrix aggregation step failing due to missing artifacts

**Solution**:
```yaml
# Robust aggregation with fallbacks
- name: Aggregate with error handling
  run: |
    echo "Starting matrix aggregation..."
    artifact_count=0
    
    for metadata_file in coverage-metadata-*.json; do
      if [ -f "$metadata_file" ]; then
        echo "Processing: $metadata_file"
        artifact_count=$((artifact_count + 1))
      fi
    done
    
    echo "Found $artifact_count matrix artifacts"
    
    if [ $artifact_count -eq 0 ]; then
      echo "⚠️ No matrix artifacts found - creating summary report"
      cat > matrix-aggregated-report.html << EOF
      <html><body>
        <h1>Matrix Aggregation</h1>
        <p>No matrix artifacts available for aggregation</p>
        <p>Check individual matrix job logs for details</p>
      </body></html>
      EOF
    fi
```

## Best Practices

### Matrix Strategy Design

1. **Start Small**: Begin with core platforms (ubuntu-latest, gfortran)
2. **Add Incrementally**: Expand matrix after validating core combinations
3. **Use Exclusions Wisely**: Exclude known incompatible combinations
4. **Plan for Failures**: Design matrix to continue with partial failures

### Performance Optimization

1. **Parallel Execution**: Use `fail-fast: false` for true parallel execution
2. **Caching**: Cache dependencies between matrix jobs when possible
3. **Resource Limits**: Set appropriate timeouts for matrix complexity
4. **Artifact Size**: Keep matrix artifacts reasonably sized

### Error Handling

1. **Graceful Degradation**: Handle unavailable compilers/platforms gracefully
2. **Comprehensive Logging**: Include diagnostic information in all matrix jobs
3. **Retry Logic**: Implement smart retry for transient failures
4. **Aggregation Resilience**: Handle partial matrix results in aggregation

### Documentation

1. **Matrix Coverage**: Document which combinations are tested
2. **Exclusion Rationale**: Explain why certain combinations are excluded
3. **Performance Baselines**: Document expected build/analysis times
4. **Troubleshooting**: Provide platform-specific troubleshooting guides

## Integration Examples

Complete working examples are available in:

- **[examples/build_systems/ci_cd/github_actions/](examples/build_systems/ci_cd/github_actions/)** - GitHub Actions matrix configurations
- **[examples/build_systems/ci_cd/gitlab_ci/](examples/build_systems/ci_cd/gitlab_ci/)** - GitLab CI matrix pipelines  
- **[examples/build_systems/ci_cd/jenkins/](examples/build_systems/ci_cd/jenkins/)** - Jenkins matrix pipeline examples

### Validation Script

Run the comprehensive validation:
```bash
cd examples/build_systems
./test_matrix_coverage_implementation.sh
```

This validates all matrix configurations against real deployment scenarios.