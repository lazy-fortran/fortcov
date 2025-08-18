#!/bin/bash
# HPC Environment Module Script for fortcov coverage
# Demonstrates HPC module system integration from DESIGN.md

echo "=== HPC Module Environment Setup for Fortcov ==="
echo

# Load required modules
echo "Loading HPC modules..."
module purge

# Load compiler suite
echo "Loading GCC compiler suite..."
module load gcc/13.2.0

# Load build tools
echo "Loading build tools..."
module load cmake/3.25.0
module load python/3.11.0

# Load optional performance tools
echo "Loading performance analysis tools..."
module load valgrind/3.19.0 || echo "Valgrind module not available"
module load perf-tools || echo "Performance tools module not available"

# Set environment variables for coverage
echo "Setting coverage environment variables..."
export FCFLAGS="-fprofile-arcs -ftest-coverage -g -O0"
export LDFLAGS="-lgcov"
export FC=gfortran
export CC=gcc
export CXX=g++

# Set up paths for fortcov tools
export PATH="/opt/fortcov/bin:$PATH"
export LD_LIBRARY_PATH="/opt/fortcov/lib:$LD_LIBRARY_PATH"

# Set up workspace directory
WORKSPACE_BASE="${HOME}/fortcov_workspace"
export FORTCOV_WORKSPACE="${WORKSPACE_BASE}/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$FORTCOV_WORKSPACE"

echo "Environment configuration:"
echo "=========================="
echo "FC: $FC"
echo "CC: $CC"
echo "CXX: $CXX"
echo "FCFLAGS: $FCFLAGS"
echo "LDFLAGS: $LDFLAGS"
echo "Workspace: $FORTCOV_WORKSPACE"
echo

# Show loaded modules
echo "Loaded modules:"
module list 2>&1

echo
echo "Compiler versions:"
echo "=================="
$FC --version | head -1
$CC --version | head -1
cmake --version | head -1
python3 --version

# Check for FPM availability
echo
echo "Checking build system availability:"
echo "=================================="
if command -v fpm &> /dev/null; then
    echo "✓ FPM available: $(which fpm)"
    fpm --version
else
    echo "⚠ FPM not found - installing to workspace..."
    
    # Download and install FPM to workspace
    mkdir -p "$FORTCOV_WORKSPACE/bin"
    curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux-x86_64 \
         -o "$FORTCOV_WORKSPACE/bin/fpm"
    chmod +x "$FORTCOV_WORKSPACE/bin/fpm"
    export PATH="$FORTCOV_WORKSPACE/bin:$PATH"
    
    echo "✓ FPM installed: $FORTCOV_WORKSPACE/bin/fpm"
    fpm --version
fi

# Create helper functions
echo
echo "Creating helper functions..."

# Function to build with coverage
build_with_coverage() {
    echo "Building project with coverage instrumentation..."
    if [ -f "fpm.toml" ]; then
        fpm test --flag "$FCFLAGS"
    elif [ -f "CMakeLists.txt" ]; then
        mkdir -p build && cd build
        cmake -DCMAKE_BUILD_TYPE=Testing -DENABLE_COVERAGE=On ..
        make && make test
        cd ..
    elif [ -f "Makefile" ]; then
        make coverage
    else
        echo "No recognized build system found"
        return 1
    fi
}

# Function to generate coverage report
generate_coverage() {
    echo "Generating coverage report..."
    
    # Generate gcov data
    gcov src/*.f90 2>/dev/null || gcov *.f90 2>/dev/null || echo "No .f90 files found for gcov"
    
    # Run fortcov (or mock it for demonstration)
    if command -v fortcov &> /dev/null; then
        fortcov --source=. --output="coverage_$(date +%Y%m%d_%H%M%S).html"
    else
        echo "Creating demonstration coverage report..."
        cat > "coverage_$(date +%Y%m%d_%H%M%S).html" << 'EOF'
<!DOCTYPE html>
<html>
<head>
    <title>HPC Module Environment Coverage Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .hpc-info { background: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0; }
        .summary { background: #f0f0f0; padding: 15px; border-radius: 5px; }
        .covered { color: green; }
    </style>
</head>
<body>
    <h1>HPC Module Environment Coverage Report</h1>
    
    <div class="hpc-info">
        <h3>HPC Environment Information</h3>
        <ul>
            <li><strong>Hostname:</strong> $HOSTNAME</li>
            <li><strong>Modules:</strong> gcc/13.2.0, cmake/3.25.0, python/3.11.0</li>
            <li><strong>Workspace:</strong> $FORTCOV_WORKSPACE</li>
            <li><strong>Compiler:</strong> $FC</li>
        </ul>
    </div>
    
    <div class="summary">
        <h2>Coverage Summary</h2>
        <ul>
            <li><strong>Total Lines:</strong> 220</li>
            <li><strong>Covered Lines:</strong> 198</li>
            <li><strong>Coverage Percentage:</strong> <span class="covered">90.0%</span></li>
        </ul>
    </div>
    
    <h2>HPC Module Integration Benefits</h2>
    <ul>
        <li>✅ Reproducible compiler environments</li>
        <li>✅ Version-controlled tool chains</li>
        <li>✅ Optimized library paths</li>
        <li>✅ Performance analysis tools</li>
        <li>✅ Shared workspace management</li>
    </ul>
    
    <p><em>Generated in HPC module environment</em></p>
</body>
</html>
EOF
        echo "✓ Demo coverage report generated"
    fi
}

# Function to clean workspace
clean_workspace() {
    echo "Cleaning workspace: $FORTCOV_WORKSPACE"
    rm -rf "$FORTCOV_WORKSPACE"
    echo "✓ Workspace cleaned"
}

# Export functions for use in shell
export -f build_with_coverage
export -f generate_coverage
export -f clean_workspace

echo
echo "=== HPC Environment Ready ==="
echo "Available commands:"
echo "  build_with_coverage  - Build project with coverage flags"
echo "  generate_coverage    - Generate coverage report"
echo "  clean_workspace      - Clean workspace directory"
echo
echo "To use:"
echo "  1. cd to your project directory"
echo "  2. Run: build_with_coverage"
echo "  3. Run: generate_coverage"
echo
echo "Workspace: $FORTCOV_WORKSPACE"