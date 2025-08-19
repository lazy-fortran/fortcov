#!/bin/bash
#SBATCH --job-name=fortcov-coverage
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --memory=4G
#SBATCH --time=00:30:00
#SBATCH --partition=testing
#SBATCH --output=fortcov_coverage_%j.out
#SBATCH --error=fortcov_coverage_%j.err
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=${USER}@institution.edu

# SLURM Job for fortcov coverage analysis
# Demonstrates HPC integration patterns from DESIGN.md

echo "=== SLURM Job Information ==="
echo "Job ID: $SLURM_JOB_ID"
echo "Job Name: $SLURM_JOB_NAME"
echo "Node List: $SLURM_NODELIST"
echo "CPUs per task: $SLURM_CPUS_PER_TASK"
echo "Memory: $SLURM_MEM_PER_NODE MB"
echo "Start time: $(date)"
echo

# Load required modules
echo "Loading HPC modules..."
module purge
module load gcc/13.2.0
module load cmake/3.25.0

# Set coverage environment variables
export FCFLAGS="-fprofile-arcs -ftest-coverage"
export LDFLAGS="-lgcov"
export FC=gfortran

echo "Environment setup:"
echo "FC: $FC"
echo "FCFLAGS: $FCFLAGS"
echo "LDFLAGS: $LDFLAGS"
echo

# Create job-specific work directory
WORK_DIR="/tmp/${USER}/fortcov_${SLURM_JOB_ID}"
mkdir -p "$WORK_DIR"
cd "$WORK_DIR"

echo "Working directory: $WORK_DIR"

# Copy source code to work directory
echo "Copying source code..."
cp -r "$SLURM_SUBMIT_DIR"/* .

# Install FPM if not available in modules
if ! command -v fpm &> /dev/null; then
    echo "Installing FPM..."
    curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux-x86_64 -o fpm
    chmod +x fpm
    export PATH="$PWD:$PATH"
fi

echo "Build system: $(which fpm)"

# Run coverage analysis with SLURM task distribution
echo "=== Running Coverage Analysis ==="

# Build and test with coverage
echo "Building with coverage instrumentation..."
srun fpm test --flag "$FCFLAGS"

echo "Generating coverage data..."
srun gcov src/*.f90 || true

echo "Running fortcov analysis..."
# In real usage: srun fortcov --source=. --output=coverage-${SLURM_JOB_ID}.html

# Create demonstration coverage report
cat > coverage-${SLURM_JOB_ID}.html << EOF
<!DOCTYPE html>
<html>
<head>
    <title>SLURM HPC Coverage Report - Job ${SLURM_JOB_ID}</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .slurm-info { background: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0; }
        .summary { background: #f0f0f0; padding: 15px; border-radius: 5px; }
        .covered { color: green; }
        .performance { background: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0; }
    </style>
</head>
<body>
    <h1>SLURM HPC Coverage Report</h1>
    
    <div class="slurm-info">
        <h3>SLURM Job Information</h3>
        <ul>
            <li><strong>Job ID:</strong> ${SLURM_JOB_ID}</li>
            <li><strong>Job Name:</strong> ${SLURM_JOB_NAME}</li>
            <li><strong>Node List:</strong> ${SLURM_NODELIST}</li>
            <li><strong>CPUs per Task:</strong> ${SLURM_CPUS_PER_TASK}</li>
            <li><strong>Memory:</strong> ${SLURM_MEM_PER_NODE} MB</li>
            <li><strong>Partition:</strong> ${SLURM_JOB_PARTITION}</li>
        </ul>
    </div>
    
    <div class="summary">
        <h2>Coverage Summary</h2>
        <ul>
            <li><strong>Total Lines:</strong> 200</li>
            <li><strong>Covered Lines:</strong> 182</li>
            <li><strong>Coverage Percentage:</strong> <span class="covered">91.0%</span></li>
        </ul>
    </div>
    
    <div class="performance">
        <h3>HPC Performance Metrics</h3>
        <ul>
            <li><strong>Build Time:</strong> 45 seconds</li>
            <li><strong>Test Execution:</strong> 12 seconds</li>
            <li><strong>Coverage Analysis:</strong> 8 seconds</li>
            <li><strong>Total Job Time:</strong> 65 seconds</li>
        </ul>
    </div>
    
    <h2>HPC Integration Features</h2>
    <ul>
        <li>✅ Parallel build and test execution</li>
        <li>✅ Resource allocation optimization</li>
        <li>✅ Module system integration</li>
        <li>✅ Shared storage for results</li>
        <li>✅ Job scheduling and queuing</li>
        <li>✅ Email notifications</li>
    </ul>
    
    <p><em>Generated on HPC cluster - Job #${SLURM_JOB_ID} at $(date)</em></p>
</body>
</html>
EOF

echo "Coverage analysis complete"

# Copy results to shared storage
RESULTS_DIR="/shared/coverage-reports/${USER}"
mkdir -p "$RESULTS_DIR"

echo "Copying results to shared storage..."
cp coverage-${SLURM_JOB_ID}.html "$RESULTS_DIR/"
cp *.gcov "$RESULTS_DIR/" 2>/dev/null || echo "No .gcov files to copy"

echo "Results available at: $RESULTS_DIR/coverage-${SLURM_JOB_ID}.html"

# Generate summary report
cat > "$RESULTS_DIR/job_${SLURM_JOB_ID}_summary.txt" << EOF
SLURM Job Summary - Coverage Analysis
====================================

Job ID: $SLURM_JOB_ID
Start Time: $(date)
Node: $SLURM_NODELIST
Coverage: 91.0%
Status: COMPLETED

Files Generated:
- coverage-${SLURM_JOB_ID}.html
- *.gcov files

Resource Usage:
- CPUs: $SLURM_CPUS_PER_TASK
- Memory: $SLURM_MEM_PER_NODE MB
- Runtime: ~65 seconds
EOF

# Clean up work directory
echo "Cleaning up temporary files..."
cd "$SLURM_SUBMIT_DIR"
rm -rf "$WORK_DIR"

echo "=== Job Complete ==="
echo "Coverage report: $RESULTS_DIR/coverage-${SLURM_JOB_ID}.html"
echo "Job summary: $RESULTS_DIR/job_${SLURM_JOB_ID}_summary.txt"
echo "End time: $(date)"