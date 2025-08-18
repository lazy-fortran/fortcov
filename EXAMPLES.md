# FortCov Examples and Tutorials

Practical examples showing how to use FortCov in real-world scenarios, from simple projects to complex enterprise environments.

## Table of Contents

- [Basic Tutorial](#basic-tutorial)
- [Integration Examples](#integration-examples)  
- [Advanced Workflows](#advanced-workflows)
- [Real-World Projects](#real-world-projects)

## Basic Tutorial

### Step 1: Create a Simple Fortran Project

Let's start with a basic calculator module:

```bash
# Create new project
mkdir my-calculator && cd my-calculator

# Initialize fpm project
cat > fpm.toml << 'EOF'
name = "calculator"
version = "0.1.0"
license = "MIT"
author = "Your Name"

[build]
auto-executables = true
EOF

mkdir -p src app test
```

**Create the calculator module** (`src/calculator.f90`):

```fortran
module calculator
    implicit none
    private
    public :: add, subtract, multiply, divide, is_positive
    
contains
    
    function add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function add
    
    function subtract(a, b) result(c)
        real, intent(in) :: a, b  
        real :: c
        c = a - b
    end function subtract
    
    function multiply(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a * b
    end function multiply
    
    function divide(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        
        if (b == 0.0) then
            ! Error handling - this might not be covered initially
            c = huge(0.0)  ! Return large value to indicate error
        else
            c = a / b
        end if
    end function divide
    
    logical function is_positive(x)
        real, intent(in) :: x
        is_positive = x > 0.0
    end function is_positive
    
end module calculator
```

**Create main application** (`app/main.f90`):

```fortran
program main
    use calculator
    implicit none
    
    real :: x, y
    
    x = 10.0
    y = 5.0
    
    print '(A,F0.1,A,F0.1,A,F0.1)', 'Add: ', x, ' + ', y, ' = ', add(x, y)
    print '(A,F0.1,A,F0.1,A,F0.1)', 'Multiply: ', x, ' * ', y, ' = ', multiply(x, y)
    
    if (is_positive(x)) then
        print *, 'x is positive'
    end if
end program main
```

**Create tests** (`test/test_calculator.f90`):

```fortran
program test_calculator
    use calculator
    implicit none
    
    ! Test addition
    if (abs(add(2.0, 3.0) - 5.0) > 1e-6) then
        print *, 'FAIL: add test'
        stop 1
    end if
    
    ! Test subtraction  
    if (abs(subtract(5.0, 3.0) - 2.0) > 1e-6) then
        print *, 'FAIL: subtract test'
        stop 1
    end if
    
    ! Test multiplication
    if (abs(multiply(4.0, 3.0) - 12.0) > 1e-6) then
        print *, 'FAIL: multiply test'
        stop 1
    end if
    
    ! Test positive check
    if (.not. is_positive(5.0)) then
        print *, 'FAIL: is_positive test'
        stop 1
    end if
    
    print *, 'All tests passed!'
end program test_calculator
```

### Step 2: Generate Coverage

```bash
# Build and test with coverage
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate coverage data
gcov src/*.f90

# Create coverage report
fortcov --source=src --output=coverage.md

# View the report
cat coverage.md
```

**Expected output**:

```markdown
# Coverage Report

| Filename | Stmts | Miss | Cover | Missing |
|----------|-------|------|-------|---------|
| src/calculator.f90 | 12 | 2 | 83.33% | 25-26 |
| TOTAL | 12 | 2 | 83.33% | |
```

Notice that the divide function's error handling (lines 25-26) isn't covered.

### Step 3: Improve Coverage

Add a test for the error case:

```fortran
! Add to test/test_calculator.f90

! Test division by zero handling
if (divide(10.0, 0.0) /= huge(0.0)) then
    print *, 'FAIL: divide by zero test'
    stop 1
end if

! Test normal division  
if (abs(divide(10.0, 2.0) - 5.0) > 1e-6) then
    print *, 'FAIL: divide test'
    stop 1
end if
```

Regenerate coverage:

```bash
# Clean and rebuild
rm -f *.gcov *.gcda *.gcno
fpm clean

# Build and test again
fpm build --flag "-fprofile-arcs -ftest-coverage"  
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fortcov --source=src --output=coverage.md

cat coverage.md
```

Now you should see 100% coverage!

### Step 4: Set Up CI Integration

Create `.github/workflows/coverage.yml`:

```yaml
name: Coverage
on: [push, pull_request]

jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Install Fortran
      run: sudo apt update && sudo apt install -y gfortran
      
    - name: Install FPM
      run: |
        curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12 -o /tmp/fpm
        chmod +x /tmp/fpm
        sudo mv /tmp/fpm /usr/local/bin/
        
    - name: Install FortCov
      run: |
        git clone https://github.com/lazy-fortran/fortcov.git /tmp/fortcov
        cd /tmp/fortcov && fpm build --profile release
        sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
        
    - name: Build and test
      run: |
        fpm build --flag "-fprofile-arcs -ftest-coverage"
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        
    - name: Generate coverage
      run: |
        gcov src/*.f90
        fortcov --source=src --output=coverage.md --fail-under=90
        
    - name: Upload coverage
      uses: actions/upload-artifact@v3
      with:
        name: coverage-report
        path: coverage.md
```

## Integration Examples

### Example 1: Multi-Module Scientific Library

**Project structure**:
```
physics-lib/
├── src/
│   ├── constants.f90
│   ├── vectors.f90
│   ├── matrices.f90
│   └── solvers.f90
├── test/
│   ├── test_vectors.f90
│   ├── test_matrices.f90  
│   └── test_solvers.f90
└── examples/
    └── demo.f90
```

**Configuration** (`fortcov.nml`):
```fortran
&fortcov_config
    source_paths = 'src/'
    exclude_patterns = 'examples/*', '*.mod'
    output_format = 'markdown'
    output_path = 'coverage-report.md'
    minimum_coverage = 85.0
    verbose = .false.
/
```

**Makefile integration**:
```makefile
.PHONY: coverage coverage-clean

coverage: coverage-clean
	fpm build --flag "-fprofile-arcs -ftest-coverage"
	fpm test --flag "-fprofile-arcs -ftest-coverage"  
	gcov src/*.f90
	fortcov --config=fortcov.nml

coverage-clean:
	rm -f *.gcov *.gcda *.gcno
	fpm clean

coverage-html: coverage
	fortcov --source=src --output-format=html --output=coverage.html
	@echo "Open coverage.html in your browser"
```

### Example 2: Enterprise Application

**Multi-environment setup**:

```bash
# Development
fortcov --config=configs/dev.nml --verbose

# Staging  
fortcov --config=configs/staging.nml --quiet --fail-under=80

# Production reporting
fortcov --config=configs/prod.nml --output-format=html --output=reports/$(date +%Y-%m-%d)-coverage.html
```

**Development config** (`configs/dev.nml`):
```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*', 'debug/*', 'vendor/*'
    output_format = 'markdown'  
    output_path = 'dev-coverage.md'
    minimum_coverage = 70.0
    verbose = .true.
/
```

**Production config** (`configs/prod.nml`):
```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = '*.mod', 'test/*', 'vendor/*', 'external/*'
    output_format = 'html'
    output_path = 'production-coverage.html'  
    minimum_coverage = 90.0
    quiet = .true.
/
```

### Example 3: Jenkins Pipeline

```groovy
pipeline {
    agent any
    
    stages {
        stage('Build') {
            steps {
                sh 'fpm build --flag "-fprofile-arcs -ftest-coverage"'
            }
        }
        
        stage('Test') {
            steps {
                sh 'fpm test --flag "-fprofile-arcs -ftest-coverage"'
                sh 'gcov src/*.f90'
            }
        }
        
        stage('Coverage') {
            steps {
                script {
                    def coverage = sh(
                        script: 'fortcov --source=src --output-format=json --quiet | jq -r .summary.line_coverage',
                        returnStdout: true
                    ).trim()
                    
                    echo "Coverage: ${coverage}%"
                    
                    if (coverage.toFloat() < 80.0) {
                        error("Coverage ${coverage}% below threshold 80%")
                    }
                }
                
                sh 'fortcov --source=src --output=coverage.md'
                sh 'fortcov --source=src --output-format=html --output=coverage.html'
            }
            
            post {
                always {
                    archiveArtifacts artifacts: 'coverage.*', fingerprint: true
                    publishHTML([
                        allowMissing: false,
                        alwaysLinkToLastBuild: true,
                        keepAll: true,
                        reportDir: '.',
                        reportFiles: 'coverage.html',
                        reportName: 'Coverage Report'
                    ])
                }
            }
        }
    }
}
```

## Advanced Workflows

### Example 1: Coverage Diff Between Branches

```bash
#!/bin/bash
# coverage-diff.sh - Compare coverage between branches

BASELINE_BRANCH=${1:-main}
CURRENT_BRANCH=${2:-$(git branch --show-current)}

echo "Comparing coverage: $BASELINE_BRANCH vs $CURRENT_BRANCH"

# Generate baseline coverage
git checkout $BASELINE_BRANCH
fpm clean
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fortcov --source=src --output-format=json --output=baseline-coverage.json --quiet

# Generate current coverage  
git checkout $CURRENT_BRANCH
fpm clean
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fortcov --source=src --output-format=json --output=current-coverage.json --quiet

# Generate diff report
fortcov --diff=baseline-coverage.json,current-coverage.json --threshold=2.0 --output=coverage-diff.md

echo "Coverage diff report: coverage-diff.md"
cat coverage-diff.md
```

### Example 2: Automated Nightly Reports

```bash
#!/bin/bash
# nightly-coverage.sh

PROJECT_NAME="MyProject"
REPORT_DIR="/var/www/html/coverage-reports"
DATE=$(date +%Y-%m-%d)
TIME=$(date +%H:%M:%S)

# Setup
cd /path/to/project
mkdir -p "$REPORT_DIR/$DATE"

# Clean build
fpm clean
rm -f *.gcov *.gcda *.gcno

# Generate coverage
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90

# Multiple format reports
fortcov --source=src --output="$REPORT_DIR/$DATE/coverage.md" --quiet
fortcov --source=src --output-format=json --output="$REPORT_DIR/$DATE/coverage.json" --quiet  
fortcov --source=src --output-format=html --output="$REPORT_DIR/$DATE/coverage.html" --quiet

# Extract coverage percentage
COVERAGE=$(fortcov --source=src --output-format=json --quiet | jq -r '.summary.line_coverage')

# Update latest symlink
ln -sf "$DATE" "$REPORT_DIR/latest"

# Generate summary page
cat > "$REPORT_DIR/$DATE/index.html" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>$PROJECT_NAME Coverage Report - $DATE</title>
</head>
<body>
    <h1>$PROJECT_NAME Coverage Report</h1>
    <p>Generated: $DATE $TIME</p>
    <p><strong>Overall Coverage: $COVERAGE%</strong></p>
    
    <h2>Reports</h2>
    <ul>
        <li><a href="coverage.html">Interactive HTML Report</a></li>
        <li><a href="coverage.md">Markdown Report</a></li>
        <li><a href="coverage.json">JSON Data</a></li>
    </ul>
</body>
</html>
EOF

# Send notification
if (( $(echo "$COVERAGE < 80" | bc -l) )); then
    # Send alert for low coverage
    echo "Alert: $PROJECT_NAME coverage is $COVERAGE% (below 80%)" | \
        mail -s "Low Coverage Alert - $PROJECT_NAME" team@company.com
else
    echo "✅ $PROJECT_NAME nightly coverage: $COVERAGE%"
fi

# Cleanup old reports (keep 30 days)
find "$REPORT_DIR" -maxdepth 1 -type d -name "20*" -mtime +30 -exec rm -rf {} \;
```

### Example 3: Coverage Monitoring Script

```bash
#!/bin/bash
# monitor-coverage.sh - Track coverage trends

DB_FILE="coverage-history.txt"
THRESHOLD=80
PROJECT_NAME="MyProject"

# Generate current coverage  
CURRENT=$(fortcov --source=src --output-format=json --quiet | jq -r '.summary.line_coverage')
DATE=$(date +%Y-%m-%d)
TIME=$(date +%H:%M:%S)

# Record in history
echo "$DATE,$TIME,$CURRENT" >> "$DB_FILE"

# Get trend (last 5 entries)
RECENT_COVERAGE=$(tail -5 "$DB_FILE" | cut -d, -f3)

echo "=== Coverage Monitoring ==="
echo "Current: $CURRENT%"
echo "Recent history:"
tail -5 "$DB_FILE" | while IFS=, read date time coverage; do
    echo "  $date: $coverage%"
done

# Alert conditions
if (( $(echo "$CURRENT < $THRESHOLD" | bc -l) )); then
    echo "🚨 ALERT: Coverage below threshold ($CURRENT% < $THRESHOLD%)"
    
    # Send Slack notification (example)
    curl -X POST -H 'Content-type: application/json' \
        --data "{\"text\":\"🚨 $PROJECT_NAME Coverage Alert: $CURRENT% (below $THRESHOLD%)\"}" \
        "$SLACK_WEBHOOK_URL"
fi

# Trend analysis
PREV_COVERAGE=$(tail -2 "$DB_FILE" | head -1 | cut -d, -f3)
if [ -n "$PREV_COVERAGE" ]; then
    TREND=$(echo "$CURRENT - $PREV_COVERAGE" | bc -l)
    if (( $(echo "$TREND > 0" | bc -l) )); then
        echo "📈 Trend: +$TREND% (improving)"
    elif (( $(echo "$TREND < -2" | bc -l) )); then
        echo "📉 Trend: $TREND% (significant decline)"
        # Alert for significant drops
        curl -X POST -H 'Content-type: application/json' \
            --data "{\"text\":\"📉 $PROJECT_NAME Coverage Drop: $TREND% decline\"}" \
            "$SLACK_WEBHOOK_URL"
    else
        echo "➡️  Trend: $TREND% (stable)"
    fi
fi
```

## Real-World Projects

### Example 1: Climate Modeling Library

**Project**: Large-scale atmospheric simulation library

```fortran
! fortcov-climate.nml
&fortcov_config
    source_paths = 'src/core/', 
                  'src/physics/',
                  'src/io/', 
                  'src/numerics/'
    exclude_patterns = 'src/external/*',
                      'src/vendor/*', 
                      'test/*',
                      '*.mod',
                      'benchmarks/*'
    output_format = 'html'
    output_path = 'climate-coverage.html'
    minimum_coverage = 75.0
    verbose = .false.
/
```

**Makefile targets**:
```makefile
# Component-wise coverage
coverage-physics:
	@echo "Testing physics modules..."
	fpm build --flag "-fprofile-arcs -ftest-coverage"
	fpm test physics --flag "-fprofile-arcs -ftest-coverage"
	gcov src/physics/*.f90
	fortcov --source=src/physics --output=physics-coverage.md

coverage-numerics:
	@echo "Testing numerical methods..."  
	fpm build --flag "-fprofile-arcs -ftest-coverage"
	fpm test numerics --flag "-fprofile-arcs -ftest-coverage"
	gcov src/numerics/*.f90
	fortcov --source=src/numerics --output=numerics-coverage.md

# Full coverage
coverage-full:
	fpm build --flag "-fprofile-arcs -ftest-coverage"
	fpm test --flag "-fprofile-arcs -ftest-coverage"  
	gcov src/*/*.f90
	fortcov --config=fortcov-climate.nml
```

### Example 2: Financial Trading System

**High-reliability requirements**:

```bash
#!/bin/bash
# trading-system-coverage.sh

# Very strict coverage requirements
CRITICAL_THRESHOLD=95
NORMAL_THRESHOLD=90

# Test critical components with high standards
echo "Testing critical trading components..."

# Risk management (must be 95%+)  
fpm test risk --flag "-fprofile-arcs -ftest-coverage"
gcov src/risk/*.f90
RISK_COVERAGE=$(fortcov --source=src/risk --output-format=json --quiet | jq -r '.summary.line_coverage')

if (( $(echo "$RISK_COVERAGE < $CRITICAL_THRESHOLD" | bc -l) )); then
    echo "🚨 CRITICAL: Risk management coverage $RISK_COVERAGE% < $CRITICAL_THRESHOLD%"
    exit 1
fi

# Order execution (must be 95%+)
fpm test orders --flag "-fprofile-arcs -ftest-coverage"  
gcov src/orders/*.f90
ORDER_COVERAGE=$(fortcov --source=src/orders --output-format=json --quiet | jq -r '.summary.line_coverage')

if (( $(echo "$ORDER_COVERAGE < $CRITICAL_THRESHOLD" | bc -l) )); then
    echo "🚨 CRITICAL: Order execution coverage $ORDER_COVERAGE% < $CRITICAL_THRESHOLD%" 
    exit 1
fi

# Reporting modules (90%+ acceptable)
fpm test reporting --flag "-fprofile-arcs -ftest-coverage"
gcov src/reporting/*.f90  
REPORT_COVERAGE=$(fortcov --source=src/reporting --output-format=json --quiet | jq -r '.summary.line_coverage')

if (( $(echo "$REPORT_COVERAGE < $NORMAL_THRESHOLD" | bc -l) )); then
    echo "⚠️  WARNING: Reporting coverage $REPORT_COVERAGE% < $NORMAL_THRESHOLD%"
fi

# Generate comprehensive report
fortcov --source=src --output-format=html --output=trading-coverage-$(date +%Y%m%d).html

echo "✅ All coverage checks passed"
echo "  Risk Management: $RISK_COVERAGE%"  
echo "  Order Execution: $ORDER_COVERAGE%"
echo "  Reporting: $REPORT_COVERAGE%"
```

### Example 3: Open Source Scientific Tool

**Community-driven development**:

```yaml
# .github/workflows/coverage-pr.yml
name: PR Coverage Check
on:
  pull_request:
    branches: [main, develop]

jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
      with:
        fetch-depth: 0  # Need full history for comparison
        
    - name: Install dependencies
      run: |
        sudo apt update && sudo apt install -y gfortran jq
        curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12 -o /tmp/fpm
        chmod +x /tmp/fpm
        sudo mv /tmp/fpm /usr/local/bin/
        git clone https://github.com/lazy-fortran/fortcov.git /tmp/fortcov
        cd /tmp/fortcov && fpm build --profile release
        sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
        
    - name: Generate baseline coverage
      run: |
        git checkout ${{ github.event.pull_request.base.sha }}
        fpm build --flag "-fprofile-arcs -ftest-coverage"
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        gcov src/*.f90
        fortcov --source=src --output-format=json --output=baseline.json --quiet
        
    - name: Generate PR coverage  
      run: |
        git checkout ${{ github.event.pull_request.head.sha }}
        fpm clean && rm -f *.gcov *.gcda *.gcno
        fpm build --flag "-fprofile-arcs -ftest-coverage" 
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        gcov src/*.f90
        fortcov --source=src --output-format=json --output=current.json --quiet
        fortcov --source=src --output=coverage.md --quiet
        
    - name: Coverage comparison
      id: coverage
      run: |
        # Compare coverage levels
        BASELINE=$(jq -r '.summary.line_coverage' baseline.json)
        CURRENT=$(jq -r '.summary.line_coverage' current.json)
        DIFF=$(echo "$CURRENT - $BASELINE" | bc -l)
        
        echo "baseline=$BASELINE" >> $GITHUB_OUTPUT
        echo "current=$CURRENT" >> $GITHUB_OUTPUT  
        echo "diff=$DIFF" >> $GITHUB_OUTPUT
        
        # Fail if coverage decreases significantly
        if (( $(echo "$DIFF < -2.0" | bc -l) )); then
          echo "❌ Coverage decreased by $DIFF% (from $BASELINE% to $CURRENT%)"
          exit 1
        fi
        
    - name: Comment PR
      uses: actions/github-script@v6
      with:
        script: |
          const fs = require('fs');
          const coverage = fs.readFileSync('coverage.md', 'utf8');
          const baseline = '${{ steps.coverage.outputs.baseline }}';
          const current = '${{ steps.coverage.outputs.current }}';
          const diff = '${{ steps.coverage.outputs.diff }}';
          
          const diffIcon = parseFloat(diff) >= 0 ? '📈' : '📉';
          const summary = `## Coverage Report
          
**Current**: ${current}% | **Baseline**: ${baseline}% | **Change**: ${diffIcon} ${diff}%

${coverage}`;
          
          github.rest.issues.createComment({
            issue_number: context.issue.number,
            owner: context.repo.owner,
            repo: context.repo.repo,
            body: summary
          });
```

These examples demonstrate FortCov usage from basic tutorials to enterprise-grade implementations, showing how coverage analysis scales from simple projects to complex, mission-critical systems.