# FortCov User Guide

This guide helps different types of users get the most out of FortCov, with specific guidance for Fortran developers, DevOps engineers, and project managers.

## Quick Navigation

- [Fortran Developer](#fortran-developer) - Integrate coverage into your development workflow
- [DevOps Engineer](#devops-engineer) - Set up CI/CD and automation  
- [Project Manager](#project-manager) - Understand reports and track progress

---

## Fortran Developer

### Getting Started

```bash
# 1. Add to your existing project
cd your-fortran-project

# 2. Build with coverage
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"

# 3. Generate coverage data  
gcov src/*.f90

# 4. Create coverage report
fortcov --source=src --output=coverage.md
```

### Daily Development Workflow

#### During Development

```bash
# Quick coverage check
fortcov --source=src --quiet && echo "✓ Coverage looks good" || echo "⚠ Coverage issues"

# Detailed analysis
fortcov --source=src --verbose --output=coverage.md
open coverage.md  # Review detailed report
```

#### Before Committing

```bash
# Check coverage meets team standards
fortcov --source=src --fail-under=80 --quiet
if [ $? -eq 0 ]; then
    echo "✓ Coverage above 80%, ready to commit"
else
    echo "⚠ Coverage below 80%, add more tests"
fi
```

#### Interactive Analysis

```bash
# Browse coverage interactively
fortcov --source=src --tui

# Use arrow keys to navigate files
# Press Enter to see line-by-line coverage
# Press 'q' to quit
```

### Understanding Coverage Results

#### Coverage Table

```markdown
| Filename                 | Stmts | Miss | Cover  | Missing     |
|--------------------------|-------|------|--------|-------------|
| src/math_utils.f90       | 45    | 5    | 88.89% | 23-25, 67   |
| src/string_utils.f90     | 32    | 0    | 100.00%|             |
```

**What this means**:
- **Stmts**: Total executable statements
- **Miss**: Number of uncovered statements  
- **Cover**: Percentage covered
- **Missing**: Line numbers not executed

#### Missing Lines

```fortran
! Example: Lines 23-25 not covered
function calculate_square_root(x)
    real, intent(in) :: x
    real :: calculate_square_root
    
    if (x < 0.0) then
        ! Lines 23-25: Error handling not tested
        print *, "Error: negative input"
        calculate_square_root = -1.0
        return
    end if
    
    calculate_square_root = sqrt(x)
end function
```

**Action**: Add test case for negative input to improve coverage.

### Common Development Scenarios

#### Excluding Test Files

```bash
# Don't include test coverage in main reports
fortcov --source=src --exclude='test/*' --exclude='*_test.f90' --output=coverage.md
```

#### Module-Specific Coverage

```bash
# Focus on specific modules
fortcov --source=src/core --output=core_coverage.md
fortcov --source=src/utils --output=utils_coverage.md
```

#### Configuration for Your Project

Create `fortcov.nml`:

```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = 'test/*', '*.mod', 'build/*'
    output_path = 'coverage.md'
    minimum_coverage = 80.0
    verbose = .false.
/
```

Then use:

```bash
fortcov --config=fortcov.nml
```

### Integration with Editors

#### VS Code

Add to `.vscode/tasks.json`:

```json
{
    "label": "Generate Coverage",
    "type": "shell",
    "command": "fpm",
    "args": [
        "build", "--flag", "-fprofile-arcs -ftest-coverage",
        "&&", "fpm", "test", "--flag", "-fprofile-arcs -ftest-coverage", 
        "&&", "gcov", "src/*.f90",
        "&&", "fortcov", "--source=src", "--output=coverage.md"
    ],
    "group": "build"
}
```

#### Vim/Neovim

Add to your `.vimrc`:

```vim
" Generate coverage report
nnoremap <leader>cov :!fortcov --source=src --output=coverage.md && echo "Coverage report generated"<CR>
```

---

## DevOps Engineer

### CI/CD Integration

#### GitHub Actions (Complete)

Create `.github/workflows/coverage.yml`:

```yaml
name: Coverage Analysis
on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  coverage:
    runs-on: ubuntu-latest
    
    steps:
    - name: Checkout code
      uses: actions/checkout@v3
      
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
      with:
        compiler: gcc
        version: 11
        
    - name: Setup FPM
      uses: fortran-lang/setup-fpm@v1
      
    - name: Install FortCov
      run: |
        git clone https://github.com/lazy-fortran/fortcov.git /tmp/fortcov
        cd /tmp/fortcov
        fpm build --profile release
        sudo ln -s $(pwd)/build/gfortran_*/app/fortcov /usr/local/bin/fortcov
        
    - name: Build with coverage
      run: |
        fpm build --flag "-fprofile-arcs -ftest-coverage"
        fpm test --flag "-fprofile-arcs -ftest-coverage"
        
    - name: Generate coverage data
      run: gcov src/*.f90
      
    - name: Create coverage report
      run: |
        fortcov --source=src --output=coverage.md --fail-under=80 --quiet
        
    - name: Upload coverage report
      uses: actions/upload-artifact@v3
      with:
        name: coverage-report
        path: coverage.md
        
    - name: Comment PR with coverage
      if: github.event_name == 'pull_request'
      uses: actions/github-script@v6
      with:
        script: |
          const fs = require('fs');
          const coverage = fs.readFileSync('coverage.md', 'utf8');
          github.rest.issues.createComment({
            issue_number: context.issue.number,
            owner: context.repo.owner,
            repo: context.repo.repo,
            body: `## Coverage Report\n\n${coverage}`
          });
```

#### GitLab CI (Production Ready)

Create `.gitlab-ci.yml`:

```yaml
stages:
  - build
  - test
  - coverage

variables:
  FF_USE_FASTZIP: "true"
  CACHE_COMPRESSION_LEVEL: "fastest"

build:
  stage: build
  image: gcc:11
  before_script:
    - apt-get update && apt-get install -y wget
    - wget https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12
    - chmod +x fpm-0.12.0-linux-x86_64-gcc-12 && mv fpm-0.12.0-linux-x86_64-gcc-12 /usr/local/bin/fpm
  script:
    - fpm build --flag "-fprofile-arcs -ftest-coverage"
  artifacts:
    paths:
      - build/
    expire_in: 1 hour

test:
  stage: test
  image: gcc:11
  dependencies:
    - build
  script:
    - fpm test --flag "-fprofile-arcs -ftest-coverage"
    - gcov src/*.f90
  artifacts:
    paths:
      - "*.gcov"
    expire_in: 1 hour

coverage:
  stage: coverage
  image: gcc:11
  dependencies:
    - test
  before_script:
    - git clone https://github.com/lazy-fortran/fortcov.git /tmp/fortcov
    - cd /tmp/fortcov && fpm build --profile release
    - cp build/gfortran_*/app/fortcov /usr/local/bin/
  script:
    - fortcov --source=src --output=coverage.md --fail-under=75 --quiet
    - fortcov --source=src --output-format=json --output=coverage.json --quiet
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml
    paths:
      - coverage.md
      - coverage.json
    expire_in: 30 days
  coverage: '/TOTAL.*?(\d+\.\d+)%/'
```

#### Jenkins Pipeline

Create `Jenkinsfile`:

```groovy
pipeline {
    agent any
    
    environment {
        FORTCOV_PATH = "/usr/local/bin/fortcov"
    }
    
    stages {
        stage('Setup') {
            steps {
                sh '''
                    if [ ! -f ${FORTCOV_PATH} ]; then
                        git clone https://github.com/lazy-fortran/fortcov.git /tmp/fortcov
                        cd /tmp/fortcov
                        fpm build --profile release
                        sudo cp build/gfortran_*/app/fortcov ${FORTCOV_PATH}
                    fi
                '''
            }
        }
        
        stage('Build and Test') {
            steps {
                sh '''
                    fpm build --flag "-fprofile-arcs -ftest-coverage"
                    fpm test --flag "-fprofile-arcs -ftest-coverage"
                    gcov src/*.f90
                '''
            }
        }
        
        stage('Coverage Analysis') {
            steps {
                sh '''
                    ${FORTCOV_PATH} --source=src --output=coverage.md --fail-under=80 --quiet
                    ${FORTCOV_PATH} --source=src --output-format=json --output=coverage.json --quiet
                '''
            }
            post {
                always {
                    archiveArtifacts artifacts: 'coverage.md,coverage.json', fingerprint: true
                    publishHTML([
                        allowMissing: false,
                        alwaysLinkToLastBuild: true,
                        keepAll: true,
                        reportDir: '.',
                        reportFiles: 'coverage.md',
                        reportName: 'Coverage Report'
                    ])
                }
            }
        }
    }
    
    post {
        failure {
            emailext (
                subject: "Coverage Failed: ${env.JOB_NAME} - ${env.BUILD_NUMBER}",
                body: "Coverage analysis failed. Check console output: ${env.BUILD_URL}",
                recipientProviders: [developers()]
            )
        }
    }
}
```

### Automation Scripts

#### Nightly Coverage Reports

Create `scripts/nightly-coverage.sh`:

```bash
#!/bin/bash

# Nightly coverage report automation
set -e

PROJECT_DIR="/path/to/your/project"
REPORT_DIR="/var/www/html/coverage"
DATE=$(date +%Y-%m-%d)

cd "$PROJECT_DIR"

# Clean and rebuild
fpm clean
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate coverage data
gcov src/*.f90

# Create reports
mkdir -p "$REPORT_DIR/$DATE"

# Multiple format reports
fortcov --source=src --output="$REPORT_DIR/$DATE/coverage.md" --quiet
fortcov --source=src --output-format=json --output="$REPORT_DIR/$DATE/coverage.json" --quiet
fortcov --source=src --output-format=html --output="$REPORT_DIR/$DATE/coverage.html" --quiet

# Update latest symlink  
ln -sf "$DATE" "$REPORT_DIR/latest"

# Send notification
if [ $? -eq 0 ]; then
    echo "✅ Nightly coverage report generated: http://reports.company.com/coverage/latest/coverage.html"
else
    echo "❌ Coverage report generation failed" >&2
    exit 1
fi
```

Add to cron:

```bash
# Run nightly coverage at 2 AM
0 2 * * * /path/to/scripts/nightly-coverage.sh
```

#### Coverage Monitoring

Create `scripts/coverage-monitor.sh`:

```bash
#!/bin/bash

# Monitor coverage trends
THRESHOLD=80
CURRENT_COVERAGE=$(fortcov --source=src --output-format=json --quiet | jq -r '.summary.line_coverage')

if (( $(echo "$CURRENT_COVERAGE < $THRESHOLD" | bc -l) )); then
    # Alert team
    curl -X POST -H 'Content-type: application/json' \
        --data "{\"text\":\"⚠️ Coverage Alert: $CURRENT_COVERAGE% (below $THRESHOLD% threshold)\"}" \
        YOUR_SLACK_WEBHOOK_URL
fi
```

### Configuration Management

#### Environment-Specific Configs

**Development** (`configs/dev.nml`):

```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = 'test/*', '*.mod', 'debug/*'
    output_format = 'markdown'
    minimum_coverage = 70.0
    verbose = .true.
/
```

**Staging** (`configs/staging.nml`):

```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = 'test/*', '*.mod', 'build/*', 'external/*'
    output_format = 'json'
    minimum_coverage = 80.0
    quiet = .true.
/
```

**Production** (`configs/prod.nml`):

```fortran
&fortcov_config
    source_paths = 'src/', 'lib/'
    exclude_patterns = 'test/*', '*.mod', 'build/*', 'external/*', 'vendor/*'
    output_format = 'html'
    minimum_coverage = 85.0
    quiet = .true.
/
```

Usage:

```bash
# Environment-specific coverage
fortcov --config=configs/dev.nml
fortcov --config=configs/staging.nml
fortcov --config=configs/prod.nml
```

---

## Project Manager

### Understanding Coverage Reports

#### Executive Summary View

When you receive a coverage report, focus on these key metrics:

```markdown
# Coverage Summary (Example)
- **Overall Coverage**: 87.3% ✅ (Target: 80%)
- **Files Analyzed**: 23
- **Critical Issues**: 2 files below 70%
- **Trend**: +2.4% from last week ⬆️
```

#### Risk Assessment

| Coverage Level | Risk | Action Needed |
|----------------|------|---------------|
| 90%+ | Low | Maintain current practices |
| 80-89% | Medium | Monitor critical paths |
| 70-79% | High | Increase testing effort |
| <70% | Critical | Immediate action required |

### Interpreting Reports

#### File-Level Analysis

```markdown
| Component | Coverage | Risk | Notes |
|-----------|----------|------|-------|
| User Authentication | 95% | Low | Well tested |
| Data Processing | 72% | High | Needs attention |
| Error Handling | 45% | Critical | Major gaps |
| UI Components | 88% | Low | Good coverage |
```

**Key Questions to Ask**:
1. Are critical business functions well covered (>85%)?
2. Which components have the highest risk?
3. How does coverage trend over time?

#### Missing Coverage Impact

```fortran
! Example: Uncovered error handling (High Risk)
subroutine process_user_data(user_input)
    ! ... main processing logic (covered by tests)
    
    ! Lines 45-52: Not covered by tests
    if (database_error) then
        call log_critical_error("Database unavailable")
        call send_admin_alert()
        stop 1  ! Application crash - HIGH RISK
    end if
end subroutine
```

**Business Impact**: Application crashes instead of graceful error handling.  
**Recommendation**: Priority testing for error scenarios.

### Tracking Progress

#### Weekly Coverage Dashboard

Create a simple tracking spreadsheet:

| Week | Overall Coverage | Critical Functions | New Features | Trend |
|------|-----------------|-------------------|--------------|-------|
| W1 | 82.1% | 89% | N/A | Baseline |
| W2 | 84.3% | 91% | Login: 78% | +2.2% ⬆️ |
| W3 | 81.7% | 89% | Payment: 65% | -2.6% ⬇️ |
| W4 | 86.2% | 93% | Payment: 82% | +4.5% ⬆️ |

#### Quality Gates

Set project milestones:

```
Sprint Goals:
- [ ] Overall coverage >85%
- [ ] All critical functions >90%
- [ ] New features >80% before merge
- [ ] No untested error handlers
```

### Team Communication

#### Coverage Report Template

Use this template for status updates:

```
## Coverage Status - Sprint X

**Overall**: 87.3% (Target: 85%) ✅

**Highlights**:
- Authentication module reached 95%
- Payment processing improved to 82%
- All critical paths now covered

**Concerns**:
- Error handling still at 45% (Action: Dev Team)
- Mobile UI coverage dropped to 73% (Action: QA Team)

**Next Sprint Goals**:
- Increase error handling coverage to 70%
- Maintain >85% overall coverage
- Test new reporting features

**Blockers**: None
```

#### Stakeholder Communication

**For Executives** (2-sentence summary):
> "Code coverage is at 87.3%, exceeding our 85% target and indicating strong software quality. Two areas need attention: error handling and mobile interfaces."

**For Development Team** (actionable details):
> "Priority 1: Error handling coverage is at 45% - focus testing on failure scenarios. Priority 2: Mobile UI coverage dropped 5% - review recent changes and add UI tests."

### Budget and Planning

#### Coverage Investment

| Investment | Impact | Timeline | Cost |
|------------|--------|----------|------|
| Developer training on testing | High | 2 weeks | Medium |
| Automated coverage in CI/CD | High | 1 week | Low |
| Coverage monitoring tools | Medium | 1 week | Low |
| Test framework improvements | Medium | 3 weeks | Medium |

#### ROI Calculation

**Cost of Low Coverage**:
- Production bugs: 4 hours × $150/hour = $600 per bug
- Customer support: 2 hours × $75/hour = $150 per bug  
- Emergency fixes: 8 hours × $150/hour = $1,200 per fix

**Coverage Investment ROI**:
- Testing effort: 20 hours × $120/hour = $2,400
- Bugs prevented: 10 × ($600 + $150 + $1,200) = $19,500
- **ROI**: 713% return on investment

### Reporting Automation

#### Automated Status Reports

Set up weekly automated reports:

```bash
# Weekly coverage summary email
fortcov --source=src --output-format=json --quiet | \
jq -r '"Coverage: \(.summary.line_coverage)% | Files: \(.files | length) | Trend: +2.3%"' | \
mail -s "Weekly Coverage Report" stakeholders@company.com
```

#### Integration with Project Tools

**Jira Integration**:
```bash
# Update Jira ticket with coverage status
COVERAGE=$(fortcov --source=src --output-format=json --quiet | jq -r '.summary.line_coverage')
curl -X PUT -H "Content-Type: application/json" \
    -d "{\"fields\":{\"customfield_coverage\":\"$COVERAGE%\"}}" \
    "https://company.atlassian.net/rest/api/2/issue/PROJ-123"
```

**Confluence Reports**:
- Embed coverage trends in team dashboards
- Link detailed reports to project documentation  
- Track coverage against sprint goals