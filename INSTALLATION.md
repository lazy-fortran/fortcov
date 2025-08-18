# FortCov Installation Guide

Complete installation instructions for FortCov on different platforms and environments.

## Quick Start

For most users, the recommended installation method:

```bash
# 1. Install prerequisites
sudo apt install gfortran  # Ubuntu/Debian
# or: brew install gcc      # macOS

# 2. Install FPM
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux.tar.gz | tar -xz -C /tmp
sudo mv /tmp/fpm /usr/local/bin/

# 3. Install FortCov
git clone https://github.com/krystophny/fortcov.git
cd fortcov
fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/

# 4. Verify installation
fortcov --help
```

## Platform-Specific Instructions

### Ubuntu / Debian

#### Prerequisites

```bash
# Update package list
sudo apt update

# Install build essentials
sudo apt install -y build-essential gfortran git curl
```

#### Install FPM (Fortran Package Manager)

```bash
# Method 1: Download binary (recommended)
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux.tar.gz | tar -xz -C /tmp
sudo mv /tmp/fpm /usr/local/bin/
chmod +x /usr/local/bin/fpm

# Method 2: Build from source
git clone https://github.com/fortran-lang/fpm.git /tmp/fpm
cd /tmp/fpm
./install.sh --prefix=/usr/local
```

#### Install FortCov

```bash
# Clone and build
git clone https://github.com/krystophny/fortcov.git
cd fortcov
fpm build --profile release

# Install system-wide
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/

# Or install for current user only
mkdir -p ~/.local/bin
cp build/gfortran_*/app/fortcov ~/.local/bin/
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### Verification

```bash
# Test all components
which gfortran && echo "✅ gfortran found" || echo "❌ gfortran missing"
which fpm && echo "✅ fpm found" || echo "❌ fpm missing"
which gcov && echo "✅ gcov found" || echo "❌ gcov missing"
which fortcov && echo "✅ fortcov found" || echo "❌ fortcov missing"

# Version check
gfortran --version | head -1
fpm --version
fortcov --version
```

### CentOS / RHEL / Fedora

#### Prerequisites

```bash
# CentOS/RHEL 8+
sudo dnf groupinstall "Development Tools"  
sudo dnf install gcc-gfortran git curl

# Older CentOS/RHEL (7)
sudo yum groupinstall "Development Tools"
sudo yum install gcc-gfortran git curl

# Fedora
sudo dnf install gcc-gfortran git curl
```

#### Install FPM

```bash
# Download and install FPM
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fmp-linux.tar.gz | tar -xz -C /tmp
sudo mv /tmp/fpm /usr/local/bin/
chmod +x /usr/local/bin/fpm
```

#### Install FortCov

```bash
git clone https://github.com/krystophny/fortcov.git
cd fortcov  
fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
```

### macOS

#### Prerequisites

Install Xcode Command Line Tools and Homebrew:

```bash
# Install Xcode Command Line Tools
xcode-select --install

# Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

#### Install Dependencies

```bash
# Install gcc (includes gfortran and gcov)
brew install gcc

# Install FPM
brew install fortran-lang/tap/fpm

# Verify versions
gfortran --version
gcov --version
fpm --version
```

#### Install FortCov

```bash
git clone https://github.com/krystophny/fortcov.git
cd fortcov
fpm build --profile release

# Install to /usr/local/bin
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/

# Or install to Homebrew prefix
cp build/gfortran_*/app/fortcov $(brew --prefix)/bin/
```

### Windows (MSYS2)

#### Install MSYS2

1. Download MSYS2 installer from https://www.msys2.org/
2. Run installer and follow instructions
3. Open MSYS2 terminal

#### Install Dependencies

```bash
# Update package database
pacman -Syu

# Install development tools
pacman -S mingw-w64-x86_64-gcc-fortran
pacman -S mingw-w64-x86_64-make
pacman -S git curl

# Install FPM
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-windows.zip -o fpm.zip
unzip fpm.zip
mv fpm.exe /usr/local/bin/
```

#### Install FortCov

```bash
git clone https://github.com/krystophny/fortcov.git
cd fortcov
fpm build --profile release

# Copy to PATH
cp build/gfortran_*/app/fortcov.exe /usr/local/bin/
```

#### Windows-Specific Notes

- Use MSYS2 terminal for all commands
- File paths use forward slashes in MSYS2
- Windows Defender may flag the executable initially

### Windows Subsystem for Linux (WSL)

#### Setup WSL

```powershell
# Enable WSL (run as Administrator in PowerShell)
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart

# Restart computer, then install Ubuntu from Microsoft Store
```

#### Install in WSL

Once in WSL Ubuntu environment, follow the Ubuntu installation instructions above.

## Development Environment Setup

### VS Code Integration

#### Install Extensions

1. **Modern Fortran** - Syntax highlighting and IntelliSense
2. **Fortran IntelliSense** - Advanced language support  
3. **Code Runner** - Quick code execution

#### Configure Tasks

Create `.vscode/tasks.json`:

```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Build with Coverage",
            "type": "shell",
            "command": "fpm",
            "args": ["build", "--flag", "-fprofile-arcs -ftest-coverage"],
            "group": "build",
            "problemMatcher": []
        },
        {
            "label": "Test with Coverage", 
            "type": "shell",
            "command": "fpm",
            "args": ["test", "--flag", "-fprofile-arcs -ftest-coverage"],
            "group": "test",
            "dependsOn": "Build with Coverage"
        },
        {
            "label": "Generate Coverage Report",
            "type": "shell",
            "command": "sh",
            "args": ["-c", "gcov src/*.f90 && fortcov --source=src --output=coverage.md"],
            "group": "build",
            "dependsOn": "Test with Coverage"
        }
    ]
}
```

#### Keyboard Shortcuts

Create `.vscode/keybindings.json`:

```json
[
    {
        "key": "ctrl+shift+c",
        "command": "workbench.action.tasks.runTask",
        "args": "Generate Coverage Report"
    }
]
```

### Vim/Neovim Configuration

Add to your `.vimrc` or `init.vim`:

```vim
" FortCov integration
command! FortCovBuild !fpm build --flag "-fprofile-arcs -ftest-coverage"
command! FortCovTest !fpm test --flag "-fprofile-arcs -ftest-coverage"
command! FortCovReport !gcov src/*.f90 && fortcov --source=src --output=coverage.md
command! FortCovFull !fpm build --flag "-fprofile-arcs -ftest-coverage" && fpm test --flag "-fprofile-arcs -ftest-coverage" && gcov src/*.f90 && fortcov --source=src --output=coverage.md

" Key mappings
nnoremap <leader>cb :FortCovBuild<CR>
nnoremap <leader>ct :FortCovTest<CR>
nnoremap <leader>cr :FortCovReport<CR>
nnoremap <leader>cf :FortCovFull<CR>
```

### Emacs Configuration

Add to your `.emacs` or `init.el`:

```elisp
;; FortCov integration
(defun fortcov-build ()
  "Build project with coverage flags"
  (interactive)
  (compile "fpm build --flag \"-fprofile-arcs -ftest-coverage\""))

(defun fortcov-test ()
  "Run tests with coverage flags"
  (interactive)
  (compile "fpm test --flag \"-fprofile-arcs -ftest-coverage\""))

(defun fortcov-report ()
  "Generate coverage report"
  (interactive)
  (compile "gcov src/*.f90 && fortcov --source=src --output=coverage.md"))

;; Key bindings (assuming you use f90-mode for Fortran)
(add-hook 'f90-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c c b") 'fortcov-build)
    (local-set-key (kbd "C-c c t") 'fortcov-test)
    (local-set-key (kbd "C-c c r") 'fortcov-report)))
```

## HPC and Cluster Environments

### Module System Integration

For HPC clusters using environment modules:

```bash
# Create module file: /path/to/modulefiles/fortcov/1.0
#%Module1.0
##
## fortcov 1.0 modulefile
##
proc ModulesHelp { } {
    puts stderr "FortCov - Fortran coverage analysis tool"
}

module-whatis "FortCov coverage analysis tool"

set basedir /opt/fortcov/1.0

prepend-path PATH $basedir/bin
setenv FORTCOV_HOME $basedir
```

Usage:

```bash
module load gcc/11.2 fpm/0.8.2 fortcov/1.0
fortcov --version
```

### Slurm Job Script

```bash
#!/bin/bash
#SBATCH --job-name=coverage-test
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:30:00
#SBATCH --partition=compute

# Load modules
module load gcc/11.2 fpm/0.8.2 fortcov/1.0

# Navigate to project
cd $SLURM_SUBMIT_DIR

# Generate coverage
fpm build --flag "-fprofile-arcs -ftest-coverage"
fpm test --flag "-fprofile-arcs -ftest-coverage"
gcov src/*.f90
fortcov --source=src --output=coverage-${SLURM_JOB_ID}.md

echo "Coverage report: coverage-${SLURM_JOB_ID}.md"
```

## Docker Installation

### Dockerfile

```dockerfile
FROM ubuntu:22.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    gfortran \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install FPM
RUN curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux.tar.gz | \
    tar -xz -C /usr/local/bin

# Install FortCov
RUN git clone https://github.com/krystophny/fortcov.git /tmp/fortcov && \
    cd /tmp/fortcov && \
    fpm build --profile release && \
    cp build/gfortran_*/app/fortcov /usr/local/bin/ && \
    rm -rf /tmp/fortcov

# Verify installation
RUN fortcov --version

WORKDIR /workspace
CMD ["bash"]
```

### Build and Use

```bash
# Build Docker image
docker build -t fortcov .

# Use with current project
docker run --rm -v $(pwd):/workspace fortcov bash -c "
    fpm build --flag '-fprofile-arcs -ftest-coverage' &&
    fpm test --flag '-fprofile-arcs -ftest-coverage' &&
    gcov src/*.f90 &&
    fortcov --source=src --output=coverage.md
"
```

### Docker Compose

```yaml
# docker-compose.yml
version: '3.8'
services:
  fortcov:
    build: .
    volumes:
      - .:/workspace
    working_dir: /workspace
    command: bash
    
  coverage:
    build: .
    volumes:
      - .:/workspace
    working_dir: /workspace
    command: >
      bash -c "
        fpm build --flag '-fprofile-arcs -ftest-coverage' &&
        fpm test --flag '-fprofile-arcs -ftest-coverage' &&
        gcov src/*.f90 &&
        fortcov --source=src --output=coverage.md
      "
```

Usage:

```bash
# Interactive development
docker-compose run --rm fortcov

# Generate coverage report
docker-compose run --rm coverage
```

## Verification and Testing

### Installation Test Script

Save as `test-installation.sh`:

```bash
#!/bin/bash
# Installation verification script

echo "=== FortCov Installation Test ==="

# Check prerequisites
echo "Checking prerequisites..."
FAILED=0

if ! command -v gfortran >/dev/null; then
    echo "❌ gfortran not found"
    FAILED=1
else
    echo "✅ gfortran: $(gfortran --version | head -1)"
fi

if ! command -v fpm >/dev/null; then
    echo "❌ fpm not found"  
    FAILED=1
else
    echo "✅ fpm: $(fpm --version)"
fi

if ! command -v gcov >/dev/null; then
    echo "❌ gcov not found"
    FAILED=1
else
    echo "✅ gcov: $(gcov --version | head -1)"
fi

if ! command -v fortcov >/dev/null; then
    echo "❌ fortcov not found"
    FAILED=1
else
    echo "✅ fortcov: $(fortcov --version | head -1)"
fi

if [ $FAILED -eq 1 ]; then
    echo "❌ Installation verification failed"
    exit 1
fi

# Test basic functionality
echo ""
echo "Testing basic functionality..."

# Create test project
TEST_DIR="/tmp/fortcov-test-$$"
mkdir -p "$TEST_DIR/src"
cd "$TEST_DIR"

# Create fpm.toml
cat > fpm.toml << 'EOF'
name = "test"
version = "0.1.0"
author = "test"
license = "MIT"
EOF

# Create test module
cat > src/test_module.f90 << 'EOF'
module test_module
    implicit none
contains
    function add_two(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x + 2
    end function add_two
end module test_module
EOF

# Create test program
mkdir test
cat > test/test_add.f90 << 'EOF'
program test_add
    use test_module
    implicit none
    
    if (add_two(3) /= 5) then
        print *, "Test failed"
        stop 1
    end if
    
    print *, "Test passed"
end program test_add
EOF

# Test coverage workflow
echo "Building with coverage..."
if ! fpm build --flag "-fprofile-arcs -ftest-coverage"; then
    echo "❌ Build failed"
    exit 1
fi

echo "Running tests..."
if ! fpm test --flag "-fprofile-arcs -ftest-coverage"; then
    echo "❌ Tests failed"
    exit 1
fi

echo "Generating coverage data..."
if ! gcov src/*.f90; then
    echo "❌ gcov failed"
    exit 1
fi

echo "Creating coverage report..."
if ! fortcov --source=src --output=coverage.md; then
    echo "❌ FortCov failed"
    exit 1
fi

# Check report was generated
if [ ! -f coverage.md ]; then
    echo "❌ Coverage report not generated"
    exit 1
fi

# Check report contains expected content
if ! grep -q "test_module.f90" coverage.md; then
    echo "❌ Coverage report missing expected content"
    exit 1
fi

# Cleanup
cd /
rm -rf "$TEST_DIR"

echo ""
echo "✅ All tests passed! FortCov is ready to use."
echo ""
echo "Quick start:"
echo "  1. fpm build --flag \"-fprofile-arcs -ftest-coverage\""
echo "  2. fpm test --flag \"-fprofile-arcs -ftest-coverage\""  
echo "  3. gcov src/*.f90"
echo "  4. fortcov --source=src --output=coverage.md"
```

Make it executable and run:

```bash
chmod +x test-installation.sh
./test-installation.sh
```

## Troubleshooting Installation

### Common Issues

#### "fpm: command not found"

**Solution**:
```bash
# Check if fpm is installed
ls -la /usr/local/bin/fpm

# If missing, reinstall:
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-linux.tar.gz | tar -xz -C /tmp
sudo mv /tmp/fpm /usr/local/bin/
chmod +x /usr/local/bin/fpm
```

#### "gfortran: command not found"

**Solution**:
```bash
# Ubuntu/Debian
sudo apt install gfortran

# CentOS/RHEL
sudo dnf install gcc-gfortran

# macOS
brew install gcc
```

#### "Permission denied" during install

**Solution**:
```bash
# Install to user directory instead
mkdir -p ~/.local/bin
cp build/gfortran_*/app/fortcov ~/.local/bin/

# Add to PATH
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

#### Build fails with "Fortran compiler not found"

**Solution**:
```bash
# Specify compiler explicitly
FC=gfortran fpm build --profile release

# Or set environment variable permanently
echo 'export FC=gfortran' >> ~/.bashrc
source ~/.bashrc
```

For additional troubleshooting, see [TROUBLESHOOTING.md](TROUBLESHOOTING.md).