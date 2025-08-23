# FortCov Installation Guide

Complete installation instructions for FortCov on different platforms.

## Quick Start

```bash
# 1. Install prerequisites
sudo apt install gfortran  # Ubuntu/Debian
# or: brew install gcc      # macOS

# 2. Install FPM
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12 -o /tmp/fpm
chmod +x /tmp/fpm
sudo mv /tmp/fpm /usr/local/bin/

# 3. Install FortCov
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov
fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/

# 4. Verify installation
fortcov --help  # Latest version includes critical memory safety improvements
```

## Platform-Specific Instructions

### Ubuntu / Debian

```bash
# Install build essentials
sudo apt update && sudo apt install -y build-essential gfortran git curl

# Install FPM
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12 -o /tmp/fpm
chmod +x /tmp/fpm && sudo mv /tmp/fpm /usr/local/bin/

# Install FortCov
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
```

### macOS

```bash
# Install Xcode Command Line Tools
xcode-select --install

# Install dependencies
brew install gcc fortran-lang/tap/fpm

# Install FortCov
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
```

### Windows (MSYS2)

```bash
# In MSYS2 terminal
pacman -S mingw-w64-x86_64-gcc-fortran git curl

# Install FPM and FortCov
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-windows.zip -o fpm.zip
unzip fmp.zip && mv fmp.exe /usr/local/bin/

git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fpm build --profile release
cp build/gfortran_*/app/fortcov.exe /usr/local/bin/
```

## Troubleshooting Installation

Common solutions to installation issues:

- **"fpm: command not found"** → Download FPM binary from releases page
- **"gfortran: command not found"** → Install GCC package for your platform  
- **"Permission denied"** → Install to `~/.local/bin/` instead of system-wide
- **Build failures** → Ensure FPM version 0.8.0+ and gfortran 9.0+

For detailed troubleshooting, see [Troubleshooting Guide](troubleshooting.md).