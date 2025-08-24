# FortCov Installation Guide

**Brief installation guide** - see main [README.md](../../README.md) for complete instructions.

## Platform Installation

**Ubuntu/Debian:**
```bash
sudo apt update && sudo apt install -y build-essential gfortran git curl
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-0.12.0-linux-x86_64-gcc-12 -o /tmp/fpm
chmod +x /tmp/fpm && sudo mv /tmp/fpm /usr/local/bin/
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fpm build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
```

**macOS:**
```bash
xcode-select --install
brew install gcc fortran-lang/tap/fpm
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fmp build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
```

**Windows (MSYS2):**
```bash
pacman -S mingw-w64-x86_64-gcc-fortran git curl
curl -fsSL https://github.com/fortran-lang/fpm/releases/latest/download/fpm-windows.zip -o fpm.zip
unzip fpm.zip && mv fpm.exe /usr/local/bin/
git clone https://github.com/lazy-fortran/fortcov.git
cd fortcov && fpm build --profile release
cp build/gfortran_*/app/fortcov.exe /usr/local/bin/
```

## Troubleshooting

**Common fixes:**
- **"fpm: command not found"** → Download FPM binary from releases page
- **"gfortran: command not found"** → Install GCC package for your platform  
- **"Permission denied"** → Install to `~/.local/bin/` instead of system-wide
- **Build failures** → Ensure FPM version 0.8.0+ and gfortran 9.0+

For complete troubleshooting, see main [README.md](../../README.md).