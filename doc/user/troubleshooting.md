# FortCov Troubleshooting

**Quick fixes** - see main [README.md](../../README.md) for complete troubleshooting.

## Prerequisites Check

```bash
# Verify all tools are installed
which gfortran && echo "✅ gfortran" || echo "❌ Install gfortran"
which gcov && echo "✅ gcov" || echo "❌ Install gcc/gcov" 
which fpm && echo "✅ fpm" || echo "❌ Install fpm"
which fortcov && echo "✅ fortcov" || echo "❌ Install fortcov"
```

## Common Issues

**Build Failures:**
```bash
# Check versions  
fpm --version    # Need 0.8.0+
gfortran --version  # Need 9.0+

# Clean build
rm -rf build/ && fpm build --profile release
```

**Memory Errors (fixed in v2.0+):**
```bash
# Update to latest version
git pull && fmp build --profile release
sudo cp build/gfortran_*/app/fortcov /usr/local/bin/
```

**Security Messages:**
```bash
# Use clean paths
fortcov --source=src  # ✅ Good
# fortcov --source="src;rm -rf /"  # ❌ Blocked

# Avoid special characters  
fortcov --output=coverage.md  # ✅ Good
# fortcov --output="report|danger"  # ❌ Blocked
```

**No Coverage Data:**
```bash
# Ensure coverage flags
fpm test --flag "-fprofile-arcs -ftest-coverage"

# Generate gcov files
find build -name "*.gcda" | xargs dirname | sort -u | while read dir; do
  gcov --object-directory="$dir" "$dir"/*.gcno 2>/dev/null || true
done

# Check for .gcov files
ls -la *.gcov || echo "No coverage data generated"
```

**Permission Errors:**
```bash
# Install to user directory instead
cp build/gfortran_*/app/fortcov ~/.local/bin/
export PATH=$HOME/.local/bin:$PATH
```

For detailed troubleshooting, see main [README.md](../../README.md).