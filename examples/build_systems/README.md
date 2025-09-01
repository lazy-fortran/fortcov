# Build System Integration Examples

This directory contains working examples of fortcov integration with various Fortran build systems and CI/CD platforms. All examples are tested and validated against real-world deployment scenarios.

## Quick Start

Each subdirectory contains a complete, working example with:
- Build system configuration files
- Sample Fortran source code
- Coverage generation scripts
- CI/CD workflow configurations
- Expected output examples

## Available Integration Examples

### Build Systems
- **[fpm/](fpm/)** - Fortran Package Manager integration patterns
- **[cmake/](cmake/)** - CMake with cmake-codecov integration
- **[make/](make/)** - Traditional Makefile-based builds
- **[meson/](meson/)** - Meson build system integration (example‑based; no root `meson.build`)

### CI/CD Platforms
- **[ci_cd/github_actions/](ci_cd/github_actions/)** - GitHub Actions workflows
- **[ci_cd/gitlab_ci/](ci_cd/gitlab_ci/)** - GitLab CI pipeline configurations
- **[ci_cd/jenkins/](ci_cd/jenkins/)** - Jenkins pipeline examples

### Specialized Environments
- **[docker/](docker/)** - Container-based coverage workflows
- **[hpc/](hpc/)** - HPC and SLURM integration patterns

## Testing Examples

All examples include automated testing to ensure they work correctly:

```bash
# Test all build system integrations
cd examples/build_systems
./test_all_examples.sh

# Test specific integration
cd fpm/basic_example
./test_example.sh
```

## Integration Pattern Summary

| Build System | Configuration | Coverage Generation | CI/CD Support |
|--------------|---------------|-------------------|---------------|
| FPM | `fpm.toml` | `fpm test --flag "-fprofile-arcs -ftest-coverage"` | ✅ |
| CMake | `CMakeLists.txt` | `make test && make fortcov_report` | ✅ |
| Make | `Makefile` | `make coverage` | ✅ |
| Meson | `meson.build` | `ninja fortcov_coverage` | ✅ |

## Performance Targets

All integration examples meet these performance requirements:
- **Build Overhead**: <10% additional compilation time
- **Analysis Speed**: <5 minutes for typical projects (<1000 source files)
- **Memory Usage**: <500MB for large projects (scalable patterns available)

## Contributing

When adding new integration examples:
1. Follow the directory structure pattern
2. Include working configuration files
3. Add automated testing scripts
4. Document expected outputs
5. Test on multiple platforms when possible

## Support

For integration-specific questions:
- Check the relevant example directory
- Review the troubleshooting guides in each example
- Run the automated validation scripts
