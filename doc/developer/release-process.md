# FortCov Release Process

This document describes the automated release process for FortCov, including version management, release preparation, and deployment procedures.

## Overview

FortCov follows semantic versioning (SemVer) with automated release pipeline infrastructure:

- **Semantic Versioning**: `MAJOR.MINOR.PATCH` format
- **Automated Releases**: GitHub Actions workflow for consistent deployments
- **Multi-platform Builds**: Linux and macOS release artifacts
- **Quality Gates**: Full test suite validation before release

## Release Types

### Patch Release (X.Y.Z → X.Y.Z+1)
- Bug fixes
- Security patches  
- Documentation updates
- No breaking changes

### Minor Release (X.Y.Z → X.Y+1.0)
- New features
- Enhanced functionality
- Backwards compatible changes
- Dependencies updates

### Major Release (X.Y.Z → X+1.0.0)
- Breaking API changes
- Architecture overhauls
- Incompatible modifications

## Automated Release Process

### Prerequisites

1. **Environment Setup**:
   ```bash
   # Ensure clean working directory
   git status
   git pull origin main
   
   # Verify build environment
   fpm build
   fpm test
   ```

2. **Version Management Tool**:
   ```bash
   # Make script executable (if needed)
   chmod +x scripts/version_manager.sh
   
   # Validate current state
   ./scripts/version_manager.sh validate
   ```

### Step-by-Step Release

#### 1. Version Preparation

```bash
# Check current version
./scripts/version_manager.sh current

# Prepare release (updates fpm.toml and CHANGELOG.md)
./scripts/version_manager.sh prepare 0.4.1

# Review changes
git diff --cached

# Alternative: Manual version bump
./scripts/version_manager.sh bump patch    # or minor/major
```

#### 2. Changelog Updates

Update `CHANGELOG.md` with release notes:

```markdown
## [0.4.1] - 2024-08-30

### Added
- Automated release pipeline infrastructure
- Multi-platform release artifacts
- Version management automation

### Fixed
- Release process documentation
- Build system compatibility

### Changed
- Enhanced CI/CD pipeline
```

#### 3. Release Commit and Tag

```bash
# Commit version changes
git commit -m "bump: version 0.4.1

🚀 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"

# Create release tag
git tag v0.4.1

# Push changes and tag
git push origin main --tags
```

#### 4. Automated Release Trigger

The release workflow is triggered automatically by the tag push:

1. **Version Validation**: Ensures fpm.toml and tag consistency
2. **Multi-platform Build**: Creates Linux and macOS binaries
3. **Quality Gates**: Runs full test suite
4. **Artifact Creation**: Generates release assets with checksums
5. **GitHub Release**: Creates release with changelog and artifacts

### Manual Release Trigger

For manual releases or hotfixes:

```bash
# Trigger workflow dispatch
gh workflow run release.yml -f version=v0.4.1
```

## Release Artifacts

Each release generates:

### Binaries
- `fortcov-linux-x86_64-VERSION` - Linux executable
- `fortcov-macos-x86_64-VERSION` - macOS executable

### Verification
- `*.sha256` - SHA256 checksums for each binary
- Release notes with installation instructions
- Changelog section for the version

### Installation Example
```bash
# Download and install release
wget https://github.com/lazy-fortran/fortcov/releases/download/v0.4.1/fortcov-linux-x86_64-0.4.1
chmod +x fortcov-linux-x86_64-0.4.1
sudo mv fortcov-linux-x86_64-0.4.1 /usr/local/bin/fortcov

# Verify installation
fortcov --version
```

## Quality Assurance

### Automated Testing

The release pipeline includes comprehensive quality gates:

1. **Build Validation**: Confirms compilation on all target platforms
2. **Test Suite**: Full test execution (335+ tests)
3. **Smoke Tests**: Basic functionality verification
4. **Artifact Validation**: Checksums and executable verification

### Release Validation

After release creation:

```bash
# Download and test release binary
wget https://github.com/lazy-fortran/fortcov/releases/latest/download/fortcov-linux-x86_64-0.4.1
chmod +x fortcov-linux-x86_64-0.4.1

# Basic functionality test
./fortcov-linux-x86_64-0.4.1 --help
./fortcov-linux-x86_64-0.4.1 --version

# SHA256 verification
sha256sum fortcov-linux-x86_64-0.4.1
# Compare with published checksum
```

## Rollback Procedures

### Git Rollback
```bash
# Remove erroneous tag
git tag -d v0.4.1
git push origin :refs/tags/v0.4.1

# Revert version commit
git revert HEAD
git push origin main
```

### GitHub Release Management
```bash
# Delete GitHub release (if needed)
gh release delete v0.4.1 --yes

# Recreate release with fixes
git tag v0.4.1
git push origin --tags
```

## Troubleshooting

### Common Issues

1. **Version Mismatch**:
   ```
   Error: Version mismatch: fpm.toml (0.4.0) != tag (v0.4.1)
   ```
   Solution: Update fpm.toml version before tagging

2. **Build Failures**:
   ```
   Error: Build failed on macOS
   ```
   Solution: Test builds locally on target platforms

3. **Test Failures**:
   ```
   Error: 3 tests failed in release validation
   ```
   Solution: Fix failing tests before release

### Debug Commands

```bash
# Check release workflow status
gh run list --workflow=release.yml

# View specific workflow run
gh run view <run-id>

# Download workflow artifacts locally
gh run download <run-id>
```

## Version Management Reference

### Version Manager Script Usage

```bash
# Show current version
./scripts/version_manager.sh current

# Bump version components
./scripts/version_manager.sh bump patch    # 0.4.0 → 0.4.1
./scripts/version_manager.sh bump minor    # 0.4.1 → 0.5.0
./scripts/version_manager.sh bump major    # 0.5.0 → 1.0.0

# Set specific version
./scripts/version_manager.sh set 1.0.0

# Prepare complete release
./scripts/version_manager.sh prepare 1.0.1

# Validate consistency
./scripts/version_manager.sh validate

# Dry run (preview changes)
./scripts/version_manager.sh --dry-run bump patch
```

### Git Workflow Integration

```bash
# Standard release workflow
./scripts/version_manager.sh prepare 0.4.1
git commit -m "bump: version 0.4.1"
git tag v0.4.1
git push origin main --tags

# Hotfix workflow
git checkout -b hotfix-0.4.2
# ... make fixes ...
./scripts/version_manager.sh set 0.4.2
git commit -m "fix: critical security patch"
git tag v0.4.2
git push origin hotfix-0.4.2 --tags
```

## Security Considerations

### Release Signing
- All releases include SHA256 checksums
- Consider GPG signing for enhanced security
- Verify artifacts before distribution

### Access Control
- Release permissions limited to maintainers
- GitHub token permissions configured minimally
- Build environments isolated and secured

---

## Next Steps

1. **Enhanced Automation**: Consider additional quality gates
2. **Signing Integration**: Implement GPG signing for releases
3. **Notification System**: Add release notifications
4. **Metrics Collection**: Track release deployment success

This release process ensures consistent, reliable, and secure deployments of FortCov while maintaining high quality standards through automated testing and validation.