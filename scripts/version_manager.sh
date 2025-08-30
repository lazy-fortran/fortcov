#!/bin/bash

# FortCov Version Management Script
# Automated semantic versioning for fpm.toml and release preparation

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
FPM_TOML="$PROJECT_ROOT/fpm.toml"
CHANGELOG="$PROJECT_ROOT/CHANGELOG.md"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Usage information
usage() {
    cat << EOF
FortCov Version Manager

USAGE:
    $(basename "$0") <command> [options]

COMMANDS:
    current                     Show current version
    bump <major|minor|patch>    Increment version component
    set <version>              Set specific version (e.g., 1.2.3)
    prepare <version>          Prepare release (update version, validate, stage)
    validate                   Validate current version consistency

EXAMPLES:
    $(basename "$0") current
    $(basename "$0") bump patch
    $(basename "$0") bump minor  
    $(basename "$0") set 1.0.0
    $(basename "$0") prepare 1.0.1
    $(basename "$0") validate

OPTIONS:
    --dry-run          Show changes without applying them
    --no-git          Skip git operations (staging/tagging)
    --help            Show this help message
EOF
}

# Validate semantic version format
validate_semver() {
    local version="$1"
    if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        log_error "Invalid semantic version format: $version (expected: X.Y.Z)"
        return 1
    fi
    return 0
}

# Extract current version from fpm.toml
get_current_version() {
    if [[ ! -f "$FPM_TOML" ]]; then
        log_error "fpm.toml not found at: $FPM_TOML"
        return 1
    fi
    
    grep '^version = ' "$FPM_TOML" | cut -d'"' -f2 || {
        log_error "Could not extract version from fpm.toml"
        return 1
    }
}

# Update version in fpm.toml
update_fpm_version() {
    local new_version="$1"
    local temp_file
    temp_file=$(mktemp)
    
    # Create updated fpm.toml with new version
    sed "s/^version = .*/version = \"$new_version\"/" "$FPM_TOML" > "$temp_file"
    
    # Validate the change was made correctly
    local updated_version
    updated_version=$(grep '^version = ' "$temp_file" | cut -d'"' -f2)
    
    if [[ "$updated_version" != "$new_version" ]]; then
        log_error "Failed to update version in fpm.toml"
        rm -f "$temp_file"
        return 1
    fi
    
    # Apply the change
    mv "$temp_file" "$FPM_TOML"
    log_success "Updated fpm.toml version to $new_version"
}

# Update changelog unreleased section
update_changelog() {
    local version="$1"
    local date
    date=$(date +%Y-%m-%d)
    
    if [[ ! -f "$CHANGELOG" ]]; then
        log_warning "CHANGELOG.md not found, skipping changelog update"
        return 0
    fi
    
    # Replace [Unreleased] with version and date
    sed -i "s/## \[Unreleased\]/## [Unreleased]\n\n## [$version] - $date/" "$CHANGELOG"
    log_success "Updated CHANGELOG.md with version $version"
}

# Bump version component
bump_version() {
    local component="$1"
    local current_version
    current_version=$(get_current_version) || return 1
    
    # Parse current version components
    local major minor patch
    IFS='.' read -r major minor patch <<< "$current_version"
    
    # Increment the specified component
    case "$component" in
        major)
            major=$((major + 1))
            minor=0
            patch=0
            ;;
        minor)
            minor=$((minor + 1))
            patch=0
            ;;
        patch)
            patch=$((patch + 1))
            ;;
        *)
            log_error "Invalid version component: $component (use: major, minor, or patch)"
            return 1
            ;;
    esac
    
    local new_version="$major.$minor.$patch"
    
    # Only log if not in dry run mode
    if [[ "${DRY_RUN:-}" != "true" ]]; then
        log_info "Bumping $component: $current_version → $new_version"
    fi
    
    echo "$new_version"
}

# Validate version consistency across files
validate_consistency() {
    local current_version
    current_version=$(get_current_version) || return 1
    
    log_info "Current version: $current_version"
    validate_semver "$current_version" || return 1
    
    # Check if there are any git tags
    if git tag -l 2>/dev/null | grep -q "v$current_version"; then
        log_warning "Git tag v$current_version already exists"
    fi
    
    # Validate fpm.toml syntax
    if ! fpm build --dry-run >/dev/null 2>&1; then
        log_error "fpm.toml validation failed - syntax error"
        return 1
    fi
    
    log_success "Version consistency validated"
    return 0
}

# Stage files for git
stage_files() {
    local files=("$@")
    
    if [[ "${NO_GIT:-}" == "true" ]]; then
        log_info "Skipping git operations (--no-git flag)"
        return 0
    fi
    
    for file in "${files[@]}"; do
        if [[ -f "$file" ]]; then
            git add "$file"
            log_info "Staged: $file"
        fi
    done
}

# Prepare release with all steps
prepare_release() {
    local version="$1"
    
    validate_semver "$version" || return 1
    
    local current_version
    current_version=$(get_current_version) || return 1
    
    if [[ "$version" == "$current_version" ]]; then
        log_error "Version $version is already current"
        return 1
    fi
    
    log_info "Preparing release $version (current: $current_version)"
    
    # Update version files
    if [[ "${DRY_RUN:-}" != "true" ]]; then
        update_fpm_version "$version" || return 1
        update_changelog "$version" || return 1
        
        # Stage changed files
        stage_files "$FPM_TOML" "$CHANGELOG"
        
        log_success "Release $version prepared successfully"
        log_info "Next steps:"
        log_info "  1. Review changes: git diff --cached"
        log_info "  2. Commit changes: git commit -m 'bump: version $version'"
        log_info "  3. Create tag: git tag v$version"
        log_info "  4. Push release: git push origin main --tags"
    else
        log_info "[DRY RUN] Would update version to $version"
        log_info "[DRY RUN] Would update CHANGELOG.md"
        log_info "[DRY RUN] Would stage files for git"
    fi
}

# Main command handler
main() {
    # Parse global options first
    while [[ $# -gt 0 ]]; do
        case $1 in
            --dry-run)
                DRY_RUN="true"
                shift
                ;;
            --no-git)
                NO_GIT="true"
                shift
                ;;
            --help)
                usage
                exit 0
                ;;
            -*)
                log_error "Unknown option: $1"
                usage
                exit 1
                ;;
            *)
                break
                ;;
        esac
    done
    
    local command="${1:-}"
    
    # Ensure we're in project root
    cd "$PROJECT_ROOT"
    
    case "$command" in
        current)
            get_current_version || exit 1
            ;;
        bump)
            local component="${2:-}"
            if [[ -z "$component" ]]; then
                log_error "Bump component required (major|minor|patch)"
                usage
                exit 1
            fi
            
            if [[ "${DRY_RUN:-}" != "true" ]]; then
                local new_version
                new_version=$(bump_version "$component") || exit 1
                update_fpm_version "$new_version" || exit 1
                stage_files "$FPM_TOML"
            else
                local current_version
                current_version=$(get_current_version) || exit 1
                local new_version
                new_version=$(bump_version "$component") || exit 1
                log_info "[DRY RUN] Would bump $component: $current_version → $new_version"
            fi
            ;;
        set)
            local version="${2:-}"
            if [[ -z "$version" ]]; then
                log_error "Version required (e.g., 1.0.0)"
                usage
                exit 1
            fi
            
            validate_semver "$version" || exit 1
            
            if [[ "${DRY_RUN:-}" != "true" ]]; then
                update_fpm_version "$version" || exit 1
                stage_files "$FPM_TOML"
            else
                log_info "[DRY RUN] Would set version to $version"
            fi
            ;;
        prepare)
            local version="${2:-}"
            if [[ -z "$version" ]]; then
                log_error "Version required (e.g., 1.0.1)"
                usage
                exit 1
            fi
            
            prepare_release "$version" || exit 1
            ;;
        validate)
            validate_consistency || exit 1
            ;;
        *)
            log_error "Unknown command: $command"
            usage
            exit 1
            ;;
    esac
}

# Execute main function with all arguments
main "$@"