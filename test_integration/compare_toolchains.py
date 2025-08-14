#!/usr/bin/env python3
"""
Comprehensive comparison of fortcov vs lcov+cobertura+pycobertura toolchain
Analyzes differences in coverage reporting and validates consistency
"""

import subprocess
import sys
import tempfile
import os
import re
from pathlib import Path


def run_command(cmd, cwd=None, capture_output=True):
    """Run shell command and return result"""
    try:
        result = subprocess.run(
            cmd, shell=True, cwd=cwd, capture_output=capture_output, text=True
        )
        return result.returncode == 0, result.stdout, result.stderr
    except Exception as e:
        return False, "", str(e)


def extract_coverage_stats(markdown_content):
    """Extract coverage statistics from markdown table"""
    stats = {}
    
    # Look for markdown table rows
    lines = markdown_content.split('\n')
    for line in lines:
        if '|' in line and not line.strip().startswith('|---'):
            parts = [p.strip() for p in line.split('|') if p.strip()]
            if len(parts) >= 4:
                filename = parts[0]
                try:
                    # Try to extract coverage percentage
                    for part in parts:
                        if '%' in part:
                            percentage = float(part.replace('%', ''))
                            stats[filename] = percentage
                            break
                except (ValueError, IndexError):
                    continue
    
    return stats


def compare_coverage_stats(fortcov_stats, reference_stats):
    """Compare coverage statistics between tools"""
    differences = []
    
    all_files = set(fortcov_stats.keys()) | set(reference_stats.keys())
    
    for filename in all_files:
        fortcov_pct = fortcov_stats.get(filename, 0.0)
        reference_pct = reference_stats.get(filename, 0.0)
        
        diff = abs(fortcov_pct - reference_pct)
        if diff > 1.0:  # More than 1% difference
            differences.append({
                'file': filename,
                'fortcov': fortcov_pct,
                'reference': reference_pct,
                'difference': diff
            })
    
    return differences


def main():
    print("🔍 Comprehensive toolchain comparison: fortcov vs standard tools")
    
    # Create temporary directory
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        coverage_dir = temp_path / "coverage"
        coverage_dir.mkdir()
        
        print(f"📁 Working directory: {temp_dir}")
        
        # Build fortcov with coverage
        print("🔨 Building fortcov with coverage instrumentation...")
        fortcov_root = Path("/home/ert/code/fortcov")
        
        success, out, err = run_command(
            "fpm build --flag '-fprofile-arcs -ftest-coverage'", 
            cwd=fortcov_root
        )
        
        if not success:
            print(f"❌ Failed to build fortcov with coverage: {err}")
            return 1
        
        # Run tests to generate coverage
        print("📊 Running tests to generate coverage data...")
        success, out, err = run_command(
            "fpm test --flag '-fprofile-arcs -ftest-coverage'", 
            cwd=fortcov_root
        )
        # Note: tests might fail but still generate coverage data
        
        # Copy coverage files
        print("📋 Copying coverage files...")
        for pattern in ["*.gcno", "*.gcda"]:
            success, out, err = run_command(
                f"find {fortcov_root} -name '{pattern}' -exec cp {{}} {coverage_dir}/ \\;",
                capture_output=False
            )
        
        # Count files
        gcno_files = list(coverage_dir.glob("*.gcno"))
        gcda_files = list(coverage_dir.glob("*.gcda"))
        
        print(f"📈 Found {len(gcno_files)} .gcno files and {len(gcda_files)} .gcda files")
        
        if not gcno_files or not gcda_files:
            print("❌ No coverage data generated")
            return 1
        
        # === Run fortcov ===
        print("\n🚀 Running fortcov...")
        fortcov_output = temp_path / "fortcov_report.md"
        
        # Create config
        config_file = temp_path / "fortcov.nml"
        config_content = f"""&fortcov_config
    input_format = 'gcov'
    output_format = 'markdown'
    output_path = '{fortcov_output}'
    source_paths = '{fortcov_root}/src/'
    exclude_patterns = '*.mod', 'build/*', 'test/*'
    verbose = .false.
    quiet = .true.
/"""
        config_file.write_text(config_content)
        
        # Find fortcov executable
        fortcov_exe = None
        for exe_path in (fortcov_root / "build").rglob("fortcov"):
            if exe_path.is_file():
                fortcov_exe = exe_path
                break
        
        if not fortcov_exe:
            print("❌ fortcov executable not found")
            return 1
        
        success, out, err = run_command(
            f"{fortcov_exe} --config={config_file}",
            cwd=coverage_dir
        )
        
        fortcov_success = fortcov_output.exists()
        if fortcov_success:
            print("✅ fortcov completed successfully")
        else:
            print(f"⚠️  fortcov completed with issues: {err}")
        
        # === Run lcov pipeline ===
        print("\n🔧 Running lcov pipeline...")
        
        # Check tool availability
        tools_available = {}
        for tool in ['lcov', 'lcov_cobertura', 'pycobertura']:
            success, _, _ = run_command(f"which {tool}")
            tools_available[tool] = success
            print(f"  {tool}: {'✅' if success else '❌'}")
        
        lcov_output = temp_path / "coverage.info"
        cobertura_output = temp_path / "coverage.xml"
        pycobertura_output = temp_path / "pycobertura_report.md"
        
        reference_stats = {}
        
        if tools_available['lcov']:
            # Run lcov
            success, out, err = run_command(
                f"lcov --capture --directory {coverage_dir} --output-file {lcov_output} "
                f"--exclude '*/test/*' --exclude '*/build/*'"
            )
            
            if success and lcov_output.exists():
                print("✅ lcov completed")
                
                if tools_available['lcov_cobertura']:
                    # Convert to Cobertura
                    success, out, err = run_command(
                        f"lcov_cobertura {lcov_output} --output {cobertura_output}"
                    )
                    
                    if success and cobertura_output.exists():
                        print("✅ lcov_cobertura completed")
                        
                        if tools_available['pycobertura']:
                            # Generate markdown
                            success, out, err = run_command(
                                f"pycobertura show --format markdown {cobertura_output}"
                            )
                            
                            if success:
                                pycobertura_output.write_text(out)
                                print("✅ pycobertura completed")
        
        # === Compare results ===
        print("\n📊 Comparing results...")
        
        if fortcov_output.exists():
            fortcov_content = fortcov_output.read_text()
            fortcov_stats = extract_coverage_stats(fortcov_content)
            
            print(f"📄 fortcov report: {len(fortcov_content.splitlines())} lines")
            print(f"📊 fortcov coverage stats: {len(fortcov_stats)} files")
            
            # Show sample
            print("\n📋 fortcov sample output:")
            for line in fortcov_content.splitlines()[:5]:
                print(f"    {line}")
        
        if pycobertura_output.exists():
            pycobertura_content = pycobertura_output.read_text()
            reference_stats = extract_coverage_stats(pycobertura_content)
            
            print(f"\n📄 pycobertura report: {len(pycobertura_content.splitlines())} lines")
            print(f"📊 pycobertura coverage stats: {len(reference_stats)} files")
            
            # Show sample
            print("\n📋 pycobertura sample output:")
            for line in pycobertura_content.splitlines()[:5]:
                print(f"    {line}")
            
            # Compare statistics
            if fortcov_stats and reference_stats:
                differences = compare_coverage_stats(fortcov_stats, reference_stats)
                
                print(f"\n🔍 Coverage comparison:")
                print(f"  Files analyzed by fortcov: {len(fortcov_stats)}")
                print(f"  Files analyzed by pycobertura: {len(reference_stats)}")
                print(f"  Significant differences (>1%): {len(differences)}")
                
                if differences:
                    print("\n⚠️  Coverage differences detected:")
                    for diff in differences[:5]:  # Show first 5
                        print(f"    {diff['file']}: fortcov={diff['fortcov']:.1f}% "
                              f"vs reference={diff['reference']:.1f}% "
                              f"(diff={diff['difference']:.1f}%)")
                else:
                    print("✅ Coverage statistics are consistent!")
        
        # === Final validation ===
        print(f"\n🎯 Validation Summary:")
        print(f"  - fortcov execution: {'✅' if fortcov_success else '❌'}")
        print(f"  - lcov compatibility: {'✅' if lcov_output.exists() else '❌'}")
        print(f"  - Cobertura conversion: {'✅' if cobertura_output.exists() else '❌'}")
        print(f"  - Markdown generation: {'✅' if pycobertura_output.exists() else '❌'}")
        
        # Save comparison report
        report_file = temp_path / "comparison_report.txt"
        with open(report_file, 'w') as f:
            f.write("Toolchain Comparison Report\n")
            f.write("==========================\n\n")
            f.write(f"fortcov stats: {len(fortcov_stats)} files\n")
            f.write(f"reference stats: {len(reference_stats)} files\n")
            if 'differences' in locals():
                f.write(f"differences: {len(differences)}\n")
        
        print(f"\n📋 Detailed report saved: {report_file}")
        print("\n🏁 Comprehensive comparison complete!")
        
        return 0


if __name__ == "__main__":
    sys.exit(main())