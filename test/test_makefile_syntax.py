#!/usr/bin/env python3
"""
Test Makefile syntax validation for issue #258.

This test validates that the Makefile in the make basic example
has correct syntax and can execute without syntax errors.
"""

import os
import subprocess
import tempfile
import unittest
from pathlib import Path


class TestMakefileSyntax(unittest.TestCase):
    """Test Makefile syntax validation for make basic example."""

    def setUp(self):
        """Set up test environment."""
        self.repo_root = Path(__file__).parent.parent
        self.makefile_dir = (
            self.repo_root / "examples" / "build_systems" / "make" / "basic_example"
        )
        self.makefile_path = self.makefile_dir / "Makefile"
        
        # Ensure test directory exists
        self.assertTrue(
            self.makefile_dir.exists(),
            f"Makefile directory does not exist: {self.makefile_dir}"
        )
        self.assertTrue(
            self.makefile_path.exists(),
            f"Makefile does not exist: {self.makefile_path}"
        )

    def test_makefile_syntax_validation(self):
        """Test that Makefile has valid syntax - no parse errors."""
        # Change to the Makefile directory
        original_cwd = os.getcwd()
        os.chdir(self.makefile_dir)
        
        try:
            # Test make dry-run to validate syntax without executing
            result = subprocess.run(
                ["make", "-n", "help"],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            # Should not fail with syntax errors
            self.assertEqual(
                result.returncode, 0,
                f"Makefile syntax error: {result.stderr}"
            )
            
            # Should not contain "missing separator" error
            self.assertNotIn(
                "missing separator",
                result.stderr.lower(),
                "Makefile contains missing separator error"
            )
            
        finally:
            os.chdir(original_cwd)

    def test_coverage_target_syntax(self):
        """Test that coverage target has valid syntax."""
        original_cwd = os.getcwd()
        os.chdir(self.makefile_dir)
        
        try:
            # Test coverage target dry-run to validate syntax
            result = subprocess.run(
                ["make", "-n", "coverage"],
                capture_output=True,
                text=True,
                timeout=10
            )
            
            # Should not fail with syntax errors
            self.assertEqual(
                result.returncode, 0,
                f"Coverage target syntax error: {result.stderr}"
            )
            
            # Should not contain "missing separator" error
            self.assertNotIn(
                "missing separator",
                result.stderr.lower(),
                "Coverage target contains missing separator error"
            )
            
            # Dry-run output should contain expected HTML generation commands
            self.assertIn("echo '<!DOCTYPE html>' > coverage.html", result.stdout)
            
        finally:
            os.chdir(original_cwd)

    def test_all_targets_syntax(self):
        """Test that all Makefile targets have valid syntax."""
        original_cwd = os.getcwd()
        os.chdir(self.makefile_dir)
        
        targets = [
            "all", "test", "coverage", "clean", "clean-coverage", 
            "distclean", "help"
        ]
        
        try:
            for target in targets:
                with self.subTest(target=target):
                    result = subprocess.run(
                        ["make", "-n", target],
                        capture_output=True,
                        text=True,
                        timeout=10
                    )
                    
                    # Should not fail with syntax errors
                    self.assertEqual(
                        result.returncode, 0,
                        f"Target '{target}' syntax error: {result.stderr}"
                    )
                    
                    # Should not contain "missing separator" error
                    self.assertNotIn(
                        "missing separator",
                        result.stderr.lower(),
                        f"Target '{target}' contains missing separator error"
                    )
                    
        finally:
            os.chdir(original_cwd)


if __name__ == "__main__":
    unittest.main()