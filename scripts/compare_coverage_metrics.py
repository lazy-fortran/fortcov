#!/usr/bin/env python3
"""
Compare overall coverage metrics between Cobertura XML (gcovr) and FortCov's
Markdown report. Fails with non-zero exit if there is a mismatch.

Usage:
  scripts/compare_coverage_metrics.py coverage.xml coverage.md

Comparison:
  - Overall coverage rate percentage only. Raw line counts can differ
    significantly between tools due to parsing heuristics and file set
    differences, so they are reported for visibility but not enforced.

This script intentionally keeps scope minimal per issue request: compare
headline totals only. Per-file comparisons can be added later if needed.
"""

from __future__ import annotations

import sys
import os
import re
import xml.etree.ElementTree as ET


def parse_cobertura_totals(xml_path: str) -> tuple[int, int, float]:
    tree = ET.parse(xml_path)
    root = tree.getroot()
    # Cobertura root has attributes lines-valid, lines-covered, line-rate
    lv_attr = root.get("lines-valid")
    lc_attr = root.get("lines-covered")
    lr_attr = root.get("line-rate")

    if lv_attr and lc_attr and lr_attr:
        lines_valid = int(float(lv_attr))
        lines_covered = int(float(lc_attr))
        line_rate = float(lr_attr)
        return lines_valid, lines_covered, line_rate

    # Fallback: compute by summing line elements
    lines_valid = 0
    lines_covered = 0
    for line in root.findall(".//line"):
        hits = int(line.get("hits", "0"))
        lines_valid += 1
        if hits > 0:
            lines_covered += 1
    line_rate = (lines_covered / lines_valid) if lines_valid else 0.0
    return lines_valid, lines_covered, line_rate


def parse_fortcov_totals(md_path: str) -> tuple[int, int, float]:
    # Expect a final TOTAL row like:
    # | TOTAL | 1124 | 387 | 34.43% |  |
    total_row = None
    with open(md_path, "r", encoding="utf-8") as f:
        for line in f:
            if line.strip().startswith("| TOTAL "):
                total_row = line.strip()

    if not total_row:
        raise RuntimeError("Could not find TOTAL row in coverage.md")

    # Split markdown table row by '|'
    parts = [p.strip() for p in total_row.strip("|\n").split("|")]
    # Expected columns: ["TOTAL", "Stmts", "Covered", "Cover", "Missing"]
    if len(parts) < 5:
        raise RuntimeError(f"Unexpected TOTAL row format: {total_row}")

    try:
        stmts = int(parts[1])
        covered = int(parts[2])
        rate_str = parts[3]
        m = re.match(r"([0-9]+\.[0-9]+)%", rate_str)
        if not m:
            # Allow integer percent
            m = re.match(r"([0-9]+)%", rate_str)
        if not m:
            raise ValueError("Could not parse percentage")
        rate = float(m.group(1)) / 100.0
    except Exception as e:
        raise RuntimeError(f"Failed to parse TOTAL row: {total_row}\n{e}")

    return stmts, covered, rate


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print("Usage: scripts/compare_coverage_metrics.py coverage.xml coverage.md",
              file=sys.stderr)
        return 2

    xml_path, md_path = argv[1], argv[2]

    cob_lv, cob_lc, cob_lr = parse_cobertura_totals(xml_path)
    fort_stmts, fort_cov, fort_lr = parse_fortcov_totals(md_path)

    # Print a concise summary for CI logs
    print(f"Cobertura: lines-valid={cob_lv} lines-covered={cob_lc} line-rate={cob_lr:.6f}")
    print(f"FortCov : stmts={fort_stmts} covered={fort_cov} line-rate={fort_lr:.6f}")

    # Compare only the overall coverage rate with a tolerance.
    # Line counts often differ across tools due to heuristics and file sets.
    # Allow configuring tolerance via env to accommodate CI environments.
    tol_env = os.environ.get("FORTCOV_COMPARE_TOL", "0.30")
    try:
        tol = float(tol_env)
    except ValueError:
        tol = 0.30
    if abs(cob_lr - fort_lr) > tol:
        print(
            f"Mismatch: line-rate: {cob_lr:.6f} != {fort_lr:.6f} (tol={tol})",
            file=sys.stderr,
        )
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
