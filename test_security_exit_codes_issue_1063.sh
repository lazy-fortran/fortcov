#!/usr/bin/env bash
set -euo pipefail

echo "=========================================="
echo " Security Exit Codes Regression Test (#1063)"
echo "=========================================="

# Build the binary
fpm build

FORTCOV_BIN=$(ls -1 build/*/app/fortcov | head -n1)

total=0
failed=0

run_case() {
  local desc="$1" expected=$2 cmd="$3"
  total=$((total+1))
  echo "\nTest $total: $desc"
  echo "Command: $cmd"
  set +e
  eval "$cmd" >/dev/null 2>&1
  local code=$?
  set -e
  echo "Exit code: $code (expected: $expected)"
  if [[ "$code" -eq "$expected" ]]; then
    echo "✅ PASS"
  else
    echo "❌ FAIL"
    failed=$((failed+1))
  fi
}

# Case 1: Path traversal in --source should be a security violation (exit 4)
run_case "Path traversal in source" 4 "$FORTCOV_BIN --source=../etc --quiet"

# Prepare a minimal gcov file to satisfy positional coverage input
echo -e "TN:\nSF:dummy.f90\nDA:1,1\nend_of_record" > dummy_1063.gcov

# Case 2: Absolute system file in --output is forbidden (exit 4)
run_case "System file output path" 4 "$FORTCOV_BIN --source=src dummy_1063.gcov --output=/dev/null --quiet"

echo "\n=========================================="
echo " Summary: $((total-failed))/$total passed"
echo "=========================================="

exit $failed

