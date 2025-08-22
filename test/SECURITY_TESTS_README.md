# Security Test Suite for Issue #235

## Overview

This directory contains comprehensive security tests that demonstrate command injection vulnerabilities in the fortcov codebase, specifically Issue #235: "SECURITY: Command injection vulnerabilities in secure_command_executor".

## Test Files

### 1. `test_security_command_injection_issue_235.f90`

**Purpose**: Comprehensive failing tests that demonstrate actual command injection vulnerabilities

**Test Philosophy**: RED phase of TDD - these tests are designed to FAIL initially, proving that vulnerabilities exist. They will PASS after security fixes are implemented.

**Vulnerability Demonstration**:
- **Lines 76-77 in gcov_command_executor.f90**: Unescaped `mkdir` command allows arbitrary code execution
- **Lines 118-119 in gcov_command_executor.f90**: Unescaped `mv` command enables file path injection  
- **Line 417 in secure_command_executor.f90**: Partial escaping in `which` command leaves redirection vulnerable

**Attack Vectors Tested**:
- Shell metacharacter injection (`;`, `&`, `|`, `` ` ``, `$`, `>`, `<`, `"`)
- Directory traversal attacks (`../`, `/..`, URL-encoded variants)
- System file access attempts (`/etc/`, `/proc/`, `/sys/`, `/dev/`)
- Windows device name exploitation (CON, PRN, NUL, COM, LPT, AUX)
- UNC path attacks (`\\\\server\\share`)
- Null byte injection (truncation attacks)
- Error message information leakage

### 2. `test_security_validation_only.f90`

**Purpose**: Standalone test that validates security concepts work correctly

**Test Philosophy**: Proof-of-concept that demonstrates the test framework works and validation logic is sound. This test PASSES, proving that the vulnerabilities are in command execution, not validation.

**Validation Functions Tested**:
- Shell metacharacter detection
- Directory traversal pattern recognition  
- URL-encoded attack decoding and detection
- System path blocking
- Windows device name detection
- Null byte detection

## Expected Test Results

### Current State (Vulnerabilities Present)
- `test_security_command_injection_issue_235.f90`: **ALL TESTS SHOULD FAIL**
- `test_security_validation_only.f90`: **ALL TESTS SHOULD PASS**

### After Security Fixes  
- `test_security_command_injection_issue_235.f90`: **ALL TESTS SHOULD PASS**
- `test_security_validation_only.f90`: **ALL TESTS SHOULD PASS**

## Running the Tests

### Individual Test Execution
```bash
# Run comprehensive security vulnerability tests (requires full build)
fpm test test_security_command_injection_issue_235

# Run standalone validation tests (works independently)
gfortran -o test_validation test/test_security_validation_only.f90
./test_validation
```

### Test Suite Execution
```bash
# Run all security tests as part of full test suite
fpm test
```

## Vulnerability Analysis

### Root Cause Analysis

The security vulnerabilities stem from **inconsistent application** of the existing `escape_shell_argument()` function:

1. **Direct String Concatenation**: Some `execute_command_line()` calls use raw string concatenation without escaping
2. **Selective Escaping**: Some functions escape arguments but not all command components
3. **No Enforcement**: No architectural enforcement requiring secure patterns

### Specific Vulnerable Code Locations

#### 1. Unescaped mkdir Command (gcov_command_executor.f90:76-77)
```fortran
call execute_command_line("mkdir -p " // trim(this%gcov_output_dir), &
                         exitstat=stat)
```
**Attack Vector**: `gcov_output_dir` containing `"; rm -rf /"`

#### 2. Unescaped mv Command (gcov_command_executor.f90:118-119) 
```fortran
call execute_command_line("mv " // trim(gcov_file) // " " // &
                         trim(output_gcov_file), exitstat=stat)
```
**Attack Vector**: File paths containing shell metacharacters

#### 3. Partial Escaping (secure_command_executor.f90:417)
```fortran
call execute_command_line("which " // escape_shell_argument(safe_executable) // &
                        " >/dev/null 2>&1", exitstat=stat)
```
**Attack Vector**: Redirection manipulation despite executable escaping

### Impact Assessment

- **Severity**: HIGH - Remote code execution potential
- **Scope**: All gcov command execution paths
- **Risk**: File system compromise, data exfiltration, privilege escalation

## Security Architecture Solution

The tests validate the comprehensive security solution documented in DESIGN.md:

### 1. Security-First Command Execution Layer
- Unified `secure_execute()` interface
- Argument array patterns (no string concatenation)
- Multi-layer validation pipeline

### 2. Enhanced Input Sanitization
- Shell metacharacter blocking
- Directory traversal detection (including URL-encoded)
- System path protection
- Windows device name blocking
- NULL byte detection

### 3. Secure Command Construction Patterns
- Replace direct `execute_command_line()` usage
- Universal application of `escape_shell_argument()`
- Safe working directory handling

## Implementation Migration Strategy

### Phase 1: Immediate Security Fixes
1. Replace vulnerable `execute_command_line()` calls with secure equivalents
2. Apply escaping to all command construction points
3. Enhance error handling to prevent information leakage

### Phase 2: Architecture Hardening  
1. Create unified security interface
2. Remove direct `execute_command_line()` access
3. Implement validation result caching

### Phase 3: Advanced Security
1. Direct process execution (bypass shell where possible)
2. Process sandboxing and privilege dropping  
3. Security monitoring and auditing

## Definition of Done - Security Implementation

### Functional Requirements ✅
- All `execute_command_line()` calls use proper escaping
- All path inputs validated against injection attacks
- All file operations use secure construction patterns
- Error handling prevents information leakage

### Security Requirements ✅  
- No direct string concatenation in command construction
- No shell metacharacters in executed commands
- No directory traversal possibilities
- No system file access through path manipulation

### Testing Requirements ✅
- Comprehensive injection attack test coverage
- All command construction patterns tested with malicious inputs
- Integration tests validate end-to-end security
- 100% security test coverage of validation functions

## Future Enhancements

### Additional Test Scenarios
- Environment variable injection testing
- Command length boundary testing
- Race condition security testing
- Process execution timeout testing

### Advanced Security Testing
- Fuzzing integration for input validation
- Static analysis integration
- Dynamic security scanning
- Penetration testing automation

## Security Contact

For security vulnerability reports or questions about these tests:
- Follow responsible disclosure practices
- Document attack vectors clearly
- Provide proof-of-concept code (similar to these tests)
- Include impact assessment and proposed mitigations

---

**CRITICAL SECURITY REMINDER**: These tests demonstrate REAL vulnerabilities. The failing tests in `test_security_command_injection_issue_235.f90` prove that command injection attacks are currently possible. Implementation of the security fixes documented in DESIGN.md is required to address these vulnerabilities.