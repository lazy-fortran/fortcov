# Issue #5: Implement GCov Binary Format Reader

## User Story
As a GCC/gfortran user, I want to parse .gcno and .gcda binary files so that I can analyze coverage data from my Fortran programs.

## Acceptance Criteria
- [ ] Parse .gcno file format (compile-time graph)
- [ ] Parse .gcda file format (runtime counters)
- [ ] Handle different GCC version formats
- [ ] Extract source file paths and line mappings

## Test Specifications (RED Phase)

### Test 1: Read GCNO magic number
**Given**: A valid .gcno file
**When**: Reading first 4 bytes
**Then**: Should match GCNO magic (0x67636E6F or "gcno")

### Test 2: Parse GCNO version
**Given**: A .gcno file header
**When**: Reading version field
**Then**: Should identify GCC version (e.g., "A03*" for GCC 10+)

### Test 3: Extract function records
**Given**: A .gcno file with 3 functions
**When**: Parsing function records
**Then**: Should return 3 function entries with names and line numbers

### Test 4: Read GCDA magic number
**Given**: A valid .gcda file
**When**: Reading first 4 bytes
**Then**: Should match GCDA magic (0x67636461 or "gcda")

### Test 5: Parse execution counters
**Given**: A .gcda file with arc counters
**When**: Reading counter sections
**Then**: Should return execution counts for each arc

### Test 6: Match GCNO and GCDA data
**Given**: Corresponding .gcno and .gcda files
**When**: Parsing both files
**Then**: Function checksums should match

### Test 7: Handle big-endian/little-endian
**Given**: Binary data in different endianness
**When**: Reading multi-byte integers
**Then**: Should correctly interpret based on system

### Test 8: Parse source file paths
**Given**: A .gcno with embedded source paths
**When**: Reading string table
**Then**: Should extract correct file paths

### Test 9: Handle missing GCDA file
**Given**: Only .gcno file present (no execution)
**When**: Parsing coverage
**Then**: Should show 0 execution counts

## Implementation Notes (GREEN Phase)
- Use stream I/O for binary reading
- Implement checksum verification
- Build arc graph for flow analysis
- Map arcs to source lines

## Technical Details
- File: `src/gcov_binary_format.f90`
- Test file: `test/test_gcov_binary_format.f90`
- Dependencies: file_utils
- Test data: Create minimal .gcno/.gcda files for testing
- Reference: GCC gcov-io.h for format specification