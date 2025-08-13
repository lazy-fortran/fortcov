# Issue #3: Implement File System Utilities

## User Story
As a coverage parser, I want file system utilities so that I can discover and read coverage data files reliably across platforms.

## Acceptance Criteria
- [ ] Find files matching glob patterns
- [ ] Resolve relative paths to absolute
- [ ] Read binary files into byte arrays
- [ ] Write text files with proper encoding

## Test Specifications (RED Phase)

### Test 1: Find files with extension pattern
**Given**: A directory with files: test.f90, test.gcda, test.gcno, other.txt
**When**: Calling find_files("*.gc*")
**Then**: Should return ["test.gcda", "test.gcno"]

### Test 2: Resolve relative path
**Given**: A relative path "./src/module.f90"
**When**: Calling resolve_path() from /home/user/project
**Then**: Should return "/home/user/project/src/module.f90"

### Test 3: Read binary file
**Given**: A binary file with bytes [0x47, 0x43, 0x4E, 0x4F]
**When**: Calling read_binary_file()
**Then**: Should return integer(1) array with values [71, 67, 78, 79]

### Test 4: Write text file
**Given**: A string "Coverage: 85.5%"
**When**: Calling write_text_file("report.md", content)
**Then**: File should exist with exact content

### Test 5: Handle non-existent file
**Given**: A path to non-existent file
**When**: Calling read_binary_file()
**Then**: Should return allocated empty array and set error flag

### Test 6: Find files recursively
**Given**: Nested directories with .f90 files
**When**: Calling find_files("**/*.f90")
**Then**: Should return all .f90 files in tree

### Test 7: Create directory if not exists
**Given**: A path to non-existent directory
**When**: Calling ensure_directory("output/reports")
**Then**: Directory should be created

## Implementation Notes (GREEN Phase)
- Use Fortran 2008 intrinsics for file operations
- Implement glob with directory traversal
- Handle platform-specific path separators

## Technical Details
- File: `src/file_utils.f90`
- Test file: `test/test_file_utils.f90`
- Dependencies: None
- Note: Create temp directories for testing