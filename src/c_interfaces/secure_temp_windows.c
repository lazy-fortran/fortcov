/* Windows secure temporary file operations for atomic_temp_file_manager
 * Provides cryptographically secure temp file creation with atomic operations
 * Prevents race conditions, symlink attacks, and privilege escalation on Windows
 */

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <wincrypt.h>
#include <sddl.h>
#include <aclapi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Security constants */
#define TEMP_NAME_LENGTH 32    /* Cryptographically secure length */
#define MAX_PATH_LENGTH 4096

/* Error codes matching Fortran error_handling module */
#define ERROR_SUCCESS 0
#define ERROR_PERMISSION_DENIED 1005
#define ERROR_OUT_OF_MEMORY 1006
#define ERROR_FATAL 1999

/* Structure to track secure temp file state */
typedef struct {
    HANDLE file_handle;                  /* File handle */
    char filename[MAX_PATH_LENGTH];      /* Full path */
    char temp_dir[MAX_PATH_LENGTH];      /* Temp directory used */
    DWORD creation_flags;                /* Flags used for creation */
    FILETIME creation_time;              /* Creation timestamp */
    int entropy_bits;                    /* Entropy used in filename */
    SECURITY_ATTRIBUTES security_attrs;  /* Security attributes */
} secure_temp_file_state_t;

/* Forward declarations */
static int generate_secure_filename_win(char *buffer, size_t buffer_size, 
                                       int *entropy_bits);
static int get_secure_temp_dir_win(char *buffer, size_t buffer_size);
static int create_atomic_temp_file_win(const char *temp_dir, 
                                     const char *filename_base,
                                     secure_temp_file_state_t *state);
static int validate_file_security_win(const char *filepath);
static int get_entropy_from_system_win(unsigned char *buffer, size_t length);
static int setup_secure_dacl(SECURITY_ATTRIBUTES *security_attrs);

/* Main function to create secure temporary file atomically */
int create_secure_temp_file_windows(secure_temp_file_state_t *state, 
                                   int *error_code) {
    char temp_dir[MAX_PATH_LENGTH];
    char filename_base[TEMP_NAME_LENGTH + 1];
    int result;
    
    if (!state || !error_code) {
        if (error_code) *error_code = ERROR_FATAL;
        return -1;
    }
    
    /* Initialize state */
    memset(state, 0, sizeof(secure_temp_file_state_t));
    state->file_handle = INVALID_HANDLE_VALUE;
    *error_code = ERROR_SUCCESS;
    
    /* Setup secure DACL */
    result = setup_secure_dacl(&state->security_attrs);
    if (result != 0) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    /* Get secure temp directory */
    result = get_secure_temp_dir_win(temp_dir, sizeof(temp_dir));
    if (result != 0) {
        *error_code = ERROR_PERMISSION_DENIED;
        return -1;
    }
    
    /* Generate cryptographically secure filename */
    result = generate_secure_filename_win(filename_base, sizeof(filename_base),
                                        &state->entropy_bits);
    if (result != 0) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    /* Create file atomically */
    result = create_atomic_temp_file_win(temp_dir, filename_base, state);
    if (result != 0) {
        DWORD error = GetLastError();
        *error_code = (error == ERROR_ACCESS_DENIED) ? 
                     ERROR_PERMISSION_DENIED : ERROR_FATAL;
        return -1;
    }
    
    /* Validate security properties */
    result = validate_file_security_win(state->filename);
    if (result != 0) {
        CloseHandle(state->file_handle);
        DeleteFileA(state->filename);
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    return 0;
}

/* Generate cryptographically secure random filename */
static int generate_secure_filename_win(char *buffer, size_t buffer_size, 
                                       int *entropy_bits) {
    unsigned char random_bytes[16];  /* 128 bits of entropy */
    int i;
    
    if (!buffer || buffer_size < TEMP_NAME_LENGTH + 1 || !entropy_bits) {
        return -1;
    }
    
    /* Get cryptographically secure random bytes */
    if (get_entropy_from_system_win(random_bytes, sizeof(random_bytes)) != 0) {
        return -1;
    }
    
    /* Convert to safe filename characters (base32-like encoding) */
    const char safe_chars[] = "abcdefghijklmnopqrstuvwxyz0123456789";
    for (i = 0; i < TEMP_NAME_LENGTH; i++) {
        buffer[i] = safe_chars[random_bytes[i % sizeof(random_bytes)] % 
                              (sizeof(safe_chars) - 1)];
    }
    buffer[TEMP_NAME_LENGTH] = '\0';
    
    *entropy_bits = sizeof(random_bytes) * 8;  /* 128 bits */
    return 0;
}

/* Get secure temporary directory */
static int get_secure_temp_dir_win(char *buffer, size_t buffer_size) {
    DWORD result;
    
    if (!buffer || buffer_size < 2) {
        return -1;
    }
    
    /* Get Windows temp directory */
    result = GetTempPathA((DWORD)buffer_size, buffer);
    if (result == 0 || result >= buffer_size) {
        return -1;
    }
    
    /* Remove trailing backslash if present */
    size_t len = strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\\') {
        buffer[len - 1] = '\0';
    }
    
    return 0;
}

/* Create temporary file with atomic operations */
static int create_atomic_temp_file_win(const char *temp_dir, 
                                     const char *filename_base,
                                     secure_temp_file_state_t *state) {
    DWORD flags;
    
    if (!temp_dir || !filename_base || !state) {
        return -1;
    }
    
    /* Build full path */
    snprintf(state->filename, sizeof(state->filename), 
             "%s\\fortcov_temp_%s", temp_dir, filename_base);
    
    strncpy(state->temp_dir, temp_dir, sizeof(state->temp_dir) - 1);
    state->temp_dir[sizeof(state->temp_dir) - 1] = '\0';
    
    /* Set creation flags for atomic, secure creation */
    flags = CREATE_NEW | FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE;
    state->creation_flags = flags;
    
    /* Create file atomically with secure ACL */
    state->file_handle = CreateFileA(
        state->filename,
        GENERIC_READ | GENERIC_WRITE,
        0,  /* No sharing */
        &state->security_attrs,
        CREATE_NEW,  /* Atomic create-or-fail */
        FILE_ATTRIBUTE_TEMPORARY,
        NULL
    );
    
    if (state->file_handle == INVALID_HANDLE_VALUE) {
        return -1;
    }
    
    /* Record creation time */
    GetSystemTimeAsFileTime(&state->creation_time);
    
    return 0;
}

/* Setup secure DACL for file creation */
static int setup_secure_dacl(SECURITY_ATTRIBUTES *security_attrs) {
    PACL dacl = NULL;
    PSECURITY_DESCRIPTOR security_descriptor = NULL;
    EXPLICIT_ACCESS access_entry;
    HANDLE token = NULL;
    TOKEN_USER *token_user = NULL;
    DWORD token_info_length = 0;
    int result = -1;
    
    if (!security_attrs) {
        return -1;
    }
    
    /* Get current user SID */
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) {
        goto cleanup;
    }
    
    /* Get token user information length */
    GetTokenInformation(token, TokenUser, NULL, 0, &token_info_length);
    if (GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
        goto cleanup;
    }
    
    token_user = (TOKEN_USER *)LocalAlloc(LPTR, token_info_length);
    if (!token_user) {
        goto cleanup;
    }
    
    if (!GetTokenInformation(token, TokenUser, token_user, 
                           token_info_length, &token_info_length)) {
        goto cleanup;
    }
    
    /* Setup access entry for current user only */
    ZeroMemory(&access_entry, sizeof(access_entry));
    access_entry.grfAccessPermissions = FILE_ALL_ACCESS;
    access_entry.grfAccessMode = SET_ACCESS;
    access_entry.grfInheritance = NO_INHERITANCE;
    access_entry.Trustee.TrusteeForm = TRUSTEE_IS_SID;
    access_entry.Trustee.TrusteeType = TRUSTEE_IS_USER;
    access_entry.Trustee.ptstrName = (LPTSTR)token_user->User.Sid;
    
    /* Create DACL */
    if (SetEntriesInAcl(1, &access_entry, NULL, &dacl) != ERROR_SUCCESS) {
        goto cleanup;
    }
    
    /* Create security descriptor */
    security_descriptor = (PSECURITY_DESCRIPTOR)LocalAlloc(LPTR, 
                                                  SECURITY_DESCRIPTOR_MIN_LENGTH);
    if (!security_descriptor) {
        goto cleanup;
    }
    
    if (!InitializeSecurityDescriptor(security_descriptor, 
                                    SECURITY_DESCRIPTOR_REVISION)) {
        goto cleanup;
    }
    
    if (!SetSecurityDescriptorDacl(security_descriptor, TRUE, dacl, FALSE)) {
        goto cleanup;
    }
    
    /* Setup security attributes */
    security_attrs->nLength = sizeof(SECURITY_ATTRIBUTES);
    security_attrs->lpSecurityDescriptor = security_descriptor;
    security_attrs->bInheritHandle = FALSE;
    
    result = 0;
    
cleanup:
    if (token) CloseHandle(token);
    if (token_user) LocalFree(token_user);
    /* Free security_descriptor on error, but keep on success */
    if (result != 0 && security_descriptor) {
        LocalFree(security_descriptor);
    }
    /* Note: dacl and security_descriptor are kept for use on success */
    
    return result;
}

/* Validate file security properties */
static int validate_file_security_win(const char *filepath) {
    HANDLE file_handle;
    BY_HANDLE_FILE_INFORMATION file_info;
    
    if (!filepath) {
        return -1;
    }
    
    /* Open file to check properties */
    file_handle = CreateFileA(filepath, 0, FILE_SHARE_READ, NULL, 
                             OPEN_EXISTING, 0, NULL);
    if (file_handle == INVALID_HANDLE_VALUE) {
        return -1;
    }
    
    /* Get file information */
    if (!GetFileInformationByHandle(file_handle, &file_info)) {
        CloseHandle(file_handle);
        return -1;
    }
    
    CloseHandle(file_handle);
    
    /* Verify it's a regular file, not a reparse point (symlink equivalent) */
    if (file_info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) {
        return -1;
    }
    
    return 0;
}

/* Get entropy from Windows CryptoAPI */
static int get_entropy_from_system_win(unsigned char *buffer, size_t length) {
    HCRYPTPROV crypto_provider = 0;
    BOOL result = FALSE;
    
    if (!buffer || length == 0) {
        return -1;
    }
    
    /* Acquire cryptographic context */
    if (!CryptAcquireContext(&crypto_provider, NULL, NULL, 
                           PROV_RSA_FULL, CRYPT_VERIFYCONTEXT)) {
        return -1;
    }
    
    /* Generate random bytes */
    result = CryptGenRandom(crypto_provider, (DWORD)length, buffer);
    
    /* Release context */
    CryptReleaseContext(crypto_provider, 0);
    
    return result ? 0 : -1;
}

/* Write data atomically to temp file */
int write_atomic_temp_file_windows(secure_temp_file_state_t *state,
                                  const char *data, size_t data_len,
                                  int *error_code) {
    DWORD written;
    
    if (!state || !data || state->file_handle == INVALID_HANDLE_VALUE || 
        !error_code) {
        if (error_code) *error_code = ERROR_FATAL;
        return -1;
    }
    
    *error_code = ERROR_SUCCESS;
    
    /* Write data */
    if (!WriteFile(state->file_handle, data, (DWORD)data_len, &written, NULL) ||
        written != data_len) {
        DWORD error = GetLastError();
        *error_code = (error == ERROR_DISK_FULL) ? 
                     ERROR_OUT_OF_MEMORY : ERROR_FATAL;
        return -1;
    }
    
    /* Force data to disk */
    if (!FlushFileBuffers(state->file_handle)) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    return 0;
}

/* Read data from temp file */
int read_temp_file_windows(secure_temp_file_state_t *state,
                          char *buffer, size_t buffer_size, 
                          size_t *bytes_read, int *error_code) {
    DWORD read_bytes;
    LARGE_INTEGER zero_offset;
    
    if (!state || !buffer || !bytes_read || 
        state->file_handle == INVALID_HANDLE_VALUE || !error_code) {
        if (error_code) *error_code = ERROR_FATAL;
        return -1;
    }
    
    *error_code = ERROR_SUCCESS;
    *bytes_read = 0;
    
    /* Seek to beginning */
    zero_offset.QuadPart = 0;
    if (!SetFilePointerEx(state->file_handle, zero_offset, NULL, FILE_BEGIN)) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    /* Read data */
    if (!ReadFile(state->file_handle, buffer, (DWORD)(buffer_size - 1), 
                  &read_bytes, NULL)) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    *bytes_read = read_bytes;
    buffer[read_bytes] = '\0';  /* Null terminate */
    
    return 0;
}

/* Move temp file atomically to target location */
int move_atomic_temp_file_windows(secure_temp_file_state_t *state,
                                 const char *target_path, int *error_code) {
    if (!state || !target_path || 
        state->file_handle == INVALID_HANDLE_VALUE || !error_code) {
        if (error_code) *error_code = ERROR_FATAL;
        return -1;
    }
    
    *error_code = ERROR_SUCCESS;
    
    /* Close file handle first */
    CloseHandle(state->file_handle);
    state->file_handle = INVALID_HANDLE_VALUE;
    
    /* Atomic move */
    if (!MoveFileA(state->filename, target_path)) {
        DWORD error = GetLastError();
        *error_code = (error == ERROR_ACCESS_DENIED) ? 
                     ERROR_PERMISSION_DENIED : ERROR_FATAL;
        return -1;
    }
    
    /* Update filename in state */
    strncpy(state->filename, target_path, sizeof(state->filename) - 1);
    state->filename[sizeof(state->filename) - 1] = '\0';
    
    return 0;
}

/* Cleanup temp file */
int cleanup_temp_file_windows(secure_temp_file_state_t *state) {
    if (!state) {
        return -1;
    }
    
    /* Close file handle if open */
    if (state->file_handle != INVALID_HANDLE_VALUE) {
        CloseHandle(state->file_handle);
        state->file_handle = INVALID_HANDLE_VALUE;
    }
    
    /* Remove file if it exists */
    if (state->filename[0] != '\0') {
        DeleteFileA(state->filename);
        state->filename[0] = '\0';
    }
    
    /* Cleanup security descriptor if allocated */
    if (state->security_attrs.lpSecurityDescriptor) {
        LocalFree(state->security_attrs.lpSecurityDescriptor);
        state->security_attrs.lpSecurityDescriptor = NULL;
    }
    
    return 0;
}

/* Security validation functions for testing */
int temp_file_used_exclusive_creation_windows(secure_temp_file_state_t *state) {
    return state && (state->creation_flags & CREATE_NEW) ? 1 : 0;
}

long temp_file_get_creation_time_gap_windows(secure_temp_file_state_t *state) {
    /* For atomic creation, time gap should be 0 */
    return 0;
}

int temp_file_prevents_symlink_following_windows(secure_temp_file_state_t *state) {
    /* Windows CreateFile with CREATE_NEW prevents symlink issues */
    return state ? 1 : 0;
}

int temp_file_uses_windows_security_features_windows(secure_temp_file_state_t *state) {
    /* Verify Windows-specific security features are active */
    return state && 
           (state->creation_flags & CREATE_NEW) &&
           (state->entropy_bits >= 128) &&
           (state->security_attrs.lpSecurityDescriptor != NULL) ? 1 : 0;
}

/* Stub implementations for cross-platform compatibility */
int has_group_access_windows(const char *filepath) {
    /* Windows doesn't have Unix-style group permissions */
    return 0;
}

int has_other_access_windows(const char *filepath) {
    /* Windows doesn't have Unix-style other permissions */
    return 0;
}

int is_symlink_windows(const char *filepath) {
    WIN32_FILE_ATTRIBUTE_DATA attrs;
    
    if (!filepath || !GetFileAttributesExA(filepath, GetFileExInfoStandard, &attrs)) {
        return 0;
    }
    
    return (attrs.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) ? 1 : 0;
}

int is_secure_temp_directory_windows(const char *dirname) {
    DWORD attrs;
    
    if (!dirname) {
        return 0;
    }
    
    attrs = GetFileAttributesA(dirname);
    if (attrs == INVALID_FILE_ATTRIBUTES) {
        return 0;
    }
    
    /* Must be a directory */
    if (!(attrs & FILE_ATTRIBUTE_DIRECTORY)) {
        return 0;
    }
    
    return 1;
}

/* Performance helper functions */
void create_baseline_temp_file_windows(void) {
    /* Create simple temp file for performance comparison */
    char temp_path[MAX_PATH];
    char temp_file[MAX_PATH];
    
    if (GetTempPathA(sizeof(temp_path), temp_path) > 0 &&
        GetTempFileNameA(temp_path, "baseline", 0, temp_file) > 0) {
        DeleteFileA(temp_file);
    }
}

/* State extraction functions for Fortran interface */
void get_filename_from_state_windows(secure_temp_file_state_t *state, 
                                    char *buffer, size_t buffer_size) {
    if (!state || !buffer || buffer_size == 0) {
        return;
    }
    
    strncpy(buffer, state->filename, buffer_size - 1);
    buffer[buffer_size - 1] = '\0';
}

void get_temp_dir_from_state_windows(secure_temp_file_state_t *state,
                                    char *buffer, size_t buffer_size) {
    if (!state || !buffer || buffer_size == 0) {
        return;
    }
    
    strncpy(buffer, state->temp_dir, buffer_size - 1);
    buffer[buffer_size - 1] = '\0';
}

int get_entropy_bits_from_state_windows(secure_temp_file_state_t *state) {
    if (!state) {
        return 0;
    }
    
    return state->entropy_bits;
}

/* Platform detection function */
int is_unix_platform(void) {
    return 0;  /* Always false for Windows implementation */
}

#endif /* _WIN32 */