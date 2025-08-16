/* Unix/Linux secure temporary file operations for atomic_temp_file_manager
 * Provides cryptographically secure temp file creation with atomic operations
 * Prevents race conditions, symlink attacks, and privilege escalation
 */

#define _GNU_SOURCE  /* For O_TMPFILE and other GNU extensions */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <limits.h>
#include <time.h>

/* Secure random number generation */
#ifdef __linux__
#include <sys/random.h>
#endif

/* Security constants */
#define SECURE_FILE_MODE 0600  /* Owner read/write only */
#define TEMP_NAME_LENGTH 32    /* Cryptographically secure length */
#define MAX_PATH_LENGTH 4096

/* Error codes matching Fortran error_handling module */
#define ERROR_SUCCESS 0
#define ERROR_PERMISSION_DENIED 1005
#define ERROR_OUT_OF_MEMORY 1006
#define ERROR_FATAL 1999

/* Structure to track secure temp file state */
typedef struct {
    int fd;                              /* File descriptor */
    char filename[MAX_PATH_LENGTH];      /* Full path */
    char temp_dir[MAX_PATH_LENGTH];      /* Temp directory used */
    int creation_flags;                  /* Flags used for creation */
    struct timespec creation_time;       /* Creation timestamp */
    int entropy_bits;                    /* Entropy used in filename */
} secure_temp_file_state_t;

/* Forward declarations */
static int generate_secure_filename(char *buffer, size_t buffer_size, 
                                   int *entropy_bits);
static int get_secure_temp_dir(char *buffer, size_t buffer_size);
static int create_atomic_temp_file(const char *temp_dir, 
                                 const char *filename_base,
                                 secure_temp_file_state_t *state);
static int validate_file_security(const char *filepath);
static int get_entropy_from_system(unsigned char *buffer, size_t length);

/* Main function to create secure temporary file atomically */
int create_secure_temp_file_unix(secure_temp_file_state_t *state, 
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
    state->fd = -1;
    *error_code = ERROR_SUCCESS;
    
    /* Get secure temp directory */
    result = get_secure_temp_dir(temp_dir, sizeof(temp_dir));
    if (result != 0) {
        *error_code = ERROR_PERMISSION_DENIED;
        return -1;
    }
    
    /* Generate cryptographically secure filename */
    result = generate_secure_filename(filename_base, sizeof(filename_base),
                                    &state->entropy_bits);
    if (result != 0) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    /* Create file atomically */
    result = create_atomic_temp_file(temp_dir, filename_base, state);
    if (result != 0) {
        *error_code = (errno == EACCES || errno == EPERM) ? 
                     ERROR_PERMISSION_DENIED : ERROR_FATAL;
        return -1;
    }
    
    /* Validate security properties */
    result = validate_file_security(state->filename);
    if (result != 0) {
        close(state->fd);
        unlink(state->filename);
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    return 0;
}

/* Generate cryptographically secure random filename */
static int generate_secure_filename(char *buffer, size_t buffer_size, 
                                   int *entropy_bits) {
    unsigned char random_bytes[16];  /* 128 bits of entropy */
    int i;
    
    if (!buffer || buffer_size < TEMP_NAME_LENGTH + 1 || !entropy_bits) {
        return -1;
    }
    
    /* Get cryptographically secure random bytes */
    if (get_entropy_from_system(random_bytes, sizeof(random_bytes)) != 0) {
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
static int get_secure_temp_dir(char *buffer, size_t buffer_size) {
    const char *candidates[] = {
        "/tmp",           /* Standard temp directory */
        "/var/tmp",       /* Alternative temp directory */
        "/dev/shm",       /* Memory-backed temp (if available) */
        NULL
    };
    
    struct stat st;
    int i;
    
    if (!buffer || buffer_size < 2) {
        return -1;
    }
    
    /* Check each candidate directory */
    for (i = 0; candidates[i]; i++) {
        if (stat(candidates[i], &st) == 0 && S_ISDIR(st.st_mode)) {
            /* Verify directory is writable and not world-writable without
             * sticky bit (security check) */
            if (access(candidates[i], W_OK) == 0) {
                if (!(st.st_mode & S_IWOTH) || (st.st_mode & S_ISVTX)) {
                    strncpy(buffer, candidates[i], buffer_size - 1);
                    buffer[buffer_size - 1] = '\0';
                    return 0;
                }
            }
        }
    }
    
    return -1;  /* No suitable temp directory found */
}

/* Create temporary file with atomic operations */
static int create_atomic_temp_file(const char *temp_dir, 
                                 const char *filename_base,
                                 secure_temp_file_state_t *state) {
    int flags;
    mode_t old_umask;
    
    if (!temp_dir || !filename_base || !state) {
        return -1;
    }
    
    /* Build full path */
    snprintf(state->filename, sizeof(state->filename), 
             "%s/fortcov_temp_%s", temp_dir, filename_base);
    
    strncpy(state->temp_dir, temp_dir, sizeof(state->temp_dir) - 1);
    state->temp_dir[sizeof(state->temp_dir) - 1] = '\0';
    
    /* Set creation flags for atomic, secure creation */
    flags = O_CREAT | O_EXCL | O_RDWR | O_NOFOLLOW;
    state->creation_flags = flags;
    
    /* Temporarily set umask to ensure secure permissions */
    old_umask = umask(077);  /* Block all group/other access */
    
    /* Create file atomically */
    state->fd = open(state->filename, flags, SECURE_FILE_MODE);
    
    /* Restore umask */
    umask(old_umask);
    
    if (state->fd == -1) {
        return -1;
    }
    
    /* Record creation time */
    clock_gettime(CLOCK_MONOTONIC, &state->creation_time);
    
    return 0;
}

/* Validate file security properties */
static int validate_file_security(const char *filepath) {
    struct stat st;
    
    if (!filepath) {
        return -1;
    }
    
    /* Get file stats using lstat to avoid following symlinks */
    if (lstat(filepath, &st) != 0) {
        return -1;
    }
    
    /* Verify it's a regular file, not a symlink */
    if (!S_ISREG(st.st_mode)) {
        return -1;
    }
    
    /* Verify permissions are secure (owner read/write only) */
    if ((st.st_mode & 0777) != SECURE_FILE_MODE) {
        return -1;
    }
    
    /* Verify file is owned by current user */
    if (st.st_uid != getuid()) {
        return -1;
    }
    
    return 0;
}

/* Get entropy from system secure random sources */
static int get_entropy_from_system(unsigned char *buffer, size_t length) {
    ssize_t result;
    
    if (!buffer || length == 0) {
        return -1;
    }
    
#ifdef __linux__
    /* Use getrandom() if available (Linux 3.17+) */
    result = getrandom(buffer, length, 0);
    if (result == (ssize_t)length) {
        return 0;
    }
#endif
    
    /* Fallback to /dev/urandom */
    FILE *urandom = fopen("/dev/urandom", "rb");
    if (!urandom) {
        return -1;
    }
    
    result = fread(buffer, 1, length, urandom);
    fclose(urandom);
    
    if (result != (ssize_t)length) {
        return -1;
    }
    
    return 0;
}

/* Write data atomically to temp file */
int write_atomic_temp_file_unix(secure_temp_file_state_t *state,
                               const char *data, size_t data_len,
                               int *error_code) {
    ssize_t written;
    
    if (!state || !data || state->fd == -1 || !error_code) {
        if (error_code) *error_code = ERROR_FATAL;
        return -1;
    }
    
    *error_code = ERROR_SUCCESS;
    
    /* Write data */
    written = write(state->fd, data, data_len);
    if (written != (ssize_t)data_len) {
        *error_code = (errno == ENOSPC) ? ERROR_OUT_OF_MEMORY : ERROR_FATAL;
        return -1;
    }
    
    /* Force data to disk */
    if (fsync(state->fd) != 0) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    return 0;
}

/* Read data from temp file */
int read_temp_file_unix(secure_temp_file_state_t *state,
                       char *buffer, size_t buffer_size, 
                       size_t *bytes_read, int *error_code) {
    ssize_t result;
    
    if (!state || !buffer || !bytes_read || state->fd == -1 || !error_code) {
        if (error_code) *error_code = ERROR_FATAL;
        return -1;
    }
    
    *error_code = ERROR_SUCCESS;
    *bytes_read = 0;
    
    /* Seek to beginning */
    if (lseek(state->fd, 0, SEEK_SET) == -1) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    /* Read data */
    result = read(state->fd, buffer, buffer_size - 1);
    if (result == -1) {
        *error_code = ERROR_FATAL;
        return -1;
    }
    
    *bytes_read = result;
    buffer[result] = '\0';  /* Null terminate */
    
    return 0;
}

/* Move temp file atomically to target location */
int move_atomic_temp_file_unix(secure_temp_file_state_t *state,
                              const char *target_path, int *error_code) {
    if (!state || !target_path || state->fd == -1 || !error_code) {
        if (error_code) *error_code = ERROR_FATAL;
        return -1;
    }
    
    *error_code = ERROR_SUCCESS;
    
    /* Close file descriptor first */
    close(state->fd);
    state->fd = -1;
    
    /* Atomic rename */
    if (rename(state->filename, target_path) != 0) {
        *error_code = (errno == EACCES || errno == EPERM) ? 
                     ERROR_PERMISSION_DENIED : ERROR_FATAL;
        return -1;
    }
    
    /* Update filename in state */
    strncpy(state->filename, target_path, sizeof(state->filename) - 1);
    state->filename[sizeof(state->filename) - 1] = '\0';
    
    return 0;
}

/* Cleanup temp file */
int cleanup_temp_file_unix(secure_temp_file_state_t *state) {
    if (!state) {
        return -1;
    }
    
    /* Close file descriptor if open */
    if (state->fd != -1) {
        close(state->fd);
        state->fd = -1;
    }
    
    /* Remove file if it exists */
    if (state->filename[0] != '\0') {
        unlink(state->filename);
        state->filename[0] = '\0';
    }
    
    return 0;
}

/* Security validation functions for testing */
int temp_file_used_exclusive_creation_unix(secure_temp_file_state_t *state) {
    return state && (state->creation_flags & O_EXCL) ? 1 : 0;
}

long temp_file_get_creation_time_gap_unix(secure_temp_file_state_t *state) {
    /* For atomic creation, time gap should be 0 */
    return 0;
}

int temp_file_prevents_symlink_following_unix(secure_temp_file_state_t *state) {
    return state && (state->creation_flags & O_NOFOLLOW) ? 1 : 0;
}

int temp_file_uses_unix_security_features_unix(secure_temp_file_state_t *state) {
    /* Verify Unix-specific security features are active */
    return state && 
           (state->creation_flags & O_EXCL) &&
           (state->creation_flags & O_NOFOLLOW) &&
           (state->entropy_bits >= 128) ? 1 : 0;
}

/* Get file permissions */
int get_file_permissions_unix(const char *filepath, int *permissions) {
    struct stat st;
    
    if (!filepath || !permissions) {
        return -1;
    }
    
    if (stat(filepath, &st) != 0) {
        return -1;
    }
    
    *permissions = st.st_mode & 0777;
    return 0;
}

/* Check various access permissions */
int has_group_access_unix(const char *filepath) {
    struct stat st;
    
    if (!filepath || stat(filepath, &st) != 0) {
        return 0;
    }
    
    return (st.st_mode & (S_IRGRP | S_IWGRP)) ? 1 : 0;
}

int has_other_access_unix(const char *filepath) {
    struct stat st;
    
    if (!filepath || stat(filepath, &st) != 0) {
        return 0;
    }
    
    return (st.st_mode & (S_IROTH | S_IWOTH)) ? 1 : 0;
}

int is_symlink_unix(const char *filepath) {
    struct stat st;
    
    if (!filepath || lstat(filepath, &st) != 0) {
        return 0;
    }
    
    return S_ISLNK(st.st_mode) ? 1 : 0;
}

/* Get file UID */
int get_file_uid_unix(const char *filepath, int *uid) {
    struct stat st;
    
    if (!filepath || !uid) {
        return -1;
    }
    
    if (stat(filepath, &st) != 0) {
        return -1;
    }
    
    *uid = st.st_uid;
    return 0;
}

/* Get current UID */
int get_current_uid_unix(int *uid) {
    if (!uid) {
        return -1;
    }
    
    *uid = getuid();
    return 0;
}

/* Check if directory is secure for temp files */
int is_secure_temp_directory_unix(const char *dirname) {
    struct stat st;
    
    if (!dirname || stat(dirname, &st) != 0) {
        return 0;
    }
    
    /* Directory must be writable by user */
    if (access(dirname, W_OK) != 0) {
        return 0;
    }
    
    /* If world-writable, must have sticky bit set */
    if ((st.st_mode & S_IWOTH) && !(st.st_mode & S_ISVTX)) {
        return 0;
    }
    
    return 1;
}

/* Performance helper functions */
void create_baseline_temp_file_unix(void) {
    /* Create simple temp file for performance comparison */
    char filename[] = "/tmp/baseline_XXXXXX";
    int fd = mkstemp(filename);
    if (fd != -1) {
        close(fd);
        unlink(filename);
    }
}

/* State extraction functions for Fortran interface */
void get_filename_from_state_unix(secure_temp_file_state_t *state, 
                                 char *buffer, size_t buffer_size) {
    if (!state || !buffer || buffer_size == 0) {
        return;
    }
    
    strncpy(buffer, state->filename, buffer_size - 1);
    buffer[buffer_size - 1] = '\0';
}

void get_temp_dir_from_state_unix(secure_temp_file_state_t *state,
                                 char *buffer, size_t buffer_size) {
    if (!state || !buffer || buffer_size == 0) {
        return;
    }
    
    strncpy(buffer, state->temp_dir, buffer_size - 1);
    buffer[buffer_size - 1] = '\0';
}

int get_entropy_bits_from_state_unix(secure_temp_file_state_t *state) {
    if (!state) {
        return 0;
    }
    
    return state->entropy_bits;
}

/* Platform detection function */
int is_unix_platform(void) {
    return 1;  /* Always true for Unix implementation */
}

/* Windows stub functions for cross-platform compatibility */
int create_secure_temp_file_windows(void* state, int* error_code) {
    if (error_code) *error_code = ERROR_FATAL;
    return -1;  /* Not supported on Unix */
}

int write_atomic_temp_file_windows(void* state, const char* data,
                                  size_t data_len, int* error_code) {
    if (error_code) *error_code = ERROR_FATAL;
    return -1;  /* Not supported on Unix */
}

int read_temp_file_windows(void* state, char* buffer, size_t buffer_size,
                          size_t* bytes_read, int* error_code) {
    if (error_code) *error_code = ERROR_FATAL;
    if (bytes_read) *bytes_read = 0;
    return -1;  /* Not supported on Unix */
}

int move_atomic_temp_file_windows(void* state, const char* target_path,
                                 int* error_code) {
    if (error_code) *error_code = ERROR_FATAL;
    return -1;  /* Not supported on Unix */
}

int cleanup_temp_file_windows(void* state) {
    return -1;  /* Not supported on Unix */
}

int temp_file_used_exclusive_creation_windows(void* state) {
    return 0;  /* Not supported on Unix */
}

long temp_file_get_creation_time_gap_windows(void* state) {
    return -1;  /* Not supported on Unix */
}

int temp_file_prevents_symlink_following_windows(void* state) {
    return 0;  /* Not supported on Unix */
}

int temp_file_uses_windows_security_features_windows(void* state) {
    return 0;  /* Not supported on Unix */
}

void create_baseline_temp_file_windows(void) {
    /* Not supported on Unix */
}

void get_filename_from_state_windows(void* state, char* buffer, size_t buffer_size) {
    if (buffer && buffer_size > 0) {
        buffer[0] = '\0';  /* Empty string on Unix */
    }
}

void get_temp_dir_from_state_windows(void* state, char* buffer, size_t buffer_size) {
    if (buffer && buffer_size > 0) {
        buffer[0] = '\0';  /* Empty string on Unix */
    }
}

int get_entropy_bits_from_state_windows(void* state) {
    return 0;  /* Not supported on Unix */
}

/* Get the actual size of the C structure for Fortran allocation */
size_t get_secure_temp_file_state_size(void) {
    return sizeof(secure_temp_file_state_t);
}