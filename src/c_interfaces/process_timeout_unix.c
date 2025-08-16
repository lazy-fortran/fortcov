/*
 * Unix process timeout management interface
 * 
 * This C interface provides Unix-specific process management functionality
 * for command timeout protection. It includes process creation, monitoring,
 * termination, and resource cleanup with security hardening.
 */

#ifdef __unix__

#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/select.h>

/* Process handle structure for timeout management */
typedef struct {
    pid_t pid;
    int status;
    time_t start_time;
    int timeout_seconds;
    int terminated;
    int sync_pipe[2];  /* Pipe for parent-child synchronization */
} process_handle_t;

/* Maximum argument count for secure parsing */
#define MAX_ARGS 64
#define MAX_ARG_LENGTH 1024

/* Secure argument parsing structure */
typedef struct {
    char* argv[MAX_ARGS];
    int argc;
    char* program;
} parsed_command_t;

/* Error codes for C interface */
#define PROCESS_SUCCESS 0
#define PROCESS_ERROR_CREATION_FAILED -1
#define PROCESS_ERROR_MONITORING_FAILED -2
#define PROCESS_ERROR_TERMINATION_FAILED -3
#define PROCESS_ERROR_CLEANUP_FAILED -4
#define PROCESS_ERROR_INVALID_PARAMS -5
#define PROCESS_ERROR_SECURITY_VIOLATION -6

/* Forward declarations for security functions */
static int validate_command_security(const char* command);
static int parse_command_securely(const char* command, parsed_command_t* parsed);
static void cleanup_parsed_command(parsed_command_t* parsed);
static int is_safe_program(const char* program);

/* Security validation: Check for dangerous patterns */
static int validate_command_security(const char* command) {
    if (!command || strlen(command) == 0) {
        return 0; /* Invalid command */
    }
    
    size_t cmd_len = strlen(command);
    
    /* SECURITY FIX: Check command length (max 256 characters) */
    if (cmd_len > 256) {
        return 0; /* Command too long - buffer overflow protection */
    }
    
    /* SECURITY FIX: Check for repetitive patterns (>20 consecutive chars) */
    if (cmd_len > 20) {
        int consecutive_count = 1;
        char prev_char = command[0];
        
        for (size_t i = 1; i < cmd_len; i++) {
            if (command[i] == prev_char) {
                consecutive_count++;
                if (consecutive_count > 20) {
                    return 0; /* Repetitive pattern attack detected */
                }
            } else {
                consecutive_count = 1;
                prev_char = command[i];
            }
        }
    }
    
    /* Block shell metacharacters that enable injection */
    const char* dangerous_patterns[] = {
        ";", "&&", "||", "|", "`", "$", "$(", "${", 
        ">", ">>", "<", "<<", "&", "~", "*", "?",
        "\n", "\r", "\t", NULL
    };
    
    for (int i = 0; dangerous_patterns[i] != NULL; i++) {
        if (strstr(command, dangerous_patterns[i]) != NULL) {
            return 0; /* Dangerous pattern found */
        }
    }
    
    /* Block attempts to access sensitive paths */
    const char* dangerous_paths[] = {
        "/etc/", "/root/", "/sys/", "/proc/", 
        "/dev/", "/var/log/", "sudo", "su", NULL
    };
    
    for (int i = 0; dangerous_paths[i] != NULL; i++) {
        if (strstr(command, dangerous_paths[i]) != NULL) {
            return 0; /* Dangerous path found */
        }
    }
    
    return 1; /* Command appears safe */
}

/* Secure command parsing without shell interpretation */
static int parse_command_securely(const char* command, parsed_command_t* parsed) {
    if (!command || !parsed) {
        return -1;
    }
    
    /* Initialize parsed structure */
    memset(parsed, 0, sizeof(parsed_command_t));
    
    /* Create working copy of command */
    size_t cmd_len = strlen(command);
    if (cmd_len >= MAX_ARG_LENGTH) {
        return -1; /* Command too long */
    }
    
    char* work_cmd = malloc(cmd_len + 1);
    if (!work_cmd) {
        return -1;
    }
    strcpy(work_cmd, command);
    
    /* Parse arguments separated by spaces */
    char* token = strtok(work_cmd, " ");
    parsed->argc = 0;
    
    while (token && parsed->argc < MAX_ARGS - 1) {
        size_t token_len = strlen(token);
        parsed->argv[parsed->argc] = malloc(token_len + 1);
        if (!parsed->argv[parsed->argc]) {
            free(work_cmd);
            cleanup_parsed_command(parsed);
            return -1;
        }
        strcpy(parsed->argv[parsed->argc], token);
        parsed->argc++;
        token = strtok(NULL, " ");
    }
    
    /* Null-terminate argv array */
    parsed->argv[parsed->argc] = NULL;
    
    /* Set program name */
    if (parsed->argc > 0) {
        size_t prog_len = strlen(parsed->argv[0]);
        parsed->program = malloc(prog_len + 1);
        if (parsed->program) {
            strcpy(parsed->program, parsed->argv[0]);
        }
    }
    
    free(work_cmd);
    return (parsed->argc > 0) ? 0 : -1;
}

/* Clean up parsed command structure */
static void cleanup_parsed_command(parsed_command_t* parsed) {
    if (!parsed) return;
    
    for (int i = 0; i < parsed->argc; i++) {
        if (parsed->argv[i]) {
            free(parsed->argv[i]);
            parsed->argv[i] = NULL;
        }
    }
    
    if (parsed->program) {
        free(parsed->program);
        parsed->program = NULL;
    }
    
    parsed->argc = 0;
}

/* Check if program is in safe whitelist */
static int is_safe_program(const char* program) {
    if (!program) return 0;
    
    /* Whitelist of safe programs - only basic utilities */
    const char* safe_programs[] = {
        "echo", "cat", "ls", "pwd", "sleep", "test", 
        "true", "false", "wc", "head", "tail", "sort",
        "gfortran", "gcc", "make", "fpm", NULL
    };
    
    /* Extract just the program name (no path) */
    const char* prog_name = strrchr(program, '/');
    if (prog_name) {
        prog_name++; /* Skip the '/' */
    } else {
        prog_name = program;
    }
    
    for (int i = 0; safe_programs[i] != NULL; i++) {
        if (strcmp(prog_name, safe_programs[i]) == 0) {
            return 1; /* Program is safe */
        }
    }
    
    return 0; /* Program not in whitelist */
}

/* Create process with timeout protection */
int create_process_with_timeout(const char* command, 
                               const char* working_dir,
                               int timeout_seconds,
                               process_handle_t* handle) {
    if (!command || !handle || timeout_seconds <= 0) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    /* Security validation - CRITICAL: Block all injection attempts */
    if (!validate_command_security(command)) {
        return PROCESS_ERROR_SECURITY_VIOLATION;
    }
    
    /* Parse command securely without shell interpretation */
    parsed_command_t parsed_cmd;
    if (parse_command_securely(command, &parsed_cmd) != 0) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    /* Validate program is in whitelist */
    if (!is_safe_program(parsed_cmd.program)) {
        cleanup_parsed_command(&parsed_cmd);
        return PROCESS_ERROR_SECURITY_VIOLATION;
    }
    
    /* Initialize handle */
    memset(handle, 0, sizeof(process_handle_t));
    handle->pid = -1;
    handle->status = 0;
    handle->timeout_seconds = timeout_seconds;
    handle->terminated = 0;
    handle->sync_pipe[0] = -1;
    handle->sync_pipe[1] = -1;
    
    /* Create synchronization pipe for race condition prevention */
    if (pipe(handle->sync_pipe) != 0) {
        cleanup_parsed_command(&parsed_cmd);
        return PROCESS_ERROR_CREATION_FAILED;
    }
    
    /* Record start time */
    handle->start_time = time(NULL);
    
    /* Fork process */
    handle->pid = fork();
    
    if (handle->pid == -1) {
        /* Fork failed */
        close(handle->sync_pipe[0]);
        close(handle->sync_pipe[1]);
        cleanup_parsed_command(&parsed_cmd);
        return PROCESS_ERROR_CREATION_FAILED;
    } else if (handle->pid == 0) {
        /* Child process */
        
        /* Close read end of pipe */
        close(handle->sync_pipe[0]);
        
        /* Change working directory if specified */
        if (working_dir && strlen(working_dir) > 0 && 
            strcmp(working_dir, ".") != 0) {
            if (chdir(working_dir) != 0) {
                close(handle->sync_pipe[1]);
                cleanup_parsed_command(&parsed_cmd);
                exit(1);
            }
        }
        
        /* Create new process group for proper cleanup */
        if (setpgid(0, 0) != 0) {
            close(handle->sync_pipe[1]);
            cleanup_parsed_command(&parsed_cmd);
            exit(1);
        }
        
        /* Signal parent that process group is established */
        char sync_byte = 1;
        write(handle->sync_pipe[1], &sync_byte, 1);
        close(handle->sync_pipe[1]);
        
        /* SECURE EXECUTION: Use execve() with parsed arguments - NO SHELL */
        execve(parsed_cmd.program, parsed_cmd.argv, NULL);
        
        /* If execve fails, try with PATH resolution */
        execvp(parsed_cmd.program, parsed_cmd.argv);
        
        /* exec failed */
        cleanup_parsed_command(&parsed_cmd);
        exit(1);
    }
    
    /* Parent process - wait for child process group establishment */
    close(handle->sync_pipe[1]);
    char sync_byte;
    fd_set read_fds;
    struct timeval timeout;
    
    FD_ZERO(&read_fds);
    FD_SET(handle->sync_pipe[0], &read_fds);
    timeout.tv_sec = 1;  /* 1 second timeout for synchronization */
    timeout.tv_usec = 0;
    
    int select_result = select(handle->sync_pipe[0] + 1, &read_fds, NULL, NULL, &timeout);
    if (select_result > 0 && FD_ISSET(handle->sync_pipe[0], &read_fds)) {
        read(handle->sync_pipe[0], &sync_byte, 1);
    }
    close(handle->sync_pipe[0]);
    
    /* Create process group - race condition eliminated by synchronization */
    if (setpgid(handle->pid, handle->pid) != 0) {
        /* Non-fatal error, continue */
    }
    
    cleanup_parsed_command(&parsed_cmd);
    return PROCESS_SUCCESS;
}

/* Monitor process for timeout - EVENT-DRIVEN, NO BUSY WAIT */
int monitor_process_timeout(process_handle_t* handle, int* timed_out) {
    if (!handle || !timed_out || handle->pid <= 0) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    *timed_out = 0;
    
    /* Check if process is already terminated */
    if (handle->terminated) {
        return PROCESS_SUCCESS;
    }
    
    /* Calculate remaining timeout */
    time_t current_time = time(NULL);
    time_t elapsed = current_time - handle->start_time;
    
    if (elapsed >= handle->timeout_seconds) {
        *timed_out = 1;
        return PROCESS_SUCCESS;
    }
    
    /* Use select() for event-driven monitoring - ELIMINATES BUSY WAIT */
    time_t remaining = handle->timeout_seconds - elapsed;
    struct timeval timeout;
    timeout.tv_sec = (remaining > 0) ? remaining : 0;
    timeout.tv_usec = 0;
    
    /* Use select() with no file descriptors - just for timeout */
    fd_set empty_set;
    FD_ZERO(&empty_set);
    
    int select_result = select(0, &empty_set, &empty_set, &empty_set, &timeout);
    
    /* Check process status after select returns */
    int status;
    pid_t result = waitpid(handle->pid, &status, WNOHANG);
    
    if (result == -1) {
        if (errno == ECHILD) {
            /* Process already terminated */
            handle->terminated = 1;
            handle->status = 0;
        } else {
            return PROCESS_ERROR_MONITORING_FAILED;
        }
    } else if (result == handle->pid) {
        /* Process terminated */
        handle->terminated = 1;
        handle->status = status;
    } else if (result == 0) {
        /* Process still running - check if timeout occurred */
        current_time = time(NULL);
        if (current_time - handle->start_time >= handle->timeout_seconds) {
            *timed_out = 1;
        }
    }
    
    return PROCESS_SUCCESS;
}

/* Terminate process tree with graceful sequence */
int terminate_process_tree(process_handle_t* handle, int graceful) {
    if (!handle || handle->pid <= 0) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    if (handle->terminated) {
        return PROCESS_SUCCESS;
    }
    
    if (graceful) {
        /* Send SIGTERM to process group for graceful termination */
        if (killpg(handle->pid, SIGTERM) == 0) {
            /* Wait briefly for graceful termination */
            for (int i = 0; i < 10; i++) {
                usleep(100000); /* 100ms */
                
                int status;
                pid_t result = waitpid(handle->pid, &status, WNOHANG);
                if (result == handle->pid || result == -1) {
                    handle->terminated = 1;
                    handle->status = (result == handle->pid) ? status : 0;
                    return PROCESS_SUCCESS;
                }
            }
        }
        
        /* Graceful termination failed, use SIGKILL */
    }
    
    /* Send SIGKILL to process group */
    if (killpg(handle->pid, SIGKILL) != 0) {
        /* Try killing individual process if process group fails */
        if (kill(handle->pid, SIGKILL) != 0) {
            return PROCESS_ERROR_TERMINATION_FAILED;
        }
    }
    
    /* Wait for process to terminate */
    int status;
    pid_t result = waitpid(handle->pid, &status, 0);
    if (result == handle->pid) {
        handle->terminated = 1;
        handle->status = status;
    } else {
        return PROCESS_ERROR_TERMINATION_FAILED;
    }
    
    return PROCESS_SUCCESS;
}

/* Clean up process resources */
int cleanup_process_resources(process_handle_t* handle) {
    if (!handle) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    /* Ensure process is terminated */
    if (!handle->terminated && handle->pid > 0) {
        /* Force termination if not already done */
        terminate_process_tree(handle, 0);
    }
    
    /* Wait for any remaining child processes to prevent zombies */
    if (handle->pid > 0) {
        int status;
        waitpid(handle->pid, &status, 0);
    }
    
    /* Clear handle */
    memset(handle, 0, sizeof(process_handle_t));
    handle->pid = -1;
    
    return PROCESS_SUCCESS;
}

/* Get process status string for debugging */
const char* get_process_status_string(int status_code) {
    switch (status_code) {
        case PROCESS_SUCCESS:
            return "Success";
        case PROCESS_ERROR_CREATION_FAILED:
            return "Process creation failed";
        case PROCESS_ERROR_MONITORING_FAILED:
            return "Process monitoring failed";
        case PROCESS_ERROR_TERMINATION_FAILED:
            return "Process termination failed";
        case PROCESS_ERROR_CLEANUP_FAILED:
            return "Resource cleanup failed";
        case PROCESS_ERROR_INVALID_PARAMS:
            return "Invalid parameters";
        default:
            return "Unknown error";
    }
}

/* Utility function to check if process exists */
int process_exists(pid_t pid) {
    if (pid <= 0) {
        return 0;
    }
    
    /* Use kill with signal 0 to check if process exists */
    return (kill(pid, 0) == 0) ? 1 : 0;
}

/* Count child processes for resource monitoring */
int count_child_processes(pid_t parent_pid) {
    if (parent_pid <= 0) {
        return 0;
    }
    
    /* This is a simplified implementation */
    /* In production, this would scan /proc or use process APIs */
    return 0;
}

/* Get correct process handle size for safe memory allocation */
size_t get_process_handle_size(void) {
    return sizeof(process_handle_t);
}

#endif /* __unix__ */