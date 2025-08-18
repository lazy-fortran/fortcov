/*
 * Windows process timeout management interface (stub implementation)
 * 
 * This C interface provides Windows-specific process management functionality
 * for command timeout protection. Currently implemented as stubs for 
 * cross-platform compatibility.
 */

#ifdef _WIN32

#include <windows.h>
#include <tlhelp32.h>
#include <stdio.h>
#include <string.h>

/* Process handle structure for timeout management */
typedef struct {
    HANDLE process_handle;
    HANDLE job_handle;
    DWORD process_id;
    DWORD start_time;
    int timeout_seconds;
    int terminated;
} process_handle_t;

/* Error codes for C interface */
#define PROCESS_SUCCESS 0
#define PROCESS_ERROR_CREATION_FAILED -1
#define PROCESS_ERROR_MONITORING_FAILED -2
#define PROCESS_ERROR_TERMINATION_FAILED -3
#define PROCESS_ERROR_CLEANUP_FAILED -4
#define PROCESS_ERROR_INVALID_PARAMS -5
#define PROCESS_ERROR_NOT_IMPLEMENTED -999

/* Create process with timeout protection (Windows stub) */
int create_process_with_timeout(const char* command, 
                               const char* working_dir,
                               int timeout_seconds,
                               process_handle_t* handle) {
    if (!command || !handle || timeout_seconds <= 0) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    /* Initialize handle */
    memset(handle, 0, sizeof(process_handle_t));
    handle->process_handle = NULL;
    handle->job_handle = NULL;
    handle->process_id = 0;
    handle->timeout_seconds = timeout_seconds;
    handle->terminated = 0;
    
    /* Record start time */
    handle->start_time = GetTickCount();
    
    /* Create job object for process management */
    handle->job_handle = CreateJobObject(NULL, NULL);
    if (handle->job_handle == NULL) {
        return PROCESS_ERROR_CREATION_FAILED;
    }
    
    /* Set job object limits to terminate all processes when job closes */
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limit_info;
    memset(&limit_info, 0, sizeof(limit_info));
    limit_info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    
    if (!SetInformationJobObject(handle->job_handle, JobObjectExtendedLimitInformation,
                                &limit_info, sizeof(limit_info))) {
        CloseHandle(handle->job_handle);
        handle->job_handle = NULL;
        return PROCESS_ERROR_CREATION_FAILED;
    }
    
    /* Prepare process creation structures */
    STARTUPINFO startup_info;
    PROCESS_INFORMATION process_info;
    
    memset(&startup_info, 0, sizeof(startup_info));
    startup_info.cb = sizeof(startup_info);
    memset(&process_info, 0, sizeof(process_info));
    
    /* Create command line copy (CreateProcess may modify it) */
    char* command_copy = _strdup(command);
    if (!command_copy) {
        CloseHandle(handle->job_handle);
        handle->job_handle = NULL;
        return PROCESS_ERROR_CREATION_FAILED;
    }
    
    /* Create process */
    BOOL success = CreateProcess(
        NULL,           /* Application name */
        command_copy,   /* Command line */
        NULL,           /* Process security attributes */
        NULL,           /* Thread security attributes */
        FALSE,          /* Inherit handles */
        CREATE_SUSPENDED | CREATE_NEW_PROCESS_GROUP, /* Creation flags */
        NULL,           /* Environment */
        working_dir,    /* Working directory */
        &startup_info,  /* Startup info */
        &process_info   /* Process info */
    );
    
    free(command_copy);
    
    if (!success) {
        CloseHandle(handle->job_handle);
        handle->job_handle = NULL;
        return PROCESS_ERROR_CREATION_FAILED;
    }
    
    /* Assign process to job object */
    if (!AssignProcessToJobObject(handle->job_handle, process_info.hProcess)) {
        TerminateProcess(process_info.hProcess, 1);
        CloseHandle(process_info.hProcess);
        CloseHandle(process_info.hThread);
        CloseHandle(handle->job_handle);
        handle->job_handle = NULL;
        return PROCESS_ERROR_CREATION_FAILED;
    }
    
    /* Resume the process */
    ResumeThread(process_info.hThread);
    CloseHandle(process_info.hThread);
    
    /* Store process information */
    handle->process_handle = process_info.hProcess;
    handle->process_id = process_info.dwProcessId;
    
    return PROCESS_SUCCESS;
}

/* Monitor process for timeout */
int monitor_process_timeout(process_handle_t* handle, int* timed_out) {
    if (!handle || !timed_out) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    *timed_out = 0;
    
    /* Check if already terminated */
    if (handle->terminated) {
        return PROCESS_SUCCESS;
    }
    
    /* Check timeout */
    DWORD current_time = GetTickCount();
    DWORD elapsed_ms = current_time - handle->start_time;
    DWORD elapsed_seconds = elapsed_ms / 1000;
    
    if (elapsed_seconds >= (DWORD)handle->timeout_seconds) {
        *timed_out = 1;
        return PROCESS_SUCCESS;
    }
    
    /* Check process status */
    if (handle->process_handle) {
        DWORD exit_code;
        if (GetExitCodeProcess(handle->process_handle, &exit_code)) {
            if (exit_code != STILL_ACTIVE) {
                /* Process has terminated */
                handle->terminated = 1;
            }
        } else {
            /* Error getting process status */
            return PROCESS_ERROR_MONITORING_FAILED;
        }
    }
    
    return PROCESS_SUCCESS;
}

/* Terminate process tree with graceful sequence */
int terminate_process_tree(process_handle_t* handle, int graceful) {
    if (!handle) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    if (handle->terminated) {
        return PROCESS_SUCCESS;
    }
    
    /* Terminate using job object if available (terminates entire process tree) */
    if (handle->job_handle) {
        if (TerminateJobObject(handle->job_handle, 1)) {
            handle->terminated = 1;
            return PROCESS_SUCCESS;
        }
    }
    
    /* Fallback: terminate individual process */
    if (handle->process_handle) {
        if (graceful) {
            /* Windows doesn't have a direct equivalent to SIGTERM */
            /* Try GenerateConsoleCtrlEvent for console applications */
            if (GenerateConsoleCtrlEvent(CTRL_C_EVENT, handle->process_id) ||
                GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, handle->process_id)) {
                
                /* Wait briefly for graceful termination */
                DWORD wait_result = WaitForSingleObject(handle->process_handle, 2000);
                if (wait_result == WAIT_OBJECT_0) {
                    handle->terminated = 1;
                    return PROCESS_SUCCESS;
                }
            }
        }
        
        /* Force termination */
        if (TerminateProcess(handle->process_handle, 1)) {
            handle->terminated = 1;
            return PROCESS_SUCCESS;
        } else {
            return PROCESS_ERROR_TERMINATION_FAILED;
        }
    }
    
    handle->terminated = 1;
    return PROCESS_SUCCESS;
}

/* Clean up process resources (Windows stub) */
int cleanup_process_resources(process_handle_t* handle) {
    if (!handle) {
        return PROCESS_ERROR_INVALID_PARAMS;
    }
    
    /* Close handles if open */
    if (handle->process_handle) {
        CloseHandle(handle->process_handle);
        handle->process_handle = NULL;
    }
    
    if (handle->job_handle) {
        CloseHandle(handle->job_handle);
        handle->job_handle = NULL;
    }
    
    /* Clear handle */
    memset(handle, 0, sizeof(process_handle_t));
    
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
int process_exists(DWORD pid) {
    if (pid == 0) {
        return 0;
    }
    
    /* Try to open the process with minimal access */
    HANDLE process_handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid);
    if (process_handle != NULL) {
        /* Check if process is still running */
        DWORD exit_code;
        BOOL result = GetExitCodeProcess(process_handle, &exit_code);
        CloseHandle(process_handle);
        
        if (result && exit_code == STILL_ACTIVE) {
            return 1; /* Process exists and is running */
        }
    }
    
    return 0; /* Process doesn't exist or has terminated */
}

/* Count child processes for resource monitoring */
int count_child_processes(DWORD parent_pid) {
    if (parent_pid == 0) {
        return 0;
    }
    
    /* Use CreateToolhelp32Snapshot to enumerate processes */
    HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE) {
        return 0;
    }
    
    PROCESSENTRY32 process_entry;
    process_entry.dwSize = sizeof(PROCESSENTRY32);
    
    int child_count = 0;
    
    /* Get first process */
    if (Process32First(snapshot, &process_entry)) {
        do {
            /* Check if this process has the specified parent */
            if (process_entry.th32ParentProcessID == parent_pid) {
                child_count++;
            }
        } while (Process32Next(snapshot, &process_entry));
    }
    
    CloseHandle(snapshot);
    return child_count;
}

#endif /* _WIN32 */