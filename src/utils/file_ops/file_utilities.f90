module file_utilities
    use file_utils_consolidated, only: find_files, find_files_with_glob, &
        read_binary_file, read_binary_file_safe, write_text_file, &
        write_text_file_safe, read_file_content, read_file_content_enhanced, &
        ensure_directory, ensure_directory_safe, resolve_path, file_exists, &
        basename
    implicit none
    private
    
    ! Re-export all procedures for backward compatibility
    public :: find_files
    public :: find_files_with_glob
    public :: resolve_path
    public :: read_binary_file
    public :: write_text_file
    public :: ensure_directory
    public :: read_binary_file_safe
    public :: write_text_file_safe
    public :: ensure_directory_safe
    public :: file_exists
    public :: read_file_content
    public :: read_file_content_enhanced
    public :: basename

end module file_utilities