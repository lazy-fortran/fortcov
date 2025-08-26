module file_utils
    use file_finder
    use binary_file_io
    use text_file_io
    use directory_operations
    use path_resolver
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

end module file_utils