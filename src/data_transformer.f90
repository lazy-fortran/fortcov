module data_transformer
    use data_transformer_types
    use data_transformer_core
    implicit none
    private
    
    ! Re-export types for backward compatibility
    public :: data_transformer_t
    public :: transformed_data_t
    public :: source_file_t
    public :: annotated_line_t
    public :: navigation_tree_t
    public :: large_file_streamer_t
    public :: coverage_summary_t
    public :: navigation_node_t
    
    ! Re-export main procedure
    public :: transform_coverage_data

end module data_transformer