module coverage_parser_impl
    use coverage_parser_factory
    implicit none
    private
    
    ! Re-export types and procedures for backward compatibility
    public :: coverage_parser_t
    public :: gcov_parser_t
    public :: mock_parser_t
    public :: create_parser

end module coverage_parser_impl