module fortran_syntax_rules
    !! Fortran language syntax rules
    !!
    !! This module provides Fortran-specific syntax rules
    !! for syntax highlighting including keywords, types, and intrinsics.
    
    use syntax_token_types
    implicit none
    private

    public :: load_fortran_rules

contains

    subroutine load_fortran_rules(rules, success)
        !! Load Fortran language rules
        type(language_rules_t), intent(out) :: rules
        logical, intent(out) :: success
        
        integer :: n_keywords, n_types, n_intrinsics, n_operators
        
        success = .true.
        call rules%init()
        
        ! Define Fortran keywords
        n_keywords = 70
        allocate(rules%keywords(n_keywords))
        
        rules%keywords = [ &
            "abstract   ", "allocatable", "allocate   ", "associate  ", &
            "block      ", "call       ", "case       ", "character  ", &
            "class      ", "close      ", "codimension", "common     ", &
            "complex    ", "contains   ", "continue   ", "critical   ", &
            "cycle      ", "data       ", "deallocate ", "default    ", &
            "do         ", "double     ", "elemental  ", "else       ", &
            "elseif     ", "elsewhere  ", "end        ", "enddo      ", &
            "endif      ", "entry      ", "enum       ", "equivalence", &
            "exit       ", "extends    ", "external   ", "final      ", &
            "forall     ", "format     ", "function   ", "generic    ", &
            "goto       ", "if         ", "implicit   ", "import     ", &
            "impure     ", "include    ", "inquire    ", "integer    ", &
            "intent     ", "interface  ", "intrinsic  ", "kind       ", &
            "logical    ", "module     ", "namelist   ", "none       ", &
            "nopass     ", "nullify    ", "only       ", "open       ", &
            "operator   ", "optional   ", "parameter  ", "pass       ", &
            "pause      ", "pointer    ", "precision  ", "print      ", &
            "private    ", "procedure  ", "program    ", "protected  " &
        ]
        
        ! Define Fortran type keywords
        n_types = 15
        allocate(rules%types(n_types))
        
        rules%types = [ &
            "real       ", "integer    ", "logical    ", "character  ", &
            "complex    ", "type       ", "class      ", "double     ", &
            "precision  ", "dimension  ", "target     ", "save       ", &
            "volatile   ", "value      ", "bind       " &
        ]
        
        ! Define Fortran intrinsic functions
        n_intrinsics = 50
        allocate(rules%intrinsics(n_intrinsics))
        
        rules%intrinsics = [ &
            "abs        ", "acos       ", "adjustl    ", "adjustr    ", &
            "aimag      ", "aint       ", "all        ", "allocated  ", &
            "anint      ", "any        ", "asin       ", "associated ", &
            "atan       ", "atan2      ", "bit_size   ", "btest      ", &
            "ceiling    ", "char       ", "cmplx      ", "command_arg", &
            "conjg      ", "cos        ", "cosh       ", "count      ", &
            "cpu_time   ", "cshift     ", "date_and_ti", "dble       ", &
            "digits     ", "dim        ", "dot_product", "dprod      ", &
            "eoshift    ", "epsilon    ", "exp        ", "exponent   ", &
            "floor      ", "fraction   ", "huge       ", "iachar     ", &
            "iand       ", "ibclr      ", "ibits      ", "ibset      ", &
            "ichar      ", "ieor       ", "index      ", "int        ", &
            "ior        ", "ishft      " &
        ]
        
        ! Define Fortran operators
        n_operators = 20
        allocate(rules%operators(n_operators))
        
        rules%operators = [ &
            "+       ", "-       ", "*       ", "/       ", &
            "**      ", "=       ", "==      ", "/=      ", &
            "<       ", ">       ", "<=      ", ">=      ", &
            ".and.   ", ".or.    ", ".not.   ", ".eqv.   ", &
            ".neqv.  ", ".true.  ", ".false. ", "=>      " &
        ]
        
    end subroutine load_fortran_rules

end module fortran_syntax_rules