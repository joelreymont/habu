\ t-parsing.fs — parsing-word literals via CHECK-DEF: S" ." C" CHAR [CHAR].

\ S" pushes ptr u8 u32; TYPE consumes both, leaving the return row untouched.
: P-STR  s" PSTR" s" R -- R" s\" S\" hi\" TYPE" CHECK-DEF ;
T{ ' P-STR catch -> 0 }T

\ multi-token string content: the closing token is the one ending with a quote.
: P-MULTI s" PMULTI" s" R -- R" s\" S\" a b c\" TYPE" CHECK-DEF ;
T{ ' P-MULTI catch -> 0 }T

\ [CHAR] pushes a char.
: P-BCHAR s" PBCHAR" s" -- char" s" [CHAR] A" CHECK-DEF ;
T{ ' P-BCHAR catch -> 0 }T

\ CHAR pushes a char.
: P-CHAR  s" PCHAR" s" -- char" s" CHAR A" CHECK-DEF ;
T{ ' P-CHAR catch -> 0 }T

\ ." prints, pushing nothing.
: P-DOT   s" PDOT" s" -- " s\" .\" msg\"" CHECK-DEF ;
T{ ' P-DOT catch -> 0 }T

\ unterminated string runs off the body end and throws.
: P-OOPS  s" POOPS" s" R -- R" s\" S\" oops" CHECK-DEF ;
T{ ' P-OOPS catch -> E-QUOT }T
