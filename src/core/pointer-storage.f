\ pointer-storage.f - pointer-valued global storage definer.

: PTR-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr a ) 0 ptr-field ;

\ Byte access views the representation of a pointer's storage. This identity
\ retype is part of the raw-memory boundary; its effect is asserted with PRIM
\ when the checker loads. Buffer algorithms using the view are checked.
: BYTE-VIEW ( ptr a -- ptr u8 ) ;

\ A cell view admits raw scalar, pointer, and quotation storage. The PRIM row
\ gives its pointee the raw kind, which cannot manufacture nominal values.
: CELL-VIEW ( ptr u8 -- ptr a ) ;
