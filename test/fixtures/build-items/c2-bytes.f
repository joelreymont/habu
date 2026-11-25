\ c2-bytes.f - a program that requires src/lib/c2-memory.f, whose package a
\ build seals before program source, and copies a byte into owned bytes.

require src/lib/memory.f
require src/lib/c2-memory.f
require src/lib/c2-bytes.f

package BITEM-C2-BYTES
private
: DOC$ ( -- str ) s" a" ;

: TYPE-BYTES ( read-view<p,q,u8> -- )
   C2-BYTES:LENGTH swap  0 ?do  i C2-MEM:BYTE@ swap emit  loop  drop ;

: PRINT-SOURCE ( read-view<p,q,u8> -- read-view<p,q,u8> )
   dup TYPE-BYTES ;

: PRINT-ROOT ( mut-view<x,y,z,u8> -- mut-view<x,y,z,u8> )
   DOC$ C2-BYTES:COPY$ drop
   [: PRINT-SOURCE ;] C2-MEM:WITH-READ ;

public
: PRINT ( -- )
   DOC$ STR:LEN MEM:BYTES-ALLOC-LEN [: PRINT-ROOT ;] C2-MEM:WITH-MUT ;
;package

: MAIN ( -- ) s" bitem-c2-bytes: " type BITEM-C2-BYTES:PRINT cr ;
