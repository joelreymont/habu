\ Explicitly mark the complete cells of the current live CREATE body.
\ Every prefix loads this after xref.f; the concatenated stage source has no
\ require registry entry for files appended ahead of this one.

package CELL-BODY
private

E-CHECKER-LAYOUT-BUFFER constant E-EXTENT
$7FFFFFFFFFFFFFFF CELL / constant MAX-CELLS

\ Only DECLARE calls this, after validating the live native CREATE stamp.
TRUSTED: CREATED-XT ( n -- [ -- ptr a ] ) ;

public

: DECLARE ( n -- )
   {: count:n :}
   count 0 <= count MAX-CELLS > or IF E-EXTENT throw THEN
   data-base LASTC-CELL + @ {: owner:n :}
   owner dbase@ < owner dbase@ ndict@ DREC * + >= or
   owner dbase@ - DREC mod 0 <> or IF E-EXTENT throw THEN
   owner XREF-ADDR>REC {: rec:ptr :}
   rec XREF-RETIRED? IF E-EXTENT throw THEN
   rec XREF-FLAGS DKIND:MASK and DKIND:ADDR <>
      IF E-EXTENT throw THEN
   rec XREF-FLAGS DNAME-ALLOC and 0= IF E-EXTENT throw THEN
   rec XREF-START CREATED-XT execute {: base:ptr :}
   base BYTE-VIEW data-base BYTE-VIEW - CELL mod 0 <>
      IF E-EXTENT throw THEN
   base count cells + here <> IF E-EXTENT throw THEN
   base count rec CHECKER-CELL-BODY-BIND ;

;package

: CELL-BODY ( n -- ) CELL-BODY:DECLARE ;
