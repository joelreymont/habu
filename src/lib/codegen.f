\ codegen.f - a shared bounded byte buffer for building generated Forth source.
\ A definer that builds source for an audited `evaluate` (DEFTYPE in
\ src/lib/type/deftype.f, task.f's SLOT-GEN, generator tools) builds the text
\ here and keeps only its own `evaluate` wrapper: the trust boundary stays per
\ package. STORAGE CLASS. CALLER-OWNED: each buffer's header is its definer's
\ dictionary storage and its text a dynamic mapping; the module keeps no state.

require src/lib/prelude.f
require src/core/dynamic-storage.f

\ errors.f leaves -4700..-4799 unclaimed, so this module declares its block;
\ error-code-lint keeps every E-* unique across the tree.
-4700 constant E-CG-CAP     \ an append would exceed a codegen buffer's capacity
-4701 constant E-CG-VALUE   \ APPEND-DECIMAL was handed a negative value

package CODEGEN

private

\ Header: [cap][cap-err][val-err], then a dynamic control record
\ (src/core/dynamic-storage.f: the head cell and its CONTROL-CELLS count cells).
\ cap-err is thrown when an append would overflow, val-err when APPEND-DECIMAL
\ gets a negative value.
3 cells constant CB-RECORD
CB-RECORD CELL / 1+ DYNAMIC-STORAGE:CONTROL-CELLS + constant CB-CELLS
: CB-CAP@  ( ptr n -- n )  @ ;
: CB-CERR@ ( ptr n -- n )  cell+ @ ;
: CB-VERR@ ( ptr n -- n )  2 cells + @ ;
CAST: CB-HEAD-CELL ( ptr n -- ptr ptr u8 )
: CB-HEAD  ( ptr n -- ptr ptr u8 )  CB-RECORD + CB-HEAD-CELL ;
: CB-CTL   ( ptr n -- ptr n )  CB-RECORD CELL + + ;

\ The mapping, reserved whole at first use: the length cell and cap bytes.
: CB-MAP ( ptr n -- ptr u8 ) {: d:ptr :}
   d CB-CTL @ 0= if
      CELL d CB-CAP@ + d CB-HEAD d CB-CTL 1 DYNAMIC-STORAGE:RESERVE
   then
   d CB-HEAD @ ;
: CB-LEN@  ( ptr n -- n )  CB-MAP CELL-VIEW @ ;
: CB-LEN!  ( n ptr n -- )  CB-MAP CELL-VIEW ! ;
: CB-DATA  ( ptr n -- ptr u8 )  CB-MAP CELL + ;   \ content bytes past the length

: CB-HEADER, ( n n n -- )   \ cap cap-err val-err --
   >r >r , r> , r> ,  CB-CELLS CB-RECORD CELL / - 0 ?do 0 , loop ;

public

\ Defines NAME as a buffer descriptor with explicit cap-err and val-err codes, so
\ a package keeps the throw code its callers see (deftype.f keeps E-VNOM-CAP)
\ without a catch-and-remap wrapper.
: BUFFER-E ( n n n -- )   \ cap cap-err val-err --
   create CB-HEADER,  does> ( -- ptr n ) ;

\ Defines NAME as a buffer descriptor that throws E-CG-CAP and E-CG-VALUE; new
\ buffers use it. `256 CODEGEN:BUFFER NAME` reads like `create`. The definer has
\ no trailing colon: a package word ending in `:` cannot be resolved through a
\ `CODEGEN:` qualifier.
: BUFFER ( n -- )   \ cap --
   E-CG-CAP E-CG-VALUE BUFFER-E ;

: RESET ( ptr n -- )  0 swap CB-LEN! ;

\ Append one byte; overflow throws the buffer's capacity error code.
: APPEND-BYTE ( n ptr n -- ) {: c:n d:ptr :}
   d CB-LEN@ 1 + d CB-CAP@ > if d CB-CERR@ throw then
   c  d CB-DATA d CB-LEN@ +  c!
   d CB-LEN@ 1 + d CB-LEN! ;

: APPEND-STRING ( ptr u8 n ptr n -- ) {: a:ptr u:n d:ptr :}
   0 begin dup u < while  dup a + c@ d APPEND-BYTE  1 +  repeat drop ;

\ A negative value throws the buffer's val-err instead of emitting garbage
\ digits.
: APPEND-DECIMAL ( n ptr n -- ) {: v:n d:ptr :}
   v 0 < if d CB-VERR@ throw then
   v 10 >= if v 10 / d recurse then  v 10 mod [char] 0 + d APPEND-BYTE ;

: CONTENTS ( ptr n -- ptr u8 n ) {: d:ptr :}  d CB-DATA d CB-LEN@ ;

\ A descriptor is one cell span: the header and the control record. Its text
\ is a mapping capture releases, outside DATA.
: SCAN-SPANS ( ptr n [ ptr u8 n -- ] -- )
   {: d:ptr cells :}
   d BYTE-VIEW CB-CELLS CELL * cells execute ;

;package
