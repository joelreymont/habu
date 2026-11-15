\ codegen.f - a shared bounded byte buffer for building text: a generated word's
\ name, or a generator tool's output. STORAGE CLASS. CALLER-OWNED: each buffer's
\ header is its definer's dictionary storage and its text a dynamic mapping; the
\ module keeps no state.

require src/lib/prelude.f
require src/core/dynamic-storage.f

\ error.f leaves -4700..-4799 unclaimed, so this module declares its block;
\ error-code-lint keeps every E-* unique across the tree.
-4700 constant E-CG-CAP     \ an append would exceed a codegen buffer's capacity
-4701 constant E-CG-VALUE   \ APPEND-DECIMAL was handed a negative value

package CODEGEN

private

\ Header: [cap], then a dynamic control record (src/core/dynamic-storage.f: the
\ head cell and its CONTROL-CELLS count cells), scratch as DYNAMIC-BUFFER's: a
\ save or a file's object holds it released.
1 cells constant CB-RECORD
CB-RECORD CELL / 1+ DYNAMIC-STORAGE:CONTROL-CELLS + constant CB-CELLS
: CB-CAP@  ( ^n -- n )  @ ;
CAST: CB-HEAD-CELL ( ^n -- ^^u8 )
: CB-HEAD  ( ^n -- ^^u8 )  CB-RECORD + CB-HEAD-CELL ;
: CB-CTL   ( ^n -- ^n )  CB-RECORD CELL + + ;

\ The mapping, reserved whole at first use: the length cell and cap bytes.
: CB-MAP ( ^n -- ^u8 ) {: d:^ :}
   d CB-CTL @ 0= if
      CELL d CB-CAP@ + d CB-HEAD d CB-CTL 1 DYNAMIC-STORAGE:RESERVE
   then
   d CB-HEAD @ ;
: CB-LEN@  ( ^n -- n )  CB-MAP CELL-VIEW @ ;
: CB-LEN!  ( n ^n -- )  CB-MAP CELL-VIEW ! ;
: CB-DATA  ( ^n -- ^u8 )  CB-MAP CELL + ;   \ content bytes past the length

: CB-HEADER, ( n -- )   \ cap --
   ,  CB-CELLS CB-RECORD CELL / - 0 ?do here scratch-cell-mark 0 , loop ;

public

\ `256 CODEGEN:BUFFER NAME` reads like `create`. The definer has no trailing
\ colon: a package word ending in `:` cannot be resolved through a `CODEGEN:`
\ qualifier.
: BUFFER ( n -- )   \ cap --
   create CB-HEADER,  does> ( -- ^n ) ;

: RESET ( ^n -- )  0 swap CB-LEN! ;

\ Append one byte; overflow throws E-CG-CAP.
: APPEND-BYTE ( n ^n -- ) {: c:n d:^ :}
   d CB-LEN@ 1 + d CB-CAP@ > if E-CG-CAP throw then
   c  d CB-DATA d CB-LEN@ +  c!
   d CB-LEN@ 1 + d CB-LEN! ;

: APPEND-STRING ( ^u8 n ^n -- ) {: a:^ u:n d:^ :}
   0 begin dup u < while  dup a + c@ d APPEND-BYTE  1 +  repeat drop ;

\ A negative value throws E-CG-VALUE instead of emitting garbage digits.
: APPEND-DECIMAL ( n ^n -- ) {: v:n d:^ :}
   v 0 < if E-CG-VALUE throw then
   v 10 >= if v 10 / d recurse then  v 10 mod [char] 0 + d APPEND-BYTE ;

: CONTENTS ( ^n -- ^u8 n ) {: d:^ :}  d CB-DATA d CB-LEN@ ;

;package
