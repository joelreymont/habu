split-str
\ codegen.f - a shared growing byte buffer for building text: a generated word's
\ name, or a generator tool's output. STORAGE CLASS. CALLER-OWNED: each buffer's
\ header is its definer's dictionary storage and its text a dynamic mapping that
\ grows as text is appended, so CONTENTS holds only until the next append; the
\ module keeps no state.

require src/lib/prelude.f
require src/core/dynamic-storage.f

package CODEGEN

private

\ Header: a dynamic control record (src/core/dynamic-storage.f: the head cell
\ and its CONTROL-CELLS count cells), scratch as DYNAMIC-BUFFER's: a save or a
\ file's object holds it released.
1 DYNAMIC-STORAGE:CONTROL-CELLS + constant CB-CELLS
$100 constant CB-FIRST
CAST: CB-HEAD ( ^n -- ^^u8 )
: CB-CTL   ( ^n -- ^n )  CELL + ;

\ The mapping, reserved at first use: the length cell and CB-FIRST bytes.
: CB-MAP ( ^n -- ^u8 ) {: d:^ :}
   d CB-CTL @ 0= if
      CELL CB-FIRST + d CB-HEAD d CB-CTL 1 DYNAMIC-STORAGE:RESERVE
   then
   d CB-HEAD @ ;
: CB-LEN@  ( ^n -- n )  CB-MAP CELL-VIEW @ ;
: CB-LEN!  ( n ^n -- )  CB-MAP CELL-VIEW ! ;
: CB-DATA  ( ^n -- ^u8 )  CB-MAP CELL + ;   \ content bytes past the length

\ Room for one more byte. RESERVE at least doubles a mapping it grows and keeps
\ its bytes.
: CB-ROOM ( ^n -- ) {: d:^ :}
   CELL d CB-LEN@ + 1 + d CB-HEAD d CB-CTL 1 DYNAMIC-STORAGE:RESERVE ;

: CB-HEADER, ( -- )
   CB-CELLS cells SCRATCH-ALLOT ;

public

\ Defines NAME as a buffer descriptor. `CODEGEN:BUFFER NAME` reads like
\ `create`. The definer has no trailing colon: a package word ending in `:`
\ cannot be resolved through a `CODEGEN:` qualifier.
: BUFFER ( -- )
   create CB-HEADER,  does> ( -- ^n ) ;

: RESET ( ^n -- )  0 swap CB-LEN! ;

: APPEND-BYTE ( n ^n -- ) {: c:n d:^ :}
   d CB-ROOM
   c  d CB-DATA d CB-LEN@ +  c!
   d CB-LEN@ 1 + d CB-LEN! ;

: APPEND-STRING ( str ^n -- ) {: a:^ u:n d:^ :}
   0 begin dup u < while  dup a + c@ d APPEND-BYTE  1 +  repeat drop ;

private

\ The digits of a value at most zero. Division truncates toward zero, so each
\ remainder is at most zero and the most negative value needs no negate.
: CB-DIGITS ( n ^n -- ) {: v:n d:^ :}
   v -10 <= if v 10 / d recurse then
   [char] 0 v 10 mod - d APPEND-BYTE ;

public

: APPEND-DECIMAL ( n ^n -- ) {: v:n d:^ :}
   v 0 < if [char] - d APPEND-BYTE then
   v 0 > if v negate else v then d CB-DIGITS ;

: CONTENTS ( ^n -- str ) {: d:^ :}  d CB-DATA d CB-LEN@ ;

;package
