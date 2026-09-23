\ The normal artifact fixture supplies a real closed window; two script args
\ leave its file roundtrip to the caller. This test transfers that capture in RAM.
require test/aot-artifact-roundtrip.f
require lib/test.f
require src/habu/aot-owned.f

package AOT-FILE

public

\ Erase the original used spans, including scalars and closure paths. The owned
\ copy must be sufficient to restore every section without help from old bytes.
: OWNED-TEST-ERASE ( -- )
   SEC-N 0 ?do
      i SEC-PTR i BASE@ + {: dst:ptr :}
      i ROW-LEN@ 0 ?do 0 dst i + c! loop
   loop ;

\ Adding this positive, cell-aligned length to the table's first payload
\ offset overflows a signed cell. IMPORT must reject it before copying bytes.
: OWNED-TEST-OVERFLOW ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup AOT-OWNED:BYTES$ drop
   $7FFFFFFFFFFFFFF8 swap 8 + U64! ;

;package

package AOT-OWNED-CAPTURE-TEST
using AOT-BUF
using AOT-WINDOW

create BEFORE 32 allot
create AFTER 32 allot
create SHA-CTX SHA256-CTX-BYTES allot   \ this fixture's digest context

: ORIGIN ( n n -- n ) {: first:n end:n :}
   first AOT-ARM:B0 @ T= end AOT-ARM:B1 @ T=
   1 SCRIPT-ARGV$ s" origin-jit" STR= if 0 else 1 then ;

: ORIGIN-CASE ( -- )
   AOT-CAPTURE:CODE-WINDOW {: first:n end:n copied:n :}
   1 SCRIPT-ARGV$ s" origin-size" STR= if copied 4 + else copied then {: bytes:n :}
   first end bytes [: ORIGIN ;] false AOT-FILE:OWN-WINDOW
   dup AOT-OWNED:ORIGIN@ 1 T= AOT-OWNED:CLOSE
   T-REPORT
   s" owned-capture: original window verified" type cr ;

: HASH ( AOT-OWNED:capture ptr u8 -- ) {: digest:ptr :}
   AOT-OWNED:BYTES$ {: a:ptr u:n :}
   SHA-CTX a u digest SHA256-IN ;

\ These are the capture buffers' own mappings. Releasing them after OWN tests
\ the transfer's lifetime without disturbing the compiler's other live stores.
: RELEASE-SOURCE ( -- )
   BLOB-STORAGE-RELEASE
   REC-STORAGE-RELEASE
   BM-STORAGE-RELEASE
   VAL-STORAGE-RELEASE
   DSITE-STORAGE-RELEASE
   AOT-NAMES-STORAGE-RELEASE ;

: TRANSFER ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup BEFORE HASH
   AOT-FILE:OWNED-TEST-ERASE
   AOTRT:FORGET-COUNTS
   AOT-IDENT:RESET
   RELEASE-SOURCE
   dup AFTER HASH
   BEFORE 32 AFTER 32 STR= TTRUE
   dup AOT-FILE:IMPORT
   AOTRT:?XTOFFS-RESTORED
   AOTRT:?RESTORED
   AOT-IDENT:COUNT 2 T=
   0 AOT-IDENT:PATH$ s" src/habu/aot-decl.f" T$=
   1 AOT-IDENT:PATH$ s" src/habu/aot-file.f" T$=
   AOT-FILE:OWN dup AFTER HASH AOT-OWNED:CLOSE
   BEFORE 32 AFTER 32 STR= TTRUE ;

: RUN ( -- )
   T-RESET
   AOTRT:CLOSURE! AOTRT:CAPTURE
   AOTRT:?XTCELLS AOTRT:?RESTORED AOTRT:SAVE-XTOFFS
   1 SCRIPT-ARGV$ 7 min s" origin-" STR= if ORIGIN-CASE exit then
   1 SCRIPT-ARGV$ s" overflow" STR= if
      AOT-FILE:OWN AOT-FILE:OWNED-TEST-OVERFLOW AOT-FILE:IMPORT
      s" owned-capture: accepted overflowing section" 79 die
   then
   AOT-FILE:OWN TRANSFER AOT-OWNED:CLOSE
   T-REPORT
   s" owned-capture: restored after source release" type cr ;

RUN
;using
;using
;package
