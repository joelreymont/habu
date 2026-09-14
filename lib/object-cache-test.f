\ object-cache-test.f - focused tests for OBJSTORE object file store.
\ Run: bin/hb --load lib/object-cache-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require lib/object.f
require lib/object-cache.f

package OBJSTORE-TEST
using OBJ
using OBJSTORE

64 constant KEY-U

create KEY1 80 allot
create KEY2 80 allot
create TEXT-BYTES 1 c, 2 c, 3 c,

: HASH$ ( -- ptr u8 n )
   s" abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: BADKEY$ ( -- ptr u8 n )
   s" 0000000000000000000000000000000000000000000000000000000000000000" ;

: SETUP ( -- )
   s" habu-object-cache" TMPDIR-MKDIR 2dup CLEANUP-TREE+ ROOT! ;

: BUILD ( -- )
   RESET
   HASH$ SOURCE!
   s" macos-aarch64" TARGET!
   s" checker-effect-v1" CHECKER!
   s" hb-arm64-v1" COMPILER!
   TEXT-BYTES 3 TEXT+
   s" SQUARE" s" n -- n" EXPORT+ ;

: COPY-KEY1 ( ptr u8 n -- )
   KEY-U T=
   KEY1 KEY-U BYTE-COPY ;

: STORE-LOADS ( -- )
   BUILD
   STORE COPY-KEY1
   KEY1 KEY-U OBJSTORE:EXISTS? TTRUE
   KEY1 KEY-U PATH$ s" .hbo" ENDS-WITH? TTRUE
   KEY1 KEY-U OBJSTORE:LOAD
   ROW-COUNT 6 T=
   4 ROW-TAG$ s" text" T$=
   5 ROW-TAG$ s" export" T$=
   KEY2 KEY-HEX
   KEY1 KEY-U KEY2 KEY-U T$= ;

: STORES-ATOMIC-OVERWRITE ( -- )
   BUILD
   STORE COPY-KEY1
   BUILD
   STORE 2drop
   KEY1 KEY-U OBJSTORE:LOAD
   ROW-COUNT 6 T= ;

: WRITE-BAD-FILE ( -- )
   BADKEY$ PATH$ s" not-an-object\n" WRITE-ALL ;

: WRITE-WRONG-KEY-FILE ( -- )
   BUILD
   BADKEY$ PATH$ BYTES$ WRITE-ALL ;

: LOAD-RETURNED-KEY ( -- )
   BUILD
   STORE {: key:ptr keyu:n :}
   TEXT-BYTES 3 DATA+
   key keyu PATH$ BYTES$ WRITE-ALL
   key keyu OBJSTORE:LOAD ;

: FAILURES ( -- )
   [: s" nope" PATH$ 2drop ;] E-OBJ-FIELD TTHROWSQ
   [: BADKEY$ OBJSTORE:LOAD ;] E-FS-OPEN TTHROWSQ
   WRITE-BAD-FILE
   [: BADKEY$ OBJSTORE:LOAD ;] E-OBJ-SCHEMA TTHROWSQ
   WRITE-WRONG-KEY-FILE
   [: BADKEY$ OBJSTORE:LOAD ;] E-OBJ-SCHEMA TTHROWSQ
   [: LOAD-RETURNED-KEY ;] E-OBJ-SCHEMA TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   STORE-LOADS
   STORES-ATOMIC-OVERWRITE
   FAILURES
   CLEANUP-RUN
   T-REPORT ;

;using
;using
;package

OBJSTORE-TEST:MAIN
