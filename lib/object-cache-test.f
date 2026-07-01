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

64 constant KEY-U

create KEY1 80 allot
create KEY2 80 allot
create TEXT-BYTES 1 c, 2 c, 3 c,

: HASH$ ( -- ptr u8 n )
   s" abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: BADKEY$ ( -- ptr u8 n )
   s" 0000000000000000000000000000000000000000000000000000000000000000" ;

: SETUP ( -- )
   s" habu-object-cache" TMPDIR-MKDIR 2dup CLEANUP-TREE+ OBJSTORE:ROOT! ;

: BUILD ( -- )
   OBJ:RESET
   HASH$ OBJ:SOURCE!
   s" macos-aarch64" OBJ:TARGET!
   s" checker-effect-v1" OBJ:CHECKER!
   s" hb-arm64-v1" OBJ:COMPILER!
   TEXT-BYTES 3 OBJ:TEXT+
   s" SQUARE" s" n -- n" OBJ:EXPORT+ ;

: COPY-KEY1 ( ptr u8 n -- )
   KEY-U T=
   KEY1 KEY-U BYTE-COPY ;

: STORE-LOADS ( -- )
   BUILD
   OBJSTORE:STORE COPY-KEY1
   KEY1 KEY-U OBJSTORE:EXISTS? TTRUE
   KEY1 KEY-U OBJSTORE:PATH$ s" .hbo" ENDS-WITH? TTRUE
   KEY1 KEY-U OBJSTORE:LOAD
   OBJ:ROW-COUNT 6 T=
   4 OBJ:ROW-TAG$ s" text" T$=
   5 OBJ:ROW-TAG$ s" export" T$=
   KEY2 OBJ:KEY-HEX
   KEY1 KEY-U KEY2 KEY-U T$= ;

: STORES-ATOMIC-OVERWRITE ( -- )
   BUILD
   OBJSTORE:STORE COPY-KEY1
   BUILD
   OBJSTORE:STORE 2drop
   KEY1 KEY-U OBJSTORE:LOAD
   OBJ:ROW-COUNT 6 T= ;

: WRITE-BAD-FILE ( -- )
   BADKEY$ OBJSTORE:PATH$ s" not-an-object\n" WRITE-ALL ;

: FAILURES ( -- )
   [: s" nope" OBJSTORE:PATH$ 2drop ;] E-OBJ-FIELD TTHROWSQ
   [: BADKEY$ OBJSTORE:LOAD ;] E-FS-OPEN TTHROWSQ
   WRITE-BAD-FILE
   [: BADKEY$ OBJSTORE:LOAD ;] E-OBJ-SCHEMA TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   STORE-LOADS
   STORES-ATOMIC-OVERWRITE
   FAILURES
   CLEANUP-RUN
   T-REPORT ;

end-package

OBJSTORE-TEST:MAIN
