\ object-index-test.f - focused tests for OBJIDX source key index.
\ Run: bin/hb --load lib/object-index-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require lib/object-index.f

package OBJIDX-TEST

64 constant KEY-U

create KEY1 80 allot
create KEY2 80 allot
create KEY3 80 allot

: SRC$ ( -- ptr u8 n )
   s" abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: SRC2$ ( -- ptr u8 n )
   s" bbcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: OBJKEY$ ( -- ptr u8 n )
   s" 1111111111111111111111111111111111111111111111111111111111111111" ;

: BADKEY$ ( -- ptr u8 n )
   s" 0000000000000000000000000000000000000000000000000000000000000000" ;

: SETUP ( -- )
   s" habu-object-index" TMPDIR-MKDIR 2dup CLEANUP-TREE+ OBJIDX:ROOT! ;

: SOURCE-KEY1! ( -- )
   SRC$ s" macos-aarch64" s" checker-effect-v1" s" hb-arm64-v1" KEY1 OBJIDX:SOURCE-KEY-HEX ;

: SOURCE-KEY2! ( -- )
   SRC$ s" linux-aarch64" s" checker-effect-v1" s" hb-arm64-v1" KEY2 OBJIDX:SOURCE-KEY-HEX ;

: SOURCE-KEY3! ( -- )
   SRC2$ s" macos-aarch64" s" checker-effect-v1" s" hb-arm64-v1" KEY3 OBJIDX:SOURCE-KEY-HEX ;

: SOURCE-KEYS ( -- )
   SOURCE-KEY1!
   SOURCE-KEY2!
   KEY1 KEY-U KEY2 KEY-U STR= TFALSE
   SOURCE-KEY3!
   KEY1 KEY-U KEY3 KEY-U STR= TFALSE
   SRC$ s" macos-aarch64" s" checker-effect-v1" s" hb-arm64-v1" KEY2 OBJIDX:SOURCE-KEY-HEX
   KEY1 KEY-U KEY2 KEY-U T$= ;

: STORE-LOADS ( -- )
   SOURCE-KEY1!
   KEY1 KEY-U OBJKEY$ OBJIDX:STORE
   KEY1 KEY-U OBJIDX:EXISTS? TTRUE
   KEY1 KEY-U OBJIDX:PATH$ s" .idx" ENDS-WITH? TTRUE
   KEY1 KEY-U OBJIDX:LOAD TTRUE
   OBJKEY$ T$= ;

: MISSES ( -- )
   SOURCE-KEY2!
   KEY2 KEY-U OBJIDX:LOAD TFALSE
   nip 0 T= ;

: WRITE-BAD-FILE ( -- )
   BADKEY$ OBJIDX:PATH$ s" not-a-key\n" WRITE-ALL ;

: FAILURES ( -- )
   [: s" nope" s" macos-aarch64" s" checker-effect-v1" s" hb-arm64-v1" KEY1 OBJIDX:SOURCE-KEY-HEX ;] E-OBJ-FIELD TTHROWSQ
   [: KEY1 KEY-U s" nope" OBJIDX:STORE ;] E-OBJ-FIELD TTHROWSQ
   [: s" nope" OBJIDX:PATH$ 2drop ;] E-OBJ-FIELD TTHROWSQ
   WRITE-BAD-FILE
   [: BADKEY$ OBJIDX:LOAD 2drop drop ;] E-OBJ-FIELD TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   SOURCE-KEYS
   STORE-LOADS
   MISSES
   FAILURES
   CLEANUP-RUN
   T-REPORT ;

;package

OBJIDX-TEST:MAIN
