\ object-resolve-test.f - focused tests for OBJRES source+ABI resolution.
\ Run: bin/hb --load lib/object-resolve-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require lib/object.f
require lib/object-cache.f
require lib/object-index.f
require lib/object-resolve.f

package OBJRES-TEST

64 constant KEY-U

create KEY2 80 allot
create SRC-KEY 80 allot
create TEXT-BYTES 1 c, 2 c, 3 c,

: SRC$ ( -- ptr u8 n )
   s" abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: SRC2$ ( -- ptr u8 n )
   s" bbcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789" ;

: TARGET$ ( -- ptr u8 n )
   s" macos-aarch64" ;

: CHECKER$ ( -- ptr u8 n )
   s" checker-effect-v1" ;

: COMPILER$ ( -- ptr u8 n )
   s" hb-arm64-v1" ;

: BADOBJ$ ( -- ptr u8 n )
   s" 1111111111111111111111111111111111111111111111111111111111111111" ;

: SETUP ( -- )
   s" habu-object-resolve" TMPDIR-MKDIR 2dup CLEANUP-TREE+ OBJRES:ROOT! ;

: BUILD ( ptr u8 n -- ) {: src:ptr srcu:n :}
   OBJ:RESET
   src srcu OBJ:SOURCE!
   TARGET$ OBJ:TARGET!
   CHECKER$ OBJ:CHECKER!
   COMPILER$ OBJ:COMPILER!
   TEXT-BYTES 3 OBJ:TEXT+
   s" MAIN" s" --" OBJ:EXPORT+ ;

: STORE-LOADS ( -- )
   SRC$ BUILD
   OBJRES:STORE nip KEY-U T=
   SRC$ TARGET$ CHECKER$ COMPILER$ OBJRES:LOAD TTRUE
   OBJ:SOURCE$ SRC$ T$=
   OBJ:TARGET$ TARGET$ T$=
   OBJ:CHECKER$ CHECKER$ T$=
   OBJ:COMPILER$ COMPILER$ T$= ;

: MISS-RETURNS-FALSE ( -- )
   SRC2$ TARGET$ CHECKER$ COMPILER$ OBJRES:LOAD TFALSE ;

: SOURCE-KEY! ( ptr u8 n -- )
   TARGET$ CHECKER$ COMPILER$ SRC-KEY OBJIDX:SOURCE-KEY-HEX ;

: STORE-WRONG-INDEX ( -- )
   SRC$ BUILD
   OBJSTORE:STORE {: key:ptr keyu:n :}
   keyu KEY-U T=
   key KEY2 KEY-U BYTE-COPY
   SRC2$ SOURCE-KEY!
   SRC-KEY KEY-U KEY2 KEY-U OBJIDX:STORE ;

: WRONG-INDEX-FAILS ( -- )
   STORE-WRONG-INDEX
   [: SRC2$ TARGET$ CHECKER$ COMPILER$ OBJRES:LOAD drop ;] E-OBJ-SCHEMA TTHROWSQ ;

: CORRUPT-INDEX-FAILS ( -- )
   SRC$ SOURCE-KEY!
   SRC-KEY KEY-U BADOBJ$ OBJIDX:STORE
   [: SRC$ TARGET$ CHECKER$ COMPILER$ OBJRES:LOAD drop ;] E-FS-OPEN TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   STORE-LOADS
   MISS-RETURNS-FALSE
   WRONG-INDEX-FAILS
   CORRUPT-INDEX-FAILS
   CLEANUP-RUN
   T-REPORT ;

;package

OBJRES-TEST:MAIN
