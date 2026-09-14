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
require lib/object-link.f

package OBJRES-TEST
using OBJ
using OBJRES
using OBJSTORE
using OBJLINK

64 constant KEY-U
$81001 constant LARGE-U
DYNAMIC-BUFFER LARGE-STORAGE n

create KEY2 80 allot
create SRC-KEY 80 allot
create TEXT-BYTES 1 c, 2 c, 3 c,

: LARGE$ ( -- ptr u8 n )
   0 LARGE-STORAGE byte-view LARGE-U ;

: PREPARE-LARGE ( -- )
   LARGE-U CELL / 1+ LARGE-STORAGE-RESERVE
   LARGE$ {: a:ptr u:n :}
   u 0 ?do i 255 and a i + c! loop ;

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
   src srcu SOURCE!
   TARGET$ TARGET!
   CHECKER$ CHECKER!
   COMPILER$ COMPILER!
   TEXT-BYTES 3 TEXT+
   s" MAIN" s" --" OBJ:EXPORT+ ;

: STORE-LOADS ( -- )
   SRC$ BUILD
   OBJRES:STORE nip KEY-U T=
   SRC$ TARGET$ CHECKER$ COMPILER$ OBJRES:LOAD TTRUE
   SOURCE$ SRC$ T$=
   OBJ:TARGET$ TARGET$ T$=
   OBJ:CHECKER$ CHECKER$ T$=
   OBJ:COMPILER$ COMPILER$ T$= ;

: ALIASED-APPEND ( -- )
   BYTES$ {: src:ptr size:n :}
   size CELL / 1+ LARGE-STORAGE-RESERVE
   src 0 LARGE-STORAGE byte-view size BYTE-COPY
   \ Appending the whole encoded record forces growth beyond its old mapping.
   BYTES$ TEXT+
   OBJLINK:RESET
   ADD APPLY
   TEXT$ {: a:ptr u:n :}
   u LARGE-U size + T=
   a LARGE-U + size 0 LARGE-STORAGE byte-view size T$= ;

\ Exercise both payload sections above the old codec and merge ceilings.
\ Self-loading must leave the source intact before any growth or copy.
: LARGE-ROUNDTRIP ( -- )
   PREPARE-LARGE
   OBJ:RESET
   SRC$ SOURCE!
   TARGET$ TARGET!
   CHECKER$ CHECKER!
   COMPILER$ COMPILER!
   LARGE$ TEXT+
   LARGE$ DATA+
   BYTES$ nip LARGE-U 4 * > TTRUE
   KEY2 KEY-HEX
   BYTES$ OBJ:LOAD
   SRC-KEY KEY-HEX
   KEY2 KEY-U SRC-KEY KEY-U T$=
   OBJRES:STORE nip KEY-U T=
   SRC$ BUILD
   OBJLINK:RESET
   ADD
   SRC$ TARGET$ CHECKER$ COMPILER$ OBJRES:LOAD TTRUE
   SRC-KEY KEY-HEX
   KEY2 KEY-U SRC-KEY KEY-U T$=
   ADD
   APPLY
   OBJECT-COUNT 2 T=
   1 OBJECT-TEXT-BASE 3 T=
   1 OBJECT-TEXT-SIZE LARGE-U T=
   TEXT$ {: a:ptr u:n :}
   u LARGE-U 3 + T=
   a 3 TEXT-BYTES 3 T$=
   a 3 + LARGE-U LARGE$ T$=
   DATA$ LARGE$ T$=
   ALIASED-APPEND
   LARGE-STORAGE-RELEASE ;

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
   LARGE-ROUNDTRIP
   MISS-RETURNS-FALSE
   WRONG-INDEX-FAILS
   CORRUPT-INDEX-FAILS
   CLEANUP-RUN
   T-REPORT ;

;using
;using
;using
;using
;package

OBJRES-TEST:MAIN
