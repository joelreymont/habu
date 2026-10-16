\ object-resolve-test.f - focused tests for OBJRES source+ABI resolution.
\ Run: bin/hb --load src/lib/object-resolve-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/content-key.f
require src/lib/object.f
require src/lib/object-cache.f
require src/lib/object-index.f
require src/lib/object-resolve.f

package OBJRES-TEST
using OBJ
using OBJRES
using OBJSTORE

64 constant KEY-U
\ Text and data, hex-encoded, put the record past the codec's former 256 KiB
\ ceiling and the cache's former fixed read.
$10001 constant LARGE-U
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
   s" habu-object-resolve" HB-TMP-MKDIR 2dup CLEANUP-TREE+ OBJRES:ROOT! ;

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

: HEX-DIGIT ( n -- n )
   dup 10 < if 48 + exit then
   87 + ;

: ALIASED-APPEND ( -- )
   BYTES$ {: src:ptr size:n :}
   size CELL / 1+ LARGE-STORAGE-RESERVE
   src 0 LARGE-STORAGE byte-view size BYTE-COPY
   \ Appending the codec's own record can move its storage during the copy.
   BYTES$ TEXT+
   s" the aliased append relocated the codec buffer" T-LABEL
   BYTES$ drop src <> TTRUE
   ROW-COUNT 7 T=
   6 ROW-TAG$ s" text" T$=
   6 0 ROW-FIELD$ {: hex:ptr hexu:n :}
   hexu size 2 * T=
   true
   size 0 ?do
      0 LARGE-STORAGE byte-view i + c@ {: b:n :}
      hex i 2 * + c@ b 4 rshift HEX-DIGIT = and
      hex i 2 * 1+ + c@ b 15 and HEX-DIGIT = and
   loop TTRUE ;

\ Exercise both payload sections above the former record and merge ceilings.
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
   SRC$ TARGET$ CHECKER$ COMPILER$ OBJRES:LOAD TTRUE
   SRC-KEY KEY-HEX
   KEY2 KEY-U SRC-KEY KEY-U T$=
   ROW-COUNT 6 T=
   4 ROW-TAG$ s" text" T$=
   4 0 ROW-FIELD$ nip LARGE-U 2 * T=
   5 ROW-TAG$ s" data" T$=
   5 0 ROW-FIELD$ nip LARGE-U 2 * T=
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

\ An index record whose object is gone is a miss, not an error: a prune takes
\ an object and an index record apart, so either can outlive the other.
: GONE-OBJECT-MISSES ( -- )
   SRC$ SOURCE-KEY!
   SRC-KEY KEY-U BADOBJ$ OBJIDX:STORE
   SRC$ TARGET$ CHECKER$ COMPILER$ OBJRES:LOAD TFALSE ;

public

: MAIN ( -- )
   T-RESET
   SETUP
   STORE-LOADS
   LARGE-ROUNDTRIP
   MISS-RETURNS-FALSE
   WRONG-INDEX-FAILS
   GONE-OBJECT-MISSES
   CLEANUP-RUN
   T-REPORT ;

;using
;using
;using
;package

OBJRES-TEST:MAIN
