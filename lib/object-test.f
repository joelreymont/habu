\ object-test.f - focused tests for lib/object.f object-record codec.
\ Run: bin/hb --load lib/object-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/content-key.f
require lib/object.f

package OBJ-TEST
using OBJ

64 constant KEY-U

create KEY1 80 allot
create KEY2 80 allot
create TEXT-BYTES 0 c, 127 c, 255 c,
create DATA-BYTES 1 c, 2 c, 16 c,

: HASH$ ( -- ptr u8 n )
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" ;

: LINE ( ptr u8 n -- )
   SB-APPEND
   STR-LF SB-APPEND-C ;

: FIELD+ ( ptr u8 n -- )
   SB-APPEND
   STR-TAB SB-APPEND-C ;

: EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" HBOBJ" LINE
   s" source" FIELD+ HASH$ LINE
   s" target" FIELD+ s" macos-aarch64" LINE
   s" checker" FIELD+ s" checker-effect-v1" LINE
   s" compiler" FIELD+ s" hb-arm64-v1" LINE
   s" require" FIELD+ s" lib/string.f" LINE
   s" text" FIELD+ s" 007fff" LINE
   s" data" FIELD+ s" 010210" LINE
   s" package" FIELD+ s" OBJ" FIELD+ s" public" LINE
   s" export" FIELD+ s" SQUARE" FIELD+ s" n -- n" LINE
   s" def" FIELD+ s" SQUARE" FIELD+ s" 1" FIELD+ s" n -- n" LINE
   s" import" FIELD+ s" PRINT" FIELD+ s" ptr u8 n --" LINE
   s" reloc" FIELD+ s" abs64" FIELD+ s" 16" FIELD+ s" PRINT" LINE
   s" type" FIELD+ s" count" FIELD+ s" nominal" LINE
   s" noret" FIELD+ s" DIE" LINE
   SB$ ;

: SOURCE-ROW$ ( -- ptr u8 n )
   SB-RESET
   s" source" FIELD+ HASH$ SB-APPEND
   SB$ ;

: BUILD ( -- )
   RESET
   HASH$ SOURCE!
   s" macos-aarch64" TARGET!
   s" checker-effect-v1" CHECKER!
   s" hb-arm64-v1" COMPILER!
   s" lib/string.f" REQUIRE+
   TEXT-BYTES 3 TEXT+
   DATA-BYTES 3 DATA+
   s" OBJ" s" public" PACKAGE+
   s" SQUARE" s" n -- n" EXPORT+
   s" SQUARE" 1 s" n -- n" DEF+
   s" PRINT" s" ptr u8 n --" IMPORT+
   s" abs64" 16 s" PRINT" RELOC+
   s" count" s" nominal" TYPE+
   s" DIE" NORET+ ;

: SERIALIZES ( -- )
   BUILD
   BYTES$ EXPECTED$ T$= ;

: LOAD-ROUNDTRIP ( -- )
   EXPECTED$ LOAD
   BYTES$ EXPECTED$ T$= ;

: LINE-ROUNDTRIP ( -- )
   s" object-line" OBJ-LINE:MAKE
   OBJ-LINE:UNMAKE
   s" object-line" T$= ;

: HEADERS ( -- )
   BUILD
   SOURCE$ HASH$ T$=
   TARGET$ s" macos-aarch64" T$=
   CHECKER$ s" checker-effect-v1" T$=
   COMPILER$ s" hb-arm64-v1" T$=
   EXPECTED$ LOAD
   SOURCE$ HASH$ T$=
   TARGET$ s" macos-aarch64" T$=
   CHECKER$ s" checker-effect-v1" T$=
   COMPILER$ s" hb-arm64-v1" T$= ;

: KEY-STABLE ( -- )
   BUILD
   KEY1 KEY-HEX
   EXPECTED$ LOAD
   KEY2 KEY-HEX
   KEY1 KEY-U KEY2 KEY-U T$= ;

: KEY-CHANGES ( -- )
   RESET
   HASH$ SOURCE!
   s" linux-aarch64" TARGET!
   s" checker-effect-v1" CHECKER!
   s" hb-arm64-v1" COMPILER!
   KEY2 KEY-HEX
   KEY1 KEY-U KEY2 KEY-U T$<> ;

: MAX-BYTES-PUBLISHED ( -- )
   MAX-BYTES $40000 > TTRUE ;

: SIZE-OVERFLOW-FAILS ( -- )
   [: TEXT-BYTES MAX-BYTES 2 / 1+ TEXT+ ;] E-OBJ-CAPACITY TTHROWSQ
   [: TEXT-BYTES MAX-BYTES 2 / DATA+ ;] E-OBJ-CAPACITY TTHROWSQ
   [: TEXT-BYTES MAX-BYTES 1+ LOAD ;] E-OBJ-CAPACITY TTHROWSQ ;

: ROW-ACCESSORS ( -- )
   BUILD
   ROW-COUNT 14 T=
   0 ROW$ SOURCE-ROW$ T$=
   0 ROW-TAG$ s" source" T$=
   0 ROW-FIELD# 1 T=
   0 0 ROW-FIELD$ HASH$ T$=
   5 ROW-TAG$ s" text" T$=
   5 0 ROW-FIELD$ s" 007fff" T$=
   6 ROW-TAG$ s" data" T$=
   6 0 ROW-FIELD$ s" 010210" T$=
   9 ROW-TAG$ s" def" T$=
   9 ROW-FIELD# 3 T=
   9 0 ROW-FIELD$ s" SQUARE" T$=
   9 1 ROW-FIELD$ s" 1" T$=
   9 2 ROW-FIELD$ s" n -- n" T$=
   11 ROW-TAG$ s" reloc" T$=
   11 ROW-FIELD# 3 T=
   11 0 ROW-FIELD$ s" abs64" T$=
   11 1 ROW-FIELD$ s" 16" T$=
   11 2 ROW-FIELD$ s" PRINT" T$= ;

create ENTRY-SCRATCH 1024 allot
variable ENTRY-SCRATCH-U

: ENTRY-BUILD ( -- )
   RESET
   HASH$ SOURCE!
   s" macos-aarch64" TARGET!
   s" checker-effect-v1" CHECKER!
   s" hb-arm64-v1" COMPILER!
   TEXT-BYTES 3 TEXT+
   s" HLP" s" --" EXPORT+
   s" HLP" 0 s" --" DEF+
   s" HLP" 1 s" 0000000000000005" ENTRY+ ;

: ENTRY-ASSERT-ROW ( -- )                         \ row 7: source/target/checker/compiler/text/export/def/entry
   7 ROW-TAG$ s" entry" T$=
   7 ROW-FIELD# 3 T=
   7 0 ROW-FIELD$ s" HLP" T$=
   7 1 ROW-FIELD$ s" 1" T$=
   7 2 ROW-FIELD$ s" 0000000000000005" T$= ;

\ A selected non-MAIN entry row (name, test mode, forged seed hex) emits, and
\ re-parses from a serialized copy with a stable content key (item 10 slice 5).
: ENTRY-ROW ( -- )
   ENTRY-BUILD
   ENTRY-ASSERT-ROW
   KEY1 KEY-HEX
   BYTES$ dup ENTRY-SCRATCH-U ! ENTRY-SCRATCH swap BYTE-COPY
   ENTRY-SCRATCH ENTRY-SCRATCH-U @ LOAD
   KEY2 KEY-HEX
   KEY1 KEY-U KEY2 KEY-U T$=
   ENTRY-ASSERT-ROW ;

: BAD-TAB ( -- )
   RESET
   s" bad	name" REQUIRE+ ;

: DUP-SOURCE ( -- )
   RESET
   HASH$ SOURCE!
   HASH$ SOURCE! ;

: BAD-HASH ( -- )
   RESET
   s" not-a-hash" SOURCE! ;

: BAD-RELOC ( -- )
   s" HBOBJ
source	0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
target	macos-aarch64
checker	checker-effect-v1
compiler	hb-arm64-v1
reloc	abs64	nope	PRINT
" LOAD ;

: BAD-SECTION ( -- )
   s" HBOBJ
source	0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
target	macos-aarch64
checker	checker-effect-v1
compiler	hb-arm64-v1
text	00x
" LOAD ;

: BAD-DEF ( -- )
   s" HBOBJ
source	0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
target	macos-aarch64
checker	checker-effect-v1
compiler	hb-arm64-v1
def	WORD	nope	n -- n
" LOAD ;

: EMPTY-INPUT ( -- )
   s" " LOAD ;

: ONE-LINE-INPUT ( -- )
   s" HBOBJ
" LOAD ;

: BAD-MAGIC ( -- )
   s" BADOBJ
" LOAD ;

: MISSING-HEADER ( -- )
   RESET
   HASH$ SOURCE!
   s" macos-aarch64" TARGET!
   s" checker-effect-v1" CHECKER!
   BYTES$ 2drop ;

: FAILURES ( -- )
   [: BAD-TAB ;] E-OBJ-FIELD TTHROWSQ
   [: DUP-SOURCE ;] E-OBJ-SCHEMA TTHROWSQ
   [: BAD-HASH ;] E-OBJ-FIELD TTHROWSQ
   [: BAD-RELOC ;] E-OBJ-FIELD TTHROWSQ
   [: BAD-SECTION ;] E-OBJ-FIELD TTHROWSQ
   [: BAD-DEF ;] E-OBJ-FIELD TTHROWSQ
   [: EMPTY-INPUT ;] E-OBJ-SCHEMA TTHROWSQ
   [: ONE-LINE-INPUT ;] E-OBJ-SCHEMA TTHROWSQ
   [: BAD-MAGIC ;] E-OBJ-SCHEMA TTHROWSQ
   [: BUILD 99 ROW$ 2drop ;] E-OBJ-FIELD TTHROWSQ
   [: BUILD 0 -1 ROW-FIELD$ 2drop ;] E-OBJ-FIELD TTHROWSQ
   [: BUILD 0 99 ROW-FIELD$ 2drop ;] E-OBJ-FIELD TTHROWSQ
   [: MISSING-HEADER ;] E-OBJ-SCHEMA TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   SERIALIZES
   LOAD-ROUNDTRIP
   LINE-ROUNDTRIP
   HEADERS
   KEY-STABLE
   KEY-CHANGES
   MAX-BYTES-PUBLISHED
   SIZE-OVERFLOW-FAILS
   ROW-ACCESSORS
   ENTRY-ROW
   FAILURES
   T-REPORT ;

;using
;package

OBJ-TEST:MAIN
