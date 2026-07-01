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

64 constant KEY-U

create KEY1 80 allot
create KEY2 80 allot

: HASH$ ( -- ptr u8 n )
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" ;

: LINE ( ptr u8 n -- )
   SB-APPEND
   STR-LF SB-APPEND-C ;

: FIELD ( ptr u8 n -- )
   SB-APPEND
   STR-TAB SB-APPEND-C ;

: EXPECTED$ ( -- ptr u8 n )
   SB-RESET
   s" HBOBJ" FIELD s" 1" LINE
   s" source" FIELD HASH$ LINE
   s" target" FIELD s" macos-aarch64" LINE
   s" checker" FIELD s" checker-effect-v1" LINE
   s" compiler" FIELD s" hb-arm64-v1" LINE
   s" require" FIELD s" lib/string.f" LINE
   s" package" FIELD s" OBJ" FIELD s" public" LINE
   s" export" FIELD s" SQUARE" FIELD s" n -- n" LINE
   s" import" FIELD s" PRINT" FIELD s" ptr u8 n --" LINE
   s" reloc" FIELD s" abs64" FIELD s" 16" FIELD s" PRINT" LINE
   s" type" FIELD s" count" FIELD s" nominal" LINE
   s" noret" FIELD s" DIE" LINE
   SB$ ;

: BUILD ( -- )
   OBJ:RESET
   HASH$ OBJ:SOURCE!
   s" macos-aarch64" OBJ:TARGET!
   s" checker-effect-v1" OBJ:CHECKER!
   s" hb-arm64-v1" OBJ:COMPILER!
   s" lib/string.f" OBJ:REQUIRE+
   s" OBJ" s" public" OBJ:PACKAGE+
   s" SQUARE" s" n -- n" OBJ:EXPORT+
   s" PRINT" s" ptr u8 n --" OBJ:IMPORT+
   s" abs64" 16 s" PRINT" OBJ:RELOC+
   s" count" s" nominal" OBJ:TYPE+
   s" DIE" OBJ:NORET+ ;

: SERIALIZES ( -- )
   BUILD
   OBJ:BYTES$ EXPECTED$ T$= ;

: LOAD-ROUNDTRIP ( -- )
   EXPECTED$ OBJ:LOAD
   OBJ:BYTES$ EXPECTED$ T$= ;

: KEY-STABLE ( -- )
   BUILD
   KEY1 OBJ:KEY-HEX
   EXPECTED$ OBJ:LOAD
   KEY2 OBJ:KEY-HEX
   KEY1 KEY-U KEY2 KEY-U T$= ;

: KEY-CHANGES ( -- )
   OBJ:RESET
   HASH$ OBJ:SOURCE!
   s" linux-aarch64" OBJ:TARGET!
   s" checker-effect-v1" OBJ:CHECKER!
   s" hb-arm64-v1" OBJ:COMPILER!
   KEY2 OBJ:KEY-HEX
   KEY1 KEY-U KEY2 KEY-U T$<> ;

: BAD-TAB ( -- )
   OBJ:RESET
   s" bad	name" OBJ:REQUIRE+ ;

: DUP-SOURCE ( -- )
   OBJ:RESET
   HASH$ OBJ:SOURCE!
   HASH$ OBJ:SOURCE! ;

: BAD-HASH ( -- )
   OBJ:RESET
   s" not-a-hash" OBJ:SOURCE! ;

: BAD-RELOC ( -- )
   s" HBOBJ	1
source	0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
target	macos-aarch64
checker	checker-effect-v1
compiler	hb-arm64-v1
reloc	abs64	nope	PRINT
" OBJ:LOAD ;

: MISSING-HEADER ( -- )
   OBJ:RESET
   HASH$ OBJ:SOURCE!
   s" macos-aarch64" OBJ:TARGET!
   s" checker-effect-v1" OBJ:CHECKER!
   OBJ:BYTES$ 2drop ;

: FAILURES ( -- )
   [: BAD-TAB ;] E-OBJ-FIELD TTHROWSQ
   [: DUP-SOURCE ;] E-OBJ-SCHEMA TTHROWSQ
   [: BAD-HASH ;] E-OBJ-FIELD TTHROWSQ
   [: BAD-RELOC ;] E-OBJ-FIELD TTHROWSQ
   [: MISSING-HEADER ;] E-OBJ-SCHEMA TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   SERIALIZES
   LOAD-ROUNDTRIP
   KEY-STABLE
   KEY-CHANGES
   FAILURES
   T-REPORT ;

end-package

OBJ-TEST:MAIN
