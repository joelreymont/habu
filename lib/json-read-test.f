\ json-read-test.f - focused tests for the checked JSON pull parser.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/float.f lib/test.f
\ lib/json-read.f lib/json-read-test.f

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/memory.f
require lib/test.f
require lib/json-read.f
require lib/json-write.f
require test/checker-assert.f

package JSON-READ-TEST
private

256 constant JRT-CAP
create JRT-BUF JRT-CAP allot
here CELL 1- and CELL swap - CELL 1- and allot
create JRT-STATE-A JR:STORAGE-BYTES allot
create JRT-STATE-B JR:STORAGE-BYTES allot
create JRT-ZERO-PTR-CELL 0 ,

: JRT-OPEN-A ( ptr u8 n -- JR:reader )
   JRT-STATE-A JR:STORAGE-BYTES 2swap JR:INIT ;

: JRT-OPEN-B ( ptr u8 n -- JR:reader )
   JRT-STATE-B JR:STORAGE-BYTES 2swap JR:INIT ;

: JRT-ZERO-PTR ( -- ptr a )
   JRT-ZERO-PTR-CELL 0 ptr-field @ ;

: JRT-ZERO-U8 ( -- ptr u8 )
   JRT-ZERO-PTR-CELL 0 ptr-field @ ;

: JRT-REJECTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 = TFALSE ;

: JRT-PRIVATE-CONSTANT ( ptr u8 n -- )
   SB-RESET
   s" JRT-BAD-PRIVATE-CONSTANT ( -- n ) " SB-APPEND
   SB-APPEND
   SB$ JRT-REJECTED ;

\ Test-local byte constants for building JSON fixtures. json-read's own byte
\ constants are package-private, so the test spells the ASCII codes itself.
34 constant JRT-DQ
32 constant JRT-SP
58 constant JRT-COLON
44 constant JRT-COMMA
123 constant JRT-LBRACE
125 constant JRT-RBRACE
91 constant JRT-LBRACK
93 constant JRT-RBRACK

: JRT-NEAR ( r r -- bool )
   f- fabs 0.000001 f< ;

: JRT-TRUE ( -- bool )
   0 0= ;

\ ---- string / escape fixtures (byte arrays keep embedded quotes) ----------
create JRT-ESC-SRC
   34 c,
   92 c, 34 c,   92 c, 92 c,   92 c, 47 c,   92 c, 98 c,
   92 c, 102 c,  92 c, 110 c,  92 c, 114 c,  92 c, 116 c,
   34 c,
: JRT-ESC-SRC$ ( -- ptr u8 n ) JRT-ESC-SRC 18 ;

create JRT-ESC-WANT
   34 c, 92 c, 47 c, 8 c, 12 c, 10 c, 13 c, 9 c,
: JRT-ESC-WANT$ ( -- ptr u8 n ) JRT-ESC-WANT 8 ;

create JRT-U-SRC 34 c, 92 c, 117 c, 48 c, 48 c, 52 c, 49 c, 34 c,
: JRT-U-SRC$ ( -- ptr u8 n ) JRT-U-SRC 8 ;
create JRT-U-WANT 65 c,
: JRT-U-WANT$ ( -- ptr u8 n ) JRT-U-WANT 1 ;

\ "😀"  (U+1F600 grinning face) -> UTF-8 F0 9F 98 80
create JRT-SUR-SRC
   34 c,
   92 c, 117 c, 68 c, 56 c, 51 c, 68 c,
   92 c, 117 c, 68 c, 69 c, 48 c, 48 c,
   34 c,
: JRT-SUR-SRC$ ( -- ptr u8 n ) JRT-SUR-SRC 14 ;
create JRT-SUR-WANT $F0 c, $9F c, $98 c, $80 c,
: JRT-SUR-WANT$ ( -- ptr u8 n ) JRT-SUR-WANT 4 ;

create JRT-HI-SRC 34 c, 104 c, 105 c, 34 c,          \ "hi"
: JRT-HI-SRC$ ( -- ptr u8 n ) JRT-HI-SRC 4 ;

create JRT-LONE-SRC 34 c, 92 c, 117 c, 68 c, 56 c, 48 c, 48 c, 34 c,   \ "\uD800"
: JRT-LONE-SRC$ ( -- ptr u8 n ) JRT-LONE-SRC 8 ;
create JRT-BADESC-SRC 34 c, 92 c, 120 c, 34 c,       \ "\x"
: JRT-BADESC-SRC$ ( -- ptr u8 n ) JRT-BADESC-SRC 4 ;
create JRT-UNTERM-SRC 34 c, 97 c, 98 c, 99 c,        \ "abc
: JRT-UNTERM-SRC$ ( -- ptr u8 n ) JRT-UNTERM-SRC 4 ;

\ round-trip value: "A\"B" holds bytes A " B
create JRT-NAME 65 c, 34 c, 66 c,
: JRT-NAME$ ( -- ptr u8 n ) JRT-NAME 3 ;

\ ---- assertion helpers ----------------------------------------------------
: JRT-STR= ( JR:reader ptr u8 n -- JR:reader ) {: ea:ptr eu:n :}
   JRT-BUF JRT-CAP JR:STR {: got:n :}
   JRT-BUF got ea eu T$= ;

\ ---- structural / SB-built fixtures ---------------------------------------
: JRT-QKEY ( ptr u8 n -- )                       \ "key":  onto the builder
   JRT-DQ SB-APPEND-C SB-APPEND JRT-DQ SB-APPEND-C JRT-COLON SB-APPEND-C ;

: JRT-QSTR ( ptr u8 n -- )                       \ "str"  onto the builder
   JRT-DQ SB-APPEND-C SB-APPEND JRT-DQ SB-APPEND-C ;

: JRT-NESTED$ ( -- ptr u8 n )
   SB-RESET
   JRT-LBRACE SB-APPEND-C
   s" a" JRT-QKEY
   JRT-LBRACK SB-APPEND-C
   s" 1" SB-APPEND JRT-COMMA SB-APPEND-C
   s" 2" SB-APPEND JRT-COMMA SB-APPEND-C
   JRT-LBRACE SB-APPEND-C s" b" JRT-QKEY s" true" SB-APPEND JRT-RBRACE SB-APPEND-C
   JRT-RBRACK SB-APPEND-C
   JRT-COMMA SB-APPEND-C
   s" c" JRT-QKEY s" x" JRT-QSTR
   JRT-RBRACE SB-APPEND-C
   SB$ ;

: JRT-FIND$ ( -- ptr u8 n )
   SB-RESET
   JRT-LBRACE SB-APPEND-C
   s" a" JRT-QKEY s" 1" SB-APPEND JRT-COMMA SB-APPEND-C
   s" b" JRT-QKEY
   JRT-LBRACE SB-APPEND-C s" x" JRT-QKEY s" 9" SB-APPEND JRT-RBRACE SB-APPEND-C
   JRT-COMMA SB-APPEND-C
   s" c" JRT-QKEY s" 3" SB-APPEND
   JRT-RBRACE SB-APPEND-C
   SB$ ;

: JRT-WS$ ( -- ptr u8 n )                         \ "  {  \"a\" : 1 }  "
   SB-RESET
   JRT-SP SB-APPEND-C JRT-SP SB-APPEND-C
   JRT-LBRACE SB-APPEND-C JRT-SP SB-APPEND-C JRT-SP SB-APPEND-C
   JRT-DQ SB-APPEND-C s" a" SB-APPEND JRT-DQ SB-APPEND-C
   JRT-SP SB-APPEND-C JRT-COLON SB-APPEND-C JRT-SP SB-APPEND-C
   s" 1" SB-APPEND JRT-SP SB-APPEND-C
   JRT-RBRACE SB-APPEND-C JRT-SP SB-APPEND-C JRT-SP SB-APPEND-C
   SB$ ;

: JRT-DEEP$ ( n -- ptr u8 n ) {: depth:n :}       \ depth '[' then depth ']'
   SB-RESET
   depth 0 ?do JRT-LBRACK SB-APPEND-C loop
   depth 0 ?do JRT-RBRACK SB-APPEND-C loop
   SB$ ;

: JRT-OPENS$ ( n -- ptr u8 n ) {: depth:n :}      \ depth '[' only (overflow probe)
   SB-RESET
   depth 0 ?do JRT-LBRACK SB-APPEND-C loop
   SB$ ;

\ ---- positive: scalars ----------------------------------------------------
: JRT-SCALAR-INT ( ptr u8 n n -- ) {: want:n :}
   JRT-OPEN-A
   JR:NEXT JR:T-INT T=
   JR:INT want T=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: JRT-SCALAR-FLOAT ( ptr u8 n r -- ) {: want:r :}
   JRT-OPEN-A
   JR:NEXT JR:T-FLOAT T=
   JR:FLOAT want JRT-NEAR TTRUE
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: JRT-TEST-INTS ( -- )
   s" 0" 0 JRT-SCALAR-INT
   s" -1" -1 JRT-SCALAR-INT
   s" 42" 42 JRT-SCALAR-INT
   s" 9223372036854775807" STR-MAX-I64 JRT-SCALAR-INT ;

: JRT-TEST-FLOATS ( -- )
   s" 3.14" 3.14 JRT-SCALAR-FLOAT
   s" 1e9" 1000000000.0 JRT-SCALAR-FLOAT
   s" -2.5E-3" -0.0025 JRT-SCALAR-FLOAT
   s" 0.5" 0.5 JRT-SCALAR-FLOAT ;

: JRT-TEST-LITERALS ( -- )
   s" true" JRT-OPEN-A JR:NEXT JR:T-TRUE T= JR:NEXT JR:T-END T= JR:CLOSE
   s" false" JRT-OPEN-A JR:NEXT JR:T-FALSE T= JR:NEXT JR:T-END T= JR:CLOSE
   s" null" JRT-OPEN-A JR:NEXT JR:T-NULL T= JR:NEXT JR:T-END T= JR:CLOSE ;

\ ---- positive: strings / escapes ------------------------------------------
: JRT-TEST-STRING ( -- )
   JRT-HI-SRC$ JRT-OPEN-A
   JR:NEXT JR:T-STR T=
   JR:SPAN$ s" hi" T$=                              \ raw span excludes quotes
   s" hi" JRT-STR=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: JRT-TEST-ESCAPES ( -- )
   JRT-ESC-SRC$ JRT-OPEN-A
   JR:NEXT JR:T-STR T=
   JRT-ESC-WANT$ JRT-STR=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: JRT-TEST-UESCAPE ( -- )
   JRT-U-SRC$ JRT-OPEN-A
   JR:NEXT JR:T-STR T=
   JRT-U-WANT$ JRT-STR=
   JR:CLOSE ;

: JRT-TEST-SURROGATE ( -- )
   JRT-SUR-SRC$ JRT-OPEN-A
   JR:NEXT JR:T-STR T=
   JRT-SUR-WANT$ JRT-STR=
   JR:CLOSE ;

\ ---- positive: structure --------------------------------------------------
: JRT-TEST-EMPTY ( -- )
   s" {}" JRT-OPEN-A
   JR:NEXT JR:T-OBJ T= JR:NEXT JR:T-OBJ-END T= JR:NEXT JR:T-END T= JR:CLOSE
   s" []" JRT-OPEN-A
   JR:NEXT JR:T-ARR T= JR:NEXT JR:T-ARR-END T= JR:NEXT JR:T-END T= JR:CLOSE ;

: JRT-TEST-NESTED ( -- )
   JRT-NESTED$ JRT-OPEN-A
   JR:NEXT JR:T-OBJ T=
   JR:NEXT JR:T-KEY T= s" a" JRT-STR=
   JR:NEXT JR:T-ARR T=
   JR:NEXT JR:T-INT T= JR:INT 1 T=
   JR:NEXT JR:T-INT T= JR:INT 2 T=
   JR:NEXT JR:T-OBJ T=
   JR:NEXT JR:T-KEY T= s" b" JRT-STR=
   JR:NEXT JR:T-TRUE T=
   JR:NEXT JR:T-OBJ-END T=
   JR:NEXT JR:T-ARR-END T=
   JR:NEXT JR:T-KEY T= s" c" JRT-STR=
   JR:NEXT JR:T-STR T= s" x" JRT-STR=
   JR:NEXT JR:T-OBJ-END T=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: JRT-TEST-WS ( -- )
   JRT-WS$ JRT-OPEN-A
   JR:NEXT JR:T-OBJ T=
   JR:NEXT JR:T-KEY T= s" a" JRT-STR=
   JR:NEXT JR:T-INT T= JR:INT 1 T=
   JR:NEXT JR:T-OBJ-END T=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: JRT-DRAIN-ARR ( JR:reader n -- JR:reader ) {: depth:n :}
   depth 0 ?do JR:NEXT JR:T-ARR T= loop
   depth 0 ?do JR:NEXT JR:T-ARR-END T= loop
   JR:NEXT JR:T-END T= ;

: JRT-TEST-DEEP ( -- )
   60 JRT-DEEP$ JRT-OPEN-A
   60 JRT-DRAIN-ARR
   JR:CLOSE ;

\ ---- round-trip against lib/json-write.f ----------------------------------
: JRT-RT-BUILD ( -- ptr u8 n )
   JSON-WRITE:RESET
   JSON-WRITE:OBJECT-START
   s" name" JRT-NAME$ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" count" 42 JSON-WRITE:FIELD-U
   JSON-WRITE:COMMA
   s" ok" JRT-TRUE JSON-WRITE:FIELD-BOOL
   JSON-WRITE:COMMA
   s" none" JSON-WRITE:FIELD-NULL
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ ;

: JRT-TEST-ROUNDTRIP ( -- )
   JRT-RT-BUILD JRT-OPEN-A
   JR:NEXT JR:T-OBJ T=
   JR:NEXT JR:T-KEY T= s" name" JRT-STR=
   JR:NEXT JR:T-STR T= JRT-NAME$ JRT-STR=
   JR:NEXT JR:T-KEY T= s" count" JRT-STR=
   JR:NEXT JR:T-INT T= JR:INT 42 T=
   JR:NEXT JR:T-KEY T= s" ok" JRT-STR=
   JR:NEXT JR:T-TRUE T=
   JR:NEXT JR:T-KEY T= s" none" JRT-STR=
   JR:NEXT JR:T-NULL T=
   JR:NEXT JR:T-OBJ-END T=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

\ ---- JR:FIND-KEY ----------------------------------------------------------
: JRT-TEST-FIND-HIT ( -- )
   JRT-FIND$ JRT-OPEN-A
   JR:NEXT JR:T-OBJ T=
   s" a" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 1 T=
   JR:CLOSE ;

: JRT-TEST-FIND-SKIP ( -- )
   JRT-FIND$ JRT-OPEN-A
   JR:NEXT JR:T-OBJ T=
   s" b" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-OBJ T=
   JR:SKIP-VALUE
   s" c" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 3 T=
   JR:CLOSE ;

: JRT-TEST-FIND-MISS ( -- )
   JRT-FIND$ JRT-OPEN-A
   JR:NEXT JR:T-OBJ T=
   s" zzz" JR:FIND-KEY TFALSE
   JR:CLOSE ;

: JRT-TEST-FIND-NOLEAK ( -- )                     \ "x" only inside nested "b"
   JRT-FIND$ JRT-OPEN-A
   JR:NEXT JR:T-OBJ T=
   s" x" JR:FIND-KEY TFALSE
   JR:CLOSE ;

\ ---- explicit reader ownership and isolation -----------------------------
: JRT-TEST-INTERLEAVE ( -- )
   s" [1,3]" JRT-OPEN-A
   S\" {\qx\q:\qtwo\q}" JRT-OPEN-B
   JR:NEXT JR:T-OBJ T=
   >r JR:NEXT JR:T-ARR T= r>
   JR:NEXT JR:T-KEY T=
   JR:SPAN$ s" x" T$=
   JR:NEXT JR:T-STR T=
   s" two" JRT-STR=
   >r JR:NEXT JR:T-INT T= JR:INT 1 T= r>
   JR:NEXT JR:T-OBJ-END T=
   JR:NEXT JR:T-END T=
   JR:CLOSE
   JR:NEXT JR:T-INT T= JR:INT 3 T=
   JR:NEXT JR:T-ARR-END T=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: JRT-BAD-ADVANCE ( JR:reader -- JR:reader )
   JR:NEXT drop ;

: JRT-CATCH-BAD ( JR:reader -- JR:reader )
   [: JRT-BAD-ADVANCE ;] catch E-JR-MALFORMED T= ;

: JRT-NESTED-CATCH-BAD ( JR:reader -- JR:reader )
   [: JRT-CATCH-BAD ;] catch 0 T= ;

: JRT-TEST-CATCH-ISOLATION ( -- )
   s" [7,8]" JRT-OPEN-A
   s" [1,]" JRT-OPEN-B
   JR:NEXT JR:T-ARR T=
   JR:NEXT JR:T-INT T=
   JRT-NESTED-CATCH-BAD
   JR:CLOSE
   JR:NEXT JR:T-ARR T=
   JR:NEXT JR:T-INT T= JR:INT 7 T=
   JR:NEXT JR:T-INT T= JR:INT 8 T=
   JR:NEXT JR:T-ARR-END T=
   JR:NEXT JR:T-END T=
   JR:CLOSE ;

: JRT-BAD-CAPACITY ( -- )
   JRT-STATE-A JR:STORAGE-BYTES 1- s" 0" JR:INIT JR:CLOSE ;

: JRT-BAD-NULL-STORAGE ( -- )
   JRT-ZERO-PTR JR:STORAGE-BYTES s" 0" JR:INIT JR:CLOSE ;

: JRT-BAD-MISALIGNED-STORAGE ( -- )
   JRT-STATE-A 1+ JR:STORAGE-BYTES s" 0" JR:INIT JR:CLOSE ;

: JRT-BAD-NEGATIVE-SOURCE ( -- )
   JRT-STATE-A JR:STORAGE-BYTES s" 0" drop -1 JR:INIT JR:CLOSE ;

: JRT-BAD-NULL-SOURCE ( -- )
   JRT-STATE-A JR:STORAGE-BYTES JRT-ZERO-U8 1 JR:INIT JR:CLOSE ;

: JRT-TEST-PRIVATE-BYTE-CONSTANTS ( -- )
   s" JR:JR-BS" JRT-PRIVATE-CONSTANT
   s" JR:JR-TAB" JRT-PRIVATE-CONSTANT
   s" JR:JR-LF" JRT-PRIVATE-CONSTANT
   s" JR:JR-FF" JRT-PRIVATE-CONSTANT
   s" JR:JR-CR" JRT-PRIVATE-CONSTANT
   s" JR:JR-SP" JRT-PRIVATE-CONSTANT
   s" JR:JR-DQ" JRT-PRIVATE-CONSTANT
   s" JR:JR-PLUS" JRT-PRIVATE-CONSTANT
   s" JR:JR-COMMA" JRT-PRIVATE-CONSTANT
   s" JR:JR-MINUS" JRT-PRIVATE-CONSTANT
   s" JR:JR-DOT" JRT-PRIVATE-CONSTANT
   s" JR:JR-SLASH" JRT-PRIVATE-CONSTANT
   s" JR:JR-ZERO" JRT-PRIVATE-CONSTANT
   s" JR:JR-COLON" JRT-PRIVATE-CONSTANT
   s" JR:JR-E-UPPER" JRT-PRIVATE-CONSTANT
   s" JR:JR-LBRACK" JRT-PRIVATE-CONSTANT
   s" JR:JR-BACKSLASH" JRT-PRIVATE-CONSTANT
   s" JR:JR-RBRACK" JRT-PRIVATE-CONSTANT
   s" JR:JR-CH-B" JRT-PRIVATE-CONSTANT
   s" JR:JR-E-LOWER" JRT-PRIVATE-CONSTANT
   s" JR:JR-CH-F" JRT-PRIVATE-CONSTANT
   s" JR:JR-CH-N" JRT-PRIVATE-CONSTANT
   s" JR:JR-CH-R" JRT-PRIVATE-CONSTANT
   s" JR:JR-CH-T" JRT-PRIVATE-CONSTANT
   s" JR:JR-CH-U" JRT-PRIVATE-CONSTANT
   s" JR:JR-LBRACE" JRT-PRIVATE-CONSTANT
   s" JR:JR-RBRACE" JRT-PRIVATE-CONSTANT ;

: JRT-TEST-PRIVATE-HEX-CONSTANTS ( -- )
   s" JR:JR-HEXUP-A" JRT-PRIVATE-CONSTANT
   s" JR:JR-HEXUP-F" JRT-PRIVATE-CONSTANT
   s" JR:JR-HEXLO-A" JRT-PRIVATE-CONSTANT
   s" JR:JR-HEXLO-F" JRT-PRIVATE-CONSTANT
   s" JR:JR-HEX-TEN" JRT-PRIVATE-CONSTANT
   s" JR:JR-HEX-BASE" JRT-PRIVATE-CONSTANT ;

: JRT-TEST-PRIVATE-UNICODE-CONSTANTS ( -- )
   s" JR:JR-UTF1-MAX" JRT-PRIVATE-CONSTANT
   s" JR:JR-UTF2-MAX" JRT-PRIVATE-CONSTANT
   s" JR:JR-UTF3-MAX" JRT-PRIVATE-CONSTANT
   s" JR:JR-UTF-MASK" JRT-PRIVATE-CONSTANT
   s" JR:JR-UTF-CONT" JRT-PRIVATE-CONSTANT
   s" JR:JR-UTF2-LEAD" JRT-PRIVATE-CONSTANT
   s" JR:JR-UTF3-LEAD" JRT-PRIVATE-CONSTANT
   s" JR:JR-UTF4-LEAD" JRT-PRIVATE-CONSTANT
   s" JR:JR-SUR-HI" JRT-PRIVATE-CONSTANT
   s" JR:JR-SUR-LO" JRT-PRIVATE-CONSTANT
   s" JR:JR-SUR-END" JRT-PRIVATE-CONSTANT
   s" JR:JR-SUR-BASE" JRT-PRIVATE-CONSTANT
   s" JR:JR-SUR-SHIFT" JRT-PRIVATE-CONSTANT ;

: JRT-TEST-PRIVATE-CONSTANTS ( -- )
   JRT-TEST-PRIVATE-BYTE-CONSTANTS
   JRT-TEST-PRIVATE-HEX-CONSTANTS
   JRT-TEST-PRIVATE-UNICODE-CONSTANTS ;

: JRT-TEST-PRIVATE-STATE ( -- )
   s" JRT-BAD-PREMINT ( ptr a -- ptr n n ) JR:JR-STORAGE>PREMINT" JRT-REJECTED
   s" JRT-BAD-MINT ( ptr a -- JR:reader ) JR:JR-MINT-READER" JRT-REJECTED
   s" JRT-BAD-PRIVATE-STATE ( JR:reader -- JR:reader ptr n ptr u8 ) JR:JR-READER>STATE" JRT-REJECTED
   s" JRT-BAD-CONSUME ( JR:reader -- ) JR:JR-CONSUME-READER" JRT-REJECTED
   s" JRT-BAD-CELLS ( JR:reader -- JR:reader ptr n ) JR:JR-READER>CELLS" JRT-REJECTED
   s" JRT-BAD-NEXT ( ptr n -- n ) JR:JR-NEXT" JRT-REJECTED
   s" JRT-BAD-INT ( ptr n -- n ) JR:JR-INT" JRT-REJECTED
   s" JRT-BAD-FLOAT ( ptr n -- r ) JR:JR-FLOAT" JRT-REJECTED
   s" JRT-BAD-STR ( ptr n ptr u8 n -- n ) JR:JR-STR" JRT-REJECTED
   s" JRT-BAD-SKIP ( ptr n -- ) JR:JR-SKIP-VALUE" JRT-REJECTED
   s" JRT-BAD-FIND ( ptr n ptr u8 ptr u8 n -- bool ) JR:JR-FIND-KEY" JRT-REJECTED ;

: JRT-TEST-OWNERSHIP ( -- )
   s" short storage is rejected before mint" T-LABEL
   [: JRT-BAD-CAPACITY ;] E-JR-CAPACITY TTHROWSQ
   s" null storage is rejected before mint" T-LABEL
   [: JRT-BAD-NULL-STORAGE ;] E-JR-STORAGE TTHROWSQ
   s" misaligned storage is rejected before mint" T-LABEL
   [: JRT-BAD-MISALIGNED-STORAGE ;] E-JR-STORAGE TTHROWSQ
   s" negative source length is rejected before mint" T-LABEL
   [: JRT-BAD-NEGATIVE-SOURCE ;] E-JR-SOURCE TTHROWSQ
   s" null nonempty source is rejected before mint" T-LABEL
   [: JRT-BAD-NULL-SOURCE ;] E-JR-SOURCE TTHROWSQ
   s" null empty source remains a valid empty reader" T-LABEL
   JRT-STATE-A JR:STORAGE-BYTES JRT-ZERO-U8 0 JR:INIT JR:CLOSE
   s" byte and Unicode implementation constants stay private" T-LABEL
   JRT-TEST-PRIVATE-CONSTANTS
   s" raw representation and state helpers stay private" T-LABEL
   JRT-TEST-PRIVATE-STATE
   s" linear reader cannot be duplicated" T-LABEL
   s" JRT-BAD-DUP ( JR:reader -- JR:reader JR:reader ) dup" JRT-REJECTED
   s" linear reader cannot be discarded" T-LABEL
   s" JRT-BAD-DROP ( JR:reader -- ) drop" JRT-REJECTED
   s" raw storage cannot construct a reader" T-LABEL
   s" JRT-BAD-RAW ( ptr a -- JR:reader ) JR:JR-MINT-READER" JRT-REJECTED
   s" reader representation cannot be projected" T-LABEL
   s" JRT-BAD-PROJECT ( JR:reader -- JR:reader ptr n ptr u8 ) JR:JR-READER>STATE" JRT-REJECTED
   s" reader state cannot be rehomed" T-LABEL
   s" JRT-BAD-REHOME ( JR:reader ptr n ptr u8 -- JR:reader ) 2>r JR-READER:UNMAKE 2drop 2r> JR-READER:MAKE" JRT-REJECTED
   s" reader cannot be closed twice" T-LABEL
   s" JRT-BAD-CLOSE ( JR:reader -- ) JR:CLOSE JR:CLOSE" JRT-REJECTED ;

\ ---- negative fixtures (each forces one named throw) ----------------------
: JRT-BAD-TRAILING ( -- )
   s" 1 2" JRT-OPEN-A JR:NEXT drop JR:NEXT drop JR:CLOSE ;

: JRT-BAD-UNTERM ( -- )
   JRT-UNTERM-SRC$ JRT-OPEN-A JR:NEXT drop JR:CLOSE ;

: JRT-BAD-ESCAPE ( -- )
   JRT-BADESC-SRC$ JRT-OPEN-A JR:NEXT drop JRT-BUF JRT-CAP JR:STR drop JR:CLOSE ;

: JRT-BAD-SURROGATE ( -- )
   JRT-LONE-SRC$ JRT-OPEN-A JR:NEXT drop JRT-BUF JRT-CAP JR:STR drop JR:CLOSE ;

: JRT-BAD-DEPTH ( -- )
   70 JRT-OPENS$ JRT-OPEN-A
   100 0 ?do JR:NEXT drop loop
   JR:CLOSE ;

: JRT-BAD-BAREWORD ( -- )
   s" nul" JRT-OPEN-A JR:NEXT drop JR:CLOSE ;

: JRT-BAD-TRAILING-COMMA ( -- )
   s" [1,]" JRT-OPEN-A JR:NEXT drop JR:NEXT drop JR:NEXT drop JR:CLOSE ;

: JRT-BAD-COLON ( -- )
   SB-RESET
   JRT-LBRACE SB-APPEND-C JRT-DQ SB-APPEND-C s" a" SB-APPEND JRT-DQ SB-APPEND-C
   JRT-SP SB-APPEND-C s" 1" SB-APPEND JRT-RBRACE SB-APPEND-C
   SB$ JRT-OPEN-A
   JR:NEXT drop JR:NEXT drop JR:CLOSE ;

: JRT-BAD-OVERFLOW ( -- )
   s" 999999999999999999999999" JRT-OPEN-A JR:NEXT drop JR:INT drop JR:CLOSE ;

: JRT-BAD-STATE ( -- )
   JRT-HI-SRC$ JRT-OPEN-A JR:NEXT drop JR:INT drop JR:CLOSE ;

: JRT-TEST-NEGATIVE ( -- )
   [: JRT-BAD-TRAILING ;] E-JR-TRAILING TTHROWSQ
   [: JRT-BAD-UNTERM ;] E-JR-STRING TTHROWSQ
   [: JRT-BAD-ESCAPE ;] E-JR-ESCAPE TTHROWSQ
   [: JRT-BAD-SURROGATE ;] E-JR-SURROGATE TTHROWSQ
   [: JRT-BAD-DEPTH ;] E-JR-DEPTH TTHROWSQ
   [: JRT-BAD-BAREWORD ;] E-JR-MALFORMED TTHROWSQ
   [: JRT-BAD-TRAILING-COMMA ;] E-JR-MALFORMED TTHROWSQ
   [: JRT-BAD-COLON ;] E-JR-COLON TTHROWSQ
   [: JRT-BAD-OVERFLOW ;] E-JR-NUMBER TTHROWSQ
   [: JRT-BAD-STATE ;] E-JR-STATE TTHROWSQ ;

: JRT-CORE ( -- )
   JRT-TEST-INTS
   JRT-TEST-FLOATS
   JRT-TEST-LITERALS
   JRT-TEST-STRING
   JRT-TEST-ESCAPES
   JRT-TEST-UESCAPE
   JRT-TEST-SURROGATE
   JRT-TEST-EMPTY
   JRT-TEST-NESTED
   JRT-TEST-WS
   JRT-TEST-DEEP
   JRT-TEST-ROUNDTRIP
   JRT-TEST-FIND-HIT
   JRT-TEST-FIND-SKIP
   JRT-TEST-FIND-MISS
   JRT-TEST-FIND-NOLEAK
   JRT-TEST-INTERLEAVE
   JRT-TEST-CATCH-ISOLATION
   JRT-TEST-OWNERSHIP
   JRT-TEST-NEGATIVE ;

public

: RUN ( -- )
   JRT-CORE ;

;package

\ ---- bounds regression: JR-AT is private, so probe it inside package JR ----
\ Guards the fail-closed source-cursor read: an index below zero or at/after the
\ source length must throw E-JR-BOUNDS, never read past the caller's buffer.
package JR

private

here CELL 1- and CELL swap - CELL 1- and allot
create JRT-BOUNDS-STATE STORAGE-BYTES allot

: JRT-BOUNDS-OPEN ( -- JR:reader )
   JRT-BOUNDS-STATE STORAGE-BYTES s" abc" INIT ;

: JRT-BOUND-AT ( JR:reader n -- JR:reader n ) {: idx:n :}
   JR-READER>CELLS idx JR-AT ;

: JRT-BAD-BOUND-HIGH ( -- )
   JRT-BOUNDS-OPEN 3 JRT-BOUND-AT drop CLOSE ;

: JRT-BAD-BOUND-LOW ( -- )
   JRT-BOUNDS-OPEN -1 JRT-BOUND-AT drop CLOSE ;

: JRT-BOUNDS ( -- )
   JRT-BOUNDS-OPEN
   s" JR-AT reads a valid index" T-LABEL
   0 JRT-BOUND-AT 97 T=
   CLOSE
   s" JR-AT rejects the length index" T-LABEL
   [: JRT-BAD-BOUND-HIGH ;] E-JR-BOUNDS TTHROWSQ
   s" JR-AT rejects a negative index" T-LABEL
   [: JRT-BAD-BOUND-LOW ;] E-JR-BOUNDS TTHROWSQ ;
;package

T-RESET
JSON-READ-TEST:RUN
package JR JRT-BOUNDS ;package
T-REPORT
s" json-read-test: ok" type cr
