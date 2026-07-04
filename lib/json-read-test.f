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

256 constant JRT-CAP
create JRT-BUF JRT-CAP allot

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
: JRT-STR= ( ptr u8 n -- ) {: ea:ptr eu:n :}     \ current string/key decodes to bytes
   JRT-BUF JRT-CAP JR-STR {: got:n :}
   JRT-BUF got ea eu T$= ;

\ ---- structural / SB-built fixtures ---------------------------------------
: JRT-QKEY ( ptr u8 n -- )                       \ "key":  onto the builder
   JR-DQ SB-APPEND-C SB-APPEND JR-DQ SB-APPEND-C JR-COLON SB-APPEND-C ;

: JRT-QSTR ( ptr u8 n -- )                       \ "str"  onto the builder
   JR-DQ SB-APPEND-C SB-APPEND JR-DQ SB-APPEND-C ;

: JRT-NESTED$ ( -- ptr u8 n )
   SB-RESET
   JR-LBRACE SB-APPEND-C
   s" a" JRT-QKEY
   JR-LBRACK SB-APPEND-C
   s" 1" SB-APPEND JR-COMMA SB-APPEND-C
   s" 2" SB-APPEND JR-COMMA SB-APPEND-C
   JR-LBRACE SB-APPEND-C s" b" JRT-QKEY s" true" SB-APPEND JR-RBRACE SB-APPEND-C
   JR-RBRACK SB-APPEND-C
   JR-COMMA SB-APPEND-C
   s" c" JRT-QKEY s" x" JRT-QSTR
   JR-RBRACE SB-APPEND-C
   SB$ ;

: JRT-FIND$ ( -- ptr u8 n )
   SB-RESET
   JR-LBRACE SB-APPEND-C
   s" a" JRT-QKEY s" 1" SB-APPEND JR-COMMA SB-APPEND-C
   s" b" JRT-QKEY
   JR-LBRACE SB-APPEND-C s" x" JRT-QKEY s" 9" SB-APPEND JR-RBRACE SB-APPEND-C
   JR-COMMA SB-APPEND-C
   s" c" JRT-QKEY s" 3" SB-APPEND
   JR-RBRACE SB-APPEND-C
   SB$ ;

: JRT-WS$ ( -- ptr u8 n )                         \ "  {  \"a\" : 1 }  "
   SB-RESET
   JR-SP SB-APPEND-C JR-SP SB-APPEND-C
   JR-LBRACE SB-APPEND-C JR-SP SB-APPEND-C JR-SP SB-APPEND-C
   JR-DQ SB-APPEND-C s" a" SB-APPEND JR-DQ SB-APPEND-C
   JR-SP SB-APPEND-C JR-COLON SB-APPEND-C JR-SP SB-APPEND-C
   s" 1" SB-APPEND JR-SP SB-APPEND-C
   JR-RBRACE SB-APPEND-C JR-SP SB-APPEND-C JR-SP SB-APPEND-C
   SB$ ;

: JRT-DEEP$ ( n -- ptr u8 n ) {: depth:n :}       \ depth '[' then depth ']'
   SB-RESET
   depth 0 ?do JR-LBRACK SB-APPEND-C loop
   depth 0 ?do JR-RBRACK SB-APPEND-C loop
   SB$ ;

: JRT-OPENS$ ( n -- ptr u8 n ) {: depth:n :}      \ depth '[' only (overflow probe)
   SB-RESET
   depth 0 ?do JR-LBRACK SB-APPEND-C loop
   SB$ ;

\ ---- positive: scalars ----------------------------------------------------
: JRT-SCALAR-INT ( ptr u8 n n -- ) {: want:n :}
   JR-INIT
   JR-NEXT JT-INT T=
   JR-INT want T=
   JR-NEXT JT-END T= ;

: JRT-SCALAR-FLOAT ( ptr u8 n r -- ) {: want:r :}
   JR-INIT
   JR-NEXT JT-FLOAT T=
   JR-FLOAT want JRT-NEAR TTRUE
   JR-NEXT JT-END T= ;

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
   s" true" JR-INIT JR-NEXT JT-TRUE T= JR-NEXT JT-END T=
   s" false" JR-INIT JR-NEXT JT-FALSE T= JR-NEXT JT-END T=
   s" null" JR-INIT JR-NEXT JT-NULL T= JR-NEXT JT-END T= ;

\ ---- positive: strings / escapes ------------------------------------------
: JRT-TEST-STRING ( -- )
   JRT-HI-SRC$ JR-INIT
   JR-NEXT JT-STR T=
   JR-SPAN$ s" hi" T$=                              \ raw span excludes quotes
   s" hi" JRT-STR=
   JR-NEXT JT-END T= ;

: JRT-TEST-ESCAPES ( -- )
   JRT-ESC-SRC$ JR-INIT
   JR-NEXT JT-STR T=
   JRT-ESC-WANT$ JRT-STR=
   JR-NEXT JT-END T= ;

: JRT-TEST-UESCAPE ( -- )
   JRT-U-SRC$ JR-INIT
   JR-NEXT JT-STR T=
   JRT-U-WANT$ JRT-STR= ;

: JRT-TEST-SURROGATE ( -- )
   JRT-SUR-SRC$ JR-INIT
   JR-NEXT JT-STR T=
   JRT-SUR-WANT$ JRT-STR= ;

\ ---- positive: structure --------------------------------------------------
: JRT-TEST-EMPTY ( -- )
   s" {}" JR-INIT
   JR-NEXT JT-OBJ T= JR-NEXT JT-OBJ-END T= JR-NEXT JT-END T=
   s" []" JR-INIT
   JR-NEXT JT-ARR T= JR-NEXT JT-ARR-END T= JR-NEXT JT-END T= ;

: JRT-TEST-NESTED ( -- )
   JRT-NESTED$ JR-INIT
   JR-NEXT JT-OBJ T=
   JR-NEXT JT-KEY T= s" a" JRT-STR=
   JR-NEXT JT-ARR T=
   JR-NEXT JT-INT T= JR-INT 1 T=
   JR-NEXT JT-INT T= JR-INT 2 T=
   JR-NEXT JT-OBJ T=
   JR-NEXT JT-KEY T= s" b" JRT-STR=
   JR-NEXT JT-TRUE T=
   JR-NEXT JT-OBJ-END T=
   JR-NEXT JT-ARR-END T=
   JR-NEXT JT-KEY T= s" c" JRT-STR=
   JR-NEXT JT-STR T= s" x" JRT-STR=
   JR-NEXT JT-OBJ-END T=
   JR-NEXT JT-END T= ;

: JRT-TEST-WS ( -- )
   JRT-WS$ JR-INIT
   JR-NEXT JT-OBJ T=
   JR-NEXT JT-KEY T= s" a" JRT-STR=
   JR-NEXT JT-INT T= JR-INT 1 T=
   JR-NEXT JT-OBJ-END T=
   JR-NEXT JT-END T= ;

: JRT-DRAIN-ARR ( n -- ) {: depth:n :}
   depth 0 ?do JR-NEXT JT-ARR T= loop
   depth 0 ?do JR-NEXT JT-ARR-END T= loop
   JR-NEXT JT-END T= ;

: JRT-TEST-DEEP ( -- )
   60 JRT-DEEP$ JR-INIT
   60 JRT-DRAIN-ARR ;

\ ---- round-trip against lib/json-write.f ----------------------------------
: JRT-RT-BUILD ( -- ptr u8 n )
   JW-RESET
   JW-OBJECT-START
   s" name" JRT-NAME$ JW-FIELD-S
   JW-COMMA
   s" count" 42 JW-FIELD-U
   JW-COMMA
   s" ok" JRT-TRUE JW-FIELD-BOOL
   JW-COMMA
   s" none" JW-FIELD-NULL
   JW-OBJECT-END
   JW$ ;

: JRT-TEST-ROUNDTRIP ( -- )
   JRT-RT-BUILD JR-INIT
   JR-NEXT JT-OBJ T=
   JR-NEXT JT-KEY T= s" name" JRT-STR=
   JR-NEXT JT-STR T= JRT-NAME$ JRT-STR=
   JR-NEXT JT-KEY T= s" count" JRT-STR=
   JR-NEXT JT-INT T= JR-INT 42 T=
   JR-NEXT JT-KEY T= s" ok" JRT-STR=
   JR-NEXT JT-TRUE T=
   JR-NEXT JT-KEY T= s" none" JRT-STR=
   JR-NEXT JT-NULL T=
   JR-NEXT JT-OBJ-END T=
   JR-NEXT JT-END T= ;

\ ---- JR-FIND-KEY ----------------------------------------------------------
: JRT-TEST-FIND-HIT ( -- )
   JRT-FIND$ JR-INIT
   JR-NEXT JT-OBJ T=
   s" a" JR-FIND-KEY TTRUE
   JR-TOKEN JT-INT T=
   JR-INT 1 T= ;

: JRT-TEST-FIND-SKIP ( -- )
   JRT-FIND$ JR-INIT
   JR-NEXT JT-OBJ T=
   s" b" JR-FIND-KEY TTRUE
   JR-TOKEN JT-OBJ T=
   JR-SKIP-VALUE
   s" c" JR-FIND-KEY TTRUE
   JR-TOKEN JT-INT T=
   JR-INT 3 T= ;

: JRT-TEST-FIND-MISS ( -- )
   JRT-FIND$ JR-INIT
   JR-NEXT JT-OBJ T=
   s" zzz" JR-FIND-KEY TFALSE ;

: JRT-TEST-FIND-NOLEAK ( -- )                     \ "x" only inside nested "b"
   JRT-FIND$ JR-INIT
   JR-NEXT JT-OBJ T=
   s" x" JR-FIND-KEY TFALSE ;

\ ---- negative fixtures (each forces one named throw) ----------------------
: JRT-BAD-TRAILING ( -- )
   s" 1 2" JR-INIT JR-NEXT drop JR-NEXT drop ;

: JRT-BAD-UNTERM ( -- )
   JRT-UNTERM-SRC$ JR-INIT JR-NEXT drop ;

: JRT-BAD-ESCAPE ( -- )
   JRT-BADESC-SRC$ JR-INIT JR-NEXT drop JRT-BUF JRT-CAP JR-STR drop ;

: JRT-BAD-SURROGATE ( -- )
   JRT-LONE-SRC$ JR-INIT JR-NEXT drop JRT-BUF JRT-CAP JR-STR drop ;

: JRT-BAD-DEPTH ( -- )
   70 JRT-OPENS$ JR-INIT
   100 0 ?do JR-NEXT drop loop ;

: JRT-BAD-BAREWORD ( -- )
   s" nul" JR-INIT JR-NEXT drop ;

: JRT-BAD-TRAILING-COMMA ( -- )
   s" [1,]" JR-INIT JR-NEXT drop JR-NEXT drop JR-NEXT drop ;

: JRT-BAD-COLON ( -- )
   SB-RESET
   JR-LBRACE SB-APPEND-C JR-DQ SB-APPEND-C s" a" SB-APPEND JR-DQ SB-APPEND-C
   JR-SP SB-APPEND-C s" 1" SB-APPEND JR-RBRACE SB-APPEND-C
   SB$ JR-INIT
   JR-NEXT drop JR-NEXT drop ;

: JRT-BAD-OVERFLOW ( -- )
   s" 999999999999999999999999" JR-INIT JR-NEXT drop JR-INT drop ;

: JRT-BAD-STATE ( -- )
   JRT-HI-SRC$ JR-INIT JR-NEXT drop JR-INT drop ;

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

: JRT-MAIN ( -- )
   T-RESET
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
   JRT-TEST-NEGATIVE
   T-REPORT
   s" json-read-test: ok" type cr ;

JRT-MAIN
