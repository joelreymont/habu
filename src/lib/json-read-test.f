\ json-read-test.f - focused tests for the checked JSON pull parser.
\ Run: bin/hb --load src/lib/json-read-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/float.f
require src/lib/memory.f
require src/lib/test.f
require src/lib/time.f
require src/lib/json.f
require src/lib/test/ending.f
require src/lib/test/guard-page.f
require src/lib/test/subject.f

package JSON-TEST
private

256 constant JRT-CAP
create JRT-BUF JRT-CAP allot
TYPED-VARIABLE JRT-A JSON:cursor
TYPED-VARIABLE JRT-B JSON:cursor
create JRT-SUBJECT-OUT $400 allot
create JRT-SUBJECT-ERR $400 allot

\ The round-trip fixture below owns the writer's bytes; src/lib/json/write.f
\ holds none.
JRT-CAP BUFFER: JRT-WRITE-BUF
TYPED-VARIABLE JRT-WRITER JSON:writer

: JRT-OPEN-A ( ptr u8 n -- JSON:reader )
   {: source:ptr len:n :}
   JRT-A source len JSON:READ-INIT ;

: JRT-OPEN-B ( ptr u8 n -- JSON:reader )
   {: source:ptr len:n :}
   JRT-B source len JSON:READ-INIT ;

\ The null address the reader must refuse, taken from the core's declared null
\ slot: a `create`d cell read back through `ptr-field` is an undeclared raw
\ storage cell and the checker refuses the pointer it publishes.
: JRT-ZERO-U8 ( -- ptr u8 )
   NULL-PTR ;

: JRT-REJECTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 = TFALSE ;

\ Test-local byte constants for building JSON fixtures. The byte constants of
\ src/lib/json/read.f are package-private, so the test spells the ASCII codes
\ itself.
34 constant JRT-DQ
32 constant JRT-SP
58 constant JRT-COLON
44 constant JRT-COMMA
123 constant JRT-LBRACE
125 constant JRT-RBRACE
91 constant JRT-LBRACK
93 constant JRT-RBRACK
107 constant JRT-K
120 constant JRT-X
49 constant JRT-DIGIT-1
50 constant JRT-DIGIT-2

: JRT-NEAR ( r r -- bool )
   f- fabs 0.000001 f< ;

: JRT-TRUE ( -- bool )
   0 0= ;

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
create JRT-BADHEX-SRC 34 c, 92 c, 117 c, 48 c, 48 c, 71 c, 48 c, 34 c, \ "\u00G0"
: JRT-BADHEX-SRC$ ( -- ptr u8 n ) JRT-BADHEX-SRC 8 ;
create JRT-LOW-SRC 34 c, 92 c, 117 c, 68 c, 67 c, 48 c, 48 c, 34 c,   \ "\uDC00"
: JRT-LOW-SRC$ ( -- ptr u8 n ) JRT-LOW-SRC 8 ;
create JRT-BADPAIR-SRC
   34 c, 92 c, 117 c, 68 c, 56 c, 48 c, 48 c,
   92 c, 117 c, 48 c, 48 c, 52 c, 49 c, 34 c,                       \ "\uD800\u0041"
: JRT-BADPAIR-SRC$ ( -- ptr u8 n ) JRT-BADPAIR-SRC 14 ;
create JRT-UNTERM-SRC 34 c, 97 c, 98 c, 99 c,        \ "abc
: JRT-UNTERM-SRC$ ( -- ptr u8 n ) JRT-UNTERM-SRC 4 ;
\ "ab\n with a stated length of 4: the source ends at the backslash, and the
\ n past that length must not complete the escape.
create JRT-RUN-BS-SRC 34 c, 97 c, 98 c, 92 c, 110 c,
: JRT-RUN-BS-SRC$ ( -- ptr u8 n ) JRT-RUN-BS-SRC 4 ;
create JRT-RUN-CTRL-SRC 34 c, 97 c, 10 c, 34 c,      \ "a<LF>": a run, then a raw control byte
: JRT-RUN-CTRL-SRC$ ( -- ptr u8 n ) JRT-RUN-CTRL-SRC 4 ;

create JRT-UTF8-SRC
   34 c,
   $C2 c, $80 c,                 \ U+0080
   $E0 c, $A0 c, $80 c,          \ U+0800
   $F4 c, $8F c, $BF c, $BF c,   \ U+10FFFF
   34 c,
: JRT-UTF8-SRC$ ( -- ptr u8 n ) JRT-UTF8-SRC 11 ;
: JRT-UTF8-WANT$ ( -- ptr u8 n ) JRT-UTF8-SRC 1+ 9 ;

create JRT-UTF8-OVERLONG 34 c, $C0 c, $AF c, 34 c,
: JRT-UTF8-OVERLONG$ ( -- ptr u8 n ) JRT-UTF8-OVERLONG 4 ;
create JRT-UTF8-STRAY 34 c, $80 c, 34 c,
: JRT-UTF8-STRAY$ ( -- ptr u8 n ) JRT-UTF8-STRAY 3 ;
create JRT-UTF8-TRUNC 34 c, $E2 c, $82 c, 34 c,
: JRT-UTF8-TRUNC$ ( -- ptr u8 n ) JRT-UTF8-TRUNC 4 ;
create JRT-UTF8-SUR 34 c, $ED c, $A0 c, $80 c, 34 c,
: JRT-UTF8-SUR$ ( -- ptr u8 n ) JRT-UTF8-SUR 5 ;
create JRT-UTF8-HIGH 34 c, $F4 c, $90 c, $80 c, $80 c, 34 c,
: JRT-UTF8-HIGH$ ( -- ptr u8 n ) JRT-UTF8-HIGH 6 ;

\ round-trip value: "A\"B" holds bytes A " B
create JRT-NAME 65 c, 34 c, 66 c,
: JRT-NAME$ ( -- ptr u8 n ) JRT-NAME 3 ;

\ {"a\u0062\n\"":9} -> key bytes `ab`, newline, quote
create JRT-FIND-ESC-SRC
   123 c, 34 c, 97 c,
   92 c, 117 c, 48 c, 48 c, 54 c, 50 c,
   92 c, 110 c, 92 c, 34 c,
   34 c, 58 c, 57 c, 125 c,
: JRT-FIND-ESC-SRC$ ( -- ptr u8 n ) JRT-FIND-ESC-SRC 17 ;
create JRT-FIND-ESC-WANT 97 c, 98 c, 10 c, 34 c,
: JRT-FIND-ESC-WANT$ ( -- ptr u8 n ) JRT-FIND-ESC-WANT 4 ;

768 constant JRT-LONG-KEY-CAP
create JRT-LONG-KEY JRT-LONG-KEY-CAP allot

: JRT-STR= ( JSON:reader ptr u8 n -- JSON:reader ) {: ea:ptr eu:n :}
   JRT-BUF JRT-CAP JSON:READ-STR {: got:n :}
   JRT-BUF got ea eu T$= ;

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

: JRT-VALUE-SPAN$ ( -- ptr u8 n )
   s\" {\"a\":[1,{\"b\":true}],\"c\":0}" ;

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

: JRT-ANCHOR$ ( -- ptr u8 n )
   SB-RESET
   JRT-LBRACE SB-APPEND-C
   s" nested" JRT-QKEY
   JRT-LBRACE SB-APPEND-C s" inner" JRT-QKEY s" 1" SB-APPEND
   JRT-RBRACE SB-APPEND-C JRT-COMMA SB-APPEND-C
   s" outer" JRT-QKEY s" 2" SB-APPEND
   JRT-RBRACE SB-APPEND-C
   SB$ ;

: JRT-LONG-KEY! ( n -- ) {: len:n :}
   len 0 ?do JRT-K JRT-LONG-KEY i + c! loop ;

: JRT-LONG-FIND$ ( n -- ptr u8 n ) {: len:n :}
   len JRT-LONG-KEY!
   SB-RESET
   JRT-LBRACE SB-APPEND-C
   JRT-LONG-KEY len JRT-QKEY s" 7" SB-APPEND
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

\ Deep documents outgrow SB, so they are built in their own buffer: two texts
\ of 100000 levels around one digit fill it.
400002 constant JRT-DEEP-CAP
create JRT-DEEP-BUF JRT-DEEP-CAP allot

: JRT-FILL ( n n n -- ) {: at:n count:n byte:n :}
   count 0 ?do byte JRT-DEEP-BUF at i + + c! loop ;

: JRT-DEEP$ ( n -- ptr u8 n ) {: depth:n :}       \ depth '[' then depth ']'
   0 depth JRT-LBRACK JRT-FILL
   depth depth JRT-RBRACK JRT-FILL
   JRT-DEEP-BUF depth 2 * ;

\ depth '[' around one digit byte, then depth ']', at offset at of the buffer.
: JRT-DEEP-ONE$ ( n n n -- ptr u8 n ) {: at:n depth:n digit:n :}
   at depth JRT-LBRACK JRT-FILL
   at depth + 1 digit JRT-FILL
   at depth + 1+ depth JRT-RBRACK JRT-FILL
   JRT-DEEP-BUF at + depth 2 * 1+ ;

: JRT-OPENS$ ( n -- ptr u8 n ) {: depth:n :}      \ depth '[' only
   0 depth JRT-LBRACK JRT-FILL
   JRT-DEEP-BUF depth ;

: JRT-DEEP-BAD$ ( n -- ptr u8 n ) {: depth:n :}   \ depth '[' then a bare x
   0 depth JRT-LBRACK JRT-FILL
   depth 1 JRT-X JRT-FILL
   JRT-DEEP-BUF depth 1+ ;

\ depth objects, each but the innermost holding only "a"; the innermost holds
\ "x":1,"y":7.
: JRT-NEST-OBJ$ ( n -- ptr u8 n ) {: depth:n :}
   SB-RESET
   depth 1- 0 ?do JRT-LBRACE SB-APPEND-C s" a" JRT-QKEY loop
   JRT-LBRACE SB-APPEND-C
   s" x" JRT-QKEY s" 1" SB-APPEND JRT-COMMA SB-APPEND-C
   s" y" JRT-QKEY s" 7" SB-APPEND
   JRT-RBRACE SB-APPEND-C
   depth 1- 0 ?do JRT-RBRACE SB-APPEND-C loop
   SB$ ;

\ Byte c at offset at of the deep buffer; answers the next offset.
: JRT-PUT ( n n -- n ) {: at:n c:n :}
   c JRT-DEEP-BUF at + c!  at 1+ ;

\ The text from offset at of the deep buffer to offset end.
: JRT-SINCE$ ( n n -- ptr u8 n ) {: end:n at:n :}
   JRT-DEEP-BUF at +  end at - ;

\ depth objects at offset at, each holding only "k", around the digit 1.
: JRT-NEST-AT$ ( n n -- ptr u8 n ) {: at:n depth:n :}
   at depth 0 ?do
      JRT-LBRACE JRT-PUT JRT-DQ JRT-PUT JRT-K JRT-PUT JRT-DQ JRT-PUT JRT-COLON JRT-PUT
   loop
   JRT-DIGIT-1 JRT-PUT
   depth 0 ?do JRT-RBRACE JRT-PUT loop
   at JRT-SINCE$ ;

\ The decimal digit of idx at unit, at offset at.
: JRT-DIGIT-PUT ( n n n -- n ) {: at:n idx:n unit:n :}
   at  idx unit / 10 mod JRT-DIGIT-1 1- +  JRT-PUT ;

\ Member idx, "k" and four digits of idx as its key and 1 as its value.
: JRT-MEMBER-AT ( n n -- n ) {: at:n idx:n :}
   at JRT-DQ JRT-PUT JRT-K JRT-PUT
   idx 1000 JRT-DIGIT-PUT  idx 100 JRT-DIGIT-PUT  idx 10 JRT-DIGIT-PUT  idx 1 JRT-DIGIT-PUT
   JRT-DQ JRT-PUT JRT-COLON JRT-PUT JRT-DIGIT-1 JRT-PUT ;

\ One object of count members at offset at, in order or reversed.
: JRT-FLAT-AT$ ( n n bool -- ptr u8 n ) {: at:n count:n back:bool :}
   at JRT-LBRACE JRT-PUT
   count 0 ?do
      i 0 > if JRT-COMMA JRT-PUT then
      back if count 1- i - else i then JRT-MEMBER-AT
   loop
   JRT-RBRACE JRT-PUT
   at JRT-SINCE$ ;

: JRT-SCALAR-INT ( ptr u8 n n -- ) {: want:n :}
   JRT-OPEN-A
   JSON:READ-NEXT JSON:T-INT T=
   JSON:READ-INT want T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-SCALAR-FLOAT ( ptr u8 n r -- ) {: want:r :}
   JRT-OPEN-A
   JSON:READ-NEXT JSON:T-FLOAT T=
   JSON:READ-FLOAT want JRT-NEAR TTRUE
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

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
   s" true" JRT-OPEN-A JSON:READ-NEXT JSON:T-TRUE T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE
   s" false" JRT-OPEN-A JSON:READ-NEXT JSON:T-FALSE T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE
   s" null" JRT-OPEN-A JSON:READ-NEXT JSON:T-NULL T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE ;

: JRT-TEST-STRING ( -- )
   JRT-HI-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JSON:READ-SPAN$ s" hi" T$=                              \ raw span excludes quotes
   s" hi" JRT-STR=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-TEST-ESCAPES ( -- )
   JRT-ESC-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-ESC-WANT$ JRT-STR=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-TEST-UESCAPE ( -- )
   JRT-U-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-U-WANT$ JRT-STR=
   JSON:READ-CLOSE ;

: JRT-TEST-SURROGATE ( -- )
   JRT-SUR-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-SUR-WANT$ JRT-STR=
   JSON:READ-CLOSE ;

: JRT-TEST-UTF8 ( -- )
   JRT-UTF8-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-UTF8-WANT$ JRT-STR=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-TEST-EMPTY ( -- )
   s" {}" JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-NEXT JSON:T-OBJ-END T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE
   s" []" JRT-OPEN-A
   JSON:READ-NEXT JSON:T-ARR T= JSON:READ-NEXT JSON:T-ARR-END T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE ;

: JRT-TEST-NESTED ( -- )
   JRT-NESTED$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-NEXT JSON:T-KEY T= s" a" JRT-STR=
   JSON:READ-NEXT JSON:T-ARR T=
   JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 1 T=
   JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 2 T=
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-NEXT JSON:T-KEY T= s" b" JRT-STR=
   JSON:READ-NEXT JSON:T-TRUE T=
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-NEXT JSON:T-ARR-END T=
   JSON:READ-NEXT JSON:T-KEY T= s" c" JRT-STR=
   JSON:READ-NEXT JSON:T-STR T= s" x" JRT-STR=
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

\ Every token kind of one document, then a number outside the kinds, named
\ for a diagnostic.
: JRT-TEST-KIND-NAMES ( -- )
   s\" {\"k\":[\"s\",1,0.5,true,false,null]}" JRT-OPEN-A
   JSON:READ-NEXT JSON:KIND$ s" an object" T$=
   JSON:READ-NEXT JSON:KIND$ s" a key" T$=
   JSON:READ-NEXT JSON:KIND$ s" an array" T$=
   JSON:READ-NEXT JSON:KIND$ s" a string" T$=
   JSON:READ-NEXT JSON:KIND$ s" an integer" T$=
   JSON:READ-NEXT JSON:KIND$ s" a number with a fraction or exponent" T$=
   JSON:READ-NEXT JSON:KIND$ s" true" T$=
   JSON:READ-NEXT JSON:KIND$ s" false" T$=
   JSON:READ-NEXT JSON:KIND$ s" null" T$=
   JSON:READ-NEXT JSON:KIND$ s" an array's end" T$=
   JSON:READ-NEXT JSON:KIND$ s" an object's end" T$=
   JSON:READ-NEXT JSON:KIND$ s" the text's end" T$=
   JSON:READ-CLOSE
   JSON:T-END 1+ JSON:KIND$ s" no token kind" T$= ;

: JRT-TEST-VALUE-SPAN ( -- )
   JRT-VALUE-SPAN$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-NEXT JSON:T-KEY T= s" a" JRT-STR=
   JSON:READ-NEXT JSON:T-ARR T=
   JSON:READ-VALUE-SPAN$ s\" [1,{\"b\":true}]" T$=
   JSON:READ-NEXT JSON:T-KEY T= s" c" JRT-STR=
   JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 0 T=
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE
   JRT-VALUE-SPAN$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-VALUE-SPAN$ s\" {\"a\":[1,{\"b\":true}],\"c\":0}" T$=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-ANY-SPAN$ ( -- ptr u8 n )
   s\" {\"o\":{\"k\":1},\"a\":[1,2],\"s\":\"x\\\"y\",\"i\":-12,\"f\":1.5e-3,\"t\":true,\"u\":false,\"n\":null}" ;

\ The next value's span is this text, and the token after it is this key.
: JRT-ANY-KEY ( JSON:reader ptr u8 n ptr u8 n -- JSON:reader )
   {: va:ptr vu:n ka:ptr ku:n :}
   JSON:READ-NEXT drop
   JSON:READ-ANY-SPAN$ va vu T$=
   JSON:READ-NEXT JSON:T-KEY T= ka ku JRT-STR= ;

: JRT-BAD-ANY-KEY ( -- )
   JRT-ANY-SPAN$ JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop
   JSON:READ-ANY-SPAN$ drop drop JSON:READ-CLOSE ;

: JRT-BAD-ANY-END ( -- )
   s" {}" JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop
   JSON:READ-ANY-SPAN$ drop drop JSON:READ-CLOSE ;

: JRT-TEST-ANY-SPAN ( -- )
   JRT-ANY-SPAN$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-NEXT JSON:T-KEY T= s" o" JRT-STR=
   s\" {\"k\":1}" s" a" JRT-ANY-KEY
   s" [1,2]" s" s" JRT-ANY-KEY
   s\" \"x\\\"y\"" s" i" JRT-ANY-KEY
   s" -12" s" f" JRT-ANY-KEY
   s" 1.5e-3" s" t" JRT-ANY-KEY
   s" true" s" u" JRT-ANY-KEY
   s" false" s" n" JRT-ANY-KEY
   JSON:READ-NEXT drop JSON:READ-ANY-SPAN$ s" null" T$=
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-CLOSE
   [: JRT-BAD-ANY-KEY ;] E-JSON-READ-STATE TTHROWSQ
   JRT-A JSON:READ-DISPOSE
   [: JRT-BAD-ANY-END ;] E-JSON-READ-STATE TTHROWSQ
   JRT-A JSON:READ-DISPOSE ;

: JRT-TEST-WS ( -- )
   JRT-WS$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-NEXT JSON:T-KEY T= s" a" JRT-STR=
   JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 1 T=
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-DRAIN-ARR ( JSON:reader n -- JSON:reader ) {: depth:n :}
   depth 0 ?do JSON:READ-NEXT JSON:T-ARR T= loop
   depth 0 ?do JSON:READ-NEXT JSON:T-ARR-END T= loop
   JSON:READ-NEXT JSON:T-END T= ;

: JRT-TEST-END-SPAN ( -- )
   s" 1  " {: source:ptr len:n :}
   source len JRT-OPEN-A
   JSON:READ-NEXT JSON:T-INT T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-TOKEN JSON:T-END T=
   JSON:READ-SPAN$ {: span:ptr span-len:n :}
   span-len 0 T=
   span source len + = TTRUE
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-TOKEN JSON:T-END T=
   JSON:READ-SPAN$ {: span2:ptr span2-len:n :}
   span2-len 0 T=
   span2 source len + = TTRUE
   JSON:READ-CLOSE ;

: JRT-RT-BUILD ( -- ptr u8 n )
   JRT-WRITER JRT-WRITE-BUF JRT-CAP JSON:WRITE-OPEN
   JSON:WRITE-OBJECT-START
   s" name" JRT-NAME$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" count" 42 JSON:WRITE-FIELD-U
   JSON:WRITE-COMMA
   s" ok" JRT-TRUE JSON:WRITE-FIELD-BOOL
   JSON:WRITE-COMMA
   s" none" JSON:WRITE-FIELD-NULL
   JSON:WRITE-OBJECT-END
   JSON:WRITTEN$ ;

: JRT-TEST-ROUNDTRIP ( -- )
   JRT-RT-BUILD JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-NEXT JSON:T-KEY T= s" name" JRT-STR=
   JSON:READ-NEXT JSON:T-STR T= JRT-NAME$ JRT-STR=
   JSON:READ-NEXT JSON:T-KEY T= s" count" JRT-STR=
   JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 42 T=
   JSON:READ-NEXT JSON:T-KEY T= s" ok" JRT-STR=
   JSON:READ-NEXT JSON:T-TRUE T=
   JSON:READ-NEXT JSON:T-KEY T= s" none" JRT-STR=
   JSON:READ-NEXT JSON:T-NULL T=
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-COPY$ ( -- ptr u8 n )
   s\" {\"a\":1,\"b\":[2,3]}" ;

: JRT-COPY ( n -- ptr u8 n ) {: indent:n :}
   JRT-COPY$ JRT-OPEN-A JSON:READ-NEXT drop
   JRT-WRITER JRT-WRITE-BUF JRT-CAP JSON:WRITE-OPEN indent JSON:WRITE-INDENT!
   JRT-BUF JRT-CAP JSON:COPY-VALUE
   JSON:WRITTEN$ {: a:ptr u:n :}
   JSON:READ-CLOSE a u ;

\ COPY-VALUE lays the value out through the writer's tokens, as any writing.
: JRT-TEST-COPY-LAYOUT ( -- )
   2 JRT-COPY s\" {\n  \"a\": 1,\n  \"b\": [\n    2,\n    3\n  ]\n}" T$=
   0 JRT-COPY JRT-COPY$ T$= ;

: JRT-TEST-FIND-HIT ( -- )
   JRT-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   s" a" JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T=
   JSON:READ-INT 1 T=
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-SKIP ( -- )
   JRT-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   s" b" JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-OBJ T=
   JSON:READ-SKIP-VALUE
   s" c" JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T=
   JSON:READ-INT 3 T=
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-MISS ( -- )
   JRT-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   s" zzz" JSON:READ-FIND-KEY TFALSE
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-NESTED-SAME-NAME ( -- )
   JRT-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   s" x" JSON:READ-FIND-KEY TFALSE
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-LONG ( n -- ) {: len:n :}
   len JRT-LONG-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JRT-LONG-KEY len JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T=
   JSON:READ-INT 7 T=
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-LENGTHS ( -- )
   256 JRT-TEST-FIND-LONG
   257 JRT-TEST-FIND-LONG
   JRT-LONG-KEY-CAP JRT-TEST-FIND-LONG ;

: JRT-TEST-FIND-ESCAPED ( -- )
   JRT-FIND-ESC-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JRT-FIND-ESC-WANT$ JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T=
   JSON:READ-INT 9 T=
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-ANCHOR ( -- )
   JRT-ANCHOR$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   s" nested" JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-OBJ T=
   s" outer" JSON:READ-FIND-KEY TFALSE
   JSON:READ-TOKEN JSON:T-OBJ-END T=
   s" outer" JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T=
   JSON:READ-INT 2 T=
   JSON:READ-CLOSE ;

: JRT-BAD-FIND-ARRAY ( -- )
   S\" [{\qx\q:1}]" JRT-OPEN-A
   JSON:READ-NEXT drop s" x" JSON:READ-FIND-KEY drop JSON:READ-CLOSE ;

: JRT-BAD-FIND-SCALAR ( -- )
   s" 1" JRT-OPEN-A
   JSON:READ-NEXT drop s" x" JSON:READ-FIND-KEY drop JSON:READ-CLOSE ;

: JRT-BAD-FIND-PHASE ( -- )
   JRT-FIND$ JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop
   s" a" JSON:READ-FIND-KEY drop JSON:READ-CLOSE ;

: JRT-BAD-FIND-KEY-LEN ( -- )
   s" {}" JRT-OPEN-A
   JSON:READ-NEXT drop JRT-ZERO-U8 -1 JSON:READ-FIND-KEY drop JSON:READ-CLOSE ;

: JRT-BAD-FIND-KEY-PTR ( -- )
   s" {}" JRT-OPEN-A
   JSON:READ-NEXT drop JRT-ZERO-U8 1 JSON:READ-FIND-KEY drop JSON:READ-CLOSE ;

: JRT-TEST-FIND-MISUSE ( -- )
   [: JRT-BAD-FIND-ARRAY ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-FIND-SCALAR ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-FIND-PHASE ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-FIND-KEY-LEN ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-FIND-KEY-PTR ;] E-JSON-READ-STATE TTHROWSQ ;

: JRT-TEST-REPEATS ( -- )
   s" {}" JSON:READ-REPEATS? TFALSE
   s\" {\"a\":1,\"b\":2}" JSON:READ-REPEATS? TFALSE
   s\" {\"a\":1,\"a\":2}" JSON:READ-REPEATS? TTRUE
   s\" {\"a\":1,\"b\":2,\"\\u0061\":3}" JSON:READ-REPEATS? TTRUE
   s\" {\"ab\":1,\"a\":2}" JSON:READ-REPEATS? TFALSE
   s\" {\"a\":{\"b\":1,\"b\":2}}" JSON:READ-REPEATS? TFALSE
   s\" {\"a\":{\"x\":[1,{\"a\":3}]},\"a\":2}" JSON:READ-REPEATS? TTRUE
   [: s" [1]" JSON:READ-REPEATS? drop ;] E-JSON-READ-STATE TTHROWSQ
   [: s\" {\"a\":1," JSON:READ-REPEATS? drop ;] E-JSON-READ-EOF TTHROWSQ ;

: JRT-TEST-VALUE-EQ-SCALARS ( -- )
   s" 1" s" 1.0" JSON:VALUE= TTRUE
   s" 1" s" 1e0" JSON:VALUE= TTRUE
   s" -0" s" 0" JSON:VALUE= TTRUE
   s" 1" s" 2" JSON:VALUE= TFALSE
   s\" \"a\"" s\" \"\\u0061\"" JSON:VALUE= TTRUE
   s\" \"a\"" s\" \"b\"" JSON:VALUE= TFALSE
   s" 1" s\" \"1\"" JSON:VALUE= TFALSE
   s" null" s"  null " JSON:VALUE= TTRUE
   s" null" s" false" JSON:VALUE= TFALSE
   s" true" s" true" JSON:VALUE= TTRUE
   s" true" s" false" JSON:VALUE= TFALSE ;

: JRT-TEST-VALUE-EQ-ARRAYS ( -- )
   s" []" s" [ ]" JSON:VALUE= TTRUE
   s" []" s" {}" JSON:VALUE= TFALSE
   s" [1,2]" s" [1.0, 2]" JSON:VALUE= TTRUE
   s" [1,2]" s" [2,1]" JSON:VALUE= TFALSE
   s" [1,2]" s" [1,2,3]" JSON:VALUE= TFALSE
   s" [1,2,3]" s" [1,2]" JSON:VALUE= TFALSE ;

: JRT-TEST-VALUE-EQ-OBJECTS ( -- )
   s\" {\"a\":1,\"b\":[true,null]}" s\"  { \"b\" : [ true , null ] , \"a\" : 1 } "
   JSON:VALUE= TTRUE
   s" {}" s" { }" JSON:VALUE= TTRUE
   s\" {\"\\u0061\":1}" s\" {\"a\":1}" JSON:VALUE= TTRUE
   s\" {\"a\":[1,{\"b\":{\"c\":\"d\",\"e\":2}}]}"
   s\" {\"a\":[1.0,{\"b\":{\"e\":2,\"c\":\"\\u0064\"}}]}" JSON:VALUE= TTRUE
   s\" {\"a\":[1,{\"b\":{\"c\":\"d\",\"e\":2}}]}"
   s\" {\"a\":[1,{\"b\":{\"c\":\"d\",\"e\":3}}]}" JSON:VALUE= TFALSE
   s\" {\"a\":1,\"b\":2}" s\" {\"a\":1}" JSON:VALUE= TFALSE
   s\" {\"a\":1}" s\" {\"a\":1,\"b\":2}" JSON:VALUE= TFALSE
   s\" {\"a\":1,\"b\":2}" s\" {\"a\":1,\"c\":2}" JSON:VALUE= TFALSE
   s\" [{\"a\":1,\"b\":2},{\"c\":3,\"d\":4}]" s\" [{\"b\":2,\"a\":1},{\"d\":4,\"c\":3}]"
   JSON:VALUE= TTRUE
   s\" [{\"a\":1,\"b\":2},{\"c\":3,\"d\":4}]" s\" [{\"b\":2,\"a\":1},{\"d\":5,\"c\":3}]"
   JSON:VALUE= TFALSE
   s\" {\"x\":{\"k\":1},\"y\":{\"j\":2}}" s\" {\"y\":{\"k\":1},\"x\":{\"j\":2}}" JSON:VALUE= TFALSE
   s\" {\"x\":{\"k\":1},\"y\":{\"k\":2}}" s\" {\"y\":{\"k\":2},\"x\":{\"k\":1}}" JSON:VALUE= TTRUE
   s\" {\"a\":1,\"ab\":2,\"b\":3,\"c\":4,\"d\":5,\"e\":6,\"f\":7,\"g\":8}"
   s\" {\"g\":8,\"f\":7,\"e\":6,\"d\":5,\"c\":4,\"b\":3,\"ab\":2,\"a\":1}" JSON:VALUE= TTRUE
   s\" {\"a\":{\"m\":1},\"z\":2,\"zz\":3}" s\" {\"zz\":3,\"z\":2,\"a\":{\"m\":1}}"
   JSON:VALUE= TTRUE ;

\ A repeated key answers false on either side, in identical texts and when its
\ copies are spelled differently.
: JRT-TEST-VALUE-EQ-REPEATS ( -- )
   s\" {\"a\":1,\"a\":1}" s\" {\"a\":1,\"b\":1}" JSON:VALUE= TFALSE
   s\" {\"a\":1,\"b\":1}" s\" {\"a\":1,\"a\":1}" JSON:VALUE= TFALSE
   s\" {\"a\":1,\"a\":1}" s\" {\"a\":1,\"a\":1}" JSON:VALUE= TFALSE
   s\" [{\"a\":1,\"\\u0061\":1}]" s\" [{\"a\":1,\"a\":1}]" JSON:VALUE= TFALSE
   s\" {\"k\":1,\"k\":1,\"j\":2}" s\" {\"k\":1,\"j\":2,\"m\":3}" JSON:VALUE= TFALSE
   s\" {\"k\":1,\"j\":2}" s\" {\"k\":1,\"k\":1,\"j\":2}" JSON:VALUE= TFALSE ;

\ Both texts are read whole, so a malformed one throws its code whatever the
\ comparison would answer.
: JRT-TEST-VALUE-EQ-MALFORMED ( -- )
   [: s\" {\"a\":1," s" {}" JSON:VALUE= drop ;] E-JSON-READ-EOF TTHROWSQ
   [: s" 1 2" s" 1" JSON:VALUE= drop ;] E-JSON-READ-TRAILING TTHROWSQ
   [: s" 1" s\" \"x\" x" JSON:VALUE= drop ;] E-JSON-READ-TRAILING TTHROWSQ
   [: s" [1,2" s" [2]" JSON:VALUE= drop ;] E-JSON-READ-EOF TTHROWSQ
   [: s" [2]" s" [1,2" JSON:VALUE= drop ;] E-JSON-READ-EOF TTHROWSQ ;

\ Numbers compare by value on their lexemes, past STR>FLOAT's digit and exponent
\ limits; an exponent past a signed cell, stated or scaled, throws.
: JRT-TEST-VALUE-EQ-NUMBERS ( -- )
   s" 0.0017453292519943296" s" 1.7453292519943296e-3" JSON:VALUE= TTRUE
   s" 1e400" s" 10E399" JSON:VALUE= TTRUE
   s" 100" s" 1e2" JSON:VALUE= TTRUE
   s" 12" s" 1.2e1" JSON:VALUE= TTRUE
   s" 0.01" s" 1e-2" JSON:VALUE= TTRUE
   s" -0.0e5" s" 0" JSON:VALUE= TTRUE
   s" 0.1" s" 0.10000000000000001" JSON:VALUE= TFALSE
   s" 1" s" -1" JSON:VALUE= TFALSE
   s" 1e2" s" 1e3" JSON:VALUE= TFALSE
   s" 10" s" 1e0000000000000000000000001" JSON:VALUE= TTRUE
   [: s" 1e9223372036854775807" s" 1" JSON:VALUE= drop ;] E-JSON-READ-NUMBER TTHROWSQ
   [: s" 1e99999999999999999999" s" 1" JSON:VALUE= drop ;] E-JSON-READ-NUMBER TTHROWSQ ;

\ 100000 arrays deep, past what recursion on the return stack survives,
\ compare equal to themselves and unequal with the innermost digit changed.
: JRT-TEST-VALUE-EQ-DEEP ( -- )
   0 100000 JRT-DIGIT-1 JRT-DEEP-ONE$ 2dup JSON:VALUE= TTRUE
   0 100000 JRT-DIGIT-1 JRT-DEEP-ONE$
   200001 100000 JRT-DIGIT-2 JRT-DEEP-ONE$ JSON:VALUE= TFALSE ;

\ The least of five VALUE= calls on each pair of equal texts, in nanoseconds; the
\ pairs alternate, so a move between core kinds reaches both sizes alike.
: JRT-EQ-LEASTS ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- n n )
   {: a:ptr u:n b:ptr v:n c:ptr x:n d:ptr y:n :}
   MEM-MAX-N MEM-MAX-N  5 0 ?do
      swap  TIME:MONO-NS  a u b v JSON:VALUE= TTRUE  TIME:MONO-NS swap -  min
      swap  TIME:MONO-NS  c x d y JSON:VALUE= TTRUE  TIME:MONO-NS swap -  min
   loop ;

\ Objects 4000 and 8000 deep against copies: the bound is 3 because a walk n log n in the bytes
\ grows under 2.2 times, and one that rereads each enclosing object for every level quadruples it.
: JRT-TEST-VALUE-EQ-DEPTH-TIME ( -- )
   200000 4000 JRT-NEST-AT$  300000 4000 JRT-NEST-AT$
   0 8000 JRT-NEST-AT$  100000 8000 JRT-NEST-AT$  JRT-EQ-LEASTS {: t4:n t8:n :}
   t8  t4 3 *  < TTRUE ;

\ One object of 4000 and of 8000 members against a reversed copy: the bound is 3 because a walk
\ n log n in the members grows under 2.2 times; one that rescans b's object per key quadruples it.
: JRT-TEST-VALUE-EQ-MEMBERS-TIME ( -- )
   200000 4000 false JRT-FLAT-AT$  300000 4000 true JRT-FLAT-AT$
   0 8000 false JRT-FLAT-AT$  100000 8000 true JRT-FLAT-AT$  JRT-EQ-LEASTS {: t4:n t8:n :}
   t8  t4 3 *  < TTRUE ;

\ Whether a one-string document decodes to the given bytes; the cursor must
\ still be on the string, so the document ends at the next token.
: JRT-EQ? ( ptr u8 n ptr u8 n -- bool )
   {: src:ptr su:n want:ptr wu:n :}
   src su JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   want wu JSON:READ-STR-EQ? {: same:bool :}
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE
   same ;

: JRT-TEST-STR-EQ ( -- )
   s\" \"\"" s" " JRT-EQ? TTRUE
   s\" \"\"" s" a" JRT-EQ? TFALSE
   s\" \"a\"" s" " JRT-EQ? TFALSE
   s\" \"initialize\"" s" initialize" JRT-EQ? TTRUE
   s\" \"\\u0069nit\\u0069alize\"" s" initialize" JRT-EQ? TTRUE
   JRT-SUR-SRC$ JRT-SUR-WANT$ JRT-EQ? TTRUE
   JRT-ESC-SRC$ JRT-ESC-WANT$ JRT-EQ? TTRUE
   s\" \"initialize\"" s" init" JRT-EQ? TFALSE
   s\" \"init\"" s" initialize" JRT-EQ? TFALSE
   s\" \"\\u0069nit\"" s" initialize" JRT-EQ? TFALSE
   s\" \"initializf\"" s" initialize" JRT-EQ? TFALSE
   s\" \"\\u0041\"" s" B" JRT-EQ? TFALSE
   s\" \"\\n\"" s\" \t" JRT-EQ? TFALSE
   s\" \"a\\nb\"" s\" a\\nb" JRT-EQ? TFALSE ;

\ A key compares the same way, and a repeated comparison sees the same key.
: JRT-TEST-STR-EQ-KEY ( -- )
   s\" {\"m\\u0065thod\":\"x\"}" JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-NEXT JSON:T-KEY T=
   s" method" JSON:READ-STR-EQ? TTRUE
   s" meth" JSON:READ-STR-EQ? TFALSE
   s" method" JSON:READ-STR-EQ? TTRUE
   JSON:READ-NEXT JSON:T-STR T=
   s" x" JSON:READ-STR-EQ? TTRUE
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-BAD-EQ-KIND ( -- )
   s" 1" JRT-OPEN-A
   JSON:READ-NEXT drop s" 1" JSON:READ-STR-EQ? drop JSON:READ-CLOSE ;

: JRT-BAD-EQ-LEN ( -- )
   JRT-HI-SRC$ JRT-OPEN-A
   JSON:READ-NEXT drop JRT-ZERO-U8 -1 JSON:READ-STR-EQ? drop JSON:READ-CLOSE ;

: JRT-BAD-EQ-PTR ( -- )
   JRT-HI-SRC$ JRT-OPEN-A
   JSON:READ-NEXT drop JRT-ZERO-U8 2 JSON:READ-STR-EQ? drop JSON:READ-CLOSE ;

: JRT-TEST-STR-EQ-MISUSE ( -- )
   [: JRT-BAD-EQ-KIND ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-EQ-LEN ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-EQ-PTR ;] E-JSON-READ-STATE TTHROWSQ ;

: JRT-TEST-INTERLEAVE ( -- )
   s" [1,3]" JRT-OPEN-A
   S\" {\qx\q:\qtwo\q}" JRT-OPEN-B
   JSON:READ-NEXT JSON:T-OBJ T=
   >r JSON:READ-NEXT JSON:T-ARR T= r>
   JSON:READ-NEXT JSON:T-KEY T=
   JSON:READ-SPAN$ s" x" T$=
   JSON:READ-NEXT JSON:T-STR T=
   s" two" JRT-STR=
   >r JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 1 T= r>
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE
   JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 3 T=
   JSON:READ-NEXT JSON:T-ARR-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-BAD-ADVANCE ( JSON:reader -- JSON:reader )
   JSON:READ-NEXT drop ;

\ The reader must survive the caught failure: `catch` restores the depth of both
\ stacks, never their contents, so a quotation literal leaves it `stale<JSON:reader>`
\ and a linear cell can be neither read nor dropped. The caught body is a name
\ reached by `[']` until the checker can prove the handle intact.
: JRT-CATCH-BAD ( JSON:reader -- JSON:reader )
   ['] JRT-BAD-ADVANCE catch E-JSON-READ-MALFORMED T= ;

: JRT-NESTED-CATCH-BAD ( JSON:reader -- JSON:reader )
   ['] JRT-CATCH-BAD catch 0 T= ;

: JRT-TEST-CATCH-ISOLATION ( -- )
   s" [7,8]" JRT-OPEN-A
   s" [1,]" JRT-OPEN-B
   JSON:READ-NEXT JSON:T-ARR T=
   JSON:READ-NEXT JSON:T-INT T=
   JRT-NESTED-CATCH-BAD
   JSON:READ-CLOSE
   JSON:READ-NEXT JSON:T-ARR T=
   JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 7 T=
   JSON:READ-NEXT JSON:T-INT T= JSON:READ-INT 8 T=
   JSON:READ-NEXT JSON:T-ARR-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-DEEP-CASE ( n -- ) {: depth:n :}
   depth JRT-DEEP$ JRT-OPEN-A
   depth JRT-DRAIN-ARR
   JSON:READ-CLOSE ;

\ A reader's first stack mapping is 4096 bytes, and READ-REPEATS? holds two
\ mappings at once of at most that size here: its scratch and its record's
\ stack. Two probes freed and mapped again land at the same addresses only when
\ nothing took the holes, so a body that leaves a mapping owned moves a probe
\ elsewhere.
\ This relies on the kernel's first-fit reuse of a freed mapping, as
\ src/lib/byte-buffer-test.f BUFT-DISPOSE-RELEASES does.
4096 constant JRT-MORE-BYTES

: JRT-PROBE ( -- SPAN:span<u8> )
   JRT-MORE-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN ;

: JRT-RELEASES ( [ -- ] -- )
   JRT-PROBE JRT-PROBE {: h1 h2 :}
   h1 SPAN:$ drop {: a1:ptr :}
   h2 SPAN:$ drop {: a2:ptr :}
   h1 MEM:FREE-SPAN h2 MEM:FREE-SPAN
   execute
   JRT-PROBE JRT-PROBE {: g1 g2 :}
   g1 SPAN:$ drop a1 = TTRUE
   g2 SPAN:$ drop a2 = TTRUE
   g1 MEM:FREE-SPAN g2 MEM:FREE-SPAN ;

\ Each case checks its own release, so a READ-CLOSE that leaks fails the case
\ that leaked rather than hiding in a mapping a later case reuses.
: JRT-DEEP-EDGES ( -- )
   64 JRT-DEEP-CASE
   65 JRT-DEEP-CASE
   128 JRT-DEEP-CASE
   129 JRT-DEEP-CASE ;

: JRT-DEEP-10000 ( -- )
   10000 JRT-DEEP-CASE ;

\ 40000 levels fill the inline stack word and 624 mapped ones, past the first
\ mapping's 512 cells, so the stack doubles once and frees the first mapping.
: JRT-DEEP-40000 ( -- )
   40000 JRT-DEEP-CASE ;

\ READ-FIND-KEY anchors on and skips inside an object 70 deep, where its level
\ and the separator before "y" are read from the grown stack.
: JRT-TEST-DEEP-FIND ( -- )
   70 JRT-NEST-OBJ$ JRT-OPEN-A
   69 0 ?do JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-NEXT JSON:T-KEY T= loop
   JSON:READ-NEXT JSON:T-OBJ T=
   s" y" JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T= JSON:READ-INT 7 T=
   JSON:READ-FINISH ;

\ The failure at depth 100 releases the grown stack; the next advance throws
\ the same code without reading it.
: JRT-DEEP-MALFORMED ( -- )
   100 JRT-DEEP-BAD$ JRT-OPEN-A
   100 0 ?do JSON:READ-NEXT JSON:T-ARR T= loop
   JRT-CATCH-BAD
   JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-DEEP-MALFORMED-THROWS ( -- )
   [: JRT-DEEP-MALFORMED ;] E-JSON-READ-MALFORMED TTHROWSQ ;

77 constant JRT-CALLER-CODE

: JRT-CALLER-FAIL ( JSON:reader -- JSON:reader )
   JRT-TRUE if JRT-CALLER-CODE throw then ;

\ The caller's own throw abandons a live reader 100 deep, so record A keeps
\ its grown stack.
: JRT-ABANDON ( -- )
   100 JRT-OPENS$ JRT-OPEN-A
   100 0 ?do JSON:READ-NEXT JSON:T-ARR T= loop
   JRT-CALLER-FAIL
   JSON:READ-CLOSE ;

: JRT-ABANDON-THROWS ( -- )
   [: JRT-ABANDON ;] JRT-CALLER-CODE TTHROWSQ ;

: JRT-ABANDON-DISPOSE ( -- )
   JRT-ABANDON-THROWS
   JRT-A JSON:READ-DISPOSE
   JRT-A JSON:READ-DISPOSE ;

\ The caller throws after a decoded escape, so record A holds its source pointer
\ and its destination registers.
: JRT-ESCAPED-ABANDON ( -- )
   JRT-ESC-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-ESC-WANT$ JRT-STR=
   JRT-CALLER-FAIL
   JSON:READ-CLOSE ;

CAST: >RAW ( ptr JSON:cursor -- ptr u8 )

\ JSON's OPAQUE cursor keeps CURSOR-BYTES private; a one-field layout spans it.
STRUCTURE held DERIVE addr FIELD record JSON:cursor ;STRUCTURE

: JRT-NONZERO-BYTES ( -- n )
   JRT-A >RAW {: raw:ptr :}
   0 HELD-BYTES 0 ?do raw i + c@ 0<> if 1+ then loop ;

: JRT-DISPOSE-ZEROES ( -- )
   [: JRT-ESCAPED-ABANDON ;] JRT-CALLER-CODE TTHROWSQ
   JRT-A JSON:READ-DISPOSE
   JRT-NONZERO-BYTES 0 T= ;

: JRT-ABANDON-REINIT ( -- )
   JRT-ABANDON-THROWS
   65 JRT-DEEP-CASE ;

\ {"b": depth arrays, then "a" twice: the first record grows its stack
\ skipping b's value and the second finding the first "a" past it.
: JRT-DEEP-REPEAT$ ( n -- ptr u8 n ) {: depth:n :}
   SB-RESET
   JRT-LBRACE SB-APPEND-C s" b" JRT-QKEY
   depth 0 ?do JRT-LBRACK SB-APPEND-C loop
   depth 0 ?do JRT-RBRACK SB-APPEND-C loop
   JRT-COMMA SB-APPEND-C s" a" JRT-QKEY s" 1" SB-APPEND
   JRT-COMMA SB-APPEND-C s" a" JRT-QKEY s" 2" SB-APPEND
   JRT-RBRACE SB-APPEND-C
   SB$ ;

\ The truncated copy throws after the repeat, with the second stack held.
: JRT-DEEP-REPEATS ( -- )
   70 JRT-DEEP-REPEAT$ JSON:READ-REPEATS? TTRUE
   [: 70 JRT-DEEP-REPEAT$ 1- JSON:READ-REPEATS? drop ;] E-JSON-READ-EOF TTHROWSQ ;

\ 70 objects deep, both texts' records grow stacks; a truncated a throws while b's
\ record holds its stack.
: JRT-DEEP-VALUE-EQ ( -- )
   70 JRT-NEST-OBJ$ 2dup JSON:VALUE= TTRUE
   [: 70 JRT-NEST-OBJ$ 1-  70 JRT-NEST-OBJ$ JSON:VALUE= drop ;] E-JSON-READ-EOF TTHROWSQ ;

: JRT-TEST-DEEP ( -- )
   s" nesting at and past the inline stack parses" T-LABEL
   [: JRT-DEEP-EDGES ;] JRT-RELEASES
   s" 10000 levels parse and READ-CLOSE releases the stack" T-LABEL
   [: JRT-DEEP-10000 ;] JRT-RELEASES
   s" 40000 levels double the stack and READ-CLOSE releases it" T-LABEL
   [: JRT-DEEP-40000 ;] JRT-RELEASES
   s" READ-FIND-KEY works 70 objects deep and READ-FINISH releases the stack" T-LABEL
   [: JRT-TEST-DEEP-FIND ;] JRT-RELEASES
   s" a failure 100 deep releases the stack" T-LABEL
   [: JRT-DEEP-MALFORMED-THROWS ;] JRT-RELEASES
   s" READ-DISPOSE releases what a caller's throw abandoned" T-LABEL
   [: JRT-ABANDON-DISPOSE ;] JRT-RELEASES
   s" DISPOSE leaves the record zero-filled" T-LABEL
   [: JRT-DISPOSE-ZEROES ;] JRT-RELEASES
   s" a record holding a leftover parses again and nothing stays mapped" T-LABEL
   [: JRT-ABANDON-REINIT ;] JRT-RELEASES
   s" READ-REPEATS? past 64 levels releases both stacks, a throw included" T-LABEL
   [: JRT-DEEP-REPEATS ;] JRT-RELEASES
   s" VALUE= past 64 levels releases every record's stack, a throw included" T-LABEL
   [: JRT-DEEP-VALUE-EQ ;] JRT-RELEASES ;

: JRT-BAD-NEGATIVE-SOURCE ( -- )
   JRT-A s" 0" drop -1 JSON:READ-INIT JSON:READ-CLOSE ;

: JRT-BAD-NULL-SOURCE ( -- )
   JRT-A JRT-ZERO-U8 1 JSON:READ-INIT JSON:READ-CLOSE ;

: JRT-TEST-SEALED ( -- )
   s" package JSON : FORGE ( ptr n -- JSON:reader ) MINT-READER ; ;package"
   {: src:ptr srcu:n :}
   src srcu JRT-SUBJECT-OUT $400 >LEN JRT-SUBJECT-ERR $400 >LEN 1000 >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu JRT-SUBJECT-OUT outu LEN>N JRT-SUBJECT-ERR erru LEN>N
   oc ENGINE-ERROR:SEAL-PACKAGE T-ENDING-EXITED= ;

: JRT-TEST-PRIVATE-STATE ( -- )
   s" JRT-BAD-MINT ( ptr n -- JSON:reader ) JSON:MINT-READER" JRT-REJECTED
   s" JRT-BAD-ERASE ( JSON:reader -- ptr n ) JSON:ERASE-READER" JRT-REJECTED
   s" JRT-BAD-PRIVATE-STATE ( JSON:reader -- JSON:reader ptr n ) JSON:READER>STATE" JRT-REJECTED
   s" JRT-BAD-CELLS ( JSON:reader -- JSON:reader ptr n ) JSON:READER>CELLS" JRT-REJECTED ;

: JRT-TEST-OWNERSHIP ( -- )
   s" negative source length is rejected before mint" T-LABEL
   [: JRT-BAD-NEGATIVE-SOURCE ;] JSON:E-READ-SOURCE TTHROWSQ
   s" null nonempty source is rejected before mint" T-LABEL
   [: JRT-BAD-NULL-SOURCE ;] JSON:E-READ-SOURCE TTHROWSQ
   s" null empty source remains a valid empty reader" T-LABEL
   JRT-A JRT-ZERO-U8 0 JSON:READ-INIT JSON:READ-CLOSE
   s" raw representation and state helpers stay private" T-LABEL
   JRT-TEST-PRIVATE-STATE
   s" the package cannot be reopened to reach private representation words" T-LABEL
   JRT-TEST-SEALED
   s" linear reader cannot be duplicated" T-LABEL
   s" JRT-BAD-DUP ( JSON:reader -- JSON:reader JSON:reader ) dup" JRT-REJECTED
   s" linear reader cannot be discarded" T-LABEL
   s" JRT-BAD-DROP ( JSON:reader -- ) drop" JRT-REJECTED
   s" raw storage cannot construct a reader" T-LABEL
   s" JRT-BAD-RAW ( ptr n -- JSON:reader ) JSON:MINT-READER" JRT-REJECTED
   s" reader representation cannot be projected" T-LABEL
   s" JRT-BAD-PROJECT ( JSON:reader -- JSON:reader ptr n ) JSON:READER>STATE" JRT-REJECTED
   s" reader state cannot be rehomed" T-LABEL
   s" JRT-BAD-REHOME ( JSON:reader ptr n ptr u8 -- JSON:reader ) 2>r JSON-READER:UNMAKE 2drop 2r> JSON-READER:MAKE" JRT-REJECTED
   s" reader cannot be closed twice" T-LABEL
   s" JRT-BAD-CLOSE ( JSON:reader -- ) JSON:READ-CLOSE JSON:READ-CLOSE" JRT-REJECTED ;

: JRT-BAD-TRAILING ( -- )
   s" 1 2" JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-UNTERM ( -- )
   JRT-UNTERM-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-ESCAPE ( -- )
   JRT-BADESC-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-RUN-BS ( -- )
   JRT-RUN-BS-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-RUN-CTRL ( -- )
   JRT-RUN-CTRL-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-SURROGATE ( -- )
   JRT-LONE-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-HEX ( -- )
   JRT-BADHEX-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-LOW-SURROGATE ( -- )
   JRT-LOW-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-SURROGATE-PAIR ( -- )
   JRT-BADPAIR-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-UTF8-OVERLONG ( -- )
   JRT-UTF8-OVERLONG$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-UTF8-STRAY ( -- )
   JRT-UTF8-STRAY$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-UTF8-TRUNC ( -- )
   JRT-UTF8-TRUNC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-UTF8-SUR ( -- )
   JRT-UTF8-SUR$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-UTF8-HIGH ( -- )
   JRT-UTF8-HIGH$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-BAREWORD ( -- )
   s" nul" JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-TRAILING-COMMA ( -- )
   s" [1,]" JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-COLON ( -- )
   SB-RESET
   JRT-LBRACE SB-APPEND-C JRT-DQ SB-APPEND-C s" a" SB-APPEND JRT-DQ SB-APPEND-C
   JRT-SP SB-APPEND-C s" 1" SB-APPEND JRT-RBRACE SB-APPEND-C
   SB$ JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-CLOSE ;

: JRT-BAD-OVERFLOW ( -- )
   s" 999999999999999999999999" JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-INT drop JSON:READ-CLOSE ;

: JRT-BAD-STATE ( -- )
   JRT-HI-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-INT drop JSON:READ-CLOSE ;


: JRT-BAD-STR-NULL ( -- )
   JRT-HI-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JRT-ZERO-U8 2 JSON:READ-STR drop JSON:READ-CLOSE ;

: JRT-BAD-STR-NEGATIVE ( -- )
   JRT-HI-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JRT-BUF -1 JSON:READ-STR drop JSON:READ-CLOSE ;

: JRT-BAD-STR-ZERO ( -- )
   JRT-HI-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JRT-BUF 0 JSON:READ-STR drop JSON:READ-CLOSE ;

: JRT-BAD-STR-SHORT ( -- )
   JRT-HI-SRC$ JRT-OPEN-A JSON:READ-NEXT drop JRT-BUF 1 JSON:READ-STR drop JSON:READ-CLOSE ;

: JRT-TEST-STR-EXACT ( -- )
   JRT-HI-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-BUF 2 JSON:READ-STR 2 T=
   JRT-BUF 2 s" hi" T$=
   JSON:READ-CLOSE ;

: JRT-TEST-NEGATIVE ( -- )
   [: JRT-BAD-TRAILING ;] E-JSON-READ-TRAILING TTHROWSQ
   [: JRT-BAD-UNTERM ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-ESCAPE ;] E-JSON-READ-ESCAPE TTHROWSQ
   [: JRT-BAD-RUN-BS ;] E-JSON-READ-ESCAPE TTHROWSQ
   [: JRT-BAD-RUN-CTRL ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-HEX ;] E-JSON-READ-ESCAPE TTHROWSQ
   [: JRT-BAD-SURROGATE ;] E-JSON-READ-SURROGATE TTHROWSQ
   [: JRT-BAD-LOW-SURROGATE ;] E-JSON-READ-SURROGATE TTHROWSQ
   [: JRT-BAD-SURROGATE-PAIR ;] E-JSON-READ-SURROGATE TTHROWSQ
   [: JRT-BAD-UTF8-OVERLONG ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-UTF8-STRAY ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-UTF8-TRUNC ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-UTF8-SUR ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-UTF8-HIGH ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-BAREWORD ;] E-JSON-READ-MALFORMED TTHROWSQ
   [: JRT-BAD-TRAILING-COMMA ;] E-JSON-READ-MALFORMED TTHROWSQ
   [: JRT-BAD-COLON ;] E-JSON-READ-COLON TTHROWSQ
   [: JRT-BAD-OVERFLOW ;] E-JSON-READ-NUMBER TTHROWSQ
   [: JRT-BAD-STATE ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-STR-NULL ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-STR-NEGATIVE ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-STR-ZERO ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-STR-SHORT ;] E-JSON-READ-STATE TTHROWSQ ;

: JRT-TRAILING-TEXT$ ( -- ptr u8 n )
   s\" {\"a\":1} x" ;

: JRT-OBJ-COMMA$ ( -- ptr u8 n )
   s\" {\"a\":1,\"b\":2,}" ;

\ Finishes inside the first member's array, two containers deep.
: JRT-FINISH-NESTED ( ptr u8 n -- )
   JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-NEXT drop
   JSON:READ-FINISH ;

: JRT-CATCH-COLON ( JSON:reader -- JSON:reader )
   ['] JRT-BAD-ADVANCE catch E-JSON-READ-COLON T= ;

\ A failed advance leaves the previous token whole: its string or number still
\ reads, never the bytes the failed scan had reached.
: JRT-CATCH-ESCAPE ( JSON:reader -- JSON:reader )
   ['] JRT-BAD-ADVANCE catch E-JSON-READ-ESCAPE T= ;

: JRT-CATCH-NUMBER ( JSON:reader -- JSON:reader )
   ['] JRT-BAD-ADVANCE catch E-JSON-READ-NUMBER T= ;

: JRT-TEST-AFTER-FAILURE ( -- )
   s\" [\"abcdef\",\"x\\q\"]" JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT JSON:T-STR T=
   JRT-CATCH-ESCAPE
   JSON:READ-TOKEN JSON:T-STR T= s" abcdef" JRT-STR=
   s" abcdef" JSON:READ-STR-EQ? TTRUE
   JSON:READ-CLOSE
   s\" {\"a\":\"bc\",\"d\"}" JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-NEXT JSON:T-STR T=
   JRT-CATCH-COLON
   JSON:READ-TOKEN JSON:T-STR T= s" bc" JRT-STR=
   JSON:READ-CLOSE
   s" [12,3.]" JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT JSON:T-INT T=
   JRT-CATCH-NUMBER
   JSON:READ-TOKEN JSON:T-INT T= JSON:READ-INT 12 T=
   JSON:READ-CLOSE ;

\ The caught advance moved the cursor past the key that lacks its colon; the
\ reader stays failed, so READ-FINISH throws that code instead of closing the
\ object.
: JRT-FINISH-AFTER-FAILURE ( -- )
   s\" {\"a\"}" JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JRT-CATCH-COLON
   JSON:READ-FINISH ;

: JRT-FINISH-AFTER-OPEN ( ptr u8 n -- )
   JRT-OPEN-A JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-FINISH ;

: JRT-FINISH-AFTER-INT ( ptr u8 n -- )
   JRT-OPEN-A JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-NEXT JSON:T-KEY T= JSON:READ-NEXT JSON:T-INT T=
   JSON:READ-FINISH ;

: JRT-TEST-FINISH ( -- )
   JRT-NESTED$ JRT-OPEN-A JSON:READ-FINISH
   JRT-NESTED$ JRT-FINISH-NESTED
   JRT-NESTED$ JRT-OPEN-A JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-SKIP-VALUE JSON:READ-FINISH
   JRT-NESTED$ JRT-OPEN-A JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-SKIP-VALUE
   JSON:READ-NEXT JSON:T-END T= JSON:READ-FINISH
   [: JRT-NESTED$ 1- JRT-FINISH-NESTED ;] E-JSON-READ-EOF TTHROWSQ
   [: JRT-TRAILING-TEXT$ JRT-FINISH-AFTER-OPEN ;] E-JSON-READ-TRAILING TTHROWSQ
   [: JRT-OBJ-COMMA$ JRT-FINISH-AFTER-INT ;] E-JSON-READ-MALFORMED TTHROWSQ
   [: JRT-FINISH-AFTER-FAILURE ;] E-JSON-READ-COLON TTHROWSQ ;

\ A prefix copied so its last byte is the last readable one, read with
\ READ-NEXT, and READ-STR on each string, to T-END or a refusal: its code, 0 at
\ T-END. A scan that reads a byte before it bounds the source faults here
\ instead of refusing.
: JRT-STEP-STR ( JSON:reader -- JSON:reader )
   JSON:READ-NEXT {: k:n :}
   k JSON:T-STR = k JSON:T-KEY = or if JRT-BUF JRT-CAP JSON:READ-STR drop then ;

: JRT-GUARDED-CODE ( ptr u8 n -- n ) {: s:ptr u:n :}
   u 0 GUARD-PAGE:TAIL {: g:ptr :}
   u 0 ?do s i + c@ g i + c! loop
   g u JRT-OPEN-A
   begin
      ['] JRT-STEP-STR catch {: code:n :}
      code 0<> if JSON:READ-CLOSE code exit then
      JSON:READ-TOKEN JSON:T-END =
   until
   JSON:READ-CLOSE 0 ;

\ The code a refusal letter names: E EOF, S STRING, C COLON, N NUMBER,
\ M MALFORMED, X ESCAPE, U SURROGATE, and . for T-END.
: JRT-REFUSAL ( n -- n ) {: c:n :}
   c [char] . = if 0 exit then
   c [char] E = if E-JSON-READ-EOF exit then
   c [char] S = if E-JSON-READ-STRING exit then
   c [char] C = if E-JSON-READ-COLON exit then
   c [char] N = if E-JSON-READ-NUMBER exit then
   c [char] M = if E-JSON-READ-MALFORMED exit then
   c [char] X = if E-JSON-READ-ESCAPE exit then
   c [char] U = if E-JSON-READ-SURROGATE exit then
   s" json-read-test: unknown refusal letter" T-EX-FAIL die ;

\ Every prefix of a document, lengths 0 through u, against one refusal letter
\ per length.
: JRT-PREFIXES ( ptr u8 n ptr u8 n -- ) {: s:ptr u:n c:ptr cu:n :}
   cu u 1+ T=
   cu u 1+ min 0 ?do s i JRT-GUARDED-CODE c i + c@ JRT-REFUSAL T= loop ;

: JRT-TEST-PREFIXES ( -- )
   s\" {\"k\":\"v\",\"n\":[1,-2.5e+3,true,false,null],\"o\":{}}"
   s" EESSCESSEESSCEEEENENENNEEMMMEEMMMMEEMMMEEESSCEEE." JRT-PREFIXES
   s\" [\"a\\\"b\\\\c\\/d\\be\\ff\\ng\\rh\\ti\"]"
   s" EESSXSSXSSXSSXSSXSSXSSXSSXSSE." JRT-PREFIXES
   s\" \"\\u0041\\u00e9\\u20AC\\uD83D\\uDE00x\""
   s" ESXXXXXSXXXXXSXXXXXSXXXXXUUXXXXSS." JRT-PREFIXES
   s\" [\"\xC3\xA9\xE2\x82\xAC\xF0\x9F\x98\x80z\"]"
   s" EESSSSSSSSSSSE." JRT-PREFIXES ;

\ A number ends at the first byte no number holds; a lexeme its grammar stops
\ short of is E-JSON-READ-NUMBER, never the next token's refusal.
: JRT-TEST-LEXEMES ( -- )
   s" 01" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" -01" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" [01]" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" 1-" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" [1+]" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" 1..2" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" 1ee5" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" [1e5e]" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" [0x1]" JRT-GUARDED-CODE E-JSON-READ-COMMA T= ;

: JRT-CORE ( -- )
   JRT-TEST-INTS
   JRT-TEST-FLOATS
   JRT-TEST-LITERALS
   JRT-TEST-STRING
   JRT-TEST-ESCAPES
   JRT-TEST-UESCAPE
   JRT-TEST-SURROGATE
   JRT-TEST-UTF8
   JRT-TEST-STR-EXACT
   JRT-TEST-EMPTY
   JRT-TEST-NESTED
   JRT-TEST-VALUE-SPAN
   JRT-TEST-ANY-SPAN
   JRT-TEST-KIND-NAMES
   JRT-TEST-WS
   JRT-TEST-DEEP
   JRT-TEST-END-SPAN
   JRT-TEST-ROUNDTRIP
   JRT-TEST-COPY-LAYOUT
   JRT-TEST-FIND-HIT
   JRT-TEST-FIND-SKIP
   JRT-TEST-FIND-MISS
   JRT-TEST-FIND-NESTED-SAME-NAME
   JRT-TEST-FIND-LENGTHS
   JRT-TEST-FIND-ESCAPED
   JRT-TEST-FIND-ANCHOR
   JRT-TEST-FIND-MISUSE
   JRT-TEST-REPEATS
   JRT-TEST-VALUE-EQ-SCALARS
   JRT-TEST-VALUE-EQ-ARRAYS
   JRT-TEST-VALUE-EQ-OBJECTS
   JRT-TEST-VALUE-EQ-REPEATS
   JRT-TEST-VALUE-EQ-MALFORMED
   JRT-TEST-VALUE-EQ-NUMBERS
   JRT-TEST-VALUE-EQ-DEEP
   JRT-TEST-VALUE-EQ-DEPTH-TIME
   JRT-TEST-VALUE-EQ-MEMBERS-TIME
   JRT-TEST-STR-EQ
   JRT-TEST-STR-EQ-KEY
   JRT-TEST-STR-EQ-MISUSE
   JRT-TEST-INTERLEAVE
   JRT-TEST-CATCH-ISOLATION
   JRT-TEST-AFTER-FAILURE
   JRT-TEST-OWNERSHIP
   JRT-TEST-NEGATIVE
   JRT-TEST-FINISH
   JRT-TEST-PREFIXES
   JRT-TEST-LEXEMES ;

public

: RUN ( -- )
   JRT-CORE ;

;package

T-RESET
JSON-TEST:RUN
T-REPORT
s" json-read-test: ok" type cr
