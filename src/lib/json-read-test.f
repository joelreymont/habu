\ json-read-test.f - focused tests for the checked JSON pull parser.
\ Run: bin/hb --load src/lib/json-read-test.f

require src/core/exit-status.f
require src/lib/error.f
require src/lib/string.f
require src/lib/float.f
require src/lib/memory.f
require src/lib/test/assert.f
require src/lib/time-cpu.f
require src/lib/json.f
require src/lib/reason.f
require src/lib/fmt.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/test/ending.f
require src/lib/test/guard-page.f
require src/lib/test/mapped.f
require src/lib/test/subject.f
require src/lib/json/read.f
require src/lib/json/write.f
require src/lib/json/value.f
require src/lib/json/copy.f
require src/lib/json/equal.f

package JSON-TEST
private

TYPED-VARIABLE JRT-STR-VEC ^VEC:vector<u8>
TYPED-VARIABLE JRT-APPEND-VEC ^VEC:vector<u8>
TYPED-VARIABLE JRT-A JSON:cursor
TYPED-VARIABLE JRT-B JSON:cursor
create JRT-SUBJECT-OUT $400 allot
create JRT-SUBJECT-ERR $400 allot

\ The writer cases' output, which RUN makes and deletes; src/lib/json/write.f
\ holds none.
TYPED-VARIABLE JRT-WRITE-OUT ^VEC:vector<u8>
TYPED-VARIABLE JRT-WRITER JSON:writer

: JRT-OPEN-A ( str -- JSON:reader )
   {: source:str :}
   JRT-A source JSON:READ-INIT ;

: JRT-OPEN-B ( str -- JSON:reader )
   {: source:str :}
   JRT-B source JSON:READ-INIT ;

\ The null address the reader must reject, taken from the core's declared null
\ slot: a `create`d cell read back through `ptr-field` is an undeclared raw
\ storage cell and the checker rejects the pointer it publishes.
: JRT-ZERO-U8 ( -- ^u8 )
   NULL-PTR ;

: JRT-REJECTED ( str -- )
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
: JRT-ESC-SRC$ ( -- str ) JRT-ESC-SRC 18 >str ;

create JRT-ESC-WANT
   34 c, 92 c, 47 c, 8 c, 12 c, 10 c, 13 c, 9 c,
: JRT-ESC-WANT$ ( -- str ) JRT-ESC-WANT 8 >str ;

create JRT-U-SRC 34 c, 92 c, 117 c, 48 c, 48 c, 52 c, 49 c, 34 c,
: JRT-U-SRC$ ( -- str ) JRT-U-SRC 8 >str ;
create JRT-U-WANT 65 c,
: JRT-U-WANT$ ( -- str ) JRT-U-WANT 1 >str ;

\ "😀"  (U+1F600 grinning face) -> UTF-8 F0 9F 98 80
create JRT-SUR-SRC
   34 c,
   92 c, 117 c, 68 c, 56 c, 51 c, 68 c,
   92 c, 117 c, 68 c, 69 c, 48 c, 48 c,
   34 c,
: JRT-SUR-SRC$ ( -- str ) JRT-SUR-SRC 14 >str ;
create JRT-SUR-WANT $F0 c, $9F c, $98 c, $80 c,
: JRT-SUR-WANT$ ( -- str ) JRT-SUR-WANT 4 >str ;

create JRT-HI-SRC 34 c, 104 c, 105 c, 34 c,          \ "hi"
: JRT-HI-SRC$ ( -- str ) JRT-HI-SRC 4 >str ;

create JRT-LONE-SRC 34 c, 92 c, 117 c, 68 c, 56 c, 48 c, 48 c, 34 c,   \ "\uD800"
: JRT-LONE-SRC$ ( -- str ) JRT-LONE-SRC 8 >str ;
create JRT-BADESC-SRC 34 c, 92 c, 120 c, 34 c,       \ "\x"
: JRT-BADESC-SRC$ ( -- str ) JRT-BADESC-SRC 4 >str ;
create JRT-BADHEX-SRC 34 c, 92 c, 117 c, 48 c, 48 c, 71 c, 48 c, 34 c, \ "\u00G0"
: JRT-BADHEX-SRC$ ( -- str ) JRT-BADHEX-SRC 8 >str ;
create JRT-LOW-SRC 34 c, 92 c, 117 c, 68 c, 67 c, 48 c, 48 c, 34 c,   \ "\uDC00"
: JRT-LOW-SRC$ ( -- str ) JRT-LOW-SRC 8 >str ;
create JRT-BADPAIR-SRC
   34 c, 92 c, 117 c, 68 c, 56 c, 48 c, 48 c,
   92 c, 117 c, 48 c, 48 c, 52 c, 49 c, 34 c,                       \ "\uD800\u0041"
: JRT-BADPAIR-SRC$ ( -- str ) JRT-BADPAIR-SRC 14 >str ;
create JRT-UNTERM-SRC 34 c, 97 c, 98 c, 99 c,        \ "abc
: JRT-UNTERM-SRC$ ( -- str ) JRT-UNTERM-SRC 4 >str ;
\ "ab\n with a stated length of 4: the source ends at the backslash, and the
\ n past that length must not complete the escape.
create JRT-RUN-BS-SRC 34 c, 97 c, 98 c, 92 c, 110 c,
: JRT-RUN-BS-SRC$ ( -- str ) JRT-RUN-BS-SRC 4 >str ;
create JRT-RUN-CTRL-SRC 34 c, 97 c, 10 c, 34 c,      \ "a<LF>": a run, then a raw control byte
: JRT-RUN-CTRL-SRC$ ( -- str ) JRT-RUN-CTRL-SRC 4 >str ;

create JRT-UTF8-SRC
   34 c,
   $C2 c, $80 c,                 \ U+0080
   $E0 c, $A0 c, $80 c,          \ U+0800
   $F4 c, $8F c, $BF c, $BF c,   \ U+10FFFF
   34 c,
: JRT-UTF8-SRC$ ( -- str ) JRT-UTF8-SRC 11 >str ;
: JRT-UTF8-WANT$ ( -- str ) JRT-UTF8-SRC 1+ 9 >str ;

create JRT-UTF8-OVERLONG 34 c, $C0 c, $AF c, 34 c,
: JRT-UTF8-OVERLONG$ ( -- str ) JRT-UTF8-OVERLONG 4 >str ;
create JRT-UTF8-STRAY 34 c, $80 c, 34 c,
: JRT-UTF8-STRAY$ ( -- str ) JRT-UTF8-STRAY 3 >str ;
create JRT-UTF8-TRUNC 34 c, $E2 c, $82 c, 34 c,
: JRT-UTF8-TRUNC$ ( -- str ) JRT-UTF8-TRUNC 4 >str ;
create JRT-UTF8-SUR 34 c, $ED c, $A0 c, $80 c, 34 c,
: JRT-UTF8-SUR$ ( -- str ) JRT-UTF8-SUR 5 >str ;
create JRT-UTF8-HIGH 34 c, $F4 c, $90 c, $80 c, $80 c, 34 c,
: JRT-UTF8-HIGH$ ( -- str ) JRT-UTF8-HIGH 6 >str ;

\ round-trip value: "A\"B" holds bytes A " B
create JRT-NAME 65 c, 34 c, 66 c,
: JRT-NAME$ ( -- str ) JRT-NAME 3 >str ;

\ {"a\u0062\n\"":9} -> key bytes `ab`, newline, quote
create JRT-FIND-ESC-SRC
   123 c, 34 c, 97 c,
   92 c, 117 c, 48 c, 48 c, 54 c, 50 c,
   92 c, 110 c, 92 c, 34 c,
   34 c, 58 c, 57 c, 125 c,
: JRT-FIND-ESC-SRC$ ( -- str ) JRT-FIND-ESC-SRC 17 >str ;
create JRT-FIND-ESC-WANT 97 c, 98 c, 10 c, 34 c,
: JRT-FIND-ESC-WANT$ ( -- str ) JRT-FIND-ESC-WANT 4 >str ;

768 constant JRT-LONG-KEY-CAP
create JRT-LONG-KEY JRT-LONG-KEY-CAP allot

: JRT-STR= ( JSON:reader str -- JSON:reader ) {: e:str :}
   JRT-STR-VEC @ JSON:READ-STR e T$= ;

: JRT-QKEY ( str -- )                       \ "key":  onto the builder
   JRT-DQ SB-APPEND-C SB-APPEND JRT-DQ SB-APPEND-C JRT-COLON SB-APPEND-C ;

: JRT-QSTR ( str -- )                       \ "str"  onto the builder
   JRT-DQ SB-APPEND-C SB-APPEND JRT-DQ SB-APPEND-C ;

: JRT-NESTED$ ( -- str )
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

: JRT-VALUE-SPAN$ ( -- str )
   s\" {\"a\":[1,{\"b\":true}],\"c\":0}" ;

: JRT-FIND$ ( -- str )
   SB-RESET
   JRT-LBRACE SB-APPEND-C
   s" a" JRT-QKEY s" 1" SB-APPEND JRT-COMMA SB-APPEND-C
   s" b" JRT-QKEY
   JRT-LBRACE SB-APPEND-C s" x" JRT-QKEY s" 9" SB-APPEND JRT-RBRACE SB-APPEND-C
   JRT-COMMA SB-APPEND-C
   s" c" JRT-QKEY s" 3" SB-APPEND
   JRT-RBRACE SB-APPEND-C
   SB$ ;

: JRT-ANCHOR$ ( -- str )
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

: JRT-LONG-FIND$ ( n -- str ) {: len:n :}
   len JRT-LONG-KEY!
   SB-RESET
   JRT-LBRACE SB-APPEND-C
   JRT-LONG-KEY len >str JRT-QKEY s" 7" SB-APPEND
   JRT-RBRACE SB-APPEND-C
   SB$ ;

: JRT-WS$ ( -- str )                         \ "  {  \"a\" : 1 }  "
   SB-RESET
   JRT-SP SB-APPEND-C JRT-SP SB-APPEND-C
   JRT-LBRACE SB-APPEND-C JRT-SP SB-APPEND-C JRT-SP SB-APPEND-C
   JRT-DQ SB-APPEND-C s" a" SB-APPEND JRT-DQ SB-APPEND-C
   JRT-SP SB-APPEND-C JRT-COLON SB-APPEND-C JRT-SP SB-APPEND-C
   s" 1" SB-APPEND JRT-SP SB-APPEND-C
   JRT-RBRACE SB-APPEND-C JRT-SP SB-APPEND-C JRT-SP SB-APPEND-C
   SB$ ;

\ Deep documents outgrow SB, so they are built in their own buffer, sized for
\ the copy case's text: JRT-COPY-DEPTH levels around one digit.
1000000 constant JRT-COPY-DEPTH
JRT-COPY-DEPTH 2 * 1+ constant JRT-DEEP-CAP
create JRT-DEEP-BUF JRT-DEEP-CAP allot

: JRT-FILL ( n n n -- ) {: at:n count:n byte:n :}
   count 0 ?do byte JRT-DEEP-BUF at i + + c! loop ;

: JRT-DEEP$ ( n -- str ) {: depth:n :}       \ depth '[' then depth ']'
   0 depth JRT-LBRACK JRT-FILL
   depth depth JRT-RBRACK JRT-FILL
   JRT-DEEP-BUF depth 2 * >str ;

\ depth '[' around one digit byte, then depth ']', at offset at of the buffer.
: JRT-DEEP-ONE$ ( n n n -- str ) {: at:n depth:n digit:n :}
   at depth JRT-LBRACK JRT-FILL
   at depth + 1 digit JRT-FILL
   at depth + 1+ depth JRT-RBRACK JRT-FILL
   JRT-DEEP-BUF at + depth 2 * 1+ >str ;

: JRT-OPENS$ ( n -- str ) {: depth:n :}      \ depth '[' only
   0 depth JRT-LBRACK JRT-FILL
   JRT-DEEP-BUF depth >str ;

: JRT-DEEP-BAD$ ( n -- str ) {: depth:n :}   \ depth '[' then a bare x
   0 depth JRT-LBRACK JRT-FILL
   depth 1 JRT-X JRT-FILL
   JRT-DEEP-BUF depth 1+ >str ;

\ depth objects, each but the innermost holding only "a"; the innermost holds
\ "x":1,"y":7.
: JRT-NEST-OBJ$ ( n -- str ) {: depth:n :}
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
: JRT-SINCE$ ( n n -- str ) {: end:n at:n :}
   JRT-DEEP-BUF at +  end at - >str ;

\ depth objects at offset at, each holding only "k", around the digit 1.
: JRT-NEST-AT$ ( n n -- str ) {: at:n depth:n :}
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
: JRT-FLAT-AT$ ( n n bool -- str ) {: at:n count:n back:bool :}
   at JRT-LBRACE JRT-PUT
   count 0 ?do
      i 0 > if JRT-COMMA JRT-PUT then
      back if count 1- i - else i then JRT-MEMBER-AT
   loop
   JRT-RBRACE JRT-PUT
   at JRT-SINCE$ ;

: JRT-SCALAR-INT ( str n -- ) {: want:n :}
   JRT-OPEN-A
   JSON:READ-NEXT JSON:T-INT T=
   JSON:READ-INT want T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-SCALAR-FLOAT ( str r -- ) {: want:r :}
   JRT-OPEN-A
   JSON:READ-NEXT JSON:T-FLOAT T=
   JSON:READ-FLOAT want JRT-NEAR TTRUE
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-TEST-INTS ( -- )
   s" READ-INT reads integers up to the largest signed cell" T-LABEL
   s" 0" 0 JRT-SCALAR-INT
   s" -1" -1 JRT-SCALAR-INT
   s" 42" 42 JRT-SCALAR-INT
   s" 9223372036854775807" STR:MAX-I64 JRT-SCALAR-INT ;

\ READ-U64 answers a value's 64 bits: 2^63 is STR:MIN-I64 and 2^64-1 is -1.
: JRT-SCALAR-U64 ( str n -- ) {: want:n :}
   JRT-OPEN-A
   JSON:READ-NEXT JSON:T-INT T=
   JSON:READ-U64 want T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-TEST-U64 ( -- )
   s" READ-U64 answers an integer's 64 bits up to 2^64-1" T-LABEL
   s" 0" 0 JRT-SCALAR-U64
   s" 9223372036854775807" STR:MAX-I64 JRT-SCALAR-U64
   s" 9223372036854775808" STR:MIN-I64 JRT-SCALAR-U64
   s" 18446744073709551615" -1 JRT-SCALAR-U64 ;

: JRT-U64-OF ( str -- )
   JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-U64 drop JSON:READ-CLOSE ;

: JRT-TEST-U64-REJECTIONS ( -- )
   s" READ-U64 rejects a negative, a value past 64 bits, a fraction and an exponent" T-LABEL
   [: s" -1" JRT-U64-OF ;] E-JSON-READ-NUMBER TTHROWSQ
   [: s" 18446744073709551616" JRT-U64-OF ;] E-JSON-READ-NUMBER TTHROWSQ
   [: s" 99999999999999999999" JRT-U64-OF ;] E-JSON-READ-NUMBER TTHROWSQ
   [: s" 1.0" JRT-U64-OF ;] E-JSON-READ-NUMBER TTHROWSQ
   [: s" 1e3" JRT-U64-OF ;] E-JSON-READ-NUMBER TTHROWSQ
   s" READ-U64 on a string throws E-JSON-READ-STATE" T-LABEL
   [: JRT-HI-SRC$ JRT-U64-OF ;] E-JSON-READ-STATE TTHROWSQ ;

: JRT-TEST-FLOATS ( -- )
   s" READ-FLOAT reads fractions and exponents" T-LABEL
   s" 3.14" 3.14 JRT-SCALAR-FLOAT
   s" 1e9" 1000000000.0 JRT-SCALAR-FLOAT
   s" -2.5E-3" -0.0025 JRT-SCALAR-FLOAT
   s" 0.5" 0.5 JRT-SCALAR-FLOAT ;

: JRT-TEST-LITERALS ( -- )
   s" true, false and null read as their own token kinds" T-LABEL
   s" true" JRT-OPEN-A JSON:READ-NEXT JSON:T-TRUE T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE
   s" false" JRT-OPEN-A JSON:READ-NEXT JSON:T-FALSE T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE
   s" null" JRT-OPEN-A JSON:READ-NEXT JSON:T-NULL T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE ;

: JRT-TEST-STRING ( -- )
   s" READ-SPAN$ answers a string's raw bytes without its quotes, and READ-STR its value" T-LABEL
   JRT-HI-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JSON:READ-SPAN$ s" hi" T$=
   s" hi" JRT-STR=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-TEST-ESCAPES ( -- )
   s" READ-STR decodes every two-character escape" T-LABEL
   JRT-ESC-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-ESC-WANT$ JRT-STR=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

\ An empty vector with room for n bytes.
: JRT-APPEND-FRESH ( n -- )
   {: room:n :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   v room VEC:RESERVE
   v JRT-APPEND-VEC ! ;
: JRT-APPEND$ ( -- str )  JRT-APPEND-VEC @ VEC:BYTES ;
: JRT-APPEND-LEN ( -- n )  JRT-APPEND-VEC @ VEC:COUNT ;

: JRT-TEST-STR-APPEND ( -- )
   s" READ-STR-APPEND appends a string's decoded bytes, and nothing for an empty string" T-LABEL
   64 JRT-APPEND-FRESH
   JRT-ESC-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-APPEND-VEC @ JSON:READ-STR-APPEND
   JRT-APPEND$ JRT-ESC-WANT$ T$=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE
   s\" \"\"" JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-APPEND-VEC @ JSON:READ-STR-APPEND
   JRT-APPEND-LEN 8 T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE
   JRT-APPEND-VEC @ VEC:DELETE ;

\ Room for 4 bytes holds "ab" but not the escaped string after it.
: JRT-TEST-STR-APPEND-GROW ( -- )
   s" READ-STR-APPEND grows a full vector and keeps what it held" T-LABEL
   4 JRT-APPEND-FRESH
   s" ab" JRT-APPEND-VEC @ VEC:APPEND
   JRT-ESC-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-APPEND-VEC @ JSON:READ-STR-APPEND
   JSON:READ-CLOSE
   JRT-APPEND-VEC @ VEC:CAP 4 > TTRUE
   JRT-APPEND$ {: got:str :}
   got STR:DATA 2 >str s" ab" T$=
   got STR:DATA 2 + got STR:LEN 2 - >str JRT-ESC-WANT$ T$=
   JRT-APPEND-VEC @ VEC:DELETE ;

: JRT-TEST-STR-APPEND-KEY ( -- )
   s" READ-STR-APPEND appends a key's decoded bytes" T-LABEL
   16 JRT-APPEND-FRESH
   s\" {\"k\\u0065y\":1}" JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JSON:READ-NEXT JSON:T-KEY T=
   JRT-APPEND-VEC @ JSON:READ-STR-APPEND
   JRT-APPEND$ s" key" T$=
   JSON:READ-NEXT JSON:T-INT T=
   JSON:READ-CLOSE
   JRT-APPEND-VEC @ VEC:DELETE ;

: JRT-BAD-APPEND-KIND ( -- )
   s" 12" JRT-OPEN-A
   JSON:READ-NEXT drop JRT-APPEND-VEC @ JSON:READ-STR-APPEND JSON:READ-CLOSE ;

: JRT-BAD-STR-KIND ( -- )
   s" 12" JRT-OPEN-A
   JSON:READ-NEXT drop JRT-APPEND-VEC @ JSON:READ-STR str> 2drop JSON:READ-CLOSE ;

: JRT-TEST-STR-APPEND-MISUSE ( -- )
   s" READ-STR-APPEND and READ-STR on a number throw E-JSON-READ-STATE and leave the vector as it was" T-LABEL
   16 JRT-APPEND-FRESH
   s" ab" JRT-APPEND-VEC @ VEC:APPEND
   [: JRT-BAD-APPEND-KIND ;] E-JSON-READ-STATE TTHROWSQ
   JRT-A JSON:READ-DISPOSE
   JRT-APPEND$ s" ab" T$=
   [: JRT-BAD-STR-KIND ;] E-JSON-READ-STATE TTHROWSQ
   JRT-A JSON:READ-DISPOSE
   JRT-APPEND$ s" ab" T$=
   JRT-APPEND-VEC @ VEC:DELETE ;

: JRT-TEST-UESCAPE ( -- )
   s" READ-STR decodes a \u escape" T-LABEL
   JRT-U-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-U-WANT$ JRT-STR=
   JSON:READ-CLOSE ;

: JRT-TEST-SURROGATE ( -- )
   s" READ-STR decodes a surrogate pair to one four-byte UTF-8 character" T-LABEL
   JRT-SUR-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-SUR-WANT$ JRT-STR=
   JSON:READ-CLOSE ;

: JRT-TEST-UTF8 ( -- )
   s" UTF-8 of two, three and four bytes reads back unchanged" T-LABEL
   JRT-UTF8-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   JRT-UTF8-WANT$ JRT-STR=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE ;

: JRT-TEST-EMPTY ( -- )
   s" an empty object and an empty array read as their start and end tokens" T-LABEL
   s" {}" JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-NEXT JSON:T-OBJ-END T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE
   s" []" JRT-OPEN-A
   JSON:READ-NEXT JSON:T-ARR T= JSON:READ-NEXT JSON:T-ARR-END T= JSON:READ-NEXT JSON:T-END T= JSON:READ-CLOSE ;

: JRT-TEST-NESTED ( -- )
   s" READ-NEXT walks nested objects and arrays token by token" T-LABEL
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
   s" KIND$ names every token kind, and a number past them as no token kind" T-LABEL
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
   s" READ-VALUE-SPAN$ answers a container's whole text and moves past it" T-LABEL
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

: JRT-ANY-SPAN$ ( -- str )
   s\" {\"o\":{\"k\":1},\"a\":[1,2],\"s\":\"x\\\"y\",\"i\":-12,\"f\":1.5e-3,\"t\":true,\"u\":false,\"n\":null}" ;

\ The next value's span is this text, and the token after it is this key.
: JRT-ANY-KEY ( JSON:reader str str -- JSON:reader )
   {: v:str k:str :}
   JSON:READ-NEXT drop
   JSON:READ-ANY-SPAN$ v T$=
   JSON:READ-NEXT JSON:T-KEY T= k JRT-STR= ;

: JRT-BAD-ANY-KEY ( -- )
   JRT-ANY-SPAN$ JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop
   JSON:READ-ANY-SPAN$ drop JSON:READ-CLOSE ;

: JRT-BAD-ANY-END ( -- )
   s" {}" JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop
   JSON:READ-ANY-SPAN$ drop JSON:READ-CLOSE ;

: JRT-TEST-ANY-SPAN ( -- )
   s" READ-ANY-SPAN$ answers the text of a value of every kind and moves past it" T-LABEL
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
   s" READ-ANY-SPAN$ on a key or an object's end throws E-JSON-READ-STATE" T-LABEL
   [: JRT-BAD-ANY-KEY ;] E-JSON-READ-STATE TTHROWSQ
   JRT-A JSON:READ-DISPOSE
   [: JRT-BAD-ANY-END ;] E-JSON-READ-STATE TTHROWSQ
   JRT-A JSON:READ-DISPOSE ;

: JRT-TEST-WS ( -- )
   s" whitespace around and between tokens is skipped" T-LABEL
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
   s" the end token's span is empty and sits at the source's end, also when read again" T-LABEL
   s" 1  " {: source:str :}
   source JRT-OPEN-A
   JSON:READ-NEXT JSON:T-INT T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-TOKEN JSON:T-END T=
   JSON:READ-SPAN$ {: span:str :}
   span STR:LEN 0 T=
   span STR:DATA source STR:DATA source STR:LEN + = TTRUE
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-TOKEN JSON:T-END T=
   JSON:READ-SPAN$ {: span2:str :}
   span2 STR:LEN 0 T=
   span2 STR:DATA source STR:DATA source STR:LEN + = TTRUE
   JSON:READ-CLOSE ;

: JRT-RT-BUILD ( -- str )
   JRT-WRITER JRT-WRITE-OUT @ JSON:OPEN
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
   s" what the writer writes reads back field by field" T-LABEL
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

: JRT-COPY$ ( -- str )
   s\" {\"a\":1,\"b\":[2,3]}" ;

: JRT-COPY ( str n -- str )
   {: src:str indent:n :}
   src JSON:PARSE
   JRT-WRITER JRT-WRITE-OUT @ JSON:OPEN indent JSON:WRITE-INDENT!
   JSON:COPY-VALUE JSON:WRITTEN$ ;

\ COPY-VALUE lays the value out through the writer's tokens, as any writing.
: JRT-TEST-COPY-LAYOUT ( -- )
   s" COPY-VALUE lays a value out at the writer's indent" T-LABEL
   JRT-COPY$ 2 JRT-COPY
   s\" {\n  \"a\": 1,\n  \"b\": [\n    2,\n    3\n  ]\n}" T$=
   JRT-COPY$ 0 JRT-COPY JRT-COPY$ T$= ;

: JRT-SCALARS$ ( -- str )
   s\" {\"ké\":\"a\\nb\",\"f\":1.5e3,\"t\":[true,false,null]}" ;

: JRT-TEST-COPY-SCALARS ( -- )
   s" COPY-VALUE writes strings, escapes, numbers and literals" T-LABEL
   JRT-SCALARS$ 0 JRT-COPY JRT-SCALARS$ T$= ;

\ A value nested deeper than the return stack holds frames copies whole.

: JRT-TEST-COPY-DEEP ( -- )
   0 JRT-COPY-DEPTH JRT-DIGIT-1 JRT-DEEP-ONE$ {: src:str :}
   src JSON:PARSE
   JRT-WRITER JRT-WRITE-OUT @ JSON:OPEN
   JSON:COPY-VALUE
   JSON:WRITTEN$ src T$= ;

\ CODE's REASON text, then released, so a later case's plain throw of CODE
\ cannot match it.
: JRT-SAID ( n str -- ) {: code:n ws:str :}
   code REASON:TEXT$ ws T$=
   REASON:RELEASE ;

: JRT-TEST-FIND-HIT ( -- )
   s" READ-FIND-KEY stops on the found key's value" T-LABEL
   JRT-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   s" a" JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T=
   JSON:READ-INT 1 T=
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-SKIP ( -- )
   s" READ-FIND-KEY finds a later key after READ-SKIP-VALUE skips an object" T-LABEL
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
   s" READ-FIND-KEY answers false for an absent key" T-LABEL
   JRT-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   s" zzz" JSON:READ-FIND-KEY TFALSE
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-NESTED-SAME-NAME ( -- )
   s" READ-FIND-KEY ignores a key of that name in a nested object" T-LABEL
   JRT-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   s" x" JSON:READ-FIND-KEY TFALSE
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-LONG ( n -- ) {: len:n :}
   len JRT-LONG-FIND$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JRT-LONG-KEY len >str JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T=
   JSON:READ-INT 7 T=
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-LENGTHS ( -- )
   s" READ-FIND-KEY finds keys of 256, 257 and 768 bytes" T-LABEL
   256 JRT-TEST-FIND-LONG
   257 JRT-TEST-FIND-LONG
   JRT-LONG-KEY-CAP JRT-TEST-FIND-LONG ;

: JRT-TEST-FIND-ESCAPED ( -- )
   s" READ-FIND-KEY matches an escaped key by its decoded bytes" T-LABEL
   JRT-FIND-ESC-SRC$ JRT-OPEN-A
   JSON:READ-NEXT JSON:T-OBJ T=
   JRT-FIND-ESC-WANT$ JSON:READ-FIND-KEY TTRUE
   JSON:READ-TOKEN JSON:T-INT T=
   JSON:READ-INT 9 T=
   JSON:READ-CLOSE ;

: JRT-TEST-FIND-ANCHOR ( -- )
   s" READ-FIND-KEY searches the innermost open object and stops at its end" T-LABEL
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
   JSON:READ-NEXT drop JRT-ZERO-U8 -1 >str JSON:READ-FIND-KEY drop JSON:READ-CLOSE ;

: JRT-BAD-FIND-KEY-PTR ( -- )
   s" {}" JRT-OPEN-A
   JSON:READ-NEXT drop JRT-ZERO-U8 1 >str JSON:READ-FIND-KEY drop JSON:READ-CLOSE ;

: JRT-TEST-FIND-MISUSE ( -- )
   s" READ-FIND-KEY on an array, a scalar, a key or an invalid key span throws E-JSON-READ-STATE" T-LABEL
   [: JRT-BAD-FIND-ARRAY ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-FIND-SCALAR ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-FIND-PHASE ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-FIND-KEY-LEN ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-FIND-KEY-PTR ;] E-JSON-READ-STATE TTHROWSQ ;

: JRT-REPEAT ( str n -- ) {: t:str want:n :}
   t JSON:READ-REPEAT MATCH option
     none OF false TTRUE ENDOF
     some OF want T= ENDOF
   ;MATCH ;

: JRT-UNREPEATED ( str -- ) {: t:str :}
   t JSON:READ-REPEAT MATCH option
     none OF true TTRUE ENDOF
     some OF drop false TTRUE ENDOF
   ;MATCH ;

: JRT-TEST-REPEATS ( -- )
   s" READ-REPEAT answers the first top-level key that repeats one, escaped spellings included" T-LABEL
   s" {}" JRT-UNREPEATED
   s\" {\"a\":1,\"b\":2}" JRT-UNREPEATED
   s\" {\"a\":1,\"a\":2}" 1 JRT-REPEAT
   s\" {\"a\":1,\"b\":2,\"\\u0061\":3}" 2 JRT-REPEAT
   s\" {\"ab\":1,\"a\":2}" JRT-UNREPEATED
   s\" {\"a\":{\"b\":1,\"b\":2}}" JRT-UNREPEATED
   s\" {\"a\":{\"x\":[1,{\"a\":3}]},\"a\":2}" 1 JRT-REPEAT
   s\" {\"b\":1,\"a\":2,\"a\":3,\"b\":4}" 2 JRT-REPEAT
   s\" {\"stem1\":1,\"stem2\":2,\"stem1\":3}" 2 JRT-REPEAT
   s\" {\"ab\":1,\"abc\":2,\"abd\":3,\"ab\":4}" 3 JRT-REPEAT
   s\" {\"a\":1,\"a\":2,\"a\":3}" 1 JRT-REPEAT
   s\" {\"aa1\":1,\"aa2\":2,\"aa3\":3,\"bb1\":4,\"bb2\":5,\"bb3\":6,\"bb1\":7}"
   6 JRT-REPEAT
   s" READ-REPEAT throws E-JSON-READ-STATE on an array and E-JSON-READ-EOF on a truncated object" T-LABEL
   [: s" [1]" JSON:READ-REPEAT drop ;] E-JSON-READ-STATE TTHROWSQ
   [: s\" {\"a\":1," JSON:READ-REPEAT drop ;] E-JSON-READ-EOF TTHROWSQ ;

: JRT-MEMBER$ ( str str -- str )
   {: s:str k:str :}
   s k JRT-A JSON:READ-MEMBER$ ;

: JRT-NO-MEMBER ( str -- )
   {: s:str :}
   [: s s" a" JRT-MEMBER$ drop ;] JSON:E-MEMBER TTHROWSQ ;

: JRT-TEST-MEMBER-FOUND ( -- )
   s" READ-MEMBER$ answers the text of a top-level member's value" T-LABEL
   s\" {\"a\":1,\"b\":2}" s" a" JRT-MEMBER$ s" 1" T$=
   s\" {\"b\":[2],\"a\":{\"c\":3}}" s" a" JRT-MEMBER$
   s\" {\"c\":3}" T$=
   s\" {\"b\":1,\"\\u0061\":\"x\",\"b\":2}" s" a" JRT-MEMBER$
   s\" \"x\"" T$=
   s\" {\"a\":{\"a\":1},\"b\":{\"a\":2}}" s" a" JRT-MEMBER$
   s\" {\"a\":1}" T$= ;

: JRT-TEST-MEMBER-REJECTED ( -- )
   s" READ-MEMBER$ throws JSON:E-MEMBER for a missing or repeated key and a text that is not an object" T-LABEL
   s\" {\"b\":1}" JRT-NO-MEMBER
   s\" {\"a\":1,\"b\":2,\"a\":1}" JRT-NO-MEMBER
   s\" [{\"a\":1}]" JRT-NO-MEMBER
   s\" \"a\"" JRT-NO-MEMBER
   s\" {\"b\":{\"a\":1}}" JRT-NO-MEMBER
   s" READ-MEMBER$ throws E-JSON-READ-EOF on a truncated text" T-LABEL
   [: s\" {\"a\":1," s" a" JRT-MEMBER$ drop ;]
   E-JSON-READ-EOF TTHROWSQ ;

\ A rejected text leaves JRT-A for the next call.
: JRT-TEST-MEMBER-REUSE ( -- )
   s" READ-MEMBER$ reads again after a rejected text" T-LABEL
   s\" {\"a\":1,\"a\":2}" JRT-NO-MEMBER
   s\" {\"a\":2}" s" a" JRT-MEMBER$ s" 2" T$=
   s\" {\"a\":3}" s" a" JRT-MEMBER$ s" 3" T$= ;

\ A reader code answers its declared name and a text.
: JRT-CODE ( n str -- ) {: code:n name:str :}
   code ERROR:NAME$ name T$=
   code ERROR:TEXT$ STR:LEN 0<> TTRUE ;

: JRT-TEST-CODES ( -- )
   s" each reader code answers its declared name and a text" T-LABEL
   E-JSON-READ-MALFORMED s" E-JSON-READ-MALFORMED" JRT-CODE
   E-JSON-READ-STRING s" E-JSON-READ-STRING" JRT-CODE
   E-JSON-READ-ESCAPE s" E-JSON-READ-ESCAPE" JRT-CODE
   E-JSON-READ-SURROGATE s" E-JSON-READ-SURROGATE" JRT-CODE
   E-JSON-READ-TRAILING s" E-JSON-READ-TRAILING" JRT-CODE
   E-JSON-READ-NUMBER s" E-JSON-READ-NUMBER" JRT-CODE
   E-JSON-READ-EOF s" E-JSON-READ-EOF" JRT-CODE
   E-JSON-READ-COLON s" E-JSON-READ-COLON" JRT-CODE
   E-JSON-READ-COMMA s" E-JSON-READ-COMMA" JRT-CODE
   E-JSON-READ-STATE s" E-JSON-READ-STATE" JRT-CODE
   JSON:E-READ-SOURCE s" JSON:E-READ-SOURCE" JRT-CODE ;

: JRT-TEST-VALUE-EQ-SCALARS ( -- )
   s" VALUE= compares scalars by value and strings by decoded bytes" T-LABEL
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

: JRT-COMPARE ( str str n -- ) {: a:str b:str want:n :}
   a b JSON:NUMBER-COMPARE MATCH option
     none OF false TTRUE ENDOF
     some OF want T= ENDOF
   ;MATCH ;

: JRT-UNCOMPARED ( str str -- ) {: a:str b:str :}
   a b JSON:NUMBER-COMPARE MATCH option
     none OF true TTRUE ENDOF
     some OF drop false TTRUE ENDOF
   ;MATCH ;

: JRT-TEST-NUMBER-COMPARE ( -- )
   s" NUMBER-COMPARE orders number lexemes by value" T-LABEL
   s" 1" s" 1.0" 0 JRT-COMPARE
   s" 10e-1" s" 1" 0 JRT-COMPARE
   s" -0" s" 0" 0 JRT-COMPARE
   s" 1" s" 2" -1 JRT-COMPARE
   s" -2" s" -10" 1 JRT-COMPARE
   s" -1" s" 1" -1 JRT-COMPARE
   s" 0.5" s" 0" 1 JRT-COMPARE
   s" 0" s" -0.5" 1 JRT-COMPARE
   s" 0.1" s" 0.10000000000000001" -1 JRT-COMPARE
   s" 123456789012345678901" s" 123456789012345678902" -1 JRT-COMPARE
   s" 1e400" s" 1e399" 1 JRT-COMPARE
   s" 1E+2" s" 100" 0 JRT-COMPARE
   s" 1e9999999999999999999999" s" 1" JRT-UNCOMPARED
   s" 1" s" -1e-9999999999999999999999" JRT-UNCOMPARED ;

: JRT-TEST-VALUE-EQ-ARRAYS ( -- )
   s" VALUE= compares arrays element by element in order" T-LABEL
   s" []" s" [ ]" JSON:VALUE= TTRUE
   s" []" s" {}" JSON:VALUE= TFALSE
   s" [1,2]" s" [1.0, 2]" JSON:VALUE= TTRUE
   s" [1,2]" s" [2,1]" JSON:VALUE= TFALSE
   s" [1,2]" s" [1,2,3]" JSON:VALUE= TFALSE
   s" [1,2,3]" s" [1,2]" JSON:VALUE= TFALSE
   \ Thirty elements outgrow the first room b's rows get, so they grow while
   \ b is indexed.
   s" [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]" dup
   JSON:VALUE= TTRUE
   s" [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"
   s" [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]"
   JSON:VALUE= TFALSE ;

: JRT-TEST-VALUE-EQ-OBJECTS ( -- )
   s" VALUE= compares objects as unordered members at every depth" T-LABEL
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
   s" VALUE= answers false when either text repeats a key" T-LABEL
   s\" {\"a\":1,\"a\":1}" s\" {\"a\":1,\"b\":1}" JSON:VALUE= TFALSE
   s\" {\"a\":1,\"b\":1}" s\" {\"a\":1,\"a\":1}" JSON:VALUE= TFALSE
   s\" {\"a\":1,\"a\":1}" s\" {\"a\":1,\"a\":1}" JSON:VALUE= TFALSE
   s\" [{\"a\":1,\"\\u0061\":1}]" s\" [{\"a\":1,\"a\":1}]" JSON:VALUE= TFALSE
   s\" {\"k\":1,\"k\":1,\"j\":2}" s\" {\"k\":1,\"j\":2,\"m\":3}" JSON:VALUE= TFALSE
   s\" {\"k\":1,\"j\":2}" s\" {\"k\":1,\"k\":1,\"j\":2}" JSON:VALUE= TFALSE ;

\ Both texts are read whole, so a malformed one throws its code whatever the
\ comparison would answer.
: JRT-TEST-VALUE-EQ-MALFORMED ( -- )
   s" VALUE= throws a malformed text's code whatever the comparison would answer" T-LABEL
   [: s\" {\"a\":1," s" {}" JSON:VALUE= drop ;] E-JSON-READ-EOF TTHROWSQ
   [: s" 1 2" s" 1" JSON:VALUE= drop ;] E-JSON-READ-TRAILING TTHROWSQ
   [: s" 1" s\" \"x\" x" JSON:VALUE= drop ;] E-JSON-READ-TRAILING TTHROWSQ
   [: s" [1,2" s" [2]" JSON:VALUE= drop ;] E-JSON-READ-EOF TTHROWSQ
   [: s" [2]" s" [1,2" JSON:VALUE= drop ;] E-JSON-READ-EOF TTHROWSQ ;

\ Numbers compare by value on their lexemes, past STR>FLOAT's digit and exponent
\ limits; an exponent past a signed cell, stated or scaled, throws.
: JRT-TEST-VALUE-EQ-NUMBERS ( -- )
   s" VALUE= compares numbers by value past STR>FLOAT's digit and exponent limits" T-LABEL
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
   s" VALUE= throws E-JSON-READ-NUMBER on an exponent past a signed cell" T-LABEL
   [: s" 1e9223372036854775807" s" 1" JSON:VALUE= drop ;] E-JSON-READ-NUMBER TTHROWSQ ;

\ 100000 arrays deep, past what recursion on the return stack survives,
\ compare equal to themselves and unequal with the innermost digit changed.
: JRT-TEST-VALUE-EQ-DEEP ( -- )
   s" VALUE= compares arrays 100000 deep" T-LABEL
   0 100000 JRT-DIGIT-1 JRT-DEEP-ONE$ dup JSON:VALUE= TTRUE
   0 100000 JRT-DIGIT-1 JRT-DEEP-ONE$
   200001 100000 JRT-DIGIT-2 JRT-DEEP-ONE$ JSON:VALUE= TFALSE ;

\ The least of five VALUE= calls on each pair of equal texts, in this thread's CPU nanoseconds, so
\ descheduling adds nothing; the pairs alternate, so a move between core kinds reaches both alike.
: JRT-EQ-LEASTS ( str str str str -- n n )
   {: a:str b:str c:str d:str :}
   MEM-MAX-N MEM-MAX-N  5 0 ?do
      swap  TIME:THREAD-CPU-NS  a b JSON:VALUE= TTRUE  TIME:THREAD-CPU-NS swap -  min
      swap  TIME:THREAD-CPU-NS  c d JSON:VALUE= TTRUE  TIME:THREAD-CPU-NS swap -  min
   loop ;

\ Objects 4000 and 8000 deep against copies: the bound is 3 because a walk n log n in the bytes
\ grows under 2.2 times, and one that rereads each enclosing object for every level quadruples it.
: JRT-TEST-VALUE-EQ-DEPTH-TIME ( -- )
   s" VALUE= on objects twice as deep takes under three times the time" T-LABEL
   200000 4000 JRT-NEST-AT$  300000 4000 JRT-NEST-AT$
   0 8000 JRT-NEST-AT$  100000 8000 JRT-NEST-AT$  JRT-EQ-LEASTS {: t4:n t8:n :}
   t8  t4 3 *  < TTRUE ;

\ One object of 4000 and of 8000 members against a reversed copy: the bound is 3 because a walk
\ n log n in the members grows under 2.2 times; one that rescans b's object per key quadruples it.
: JRT-TEST-VALUE-EQ-MEMBERS-TIME ( -- )
   s" VALUE= on twice the members takes under three times the time" T-LABEL
   200000 4000 false JRT-FLAT-AT$  300000 4000 true JRT-FLAT-AT$
   0 8000 false JRT-FLAT-AT$  100000 8000 true JRT-FLAT-AT$  JRT-EQ-LEASTS {: t4:n t8:n :}
   t8  t4 3 *  < TTRUE ;

\ Whether a one-string document decodes to the given bytes; the cursor must
\ still be on the string, so the document ends at the next token.
: JRT-EQ? ( str str -- bool )
   {: src:str want:str :}
   src JRT-OPEN-A
   JSON:READ-NEXT JSON:T-STR T=
   want JSON:READ-STR-EQ? {: same:bool :}
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-CLOSE
   same ;

: JRT-TEST-STR-EQ ( -- )
   s" READ-STR-EQ? compares a string's decoded bytes with a span" T-LABEL
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
   s" READ-STR-EQ? compares a key, and a repeated comparison sees the same key" T-LABEL
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
   JSON:READ-NEXT drop JRT-ZERO-U8 -1 >str JSON:READ-STR-EQ? drop JSON:READ-CLOSE ;

: JRT-BAD-EQ-PTR ( -- )
   JRT-HI-SRC$ JRT-OPEN-A
   JSON:READ-NEXT drop JRT-ZERO-U8 2 >str JSON:READ-STR-EQ? drop JSON:READ-CLOSE ;

: JRT-TEST-STR-EQ-MISUSE ( -- )
   s" READ-STR-EQ? on a number or with an invalid span throws E-JSON-READ-STATE" T-LABEL
   [: JRT-BAD-EQ-KIND ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-EQ-LEN ;] E-JSON-READ-STATE TTHROWSQ
   [: JRT-BAD-EQ-PTR ;] E-JSON-READ-STATE TTHROWSQ ;

: JRT-TEST-INTERLEAVE ( -- )
   s" two readers advance independently, interleaved" T-LABEL
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
   s" a failure caught in one reader leaves another reading on" T-LABEL
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

CAST: >RAW ( ^JSON:cursor -- ^u8 )

\ JSON's OPAQUE cursor keeps CURSOR-BYTES private; a one-field layout spans it.
STRUCTURE held DERIVE addr FIELD record JSON:cursor ;STRUCTURE

: JRT-NONZERO-BYTES ( -- n )
   JRT-A >RAW {: raw:^ :}
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
: JRT-DEEP-REPEAT$ ( n -- str ) {: depth:n :}
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
   70 JRT-DEEP-REPEAT$ 2 JRT-REPEAT
   [: 70 JRT-DEEP-REPEAT$ str> 1- >str JSON:READ-REPEAT drop ;] E-JSON-READ-EOF TTHROWSQ ;

\ 70 objects deep, both texts' records grow stacks; a truncated a throws while b's
\ record holds its stack.
: JRT-DEEP-VALUE-EQ ( -- )
   70 JRT-NEST-OBJ$ dup JSON:VALUE= TTRUE
   [: 70 JRT-NEST-OBJ$ str> 1- >str  70 JRT-NEST-OBJ$ JSON:VALUE= drop ;] E-JSON-READ-EOF TTHROWSQ ;

\ This VALUE='s scratch fits a pooled MEM:WITH-BYTES slot, and the pool keeps
\ a chunk it maps for later scopes, which is no leak. One call before the
\ release check maps any chunk the slot needs, so the probes test the
\ records' stacks, not the pool.
: JRT-WARM-VALUE-EQ ( -- )
   70 JRT-NEST-OBJ$ dup JSON:VALUE= drop ;

\ READ-REPEAT opens the task's first MEM pool scope, and the pool keeps the
\ chunk it maps; one call maps it before the release check.
: JRT-WARM-REPEAT ( -- )
   70 JRT-DEEP-REPEAT$ JSON:READ-REPEAT drop ;

: JRT-TEST-DEEP ( -- )
   s" nesting at and past the inline stack parses" T-LABEL
   [: JRT-DEEP-EDGES ;] MAPPED:RELEASED? TTRUE
   s" 10000 levels parse and READ-CLOSE releases the stack" T-LABEL
   [: JRT-DEEP-10000 ;] MAPPED:RELEASED? TTRUE
   s" 40000 levels double the stack and READ-CLOSE releases it" T-LABEL
   [: JRT-DEEP-40000 ;] MAPPED:RELEASED? TTRUE
   s" READ-FIND-KEY works 70 objects deep and READ-FINISH releases the stack" T-LABEL
   [: JRT-TEST-DEEP-FIND ;] MAPPED:RELEASED? TTRUE
   \ A task's first rejection maps its REASON text for good, so one is taken
   \ before the probes.
   [: s" x" JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-CLOSE ;]
   E-JSON-READ-MALFORMED TTHROWSQ
   s" a failure 100 deep releases the stack" T-LABEL
   [: JRT-DEEP-MALFORMED-THROWS ;] MAPPED:RELEASED? TTRUE
   s" READ-DISPOSE releases what a caller's throw abandoned" T-LABEL
   [: JRT-ABANDON-DISPOSE ;] MAPPED:RELEASED? TTRUE
   s" DISPOSE leaves the record zero-filled" T-LABEL
   [: JRT-DISPOSE-ZEROES ;] MAPPED:RELEASED? TTRUE
   s" a record holding a leftover parses again and nothing stays mapped" T-LABEL
   [: JRT-ABANDON-REINIT ;] MAPPED:RELEASED? TTRUE
   s" READ-REPEAT past 64 levels releases both stacks, a throw included" T-LABEL
   \ With the pool chunk mapped, READ-REPEAT holds two mappings here at once,
   \ its scratch and its record's stack, each one page at most.
   JRT-WARM-REPEAT
   [: JRT-DEEP-REPEATS ;] MAPPED:RELEASED? TTRUE
   s" VALUE= past 64 levels releases every record's stack, a throw included" T-LABEL
   JRT-WARM-VALUE-EQ
   [: JRT-DEEP-VALUE-EQ ;] MAPPED:RELEASED? TTRUE ;

: JRT-BAD-NEGATIVE-SOURCE ( -- )
   JRT-A s" 0" STR:DATA -1 >str JSON:READ-INIT JSON:READ-CLOSE ;

: JRT-BAD-NULL-SOURCE ( -- )
   JRT-A JRT-ZERO-U8 1 >str JSON:READ-INIT JSON:READ-CLOSE ;

: JRT-TEST-SEALED ( -- )
   s" package JSON : FORGE ( ^JSON:cursor -- JSON:reader ) MINT-READER ; ;package"
   {: src:str :}
   src JRT-SUBJECT-OUT $400 >LEN JRT-SUBJECT-ERR $400 >LEN 1000 >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src JRT-SUBJECT-OUT outu LEN>N >str JRT-SUBJECT-ERR erru LEN>N >str
   oc 70 T-ENDING-EXITED= ;

: JRT-TEST-PRIVATE-STATE ( -- )
   s" JRT-BAD-MINT ( ^JSON:cursor -- JSON:reader ) JSON:MINT-READER" JRT-REJECTED
   s" JRT-BAD-ERASE ( JSON:reader -- ^JSON:cursor ) JSON:ERASE-READER" JRT-REJECTED
   s" JRT-BAD-PRIVATE-STATE ( JSON:reader -- JSON:reader ^JSON:cursor ) JSON:READER>STATE" JRT-REJECTED ;

: JRT-TEST-OWNERSHIP ( -- )
   s" negative source length is rejected before mint" T-LABEL
   [: JRT-BAD-NEGATIVE-SOURCE ;] JSON:E-READ-SOURCE TTHROWSQ
   s" null nonempty source is rejected before mint" T-LABEL
   [: JRT-BAD-NULL-SOURCE ;] JSON:E-READ-SOURCE TTHROWSQ
   s" null empty source remains a valid empty reader" T-LABEL
   JRT-A JRT-ZERO-U8 0 >str JSON:READ-INIT JSON:READ-CLOSE
   s" raw representation and state helpers stay private" T-LABEL
   JRT-TEST-PRIVATE-STATE
   s" the package cannot be reopened to reach private representation words" T-LABEL
   JRT-TEST-SEALED
   s" linear reader cannot be duplicated" T-LABEL
   s" JRT-BAD-DUP ( JSON:reader -- JSON:reader JSON:reader ) dup" JRT-REJECTED
   s" linear reader cannot be discarded" T-LABEL
   s" JRT-BAD-DROP ( JSON:reader -- ) drop" JRT-REJECTED
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


: JRT-TEST-NEGATIVE ( -- )
   s" a value after the document throws E-JSON-READ-TRAILING" T-LABEL
   [: JRT-BAD-TRAILING ;] E-JSON-READ-TRAILING TTHROWSQ
   s" an unterminated string or a raw control byte throws E-JSON-READ-STRING, a bad escape E-JSON-READ-ESCAPE" T-LABEL
   [: JRT-BAD-UNTERM ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-ESCAPE ;] E-JSON-READ-ESCAPE TTHROWSQ
   [: JRT-BAD-RUN-BS ;] E-JSON-READ-ESCAPE TTHROWSQ
   [: JRT-BAD-RUN-CTRL ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-HEX ;] E-JSON-READ-ESCAPE TTHROWSQ
   s" a lone or unpaired surrogate escape throws E-JSON-READ-SURROGATE" T-LABEL
   [: JRT-BAD-SURROGATE ;] E-JSON-READ-SURROGATE TTHROWSQ
   [: JRT-BAD-LOW-SURROGATE ;] E-JSON-READ-SURROGATE TTHROWSQ
   [: JRT-BAD-SURROGATE-PAIR ;] E-JSON-READ-SURROGATE TTHROWSQ
   s" malformed UTF-8 throws E-JSON-READ-STRING" T-LABEL
   [: JRT-BAD-UTF8-OVERLONG ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-UTF8-STRAY ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-UTF8-TRUNC ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-UTF8-SUR ;] E-JSON-READ-STRING TTHROWSQ
   [: JRT-BAD-UTF8-HIGH ;] E-JSON-READ-STRING TTHROWSQ
   s" a bare word or a trailing comma throws E-JSON-READ-MALFORMED, a missing colon E-JSON-READ-COLON" T-LABEL
   [: JRT-BAD-BAREWORD ;] E-JSON-READ-MALFORMED TTHROWSQ
   [: JRT-BAD-TRAILING-COMMA ;] E-JSON-READ-MALFORMED TTHROWSQ
   [: JRT-BAD-COLON ;] E-JSON-READ-COLON TTHROWSQ
   s" READ-INT past a signed cell throws E-JSON-READ-NUMBER" T-LABEL
   [: JRT-BAD-OVERFLOW ;] E-JSON-READ-NUMBER TTHROWSQ
   s" READ-INT on a string throws E-JSON-READ-STATE" T-LABEL
   [: JRT-BAD-STATE ;] E-JSON-READ-STATE TTHROWSQ ;

: JRT-TRAILING-TEXT$ ( -- str )
   s\" {\"a\":1} x" ;

: JRT-OBJ-COMMA$ ( -- str )
   s\" {\"a\":1,\"b\":2,}" ;

\ Finishes inside the first member's array, two containers deep.
: JRT-FINISH-NESTED ( str -- )
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
   s" a failed advance leaves the previous token whole" T-LABEL
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

: JRT-FINISH-AFTER-OPEN ( str -- )
   JRT-OPEN-A JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-FINISH ;

: JRT-FINISH-AFTER-INT ( str -- )
   JRT-OPEN-A JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-NEXT JSON:T-KEY T= JSON:READ-NEXT JSON:T-INT T=
   JSON:READ-FINISH ;

: JRT-TEST-FINISH ( -- )
   s" READ-FINISH closes a document from any depth" T-LABEL
   JRT-NESTED$ JRT-OPEN-A JSON:READ-FINISH
   JRT-NESTED$ JRT-FINISH-NESTED
   JRT-NESTED$ JRT-OPEN-A JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-SKIP-VALUE JSON:READ-FINISH
   JRT-NESTED$ JRT-OPEN-A JSON:READ-NEXT JSON:T-OBJ T= JSON:READ-SKIP-VALUE
   JSON:READ-NEXT JSON:T-END T= JSON:READ-FINISH
   s" READ-FINISH throws a truncated, trailing, malformed or failed document's code" T-LABEL
   [: JRT-NESTED$ str> 1- >str JRT-FINISH-NESTED ;] E-JSON-READ-EOF TTHROWSQ
   [: JRT-TRAILING-TEXT$ JRT-FINISH-AFTER-OPEN ;] E-JSON-READ-TRAILING TTHROWSQ
   [: JRT-OBJ-COMMA$ JRT-FINISH-AFTER-INT ;] E-JSON-READ-MALFORMED TTHROWSQ
   [: JRT-FINISH-AFTER-FAILURE ;] E-JSON-READ-COLON TTHROWSQ ;

: JRT-STEP-STR ( JSON:reader -- JSON:reader )
   JSON:READ-NEXT {: k:n :}
   k JSON:T-STR = k JSON:T-KEY = or if
      JRT-STR-VEC @ JSON:READ-STR str> 2drop
   then ;

\ The code a read of S to T-END throws, decoding each string and key, else 0. S
\ ends at a guard page, so a read past it faults instead of rejecting.
: JRT-GUARDED-CODE ( str -- n ) {: s:str :}
   s STR:LEN 0 GUARD-PAGE:TAIL {: g:^ :}
   s STR:LEN 0 ?do s STR:DATA i + c@ g i + c! loop
   g s STR:LEN >str JRT-OPEN-A
   begin
      ['] JRT-STEP-STR catch {: code:n :}
      code 0<> if JSON:READ-CLOSE code exit then
      JSON:READ-TOKEN JSON:T-END =
   until
   JSON:READ-CLOSE 0 ;

\ The code a rejection letter names: E EOF, S STRING, C COLON, N NUMBER,
\ M MALFORMED, X ESCAPE, U SURROGATE, and . for T-END.
: JRT-REJECTION ( n -- n ) {: c:n :}
   c [char] . = if 0 exit then
   c [char] E = if E-JSON-READ-EOF exit then
   c [char] S = if E-JSON-READ-STRING exit then
   c [char] C = if E-JSON-READ-COLON exit then
   c [char] N = if E-JSON-READ-NUMBER exit then
   c [char] M = if E-JSON-READ-MALFORMED exit then
   c [char] X = if E-JSON-READ-ESCAPE exit then
   c [char] U = if E-JSON-READ-SURROGATE exit then
   s" json-read-test: unknown rejection letter" EXIT-STATUS:FAILURE die ;

\ Every prefix of a document, lengths 0 through u, against one rejection letter
\ per length.
: JRT-PREFIXES ( str str -- ) {: s:str c:str :}
   c STR:LEN s STR:LEN 1+ T=
   c STR:LEN s STR:LEN 1+ min 0 ?do s STR:DATA i >str JRT-GUARDED-CODE c STR:DATA i + c@ JRT-REJECTION T= loop ;

: JRT-TEST-PREFIXES ( -- )
   s" each prefix of a document throws the code its length calls for, never reading past its end" T-LABEL
   s\" {\"k\":\"v\",\"n\":[1,-2.5e+3,true,false,null],\"o\":{}}"
   s" EESSCESSEESSCEEEENENENNEEMMMEEMMMMEEMMMEEESSCEEE." JRT-PREFIXES
   s\" [\"a\\\"b\\\\c\\/d\\be\\ff\\ng\\rh\\ti\"]"
   s" EESSXSSXSSXSSXSSXSSXSSXSSXSSE." JRT-PREFIXES
   s\" \"\\u0041\\u00e9\\u20AC\\uD83D\\uDE00x\""
   s" ESXXXXXSXXXXXSXXXXXSXXXXXUUXXXXSS." JRT-PREFIXES
   s\" [\"\xC3\xA9\xE2\x82\xAC\xF0\x9F\x98\x80z\"]"
   s" EESSSSSSSSSSSE." JRT-PREFIXES ;

\ A number ends at the first byte no number holds; a lexeme its grammar stops
\ short of is E-JSON-READ-NUMBER, never the next token's rejection.
: JRT-TEST-LEXEMES ( -- )
   s" a number ends at the first byte no number holds, and a lexeme cut short throws E-JSON-READ-NUMBER" T-LABEL
   s" 01" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" -01" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" [01]" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" 1-" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" [1+]" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" 1..2" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" 1ee5" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" [1e5e]" JRT-GUARDED-CODE E-JSON-READ-NUMBER T=
   s" [0x1]" JRT-GUARDED-CODE E-JSON-READ-COMMA T= ;

\ A document's first failed advance: its code and its REASON text.
: JRT-WHY ( str n str -- ) {: s:str code:n ws:str :}
   s JRT-GUARDED-CODE code T=
   code ws JRT-SAID ;

: JRT-TEST-REASON-CODES ( -- )
   s" a failed advance throws its code with its REASON text" T-LABEL
   s\" [1,\n ]" E-JSON-READ-MALFORMED
   s" 2:2: expected a value, found `]`; write a value, or remove a trailing comma"
   JRT-WHY
   s\" [\n \"abc" E-JSON-READ-STRING
   s\" 2:6: expected the closing `\"` of the string, found the end of the input; close the string with `\"`"
   JRT-WHY
   s\" [\n \"a\xC3(\"]" E-JSON-READ-STRING
   s" 2:4: expected a UTF-8 character, found byte $C3; write the text in UTF-8"
   JRT-WHY
   s\" {\n \"a\":\"\\q\"}" E-JSON-READ-ESCAPE
   s\" 2:8: expected an escape letter from `\"\\/bfnrt`, or `u` and four hex digits, found `q`; write a valid escape, or `\\\\` for a backslash itself"
   JRT-WHY
   s\" [\n \"\\uDC00x\"]" E-JSON-READ-SURROGATE
   s" 2:3: expected a `\uD800`-`\uDBFF` escape and then a `\uDC00`-`\uDFFF` escape, found `\uDC00x`; write both halves of the surrogate pair, or the character itself in UTF-8"
   JRT-WHY
   s\" \"\\ud800\"" E-JSON-READ-SURROGATE
   s\" 1:8: expected a `\\uD800`-`\\uDBFF` escape and then a `\\uDC00`-`\\uDFFF` escape, found `\"`; write both halves of the surrogate pair, or the character itself in UTF-8"
   JRT-WHY
   s\" [\"\\ud800\\u0041\"]" E-JSON-READ-SURROGATE
   s" 1:9: expected a `\uD800`-`\uDBFF` escape and then a `\uDC00`-`\uDFFF` escape, found `\u0041`; write both halves of the surrogate pair, or the character itself in UTF-8"
   JRT-WHY
   s\" [\"\\ud800\\n\"]" E-JSON-READ-SURROGATE
   s" 1:9: expected a `\uD800`-`\uDBFF` escape and then a `\uDC00`-`\uDFFF` escape, found `\n`; write both halves of the surrogate pair, or the character itself in UTF-8"
   JRT-WHY
   s\" {\"a\":1}\n x" E-JSON-READ-TRAILING
   s" 2:2: expected the end of the input after the top-level value, found `x`; remove the text after the value, or put all the values in one array"
   JRT-WHY
   s\" [\n 01]" E-JSON-READ-NUMBER
   s" 2:2: expected a number, found `01`; write a JSON number such as `-12`, `0.5` or `2e-3`: digits after `-`, `.` and `e`, and no leading `0` before more digits"
   JRT-WHY
   s\" {\"a\":\n [1," E-JSON-READ-EOF
   s" 2:5: expected a value, found the end of the input; write the rest of the document, closing each open array with `]` and object with `}`"
   JRT-WHY
   s\" {\n \"a\" 1}" E-JSON-READ-COLON
   s" 2:6: expected `:` after the member name, found `1`; write `:` between the name and its value"
   JRT-WHY
   s\" {\"a\":1\n \"b\":2}" E-JSON-READ-COMMA
   s\" 2:2: expected `,` or the close of the open array or object, found `\"b\"`; write `,` between the values, or close the open array with `]` or object with `}`"
   JRT-WHY
   s\" {\"a\":1\n \"gr\xC3\xB6\xC3\x9Fe\":2}" E-JSON-READ-COMMA
   s\" 2:2: expected `,` or the close of the open array or object, found `\"gr\xC3\xB6\xC3\x9Fe\"`; write `,` between the values, or close the open array with `]` or object with `}`"
   JRT-WHY
   s\" {\"a\":1\n \"\xC3\xA9aaaaaaaaaaaaaaaaaaaaaaaaaaaa\xC3\xA9\":2}" E-JSON-READ-COMMA
   s\" 2:2: expected `,` or the close of the open array or object, found `\"\xC3\xA9aaaaaaaaaaaaaaaaaaaaaaaaaaaa...`; write `,` between the values, or close the open array with `]` or object with `}`"
   JRT-WHY
   s\" {\"a\":\n }" E-JSON-READ-MALFORMED
   s" 2:2: expected a value, found `}`; write the member's value"
   JRT-WHY
   s\" \n ]" E-JSON-READ-MALFORMED
   s" 2:2: expected a value, found `]`; write a JSON value"
   JRT-WHY
   s" {" E-JSON-READ-EOF
   s" 1:2: expected a member name or `}`, found the end of the input; write the rest of the document, closing each open array with `]` and object with `}`"
   JRT-WHY
   s\" {\"a\":1,}" E-JSON-READ-MALFORMED
   s" 1:8: expected a member name, found `}`; write the next member's name in double quotes, or remove a trailing comma"
   JRT-WHY ;

: JRT-TEST-REASON-FOUND ( -- )
   s" a rejection's found text shows the token it rejected" T-LABEL
   s\" [\n tru]" E-JSON-READ-MALFORMED
   s" 2:2: expected `true`, found `tru`; write the word in full and in lower case, or quote the text as a string"
   JRT-WHY
   s\" {\"a\":\n abc}" E-JSON-READ-MALFORMED
   s" 2:2: expected a value, found `abc`; write a value: an object, an array, a string in double quotes, a number, `true`, `false` or `null`"
   JRT-WHY
   s\" [\n abcdefghijklmnopqrstuvwxyz0123456789]" E-JSON-READ-MALFORMED
   s" 2:2: expected a value or `]`, found `abcdefghijklmnopqrstuvwxyz012345...`; write a value, or close the array with `]`"
   JRT-WHY
   s\" [caf\xC3\xA9]" E-JSON-READ-MALFORMED
   s\" 1:2: expected a value or `]`, found `caf\xC3\xA9`; write a value, or close the array with `]`"
   JRT-WHY
   s\" [\xC3\xA9crit]" E-JSON-READ-MALFORMED
   s\" 1:2: expected a value or `]`, found `\xC3\xA9crit`; write a value, or close the array with `]`"
   JRT-WHY
   s\" [abcdefghijklmnopqrstuvwxyz01234\xC3\xA9xyz]" E-JSON-READ-MALFORMED
   s" 1:2: expected a value or `]`, found `abcdefghijklmnopqrstuvwxyz01234...`; write a value, or close the array with `]`"
   JRT-WHY
   s\" [a\xE2\x80\xAEb\xC2\x85c\xF3\xA0\x80\x81d\xE2\x80\xA8e]" E-JSON-READ-MALFORMED
   s" 1:2: expected a value or `]`, found `aU+202EbU+0085cU+E0001dU+2028e`; write a value, or close the array with `]`"
   JRT-WHY
   s\" {\"a\":1\n \"x\xE2\x80\xAEy\xC2\x85z\":2}" E-JSON-READ-COMMA
   s\" 2:2: expected `,` or the close of the open array or object, found `\"xU+202EyU+0085z\"`; write `,` between the values, or close the open array with `]` or object with `}`"
   JRT-WHY
   s\" [\n \"a\tb\"]" E-JSON-READ-STRING
   s" 2:4: expected a character or an escape, found byte $09; write the control byte as an escape such as `\t`, `\n` or `\u001F`"
   JRT-WHY
   s\" {\"\xC3\xA9\":1,\n \"\xC3\xBC\" 2}" E-JSON-READ-COLON
   s" 2:7: expected `:` after the member name, found `2`; write `:` between the name and its value"
   JRT-WHY ;

: JRT-TRAILING-COMMA-WHY$ ( -- str )
   s" 1:4: expected a value, found `]`; write a value, or remove a trailing comma" ;

: JRT-OTHER-REASON ( -- )
   [: s" another reader's text" E-JSON-READ-MALFORMED REASON:THROW ;]
   E-JSON-READ-MALFORMED TTHROWSQ ;

\ A failed reader advanced again throws its own text, whatever text another
\ throw of the same code left since.
: JRT-TEST-REASON-AGAIN ( -- )
   s" a failed reader advanced again throws its own text" T-LABEL
   s" [1,]" JRT-OPEN-A
   JSON:READ-NEXT drop JSON:READ-NEXT drop
   JRT-CATCH-BAD
   JRT-OTHER-REASON
   JRT-CATCH-BAD
   E-JSON-READ-MALFORMED JRT-TRAILING-COMMA-WHY$ JRT-SAID
   JSON:READ-CLOSE ;

: JRT-THIRD ( str -- JSON:reader )
   JRT-OPEN-A JSON:READ-NEXT drop JSON:READ-NEXT drop JSON:READ-NEXT drop ;

\ An accessor rejects the token it stands on, a column counting bytes.
: JRT-TEST-REASON-ACCESSORS ( -- )
   s" READ-U64 rejects a negative integer at its token" T-LABEL
   [: s\" [\"\xC3\xA9\", -1]" JRT-THIRD JSON:READ-U64 drop JSON:READ-CLOSE ;]
   E-JSON-READ-NUMBER TTHROWSQ
   E-JSON-READ-NUMBER
   s" 1:8: expected an integer from 0 to 18446744073709551615, found `-1`; write digits alone, with no sign, fraction or exponent, or read the number with READ-FLOAT"
   JRT-SAID
   s" READ-INT rejects a string at its opening quote" T-LABEL
   [: s\" [1,\n \"x\"]" JRT-THIRD JSON:READ-INT drop JSON:READ-CLOSE ;]
   E-JSON-READ-STATE TTHROWSQ
   E-JSON-READ-STATE
   s\" 2:2: expected an integer, found `\"x\"`; write that kind of value here, or read this token with the word for its kind"
   JRT-SAID ;

: JRT-MEMBER-SAYS ( str str -- ) {: s:str ws:str :}
   s JRT-NO-MEMBER
   JSON:E-MEMBER ws JRT-SAID ;

: JRT-TEST-REASON-MEMBER ( -- )
   s" READ-MEMBER$ rejects a text that is no object at its first token" T-LABEL
   s\" [{\"a\":1}]"
   s" 1:1: expected an object, found `[`; write the text as one JSON object holding the member"
   JRT-MEMBER-SAYS
   s" READ-MEMBER$ rejects a missing member at the object's close" T-LABEL
   s\" {\"b\":1}"
   s" 1:7: expected the member the key names, found `}`; add the member to the object"
   JRT-MEMBER-SAYS
   s" READ-MEMBER$ rejects a repeated member at its second value" T-LABEL
   s\" {\"a\":1,\n \"a\":[2]}"
   s" 2:6: expected the member once, found `[`; remove the repeated member, keeping one"
   JRT-MEMBER-SAYS ;

variable JRT-ORDER      \ the indices handlers saw in call order, a digit each, plus one
variable JRT-COUNT

: JRT-SAW ( n -- )
   1+ JRT-ORDER @ 10 * + JRT-ORDER ! ;

: JRT-FIRST ( str -- JSON:reader )
   JRT-OPEN-A JSON:READ-NEXT drop ;

TYPED-VARIABLE JRT-SEEN ^VEC:vector<u8>
TYPED-VARIABLE JRT-INNER-SEEN ^VEC:vector<u8>   \ the nested object's, while JRT-SEEN is in use

: JRT-UNSEEN ( -- )  JRT-SEEN @ 0 VEC:RESIZE ;

\ Reads every listed member with READ-LISTED, seen emptied first, calling the
\ handler on each member's value with its index.
: JRT-LISTED-EACH
   ( JSON:reader str ^VEC:vector<u8> [ JSON:reader n -- JSON:reader ] -- JSON:reader )
   {: names:str seen:^ h :}
   seen 0 VEC:RESIZE
   begin names seen JSON:READ-LISTED dup -1 <> while h execute repeat drop ;

: JRT-TLS-H ( JSON:reader n -- JSON:reader ) {: idx:n :}
   idx JRT-SAW
   idx 0 = if s" k" JSON:READ-STR-EQ? TTRUE exit then
   JSON:READ-TOKEN JSON:T-TRUE T= ;

: JRT-TOP-H ( JSON:reader n -- JSON:reader ) {: idx:n :}
   idx JRT-SAW
   idx 0 = if s" a" JSON:READ-STR-EQ? TTRUE exit then
   idx 1 = if JSON:READ-INT 8080 T= exit then
   s" key on" JRT-INNER-SEEN @ [: JRT-TLS-H ;] JRT-LISTED-EACH ;

: JRT-TOP-NAMES$ ( -- str ) s" host port tls" ;

: JRT-SKIP-H ( JSON:reader n -- JSON:reader )
   drop JSON:READ-SKIP-VALUE ;

: JRT-PORT-H ( JSON:reader n -- JSON:reader )
   1 = if
      s" a port from 1 to 65535"
      s" write the port as an integer from 1 to 65535"
      JSON:READ-REJECT
   then
   JSON:READ-SKIP-VALUE ;

\ READ-LISTED over the text, with the names and the handler, throws E-MEMBER
\ with the text wa wu.
: JRT-LISTED-H-SAY
   ( str str [ JSON:reader n -- JSON:reader ] str -- )
   {: t:str ns:str h ws:str :}
   [: t JRT-FIRST ns JRT-SEEN @ h JRT-LISTED-EACH JSON:READ-FINISH ;]
   JSON:E-MEMBER TTHROWSQ
   JSON:E-MEMBER ws JRT-SAID ;

\ JRT-LISTED-H-SAY, skipping each value.
: JRT-LISTED-SAY ( str str str -- )
   {: s:str ns:str ws:str :}
   s ns [: JRT-SKIP-H ;] ws JRT-LISTED-H-SAY ;

: JRT-PORT-HOST$ ( -- str ) s" host port?" ;

: JRT-TEST-LISTED-READ ( -- )
   s" READ-LISTED answers each listed member's index, then -1 at the `}`" T-LABEL
   JRT-UNSEEN
   s\" {\"port\":1,\"host\":\"a\"}" JRT-FIRST
   JRT-PORT-HOST$ JRT-SEEN @ JSON:READ-LISTED 1 T=
   JSON:READ-INT 1 T=
   JRT-PORT-HOST$ JRT-SEEN @ JSON:READ-LISTED 0 T=
   s" a" JSON:READ-STR-EQ? TTRUE
   JRT-PORT-HOST$ JRT-SEEN @ JSON:READ-LISTED -1 T=
   JSON:READ-TOKEN JSON:T-OBJ-END T=
   JSON:READ-FINISH
   s" READ-LISTED reads an object without an optional member" T-LABEL
   JRT-UNSEEN
   s\" {\"host\":\"a\"}" JRT-FIRST
   JRT-PORT-HOST$ JRT-SEEN @ JSON:READ-LISTED 0 T=
   JRT-PORT-HOST$ JRT-SEEN @ JSON:READ-LISTED -1 T=
   JSON:READ-TOKEN JSON:T-OBJ-END T=
   JSON:READ-FINISH
   s" READ-LISTED reads each listed member once, in any order, and a nested object" T-LABEL
   0 JRT-ORDER !
   s\" {\"port\":8080,\"host\":\"a\",\"tls\":{\"on\":true,\"key\":\"k\"}}"
   JRT-FIRST JRT-TOP-NAMES$ JRT-SEEN @ [: JRT-TOP-H ;] JRT-LISTED-EACH
   JSON:READ-TOKEN JSON:T-OBJ-END T=
   JSON:READ-FINISH
   JRT-ORDER @ 21321 T= ;

: JRT-BAD-LISTED-KEY ( -- )
   JRT-UNSEEN
   s\" {\"a\":1,\"b\":2}" JRT-FIRST
   s" a b" JRT-SEEN @ JSON:READ-LISTED drop
   JSON:READ-NEXT drop
   s" a b" JRT-SEEN @ JSON:READ-LISTED drop JSON:READ-CLOSE ;

: JRT-BAD-LISTED-INNER ( -- )
   JRT-UNSEEN
   s\" {\"a\":{\"x\":1,\"b\":2}}" JRT-FIRST
   s" a b" JRT-SEEN @ JSON:READ-LISTED drop
   JSON:READ-NEXT drop JSON:READ-NEXT drop
   s" a b" JRT-SEEN @ JSON:READ-LISTED drop JSON:READ-CLOSE ;

: JRT-TEST-LISTED-REJECTED ( -- )
   s" READ-LISTED rejects an unknown key, listing the names" T-LABEL
   s\" {\"host\":\"a\",\n \"colour\":1}" s" host user system"
   s\" 2:2: expected one of `host`, `user`, `system`, found `\"colour\"`; rename the member to one of those, or remove it"
   JRT-LISTED-SAY
   s" READ-LISTED rejects a repeated key at its second key" T-LABEL
   s\" {\"host\":\"a\",\"host\":\"b\"}" s" host"
   s\" 1:13: expected each member once, found `\"host\"`; remove the repeated member, keeping one"
   JRT-LISTED-SAY
   s" READ-LISTED rejects a missing member at the object's close" T-LABEL
   s\" {\"host\":\"a\"}" s" host user"
   s" 1:12: expected the member `user`, found `}`; add the member to the object"
   JRT-LISTED-SAY
   s" READ-LISTED rejects a value that is no object at its first token" T-LABEL
   s" [1]" s" host"
   s" 1:1: expected an object, found `[`; write an object here"
   JRT-LISTED-SAY
   s" READ-LISTED rejects a call with the reader on the next member's key" T-LABEL
   [: JRT-BAD-LISTED-KEY ;] E-JSON-READ-STATE TTHROWSQ
   E-JSON-READ-STATE
   s\" 1:8: expected the last token of the member's value, found `\"b\"`; leave the reader on the value's last token, with READ-SKIP-VALUE for a value not read"
   JRT-SAID
   s" READ-LISTED rejects a call from inside a member's object" T-LABEL
   [: JRT-BAD-LISTED-INNER ;] E-JSON-READ-STATE TTHROWSQ
   E-JSON-READ-STATE
   s" 1:11: expected the last token of the member's value, found `1`; leave the reader on the value's last token, with READ-SKIP-VALUE for a value not read"
   JRT-SAID ;

: JRT-TEST-LISTED-HANDLER ( -- )
   s" READ-REJECT rejects a READ-LISTED member's value at its token" T-LABEL
   s\" {\"host\":\"a\",\n \"port\":70000}" s" host port" [: JRT-PORT-H ;]
   s" 2:9: expected a port from 1 to 65535, found `70000`; write the port as an integer from 1 to 65535"
   JRT-LISTED-H-SAY
   s" a nested READ-LISTED rejects its own unknown key at its place" T-LABEL
   s\" {\"host\":\"a\",\n \"tls\":{\"on\":true,\"pin\":1}}"
   JRT-TOP-NAMES$ [: JRT-TOP-H ;]
   s\" 2:19: expected one of `key`, `on`, found `\"pin\"`; rename the member to one of those, or remove it"
   JRT-LISTED-H-SAY ;

\ Unnamed members first, between and last: an object holding listed names,
\ an array, a repeated key and the key `*`.
: JRT-WILD$ ( -- str )
   s\" {\"x\":{\"host\":[1,{\"port\":2}]},\"host\":\"a\",\"y\":[1,[2],{\"c\":3}],\"port\":8080,\"x\":true,\"*\":\"q\"}" ;

: JRT-TEST-LISTED-WILD ( -- )
   s" READ-LISTED under `*` skips each unnamed member wherever it sits" T-LABEL
   0 JRT-ORDER !
   JRT-WILD$ JRT-FIRST s" host port *" JRT-SEEN @ [: JRT-TOP-H ;] JRT-LISTED-EACH
   JSON:READ-TOKEN JSON:T-OBJ-END T=
   JSON:READ-FINISH
   JRT-ORDER @ 12 T=
   s" READ-LISTED under `*` rejects a repeated listed key at its second key" T-LABEL
   s\" {\"host\":\"a\",\"x\":1,\"host\":\"b\"}" s" host *"
   s\" 1:19: expected each member once, found `\"host\"`; remove the repeated member, keeping one"
   JRT-LISTED-SAY
   s" READ-LISTED under `*` rejects a missing member at the object's close" T-LABEL
   s\" {\"x\":1,\"host\":\"a\"}" s" host user *"
   s" 1:18: expected the member `user`, found `}`; add the member to the object"
   JRT-LISTED-SAY
   s" READ-LISTED over `*` alone skips every member, answering only -1" T-LABEL
   0 JRT-ORDER !
   JRT-WILD$ JRT-FIRST s" *" JRT-SEEN @ [: JRT-TOP-H ;] JRT-LISTED-EACH
   JSON:READ-TOKEN JSON:T-OBJ-END T=
   JSON:READ-FINISH
   JRT-ORDER @ 0 T= ;

create JRT-NAMES 400 allot

: JRT-NAME-PUT ( n str -- n ) {: at:n s:str :}
   s STR:DATA JRT-NAMES at + s STR:LEN BYTE-COPY at s STR:LEN + ;

: JRT-HUNDRED-NAME ( n n -- n ) {: at:n idx:n :}
   at idx 0<> if s"  " JRT-NAME-PUT then
   s" m" JRT-NAME-PUT idx FMT:INT$ JRT-NAME-PUT ;

\ m0 m1 ... m99
: JRT-HUNDRED-NAMES$ ( -- str )
   0 100 0 ?do i JRT-HUNDRED-NAME loop JRT-NAMES swap >str ;

: JRT-HUNDRED-MEMBER ( n -- ) {: idx:n :}
   JRT-DQ SB-APPEND-C s" m" SB-APPEND idx FMT:INT$ SB-APPEND
   JRT-DQ SB-APPEND-C JRT-COLON SB-APPEND-C idx FMT:INT$ SB-APPEND ;

\ {"m99":99,...,"m0":0}
: JRT-HUNDRED$ ( -- str )
   SB-RESET JRT-LBRACE SB-APPEND-C
   100 0 ?do
      i 0<> if JRT-COMMA SB-APPEND-C then
      99 i - JRT-HUNDRED-MEMBER
   loop
   JRT-RBRACE SB-APPEND-C SB$ ;

: JRT-HUNDRED-H ( JSON:reader n -- JSON:reader ) {: idx:n :}
   JSON:READ-INT idx T=
   JRT-COUNT @ 1+ JRT-COUNT ! ;

: JRT-TEST-LISTED-HUNDRED ( -- )
   s" READ-LISTED reads every member of a list of 100 names" T-LABEL
   0 JRT-COUNT !
   JRT-HUNDRED$ JRT-FIRST
   JRT-HUNDRED-NAMES$ JRT-SEEN @ [: JRT-HUNDRED-H ;] JRT-LISTED-EACH
   JSON:READ-FINISH
   JRT-COUNT @ 100 T= ;

: JRT-TEST-LISTED-OPTIONAL ( -- )
   s" READ-LISTED rejects an optional member repeated at its second key" T-LABEL
   s\" {\"port\":1,\"port\":2}" s" host? port?"
   s\" 1:11: expected each member once, found `\"port\"`; remove the repeated member, keeping one"
   JRT-LISTED-SAY
   s" READ-LISTED lists an optional name without its `?`" T-LABEL
   s\" {\"colour\":1}" JRT-PORT-HOST$
   s\" 1:2: expected one of `host`, `port`, found `\"colour\"`; rename the member to one of those, or remove it"
   JRT-LISTED-SAY ;

\ READ-ANY-SPAN$ of the value the reader is on, rewound and read again to the
\ same span.
: JRT-REWOUND ( JSON:reader -- JSON:reader )
   JSON:READ-ANY-SPAN$ {: s:str :}
   s JSON:READ-REWIND
   JSON:READ-ANY-SPAN$ {: s2:str :}
   s2 STR:DATA s STR:DATA - 0 T=
   s2 STR:LEN s STR:LEN T= ;

: JRT-BAD-REWIND-SPAN ( -- )
   s\" {\"a\":[1],\"b\":2}" JRT-FIRST
   s" a" JSON:READ-FIND-KEY drop
   JSON:READ-ANY-SPAN$ {: s:str :}
   s" b" JSON:READ-FIND-KEY drop
   s JSON:READ-REWIND JSON:READ-CLOSE ;

: JRT-BAD-REWIND-END ( -- )
   s" [1]" JRT-FIRST
   JSON:READ-ANY-SPAN$ {: s:str :}
   JSON:READ-NEXT JSON:T-END T=
   s JSON:READ-REWIND JSON:READ-CLOSE ;

: JRT-TEST-REWIND ( -- )
   s" READ-REWIND reads a nested object and a string again to the same span" T-LABEL
   s\" {\"a\":{\"b\":[1,{}]},\"c\":\"x\\\"y\"}" JRT-FIRST
   s" a" JSON:READ-FIND-KEY TTRUE
   JRT-REWOUND
   JSON:READ-TOKEN JSON:T-OBJ-END T=
   s" c" JSON:READ-FIND-KEY TTRUE
   JRT-REWOUND
   JSON:READ-TOKEN JSON:T-STR T=
   JSON:READ-NEXT JSON:T-OBJ-END T=
   JSON:READ-FINISH
   s" READ-REWIND rejects a span that does not end at the reader" T-LABEL
   [: JRT-BAD-REWIND-SPAN ;] E-JSON-READ-STATE TTHROWSQ
   E-JSON-READ-STATE
   s" 1:14: expected the span of the value the reader just read, found `2`; pass READ-ANY-SPAN$'s answer for that value before the reader moves past it"
   JRT-SAID
   s" READ-REWIND reads a top-level array again to the same span" T-LABEL
   s" [1,2]" JRT-FIRST
   JRT-REWOUND
   JSON:READ-TOKEN JSON:T-ARR-END T=
   JSON:READ-NEXT JSON:T-END T=
   JSON:READ-FINISH
   s" READ-REWIND rejects a top-level value once the text's end was read" T-LABEL
   [: JRT-BAD-REWIND-END ;] E-JSON-READ-STATE TTHROWSQ
   E-JSON-READ-STATE
   s" 1:4: expected the span of the value the reader just read, found the end of the input; pass READ-ANY-SPAN$'s answer for that value before the reader moves past it"
   JRT-SAID ;

: JRT-TEST-READ-LISTED ( -- )
   JRT-TEST-LISTED-READ
   JRT-TEST-LISTED-REJECTED
   JRT-TEST-LISTED-HANDLER
   JRT-TEST-LISTED-HUNDRED
   JRT-TEST-LISTED-OPTIONAL
   JRT-TEST-LISTED-WILD
   JRT-TEST-REWIND ;

: JRT-TEST-REASON-WHOLE ( -- )
   s" READ-REPEAT rejects a text that is no object at its first token" T-LABEL
   [: s\" \n \"a\"" JSON:READ-REPEAT drop ;] E-JSON-READ-STATE TTHROWSQ
   E-JSON-READ-STATE
   s\" 2:2: expected an object, found `\"a\"`; write the text as one JSON object"
   JRT-SAID
   s" VALUE= rejects an exponent no cell holds at its number" T-LABEL
   [: s\" [1,\n 1e99999999999999999999]" s" [1,1]" JSON:VALUE= drop ;]
   E-JSON-READ-NUMBER TTHROWSQ
   E-JSON-READ-NUMBER
   s" 2:2: expected a number whose exponent a signed cell holds, found `1e99999999999999999999`; write the number with an exponent nearer zero"
   JRT-SAID
   [: s" [1,1]" s\" [1,\n  1e99999999999999999999]" JSON:VALUE= drop ;]
   E-JSON-READ-NUMBER TTHROWSQ
   E-JSON-READ-NUMBER
   s" 2:3: expected a number whose exponent a signed cell holds, found `1e99999999999999999999`; write the number with an exponent nearer zero"
   JRT-SAID ;

: JRT-CORE ( -- )
   JRT-TEST-INTS
   JRT-TEST-U64
   JRT-TEST-U64-REJECTIONS
   JRT-TEST-FLOATS
   JRT-TEST-LITERALS
   JRT-TEST-STRING
   JRT-TEST-ESCAPES
   JRT-TEST-STR-APPEND
   JRT-TEST-STR-APPEND-GROW
   JRT-TEST-STR-APPEND-KEY
   JRT-TEST-STR-APPEND-MISUSE
   JRT-TEST-UESCAPE
   JRT-TEST-SURROGATE
   JRT-TEST-UTF8
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
   JRT-TEST-COPY-SCALARS
   JRT-TEST-COPY-DEEP
   JRT-TEST-FIND-HIT
   JRT-TEST-FIND-SKIP
   JRT-TEST-FIND-MISS
   JRT-TEST-FIND-NESTED-SAME-NAME
   JRT-TEST-FIND-LENGTHS
   JRT-TEST-FIND-ESCAPED
   JRT-TEST-FIND-ANCHOR
   JRT-TEST-FIND-MISUSE
   JRT-TEST-REPEATS
   JRT-TEST-MEMBER-FOUND
   JRT-TEST-MEMBER-REJECTED
   JRT-TEST-MEMBER-REUSE
   JRT-TEST-CODES
   JRT-TEST-VALUE-EQ-SCALARS
   JRT-TEST-NUMBER-COMPARE
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
   JRT-TEST-LEXEMES
   JRT-TEST-REASON-CODES
   JRT-TEST-REASON-FOUND
   JRT-TEST-REASON-AGAIN
   JRT-TEST-REASON-ACCESSORS
   JRT-TEST-REASON-MEMBER
   JRT-TEST-READ-LISTED
   JRT-TEST-REASON-WHOLE ;

public

: RUN ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW JRT-WRITE-OUT !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW JRT-STR-VEC !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW JRT-SEEN !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW JRT-INNER-SEEN !
   JRT-CORE
   JRT-INNER-SEEN @ VEC:DELETE  JRT-SEEN @ VEC:DELETE
   JRT-STR-VEC @ VEC:DELETE  JRT-WRITE-OUT @ VEC:DELETE ;

;package

T-RESET
JSON-TEST:RUN
T-REPORT
s" json-read-test: ok" type cr
