\ json.f - bounded JSON/JSONL parser and compact writer for Habu tools.
\ Load directly with bin/hb. Parser failures throw named JSON errors.

-7100 constant E-JSON-SYNTAX
-7101 constant E-JSON-CAPACITY
-7102 constant E-JSON-TYPE

0 constant J-NULL
1 constant J-BOOL
2 constant J-NUM
3 constant J-STR
4 constant J-ARR
5 constant J-OBJ

8 constant J-BS
9 constant J-TAB
10 constant J-LF
12 constant J-FF
13 constant J-CR
32 constant J-SP
34 constant J-DQ
44 constant J-COMMA
45 constant J-MINUS
46 constant J-DOT
47 constant J-SLASH
48 constant J-ZERO
58 constant J-COLON
91 constant J-LBRACK
92 constant J-BACKSLASH
93 constant J-RBRACK
123 constant J-LBRACE
125 constant J-RBRACE

$400 constant JSON-MAX-NODES
$800 constant JSON-MAX-ITEMS
$800 constant JSON-MAX-PAIRS
$8000 constant JSON-STR-BOOT-CAP
$8000 constant JSON-OUT-CAP
$10000 constant JSON-STR-GRAIN
$7FFFFFFFFFFFFFFF constant JSON-MAX-N
JSON-MAX-N JSON-STR-GRAIN / constant JSON-MAX-STR-GRAINS
0 constant JSON-MMAP-ANY
3 constant JSON-MMAP-RW
$1002 constant JSON-MMAP-PRIVATE-ANON
-1 constant JSON-MMAP-FD
0 constant JSON-MMAP-OFF
64 constant JSON-MAX-DEPTH
128 constant JSON-ERR-CAP

create J-KINDS JSON-MAX-NODES cells allot
create J-VALS  JSON-MAX-NODES cells allot
create J-OFFS  JSON-MAX-NODES cells allot
create J-LENS  JSON-MAX-NODES cells allot
create J-START JSON-MAX-NODES cells allot
create J-COUNT JSON-MAX-NODES cells allot

create J-ITEMS JSON-MAX-ITEMS cells allot
create J-ITEM-NEXT JSON-MAX-ITEMS cells allot
create J-KEY-OFF JSON-MAX-PAIRS cells allot
create J-KEY-LEN JSON-MAX-PAIRS cells allot
create J-PAIR-VAL JSON-MAX-PAIRS cells allot
create J-PAIR-NEXT JSON-MAX-PAIRS cells allot

create JSON-STR-BOOT JSON-STR-BOOT-CAP allot
create JSON-OUT-BUF JSON-OUT-CAP allot
create JSON-ERR-BUF JSON-ERR-CAP allot

variable JSON-NODES
variable JSON-ITEMS
variable JSON-PAIRS
variable JSON-STR-LEN
variable JSON-OUT-LEN
variable JSON-ERR-LEN
variable JSON-ERR-POS
variable JSON-STR-P
variable JSON-STR-CAP-U

variable JSON-A
variable JSON-U
variable JSON-I
variable JSON-DEPTH
variable JSON-TMP
variable JSON-TMP2
variable JSON-START
variable JSON-CNT
variable JSON-VN
variable JSON-KO
variable JSON-KL
variable JSON-NODE
variable JSON-UHI
variable JSON-ULO

variable JSON-GI
variable JSON-GOT
variable JSON-GKA
variable JSON-GKL
variable JSON-GVAL

variable JSONL-A
variable JSONL-U
variable JSONL-I
variable JSONL-LA
variable JSONL-LU
variable JSONL-ROOT
variable JSONL-SKIPS
variable JSONL-MODE
variable JSONL-LINE-N
variable JSON-PARSE-TRY-ROOT

0 constant JSONL-MODE-STRICT
1 constant JSONL-MODE-SKIP
0 constant JSONL-ROW-JSON
1 constant JSONL-ROW-BLANK
2 constant JSONL-ROW-ERROR
3 constant JSONL-ROW-EOF
0 constant JSON-PARSE-OK
1 constant JSON-PARSE-THROW
: JSON-A@ JSON-A @ ;
s" JSON-A@" s" -- ptr u8" TRUST
: JSON-GKA@ JSON-GKA @ ;
s" JSON-GKA@" s" -- ptr u8" TRUST
: JSONL-A@ JSONL-A @ ;
s" JSONL-A@" s" -- ptr u8" TRUST
: JSONL-LA@ JSONL-LA @ ;
s" JSONL-LA@" s" -- ptr u8" TRUST
: JSON-STR-BUF JSON-STR-P @ ;
s" JSON-STR-BUF" s" -- ptr u8" TRUST

JSON-STR-BOOT JSON-STR-P !
JSON-STR-BOOT-CAP JSON-STR-CAP-U !

: JSON-COPY ( ptr u8 i64 ptr u8 -- )
   {: a:ptr u dst:ptr :}
   0 begin dup u < while
      dup a + c@  over dst + c!
      1+
   repeat drop ;

: JSON-STR= ( ptr u8 i64 ptr u8 i64 -- bool )
   {: a:ptr u b:ptr v :}
   u v <> IF 0 0= 0= exit THEN
   0 begin dup u < while
      dup a + c@  over b + c@  <> IF drop 0 0= 0= exit THEN
      1+
   repeat drop  0 0= ;

: JSON-SET-ERROR ( ptr u8 i64 -- )
   {: a:ptr u :}
   u JSON-ERR-CAP > IF s" json: error message overflow" 76 die THEN
   a u JSON-ERR-BUF JSON-COPY
   u JSON-ERR-LEN ! ;

: JSON-ERROR$ ( -- ptr u8 i64 )
   JSON-ERR-BUF JSON-ERR-LEN @ ;

: JSON-FAIL ( ptr u8 i64 i64 -- )
   {: a u code :}
   a u JSON-SET-ERROR
   JSON-I @ JSON-ERR-POS !
   code throw ;

: JSON-SYNTAX ( ptr u8 i64 -- )
   E-JSON-SYNTAX JSON-FAIL ;

: JSON-CAPACITY ( ptr u8 i64 -- )
   E-JSON-CAPACITY JSON-FAIL ;

: JSON-TYPE-ERROR ( ptr u8 i64 -- )
   E-JSON-TYPE JSON-FAIL ;

: JSON-STR-CAP ( -- n )
   JSON-STR-CAP-U @ ;

: JSON-CHECK-STR-NEED ( n -- )
   dup 0 < IF s" json: string buffer full" JSON-CAPACITY THEN
   dup JSON-MAX-N > IF s" json: string buffer full" JSON-CAPACITY THEN
   drop ;

: JSON-STR-AT-LEAST-ONE ( n -- n )
   dup 1 < IF drop 1 THEN ;

: JSON-STR-GRAINS ( n -- n ) {: need :}
   need JSON-CHECK-STR-NEED
   need JSON-STR-AT-LEAST-ONE 1 - JSON-STR-GRAIN / 1 + dup JSON-MAX-STR-GRAINS > IF
      s" json: string buffer full" JSON-CAPACITY
   THEN ;

: JSON-STR-SPAN ( n -- n )
   JSON-STR-GRAINS JSON-STR-GRAIN * ;

TRUSTED: JSON-ALLOC-STR-PTR ( n -- ptr u8 )
   JSON-MMAP-ANY swap JSON-MMAP-RW JSON-MMAP-PRIVATE-ANON JSON-MMAP-FD JSON-MMAP-OFF mmap
   dup 0 < IF s" json: string buffer mmap failed" JSON-CAPACITY THEN ;

: JSON-COPY-STR-OLD ( ptr u8 -- ) {: dst:ptr :}
   JSON-STR-BUF JSON-STR-LEN @ dst JSON-COPY ;

: JSON-GROW-STR ( n -- ) {: need :}
   need JSON-STR-SPAN {: cap :}
   cap JSON-ALLOC-STR-PTR {: dst:ptr :}
   dst JSON-COPY-STR-OLD
   dst JSON-STR-P !
   cap JSON-STR-CAP-U ! ;

: JSON-ENSURE-STR ( n -- ) {: need :}
   need JSON-CHECK-STR-NEED
   need JSON-STR-CAP <= IF exit THEN
   need JSON-GROW-STR ;

: JSON-RESET ( -- )
   0 JSON-NODES !
   0 JSON-ITEMS !
   0 JSON-PAIRS !
   0 JSON-STR-LEN !
   0 JSON-OUT-LEN !
   0 JSON-ERR-LEN !
   0 JSON-ERR-POS !
   0 JSON-DEPTH ! ;

: J-CELL@ ( i64 ptr n -- i64 )
   {: n base:ptr :}
   base n cells + @ ;

: J-CELL! ( i64 i64 ptr n -- )
   {: x n base:ptr :}
   x base n cells + ! ;

: J-KIND@ ( i64 -- i64 )  J-KINDS J-CELL@ ;
: J-VAL@  ( i64 -- i64 )  J-VALS J-CELL@ ;
: J-OFF@  ( i64 -- i64 )  J-OFFS J-CELL@ ;
: J-LEN@  ( i64 -- i64 )  J-LENS J-CELL@ ;
: J-START@ ( i64 -- i64 ) J-START J-CELL@ ;
: J-COUNT@ ( i64 -- i64 ) J-COUNT J-CELL@ ;

: J-KIND! ( i64 i64 -- ) J-KINDS J-CELL! ;
: J-VAL!  ( i64 i64 -- ) J-VALS J-CELL! ;
: J-OFF!  ( i64 i64 -- ) J-OFFS J-CELL! ;
: J-LEN!  ( i64 i64 -- ) J-LENS J-CELL! ;
: J-START! ( i64 i64 -- ) J-START J-CELL! ;
: J-COUNT! ( i64 i64 -- ) J-COUNT J-CELL! ;

: JSON-NEW ( i64 -- i64 )
   {: kind :}
   JSON-NODES @ JSON-MAX-NODES >= IF s" json: node table full" JSON-CAPACITY THEN
   JSON-NODES @ JSON-NODE !
   JSON-NODES @ 1+ JSON-NODES !
   kind JSON-NODE @ J-KIND!
   0 JSON-NODE @ J-VAL!
   0 JSON-NODE @ J-OFF!
   0 JSON-NODE @ J-LEN!
   0 JSON-NODE @ J-START!
   0 JSON-NODE @ J-COUNT!
   JSON-NODE @ ;

: JSON-STR+ ( n -- )
   {: c :}
   JSON-STR-LEN @ 1+ JSON-ENSURE-STR
   c JSON-STR-BUF JSON-STR-LEN @ + c!
   JSON-STR-LEN @ 1+ JSON-STR-LEN ! ;

: JSON-SAVE$ ( ptr u8 i64 -- i64 i64 )
   {: a:ptr u :}
   JSON-STR-LEN @ JSON-START !
   JSON-STR-LEN @ u + JSON-ENSURE-STR
   a u JSON-STR-BUF JSON-STR-LEN @ + JSON-COPY
   JSON-STR-LEN @ u + JSON-STR-LEN !
   JSON-START @ u ;

: JSON-WHITE? ( n -- bool )
   dup J-SP = over J-TAB = or over J-LF = or swap J-CR = or ;

: JSON-DIGIT? ( n -- bool )
   dup J-ZERO >= swap 57 <= and ;

: JSON-DIGIT1? ( n -- bool )
   dup 49 >= swap 57 <= and ;

: JSON-END? ( -- bool )
   JSON-I @ JSON-U @ >= ;

: JSON-AT ( -- n )
   JSON-END? IF s" json: unexpected end" JSON-SYNTAX THEN
   JSON-A@ JSON-I @ + c@ ;

: JSON-TAKE ( -- n )
   JSON-AT
   JSON-I @ 1+ JSON-I ! ;

: JSON-SKIP-WS ( -- )
   begin JSON-END? 0= while
      JSON-AT JSON-WHITE? IF JSON-I @ 1+ JSON-I ! ELSE exit THEN
   repeat ;

: JSON-EXPECT ( n -- )
   {: c :}
   JSON-TAKE c <> IF s" json: unexpected character" JSON-SYNTAX THEN ;

: JSON-HEX? ( n -- n )
   dup 48 >= over 57 <= and IF 48 - exit THEN
   dup 65 >= over 70 <= and IF 55 - exit THEN
   dup 97 >= over 102 <= and IF 87 - exit THEN
   drop -1 ;

: JSON-HEX-DIGIT ( -- n )
   JSON-TAKE JSON-HEX?
   dup 0< IF s" json: bad unicode escape" JSON-SYNTAX THEN ;

: JSON-HEX4 ( -- n )
   JSON-HEX-DIGIT 16 *
   JSON-HEX-DIGIT +
   16 *
   JSON-HEX-DIGIT +
   16 *
   JSON-HEX-DIGIT + ;

: JSON-UTF8+ ( n -- )
   {: cp :}
   cp $80 < IF cp JSON-STR+ exit THEN
   cp $800 < IF
      cp 6 rshift $C0 or JSON-STR+
      cp $3F and $80 or JSON-STR+
      exit
   THEN
   cp $10000 < IF
      cp 12 rshift $E0 or JSON-STR+
      cp 6 rshift $3F and $80 or JSON-STR+
      cp $3F and $80 or JSON-STR+
      exit
   THEN
   cp $10FFFF > IF s" json: unicode escape out of range" JSON-SYNTAX THEN
   cp 18 rshift $F0 or JSON-STR+
   cp 12 rshift $3F and $80 or JSON-STR+
   cp 6 rshift $3F and $80 or JSON-STR+
   cp $3F and $80 or JSON-STR+ ;

: JSON-UNICODE+ ( -- )
   JSON-HEX4 JSON-UHI !
   JSON-UHI @ $D800 >= JSON-UHI @ $DBFF <= and IF
      JSON-TAKE J-BACKSLASH <> IF s" json: missing low surrogate" JSON-SYNTAX THEN
      JSON-TAKE 117 <> IF s" json: missing low surrogate" JSON-SYNTAX THEN
      JSON-HEX4 JSON-ULO !
      JSON-ULO @ $DC00 < JSON-ULO @ $DFFF > or IF s" json: bad low surrogate" JSON-SYNTAX THEN
      JSON-UHI @ $D800 - 10 lshift
      JSON-ULO @ $DC00 - +
      $10000 +
      JSON-UTF8+
      exit
   THEN
   JSON-UHI @ $DC00 >= JSON-UHI @ $DFFF <= and IF s" json: lone low surrogate" JSON-SYNTAX THEN
   JSON-UHI @ JSON-UTF8+ ;

: JSON-ESCAPE+ ( -- )
   JSON-END? IF s" json: bad string escape" JSON-SYNTAX THEN
   JSON-TAKE JSON-TMP !
   JSON-TMP @ J-DQ = IF J-DQ JSON-STR+ exit THEN
   JSON-TMP @ J-BACKSLASH = IF J-BACKSLASH JSON-STR+ exit THEN
   JSON-TMP @ J-SLASH = IF J-SLASH JSON-STR+ exit THEN
   JSON-TMP @ 98 = IF J-BS JSON-STR+ exit THEN
   JSON-TMP @ 102 = IF J-FF JSON-STR+ exit THEN
   JSON-TMP @ 110 = IF J-LF JSON-STR+ exit THEN
   JSON-TMP @ 114 = IF J-CR JSON-STR+ exit THEN
   JSON-TMP @ 116 = IF J-TAB JSON-STR+ exit THEN
   JSON-TMP @ 117 = IF JSON-UNICODE+ exit THEN
   s" json: bad string escape" JSON-SYNTAX ;

: JSON-PARSE-STRING$ ( -- i64 i64 )
   J-DQ JSON-EXPECT
   JSON-STR-LEN @ JSON-START !
   begin
      JSON-END? IF s" json: unterminated string" JSON-SYNTAX THEN
      JSON-TAKE JSON-TMP !
      JSON-TMP @ J-DQ = IF
         JSON-START @
         JSON-STR-LEN @ JSON-START @ -
         exit
      THEN
      JSON-TMP @ J-BACKSLASH = IF
         JSON-ESCAPE+
      ELSE
         JSON-TMP @ J-SP < IF s" json: control character in string" JSON-SYNTAX THEN
         JSON-TMP @ JSON-STR+
      THEN
   again ;

: JSON-MATCH ( ptr u8 i64 -- )
   {: a:ptr u :}
   JSON-I @ u + JSON-U @ > IF s" json: unexpected end" JSON-SYNTAX THEN
   0 begin dup u < while
      dup a + c@  over JSON-I @ + JSON-A@ + c@  <> IF s" json: bad literal" JSON-SYNTAX THEN
      1+
   repeat drop
   JSON-I @ u + JSON-I ! ;

: JSON-NUM-DIGITS ( -- i64 )
   0 JSON-CNT !
   begin JSON-END? 0= while
      JSON-AT JSON-DIGIT? IF
         JSON-I @ 1+ JSON-I !
         JSON-CNT @ 1+ JSON-CNT !
      ELSE JSON-CNT @ exit THEN
   repeat
   JSON-CNT @ ;

: JSON-PARSE-NUMBER$ ( -- i64 i64 )
   JSON-I @ JSON-START !
   JSON-AT J-MINUS = IF
      JSON-I @ 1+ JSON-I !
      JSON-END? IF s" json: bad number" JSON-SYNTAX THEN
   THEN
   JSON-AT J-ZERO = IF
      JSON-I @ 1+ JSON-I !
      JSON-END? 0= IF
         JSON-AT JSON-DIGIT? IF s" json: leading zero" JSON-SYNTAX THEN
      THEN
   ELSE
      JSON-AT JSON-DIGIT1? 0= IF s" json: bad number" JSON-SYNTAX THEN
      JSON-NUM-DIGITS drop
   THEN
   JSON-END? 0= IF
      JSON-AT J-DOT = IF
         JSON-I @ 1+ JSON-I !
         JSON-NUM-DIGITS 0= IF s" json: bad fraction" JSON-SYNTAX THEN
      THEN
   THEN
   JSON-END? 0= IF
      JSON-AT dup 101 = swap 69 = or IF
         JSON-I @ 1+ JSON-I !
         JSON-END? IF s" json: bad exponent" JSON-SYNTAX THEN
         JSON-AT dup 43 = swap J-MINUS = or IF JSON-I @ 1+ JSON-I ! THEN
         JSON-NUM-DIGITS 0= IF s" json: bad exponent" JSON-SYNTAX THEN
      THEN
   THEN
   JSON-A@ JSON-START @ +  JSON-I @ JSON-START @ -  JSON-SAVE$ ;

: JSON-ITEM-APPEND ( i64 i64 -- )
   {: arr node :}
   JSON-ITEMS @ JSON-MAX-ITEMS >= IF s" json: array item table full" JSON-CAPACITY THEN
   node JSON-ITEMS @ J-ITEMS J-CELL!
   -1 JSON-ITEMS @ J-ITEM-NEXT J-CELL!
   arr J-COUNT@ 0= IF
      JSON-ITEMS @ arr J-START!
   ELSE
      JSON-ITEMS @ arr J-VAL@ J-ITEM-NEXT J-CELL!
   THEN
   JSON-ITEMS @ arr J-VAL!
   arr J-COUNT@ 1+ arr J-COUNT!
   JSON-ITEMS @ 1+ JSON-ITEMS ! ;

: JSON-PAIR-APPEND ( i64 i64 i64 i64 -- )
   {: obj ko kl node :}
   JSON-PAIRS @ JSON-MAX-PAIRS >= IF s" json: object pair table full" JSON-CAPACITY THEN
   ko JSON-PAIRS @ J-KEY-OFF J-CELL!
   kl JSON-PAIRS @ J-KEY-LEN J-CELL!
   node JSON-PAIRS @ J-PAIR-VAL J-CELL!
   -1 JSON-PAIRS @ J-PAIR-NEXT J-CELL!
   obj J-COUNT@ 0= IF
      JSON-PAIRS @ obj J-START!
   ELSE
      JSON-PAIRS @ obj J-VAL@ J-PAIR-NEXT J-CELL!
   THEN
   JSON-PAIRS @ obj J-VAL!
   obj J-COUNT@ 1+ obj J-COUNT!
   JSON-PAIRS @ 1+ JSON-PAIRS ! ;

: JSON-VALUE ( -- i64 )
   JSON-DEPTH @ JSON-MAX-DEPTH >= IF s" json: nesting too deep" JSON-CAPACITY THEN
   JSON-DEPTH @ 1+ JSON-DEPTH !
   JSON-SKIP-WS
   JSON-END? IF s" json: empty value" JSON-SYNTAX THEN
   JSON-AT JSON-TMP !
   JSON-TMP @ J-LBRACE = IF
      JSON-TAKE drop
      J-OBJ JSON-NEW >r
      -1 r@ J-START!
      -1 r@ J-VAL!
      JSON-SKIP-WS
      JSON-END? IF s" json: unterminated object" JSON-SYNTAX THEN
      JSON-AT J-RBRACE = IF
         JSON-TAKE drop
      ELSE
         begin
            JSON-SKIP-WS
            JSON-AT J-DQ <> IF s" json: object key must be string" JSON-SYNTAX THEN
            JSON-PARSE-STRING$ >r >r
            JSON-SKIP-WS
            J-COLON JSON-EXPECT
            RECURSE JSON-NODE !
            r> JSON-KO !
            r> JSON-KL !
            r@ JSON-KO @ JSON-KL @ JSON-NODE @ JSON-PAIR-APPEND
            JSON-SKIP-WS
            JSON-END? IF s" json: unterminated object" JSON-SYNTAX THEN
            JSON-AT J-RBRACE = IF
               JSON-TAKE drop
               0 0= 0=
            ELSE
               J-COMMA JSON-EXPECT
               JSON-SKIP-WS
               JSON-AT J-RBRACE = IF s" json: trailing object comma" JSON-SYNTAX THEN
               0 0=
            THEN
         while repeat
      THEN
      r> JSON-VN !
   ELSE JSON-TMP @ J-LBRACK = IF
      JSON-TAKE drop
      J-ARR JSON-NEW >r
      -1 r@ J-START!
      -1 r@ J-VAL!
      JSON-SKIP-WS
      JSON-END? IF s" json: unterminated array" JSON-SYNTAX THEN
      JSON-AT J-RBRACK = IF
         JSON-TAKE drop
      ELSE
         begin
            RECURSE JSON-NODE !
            r@ JSON-NODE @ JSON-ITEM-APPEND
            JSON-SKIP-WS
            JSON-END? IF s" json: unterminated array" JSON-SYNTAX THEN
            JSON-AT J-RBRACK = IF
               JSON-TAKE drop
               0 0= 0=
            ELSE
               J-COMMA JSON-EXPECT
               JSON-SKIP-WS
               JSON-AT J-RBRACK = IF s" json: trailing array comma" JSON-SYNTAX THEN
               0 0=
            THEN
         while repeat
      THEN
      r> JSON-VN !
   ELSE JSON-TMP @ J-DQ = IF
      JSON-PARSE-STRING$ JSON-TMP2 ! JSON-TMP !
      J-STR JSON-NEW JSON-VN !
      JSON-TMP @ JSON-VN @ J-OFF!
      JSON-TMP2 @ JSON-VN @ J-LEN!
   ELSE JSON-TMP @ J-MINUS = JSON-TMP @ JSON-DIGIT? or IF
      JSON-PARSE-NUMBER$ JSON-TMP2 ! JSON-TMP !
      J-NUM JSON-NEW JSON-VN !
      JSON-TMP @ JSON-VN @ J-OFF!
      JSON-TMP2 @ JSON-VN @ J-LEN!
   ELSE JSON-TMP @ 116 = IF
      s" true" JSON-MATCH
      J-BOOL JSON-NEW JSON-VN !
      -1 JSON-VN @ J-VAL!
   ELSE JSON-TMP @ 102 = IF
      s" false" JSON-MATCH
      J-BOOL JSON-NEW JSON-VN !
      0 JSON-VN @ J-VAL!
   ELSE JSON-TMP @ 110 = IF
      s" null" JSON-MATCH
      J-NULL JSON-NEW JSON-VN !
   ELSE
      s" json: bad value" JSON-SYNTAX
   THEN THEN THEN THEN THEN THEN THEN
   JSON-DEPTH @ 1- JSON-DEPTH !
   JSON-VN @ ;

: JSON-PARSE ( ptr u8 i64 -- i64 )
   {: a:ptr u :}
   JSON-RESET
   a JSON-A !
   u JSON-U !
   0 JSON-I !
   JSON-VALUE JSON-VN !
   JSON-SKIP-WS
   JSON-I @ JSON-U @ <> IF s" json: trailing data" JSON-SYNTAX THEN
   JSON-VN @ ;

: JSON-PARSE-TRY ( ptr u8 i64 -- i64 i64 i64 )
   [: 2dup JSON-PARSE JSON-PARSE-TRY-ROOT ! ;] catch {: code :}
   2drop
   code 0= IF JSON-PARSE-TRY-ROOT @ JSON-PARSE-OK 0 exit THEN
   -1 JSON-PARSE-THROW code ;

: JSON-KIND ( i64 -- i64 ) J-KIND@ ;
: JSON-COUNT ( i64 -- i64 ) J-COUNT@ ;

: JSON-REQUIRE-KIND ( i64 i64 -- )
   {: node kind :}
   node J-KIND@ kind <> IF s" json: wrong node kind" JSON-TYPE-ERROR THEN ;

: JSON-STRING$ ( i64 -- ptr u8 i64 )
   {: node :}
   node J-STR JSON-REQUIRE-KIND
   JSON-STR-BUF node J-OFF@ +  node J-LEN@ ;

: JSON-NUMBER$ ( i64 -- ptr u8 i64 )
   {: node :}
   node J-NUM JSON-REQUIRE-KIND
   JSON-STR-BUF node J-OFF@ +  node J-LEN@ ;

: JSON-BOOL@ ( i64 -- bool )
   {: node :}
   node J-BOOL JSON-REQUIRE-KIND
   node J-VAL@ 0 <> ;

: JSON-NULL? ( i64 -- bool )
   J-KIND@ J-NULL = ;

: JSON-ARR@ ( i64 i64 -- i64 )
   {: node idx :}
   node J-ARR JSON-REQUIRE-KIND
   idx 0< idx node J-COUNT@ >= or IF s" json: array index out of range" JSON-TYPE-ERROR THEN
   node J-START@ JSON-TMP !
   0 JSON-GI !
   begin JSON-GI @ idx < while
      JSON-TMP @ J-ITEM-NEXT J-CELL@ JSON-TMP !
      JSON-GI @ 1+ JSON-GI !
   repeat
   JSON-TMP @ J-ITEMS J-CELL@ ;

: JSON-OBJ@ ( i64 i64 -- ptr u8 i64 i64 )
   {: node idx :}
   node J-OBJ JSON-REQUIRE-KIND
   idx 0< idx node J-COUNT@ >= or IF s" json: object index out of range" JSON-TYPE-ERROR THEN
   node J-START@ JSON-TMP !
   0 JSON-GI !
   begin JSON-GI @ idx < while
      JSON-TMP @ J-PAIR-NEXT J-CELL@ JSON-TMP !
      JSON-GI @ 1+ JSON-GI !
   repeat
   JSON-STR-BUF JSON-TMP @ J-KEY-OFF J-CELL@ +
   JSON-TMP @ J-KEY-LEN J-CELL@
   JSON-TMP @ J-PAIR-VAL J-CELL@ ;

: JSON-GET ( i64 ptr u8 i64 -- i64 )
   {: node key:ptr ku :}
   node J-OBJ JSON-REQUIRE-KIND
   -1 JSON-GOT !
   0 JSON-GI !
   begin JSON-GI @ node J-COUNT@ < while
      node JSON-GI @ JSON-OBJ@ JSON-GVAL ! JSON-GKL ! JSON-GKA !
      JSON-GKA@ JSON-GKL @ key ku JSON-STR= IF
         JSON-GVAL @ JSON-GOT !
         node J-COUNT@ JSON-GI !
      ELSE
         JSON-GI @ 1+ JSON-GI !
      THEN
   repeat
   JSON-GOT @ ;

: JSONW-RESET ( -- )
   0 JSON-OUT-LEN ! ;

: JSONW-C ( n -- )
   {: c :}
   JSON-OUT-LEN @ 1+ JSON-OUT-CAP > IF s" json: writer buffer full" JSON-CAPACITY THEN
   c JSON-OUT-BUF JSON-OUT-LEN @ + c!
   JSON-OUT-LEN @ 1+ JSON-OUT-LEN ! ;

: JSONW-RAW ( ptr u8 i64 -- )
   {: a:ptr u :}
   JSON-OUT-LEN @ u + JSON-OUT-CAP > IF s" json: writer buffer full" JSON-CAPACITY THEN
   a u JSON-OUT-BUF JSON-OUT-LEN @ + JSON-COPY
   JSON-OUT-LEN @ u + JSON-OUT-LEN ! ;

: JSONW-HEX ( n -- n )
   dup 10 < IF 48 + ELSE 55 + THEN ;

: JSONW-U00 ( n -- )
   J-BACKSLASH JSONW-C
   117 JSONW-C
   48 JSONW-C
   48 JSONW-C
   dup 4 rshift JSONW-HEX JSONW-C
   $F and JSONW-HEX JSONW-C ;

: JSONW-ESC-C ( n -- )
   {: c :}
   c J-DQ = IF J-BACKSLASH JSONW-C J-DQ JSONW-C exit THEN
   c J-BACKSLASH = IF J-BACKSLASH JSONW-C J-BACKSLASH JSONW-C exit THEN
   c J-BS = IF J-BACKSLASH JSONW-C 98 JSONW-C exit THEN
   c J-FF = IF J-BACKSLASH JSONW-C 102 JSONW-C exit THEN
   c J-LF = IF J-BACKSLASH JSONW-C 110 JSONW-C exit THEN
   c J-CR = IF J-BACKSLASH JSONW-C 114 JSONW-C exit THEN
   c J-TAB = IF J-BACKSLASH JSONW-C 116 JSONW-C exit THEN
   c J-SP < IF c JSONW-U00 exit THEN
   c JSONW-C ;

: JSONW-STRING ( ptr u8 i64 -- )
   {: a:ptr u :}
   J-DQ JSONW-C
   0 begin dup u < while
      dup a + c@ JSONW-ESC-C
      1+
   repeat drop
   J-DQ JSONW-C ;

: JSONW-KEY ( ptr u8 i64 -- )
   JSONW-STRING
   J-COLON JSONW-C ;

: JSONW-OBJECT-START ( -- ) J-LBRACE JSONW-C ;
: JSONW-OBJECT-END ( -- ) J-RBRACE JSONW-C ;
: JSONW-ARRAY-START ( -- ) J-LBRACK JSONW-C ;
: JSONW-ARRAY-END ( -- ) J-RBRACK JSONW-C ;
: JSONW-COMMA ( -- ) J-COMMA JSONW-C ;

: JSON-EMIT ( i64 -- )
   {: node :}
   node J-KIND@ J-NULL = IF s" null" JSONW-RAW exit THEN
   node J-KIND@ J-BOOL = IF
      node JSON-BOOL@ IF s" true" ELSE s" false" THEN JSONW-RAW
      exit
   THEN
   node J-KIND@ J-NUM = IF node JSON-NUMBER$ JSONW-RAW exit THEN
   node J-KIND@ J-STR = IF node JSON-STRING$ JSONW-STRING exit THEN
   node J-KIND@ J-ARR = IF
      J-LBRACK JSONW-C
      0 begin dup node J-COUNT@ < while
         dup 0 > IF J-COMMA JSONW-C THEN
         node over JSON-ARR@ RECURSE
         1+
      repeat drop
      J-RBRACK JSONW-C
      exit
   THEN
   node J-KIND@ J-OBJ = IF
      J-LBRACE JSONW-C
      0 begin dup node J-COUNT@ < while
         dup 0 > IF J-COMMA JSONW-C THEN
         node over JSON-OBJ@ JSON-GVAL ! JSON-GKL ! JSON-GKA !
         JSON-GKA@ JSON-GKL @ JSONW-KEY
         JSON-GVAL @ RECURSE
         1+
      repeat drop
      J-RBRACE JSONW-C
      exit
   THEN
   s" json: unknown node kind" JSON-TYPE-ERROR ;

: JSON-WRITE ( i64 -- ptr u8 i64 )
   JSONW-RESET
   JSON-EMIT
   JSON-OUT-BUF JSON-OUT-LEN @ ;

: JSON-TRIM-LEFT ( ptr u8 i64 -- ptr u8 i64 )
   {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ JSON-WHITE? 0= IF dup a + u rot - exit THEN
      1+
   repeat drop
   a 0 ;

: JSON-TRIM-RIGHT ( ptr u8 i64 -- ptr u8 i64 )
   {: a:ptr u :}
   u JSON-TMP !
   begin JSON-TMP @ 0 > while
      a JSON-TMP @ 1- + c@ JSON-WHITE? IF
         JSON-TMP @ 1- JSON-TMP !
      ELSE
         a JSON-TMP @ exit
      THEN
   repeat
   a 0 ;

: JSON-TRIM ( ptr u8 i64 -- ptr u8 i64 )
   JSON-TRIM-LEFT JSON-TRIM-RIGHT ;

: JSONL-START-MODE ( ptr u8 i64 i64 -- )
   {: a:ptr u mode :}
   mode JSONL-MODE-STRICT <> mode JSONL-MODE-SKIP <> and IF
      s" jsonl: bad cursor mode" JSON-TYPE-ERROR
   THEN
   a JSONL-A !
   u JSONL-U !
   0 JSONL-I !
   0 JSONL-SKIPS !
   0 JSONL-LINE-N !
   mode JSONL-MODE ! ;

: JSONL-START-STRICT ( ptr u8 i64 -- )
   JSONL-MODE-STRICT JSONL-START-MODE ;

: JSONL-START-SKIP ( ptr u8 i64 -- )
   JSONL-MODE-SKIP JSONL-START-MODE ;

: JSONL-START ( ptr u8 i64 -- )
   JSONL-START-SKIP ;

: JSONL-SKIPPED ( -- i64 )
   JSONL-SKIPS @ ;

: JSONL-SKIP ( -- )
   JSONL-SKIPS @ 1+ JSONL-SKIPS ! ;

: JSONL-SKIP-MODE? ( -- bool )
   JSONL-MODE @ JSONL-MODE-SKIP = ;

: JSONL-TRUE ( -- bool )
   0 0= ;

: JSONL-FALSE ( -- bool )
   JSONL-TRUE 0= ;

: JSONL-LINE# ( -- i64 )
   JSONL-LINE-N @ ;

: JSONL-LINE$ ( -- ptr u8 i64 )
   JSONL-LA@ JSONL-LU @ ;

: JSONL-LINE++ ( -- )
   JSONL-LINE-N @ 1+ JSONL-LINE-N ! ;

: JSONL-TAKE-LINE ( -- bool )
   JSONL-I @ JSONL-U @ >= IF JSONL-FALSE exit THEN
   JSONL-I @ JSON-START !
   begin JSONL-I @ JSONL-U @ < while
      JSONL-A@ JSONL-I @ + c@ J-LF = IF
         JSONL-A@ JSON-START @ +  JSONL-I @ JSON-START @ -  JSON-TRIM
         JSONL-LU ! JSONL-LA !
         JSONL-I @ 1+ JSONL-I !
         JSONL-LINE++ JSONL-TRUE exit
      THEN
      JSONL-I @ 1+ JSONL-I !
   repeat
   JSONL-A@ JSON-START @ +  JSONL-U @ JSON-START @ -  JSON-TRIM
   JSONL-LU ! JSONL-LA !
   JSONL-U @ JSONL-I !
   JSONL-LINE++ JSONL-TRUE ;

: JSONL-PARSE-LINE ( -- i64 )
   JSONL-LA@ JSONL-LU @ JSON-PARSE ;

: JSONL-PARSE-TRY ( -- i64 i64 i64 )
   JSONL-LA@ JSONL-LU @ JSON-PARSE-TRY ;

: JSONL-PARSE-ROW ( -- i64 i64 i64 bool )
   JSONL-PARSE-TRY JSON-TMP ! JSON-TMP2 ! JSONL-ROOT !
   JSON-TMP2 @ JSON-PARSE-OK = IF
      JSONL-ROOT @ JSONL-ROW-JSON JSON-TMP @ JSONL-TRUE exit
   THEN
   JSONL-ROOT @ JSONL-ROW-ERROR JSON-TMP @ JSONL-TRUE ;

: JSONL-NEXT-ROW ( -- i64 i64 i64 bool )
   JSONL-TAKE-LINE 0= IF
      -1 JSONL-ROW-EOF 0 JSONL-FALSE exit
   THEN
   JSONL-LU @ 0= IF
      -1 JSONL-ROW-BLANK 0 JSONL-TRUE exit
   THEN
   JSONL-PARSE-ROW ;

: JSONL-OBJECT? ( i64 -- bool )
   JSON-KIND J-OBJ = ;

: JSONL-OBJECT-OR-SKIP ( i64 -- i64 bool )
   dup JSONL-OBJECT? 0= IF
      JSONL-SKIP-MODE? IF
         drop JSONL-SKIP -1 0 0= 0= exit
      THEN
      s" jsonl: row is not object" JSON-TYPE-ERROR
   THEN
   0 0= ;

: JSONL-RECOVER? ( i64 -- bool )
   {: code :}
   code E-JSON-SYNTAX = JSONL-SKIP-MODE? and IF
      JSONL-SKIP 0 0= exit
   THEN
   code throw
   0 0= 0= ;

: JSONL-NEXT-OBJECT ( -- i64 )
   begin JSONL-TAKE-LINE while
      JSONL-LU @ 0= IF
         JSONL-SKIP
      ELSE
         JSONL-SKIP-MODE? IF
            JSONL-PARSE-TRY JSON-TMP ! JSON-TMP2 ! JSONL-ROOT !
            JSON-TMP2 @ JSON-PARSE-OK = IF
               JSONL-ROOT @ JSONL-OBJECT-OR-SKIP IF exit THEN drop
            ELSE
               JSON-TMP @ JSONL-RECOVER? drop
            THEN
         ELSE
            JSONL-PARSE-LINE JSONL-ROOT !
            JSONL-ROOT @ JSONL-OBJECT-OR-SKIP IF exit THEN drop
         THEN
      THEN
   repeat
   -1 ;
