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
$8000 constant JSON-STR-CAP
$8000 constant JSON-OUT-CAP
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

create JSON-STR-BUF JSON-STR-CAP allot
create JSON-OUT-BUF JSON-OUT-CAP allot
create JSON-ERR-BUF JSON-ERR-CAP allot

variable JSON-NODES
variable JSON-ITEMS
variable JSON-PAIRS
variable JSON-STR-LEN
variable JSON-OUT-LEN
variable JSON-ERR-LEN
variable JSON-ERR-POS

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

: JSON-COPY ( addr i64 addr -- )
   {: a u dst :}
   0 begin dup u < while
      dup a + c@  over dst + c!
      1+
   repeat drop ;

: JSON-STR= ( addr i64 addr i64 -- bool )
   {: a u b v :}
   u v <> IF 0 exit THEN
   0 begin dup u < while
      dup a + c@  over b + c@  <> IF drop 0 exit THEN
      1+
   repeat drop  -1 ;

: JSON-SET-ERROR ( addr i64 -- )
   {: a u :}
   u JSON-ERR-CAP > IF s" json: error message overflow" 76 die THEN
   a u JSON-ERR-BUF JSON-COPY
   u JSON-ERR-LEN ! ;

: JSON-ERROR$ ( -- addr i64 )
   JSON-ERR-BUF JSON-ERR-LEN @ ;

: JSON-FAIL ( addr i64 i64 -- )
   {: a u code :}
   a u JSON-SET-ERROR
   JSON-I @ JSON-ERR-POS !
   code throw ;

: JSON-SYNTAX ( addr i64 -- )
   E-JSON-SYNTAX JSON-FAIL ;

: JSON-CAPACITY ( addr i64 -- )
   E-JSON-CAPACITY JSON-FAIL ;

: JSON-TYPE-ERROR ( addr i64 -- )
   E-JSON-TYPE JSON-FAIL ;

: JSON-RESET ( -- )
   0 JSON-NODES !
   0 JSON-ITEMS !
   0 JSON-PAIRS !
   0 JSON-STR-LEN !
   0 JSON-OUT-LEN !
   0 JSON-ERR-LEN !
   0 JSON-ERR-POS !
   0 JSON-DEPTH ! ;

: J-CELL@ ( i64 addr -- i64 )
   {: n base :}
   base n cells + @ ;

: J-CELL! ( i64 i64 addr -- )
   {: x n base :}
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

: JSON-STR+ ( i64 -- )
   {: c :}
   JSON-STR-LEN @ 1+ JSON-STR-CAP > IF s" json: string buffer full" JSON-CAPACITY THEN
   c JSON-STR-BUF JSON-STR-LEN @ + c!
   JSON-STR-LEN @ 1+ JSON-STR-LEN ! ;

: JSON-SAVE$ ( addr i64 -- i64 i64 )
   {: a u :}
   JSON-STR-LEN @ JSON-START !
   JSON-STR-LEN @ u + JSON-STR-CAP > IF s" json: string buffer full" JSON-CAPACITY THEN
   a u JSON-STR-BUF JSON-STR-LEN @ + JSON-COPY
   JSON-STR-LEN @ u + JSON-STR-LEN !
   JSON-START @ u ;

: JSON-WHITE? ( i64 -- bool )
   dup J-SP = over J-TAB = or over J-LF = or swap J-CR = or ;

: JSON-DIGIT? ( i64 -- bool )
   dup J-ZERO >= swap 57 <= and ;

: JSON-DIGIT1? ( i64 -- bool )
   dup 49 >= swap 57 <= and ;

: JSON-END? ( -- bool )
   JSON-I @ JSON-U @ >= ;

: JSON-AT ( -- i64 )
   JSON-END? IF s" json: unexpected end" JSON-SYNTAX THEN
   JSON-A @ JSON-I @ + c@ ;

: JSON-TAKE ( -- i64 )
   JSON-AT
   JSON-I @ 1+ JSON-I ! ;

: JSON-SKIP-WS ( -- )
   begin JSON-END? 0= while
      JSON-AT JSON-WHITE? IF JSON-I @ 1+ JSON-I ! ELSE exit THEN
   repeat ;

: JSON-EXPECT ( i64 -- )
   {: c :}
   JSON-TAKE c <> IF s" json: unexpected character" JSON-SYNTAX THEN ;

: JSON-HEX? ( i64 -- i64 )
   dup 48 >= over 57 <= and IF 48 - exit THEN
   dup 65 >= over 70 <= and IF 55 - exit THEN
   dup 97 >= over 102 <= and IF 87 - exit THEN
   drop -1 ;

: JSON-HEX-DIGIT ( -- i64 )
   JSON-TAKE JSON-HEX?
   dup 0< IF s" json: bad unicode escape" JSON-SYNTAX THEN ;

: JSON-HEX4 ( -- i64 )
   JSON-HEX-DIGIT 16 *
   JSON-HEX-DIGIT +
   16 *
   JSON-HEX-DIGIT +
   16 *
   JSON-HEX-DIGIT + ;

: JSON-UTF8+ ( i64 -- )
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

: JSON-MATCH ( addr i64 -- )
   {: a u :}
   JSON-I @ u + JSON-U @ > IF s" json: unexpected end" JSON-SYNTAX THEN
   0 begin dup u < while
      dup a + c@  over JSON-I @ + JSON-A @ + c@  <> IF s" json: bad literal" JSON-SYNTAX THEN
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
   JSON-A @ JSON-START @ +  JSON-I @ JSON-START @ -  JSON-SAVE$ ;

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
               0
            ELSE
               J-COMMA JSON-EXPECT
               JSON-SKIP-WS
               JSON-AT J-RBRACE = IF s" json: trailing object comma" JSON-SYNTAX THEN
               -1
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
               0
            ELSE
               J-COMMA JSON-EXPECT
               JSON-SKIP-WS
               JSON-AT J-RBRACK = IF s" json: trailing array comma" JSON-SYNTAX THEN
               -1
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

: JSON-PARSE ( addr i64 -- i64 )
   {: a u :}
   JSON-RESET
   a JSON-A !
   u JSON-U !
   0 JSON-I !
   JSON-VALUE JSON-VN !
   JSON-SKIP-WS
   JSON-I @ JSON-U @ <> IF s" json: trailing data" JSON-SYNTAX THEN
   JSON-VN @ ;

: JSON-KIND ( i64 -- i64 ) J-KIND@ ;
: JSON-COUNT ( i64 -- i64 ) J-COUNT@ ;

: JSON-REQUIRE-KIND ( i64 i64 -- )
   {: node kind :}
   node J-KIND@ kind <> IF s" json: wrong node kind" JSON-TYPE-ERROR THEN ;

: JSON-STRING$ ( i64 -- addr i64 )
   {: node :}
   node J-STR JSON-REQUIRE-KIND
   JSON-STR-BUF node J-OFF@ +  node J-LEN@ ;

: JSON-NUMBER$ ( i64 -- addr i64 )
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

: JSON-OBJ@ ( i64 i64 -- addr i64 i64 )
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

: JSON-GET ( i64 addr i64 -- i64 )
   {: node key ku :}
   node J-OBJ JSON-REQUIRE-KIND
   -1 JSON-GOT !
   0 JSON-GI !
   begin JSON-GI @ node J-COUNT@ < while
      node JSON-GI @ JSON-OBJ@ JSON-GVAL ! JSON-GKL ! JSON-GKA !
      JSON-GKA @ JSON-GKL @ key ku JSON-STR= IF
         JSON-GVAL @ JSON-GOT !
         node J-COUNT@ JSON-GI !
      ELSE
         JSON-GI @ 1+ JSON-GI !
      THEN
   repeat
   JSON-GOT @ ;

: JSONW-RESET ( -- )
   0 JSON-OUT-LEN ! ;

: JSONW-C ( i64 -- )
   {: c :}
   JSON-OUT-LEN @ 1+ JSON-OUT-CAP > IF s" json: writer buffer full" JSON-CAPACITY THEN
   c JSON-OUT-BUF JSON-OUT-LEN @ + c!
   JSON-OUT-LEN @ 1+ JSON-OUT-LEN ! ;

: JSONW-RAW ( addr i64 -- )
   {: a u :}
   JSON-OUT-LEN @ u + JSON-OUT-CAP > IF s" json: writer buffer full" JSON-CAPACITY THEN
   a u JSON-OUT-BUF JSON-OUT-LEN @ + JSON-COPY
   JSON-OUT-LEN @ u + JSON-OUT-LEN ! ;

: JSONW-HEX ( i64 -- i64 )
   dup 10 < IF 48 + ELSE 55 + THEN ;

: JSONW-U00 ( i64 -- )
   J-BACKSLASH JSONW-C
   117 JSONW-C
   48 JSONW-C
   48 JSONW-C
   dup 4 rshift JSONW-HEX JSONW-C
   $F and JSONW-HEX JSONW-C ;

: JSONW-ESC-C ( i64 -- )
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

: JSONW-STRING ( addr i64 -- )
   {: a u :}
   J-DQ JSONW-C
   0 begin dup u < while
      dup a + c@ JSONW-ESC-C
      1+
   repeat drop
   J-DQ JSONW-C ;

: JSONW-KEY ( addr i64 -- )
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
         JSON-GKA @ JSON-GKL @ JSONW-KEY
         JSON-GVAL @ RECURSE
         1+
      repeat drop
      J-RBRACE JSONW-C
      exit
   THEN
   s" json: unknown node kind" JSON-TYPE-ERROR ;

: JSON-WRITE ( i64 -- addr i64 )
   JSONW-RESET
   JSON-EMIT
   JSON-OUT-BUF JSON-OUT-LEN @ ;

: JSON-TRIM-LEFT ( addr i64 -- addr i64 )
   {: a u :}
   0 begin dup u < while
      dup a + c@ JSON-WHITE? 0= IF dup a + u rot - exit THEN
      1+
   repeat drop
   a 0 ;

: JSON-TRIM-RIGHT ( addr i64 -- addr i64 )
   {: a u :}
   u JSON-TMP !
   begin JSON-TMP @ 0 > while
      a JSON-TMP @ 1- + c@ JSON-WHITE? IF
         JSON-TMP @ 1- JSON-TMP !
      ELSE
         a JSON-TMP @ exit
      THEN
   repeat
   a 0 ;

: JSON-TRIM ( addr i64 -- addr i64 )
   JSON-TRIM-LEFT JSON-TRIM-RIGHT ;

: JSONL-START ( addr i64 -- )
   {: a u :}
   a JSONL-A !
   u JSONL-U !
   0 JSONL-I !
   0 JSONL-SKIPS ! ;

: JSONL-SKIPPED ( -- i64 )
   JSONL-SKIPS @ ;

: JSONL-TAKE-LINE ( -- bool )
   JSONL-I @ JSONL-U @ >= IF 0 exit THEN
   JSONL-I @ JSON-START !
   begin JSONL-I @ JSONL-U @ < while
      JSONL-A @ JSONL-I @ + c@ J-LF = IF
         JSONL-A @ JSON-START @ +  JSONL-I @ JSON-START @ -  JSON-TRIM
         JSONL-LU ! JSONL-LA !
         JSONL-I @ 1+ JSONL-I !
         -1 exit
      THEN
      JSONL-I @ 1+ JSONL-I !
   repeat
   JSONL-A @ JSON-START @ +  JSONL-U @ JSON-START @ -  JSON-TRIM
   JSONL-LU ! JSONL-LA !
   JSONL-U @ JSONL-I !
   -1 ;

\ Unchecked boundary: this JSONL layer uses catch to skip prose/invalid lines.
\ catch is not modeled by the checker; the strict JSON parser it calls remains
\ typed and throws only the JSON error constants above.
: JSONL-PARSE-LINE
   \ ( -- node )
   JSONL-LA @ JSONL-LU @ JSON-PARSE ;

: JSONL-TRY
   \ ( -- code )
   ['] JSONL-PARSE-LINE catch
   dup 0= IF drop JSONL-ROOT ! 0 exit THEN ;

: JSONL-SKIP
   \ ( -- )
   JSONL-SKIPS @ 1+ JSONL-SKIPS ! ;

: JSONL-NEXT-OBJECT
   \ ( -- node|-1 )
   begin JSONL-TAKE-LINE while
      JSONL-LU @ 0= IF
         JSONL-SKIP
      ELSE
         JSONL-TRY JSON-TMP !
         JSON-TMP @ 0= IF
            JSONL-ROOT @ JSON-KIND J-OBJ = IF JSONL-ROOT @ exit THEN
            JSONL-SKIP
         ELSE
            JSON-TMP @ E-JSON-SYNTAX = IF
               JSONL-SKIP
            ELSE
               JSON-TMP @ throw
            THEN
         THEN
      THEN
   repeat
   -1 ;
