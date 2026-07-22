\ json-read.f - checked zero-allocation JSON pull/cursor parser.
\
\ The parser is a cursor over the caller's source buffer and fixed storage. INIT
\ returns a linear reader; every operation threads that reader and CLOSE consumes
\ it. No allocation or process-global parser state is involved. Structural
\ well-formedness (RFC 8259: strict, no comments, no trailing commas, no single
\ quotes, string escapes, and Unicode scalar encoding) is validated as the cursor
\ advances; number and value decode happen on demand. Every failure is a named
\ throw in the -3900..-3999 JSON-reader block.
\
\ The parser lives in `package JR`. External callers use the qualified public API
\ (JR:INIT, JR:CLOSE, JR:NEXT, JR:TOKEN, JR:SPAN$, JR:INT, JR:FLOAT, JR:STR,
\ JR:SKIP-VALUE, JR:FIND-KEY) and the qualified token kinds (JR:T-OBJ ..
\ JR:T-END). Callers allocate JR:STORAGE-BYTES at a cell-aligned address and keep
\ both storage and source live and exclusive until CLOSE. The byte constants,
\ opaque representation leaves, cursor state, and scan/decode helpers - including
\ the bounds-checked source reader AT - are package-private and sealed after the
\ module is assembled.
\
\ Cursor state machine (STATE): a value token drives one transition each.
\  ST-VALUE   value required (doc start / after ':' / after ',' in an array)
\  ST-ELEM    array just opened: a value or ']'
\  ST-KEY     object just opened: a key string or '}'
\  ST-MEMBER  after ',' in an object: a key string (no '}': trailing comma)
\  ST-SEP     value complete inside a container: ',' or the matching close
\  ST-DONE    top-level value complete: only whitespace then JR:T-END
\ A ',' in ST-SEP is consumed silently (STEP returns RETRY and NEXT loops);
\ container closes are emitted as T-OBJ-END / T-ARR-END.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/adt/option.f                 \ option<CAD-NUM:index> for STR:INDEX-OF (switchover wave A)

package JR

public

DEFLINEAR JR:reader

private

\ ---- byte constants -------------------------------------------------------
8 constant BS
9 constant TAB
10 constant LF
12 constant FF
13 constant CR
32 constant SP
34 constant DQ
43 constant PLUS
44 constant COMMA
45 constant MINUS
46 constant DOT
47 constant SLASH
48 constant ZERO
58 constant COLON
69 constant E-UPPER
91 constant LBRACK
92 constant BACKSLASH
93 constant RBRACK
98 constant CH-B
101 constant E-LOWER
102 constant CH-F
110 constant CH-N
114 constant CH-R
116 constant CH-T
117 constant CH-U
123 constant LBRACE
125 constant RBRACE

65 constant HEXUP-A
70 constant HEXUP-F
97 constant HEXLO-A
102 constant HEXLO-F
10 constant HEX-TEN
16 constant HEX-BASE

\ ---- unicode / utf-8 ------------------------------------------------------
$80 constant UTF1-MAX
$800 constant UTF2-MAX
$10000 constant UTF3-MAX
$C2 constant UTF2-MIN
$E0 constant UTF2-END
$F0 constant UTF3-END
$F5 constant UTF4-END
$A0 constant UTF-E0-MIN
$90 constant UTF-F0-MIN
$ED constant UTF-SUR-LEAD
$9F constant UTF-SUR-MAX
$F4 constant UTF-MAX-LEAD
$8F constant UTF-MAX-SECOND
$3F constant UTF-MASK
$80 constant UTF-CONT
$C0 constant UTF2-LEAD
$E0 constant UTF3-LEAD
$F0 constant UTF4-LEAD
$D800 constant SUR-HI
$DC00 constant SUR-LO
$E000 constant SUR-END
$10000 constant SUR-BASE
10 constant SUR-SHIFT

\ ---- token kinds (public: JR:T-*) -----------------------------------------
public

0 constant T-OBJ
1 constant T-OBJ-END
2 constant T-ARR
3 constant T-ARR-END
4 constant T-KEY
5 constant T-STR
6 constant T-INT
7 constant T-FLOAT
8 constant T-TRUE
9 constant T-FALSE
10 constant T-NULL
11 constant T-END

private

99 constant RETRY

\ ---- parser state ---------------------------------------------------------
0 constant ST-VALUE
1 constant ST-ELEM
2 constant ST-KEY
3 constant ST-MEMBER
4 constant ST-SEP
5 constant ST-DONE

1 constant CTX-OBJ
2 constant CTX-ARR
64 constant MAX-DEPTH
256 constant KEY-CAP

16 constant STATE-CELLS
STATE-CELLS cells constant CTX-OFF
CTX-OFF MAX-DEPTH + constant KEY-OFF

public

KEY-OFF KEY-CAP + constant STORAGE-BYTES

private

\ ---- opaque reader representation boundary ------------------------------
\ These four private leaves are one audited abstraction. Checked INIT owns all
\ validation and initialization; only the representation refinements themselves
\ remain trusted until bounded host storage can express extent and lifetime.
TRUSTED: STORAGE>PREMINT ( ptr a -- ptr n n )
   dup ;

TRUSTED: MINT-READER ( ptr a -- JR:reader ) ;

TRUSTED: READER>STATE ( JR:reader -- JR:reader ptr n ptr u8 )
   dup dup ;

TRUSTED: CONSUME-READER ( JR:reader -- )
   drop ;

: READER>CELLS ( JR:reader -- JR:reader ptr n )
   READER>STATE drop ;

: CELL-AT ( ptr n n -- ptr n )
   cells + ;

: SRC-FIELD ( ptr n -- ptr ptr u8 )
   0 ptr-field ;

: SRC@ ( ptr n -- ptr u8 )
   SRC-FIELD @ ;

: SRC! ( ptr n ptr u8 -- )
   swap SRC-FIELD ! ;

: SRC-U@ ( ptr n -- n ) 1 CELL-AT @ ;
: SRC-U! ( ptr n n -- ) swap 1 CELL-AT ! ;
: POS@ ( ptr n -- n ) 2 CELL-AT @ ;
: POS! ( ptr n n -- ) swap 2 CELL-AT ! ;
: DEPTH@ ( ptr n -- n ) 3 CELL-AT @ ;
: DEPTH! ( ptr n n -- ) swap 3 CELL-AT ! ;
: STATE@ ( ptr n -- n ) 4 CELL-AT @ ;
: STATE! ( ptr n n -- ) swap 4 CELL-AT ! ;
: KIND@ ( ptr n -- n ) 5 CELL-AT @ ;
: KIND! ( ptr n n -- ) swap 5 CELL-AT ! ;
: TOK-OFF@ ( ptr n -- n ) 6 CELL-AT @ ;
: TOK-OFF! ( ptr n n -- ) swap 6 CELL-AT ! ;
: TOK-LEN@ ( ptr n -- n ) 7 CELL-AT @ ;
: TOK-LEN! ( ptr n n -- ) swap 7 CELL-AT ! ;

: UP-FIELD ( ptr n -- ptr ptr u8 ) 8 ptr-field ;
: UP@ ( ptr n -- ptr u8 ) UP-FIELD @ ;
: UP! ( ptr n ptr u8 -- ) swap UP-FIELD ! ;
: UCAP@ ( ptr n -- n ) 9 CELL-AT @ ;
: UCAP! ( ptr n n -- ) swap 9 CELL-AT ! ;
: UO@ ( ptr n -- n ) 10 CELL-AT @ ;
: UO! ( ptr n n -- ) swap 10 CELL-AT ! ;
: UI@ ( ptr n -- n ) 11 CELL-AT @ ;
: UI! ( ptr n n -- ) swap 11 CELL-AT ! ;
: UE@ ( ptr n -- n ) 12 CELL-AT @ ;
: UE! ( ptr n n -- ) swap 12 CELL-AT ! ;
: HI@ ( ptr n -- n ) 13 CELL-AT @ ;
: HI! ( ptr n n -- ) swap 13 CELL-AT ! ;
: LO@ ( ptr n -- n ) 14 CELL-AT @ ;
: LO! ( ptr n n -- ) swap 14 CELL-AT ! ;
: NI@ ( ptr n -- n ) 15 CELL-AT @ ;
: NI! ( ptr n n -- ) swap 15 CELL-AT ! ;

: CTX-BYTE ( ptr n n -- ptr n )
   CTX-OFF + + ;

: KEY-BUF ( ptr u8 -- ptr u8 )
   KEY-OFF + ;

: INIT-STATE ( ptr n ptr u8 n -- ) {: state:ptr source:ptr len:n :}
   state source SRC!
   state len SRC-U!
   state 0 POS!
   state 0 DEPTH!
   state ST-VALUE STATE!
   state T-END KIND!
   state 0 TOK-OFF!
   state 0 TOK-LEN! ;

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

\ ---- source cursor --------------------------------------------------------
: IN-BOUNDS? ( ptr n n -- bool ) {: state:ptr idx:n :}
   idx 0 >= idx state SRC-U@ < and ;

: AT ( ptr n n -- n ) {: state:ptr idx:n :}
   state idx IN-BOUNDS? 0= if E-JR-BOUNDS throw then
   state SRC@ idx + c@ ;

: EOF? ( ptr n -- bool ) {: state:ptr :}
   state POS@ state SRC-U@ >= ;

: PEEK ( ptr n -- n ) {: state:ptr :}
   state state POS@ AT ;

: ADVANCE ( ptr n -- ) {: state:ptr :}
   state state POS@ 1+ POS! ;

public

: INIT ( ptr a n ptr u8 n -- JR:reader ) {: storage:ptr cap:n source:ptr len:n :}
   storage 0= if E-STORAGE throw then
   storage STORAGE>PREMINT
   CELL 1- and 0 <> if E-STORAGE throw then
   cap STORAGE-BYTES < if E-CAPACITY throw then
   len 0 < if E-SOURCE throw then
   len 0 > source 0= and if E-SOURCE throw then
   source len INIT-STATE
   storage MINT-READER ;

: CLOSE ( JR:reader -- )
   CONSUME-READER ;

: TOKEN ( JR:reader -- JR:reader n )
   READER>CELLS KIND@ ;

: SPAN$ ( JR:reader -- JR:reader ptr u8 n )
   READER>CELLS {: state:ptr :}
   state SRC@ state TOK-OFF@ + state TOK-LEN@ ;

private

\ ---- whitespace -----------------------------------------------------------
: WS? ( n -- bool )
   dup SP = over TAB = or over LF = or swap CR = or ;

: SKIP-WS ( ptr n -- ) {: state:ptr :}
   begin state EOF? 0= if state PEEK WS? else FALSE then while
      state ADVANCE
   repeat ;

\ ---- container stack ------------------------------------------------------
: PUSH ( ptr n n -- ) {: state:ptr kind:n :}
   state DEPTH@ MAX-DEPTH >= if E-JR-DEPTH throw then
   kind state state DEPTH@ CTX-BYTE c!
   state state DEPTH@ 1+ DEPTH! ;

: POP ( ptr n -- ) {: state:ptr :}
   state state DEPTH@ 1- DEPTH! ;

: CUR-CTX ( ptr n -- n ) {: state:ptr :}
   state state DEPTH@ 1- CTX-BYTE c@ ;

: AFTER-VALUE ( ptr n -- ) {: state:ptr :}
   state DEPTH@ 0= if ST-DONE else ST-SEP then state swap STATE! ;

\ ---- string token scanning ------------------------------------------------
: HEX-VAL ( n -- n bool )
   dup STR-DIGIT? if STR-DIGIT-VALUE TRUE exit then
   dup HEXUP-A >= over HEXUP-F <= and if HEXUP-A - HEX-TEN + TRUE exit then
   dup HEXLO-A >= over HEXLO-F <= and if HEXLO-A - HEX-TEN + TRUE exit then
   drop 0 FALSE ;

: SCAN-HEX ( ptr n n -- n ) {: state:ptr value:n :}
   state EOF? if E-JR-ESCAPE throw then
   value HEX-BASE *
   state PEEK HEX-VAL 0= if E-JR-ESCAPE throw then
   +
   state ADVANCE ;

: SCAN-HEX4 ( ptr n -- n ) {: state:ptr :}
   state 0 SCAN-HEX
   state swap SCAN-HEX
   state swap SCAN-HEX
   state swap SCAN-HEX ;

: SIMPLE-ESC? ( n -- bool )
   dup DQ = over BACKSLASH = or over SLASH = or over CH-B = or
   over CH-F = or over CH-N = or over CH-R = or swap CH-T = or ;

: SCAN-SURROGATE ( ptr n n -- ) {: state:ptr hi:n :}
   hi SUR-LO >= if E-JR-SURROGATE throw then
   state EOF? if E-JR-SURROGATE throw then
   state PEEK BACKSLASH <> if E-JR-SURROGATE throw then
   state ADVANCE
   state EOF? if E-JR-SURROGATE throw then
   state PEEK CH-U <> if E-JR-SURROGATE throw then
   state ADVANCE
   state SCAN-HEX4 {: lo:n :}
   lo SUR-LO < lo SUR-END >= or if E-JR-SURROGATE throw then ;

: SCAN-ESC ( ptr n -- ) {: state:ptr :}
   state ADVANCE
   state EOF? if E-JR-ESCAPE throw then
   state PEEK dup SIMPLE-ESC? if drop state ADVANCE exit then
   CH-U <> if E-JR-ESCAPE throw then
   state ADVANCE
   state SCAN-HEX4 {: cp:n :}
   cp SUR-HI < if exit then
   cp SUR-END >= if exit then
   state cp SCAN-SURROGATE ;

: REQUIRE-UTF-BYTES ( ptr n n -- ) {: state:ptr count:n :}
   state POS@ count + state SRC-U@ > if E-JR-STRING throw then ;

: CONT? ( n -- bool )
   $C0 and UTF-CONT = ;

: UTF-BYTE ( ptr n n -- n ) {: state:ptr off:n :}
   state state POS@ off + AT ;

: ADVANCE-N ( ptr n n -- ) {: state:ptr count:n :}
   state state POS@ count + POS! ;

: SCAN-UTF2 ( ptr n -- ) {: state:ptr :}
   state 2 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE CONT? 0= if E-JR-STRING throw then
   state 2 ADVANCE-N ;

: SCAN-UTF3 ( ptr n n -- ) {: state:ptr lead:n :}
   state 3 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE {: second:n :}
   second CONT? 0= state 2 UTF-BYTE CONT? 0= or if E-JR-STRING throw then
   lead UTF2-END = second UTF-E0-MIN < and if E-JR-STRING throw then
   lead UTF-SUR-LEAD = second UTF-SUR-MAX > and if E-JR-STRING throw then
   state 3 ADVANCE-N ;

: SCAN-UTF4 ( ptr n n -- ) {: state:ptr lead:n :}
   state 4 REQUIRE-UTF-BYTES
   state 1 UTF-BYTE {: second:n :}
   second CONT? 0= state 2 UTF-BYTE CONT? 0= or
   state 3 UTF-BYTE CONT? 0= or if E-JR-STRING throw then
   lead UTF3-END = second UTF-F0-MIN < and if E-JR-STRING throw then
   lead UTF-MAX-LEAD = second UTF-MAX-SECOND > and if E-JR-STRING throw then
   state 4 ADVANCE-N ;

: SCAN-UTF8 ( ptr n n -- ) {: state:ptr lead:n :}
   lead UTF2-MIN >= lead UTF2-END < and if state SCAN-UTF2 exit then
   lead UTF2-END >= lead UTF3-END < and if state lead SCAN-UTF3 exit then
   lead UTF3-END >= lead UTF4-END < and if state lead SCAN-UTF4 exit then
   E-JR-STRING throw ;

: SCAN-STRING ( ptr n -- ) {: state:ptr :}
   state ADVANCE
   state state POS@ TOK-OFF!
   begin
      state EOF? if E-JR-STRING throw then
      state PEEK
      dup DQ = if
         drop
         state state POS@ state TOK-OFF@ - TOK-LEN!
         state ADVANCE exit
      then
      dup BACKSLASH = if
         drop state SCAN-ESC
      else
         dup SP < if drop E-JR-STRING throw then
         dup UTF1-MAX < if drop state ADVANCE else state swap SCAN-UTF8 then
      then
   again ;

\ ---- number token scanning + validation -----------------------------------
: NUM-CHAR? ( n -- bool )
   dup STR-DIGIT? if drop TRUE exit then
   dup MINUS = if drop TRUE exit then
   dup PLUS = if drop TRUE exit then
   dup DOT = if drop TRUE exit then
   dup E-LOWER = if drop TRUE exit then
   E-UPPER = ;

: SCAN-NUMBER ( ptr n -- ) {: state:ptr :}
   state state POS@ TOK-OFF!
   begin state EOF? 0= if state PEEK NUM-CHAR? else FALSE then while
      state ADVANCE
   repeat
   state state POS@ state TOK-OFF@ - TOK-LEN! ;

: NV-DIGIT? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state NI@ u >= if FALSE exit then
   a state NI@ + c@ STR-DIGIT? ;

: NV-SKIP-DIGITS ( ptr n ptr u8 n -- n ) {: state:ptr a:ptr u:n :}
   0 begin state a u NV-DIGIT? while
      state state NI@ 1+ NI!
      1+
   repeat ;

: NV-INT? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state a u NV-DIGIT? 0= if FALSE exit then
   a state NI@ + c@ ZERO = if
      state state NI@ 1+ NI!
      state a u NV-DIGIT? if FALSE exit then
      TRUE exit
   then
   state a u NV-SKIP-DIGITS drop TRUE ;

: NV-FRAC? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state NI@ u >= if TRUE exit then
   a state NI@ + c@ DOT <> if TRUE exit then
   state state NI@ 1+ NI!
   state a u NV-SKIP-DIGITS 0= if FALSE exit then
   TRUE ;

: NV-EXP? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state NI@ u >= if TRUE exit then
   a state NI@ + c@ dup E-LOWER <> swap E-UPPER <> and if TRUE exit then
   state state NI@ 1+ NI!
   state NI@ u < if
      a state NI@ + c@ dup PLUS = swap MINUS = or if
         state state NI@ 1+ NI!
      then
   then
   state a u NV-SKIP-DIGITS 0= if FALSE exit then
   TRUE ;

: JSON-NUMBER? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   u 0= if FALSE exit then
   state 0 NI!
   a state NI@ + c@ MINUS = if state state NI@ 1+ NI! then
   state a u NV-INT? 0= if FALSE exit then
   state a u NV-FRAC? 0= if FALSE exit then
   state a u NV-EXP? 0= if FALSE exit then
   state NI@ u = ;

: RAW-SPAN$ ( ptr n -- ptr u8 n ) {: state:ptr :}
   state SRC@ state TOK-OFF@ + state TOK-LEN@ ;

: SPAN-HAS? ( ptr n n -- bool ) {: state:ptr c:n :}
   state RAW-SPAN$ STR:LENGTH c STR:INDEX-OF MATCH option
     none OF FALSE ENDOF
     some OF drop TRUE ENDOF
   ;MATCH ;

: NUM-FLOATY? ( ptr n -- bool ) {: state:ptr :}
   state DOT SPAN-HAS? if TRUE exit then
   state E-LOWER SPAN-HAS? if TRUE exit then
   state E-UPPER SPAN-HAS? ;

\ ---- literal tokens -------------------------------------------------------
: MATCH-LIT? ( ptr n ptr u8 n -- bool ) {: state:ptr la:ptr lu:n :}
   state POS@ lu + state SRC-U@ > if FALSE exit then
   0 begin dup lu < while
      dup la + c@ over state swap state POS@ + AT <> if
         drop FALSE exit
      then
      1+
   repeat drop TRUE ;

: READ-LIT ( ptr n ptr u8 n n -- n ) {: state:ptr la:ptr lu:n kind:n :}
   state la lu MATCH-LIT? 0= if E-JR-MALFORMED throw then
   state state POS@ TOK-OFF!
   state lu TOK-LEN!
   state kind KIND!
   state state POS@ lu + POS!
   state AFTER-VALUE
   kind ;

: READ-TRUE ( ptr n -- n )
   s" true" T-TRUE READ-LIT ;

: READ-FALSE ( ptr n -- n )
   s" false" T-FALSE READ-LIT ;

: READ-NULL ( ptr n -- n )
   s" null" T-NULL READ-LIT ;

\ ---- value readers --------------------------------------------------------
: OPEN-OBJ ( ptr n -- n ) {: state:ptr :}
   state state POS@ TOK-OFF!
   state 1 TOK-LEN!
   state T-OBJ KIND!
   state ADVANCE
   state CTX-OBJ PUSH
   state ST-KEY STATE!
   T-OBJ ;

: OPEN-ARR ( ptr n -- n ) {: state:ptr :}
   state state POS@ TOK-OFF!
   state 1 TOK-LEN!
   state T-ARR KIND!
   state ADVANCE
   state CTX-ARR PUSH
   state ST-ELEM STATE!
   T-ARR ;

: CLOSE-OBJ ( ptr n -- n ) {: state:ptr :}
   state state POS@ TOK-OFF!
   state 1 TOK-LEN!
   state T-OBJ-END KIND!
   state ADVANCE
   state POP
   state AFTER-VALUE
   T-OBJ-END ;

: CLOSE-ARR ( ptr n -- n ) {: state:ptr :}
   state state POS@ TOK-OFF!
   state 1 TOK-LEN!
   state T-ARR-END KIND!
   state ADVANCE
   state POP
   state AFTER-VALUE
   T-ARR-END ;

: READ-STRING ( ptr n -- n ) {: state:ptr :}
   state SCAN-STRING
   state T-STR KIND!
   state AFTER-VALUE
   T-STR ;

: READ-KEY ( ptr n -- n ) {: state:ptr :}
   state SCAN-STRING
   state T-KEY KIND!
   state SKIP-WS
   state EOF? if E-JR-COLON throw then
   state PEEK COLON <> if E-JR-COLON throw then
   state ADVANCE
   state ST-VALUE STATE!
   T-KEY ;

: READ-NUMBER ( ptr n -- n ) {: state:ptr :}
   state SCAN-NUMBER
   state state RAW-SPAN$ JSON-NUMBER? 0= if E-JR-NUMBER throw then
   state NUM-FLOATY? if T-FLOAT else T-INT then state swap KIND!
   state AFTER-VALUE
   state KIND@ ;

: READ-VALUE ( ptr n -- n ) {: state:ptr :}
   state PEEK {: c:n :}
   c LBRACE = if state OPEN-OBJ exit then
   c LBRACK = if state OPEN-ARR exit then
   c DQ = if state READ-STRING exit then
   c CH-T = if state READ-TRUE exit then
   c CH-F = if state READ-FALSE exit then
   c CH-N = if state READ-NULL exit then
   c MINUS = if state READ-NUMBER exit then
   c STR-DIGIT? if state READ-NUMBER exit then
   E-JR-MALFORMED throw ;

\ ---- state dispatch -------------------------------------------------------
: DO-VALUE ( ptr n -- n )
   READ-VALUE ;

: DO-ELEM ( ptr n -- n ) {: state:ptr :}
   state PEEK RBRACK = if state CLOSE-ARR exit then
   state READ-VALUE ;

: DO-KEY ( ptr n -- n ) {: state:ptr :}
   state PEEK RBRACE = if state CLOSE-OBJ exit then
   state PEEK DQ = if state READ-KEY exit then
   E-JR-MALFORMED throw ;

: DO-MEMBER ( ptr n -- n ) {: state:ptr :}
   state PEEK DQ = if state READ-KEY exit then
   E-JR-MALFORMED throw ;

: DO-SEP ( ptr n -- n ) {: state:ptr :}
   state PEEK COMMA = if
      state ADVANCE
      state CUR-CTX CTX-OBJ = if ST-MEMBER else ST-VALUE then
      state swap STATE!
      RETRY exit
   then
   state PEEK RBRACE = state CUR-CTX CTX-OBJ = and if
      state CLOSE-OBJ exit
   then
   state PEEK RBRACK = state CUR-CTX CTX-ARR = and if
      state CLOSE-ARR exit
   then
   E-JR-COMMA throw ;

: PUBLISH-END ( ptr n -- n ) {: state:ptr :}
   state T-END KIND!
   state state POS@ TOK-OFF!
   state 0 TOK-LEN!
   T-END ;

: STEP ( ptr n -- n ) {: state:ptr :}
   state SKIP-WS
   state EOF? if
      state STATE@ ST-DONE = if state PUBLISH-END exit then
     E-JR-EOF throw
   then
   state STATE@ ST-DONE = if E-JR-TRAILING throw then
   state STATE@ ST-VALUE = if state DO-VALUE exit then
   state STATE@ ST-ELEM = if state DO-ELEM exit then
   state STATE@ ST-KEY = if state DO-KEY exit then
   state STATE@ ST-MEMBER = if state DO-MEMBER exit then
   state DO-SEP ;

: NEXT-INNER ( ptr n -- n ) {: state:ptr :}
   begin state STEP dup RETRY = while drop repeat ;

public

: NEXT ( JR:reader -- JR:reader n )
   READER>CELLS NEXT-INNER ;

private

\ ---- number value decode --------------------------------------------------
: INT-INNER ( ptr n -- n ) {: state:ptr :}
   state KIND@ T-INT <> if E-JR-STATE throw then
   state RAW-SPAN$ STR>NUMBER? MATCH option
     none OF E-JR-NUMBER throw ENDOF
     some OF ENDOF
   ;MATCH ;

: FLOAT-INNER ( ptr n -- r ) {: state:ptr :}
   state KIND@ dup T-INT = swap T-FLOAT = or 0= if E-JR-STATE throw then
   state RAW-SPAN$ STR>FLOAT MATCH option
     none OF E-JR-NUMBER throw ENDOF
     some OF ENDOF
   ;MATCH ;

public

: INT ( JR:reader -- JR:reader n )
   READER>CELLS INT-INNER ;

: FLOAT ( JR:reader -- JR:reader r )
   READER>CELLS FLOAT-INNER ;

private

\ ---- string value decode (unescape into caller buffer) --------------------
: SPAN-AT ( ptr n n -- n ) {: state:ptr idx:n :}
   state state TOK-OFF@ idx + AT ;

: UNESC-CHAR ( ptr n -- n ) {: state:ptr :}
   state state UI@ SPAN-AT ;

: EMIT-BYTE ( ptr n n -- ) {: state:ptr byte:n :}
   state UO@ state UCAP@ >= if E-JR-STATE throw then
   byte state UP@ state UO@ + c!
   state state UO@ 1+ UO! ;

: HEX-NEXT ( ptr n n -- n ) {: state:ptr value:n :}
   value HEX-BASE *
   state state UI@ SPAN-AT HEX-VAL 0= if E-JR-ESCAPE throw then
   +
   state state UI@ 1+ UI! ;

: READ-HEX4 ( ptr n -- n ) {: state:ptr :}
   state UI@ 4 + state TOK-LEN@ > if E-JR-ESCAPE throw then
   state 0 HEX-NEXT
   state swap HEX-NEXT
   state swap HEX-NEXT
   state swap HEX-NEXT ;

: EMIT-UTF8 ( ptr n n -- ) {: state:ptr cp:n :}
   cp UTF1-MAX < if state cp EMIT-BYTE exit then
   cp UTF2-MAX < if
      state cp 6 rshift UTF2-LEAD or EMIT-BYTE
      state cp UTF-MASK and UTF-CONT or EMIT-BYTE exit
   then
   cp UTF3-MAX < if
      state cp 12 rshift UTF3-LEAD or EMIT-BYTE
      state cp 6 rshift UTF-MASK and UTF-CONT or EMIT-BYTE
      state cp UTF-MASK and UTF-CONT or EMIT-BYTE exit
   then
   state cp 18 rshift UTF4-LEAD or EMIT-BYTE
   state cp 12 rshift UTF-MASK and UTF-CONT or EMIT-BYTE
   state cp 6 rshift UTF-MASK and UTF-CONT or EMIT-BYTE
   state cp UTF-MASK and UTF-CONT or EMIT-BYTE ;

: UNI ( ptr n -- ) {: state:ptr :}
   state state READ-HEX4 HI!
   state HI@ SUR-HI < if state state HI@ EMIT-UTF8 exit then
   state HI@ SUR-END >= if state state HI@ EMIT-UTF8 exit then
   state HI@ SUR-LO >= if E-JR-SURROGATE throw then
   state UI@ 2 + state TOK-LEN@ > if E-JR-SURROGATE throw then
   state state UI@ SPAN-AT BACKSLASH <> if E-JR-SURROGATE throw then
   state state UI@ 1+ SPAN-AT CH-U <> if E-JR-SURROGATE throw then
   state state UI@ 2 + UI!
   state state READ-HEX4 LO!
   state LO@ SUR-LO < if E-JR-SURROGATE throw then
   state LO@ SUR-END >= if E-JR-SURROGATE throw then
   state SUR-BASE
   state HI@ SUR-HI - SUR-SHIFT lshift +
   state LO@ SUR-LO - +
   EMIT-UTF8 ;

: ESC-BYTE ( ptr n n -- ) {: state:ptr esc:n :}
   esc DQ = if state DQ EMIT-BYTE exit then
   esc BACKSLASH = if state BACKSLASH EMIT-BYTE exit then
   esc SLASH = if state SLASH EMIT-BYTE exit then
   esc CH-B = if state BS EMIT-BYTE exit then
   esc CH-F = if state FF EMIT-BYTE exit then
   esc CH-N = if state LF EMIT-BYTE exit then
   esc CH-R = if state CR EMIT-BYTE exit then
   esc CH-T = if state TAB EMIT-BYTE exit then
   E-JR-ESCAPE throw ;

: UNESC-STEP ( ptr n -- ) {: state:ptr :}
   state UNESC-CHAR BACKSLASH <> if
      state state UNESC-CHAR EMIT-BYTE
      state state UI@ 1+ UI! exit
   then
   state UI@ 1+ state TOK-LEN@ >= if E-JR-ESCAPE throw then
   state state state UI@ 1+ SPAN-AT UE!
   state state UI@ 2 + UI!
   state UE@ CH-U = if state UNI exit then
   state state UE@ ESC-BYTE ;

: STR-INNER ( ptr n ptr u8 n -- n ) {: state:ptr dst:ptr cap:n :}
   state KIND@ dup T-STR = swap T-KEY = or 0= if E-JR-STATE throw then
   dst 0= cap 0 < or if E-JR-STATE throw then
   state dst UP!
   state cap UCAP!
   state 0 UI!
   state 0 UO!
   begin state UI@ state TOK-LEN@ < while
      state UNESC-STEP
   repeat
   state UO@ ;

public

: STR ( JR:reader ptr u8 n -- JR:reader n ) {: dst:ptr cap:n :}
   READER>CELLS dst cap STR-INNER ;

private

\ ---- skip / find ----------------------------------------------------------
: SCALAR? ( n -- bool )
   dup T-STR = over T-INT = or over T-FLOAT = or
   over T-TRUE = or over T-FALSE = or swap T-NULL = or ;

: OPENER? ( n -- bool )
   dup T-OBJ = swap T-ARR = or ;

: CLOSER? ( n -- bool )
   dup T-OBJ-END = swap T-ARR-END = or ;

: SKIP-INNER ( ptr n -- ) {: state:ptr :}
   state KIND@ SCALAR? if exit then
   state KIND@ OPENER? 0= if E-JR-STATE throw then
   1 begin dup 0 > while
      state NEXT-INNER
      dup OPENER? if drop 1+ else
      dup CLOSER? if drop 1- else
      dup T-END = if E-JR-EOF throw else drop then then then
   repeat drop ;

: FIND-INNER ( ptr n ptr u8 ptr u8 n -- bool ) {: state:ptr bytes:ptr ka:ptr ku:n :}
   begin
      state NEXT-INNER
      dup T-OBJ-END = if drop FALSE exit then
      dup T-KEY = if
         drop
         state bytes KEY-BUF KEY-CAP STR-INNER
         bytes KEY-BUF swap ka ku STR= if
            state NEXT-INNER drop TRUE exit
         then
         state NEXT-INNER drop
         state SKIP-INNER
      else
        T-END = if E-JR-EOF throw then
      then
   again ;

public

: SKIP-VALUE ( JR:reader -- JR:reader )
   READER>CELLS SKIP-INNER ;

: FIND-KEY ( JR:reader ptr u8 n -- JR:reader bool ) {: key:ptr len:n :}
   READER>STATE key len FIND-INNER ;

private

get-current prot-wid-add

public

get-current prot-wid-add

;package
