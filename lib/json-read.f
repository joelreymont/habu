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

0 constant UMODE-WRITE
1 constant UMODE-MATCH

18 constant STATE-CELLS
STATE-CELLS cells constant CTX-OFF

0 constant SRC-IDX
1 cells constant SRC-U-OFF
2 cells constant POS-OFF
3 cells constant DEPTH-OFF
4 cells constant STATE-OFF
5 cells constant KIND-OFF
6 cells constant TOK-AT-OFF
7 cells constant TOK-LEN-OFF
8 constant UP-IDX
9 cells constant UCAP-OFF
10 cells constant UO-OFF
11 cells constant UI-OFF
12 cells constant UE-OFF
13 cells constant HI-OFF
14 cells constant LO-OFF
15 cells constant NI-OFF
16 cells constant UMODE-OFF
17 cells constant UMATCH-OFF

\ The container stack is one CELL per open container, not one byte: the whole
\ storage block is a cell block (`ptr n`), so a byte-packed stack would need the
\ block read back as `ptr u8` at a cell address, which the pointee rule refuses.
CTX-OFF MAX-DEPTH cells + constant STORAGE-SIZE

public

: STORAGE-BYTES ( -- n )
   STORAGE-SIZE ;

private

\ ---- opaque reader representation boundary ------------------------------
\ STORAGE>PREMINT, READER>STATE, and CONSUME-READER are one audited abstraction.
\ Checked INIT owns validation and initialization; only those representation
\ refinements remain trusted until bounded storage can express extent and lifetime.
\ Retirement owner: cap:raw-pointer-lifetime.
\ Expose cell-state and numeric-address views for checked pre-mint validation.
TRUSTED: STORAGE>PREMINT ( ptr a -- ptr n n )
   dup ;

TRUSTED: MINT-READER ( ptr a -- JR:reader ) ;

\ Keep the linear token live while exposing views of its same backing storage.
TRUSTED: READER>STATE ( JR:reader -- JR:reader ptr n ptr u8 )
   dup dup ;

\ CLOSE consumes the token but never frees caller-owned storage.
TRUSTED: CONSUME-READER ( JR:reader -- )
   drop ;

: CTX-SLOT ( ptr n n -- ptr n )   \ ( state depth -- cell address of that stack slot )
   cells CTX-OFF + + ;

: INIT-STATE ( ptr n ptr u8 n -- ) {: state:ptr source:ptr len:n :}
   source state SRC-IDX ptr-field !
   len state SRC-U-OFF + !
   0 state POS-OFF + !
   0 state DEPTH-OFF + !
   ST-VALUE state STATE-OFF + !
   T-END state KIND-OFF + !
   0 state TOK-AT-OFF + !
   0 state TOK-LEN-OFF + !
   UMODE-WRITE state UMODE-OFF + !
   1 state UMATCH-OFF + ! ;

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

\ ---- source cursor --------------------------------------------------------
: IN-BOUNDS? ( ptr n n -- bool ) {: state:ptr idx:n :}
   idx 0 >= idx state SRC-U-OFF + @ < and ;

: AT ( ptr n n -- n ) {: state:ptr idx:n :}
   state idx IN-BOUNDS? 0= if E-JR-BOUNDS throw then
   state SRC-IDX ptr-field @ idx + c@ ;

: EOF? ( ptr n -- bool ) {: state:ptr :}
   state POS-OFF + @ state SRC-U-OFF + @ >= ;

: PEEK ( ptr n -- n ) {: state:ptr :}
   state state POS-OFF + @ AT ;

: ADVANCE ( ptr n -- ) {: state:ptr :}
   state POS-OFF + dup @ 1+ swap ! ;

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
   READER>STATE drop KIND-OFF + @ ;

: SPAN$ ( JR:reader -- JR:reader ptr u8 n )
   READER>STATE drop {: state:ptr :}
   state SRC-IDX ptr-field @ state TOK-AT-OFF + @ +
   state TOK-LEN-OFF + @ ;

private

\ ---- whitespace -----------------------------------------------------------
: WS? ( n -- bool )
   dup SP = over TAB = or over LF = or swap CR = or ;

: SKIP-WS ( ptr n -- ) {: state:ptr :}
   begin
      state POS-OFF + @ state SRC-U-OFF + @ < if
         state SRC-IDX ptr-field @ state POS-OFF + @ + c@ WS?
      else
         FALSE
      then
   while
      state POS-OFF + dup @ 1+ swap !
   repeat ;

\ ---- container stack ------------------------------------------------------
: PUSH ( ptr n n -- ) {: state:ptr kind:n :}
   state DEPTH-OFF + @ MAX-DEPTH >= if E-JR-DEPTH throw then
   kind state state DEPTH-OFF + @ CTX-SLOT !
   state DEPTH-OFF + dup @ 1+ swap ! ;

: POP ( ptr n -- ) {: state:ptr :}
   state DEPTH-OFF + dup @ 1- swap ! ;

: CUR-CTX ( ptr n -- n ) {: state:ptr :}
   state state DEPTH-OFF + @ 1- CTX-SLOT @ ;

: AFTER-VALUE ( ptr n -- ) {: state:ptr :}
   state DEPTH-OFF + @ 0= if ST-DONE else ST-SEP then
   state STATE-OFF + ! ;

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
   state POS-OFF + @ count + state SRC-U-OFF + @ > if E-JR-STRING throw then ;

: CONT? ( n -- bool )
   $C0 and UTF-CONT = ;

: UTF-BYTE ( ptr n n -- n ) {: state:ptr off:n :}
   state state POS-OFF + @ off + AT ;

: ADVANCE-N ( ptr n n -- ) {: state:ptr count:n :}
   state POS-OFF + dup @ count + swap ! ;

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
   state POS-OFF + @ state TOK-AT-OFF + !
   begin
      state EOF? if E-JR-STRING throw then
      state PEEK
      dup DQ = if
         drop
         state POS-OFF + @ state TOK-AT-OFF + @ -
         state TOK-LEN-OFF + !
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
   state POS-OFF + @ state TOK-AT-OFF + !
   begin
      state POS-OFF + @ state SRC-U-OFF + @ < if
         state SRC-IDX ptr-field @ state POS-OFF + @ + c@ NUM-CHAR?
      else
         FALSE
      then
   while
      state POS-OFF + dup @ 1+ swap !
   repeat
   state POS-OFF + @ state TOK-AT-OFF + @ -
   state TOK-LEN-OFF + ! ;

: NV-DIGIT? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state NI-OFF + @ u >= if FALSE exit then
   a state NI-OFF + @ + c@ STR-DIGIT? ;

: NV-SKIP-DIGITS ( ptr n ptr u8 n -- n ) {: state:ptr a:ptr u:n :}
   0 begin state a u NV-DIGIT? while
      state NI-OFF + dup @ 1+ swap !
      1+
   repeat ;

: NV-INT? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state a u NV-DIGIT? 0= if FALSE exit then
   a state NI-OFF + @ + c@ ZERO = if
      state NI-OFF + dup @ 1+ swap !
      state a u NV-DIGIT? if FALSE exit then
      TRUE exit
   then
   state a u NV-SKIP-DIGITS drop TRUE ;

: NV-FRAC? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state NI-OFF + @ u >= if TRUE exit then
   a state NI-OFF + @ + c@ DOT <> if TRUE exit then
   T-FLOAT state KIND-OFF + !
   state NI-OFF + dup @ 1+ swap !
   state a u NV-SKIP-DIGITS 0= if FALSE exit then
   TRUE ;

: NV-EXP? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state NI-OFF + @ u >= if TRUE exit then
   a state NI-OFF + @ + c@ dup E-LOWER <> swap E-UPPER <> and if TRUE exit then
   T-FLOAT state KIND-OFF + !
   state NI-OFF + dup @ 1+ swap !
   state NI-OFF + @ u < if
      a state NI-OFF + @ + c@ dup PLUS = swap MINUS = or if
         state NI-OFF + dup @ 1+ swap !
      then
   then
   state a u NV-SKIP-DIGITS 0= if FALSE exit then
   TRUE ;

: JSON-NUMBER? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   u 0= if FALSE exit then
   0 state NI-OFF + !
   a state NI-OFF + @ + c@ MINUS = if
      state NI-OFF + dup @ 1+ swap !
   then
   state a u NV-INT? 0= if FALSE exit then
   state a u NV-FRAC? 0= if FALSE exit then
   state a u NV-EXP? 0= if FALSE exit then
   state NI-OFF + @ u = ;

: RAW-SPAN$ ( ptr n -- ptr u8 n ) {: state:ptr :}
   state SRC-IDX ptr-field @ state TOK-AT-OFF + @ +
   state TOK-LEN-OFF + @ ;

\ ---- literal tokens -------------------------------------------------------
: MATCH-LIT? ( ptr n ptr u8 n -- bool ) {: state:ptr la:ptr lu:n :}
   state POS-OFF + @ lu + state SRC-U-OFF + @ > if FALSE exit then
   0 begin dup lu < while
      dup la + c@
      over state SRC-IDX ptr-field @ state POS-OFF + @ + + c@ <> if
         drop FALSE exit
      then
      1+
   repeat drop TRUE ;

: READ-LIT ( ptr n ptr u8 n n -- n ) {: state:ptr la:ptr lu:n kind:n :}
   state la lu MATCH-LIT? 0= if E-JR-MALFORMED throw then
   state POS-OFF + @ state TOK-AT-OFF + !
   lu state TOK-LEN-OFF + !
   kind state KIND-OFF + !
   state POS-OFF + dup @ lu + swap !
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
   state POS-OFF + @ state TOK-AT-OFF + !
   1 state TOK-LEN-OFF + !
   T-OBJ state KIND-OFF + !
   state POS-OFF + dup @ 1+ swap !
   state CTX-OBJ PUSH
   ST-KEY state STATE-OFF + !
   T-OBJ ;

: OPEN-ARR ( ptr n -- n ) {: state:ptr :}
   state POS-OFF + @ state TOK-AT-OFF + !
   1 state TOK-LEN-OFF + !
   T-ARR state KIND-OFF + !
   state POS-OFF + dup @ 1+ swap !
   state CTX-ARR PUSH
   ST-ELEM state STATE-OFF + !
   T-ARR ;

: CLOSE-OBJ ( ptr n -- n ) {: state:ptr :}
   state POS-OFF + @ state TOK-AT-OFF + !
   1 state TOK-LEN-OFF + !
   T-OBJ-END state KIND-OFF + !
   state POS-OFF + dup @ 1+ swap !
   state POP
   state AFTER-VALUE
   T-OBJ-END ;

: CLOSE-ARR ( ptr n -- n ) {: state:ptr :}
   state POS-OFF + @ state TOK-AT-OFF + !
   1 state TOK-LEN-OFF + !
   T-ARR-END state KIND-OFF + !
   state POS-OFF + dup @ 1+ swap !
   state POP
   state AFTER-VALUE
   T-ARR-END ;

: READ-STRING ( ptr n -- n ) {: state:ptr :}
   state SCAN-STRING
   T-STR state KIND-OFF + !
   state AFTER-VALUE
   T-STR ;

: READ-KEY ( ptr n -- n ) {: state:ptr :}
   state SCAN-STRING
   T-KEY state KIND-OFF + !
   state SKIP-WS
   state POS-OFF + @ state SRC-U-OFF + @ >= if E-JR-COLON throw then
   state SRC-IDX ptr-field @ state POS-OFF + @ + c@ COLON <> if
      E-JR-COLON throw
   then
   state POS-OFF + dup @ 1+ swap !
   ST-VALUE state STATE-OFF + !
   T-KEY ;

: READ-NUMBER ( ptr n -- n ) {: state:ptr :}
   state SCAN-NUMBER
   state KIND-OFF + @ {: prior:n :}
   T-INT state KIND-OFF + !
   state state RAW-SPAN$ JSON-NUMBER? 0= if
      prior state KIND-OFF + !
      E-JR-NUMBER throw
   then
   state AFTER-VALUE
   state KIND-OFF + @ ;

: READ-VALUE ( ptr n n -- n ) {: state:ptr c:n :}
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
: DO-ELEM ( ptr n n -- n ) {: state:ptr c:n :}
   c RBRACK = if state CLOSE-ARR exit then
   state c READ-VALUE ;

: DO-KEY ( ptr n n -- n ) {: state:ptr c:n :}
   c RBRACE = if state CLOSE-OBJ exit then
   c DQ = if state READ-KEY exit then
   E-JR-MALFORMED throw ;

: DO-MEMBER ( ptr n n -- n ) {: state:ptr c:n :}
   c DQ = if state READ-KEY exit then
   E-JR-MALFORMED throw ;

: DO-SEP ( ptr n n -- n ) {: state:ptr c:n :}
   c COMMA = if
      state POS-OFF + dup @ 1+ swap !
      state CUR-CTX CTX-OBJ = if ST-MEMBER else ST-VALUE then
      state STATE-OFF + !
      RETRY exit
   then
   c RBRACE = state CUR-CTX CTX-OBJ = and if
      state CLOSE-OBJ exit
   then
   c RBRACK = state CUR-CTX CTX-ARR = and if
      state CLOSE-ARR exit
   then
   E-JR-COMMA throw ;

: PUBLISH-END ( ptr n -- n ) {: state:ptr :}
   T-END state KIND-OFF + !
   state POS-OFF + @ state TOK-AT-OFF + !
   0 state TOK-LEN-OFF + !
   T-END ;

: STEP ( ptr n -- n ) {: state:ptr :}
   state SKIP-WS
   state POS-OFF + @ state SRC-U-OFF + @ >= if
      state STATE-OFF + @ ST-DONE = if state PUBLISH-END exit then
     E-JR-EOF throw
   then
   state SRC-IDX ptr-field @ state POS-OFF + @ + c@ {: c:n :}
   state STATE-OFF + @ ST-DONE = if E-JR-TRAILING throw then
   state STATE-OFF + @ ST-VALUE = if state c READ-VALUE exit then
   state STATE-OFF + @ ST-ELEM = if state c DO-ELEM exit then
   state STATE-OFF + @ ST-KEY = if state c DO-KEY exit then
   state STATE-OFF + @ ST-MEMBER = if state c DO-MEMBER exit then
   state c DO-SEP ;

: NEXT-INNER ( ptr n -- n ) {: state:ptr :}
   begin state STEP dup RETRY = while drop repeat ;

public

: NEXT ( JR:reader -- JR:reader n )
   READER>STATE drop NEXT-INNER ;

private

\ ---- number value decode --------------------------------------------------
: INT-INNER ( ptr n -- n ) {: state:ptr :}
   state KIND-OFF + @ T-INT <> if E-JR-STATE throw then
   state RAW-SPAN$ STR>NUMBER? MATCH option
     none OF E-JR-NUMBER throw ENDOF
     some OF ENDOF
   ;MATCH ;

: FLOAT-INNER ( ptr n -- r ) {: state:ptr :}
   state KIND-OFF + @ dup T-INT = swap T-FLOAT = or 0= if E-JR-STATE throw then
   state RAW-SPAN$ STR>FLOAT MATCH option
     none OF E-JR-NUMBER throw ENDOF
     some OF ENDOF
   ;MATCH ;

public

: INT ( JR:reader -- JR:reader n )
   READER>STATE drop INT-INNER ;

: FLOAT ( JR:reader -- JR:reader r )
   READER>STATE drop FLOAT-INNER ;

private

\ ---- string value decode (unescape into caller buffer) --------------------
: SPAN-AT ( ptr n n -- n ) {: state:ptr idx:n :}
   state state TOK-AT-OFF + @ idx + AT ;

: UNESC-CHAR ( ptr n -- n ) {: state:ptr :}
   state state UI-OFF + @ SPAN-AT ;

: WRITE-BYTE ( ptr n n -- ) {: state:ptr byte:n :}
   state UO-OFF + @ state UCAP-OFF + @ >= if E-JR-STATE throw then
   byte state UP-IDX ptr-field @ state UO-OFF + @ + c! ;

: MATCH-BYTE ( ptr n n -- ) {: state:ptr byte:n :}
   state UMATCH-OFF + @ 0= if exit then
   state UO-OFF + @ state UCAP-OFF + @ >= if
      0 state UMATCH-OFF + ! exit
   then
   state UP-IDX ptr-field @ state UO-OFF + @ + c@ byte <> if
      0 state UMATCH-OFF + !
   then ;

: EMIT-BYTE ( ptr n n -- ) {: state:ptr byte:n :}
   state UMODE-OFF + @ UMODE-MATCH = if
      state byte MATCH-BYTE
   else
      state byte WRITE-BYTE
   then
   state UO-OFF + dup @ 1+ swap ! ;

: HEX-NEXT ( ptr n n -- n ) {: state:ptr value:n :}
   value HEX-BASE *
   state state UI-OFF + @ SPAN-AT HEX-VAL 0= if E-JR-ESCAPE throw then
   +
   state UI-OFF + dup @ 1+ swap ! ;

: READ-HEX4 ( ptr n -- n ) {: state:ptr :}
   state UI-OFF + @ 4 + state TOK-LEN-OFF + @ > if E-JR-ESCAPE throw then
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
   state READ-HEX4 state HI-OFF + !
   state HI-OFF + @ SUR-HI < if
      state state HI-OFF + @ EMIT-UTF8 exit
   then
   state HI-OFF + @ SUR-END >= if
      state state HI-OFF + @ EMIT-UTF8 exit
   then
   state HI-OFF + @ SUR-LO >= if E-JR-SURROGATE throw then
   state UI-OFF + @ 2 + state TOK-LEN-OFF + @ > if E-JR-SURROGATE throw then
   state state UI-OFF + @ SPAN-AT BACKSLASH <> if E-JR-SURROGATE throw then
   state state UI-OFF + @ 1+ SPAN-AT CH-U <> if E-JR-SURROGATE throw then
   state UI-OFF + dup @ 2 + swap !
   state READ-HEX4 state LO-OFF + !
   state LO-OFF + @ SUR-LO < if E-JR-SURROGATE throw then
   state LO-OFF + @ SUR-END >= if E-JR-SURROGATE throw then
   state SUR-BASE
   state HI-OFF + @ SUR-HI - SUR-SHIFT lshift +
   state LO-OFF + @ SUR-LO - +
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
      state UI-OFF + dup @ 1+ swap ! exit
   then
   state UI-OFF + @ 1+ state TOK-LEN-OFF + @ >= if E-JR-ESCAPE throw then
   state state UI-OFF + @ 1+ SPAN-AT state UE-OFF + !
   state UI-OFF + dup @ 2 + swap !
   state UE-OFF + @ CH-U = if state UNI exit then
   state state UE-OFF + @ ESC-BYTE ;

: UNESC-RESET ( ptr n ptr u8 n n -- ) {: state:ptr dst:ptr cap:n mode:n :}
   dst state UP-IDX ptr-field !
   cap state UCAP-OFF + !
   0 state UI-OFF + !
   0 state UO-OFF + !
   mode state UMODE-OFF + !
   1 state UMATCH-OFF + ! ;

: UNESC-ALL ( ptr n -- ) {: state:ptr :}
   begin state UI-OFF + @ state TOK-LEN-OFF + @ < while
      state UNESC-STEP
   repeat ;

: STR-INNER ( ptr n ptr u8 n -- n ) {: state:ptr dst:ptr cap:n :}
   state KIND-OFF + @ dup T-STR = swap T-KEY = or 0= if E-JR-STATE throw then
   dst 0= cap 0 < or if E-JR-STATE throw then
   state dst cap UMODE-WRITE UNESC-RESET
   state UNESC-ALL
   state UO-OFF + @ ;

: MATCH-INNER ( ptr n ptr u8 n -- bool ) {: state:ptr key:ptr len:n :}
   state KIND-OFF + @ T-KEY <> if E-JR-STATE throw then
   state key len UMODE-MATCH UNESC-RESET
   state UNESC-ALL
   state UMATCH-OFF + @ 0= 0=
   state UO-OFF + @ len = and ;

public

: STR ( JR:reader ptr u8 n -- JR:reader n ) {: dst:ptr cap:n :}
   READER>STATE drop dst cap STR-INNER ;

private

\ ---- skip / find ----------------------------------------------------------
: SCALAR? ( n -- bool )
   dup T-STR = over T-INT = or over T-FLOAT = or
   over T-TRUE = or over T-FALSE = or swap T-NULL = or ;

: OPENER? ( n -- bool )
   dup T-OBJ = swap T-ARR = or ;

: CLOSER? ( n -- bool )
   dup T-OBJ-END = swap T-ARR-END = or ;

: VALUE-TOKEN? ( n -- bool )
   dup SCALAR? swap OPENER? or ;

: SKIP-INNER ( ptr n -- ) {: state:ptr :}
   state KIND-OFF + @ SCALAR? if exit then
   state KIND-OFF + @ OPENER? 0= if E-JR-STATE throw then
   1 begin dup 0 > while
      state NEXT-INNER
      dup OPENER? if drop 1+ else
      dup CLOSER? if drop 1- else
      dup T-END = if E-JR-EOF throw else drop then then then
   repeat drop ;

: FIND-PHASE? ( n -- bool )
   dup ST-KEY = swap ST-SEP = or ;

: REQUIRE-KEY-SPAN ( ptr u8 n -- ) {: key:ptr len:n :}
   len 0 < if E-JR-STATE throw then
   len 0 > key 0= and if E-JR-STATE throw then ;

: FIND-ANCHOR ( ptr n -- n ) {: state:ptr :}
   state DEPTH-OFF + @ dup 0 <= if E-JR-STATE throw then
   state CUR-CTX CTX-OBJ <> if E-JR-STATE throw then
   state STATE-OFF + @ FIND-PHASE? 0= if E-JR-STATE throw then ;

: REQUIRE-KEY-DEPTH ( ptr n n -- ) {: state:ptr depth:n :}
   state DEPTH-OFF + @ depth <> if E-JR-STATE throw then ;

: FIND-INNER ( ptr n ptr u8 n -- bool ) {: state:ptr key:ptr len:n :}
   key len REQUIRE-KEY-SPAN
   state FIND-ANCHOR {: depth:n :}
   begin
      state NEXT-INNER {: token:n :}
      token T-OBJ-END = if
         state depth 1- REQUIRE-KEY-DEPTH
         FALSE exit
      then
      token T-KEY <> if E-JR-STATE throw then
      state depth REQUIRE-KEY-DEPTH
      state key len MATCH-INNER {: found:bool :}
      state NEXT-INNER {: value:n :}
      value VALUE-TOKEN? 0= if E-JR-STATE throw then
      found if TRUE exit then
      state SKIP-INNER
      state depth REQUIRE-KEY-DEPTH
      state STATE-OFF + @ ST-SEP <> if E-JR-STATE throw then
   again ;

public

: SKIP-VALUE ( JR:reader -- JR:reader )
   READER>STATE drop SKIP-INNER ;

: FIND-KEY ( JR:reader ptr u8 n -- JR:reader bool ) {: key:ptr len:n :}
   READER>STATE drop key len FIND-INNER ;

private

get-current prot-wid-add

public

get-current prot-wid-add

;package
