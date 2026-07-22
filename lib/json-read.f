\ json-read.f - checked zero-allocation JSON pull/cursor parser.
\
\ The parser is a cursor over the caller's source buffer and fixed storage. INIT
\ returns a linear reader; every operation threads that reader and CLOSE consumes
\ it. No allocation or process-global parser state is involved. Structural
\ well-formedness (RFC 8259: strict, no comments, no trailing commas, no single
\ quotes) is validated as the cursor advances; number/string decode is validated
\ on demand by JR:INT / JR:FLOAT / JR:STR. Every failure is a named throw in the
\ -3900..-3999 block from lib/errors.f.
\
\ The parser lives in `package JR`. External callers use the qualified public API
\ (JR:INIT, JR:CLOSE, JR:NEXT, JR:TOKEN, JR:SPAN$, JR:INT, JR:FLOAT, JR:STR,
\ JR:SKIP-VALUE, JR:FIND-KEY) and the qualified token kinds (JR:T-OBJ ..
\ JR:T-END). Callers allocate JR:STORAGE-BYTES at a cell-aligned address and keep
\ both storage and source live and exclusive until CLOSE. The byte constants,
\ opaque representation leaves, cursor state, and scan/decode helpers - including
\ the bounds-checked source reader JR-AT - are package-private.
\
\ Cursor state machine (JR-STATE): a value token drives one transition each.
\   ST-VALUE   value required (doc start / after ':' / after ',' in an array)
\   ST-ELEM    array just opened: a value or ']'
\   ST-KEY     object just opened: a key string or '}'
\   ST-MEMBER  after ',' in an object: a key string (no '}': trailing comma)
\   ST-SEP     value complete inside a container: ',' or the matching close
\   ST-DONE    top-level value complete: only whitespace then JR:T-END
\ A ',' in ST-SEP is consumed silently (JR-STEP returns JR-RETRY and NEXT loops);
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
8 constant JR-BS
9 constant JR-TAB
10 constant JR-LF
12 constant JR-FF
13 constant JR-CR
32 constant JR-SP
34 constant JR-DQ
43 constant JR-PLUS
44 constant JR-COMMA
45 constant JR-MINUS
46 constant JR-DOT
47 constant JR-SLASH
48 constant JR-ZERO
58 constant JR-COLON
69 constant JR-E-UPPER
91 constant JR-LBRACK
92 constant JR-BACKSLASH
93 constant JR-RBRACK
98 constant JR-CH-B
101 constant JR-E-LOWER
102 constant JR-CH-F
110 constant JR-CH-N
114 constant JR-CH-R
116 constant JR-CH-T
117 constant JR-CH-U
123 constant JR-LBRACE
125 constant JR-RBRACE

65 constant JR-HEXUP-A
70 constant JR-HEXUP-F
97 constant JR-HEXLO-A
102 constant JR-HEXLO-F
10 constant JR-HEX-TEN
16 constant JR-HEX-BASE

\ ---- unicode / utf-8 ------------------------------------------------------
$80 constant JR-UTF1-MAX
$800 constant JR-UTF2-MAX
$10000 constant JR-UTF3-MAX
$3F constant JR-UTF-MASK
$80 constant JR-UTF-CONT
$C0 constant JR-UTF2-LEAD
$E0 constant JR-UTF3-LEAD
$F0 constant JR-UTF4-LEAD
$D800 constant JR-SUR-HI
$DC00 constant JR-SUR-LO
$E000 constant JR-SUR-END
$10000 constant JR-SUR-BASE
10 constant JR-SUR-SHIFT

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

99 constant JR-RETRY

\ ---- parser state ---------------------------------------------------------
0 constant ST-VALUE
1 constant ST-ELEM
2 constant ST-KEY
3 constant ST-MEMBER
4 constant ST-SEP
5 constant ST-DONE

1 constant JR-CTX-OBJ
2 constant JR-CTX-ARR
64 constant JR-MAX-DEPTH
256 constant JR-KEY-CAP

16 constant JR-CELL-COUNT
JR-CELL-COUNT cells constant JR-CTX-OFF
JR-CTX-OFF JR-MAX-DEPTH + constant JR-KEY-OFF

public

JR-KEY-OFF JR-KEY-CAP + constant STORAGE-BYTES

private

\ ---- opaque reader representation boundary ------------------------------
\ These four private leaves are one audited abstraction. Checked INIT owns all
\ validation and initialization; only the representation refinements themselves
\ remain trusted until bounded host storage can express extent and lifetime.
TRUSTED: JR-STORAGE>PREMINT ( ptr a -- ptr n n )
   dup ;

TRUSTED: JR-MINT-READER ( ptr a -- JR:reader ) ;

TRUSTED: JR-READER>STATE ( JR:reader -- JR:reader ptr n ptr u8 )
   dup dup ;

TRUSTED: JR-CONSUME-READER ( JR:reader -- )
   drop ;

: JR-READER>CELLS ( JR:reader -- JR:reader ptr n )
   JR-READER>STATE drop ;

: JR-CELL ( ptr n n -- ptr n )
   cells + ;

: JR-SRC-FIELD ( ptr n -- ptr ptr u8 )
   0 ptr-field ;

: JR-SRC@ ( ptr n -- ptr u8 )
   JR-SRC-FIELD @ ;

: JR-SRC! ( ptr n ptr u8 -- )
   swap JR-SRC-FIELD ! ;

: JR-SRC-U@ ( ptr n -- n ) 1 JR-CELL @ ;
: JR-SRC-U! ( ptr n n -- ) swap 1 JR-CELL ! ;
: JR-POS@ ( ptr n -- n ) 2 JR-CELL @ ;
: JR-POS! ( ptr n n -- ) swap 2 JR-CELL ! ;
: JR-DEPTH@ ( ptr n -- n ) 3 JR-CELL @ ;
: JR-DEPTH! ( ptr n n -- ) swap 3 JR-CELL ! ;
: JR-STATE@ ( ptr n -- n ) 4 JR-CELL @ ;
: JR-STATE! ( ptr n n -- ) swap 4 JR-CELL ! ;
: JR-KIND@ ( ptr n -- n ) 5 JR-CELL @ ;
: JR-KIND! ( ptr n n -- ) swap 5 JR-CELL ! ;
: JR-TOK-OFF@ ( ptr n -- n ) 6 JR-CELL @ ;
: JR-TOK-OFF! ( ptr n n -- ) swap 6 JR-CELL ! ;
: JR-TOK-LEN@ ( ptr n -- n ) 7 JR-CELL @ ;
: JR-TOK-LEN! ( ptr n n -- ) swap 7 JR-CELL ! ;

: JR-UP-FIELD ( ptr n -- ptr ptr u8 ) 8 ptr-field ;
: JR-UP@ ( ptr n -- ptr u8 ) JR-UP-FIELD @ ;
: JR-UP! ( ptr n ptr u8 -- ) swap JR-UP-FIELD ! ;
: JR-UCAP@ ( ptr n -- n ) 9 JR-CELL @ ;
: JR-UCAP! ( ptr n n -- ) swap 9 JR-CELL ! ;
: JR-UO@ ( ptr n -- n ) 10 JR-CELL @ ;
: JR-UO! ( ptr n n -- ) swap 10 JR-CELL ! ;
: JR-UI@ ( ptr n -- n ) 11 JR-CELL @ ;
: JR-UI! ( ptr n n -- ) swap 11 JR-CELL ! ;
: JR-UE@ ( ptr n -- n ) 12 JR-CELL @ ;
: JR-UE! ( ptr n n -- ) swap 12 JR-CELL ! ;
: JR-HI@ ( ptr n -- n ) 13 JR-CELL @ ;
: JR-HI! ( ptr n n -- ) swap 13 JR-CELL ! ;
: JR-LO@ ( ptr n -- n ) 14 JR-CELL @ ;
: JR-LO! ( ptr n n -- ) swap 14 JR-CELL ! ;
: JR-NI@ ( ptr n -- n ) 15 JR-CELL @ ;
: JR-NI! ( ptr n n -- ) swap 15 JR-CELL ! ;

: JR-CTX-BYTE ( ptr n n -- ptr n )
   JR-CTX-OFF + + ;

: JR-KEY-BUF ( ptr u8 -- ptr u8 )
   JR-KEY-OFF + ;

: JR-CLEAR-STORAGE ( ptr n -- ) {: state:ptr :}
   STORAGE-BYTES 0 ?do 0 state i + c! loop ;

: JR-INIT-STATE ( ptr n ptr u8 n -- ) {: state:ptr source:ptr len:n :}
   state JR-CLEAR-STORAGE
   state source JR-SRC!
   state len JR-SRC-U!
   state 0 JR-POS!
   state 0 JR-DEPTH!
   state ST-VALUE JR-STATE!
   state T-END JR-KIND!
   state 0 JR-TOK-OFF!
   state 0 JR-TOK-LEN! ;

: JR-TRUE ( -- bool )
   0 0= ;

: JR-FALSE ( -- bool )
   JR-TRUE 0= ;

\ ---- source cursor --------------------------------------------------------
: JR-IN-BOUNDS? ( ptr n n -- bool ) {: state:ptr idx:n :}
   idx 0 >= idx state JR-SRC-U@ < and ;

: JR-AT ( ptr n n -- n ) {: state:ptr idx:n :}
   state idx JR-IN-BOUNDS? 0= if E-JR-BOUNDS throw then
   state JR-SRC@ idx + c@ ;

: JR-EOF? ( ptr n -- bool ) {: state:ptr :}
   state JR-POS@ state JR-SRC-U@ >= ;

: JR-PEEK ( ptr n -- n ) {: state:ptr :}
   state state JR-POS@ JR-AT ;

: JR-ADVANCE ( ptr n -- ) {: state:ptr :}
   state state JR-POS@ 1+ JR-POS! ;

public

: INIT ( ptr a n ptr u8 n -- JR:reader ) {: storage:ptr cap:n source:ptr len:n :}
   storage 0= if E-JR-STORAGE throw then
   storage JR-STORAGE>PREMINT
   CELL 1- and 0 <> if E-JR-STORAGE throw then
   cap STORAGE-BYTES < if E-JR-CAPACITY throw then
   len 0 < if E-JR-SOURCE throw then
   len 0 > source 0= and if E-JR-SOURCE throw then
   source len JR-INIT-STATE
   storage JR-MINT-READER ;

: CLOSE ( JR:reader -- )
   JR-CONSUME-READER ;

: TOKEN ( JR:reader -- JR:reader n )
   JR-READER>CELLS JR-KIND@ ;

: SPAN$ ( JR:reader -- JR:reader ptr u8 n )
   JR-READER>CELLS {: state:ptr :}
   state JR-SRC@ state JR-TOK-OFF@ + state JR-TOK-LEN@ ;

private

\ ---- whitespace -----------------------------------------------------------
: JR-WS? ( n -- bool )
   dup JR-SP = over JR-TAB = or over JR-LF = or swap JR-CR = or ;

: JR-SKIP-WS ( ptr n -- ) {: state:ptr :}
   begin state JR-EOF? 0= if state JR-PEEK JR-WS? else JR-FALSE then while
      state JR-ADVANCE
   repeat ;

\ ---- container stack ------------------------------------------------------
: JR-PUSH ( ptr n n -- ) {: state:ptr kind:n :}
   state JR-DEPTH@ JR-MAX-DEPTH >= if E-JR-DEPTH throw then
   kind state state JR-DEPTH@ JR-CTX-BYTE c!
   state state JR-DEPTH@ 1+ JR-DEPTH! ;

: JR-POP ( ptr n -- ) {: state:ptr :}
   state state JR-DEPTH@ 1- JR-DEPTH! ;

: JR-CUR-CTX ( ptr n -- n ) {: state:ptr :}
   state state JR-DEPTH@ 1- JR-CTX-BYTE c@ ;

: JR-AFTER-VALUE ( ptr n -- ) {: state:ptr :}
   state JR-DEPTH@ 0= if ST-DONE else ST-SEP then state swap JR-STATE! ;

\ ---- string token scanning ------------------------------------------------
: JR-SCAN-ESC ( ptr n -- ) {: state:ptr :}
   state JR-ADVANCE
   state JR-EOF? if E-JR-STRING throw then
   state JR-ADVANCE ;

: JR-SCAN-STRING ( ptr n -- ) {: state:ptr :}
   state JR-ADVANCE
   state state JR-POS@ JR-TOK-OFF!
   begin
      state JR-EOF? if E-JR-STRING throw then
      state JR-PEEK
      dup JR-DQ = if
         drop
         state state JR-POS@ state JR-TOK-OFF@ - JR-TOK-LEN!
         state JR-ADVANCE exit
      then
      dup JR-BACKSLASH = if
         drop state JR-SCAN-ESC
      else
         JR-SP < if E-JR-STRING throw then
         state JR-ADVANCE
      then
   again ;

\ ---- number token scanning + validation -----------------------------------
: JR-NUM-CHAR? ( n -- bool )
   dup STR-DIGIT? if drop JR-TRUE exit then
   dup JR-MINUS = if drop JR-TRUE exit then
   dup JR-PLUS = if drop JR-TRUE exit then
   dup JR-DOT = if drop JR-TRUE exit then
   dup JR-E-LOWER = if drop JR-TRUE exit then
   JR-E-UPPER = ;

: JR-SCAN-NUMBER ( ptr n -- ) {: state:ptr :}
   state state JR-POS@ JR-TOK-OFF!
   begin state JR-EOF? 0= if state JR-PEEK JR-NUM-CHAR? else JR-FALSE then while
      state JR-ADVANCE
   repeat
   state state JR-POS@ state JR-TOK-OFF@ - JR-TOK-LEN! ;

: JR-NV-DIGIT? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state JR-NI@ u >= if JR-FALSE exit then
   a state JR-NI@ + c@ STR-DIGIT? ;

: JR-NV-SKIP-DIGITS ( ptr n ptr u8 n -- n ) {: state:ptr a:ptr u:n :}
   0 begin state a u JR-NV-DIGIT? while
      state state JR-NI@ 1+ JR-NI!
      1+
   repeat ;

: JR-NV-INT? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state a u JR-NV-DIGIT? 0= if JR-FALSE exit then
   a state JR-NI@ + c@ JR-ZERO = if
      state state JR-NI@ 1+ JR-NI!
      state a u JR-NV-DIGIT? if JR-FALSE exit then
      JR-TRUE exit
   then
   state a u JR-NV-SKIP-DIGITS drop JR-TRUE ;

: JR-NV-FRAC? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state JR-NI@ u >= if JR-TRUE exit then
   a state JR-NI@ + c@ JR-DOT <> if JR-TRUE exit then
   state state JR-NI@ 1+ JR-NI!
   state a u JR-NV-SKIP-DIGITS 0= if JR-FALSE exit then
   JR-TRUE ;

: JR-NV-EXP? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   state JR-NI@ u >= if JR-TRUE exit then
   a state JR-NI@ + c@ dup JR-E-LOWER <> swap JR-E-UPPER <> and if JR-TRUE exit then
   state state JR-NI@ 1+ JR-NI!
   state JR-NI@ u < if
      a state JR-NI@ + c@ dup JR-PLUS = swap JR-MINUS = or if
         state state JR-NI@ 1+ JR-NI!
      then
   then
   state a u JR-NV-SKIP-DIGITS 0= if JR-FALSE exit then
   JR-TRUE ;

: JR-JSON-NUMBER? ( ptr n ptr u8 n -- bool ) {: state:ptr a:ptr u:n :}
   u 0= if JR-FALSE exit then
   state 0 JR-NI!
   a state JR-NI@ + c@ JR-MINUS = if state state JR-NI@ 1+ JR-NI! then
   state a u JR-NV-INT? 0= if JR-FALSE exit then
   state a u JR-NV-FRAC? 0= if JR-FALSE exit then
   state a u JR-NV-EXP? 0= if JR-FALSE exit then
   state JR-NI@ u = ;

: JR-SPAN$ ( ptr n -- ptr u8 n ) {: state:ptr :}
   state JR-SRC@ state JR-TOK-OFF@ + state JR-TOK-LEN@ ;

: JR-SPAN-HAS? ( ptr n n -- bool ) {: state:ptr c:n :}
   state JR-SPAN$ STR:LENGTH c STR:INDEX-OF MATCH option
     none OF JR-FALSE ENDOF
     some OF drop JR-TRUE ENDOF
   ;MATCH ;

: JR-NUM-FLOATY? ( ptr n -- bool ) {: state:ptr :}
   state JR-DOT JR-SPAN-HAS? if JR-TRUE exit then
   state JR-E-LOWER JR-SPAN-HAS? if JR-TRUE exit then
   state JR-E-UPPER JR-SPAN-HAS? ;

\ ---- literal tokens -------------------------------------------------------
: JR-MATCH-LIT? ( ptr n ptr u8 n -- bool ) {: state:ptr la:ptr lu:n :}
   state JR-POS@ lu + state JR-SRC-U@ > if JR-FALSE exit then
   0 begin dup lu < while
      dup la + c@ over state swap state JR-POS@ + JR-AT <> if
         drop JR-FALSE exit
      then
      1+
   repeat drop JR-TRUE ;

: JR-READ-LIT ( ptr n ptr u8 n n -- n ) {: state:ptr la:ptr lu:n kind:n :}
   state la lu JR-MATCH-LIT? 0= if E-JR-MALFORMED throw then
   state state JR-POS@ JR-TOK-OFF!
   state lu JR-TOK-LEN!
   state kind JR-KIND!
   state state JR-POS@ lu + JR-POS!
   state JR-AFTER-VALUE
   kind ;

: JR-READ-TRUE ( ptr n -- n )
   s" true" T-TRUE JR-READ-LIT ;

: JR-READ-FALSE ( ptr n -- n )
   s" false" T-FALSE JR-READ-LIT ;

: JR-READ-NULL ( ptr n -- n )
   s" null" T-NULL JR-READ-LIT ;

\ ---- value readers --------------------------------------------------------
: JR-OPEN-OBJ ( ptr n -- n ) {: state:ptr :}
   state state JR-POS@ JR-TOK-OFF!
   state 1 JR-TOK-LEN!
   state T-OBJ JR-KIND!
   state JR-ADVANCE
   state JR-CTX-OBJ JR-PUSH
   state ST-KEY JR-STATE!
   T-OBJ ;

: JR-OPEN-ARR ( ptr n -- n ) {: state:ptr :}
   state state JR-POS@ JR-TOK-OFF!
   state 1 JR-TOK-LEN!
   state T-ARR JR-KIND!
   state JR-ADVANCE
   state JR-CTX-ARR JR-PUSH
   state ST-ELEM JR-STATE!
   T-ARR ;

: JR-CLOSE-OBJ ( ptr n -- n ) {: state:ptr :}
   state state JR-POS@ JR-TOK-OFF!
   state 1 JR-TOK-LEN!
   state T-OBJ-END JR-KIND!
   state JR-ADVANCE
   state JR-POP
   state JR-AFTER-VALUE
   T-OBJ-END ;

: JR-CLOSE-ARR ( ptr n -- n ) {: state:ptr :}
   state state JR-POS@ JR-TOK-OFF!
   state 1 JR-TOK-LEN!
   state T-ARR-END JR-KIND!
   state JR-ADVANCE
   state JR-POP
   state JR-AFTER-VALUE
   T-ARR-END ;

: JR-READ-STRING ( ptr n -- n ) {: state:ptr :}
   state JR-SCAN-STRING
   state T-STR JR-KIND!
   state JR-AFTER-VALUE
   T-STR ;

: JR-READ-KEY ( ptr n -- n ) {: state:ptr :}
   state JR-SCAN-STRING
   state T-KEY JR-KIND!
   state JR-SKIP-WS
   state JR-EOF? if E-JR-COLON throw then
   state JR-PEEK JR-COLON <> if E-JR-COLON throw then
   state JR-ADVANCE
   state ST-VALUE JR-STATE!
   T-KEY ;

: JR-READ-NUMBER ( ptr n -- n ) {: state:ptr :}
   state JR-SCAN-NUMBER
   state state JR-SPAN$ JR-JSON-NUMBER? 0= if E-JR-NUMBER throw then
   state JR-NUM-FLOATY? if T-FLOAT else T-INT then state swap JR-KIND!
   state JR-AFTER-VALUE
   state JR-KIND@ ;

: JR-READ-VALUE ( ptr n -- n ) {: state:ptr :}
   state JR-PEEK {: c:n :}
   c JR-LBRACE = if state JR-OPEN-OBJ exit then
   c JR-LBRACK = if state JR-OPEN-ARR exit then
   c JR-DQ = if state JR-READ-STRING exit then
   c JR-CH-T = if state JR-READ-TRUE exit then
   c JR-CH-F = if state JR-READ-FALSE exit then
   c JR-CH-N = if state JR-READ-NULL exit then
   c JR-MINUS = if state JR-READ-NUMBER exit then
   c STR-DIGIT? if state JR-READ-NUMBER exit then
   E-JR-MALFORMED throw ;

\ ---- state dispatch -------------------------------------------------------
: JR-DO-VALUE ( ptr n -- n )
   JR-READ-VALUE ;

: JR-DO-ELEM ( ptr n -- n ) {: state:ptr :}
   state JR-PEEK JR-RBRACK = if state JR-CLOSE-ARR exit then
   state JR-READ-VALUE ;

: JR-DO-KEY ( ptr n -- n ) {: state:ptr :}
   state JR-PEEK JR-RBRACE = if state JR-CLOSE-OBJ exit then
   state JR-PEEK JR-DQ = if state JR-READ-KEY exit then
   E-JR-MALFORMED throw ;

: JR-DO-MEMBER ( ptr n -- n ) {: state:ptr :}
   state JR-PEEK JR-DQ = if state JR-READ-KEY exit then
   E-JR-MALFORMED throw ;

: JR-DO-SEP ( ptr n -- n ) {: state:ptr :}
   state JR-PEEK JR-COMMA = if
      state JR-ADVANCE
      state JR-CUR-CTX JR-CTX-OBJ = if ST-MEMBER else ST-VALUE then
      state swap JR-STATE!
      JR-RETRY exit
   then
   state JR-PEEK JR-RBRACE = state JR-CUR-CTX JR-CTX-OBJ = and if
      state JR-CLOSE-OBJ exit
   then
   state JR-PEEK JR-RBRACK = state JR-CUR-CTX JR-CTX-ARR = and if
      state JR-CLOSE-ARR exit
   then
   E-JR-COMMA throw ;

: JR-STEP ( ptr n -- n ) {: state:ptr :}
   state JR-SKIP-WS
   state JR-EOF? if
      state JR-STATE@ ST-DONE = if T-END exit then
      E-JR-EOF throw
   then
   state JR-STATE@ ST-DONE = if E-JR-TRAILING throw then
   state JR-STATE@ ST-VALUE = if state JR-DO-VALUE exit then
   state JR-STATE@ ST-ELEM = if state JR-DO-ELEM exit then
   state JR-STATE@ ST-KEY = if state JR-DO-KEY exit then
   state JR-STATE@ ST-MEMBER = if state JR-DO-MEMBER exit then
   state JR-DO-SEP ;

: JR-NEXT ( ptr n -- n ) {: state:ptr :}
   begin state JR-STEP dup JR-RETRY = while drop repeat ;

public

: NEXT ( JR:reader -- JR:reader n )
   JR-READER>CELLS JR-NEXT ;

private

\ ---- number value decode --------------------------------------------------
: JR-INT ( ptr n -- n ) {: state:ptr :}
   state JR-KIND@ T-INT <> if E-JR-STATE throw then
   state JR-SPAN$ STR>NUMBER? MATCH option
     none OF E-JR-NUMBER throw ENDOF
     some OF ENDOF
   ;MATCH ;

: JR-FLOAT ( ptr n -- r ) {: state:ptr :}
   state JR-KIND@ dup T-INT = swap T-FLOAT = or 0= if E-JR-STATE throw then
   state JR-SPAN$ STR>FLOAT MATCH option
     none OF E-JR-NUMBER throw ENDOF
     some OF ENDOF
   ;MATCH ;

public

: INT ( JR:reader -- JR:reader n )
   JR-READER>CELLS JR-INT ;

: FLOAT ( JR:reader -- JR:reader r )
   JR-READER>CELLS JR-FLOAT ;

private

\ ---- string value decode (unescape into caller buffer) --------------------
: JR-SPAN-AT ( ptr n n -- n ) {: state:ptr idx:n :}
   state state JR-TOK-OFF@ idx + JR-AT ;

: JR-UNESC-CHAR ( ptr n -- n ) {: state:ptr :}
   state state JR-UI@ JR-SPAN-AT ;

: JR-EMIT ( ptr n n -- ) {: state:ptr byte:n :}
   state JR-UO@ state JR-UCAP@ >= if E-JR-STATE throw then
   byte state JR-UP@ state JR-UO@ + c!
   state state JR-UO@ 1+ JR-UO! ;

: JR-HEX-VAL ( n -- n bool )
   dup STR-DIGIT? if STR-DIGIT-VALUE JR-TRUE exit then
   dup JR-HEXUP-A >= over JR-HEXUP-F <= and if JR-HEXUP-A - JR-HEX-TEN + JR-TRUE exit then
   dup JR-HEXLO-A >= over JR-HEXLO-F <= and if JR-HEXLO-A - JR-HEX-TEN + JR-TRUE exit then
   drop 0 JR-FALSE ;

: JR-HEX-NEXT ( ptr n n -- n ) {: state:ptr value:n :}
   value JR-HEX-BASE *
   state state JR-UI@ JR-SPAN-AT JR-HEX-VAL 0= if E-JR-ESCAPE throw then
   +
   state state JR-UI@ 1+ JR-UI! ;

: JR-READ-HEX4 ( ptr n -- n ) {: state:ptr :}
   state JR-UI@ 4 + state JR-TOK-LEN@ > if E-JR-ESCAPE throw then
   state 0 JR-HEX-NEXT
   state swap JR-HEX-NEXT
   state swap JR-HEX-NEXT
   state swap JR-HEX-NEXT ;

: JR-EMIT-UTF8 ( ptr n n -- ) {: state:ptr cp:n :}
   cp JR-UTF1-MAX < if state cp JR-EMIT exit then
   cp JR-UTF2-MAX < if
      state cp 6 rshift JR-UTF2-LEAD or JR-EMIT
      state cp JR-UTF-MASK and JR-UTF-CONT or JR-EMIT exit
   then
   cp JR-UTF3-MAX < if
      state cp 12 rshift JR-UTF3-LEAD or JR-EMIT
      state cp 6 rshift JR-UTF-MASK and JR-UTF-CONT or JR-EMIT
      state cp JR-UTF-MASK and JR-UTF-CONT or JR-EMIT exit
   then
   state cp 18 rshift JR-UTF4-LEAD or JR-EMIT
   state cp 12 rshift JR-UTF-MASK and JR-UTF-CONT or JR-EMIT
   state cp 6 rshift JR-UTF-MASK and JR-UTF-CONT or JR-EMIT
   state cp JR-UTF-MASK and JR-UTF-CONT or JR-EMIT ;

: JR-UNI ( ptr n -- ) {: state:ptr :}
   state state JR-READ-HEX4 JR-HI!
   state JR-HI@ JR-SUR-HI < if state state JR-HI@ JR-EMIT-UTF8 exit then
   state JR-HI@ JR-SUR-END >= if state state JR-HI@ JR-EMIT-UTF8 exit then
   state JR-HI@ JR-SUR-LO >= if E-JR-SURROGATE throw then
   state JR-UI@ 2 + state JR-TOK-LEN@ > if E-JR-SURROGATE throw then
   state state JR-UI@ JR-SPAN-AT JR-BACKSLASH <> if E-JR-SURROGATE throw then
   state state JR-UI@ 1+ JR-SPAN-AT JR-CH-U <> if E-JR-SURROGATE throw then
   state state JR-UI@ 2 + JR-UI!
   state state JR-READ-HEX4 JR-LO!
   state JR-LO@ JR-SUR-LO < if E-JR-SURROGATE throw then
   state JR-LO@ JR-SUR-END >= if E-JR-SURROGATE throw then
   state JR-SUR-BASE
   state JR-HI@ JR-SUR-HI - JR-SUR-SHIFT lshift +
   state JR-LO@ JR-SUR-LO - +
   JR-EMIT-UTF8 ;

: JR-ESC-BYTE ( ptr n n -- ) {: state:ptr esc:n :}
   esc JR-DQ = if state JR-DQ JR-EMIT exit then
   esc JR-BACKSLASH = if state JR-BACKSLASH JR-EMIT exit then
   esc JR-SLASH = if state JR-SLASH JR-EMIT exit then
   esc JR-CH-B = if state JR-BS JR-EMIT exit then
   esc JR-CH-F = if state JR-FF JR-EMIT exit then
   esc JR-CH-N = if state JR-LF JR-EMIT exit then
   esc JR-CH-R = if state JR-CR JR-EMIT exit then
   esc JR-CH-T = if state JR-TAB JR-EMIT exit then
   E-JR-ESCAPE throw ;

: JR-UNESC-STEP ( ptr n -- ) {: state:ptr :}
   state JR-UNESC-CHAR JR-BACKSLASH <> if
      state state JR-UNESC-CHAR JR-EMIT
      state state JR-UI@ 1+ JR-UI! exit
   then
   state JR-UI@ 1+ state JR-TOK-LEN@ >= if E-JR-ESCAPE throw then
   state state state JR-UI@ 1+ JR-SPAN-AT JR-UE!
   state state JR-UI@ 2 + JR-UI!
   state JR-UE@ JR-CH-U = if state JR-UNI exit then
   state state JR-UE@ JR-ESC-BYTE ;

: JR-STR ( ptr n ptr u8 n -- n ) {: state:ptr dst:ptr cap:n :}
   state JR-KIND@ dup T-STR = swap T-KEY = or 0= if E-JR-STATE throw then
   state dst JR-UP!
   state cap JR-UCAP!
   state 0 JR-UI!
   state 0 JR-UO!
   begin state JR-UI@ state JR-TOK-LEN@ < while
      state JR-UNESC-STEP
   repeat
   state JR-UO@ ;

public

: STR ( JR:reader ptr u8 n -- JR:reader n ) {: dst:ptr cap:n :}
   JR-READER>CELLS dst cap JR-STR ;

private

\ ---- skip / find ----------------------------------------------------------
: JR-SCALAR? ( n -- bool )
   dup T-STR = over T-INT = or over T-FLOAT = or
   over T-TRUE = or over T-FALSE = or swap T-NULL = or ;

: JR-OPENER? ( n -- bool )
   dup T-OBJ = swap T-ARR = or ;

: JR-CLOSER? ( n -- bool )
   dup T-OBJ-END = swap T-ARR-END = or ;

: JR-SKIP-VALUE ( ptr n -- ) {: state:ptr :}
   state JR-KIND@ JR-SCALAR? if exit then
   state JR-KIND@ JR-OPENER? 0= if E-JR-STATE throw then
   1 begin dup 0 > while
      state JR-NEXT
      dup JR-OPENER? if drop 1+ else
      dup JR-CLOSER? if drop 1- else
      dup T-END = if E-JR-EOF throw else drop then then then
   repeat drop ;

: JR-FIND-KEY ( ptr n ptr u8 ptr u8 n -- bool ) {: state:ptr bytes:ptr ka:ptr ku:n :}
   begin
      state JR-NEXT
      dup T-OBJ-END = if drop JR-FALSE exit then
      dup T-KEY = if
         drop
         state bytes JR-KEY-BUF JR-KEY-CAP JR-STR
         bytes JR-KEY-BUF swap ka ku STR= if
            state JR-NEXT drop JR-TRUE exit
         then
         state JR-NEXT drop
         state JR-SKIP-VALUE
      else
         T-END = if E-JR-EOF throw then
      then
   again ;

public

: SKIP-VALUE ( JR:reader -- JR:reader )
   JR-READER>CELLS JR-SKIP-VALUE ;

: FIND-KEY ( JR:reader ptr u8 n -- JR:reader bool ) {: key:ptr len:n :}
   JR-READER>STATE key len JR-FIND-KEY ;

;package
