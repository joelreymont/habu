\ json-read.f - checked zero-allocation JSON pull/cursor parser.
\
\ Load after lib/errors.f, lib/string.f, and lib/float.f. The parser is a cursor
\ over the caller's source buffer (no allocation): JR-INIT ( ptr u8 n -- ) points
\ it at the bytes, JR-NEXT ( -- kind ) advances to and returns the next typed
\ token, and the span/value accessors read the current token in place. Structural
\ well-formedness (RFC 8259: strict, no comments, no trailing commas, no single
\ quotes) is validated as the cursor advances; number/string decode is validated
\ on demand by JR-INT/JR-FLOAT/JR-STR. Every failure is a named throw in the
\ -3900..-3999 block from lib/errors.f.
\
\ Cursor state machine (JR-STATE): a value token drives one transition each.
\   ST-VALUE   value required (doc start / after ':' / after ',' in an array)
\   ST-ELEM    array just opened: a value or ']'
\   ST-KEY     object just opened: a key string or '}'
\   ST-MEMBER  after ',' in an object: a key string (no '}': trailing comma)
\   ST-SEP     value complete inside a container: ',' or the matching close
\   ST-DONE    top-level value complete: only whitespace then JT-END
\ A ',' in ST-SEP is consumed silently (JR-STEP returns JR-RETRY and JR-NEXT
\ loops); container closes are emitted as JT-OBJ-END / JT-ARR-END.

require lib/adt/option.f                 \ option<idx> INDEX-OF consumer (switchover wave A)

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

\ ---- token kinds ----------------------------------------------------------
0 constant JT-OBJ
1 constant JT-OBJ-END
2 constant JT-ARR
3 constant JT-ARR-END
4 constant JT-KEY
5 constant JT-STR
6 constant JT-INT
7 constant JT-FLOAT
8 constant JT-TRUE
9 constant JT-FALSE
10 constant JT-NULL
11 constant JT-END
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

variable JR-SRC-P                            \ typed source-pointer field holder
variable JR-SRC-U                            \ source length
variable JR-POS                              \ read cursor (0-based)
variable JR-DEPTH                            \ container nesting depth
variable JR-STATE                            \ ST-* expectation
variable JR-KIND                             \ current token kind
variable JR-TOK-OFF                          \ current token span start (source offset)
variable JR-TOK-LEN                          \ current token span length
create JR-CTX JR-MAX-DEPTH allot             \ per-level container byte
create JR-KEY-BUF JR-KEY-CAP allot           \ JR-FIND-KEY unescape scratch

variable JR-UP                               \ JR-STR dst-pointer field holder
variable JR-UCAP                             \ JR-STR dst capacity
variable JR-UO                               \ JR-STR output count
variable JR-UI                               \ JR-STR input cursor (span-relative)
variable JR-UE                               \ current escape char
variable JR-HI                               \ surrogate high half
variable JR-LO                               \ surrogate low half
variable JR-NI                               \ number-validator cursor

: JR-TRUE ( -- bool )
   0 0= ;

: JR-FALSE ( -- bool )
   JR-TRUE 0= ;

\ ---- source cursor --------------------------------------------------------
: JR-SRC-FIELD ( -- ptr ptr u8 )
   JR-SRC-P 0 ptr-field ;

: JR-SRC@ ( -- ptr u8 )
   JR-SRC-FIELD @ ;

: JR-SRC! ( ptr u8 -- )
   JR-SRC-FIELD ! ;

: JR-AT ( n -- n )
   JR-SRC@ + c@ ;

: JR-EOF? ( -- bool )
   JR-POS @ JR-SRC-U @ >= ;

: JR-PEEK ( -- n )
   JR-POS @ JR-AT ;

: JR-ADVANCE ( -- )
   JR-POS @ 1+ JR-POS ! ;

: JR-INIT ( ptr u8 n -- ) {: a:ptr u:n :}
   a JR-SRC!
   u JR-SRC-U !
   0 JR-POS !
   0 JR-DEPTH !
   ST-VALUE JR-STATE !
   JT-END JR-KIND !
   0 JR-TOK-OFF !
   0 JR-TOK-LEN ! ;

: JR-TOKEN ( -- n )
   JR-KIND @ ;

: JR-SPAN$ ( -- ptr u8 n )
   JR-SRC@ JR-TOK-OFF @ + JR-TOK-LEN @ ;

\ ---- whitespace -----------------------------------------------------------
: JR-WS? ( n -- bool )
   dup JR-SP = over JR-TAB = or over JR-LF = or swap JR-CR = or ;

: JR-SKIP-WS ( -- )
   begin JR-EOF? 0= if JR-PEEK JR-WS? else JR-FALSE then while
      JR-ADVANCE
   repeat ;

\ ---- container stack ------------------------------------------------------
: JR-PUSH ( n -- )
   JR-DEPTH @ JR-MAX-DEPTH >= if E-JR-DEPTH throw then
   JR-CTX JR-DEPTH @ + c!
   JR-DEPTH @ 1+ JR-DEPTH ! ;

: JR-POP ( -- )
   JR-DEPTH @ 1- JR-DEPTH ! ;

: JR-CUR-CTX ( -- n )
   JR-CTX JR-DEPTH @ 1- + c@ ;

: JR-AFTER-VALUE ( -- )
   JR-DEPTH @ 0= if ST-DONE else ST-SEP then JR-STATE ! ;

\ ---- string token scanning ------------------------------------------------
: JR-SCAN-ESC ( -- )                         \ JR-POS at backslash; step past escape
   JR-ADVANCE
   JR-EOF? if E-JR-STRING throw then
   JR-ADVANCE ;

: JR-SCAN-STRING ( -- )                      \ JR-POS at opening quote; set span, pass close
   JR-ADVANCE
   JR-POS @ JR-TOK-OFF !
   begin
      JR-EOF? if E-JR-STRING throw then
      JR-PEEK
      dup JR-DQ = if
         drop
         JR-POS @ JR-TOK-OFF @ - JR-TOK-LEN !
         JR-ADVANCE exit
      then
      dup JR-BACKSLASH = if
         drop JR-SCAN-ESC
      else
         JR-SP < if E-JR-STRING throw then
         JR-ADVANCE
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

: JR-SCAN-NUMBER ( -- )                      \ set span over the number run at JR-POS
   JR-POS @ JR-TOK-OFF !
   begin JR-EOF? 0= if JR-PEEK JR-NUM-CHAR? else JR-FALSE then while
      JR-ADVANCE
   repeat
   JR-POS @ JR-TOK-OFF @ - JR-TOK-LEN ! ;

: JR-NV-DIGIT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   JR-NI @ u >= if JR-FALSE exit then
   a JR-NI @ + c@ STR-DIGIT? ;

: JR-NV-SKIP-DIGITS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin a u JR-NV-DIGIT? while
      JR-NI @ 1+ JR-NI !
      1+
   repeat ;

: JR-NV-INT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u JR-NV-DIGIT? 0= if JR-FALSE exit then
   a JR-NI @ + c@ JR-ZERO = if
      JR-NI @ 1+ JR-NI !
      a u JR-NV-DIGIT? if JR-FALSE exit then     \ leading zero
      JR-TRUE exit
   then
   a u JR-NV-SKIP-DIGITS drop JR-TRUE ;

: JR-NV-FRAC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   JR-NI @ u >= if JR-TRUE exit then
   a JR-NI @ + c@ JR-DOT <> if JR-TRUE exit then
   JR-NI @ 1+ JR-NI !
   a u JR-NV-SKIP-DIGITS 0= if JR-FALSE exit then
   JR-TRUE ;

: JR-NV-EXP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   JR-NI @ u >= if JR-TRUE exit then
   a JR-NI @ + c@ dup JR-E-LOWER <> swap JR-E-UPPER <> and if JR-TRUE exit then
   JR-NI @ 1+ JR-NI !
   JR-NI @ u < if
      a JR-NI @ + c@ dup JR-PLUS = swap JR-MINUS = or if JR-NI @ 1+ JR-NI ! then
   then
   a u JR-NV-SKIP-DIGITS 0= if JR-FALSE exit then
   JR-TRUE ;

: JR-JSON-NUMBER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if JR-FALSE exit then
   0 JR-NI !
   a JR-NI @ + c@ JR-MINUS = if JR-NI @ 1+ JR-NI ! then
   a u JR-NV-INT? 0= if JR-FALSE exit then
   a u JR-NV-FRAC? 0= if JR-FALSE exit then
   a u JR-NV-EXP? 0= if JR-FALSE exit then
   JR-NI @ u = ;

: JR-SPAN-HAS? ( n -- bool ) {: c:n :}
   JR-SPAN$ c INDEX-OF MATCH option
     none OF JR-FALSE ENDOF
     some OF drop JR-TRUE ENDOF
   ;MATCH ;

: JR-NUM-FLOATY? ( -- bool )
   JR-DOT JR-SPAN-HAS? if JR-TRUE exit then
   JR-E-LOWER JR-SPAN-HAS? if JR-TRUE exit then
   JR-E-UPPER JR-SPAN-HAS? ;

\ ---- literal tokens -------------------------------------------------------
: JR-MATCH-LIT? ( ptr u8 n -- bool ) {: la:ptr lu:n :}
   JR-POS @ lu + JR-SRC-U @ > if JR-FALSE exit then
   0 begin dup lu < while
      dup la + c@ over JR-POS @ + JR-AT <> if drop JR-FALSE exit then
      1+
   repeat drop JR-TRUE ;

: JR-READ-LIT ( ptr u8 n n -- n ) {: la:ptr lu:n kind:n :}
   la lu JR-MATCH-LIT? 0= if E-JR-MALFORMED throw then
   JR-POS @ JR-TOK-OFF !
   lu JR-TOK-LEN !
   kind JR-KIND !
   JR-POS @ lu + JR-POS !
   JR-AFTER-VALUE
   kind ;

: JR-READ-TRUE ( -- n )
   s" true" JT-TRUE JR-READ-LIT ;

: JR-READ-FALSE ( -- n )
   s" false" JT-FALSE JR-READ-LIT ;

: JR-READ-NULL ( -- n )
   s" null" JT-NULL JR-READ-LIT ;

\ ---- value readers --------------------------------------------------------
: JR-OPEN-OBJ ( -- n )
   JR-POS @ JR-TOK-OFF ! 1 JR-TOK-LEN ! JT-OBJ JR-KIND !
   JR-ADVANCE
   JR-CTX-OBJ JR-PUSH
   ST-KEY JR-STATE !
   JT-OBJ ;

: JR-OPEN-ARR ( -- n )
   JR-POS @ JR-TOK-OFF ! 1 JR-TOK-LEN ! JT-ARR JR-KIND !
   JR-ADVANCE
   JR-CTX-ARR JR-PUSH
   ST-ELEM JR-STATE !
   JT-ARR ;

: JR-CLOSE-OBJ ( -- n )
   JR-POS @ JR-TOK-OFF ! 1 JR-TOK-LEN ! JT-OBJ-END JR-KIND !
   JR-ADVANCE
   JR-POP
   JR-AFTER-VALUE
   JT-OBJ-END ;

: JR-CLOSE-ARR ( -- n )
   JR-POS @ JR-TOK-OFF ! 1 JR-TOK-LEN ! JT-ARR-END JR-KIND !
   JR-ADVANCE
   JR-POP
   JR-AFTER-VALUE
   JT-ARR-END ;

: JR-READ-STRING ( -- n )
   JR-SCAN-STRING
   JT-STR JR-KIND !
   JR-AFTER-VALUE
   JT-STR ;

: JR-READ-KEY ( -- n )
   JR-SCAN-STRING
   JT-KEY JR-KIND !
   JR-SKIP-WS
   JR-EOF? if E-JR-COLON throw then
   JR-PEEK JR-COLON <> if E-JR-COLON throw then
   JR-ADVANCE
   ST-VALUE JR-STATE !
   JT-KEY ;

: JR-READ-NUMBER ( -- n )
   JR-SCAN-NUMBER
   JR-SPAN$ JR-JSON-NUMBER? 0= if E-JR-NUMBER throw then
   JR-NUM-FLOATY? if JT-FLOAT else JT-INT then JR-KIND !
   JR-AFTER-VALUE
   JR-KIND @ ;

: JR-READ-VALUE ( -- n ) JR-PEEK {: c:n :}
   c JR-LBRACE = if JR-OPEN-OBJ exit then
   c JR-LBRACK = if JR-OPEN-ARR exit then
   c JR-DQ = if JR-READ-STRING exit then
   c JR-CH-T = if JR-READ-TRUE exit then
   c JR-CH-F = if JR-READ-FALSE exit then
   c JR-CH-N = if JR-READ-NULL exit then
   c JR-MINUS = if JR-READ-NUMBER exit then
   c STR-DIGIT? if JR-READ-NUMBER exit then
   E-JR-MALFORMED throw ;

\ ---- state dispatch -------------------------------------------------------
: JR-DO-VALUE ( -- n )
   JR-READ-VALUE ;

: JR-DO-ELEM ( -- n )
   JR-PEEK JR-RBRACK = if JR-CLOSE-ARR exit then
   JR-READ-VALUE ;

: JR-DO-KEY ( -- n )
   JR-PEEK JR-RBRACE = if JR-CLOSE-OBJ exit then
   JR-PEEK JR-DQ = if JR-READ-KEY exit then
   E-JR-MALFORMED throw ;

: JR-DO-MEMBER ( -- n )
   JR-PEEK JR-DQ = if JR-READ-KEY exit then
   E-JR-MALFORMED throw ;

: JR-DO-SEP ( -- n )
   JR-PEEK JR-COMMA = if
      JR-ADVANCE
      JR-CUR-CTX JR-CTX-OBJ = if ST-MEMBER else ST-VALUE then JR-STATE !
      JR-RETRY exit
   then
   JR-PEEK JR-RBRACE = JR-CUR-CTX JR-CTX-OBJ = and if JR-CLOSE-OBJ exit then
   JR-PEEK JR-RBRACK = JR-CUR-CTX JR-CTX-ARR = and if JR-CLOSE-ARR exit then
   E-JR-COMMA throw ;

: JR-STEP ( -- n )
   JR-SKIP-WS
   JR-EOF? if
      JR-STATE @ ST-DONE = if JT-END exit then
      E-JR-EOF throw
   then
   JR-STATE @ ST-DONE = if E-JR-TRAILING throw then
   JR-STATE @ ST-VALUE = if JR-DO-VALUE exit then
   JR-STATE @ ST-ELEM = if JR-DO-ELEM exit then
   JR-STATE @ ST-KEY = if JR-DO-KEY exit then
   JR-STATE @ ST-MEMBER = if JR-DO-MEMBER exit then
   JR-DO-SEP ;

: JR-NEXT ( -- n )
   begin JR-STEP dup JR-RETRY = while drop repeat ;

\ ---- number value decode --------------------------------------------------
: JR-INT ( -- n )
   JR-KIND @ JT-INT <> if E-JR-STATE throw then
   JR-SPAN$ STR>NUMBER? 0= if E-JR-NUMBER throw then ;

: JR-FLOAT ( -- r )
   JR-KIND @ dup JT-INT = swap JT-FLOAT = or 0= if E-JR-STATE throw then
   JR-SPAN$ STR>FLOAT 0= if E-JR-NUMBER throw then ;

\ ---- string value decode (unescape into caller buffer) --------------------
: JR-UDST-FIELD ( -- ptr ptr u8 )
   JR-UP 0 ptr-field ;

: JR-UDST@ ( -- ptr u8 )
   JR-UDST-FIELD @ ;

: JR-UDST! ( ptr u8 -- )
   JR-UDST-FIELD ! ;

: JR-SPAN-AT ( n -- n )
   JR-TOK-OFF @ + JR-AT ;

: JR-UNESC-CHAR ( -- n )
   JR-UI @ JR-SPAN-AT ;

: JR-EMIT ( n -- ) {: b:n :}
   JR-UO @ JR-UCAP @ >= if E-JR-STATE throw then
   b JR-UDST@ JR-UO @ + c!
   JR-UO @ 1+ JR-UO ! ;

: JR-HEX-VAL ( n -- n bool )
   dup STR-DIGIT? if STR-DIGIT-VALUE JR-TRUE exit then
   dup JR-HEXUP-A >= over JR-HEXUP-F <= and if JR-HEXUP-A - JR-HEX-TEN + JR-TRUE exit then
   dup JR-HEXLO-A >= over JR-HEXLO-F <= and if JR-HEXLO-A - JR-HEX-TEN + JR-TRUE exit then
   drop 0 JR-FALSE ;

: JR-HEX-NEXT ( n -- n )
   JR-HEX-BASE *
   JR-UI @ JR-SPAN-AT JR-HEX-VAL 0= if E-JR-ESCAPE throw then
   +
   JR-UI @ 1+ JR-UI ! ;

: JR-READ-HEX4 ( -- n )
   JR-UI @ 4 + JR-TOK-LEN @ > if E-JR-ESCAPE throw then
   0 JR-HEX-NEXT JR-HEX-NEXT JR-HEX-NEXT JR-HEX-NEXT ;

: JR-EMIT-UTF8 ( n -- ) {: cp:n :}
   cp JR-UTF1-MAX < if cp JR-EMIT exit then
   cp JR-UTF2-MAX < if
      cp 6 rshift JR-UTF2-LEAD or JR-EMIT
      cp JR-UTF-MASK and JR-UTF-CONT or JR-EMIT exit
   then
   cp JR-UTF3-MAX < if
      cp 12 rshift JR-UTF3-LEAD or JR-EMIT
      cp 6 rshift JR-UTF-MASK and JR-UTF-CONT or JR-EMIT
      cp JR-UTF-MASK and JR-UTF-CONT or JR-EMIT exit
   then
   cp 18 rshift JR-UTF4-LEAD or JR-EMIT
   cp 12 rshift JR-UTF-MASK and JR-UTF-CONT or JR-EMIT
   cp 6 rshift JR-UTF-MASK and JR-UTF-CONT or JR-EMIT
   cp JR-UTF-MASK and JR-UTF-CONT or JR-EMIT ;

: JR-UNI ( -- )                              \ JR-UI just past the 'u'; emit UTF-8
   JR-READ-HEX4 JR-HI !
   JR-HI @ JR-SUR-HI < if JR-HI @ JR-EMIT-UTF8 exit then
   JR-HI @ JR-SUR-END >= if JR-HI @ JR-EMIT-UTF8 exit then
   JR-HI @ JR-SUR-LO >= if E-JR-SURROGATE throw then
   JR-UI @ 2 + JR-TOK-LEN @ > if E-JR-SURROGATE throw then
   JR-UI @ JR-SPAN-AT JR-BACKSLASH <> if E-JR-SURROGATE throw then
   JR-UI @ 1+ JR-SPAN-AT JR-CH-U <> if E-JR-SURROGATE throw then
   JR-UI @ 2 + JR-UI !
   JR-READ-HEX4 JR-LO !
   JR-LO @ JR-SUR-LO < if E-JR-SURROGATE throw then
   JR-LO @ JR-SUR-END >= if E-JR-SURROGATE throw then
   JR-SUR-BASE  JR-HI @ JR-SUR-HI - JR-SUR-SHIFT lshift +  JR-LO @ JR-SUR-LO - +  JR-EMIT-UTF8 ;

: JR-ESC-BYTE ( n -- ) {: e:n :}
   e JR-DQ = if JR-DQ JR-EMIT exit then
   e JR-BACKSLASH = if JR-BACKSLASH JR-EMIT exit then
   e JR-SLASH = if JR-SLASH JR-EMIT exit then
   e JR-CH-B = if JR-BS JR-EMIT exit then
   e JR-CH-F = if JR-FF JR-EMIT exit then
   e JR-CH-N = if JR-LF JR-EMIT exit then
   e JR-CH-R = if JR-CR JR-EMIT exit then
   e JR-CH-T = if JR-TAB JR-EMIT exit then
   E-JR-ESCAPE throw ;

: JR-UNESC-STEP ( -- )
   JR-UNESC-CHAR JR-BACKSLASH <> if
      JR-UNESC-CHAR JR-EMIT JR-UI @ 1+ JR-UI ! exit
   then
   JR-UI @ 1+ JR-TOK-LEN @ >= if E-JR-ESCAPE throw then
   JR-UI @ 1+ JR-SPAN-AT JR-UE !
   JR-UI @ 2 + JR-UI !
   JR-UE @ JR-CH-U = if JR-UNI exit then
   JR-UE @ JR-ESC-BYTE ;

: JR-STR ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   JR-KIND @ dup JT-STR = swap JT-KEY = or 0= if E-JR-STATE throw then
   dst JR-UDST!
   cap JR-UCAP !
   0 JR-UI !
   0 JR-UO !
   begin JR-UI @ JR-TOK-LEN @ < while
      JR-UNESC-STEP
   repeat
   JR-UO @ ;

\ ---- skip / find ----------------------------------------------------------
: JR-SCALAR? ( n -- bool )
   dup JT-STR = over JT-INT = or over JT-FLOAT = or
   over JT-TRUE = or over JT-FALSE = or swap JT-NULL = or ;

: JR-OPENER? ( n -- bool )
   dup JT-OBJ = swap JT-ARR = or ;

: JR-CLOSER? ( n -- bool )
   dup JT-OBJ-END = swap JT-ARR-END = or ;

: JR-SKIP-VALUE ( -- )
   JR-KIND @ JR-SCALAR? if exit then
   JR-KIND @ JR-OPENER? 0= if E-JR-STATE throw then
   1 begin dup 0 > while
      JR-NEXT
      dup JR-OPENER? if drop 1+ else
      dup JR-CLOSER? if drop 1- else
      dup JT-END = if E-JR-EOF throw else drop then then then
   repeat drop ;

: JR-FIND-KEY ( ptr u8 n -- bool ) {: ka:ptr ku:n :}
   begin
      JR-NEXT
      dup JT-OBJ-END = if drop JR-FALSE exit then
      dup JT-KEY = if
         drop
         JR-KEY-BUF JR-KEY-CAP JR-STR
         JR-KEY-BUF swap ka ku STR= if
            JR-NEXT drop JR-TRUE exit
         then
         JR-NEXT drop
         JR-SKIP-VALUE
      else
         JT-END = if E-JR-EOF throw then
      then
   again ;
