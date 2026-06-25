\ regex.f - bounded capture-free regex scanner/tokenizer.

$2E constant RX-C-DOT
$5E constant RX-C-CARET
$24 constant RX-C-DOLLAR
$5B constant RX-C-LBRACKET
$5D constant RX-C-RBRACKET
$5C constant RX-C-BACKSLASH
$3F constant RX-C-QUESTION
$2A constant RX-C-STAR
$2B constant RX-C-PLUS
$2D constant RX-C-DASH
$28 constant RX-C-LPAREN
$29 constant RX-C-RPAREN
$7B constant RX-C-LBRACE
$7D constant RX-C-RBRACE
$7C constant RX-C-BAR
255 constant RX-TOK-MAX-LEN

1 constant RX-TOK-LITERAL
2 constant RX-TOK-DOT
3 constant RX-TOK-BOL
4 constant RX-TOK-EOL
5 constant RX-TOK-CLASS
6 constant RX-TOK-NCLASS
7 constant RX-TOK-QUESTION
8 constant RX-TOK-STAR
9 constant RX-TOK-PLUS
256 constant RX-STATE-CAP
1 constant RX-FLAG-ON
0 constant RX-NO-QUANT

create RX-ACTIVE RX-STATE-CAP allot
create RX-NEXT RX-STATE-CAP allot
variable RX-BEST
variable RX-COUNT-N
variable RX-COUNT-POS

: RX-ESCAPABLE? ( n -- bool )
   dup RX-C-DOT =
   over RX-C-CARET = or
   over RX-C-DOLLAR = or
   over RX-C-LBRACKET = or
   over RX-C-RBRACKET = or
   over RX-C-BACKSLASH = or
   over RX-C-QUESTION = or
   over RX-C-STAR = or
   over RX-C-PLUS = or
   over RX-C-DASH = or
   over RX-C-LPAREN = or
   over RX-C-RPAREN = or
   over RX-C-LBRACE = or
   over RX-C-RBRACE = or
   swap RX-C-BAR = or ;

: RX-UNSUPPORTED-META? ( n -- bool )
   dup RX-C-RBRACKET =
   over RX-C-LPAREN = or
   over RX-C-RPAREN = or
   over RX-C-LBRACE = or
   over RX-C-RBRACE = or
   swap RX-C-BAR = or ;

: RX-CHECK-BYTE ( n -- ) {: c :}
   c 0 < if E-RX-SYNTAX throw then
   c RX-TOK-MAX-LEN > if E-RX-SYNTAX throw then ;

: RX-NEED ( len off len -- ) {: cap out add :}
   cap LEN>N 0 < if E-RX-CAPACITY throw then
   out OFF>N 0 < if E-RX-CAPACITY throw then
   add LEN>N 0 < if E-RX-CAPACITY throw then
   add LEN>N cap LEN>N out OFF>N - > if E-RX-CAPACITY throw then ;

: RX-EMIT-1 ( n ptr u8 len off -- off ) {: op dst:ptr cap out :}
   op RX-CHECK-BYTE
   cap out 1 >LEN RX-NEED
   op dst out OFF>N + c!
   out OFF>N 1 + >OFF ;

: RX-EMIT-LIT ( n ptr u8 len off -- off ) {: c dst:ptr cap out :}
   c RX-CHECK-BYTE
   cap out 2 >LEN RX-NEED
   RX-TOK-LITERAL dst out OFF>N + c!
   c dst out OFF>N 1 + + c!
   out OFF>N 2 + >OFF ;

: RX-EMIT-RANGE ( n ptr u8 len ptr u8 len off -- off ) {: op src:ptr raw dst:ptr cap out :}
   raw LEN>N 0 <= if E-RX-SYNTAX throw then
   raw LEN>N RX-TOK-MAX-LEN > if E-RX-CAPACITY throw then
   op RX-CHECK-BYTE
   cap out raw LEN>N 2 + >LEN RX-NEED
   op dst out OFF>N + c!
   raw LEN>N dst out OFF>N 1 + + c!
   src dst out OFF>N 2 + + raw LEN>N BYTE-COPY
   out OFF>N raw LEN>N + 2 + >OFF ;

: RX-SCAN-CLASS-BODY ( ptr u8 len off -- off ) {: a:ptr u idx0 :}
   idx0 OFF>N begin dup u LEN>N < while
      dup a + c@
      dup RX-C-RBRACKET = if
         drop dup idx0 OFF>N = if E-RX-SYNTAX throw then
         >OFF exit
      then
      RX-C-BACKSLASH = if
         dup 1 + u LEN>N >= if E-RX-SYNTAX throw then
         a over 1 + + c@ RX-ESCAPABLE? 0= if E-RX-SYNTAX throw then
         2 +
      else
         1 +
      then
   repeat drop E-RX-SYNTAX throw ;

: RX-SCAN-CLASS ( ptr u8 len off -- off off n ) {: a:ptr u start :}
   start OFF>N 1 + u LEN>N >= if E-RX-SYNTAX throw then
   a start OFF>N 1 + + c@ RX-C-CARET = if
      start OFF>N 2 + dup u LEN>N >= if E-RX-SYNTAX throw then
      >OFF dup a u rot RX-SCAN-CLASS-BODY
      1 exit
   then
   start OFF>N 1 + >OFF dup a u rot RX-SCAN-CLASS-BODY
   0 ;

: RX-EMIT-CLASS-DONE ( ptr u8 ptr u8 len off off off n -- off off ) {: a:ptr dst:ptr cap out body close neg :}
   neg 0= if RX-TOK-CLASS else RX-TOK-NCLASS then
   a body OFF>N + close OFF>N body OFF>N - >LEN dst cap out RX-EMIT-RANGE
   close OFF>N 1 + >OFF ;

: RX-EMIT-CLASS ( ptr u8 len off ptr u8 len off -- off off ) {: a:ptr u start dst:ptr cap out :}
   a dst cap out a u start RX-SCAN-CLASS RX-EMIT-CLASS-DONE ;

: RX-SCAN-ESCAPE ( ptr u8 len off ptr u8 len off -- off off ) {: a:ptr u idx dst:ptr cap out :}
   idx OFF>N 1 + u LEN>N >= if E-RX-SYNTAX throw then
   a idx OFF>N 1 + + c@ dup RX-ESCAPABLE? 0= if E-RX-SYNTAX throw then
   dst cap out RX-EMIT-LIT
   idx OFF>N 2 + >OFF swap ;

: RX-SCAN-ONE ( ptr u8 len off ptr u8 len off -- off off ) {: a:ptr u idx dst:ptr cap out :}
   a idx OFF>N + c@
   dup RX-C-BACKSLASH = if drop a u idx dst cap out RX-SCAN-ESCAPE exit then
   dup RX-C-DOT = if drop RX-TOK-DOT dst cap out RX-EMIT-1 idx OFF>N 1 + >OFF swap exit then
   dup RX-C-CARET = if drop RX-TOK-BOL dst cap out RX-EMIT-1 idx OFF>N 1 + >OFF swap exit then
   dup RX-C-DOLLAR = if drop RX-TOK-EOL dst cap out RX-EMIT-1 idx OFF>N 1 + >OFF swap exit then
   dup RX-C-QUESTION = if drop RX-TOK-QUESTION dst cap out RX-EMIT-1 idx OFF>N 1 + >OFF swap exit then
   dup RX-C-STAR = if drop RX-TOK-STAR dst cap out RX-EMIT-1 idx OFF>N 1 + >OFF swap exit then
   dup RX-C-PLUS = if drop RX-TOK-PLUS dst cap out RX-EMIT-1 idx OFF>N 1 + >OFF swap exit then
   dup RX-C-LBRACKET = if drop a u idx dst cap out RX-EMIT-CLASS swap exit then
   dup RX-UNSUPPORTED-META? if E-RX-SYNTAX throw then
   dst cap out RX-EMIT-LIT
   idx OFF>N 1 + >OFF swap ;

: RX-COMPILE ( ptr u8 len ptr u8 len -- len ) {: a:ptr u dst:ptr cap :}
   u LEN>N 0 < if E-RX-SYNTAX throw then
   cap LEN>N 0 < if E-RX-CAPACITY throw then
   0 >OFF 0 >OFF begin over OFF>N u LEN>N < while
      >r
      a u rot dst cap r> RX-SCAN-ONE
   repeat
   nip OFF>N >LEN ;

: RX-CHECK-MATCH-ARGS ( len len -- ) {: text-u rx-u :}
   text-u LEN>N 0 < if E-RX-SYNTAX throw then
   rx-u LEN>N 0 < if E-RX-CAPACITY throw then
   rx-u LEN>N RX-TOK-MAX-LEN > if E-RX-CAPACITY throw then ;

: RX-FLAGS-CLEAR ( ptr u8 len -- ) {: flags:ptr u :}
   u LEN>N RX-STATE-CAP > if E-RX-CAPACITY throw then
   0 begin dup u LEN>N < while
      0 over flags + c!
      1+
   repeat drop ;

: RX-FLAG? ( ptr u8 off -- bool ) {: flags:ptr off :}
   off OFF>N 0 < if E-RX-CAPACITY throw then
   off OFF>N RX-STATE-CAP >= if E-RX-CAPACITY throw then
   flags off OFF>N + c@ 0 <> ;

: RX-ANY-FLAG? ( ptr u8 len -- bool ) {: flags:ptr u :}
   0 begin dup u LEN>N < while
      flags over + c@ 0 <> if drop STR-TRUE exit then
      1+
   repeat drop STR-FALSE ;

: RX-ADD-STATE ( ptr u8 len off -- ) {: flags:ptr rx-u off :}
   off OFF>N 0 < if E-RX-SYNTAX throw then
   off OFF>N rx-u LEN>N > if E-RX-SYNTAX throw then
   off OFF>N RX-STATE-CAP >= if E-RX-CAPACITY throw then
   RX-FLAG-ON flags off OFF>N + c! ;

: RX-QUANT? ( n -- bool )
   dup RX-TOK-QUESTION =
   over RX-TOK-STAR = or
   swap RX-TOK-PLUS = or ;

: RX-ZERO-QUANT? ( n -- bool )
   dup RX-TOK-QUESTION =
   swap RX-TOK-STAR = or ;

: RX-CONSUMING? ( n -- bool )
   dup RX-TOK-LITERAL =
   over RX-TOK-DOT = or
   over RX-TOK-CLASS = or
   swap RX-TOK-NCLASS = or ;

: RX-ANCHOR? ( n -- bool )
   dup RX-TOK-BOL =
   swap RX-TOK-EOL = or ;

: RX-FIXED-ATOM-LEN ( len off len -- len ) {: rx-u off len :}
   off OFF>N len LEN>N + rx-u LEN>N > if E-RX-SYNTAX throw then
   len ;

: RX-CLASS-RAW-LEN ( ptr u8 len off -- len ) {: rx:ptr rx-u off :}
   off OFF>N 1 + rx-u LEN>N >= if E-RX-SYNTAX throw then
   rx off OFF>N 1 + + c@
   dup 0 <= if E-RX-SYNTAX throw then
   dup RX-TOK-MAX-LEN > if E-RX-CAPACITY throw then
   off OFF>N 2 + over + rx-u LEN>N > if E-RX-SYNTAX throw then
   >LEN ;

: RX-ATOM-LEN ( ptr u8 len off -- len ) {: rx:ptr rx-u off :}
   off OFF>N 0 < if E-RX-SYNTAX throw then
   off OFF>N rx-u LEN>N >= if E-RX-SYNTAX throw then
   rx off OFF>N + c@
   dup RX-TOK-LITERAL = if drop rx-u off 2 >LEN RX-FIXED-ATOM-LEN exit then
   dup RX-TOK-DOT = if drop rx-u off 1 >LEN RX-FIXED-ATOM-LEN exit then
   dup RX-TOK-BOL = if drop rx-u off 1 >LEN RX-FIXED-ATOM-LEN exit then
   dup RX-TOK-EOL = if drop rx-u off 1 >LEN RX-FIXED-ATOM-LEN exit then
   dup RX-TOK-CLASS = over RX-TOK-NCLASS = or if
      drop rx rx-u off RX-CLASS-RAW-LEN LEN>N 2 + >LEN exit
   then
   RX-QUANT? if E-RX-SYNTAX throw then
   E-RX-SYNTAX throw ;

: RX-ATOM-END ( ptr u8 len off -- off ) {: rx:ptr rx-u off :}
   off OFF>N rx rx-u off RX-ATOM-LEN LEN>N + >OFF ;

: RX-QUANT-AT ( ptr u8 len off -- n ) {: rx:ptr rx-u off :}
   rx rx-u off RX-ATOM-END
   dup OFF>N rx-u LEN>N < if
      rx over OFF>N + c@
      dup RX-QUANT? if nip exit then
      drop
   then
   drop RX-NO-QUANT ;

: RX-AFTER-ATOM-QUANT ( ptr u8 len off -- off ) {: rx:ptr rx-u off :}
   rx rx-u off RX-ATOM-END
   dup OFF>N rx-u LEN>N < if
      rx over OFF>N + c@ RX-QUANT? if OFF>N 1 + >OFF exit then
   then ;

: RX-VALIDATE-STEP ( ptr u8 len off -- off ) {: rx:ptr rx-u off :}
   rx off OFF>N + c@
   dup RX-QUANT? if E-RX-SYNTAX throw then
   dup RX-ANCHOR? if
      rx rx-u off RX-QUANT-AT RX-NO-QUANT <> if E-RX-SYNTAX throw then
      drop rx rx-u off RX-ATOM-END exit
   then
   RX-CONSUMING? 0= if E-RX-SYNTAX throw then
   rx rx-u off RX-AFTER-ATOM-QUANT ;

: RX-VALIDATE ( ptr u8 len -- ) {: rx:ptr rx-u :}
   0 >OFF begin dup OFF>N rx-u LEN>N < while
      dup >r rx rx-u r> RX-VALIDATE-STEP nip
   repeat drop ;

: RX-CLASS-RANGE-CAND? ( ptr u8 len off -- bool ) {: body:ptr raw ix :}
   ix OFF>N 2 + raw LEN>N < if
      body ix OFF>N + c@ RX-C-DASH <>
      body ix OFF>N 1 + + c@ RX-C-DASH = and
      body ix OFF>N 2 + + c@ RX-C-BACKSLASH <> and
      body ix OFF>N 2 + + c@ RX-C-DASH <> and
      exit
   then
   STR-FALSE ;

: RX-CLASS-RANGE-MATCH? ( n ptr u8 off -- bool ) {: c body:ptr ix :}
   body ix OFF>N + c@ body ix OFF>N 2 + + c@ > if E-RX-SYNTAX throw then
   c body ix OFF>N + c@ >=
   c body ix OFF>N 2 + + c@ <= and ;

: RX-CLASS-ESC-MATCH? ( n ptr u8 len off -- bool ) {: c body:ptr raw ix :}
   ix OFF>N 1 + raw LEN>N >= if E-RX-SYNTAX throw then
   body ix OFF>N 1 + + c@ dup RX-ESCAPABLE? 0= if E-RX-SYNTAX throw then
   c = ;

: RX-CLASS-MEMBER? ( n ptr u8 len -- bool ) {: c body:ptr raw :}
   0 >OFF begin dup OFF>N raw LEN>N < while
      dup >r body raw r> RX-CLASS-RANGE-CAND? if
         dup >r c body r> RX-CLASS-RANGE-MATCH? if drop STR-TRUE exit then
         OFF>N 3 + >OFF
      else
         dup OFF>N body + c@ RX-C-BACKSLASH = if
            dup >r c body raw r> RX-CLASS-ESC-MATCH? if drop STR-TRUE exit then
            OFF>N 2 + >OFF
         else
            c over OFF>N body + c@ = if drop STR-TRUE exit then
            OFF>N 1 + >OFF
         then
      then
   repeat drop STR-FALSE ;

: RX-ATOM-CHAR-MATCH? ( n ptr u8 len off -- bool ) {: c rx:ptr rx-u off :}
   rx off OFF>N + c@
   dup RX-TOK-LITERAL = if
      drop off OFF>N 1 + rx-u LEN>N >= if E-RX-SYNTAX throw then
      c rx off OFF>N 1 + + c@ = exit
   then
   dup RX-TOK-DOT = if drop STR-TRUE exit then
   dup RX-TOK-CLASS = if
      drop c rx off OFF>N 2 + + rx rx-u off RX-CLASS-RAW-LEN RX-CLASS-MEMBER? exit
   then
   dup RX-TOK-NCLASS = if
      drop c rx off OFF>N 2 + + rx rx-u off RX-CLASS-RAW-LEN RX-CLASS-MEMBER? 0= exit
   then
   RX-CONSUMING? 0= if E-RX-SYNTAX throw then
   E-RX-SYNTAX throw ;

: RX-ANCHOR-MATCH? ( n off len -- bool ) {: op pos text-u :}
   op RX-TOK-BOL = if pos OFF>N 0 = exit then
   op RX-TOK-EOL = if pos OFF>N text-u LEN>N = exit then
   E-RX-SYNTAX throw ;

: RX-CLOSE-ONE ( ptr u8 len off len ptr u8 off -- ) {: rx:ptr rx-u pos text-u flags:ptr off :}
   flags off RX-FLAG? 0= if exit then
   off OFF>N rx-u LEN>N = if exit then
   rx off OFF>N + c@
   dup RX-QUANT? if E-RX-SYNTAX throw then
   dup RX-ANCHOR? if
      rx rx-u off RX-QUANT-AT RX-NO-QUANT <> if E-RX-SYNTAX throw then
      pos text-u RX-ANCHOR-MATCH? if
         flags rx-u rx rx-u off RX-ATOM-END RX-ADD-STATE
      then
      exit
   then
   dup RX-CONSUMING? 0= if E-RX-SYNTAX throw then
   drop
   rx rx-u off RX-QUANT-AT RX-ZERO-QUANT? if
      flags rx-u rx rx-u off RX-AFTER-ATOM-QUANT RX-ADD-STATE
   then ;

: RX-CLOSE ( ptr u8 len off len ptr u8 -- ) {: rx:ptr rx-u pos text-u flags:ptr :}
   0 >OFF begin dup OFF>N rx-u LEN>N <= while
      dup >r rx rx-u pos text-u flags r> RX-CLOSE-ONE
      OFF>N 1 + >OFF
   repeat drop ;

: RX-RESET-STATES ( len -- ) {: rx-u :}
   RX-ACTIVE rx-u LEN>N 1 + >LEN RX-FLAGS-CLEAR
   RX-NEXT rx-u LEN>N 1 + >LEN RX-FLAGS-CLEAR
   RX-ACTIVE rx-u 0 >OFF RX-ADD-STATE ;

: RX-NEXT>ACTIVE ( len -- ) {: rx-u :}
   RX-NEXT RX-ACTIVE rx-u LEN>N 1 + BYTE-COPY
   RX-NEXT rx-u LEN>N 1 + >LEN RX-FLAGS-CLEAR ;

: RX-ADD-CONSUME-TARGET ( ptr u8 len off ptr u8 -- ) {: rx:ptr rx-u off flags:ptr :}
   rx rx-u off RX-QUANT-AT
   dup RX-NO-QUANT = if
      drop flags rx-u rx rx-u off RX-ATOM-END RX-ADD-STATE exit
   then
   dup RX-TOK-QUESTION = if
      drop flags rx-u rx rx-u off RX-AFTER-ATOM-QUANT RX-ADD-STATE exit
   then
   dup RX-TOK-STAR = if
      drop flags rx-u off RX-ADD-STATE exit
   then
   RX-TOK-PLUS = if
      flags rx-u off RX-ADD-STATE
      flags rx-u rx rx-u off RX-AFTER-ATOM-QUANT RX-ADD-STATE
      exit
   then
   E-RX-SYNTAX throw ;

: RX-CONSUME-STATE ( ptr u8 ptr u8 len off ptr u8 ptr u8 off -- ) {: text:ptr rx:ptr rx-u pos active:ptr next:ptr off :}
   active off RX-FLAG? 0= if exit then
   off OFF>N rx-u LEN>N = if exit then
   rx off OFF>N + c@
   dup RX-QUANT? if E-RX-SYNTAX throw then
   dup RX-CONSUMING? if
      drop text pos OFF>N + c@ rx rx-u off RX-ATOM-CHAR-MATCH? if
         rx rx-u off next RX-ADD-CONSUME-TARGET
      then
      exit
   then
   drop ;

: RX-CONSUME-CHAR ( ptr u8 ptr u8 len off -- ) {: text:ptr rx:ptr rx-u pos :}
   RX-NEXT rx-u LEN>N 1 + >LEN RX-FLAGS-CLEAR
   0 >OFF begin dup OFF>N rx-u LEN>N <= while
      dup >r text rx rx-u pos RX-ACTIVE RX-NEXT r> RX-CONSUME-STATE
      OFF>N 1 + >OFF
   repeat drop
   rx-u RX-NEXT>ACTIVE ;

: RX-ACCEPT? ( len -- bool ) {: rx-u :}
   RX-ACTIVE rx-u LEN>N >OFF RX-FLAG? ;

: RX-PREFIX-LEN ( ptr u8 len ptr u8 len off -- len bool ) {: text:ptr text-u rx:ptr rx-u start :}
   start OFF>N 0 < if E-RX-SYNTAX throw then
   start OFF>N text-u LEN>N > if 0 >LEN STR-FALSE exit then
   -1 RX-BEST !
   rx-u RX-RESET-STATES
   rx rx-u start text-u RX-ACTIVE RX-CLOSE
   rx-u RX-ACCEPT? if 0 RX-BEST ! then
   start begin
      dup OFF>N text-u LEN>N < RX-ACTIVE rx-u LEN>N 1 + >LEN RX-ANY-FLAG? and
   while
      dup >r text rx rx-u r> RX-CONSUME-CHAR
      OFF>N 1 + >OFF
      dup >r rx rx-u r> text-u RX-ACTIVE RX-CLOSE
      rx-u RX-ACCEPT? if dup OFF>N start OFF>N - RX-BEST ! then
   repeat drop
   RX-BEST @ dup 0 < if drop 0 >LEN STR-FALSE exit then
   >LEN STR-TRUE ;

: RX-PREPARE ( len ptr u8 len -- ) {: text-u rx:ptr rx-u :}
   text-u rx-u RX-CHECK-MATCH-ARGS
   rx rx-u RX-VALIDATE ;

: RX-MATCH? ( ptr u8 len ptr u8 len -- bool ) {: text:ptr text-u rx:ptr rx-u :}
   text-u rx rx-u RX-PREPARE
   text text-u rx rx-u 0 >OFF RX-PREFIX-LEN if
      LEN>N text-u LEN>N = exit
   then
   drop STR-FALSE ;

: RX-FIND-FROM ( ptr u8 len ptr u8 len off -- off len bool ) {: text:ptr text-u rx:ptr rx-u from :}
   from OFF>N 0 < if E-RX-SYNTAX throw then
   from begin dup OFF>N text-u LEN>N <= while
      dup >r text text-u rx rx-u r> RX-PREFIX-LEN if
         STR-TRUE exit
      then
      drop OFF>N 1 + >OFF
   repeat drop
   0 >OFF 0 >LEN STR-FALSE ;

: RX-FIND ( ptr u8 len ptr u8 len -- off len bool ) {: text:ptr text-u rx:ptr rx-u :}
   text-u rx rx-u RX-PREPARE
   text text-u rx rx-u 0 >OFF RX-FIND-FROM ;

: RX-COUNT ( ptr u8 len ptr u8 len -- count ) {: text:ptr text-u rx:ptr rx-u :}
   text-u rx rx-u RX-PREPARE
   0 RX-COUNT-N !
   0 RX-COUNT-POS !
   begin RX-COUNT-POS @ text-u LEN>N <= while
      text text-u rx rx-u RX-COUNT-POS @ >OFF RX-FIND-FROM if
         RX-COUNT-N @ 1+ RX-COUNT-N !
         LEN>N dup 0 > if
            swap OFF>N swap +
         else
            drop OFF>N 1+
         then
         RX-COUNT-POS !
      else
         2drop text-u LEN>N 1 + RX-COUNT-POS !
      then
   repeat
   RX-COUNT-N @ >COUNT ;
