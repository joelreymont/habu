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

: RX-NEED ( n n n -- ) {: cap out add :}
   cap 0 < if E-RX-CAPACITY throw then
   out 0 < if E-RX-CAPACITY throw then
   add 0 < if E-RX-CAPACITY throw then
   add cap out - > if E-RX-CAPACITY throw then ;

: RX-EMIT-1 ( n ptr u8 n n -- n ) {: op dst:ptr cap out :}
   op RX-CHECK-BYTE
   cap out 1 RX-NEED
   op dst out + c!
   out 1 + ;

: RX-EMIT-LIT ( n ptr u8 n n -- n ) {: c dst:ptr cap out :}
   c RX-CHECK-BYTE
   cap out 2 RX-NEED
   RX-TOK-LITERAL dst out + c!
   c dst out 1 + + c!
   out 2 + ;

: RX-EMIT-RANGE ( n ptr u8 n ptr u8 n n -- n ) {: op src:ptr raw dst:ptr cap out :}
   raw 0 <= if E-RX-SYNTAX throw then
   raw RX-TOK-MAX-LEN > if E-RX-CAPACITY throw then
   op RX-CHECK-BYTE
   cap out raw 2 + RX-NEED
   op dst out + c!
   raw dst out 1 + + c!
   src dst out 2 + + raw BYTE-COPY
   out raw + 2 + ;

: RX-SCAN-CLASS-BODY ( ptr u8 n n -- n ) {: a:ptr u idx0 :}
   idx0 begin dup u < while
      dup a + c@
      dup RX-C-RBRACKET = if
         drop dup idx0 = if E-RX-SYNTAX throw then
         exit
      then
      RX-C-BACKSLASH = if
         dup 1 + u >= if E-RX-SYNTAX throw then
         a over 1 + + c@ RX-ESCAPABLE? 0= if E-RX-SYNTAX throw then
         2 +
      else
         1 +
      then
   repeat drop E-RX-SYNTAX throw 0 ;

: RX-SCAN-CLASS ( ptr u8 n n -- n n n ) {: a:ptr u start :}
   start 1 + u >= if E-RX-SYNTAX throw then
   a start 1 + + c@ RX-C-CARET = if
      start 2 + dup u >= if E-RX-SYNTAX throw then
      dup a u rot RX-SCAN-CLASS-BODY
      1 exit
   then
   start 1 + dup a u rot RX-SCAN-CLASS-BODY
   0 ;

: RX-EMIT-CLASS-DONE ( ptr u8 ptr u8 n n n n n -- n n ) {: a:ptr dst:ptr cap out body close neg :}
   neg 0= if RX-TOK-CLASS else RX-TOK-NCLASS then
   a body + close body - dst cap out RX-EMIT-RANGE
   close 1 + ;

: RX-EMIT-CLASS ( ptr u8 n n ptr u8 n n -- n n ) {: a:ptr u start dst:ptr cap out :}
   a dst cap out a u start RX-SCAN-CLASS RX-EMIT-CLASS-DONE ;

: RX-SCAN-ESCAPE ( ptr u8 n n ptr u8 n n -- n n ) {: a:ptr u idx dst:ptr cap out :}
   idx 1 + u >= if E-RX-SYNTAX throw then
   a idx 1 + + c@ dup RX-ESCAPABLE? 0= if E-RX-SYNTAX throw then
   dst cap out RX-EMIT-LIT
   idx 2 + swap ;

: RX-SCAN-ONE ( ptr u8 n n ptr u8 n n -- n n ) {: a:ptr u idx dst:ptr cap out :}
   a idx + c@
   dup RX-C-BACKSLASH = if drop a u idx dst cap out RX-SCAN-ESCAPE exit then
   dup RX-C-DOT = if drop RX-TOK-DOT dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-CARET = if drop RX-TOK-BOL dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-DOLLAR = if drop RX-TOK-EOL dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-QUESTION = if drop RX-TOK-QUESTION dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-STAR = if drop RX-TOK-STAR dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-PLUS = if drop RX-TOK-PLUS dst cap out RX-EMIT-1 idx 1 + swap exit then
   dup RX-C-LBRACKET = if drop a u idx dst cap out RX-EMIT-CLASS swap exit then
   dup RX-UNSUPPORTED-META? if E-RX-SYNTAX throw then
   dst cap out RX-EMIT-LIT
   idx 1 + swap ;

: RX-COMPILE ( ptr u8 n ptr u8 n -- n ) {: a:ptr u dst:ptr cap :}
   u 0 < if E-RX-SYNTAX throw then
   cap 0 < if E-RX-CAPACITY throw then
   0 0 begin over u < while
      >r
      a u rot dst cap r> RX-SCAN-ONE
   repeat
   nip ;

: RX-CHECK-MATCH-ARGS ( n n -- ) {: text-u rx-u :}
   text-u 0 < if E-RX-SYNTAX throw then
   rx-u 0 < if E-RX-CAPACITY throw then
   rx-u RX-TOK-MAX-LEN > if E-RX-CAPACITY throw then ;

: RX-FLAGS-CLEAR ( ptr u8 n -- ) {: flags:ptr u :}
   u RX-STATE-CAP > if E-RX-CAPACITY throw then
   0 begin dup u < while
      0 over flags + c!
      1+
   repeat drop ;

: RX-FLAG? ( ptr u8 n -- bool ) {: flags:ptr off :}
   flags off + c@ 0 <> ;

: RX-ANY-FLAG? ( ptr u8 n -- bool ) {: flags:ptr u :}
   0 begin dup u < while
      flags over + c@ 0 <> if drop STR-TRUE exit then
      1+
   repeat drop STR-FALSE ;

: RX-ADD-STATE ( ptr u8 n n -- ) {: flags:ptr rx-u off :}
   off 0 < if E-RX-SYNTAX throw then
   off rx-u > if E-RX-SYNTAX throw then
   off RX-STATE-CAP >= if E-RX-CAPACITY throw then
   RX-FLAG-ON flags off + c! ;

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

: RX-FIXED-ATOM-LEN ( n n n -- n ) {: rx-u off len :}
   off len + rx-u > if E-RX-SYNTAX throw then
   len ;

: RX-CLASS-RAW-LEN ( ptr u8 n n -- n ) {: rx:ptr rx-u off :}
   off 1 + rx-u >= if E-RX-SYNTAX throw then
   rx off 1 + + c@
   dup 0 <= if E-RX-SYNTAX throw then
   dup RX-TOK-MAX-LEN > if E-RX-CAPACITY throw then
   off 2 + over + rx-u > if E-RX-SYNTAX throw then ;

: RX-ATOM-LEN ( ptr u8 n n -- n ) {: rx:ptr rx-u off :}
   off 0 < if E-RX-SYNTAX throw then
   off rx-u >= if E-RX-SYNTAX throw then
   rx off + c@
   dup RX-TOK-LITERAL = if drop rx-u off 2 RX-FIXED-ATOM-LEN exit then
   dup RX-TOK-DOT = if drop rx-u off 1 RX-FIXED-ATOM-LEN exit then
   dup RX-TOK-BOL = if drop rx-u off 1 RX-FIXED-ATOM-LEN exit then
   dup RX-TOK-EOL = if drop rx-u off 1 RX-FIXED-ATOM-LEN exit then
   dup RX-TOK-CLASS = over RX-TOK-NCLASS = or if
      drop rx rx-u off RX-CLASS-RAW-LEN 2 + exit
   then
   RX-QUANT? if E-RX-SYNTAX throw then
   E-RX-SYNTAX throw 0 ;

: RX-ATOM-END ( ptr u8 n n -- n ) {: rx:ptr rx-u off :}
   off rx rx-u off RX-ATOM-LEN + ;

: RX-QUANT-AT ( ptr u8 n n -- n ) {: rx:ptr rx-u off :}
   rx rx-u off RX-ATOM-END
   dup rx-u < if
      rx over + c@
      dup RX-QUANT? if nip exit then
      drop
   then
   drop RX-NO-QUANT ;

: RX-AFTER-ATOM-QUANT ( ptr u8 n n -- n ) {: rx:ptr rx-u off :}
   rx rx-u off RX-ATOM-END
   dup rx-u < if
      rx over + c@ RX-QUANT? if 1+ exit then
   then ;

: RX-VALIDATE-STEP ( ptr u8 n n -- n ) {: rx:ptr rx-u off :}
   rx off + c@
   dup RX-QUANT? if E-RX-SYNTAX throw then
   dup RX-ANCHOR? if
      rx rx-u off RX-QUANT-AT RX-NO-QUANT <> if E-RX-SYNTAX throw then
      drop rx rx-u off RX-ATOM-END exit
   then
   RX-CONSUMING? 0= if E-RX-SYNTAX throw then
   rx rx-u off RX-AFTER-ATOM-QUANT ;

: RX-VALIDATE ( ptr u8 n -- ) {: rx:ptr rx-u :}
   0 begin dup rx-u < while
      dup >r rx rx-u r> RX-VALIDATE-STEP nip
   repeat drop ;

: RX-CLASS-RANGE-CAND? ( ptr u8 n n -- bool ) {: body:ptr raw ix :}
   ix 2 + raw < if
      body ix + c@ RX-C-DASH <>
      body ix 1 + + c@ RX-C-DASH = and
      body ix 2 + + c@ RX-C-BACKSLASH <> and
      body ix 2 + + c@ RX-C-DASH <> and
      exit
   then
   STR-FALSE ;

: RX-CLASS-RANGE-MATCH? ( n ptr u8 n -- bool ) {: c body:ptr ix :}
   body ix + c@ body ix 2 + + c@ > if E-RX-SYNTAX throw then
   c body ix + c@ >=
   c body ix 2 + + c@ <= and ;

: RX-CLASS-ESC-MATCH? ( n ptr u8 n n -- bool ) {: c body:ptr raw ix :}
   ix 1 + raw >= if E-RX-SYNTAX throw then
   body ix 1 + + c@ dup RX-ESCAPABLE? 0= if E-RX-SYNTAX throw then
   c = ;

: RX-CLASS-MEMBER? ( n ptr u8 n -- bool ) {: c body:ptr raw :}
   0 begin dup raw < while
      dup >r body raw r> RX-CLASS-RANGE-CAND? if
         dup >r c body r> RX-CLASS-RANGE-MATCH? if drop STR-TRUE exit then
         3 +
      else
         dup body + c@ RX-C-BACKSLASH = if
            dup >r c body raw r> RX-CLASS-ESC-MATCH? if drop STR-TRUE exit then
            2 +
         else
            c over body + c@ = if drop STR-TRUE exit then
            1 +
         then
      then
   repeat drop STR-FALSE ;

: RX-ATOM-CHAR-MATCH? ( n ptr u8 n n -- bool ) {: c rx:ptr rx-u off :}
   rx off + c@
   dup RX-TOK-LITERAL = if
      drop off 1 + rx-u >= if E-RX-SYNTAX throw then
      c rx off 1 + + c@ = exit
   then
   dup RX-TOK-DOT = if drop STR-TRUE exit then
   dup RX-TOK-CLASS = if
      drop c rx off 2 + + rx rx-u off RX-CLASS-RAW-LEN RX-CLASS-MEMBER? exit
   then
   dup RX-TOK-NCLASS = if
      drop c rx off 2 + + rx rx-u off RX-CLASS-RAW-LEN RX-CLASS-MEMBER? 0= exit
   then
   RX-CONSUMING? 0= if E-RX-SYNTAX throw then
   E-RX-SYNTAX throw STR-FALSE ;

: RX-ANCHOR-MATCH? ( n n n -- bool ) {: op pos text-u :}
   op RX-TOK-BOL = if pos 0 = exit then
   op RX-TOK-EOL = if pos text-u = exit then
   E-RX-SYNTAX throw STR-FALSE ;

: RX-CLOSE-ONE ( ptr u8 n n n ptr u8 n -- ) {: rx:ptr rx-u pos text-u flags:ptr off :}
   flags off RX-FLAG? 0= if exit then
   off rx-u = if exit then
   rx off + c@
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

: RX-CLOSE ( ptr u8 n n n ptr u8 -- ) {: rx:ptr rx-u pos text-u flags:ptr :}
   0 begin dup rx-u <= while
      dup >r rx rx-u pos text-u flags r> RX-CLOSE-ONE
      1+
   repeat drop ;

: RX-RESET-STATES ( n -- ) {: rx-u :}
   RX-ACTIVE rx-u 1 + RX-FLAGS-CLEAR
   RX-NEXT rx-u 1 + RX-FLAGS-CLEAR
   RX-ACTIVE rx-u 0 RX-ADD-STATE ;

: RX-NEXT>ACTIVE ( n -- ) {: rx-u :}
   RX-NEXT RX-ACTIVE rx-u 1 + BYTE-COPY
   RX-NEXT rx-u 1 + RX-FLAGS-CLEAR ;

: RX-ADD-CONSUME-TARGET ( ptr u8 n n ptr u8 -- ) {: rx:ptr rx-u off flags:ptr :}
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

: RX-CONSUME-STATE ( ptr u8 ptr u8 n n ptr u8 ptr u8 n -- ) {: text:ptr rx:ptr rx-u pos active:ptr next:ptr off :}
   active off RX-FLAG? 0= if exit then
   off rx-u = if exit then
   rx off + c@
   dup RX-QUANT? if E-RX-SYNTAX throw then
   dup RX-CONSUMING? if
      drop text pos + c@ rx rx-u off RX-ATOM-CHAR-MATCH? if
         rx rx-u off next RX-ADD-CONSUME-TARGET
      then
      exit
   then
   drop ;

: RX-CONSUME-CHAR ( ptr u8 ptr u8 n n -- ) {: text:ptr rx:ptr rx-u pos :}
   RX-NEXT rx-u 1 + RX-FLAGS-CLEAR
   0 begin dup rx-u <= while
      dup >r text rx rx-u pos RX-ACTIVE RX-NEXT r> RX-CONSUME-STATE
      1+
   repeat drop
   rx-u RX-NEXT>ACTIVE ;

: RX-ACCEPT? ( n -- bool ) {: rx-u :}
   RX-ACTIVE rx-u RX-FLAG? ;

: RX-PREFIX-LEN ( ptr u8 n ptr u8 n n -- n bool ) {: text:ptr text-u rx:ptr rx-u start :}
   start 0 < if E-RX-SYNTAX throw then
   start text-u > if 0 STR-FALSE exit then
   -1 RX-BEST !
   rx-u RX-RESET-STATES
   rx rx-u start text-u RX-ACTIVE RX-CLOSE
   rx-u RX-ACCEPT? if 0 RX-BEST ! then
   start begin
      dup text-u < RX-ACTIVE rx-u 1 + RX-ANY-FLAG? and
   while
      dup >r text rx rx-u r> RX-CONSUME-CHAR
      1+
      dup >r rx rx-u r> text-u RX-ACTIVE RX-CLOSE
      rx-u RX-ACCEPT? if dup start - RX-BEST ! then
   repeat drop
   RX-BEST @ dup 0 < if drop 0 STR-FALSE exit then
   STR-TRUE ;

: RX-PREPARE ( n ptr u8 n -- ) {: text-u rx:ptr rx-u :}
   text-u rx-u RX-CHECK-MATCH-ARGS
   rx rx-u RX-VALIDATE ;

: RX-MATCH? ( ptr u8 n ptr u8 n -- bool ) {: text:ptr text-u rx:ptr rx-u :}
   text-u rx rx-u RX-PREPARE
   text text-u rx rx-u 0 RX-PREFIX-LEN if
      text-u = exit
   then
   drop STR-FALSE ;

: RX-FIND-FROM ( ptr u8 n ptr u8 n n -- n n bool ) {: text:ptr text-u rx:ptr rx-u from :}
   from 0 < if E-RX-SYNTAX throw then
   from begin dup text-u <= while
      dup >r text text-u rx rx-u r> RX-PREFIX-LEN if
         STR-TRUE exit
      then
      drop 1+
   repeat drop
   0 0 STR-FALSE ;

: RX-FIND ( ptr u8 n ptr u8 n -- n n bool ) {: text:ptr text-u rx:ptr rx-u :}
   text-u rx rx-u RX-PREPARE
   text text-u rx rx-u 0 RX-FIND-FROM ;

: RX-COUNT ( ptr u8 n ptr u8 n -- n ) {: text:ptr text-u rx:ptr rx-u :}
   text-u rx rx-u RX-PREPARE
   0 RX-COUNT-N !
   0 RX-COUNT-POS !
   begin RX-COUNT-POS @ text-u <= while
      text text-u rx rx-u RX-COUNT-POS @ RX-FIND-FROM if
         RX-COUNT-N @ 1+ RX-COUNT-N !
         dup 0 > if
            +
         else
            drop 1+
         then
         RX-COUNT-POS !
      else
         2drop text-u 1 + RX-COUNT-POS !
      then
   repeat
   RX-COUNT-N @ ;
