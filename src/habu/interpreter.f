\ interpreter.f - the interpreter written in Habu: numbers, the dictionary
\ search and the token readers, the package keywords, the definition heads and
\ body capture, then the loop, which reads a buffer token by token.
\ INTERPRETER:EVAL-FRAMED and EVAL-FRAMED-RANGE run the loop under the engine's
\ evaluate frame, and INTERPRETER:INSTALL-EVALUATE stores them in the engine's
\ evaluate cells.
\
\ NUMBER reads a token: an optional `-`, an optional `$` for radix 16 with
\ digits a-f and A-F, then digits. A radix-10 token may end in one `.` and one
\ or more digits, which makes it a float; its value is then the double's bits.
\ A token of that shape can still be out of range, and the interpreter then
\ reports it undefined without looking it up as a name. Its digits may not
\ carry past 2^64 - 1. A decimal integer's magnitude is at most MAX-N, or 2^63
\ when negative; a hex integer keeps all 64 bits and negates modulo 2^64. A
\ float's integer part is at most MAX-N and its fraction at most 18 digits.
\
\ A backslash or `(` opens a comment only as a whole one-byte token. The comment
\ runs past the next newline or `)`. A backslash comment may also run to the end
\ of the input; a `(` comment the input ends inside is refused at the `(`.
\
\ A float's exact value, the integer plus the fraction's remainder, is
\ normalized to 53 retained bits and rounded once to nearest, ties to even; the
\ sign applies even to zero.

require src/lib/prelude.f
require src/core/bytes.f
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/wl.f
require src/habu/xref.f
require src/compiler/native/dict.f
require src/habu/escape.f
require src/core/top-row.f

package INTERPRETER
private

$7FFFFFFFFFFFFFFF constant MAX-N
$8000000000000000 constant MIN-N


\ Unsigned order: flipping the sign bit maps it onto signed order.
: U> ( n n -- bool )
   MIN-N xor swap MIN-N xor < ;


: BETWEEN? ( n n n -- bool ) {: c:n lo:n hi:n :}
   c lo >= c hi <= and ;


: DIGIT ( n n -- n bool ) {: c:n radix:n :}
   c [char] 0 [char] 9 BETWEEN? if c [char] 0 - true exit then
   radix 16 <> if 0 false exit then
   c [char] a [char] f BETWEEN? if c [char] a - 10 + true exit then
   c [char] A [char] F BETWEEN? if c [char] A - 10 + true exit then
   0 false ;


: AT? ( ptr u8 n n n -- bool ) {: a:ptr u:n idx:n c:n :}
   idx u < if a idx + c@ c = else false then ;


: RUN-END ( ptr u8 n n n -- n ) {: a:ptr u:n idx:n radix:n :}
   idx begin
      dup u < if dup a + c@ radix DIGIT nip else false then
   while 1+ repeat ;


\ 2^64 - 1 is limit * radix + top, so int * radix + d carries out of the
\ cell when int is past limit, or at limit with d past top.
: CARRY-BOUND ( n -- n n )
   16 = if $0FFFFFFFFFFFFFFF 15 else 1844674407370955161 5 then ;


: CARRIES? ( n n n -- bool ) {: int:n d:n radix:n :}
   radix CARRY-BOUND {: limit:n top:n :}
   int limit U>  int limit = d top > and  or ;


\ Wrapped latches the first carry out of the cell.
: INT-STEP ( n bool n n -- n bool ) {: int:n wrapped:bool d:n radix:n :}
   int radix * d +
   int d radix CARRIES? wrapped or ;


: INTEGER ( ptr u8 n n n -- n bool ) {: a:ptr lo:n hi:n radix:n :}
   0 false
   hi lo ?do a i + c@ radix DIGIT drop radix INT-STEP loop ;


\ The fraction stays below the scale, so bounding the scale by MAX-N / 10 (18
\ digits) keeps both in a signed cell; past it the latch refuses the token.
: FRAC-STEP ( n n bool n -- n n bool ) {: frac:n scale:n wrapped:bool d:n :}
   frac 10 * d +
   scale 10 *
   scale MAX-N 10 / > wrapped or ;


: FRACTION ( ptr u8 n n bool -- n n bool ) {: a:ptr lo:n hi:n wrapped:bool :}
   0 1 wrapped
   hi lo ?do a i + c@ 10 DIGIT drop FRAC-STEP loop ;


: FRACTION? ( ptr u8 n n n -- bool ) {: a:ptr u:n dot:n radix:n :}
   radix 10 <> if false exit then
   a u dot [char] . AT? 0= if false exit then
   dot 1+ u <  a u dot 1+ 10 RUN-END u =  and ;


: NOT-NUMBER ( -- n bool bool bool )
   0 false false false ;


: OUT-OF-RANGE ( -- n bool bool bool )
   0 false false true ;


: INT-OUT? ( n bool bool n -- bool ) {: int:n wrapped:bool neg:bool radix:n :}
   wrapped if true exit then
   radix 16 = if false exit then
   neg if int MIN-N U> else int MAX-N U> then ;


: INT-FINISH ( n bool bool n -- n bool bool bool ) {: int:n wrapped:bool neg:bool radix:n :}
   int wrapped neg radix INT-OUT? if OUT-OF-RANGE exit then
   int neg if negate then false true false ;


\ With frac < scale <= 10^18, doubling the remainder always fits a cell. Every
\ admitted nonzero value is at least 10^-18: none is subnormal.
: FLOAT-LEFT-STEP ( n n n n -- n n n n )
   {: sig:n rem:n scale:n exp:n :}
   rem 2 * {: twice:n :}
   twice scale >= if
      sig 2 * 1+ twice scale - scale exp 1-
   else
      sig 2 * twice scale exp 1-
   then ;

: FLOAT-RIGHT-STEP ( n n n bool -- n n n bool )
   {: sig:n exp:n guard:n sticky:bool :}
   sig 1 rshift exp 1+ sig 1 and sticky guard 0<> or ;

: FLOAT-ROUND ( n n n bool -- n )
   {: sig:n exp:n guard:n sticky:bool :}
   sig guard 0<> sticky sig 1 and 0<> or and if 1+ then
   dup $20000000000000 = if 1 rshift exp 1+ else exp then
   1023 + 52 lshift swap $FFFFFFFFFFFFF and or ;

: FLOAT-BITS ( n n n -- n )
   {: int:n frac:n scale:n :}
   int frac or 0= if 0 exit then
   int $20000000000000 >= if
      int 52 0 frac 0<>
      begin 2over drop $20000000000000 >= while FLOAT-RIGHT-STEP repeat
      FLOAT-ROUND exit
   then
   int frac scale 52
   begin 2over drop $10000000000000 < while FLOAT-LEFT-STEP repeat
   {: sig:n rem:n denom:n exp:n :}
   rem 2 * {: twice:n :}
   twice denom >= if sig exp 1 twice denom <> else sig exp 0 twice 0<> then
   FLOAT-ROUND ;

: FLOAT-FINISH ( n n n bool bool -- n bool bool bool )
   {: int:n frac:n scale:n wrapped:bool neg:bool :}
   wrapped int MAX-N U> or if OUT-OF-RANGE exit then
   int frac scale FLOAT-BITS
   neg if MIN-N or then true true false ;


\ An optional `-`, then an optional `$` for radix 16: the index after them,
\ whether `-` led, and the radix.
: PREFIX ( ptr u8 n -- n bool n ) {: a:ptr u:n :}
   a u 0 [char] - AT? {: neg:bool :}
   neg if 1 else 0 then {: idx:n :}
   a u idx [char] $ AT? if idx 1+ neg 16 else idx neg 10 then ;

public

\ The token's value (a float's bits), whether it is a float, whether it is a
\ number, and whether its shape is a number's but its value out of range.
\ The value and the float flag are zero unless it is a number.
: NUMBER ( ptr u8 n -- n bool bool bool ) {: a:ptr u:n :}
   a u PREFIX {: lo:n neg:bool radix:n :}
   lo u >= if NOT-NUMBER exit then
   a u lo radix RUN-END {: dot:n :}
   a lo dot radix INTEGER {: int:n wrapped:bool :}
   dot u = if int wrapped neg radix INT-FINISH exit then
   a u dot radix FRACTION? 0= if NOT-NUMBER exit then
   int  a dot 1+ u wrapped FRACTION  neg FLOAT-FINISH ;

;package

\ FIND answers the row the engine's lookup binds a token to whatever its flags:
\ the interpreter must see an internal row in order to refuse it.

package INTERPRETER

private

\ Not the lookup: the row one wordlist holds under a name, whatever its flags.
: WL-PROBE ( ptr u8 n n -- ptr n ) xref-search-wl ;

: COLON-AT ( ptr u8 n n -- n )
   {: a:ptr u:n from:n :}
   u from ?do
      a i + c@ $3A = if i unloop exit then
   loop
   -1 ;

\ The scope's answer (open package, enclosing package, global, NAME:tail), never
\ a used public's: a row the used publics bound is a miss, except in the open
\ or the enclosing package's public wordlist, which the scope searched first.
: FIND-SCOPE ( ptr u8 n -- ptr n )
   scope-find drop drop {: rec:ptr used:ptr :}
   rec used <> if rec exit then
   rec XREF-FOUND? 0= if rec exit then
   rec XREF-WORDLIST {: wl:n :}
   wl NDICT:OPEN-PUB = if rec exit then
   wl data-base ENCLOSING:PUB + @ = if rec exit then
   XREF-NULL ;

public

: FIND ( ptr u8 n -- ptr n )
   NDICT:SCOPE-REC ;

;package

\ The loop reads the engine's input cells, so a parsing word reads its buffer.

package INTERPRETER

private

70 constant RC-REJECT            \ a refused token, catchable
74 constant RC-BAD-LITERAL       \ no closing quote, or a bad escape
74 constant RC-NO-NAME           \ a keyword's operand is missing
74 constant RC-UNCLOSED          \ the input ends inside a definition or a ( comment, or closes one it did not open
76 constant RC-TOO-LONG          \ a counted string past CSTR-MAX bytes
79 constant RC-TASK-THREAD       \ source read on a task's thread
52 constant MIN-IN-SHIFT         \ record flag bits 52-59
32 constant BLANK                \ every byte at or below it separates tokens
$0A constant NEWLINE
$5C constant LINE-COMMENT        \ a backslash
$28 constant OPEN-COMMENT        \ (
$29 constant CLOSE-COMMENT       \ )
$22 constant QUOTE               \ a double quote closes a string literal
$20 constant CASE-BIT            \ ORed into A-Z it gives a-z
255 constant CSTR-MAX            \ a counted string's length is one byte

create NL NEWLINE c,

: CELL@ ( n -- n )
   data-base + @ ;

: CELL! ( n n -- )
   data-base + ! ;

\ The input cells hold byte addresses. These two are the file's one crossing
\ between such a cell and a byte pointer.
: ADDR@ ( n -- ptr u8 ) data-base + 0 ptr-field @ ;
: ADDR! ( ptr u8 n -- ) data-base + 0 ptr-field ! ;

\ A cell of a frame the engine holds.
: FR! ( n ptr u8 n -- )
   + CELL-VIEW ! ;

: FR@ ( ptr u8 n -- n )
   + CELL-VIEW @ ;

: TOKEN$ ( -- ptr u8 n )
   TKA-CELL ADDR@ TKL-CELL CELL@ ;

: FOLD ( n -- n ) {: c:n :}
   c [char] A [char] Z BETWEEN? if c CASE-BIT or exit then
   c ;

\ Whether the span a u spells kw, which is lowercase. Only the span's A-Z are
\ folded.
: FOLDED= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n kw:ptr v:n :}
   u v <> if false exit then
   u 0 ?do
      a i + c@ FOLD  kw i + c@ <> if false unloop exit then
   loop
   true ;

: TOKEN-IS? ( ptr u8 n -- bool )
   TOKEN$ 2swap FOLDED= ;

: BLANKS-END ( ptr u8 ptr u8 -- ptr u8 ) {: p:ptr e:ptr :}
   p begin dup e < if dup c@ BLANK <= else false then while 1 + repeat ;

: WORD-END ( ptr u8 ptr u8 -- ptr u8 ) {: p:ptr e:ptr :}
   p begin dup e < if dup c@ BLANK > else false then while 1 + repeat ;

\ With no token left INP is the end and the token cells keep the last token.
: TOKEN ( -- bool )
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:ptr e:ptr :}
   p e BLANKS-END {: s:ptr :}
   s e WORD-END {: t:ptr :}
   t INP-CELL ADDR!
   s t = if false exit then
   s TKA-CELL ADDR!
   t s - TKL-CELL CELL!
   true ;

: REFUSE ( ptr u8 n -- )
   ERROR-TEXT:TYPE TOKEN$ ERROR-TEXT:TYPE NL 1 ERROR-TEXT:TYPE
   RC-REJECT throw ;

: REFUSAL ( ptr u8 n -- )
   SRCLOC:PATH-CELL ADDR@ SRCLOC:PATHLEN-CELL CELL@ SRCLOC:LINE
   ERROR-TEXT:PREFIX ;

: UNDEFINED-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   s" E-UNDEFINED" REFUSAL
   s" undefined word '" ERROR-TEXT:TYPE  a u ERROR-TEXT:TYPE
   s" '" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   RC-REJECT throw ;

: UNDERFLOW ( -- )
   s" E-UNDERFLOW" REFUSAL
   s" stack underflow at '" ERROR-TEXT:TYPE  TOKEN$ ERROR-TEXT:TYPE
   s" '" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   RC-REJECT throw ;

: UNDEFINED ( -- )
   TOKEN$ UNDEFINED-NAME ;

: AMBIGUOUS ( -- )
   s" E-USING-AMBIGUOUS" REFUSAL
   s" ambiguous bare word resolves in multiple used packages: " ERROR-TEXT:TYPE
   TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   ENGINE-ERROR:USING-AMBIGUOUS throw ;

: SHADOWED ( -- )
   s" E-USING-SHADOW-GLOBAL" REFUSAL
   s" bare word a global and a used package both export: " ERROR-TEXT:TYPE
   TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   ENGINE-ERROR:USING-SHADOW-GLOBAL throw ;

\ No program code runs after the text: the exit hook is cleared before `die`,
\ which would otherwise call it.
: FAIL-CLOSED ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   a u ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   0 EXIT-HOOK-CELL CELL!
   s" " rc die ;

: SEALED-FAIL ( ptr u8 n -- ) {: a:ptr u:n :}
   s" E-SEALED-PACKAGE" REFUSAL
   s" '" ERROR-TEXT:TYPE  a u ERROR-TEXT:TYPE  s" ' is sealed" ERROR-TEXT:TYPE
   s" " ENGINE-ERROR:SEAL-PACKAGE FAIL-CLOSED ;

\ While a checker overlay is open a definer head, a new package row or private
\ wid, or an export throws by name: the overlay's close would drop what it
\ wrote.
: OVERLAY-GUARD ( -- )
   REPLAY-SCOPE:LATCH CELL@ 0= if exit then
   s" E-OVERLAY-OPEN" REFUSAL
   s" definition while a checker replay is open: " ERROR-TEXT:TYPE
   TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   ENGINE-ERROR:OVERLAY-OPEN throw ;

\ Whether c was found: INP stops past it, else at the end of the input.
: SKIP-PAST ( n -- bool )
   {: c:n :}
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:ptr e:ptr :}
   p begin dup e < if dup c@ c <> else false then while 1 + repeat
   dup e < {: found:bool :}
   found if 1 + then
   INP-CELL ADDR!
   found ;

\ The refusal's line is the `(`'s, so the cursor goes back to the token.
: UNCLOSED ( -- )
   TKA-CELL CELL@ INP-CELL CELL!
   s" E-UNTERMINATED-COMMENT" REFUSAL
   s" source ended inside a ( comment" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   RC-UNCLOSED throw ;

: COMMENT? ( -- bool )
   TKL-CELL CELL@ 1 <> if false exit then
   TKA-CELL ADDR@ c@ {: b:n :}
   b LINE-COMMENT = if NEWLINE SKIP-PAST drop true exit then
   b OPEN-COMMENT = if CLOSE-COMMENT SKIP-PAST 0= if UNCLOSED then true exit then
   false ;

\ A word or the hook runs through execute-floor, which answers whether it left
\ the stack below its base and then resets the stack to the base.
: FLOORED ( bool -- )
   if UNDERFLOW then ;

\ CHECK:TOP-LEVEL gets ( token class flags ) for each number after the push
\ and for each word, past its gates, before it runs, and consumes those four
\ cells. A check that leaves the stack below its base on a word event is
\ refused here, before the word runs.
CAST: HOOK-CALL ( [ -- ] -- n )

\ The call runs under the floor, so it pushes the event itself: a quotation
\ reads no local, and the class and flags wait in these cells.
variable EVENT-CLASS
variable EVENT-FLAGS

: HOOK ( n n -- )
   EVENT-FLAGS ! EVENT-CLASS !
   [: TOKEN$ EVENT-CLASS @ EVENT-FLAGS @ CHECK:TOP-LEVEL ;]
   HOOK-CALL execute-floor FLOORED ;

variable VALUE

: NUMERAL? ( -- bool )
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   range if UNDEFINED then
   v VALUE !
   num ;

\ EVAL-FRAMED-RANGE restores it as the buffer ends, since a stripped image
\ would hold a record that outlived its buffer as an undeclared dictionary
\ pointer.
TYPED-VARIABLE REC ptr n

\ A sealed source may name a record it defined after the seal or a callable
\ public record in an admitted wordlist.
: POLICY-REFUSE ( -- )
   s" E-POLICY" REFUSAL
   s" not in vocabulary: " ERROR-TEXT:TYPE
   TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  ENGINE-ERROR:POLICY throw ;

: POLICY-CHECK-REC ( ptr n -- ) {: rec:ptr :}
   POLICY-NDICT-CELL CELL@ {: seal:n :}
   seal 0= if exit then
   rec XREF-FOUND? 0= if POLICY-REFUSE then
   rec XREF-NULL - seal XREF-REC-ADDR < if
      rec XREF-WORDLIST {: wid:n :}
      wid 0 < wid PROT-WID-MAX >= or if POLICY-REFUSE then
      wid 6 rshift 8 * data-base POLICY-BITS-OFF + + @
      wid 63 and rshift 1 and 0= if POLICY-REFUSE then
   then
   rec XREF-FLAGS DNAME-INT and 0<> if POLICY-REFUSE then
   rec XREF-WORDLIST OWNER-API-PRI-WID = if POLICY-REFUSE then
   rec XREF-START 0= if POLICY-REFUSE then ;

\ At top level the ordinary `require`'s global record, spelled so, is in the
\ vocabulary as well. A body never admits it.
: POLICY-REQUIRE? ( ptr n -- bool ) {: rec:ptr :}
   POLICY-REQUIRE-CELL CELL@ {: req:n :}
   req 0= if false exit then
   rec XREF-FOUND? 0= if false exit then
   rec XREF-START req <> if false exit then
   rec XREF-WORDLIST WL:GLOBAL? 0= if false exit then
   s" require" TOKEN-IS? ;

: POLICY-CHECK-TOP ( ptr n -- ) {: rec:ptr :}
   rec POLICY-REQUIRE? if exit then
   rec POLICY-CHECK-REC ;

: LOOKUP-GO ( -- )
   TOKEN$ FIND REC !
   REC @ POLICY-CHECK-TOP
   REC @ XREF-FOUND? 0= if exit then
   REC @ XREF-WORDLIST WL:GLOBAL? 0= if exit then
   TOKEN$ scope-find {: rec:ptr used:ptr used2:ptr flags:n :}
   flags SCOPE-FIND-AMBIGUOUS and 0<> if E-USING-AMBIGUOUS throw then
   used XREF-FOUND? if SHADOWED then ;

: SEARCH ( -- )
   XREF-NULL REC !
   [: LOOKUP-GO ;] catch {: code:n :}
   code E-USING-AMBIGUOUS = if AMBIGUOUS then
   code 0<> if code throw then ;

: LOOKUP ( -- )
   SEARCH
   REC @ XREF-FOUND? 0= if UNDEFINED then ;

: MIN-IN ( n -- n )
   DNAME-MIN-IN-MASK and MIN-IN-SHIFT rshift ;

\ The gates before a record's xt leaves the loop, in the engine's order: a
\ wide effect, then an internal word.
: XT-GATE ( -- )
   REC @ XREF-FLAGS {: f:n :}
   f DNAME-WIDE and 0<> if
      s" E-LAYOUT-VALUE" REFUSAL  s" interpret-mode layout value: " REFUSE
   then
   f DNAME-INT and 0<> if
      s" E-UNSAFE" REFUSAL  s" internal engine word: " REFUSE
   then ;

\ Name dispatch and tick reject an exact native code entry, including an alias,
\ when it carries a baked scope kind. Arbitrary untyped xref xt execution is
\ outside these paths.
: SCOPE-ENTRY-GUARD ( -- )
   REC @ XREF-START scope-kind? 0<> if
      s" E-UNSAFE" REFUSAL  s" internal engine word: " REFUSE
   then ;

: GATE ( -- )
   XT-GATE
   SCOPE-ENTRY-GUARD
   depth REC @ XREF-FLAGS MIN-IN < if
      s" E-UNDERFLOW" REFUSAL  s" interpret stack underdepth: " REFUSE
   then ;

\ The flag word as the hook reads it: bit 0 found, bit 1 immediate, bits 8-15
\ the certified inputs.
: WORD-FLAGS ( -- n )
   REC @ XREF-FLAGS {: f:n :}
   1 f DNAME-IMM and 0<> if 2 or then
   f MIN-IN 8 lshift or ;

\ The dictionary record index of each word run since RECORD-RUNS, in order.
DYNAMIC-BUFFER RUNS n
variable RUNS-N
variable RUNS-CAP
TYPED-VARIABLE RUNS-ON bool

: RUN+ ( n -- ) {: idx:n :}
   RUNS-ON @ 0= if exit then
   RUNS-N @ RUNS-CAP @ = if
      RUNS-CAP @ 0= if 1024 else RUNS-CAP @ 2 * then
      dup RUNS-RESERVE RUNS-CAP !
   then
   idx RUNS-N @ RUNS !
   RUNS-N @ 1 + RUNS-N ! ;


\ The flags and xt are read before the hooks run.
: RUN-FOUND ( -- )
   GATE
   WORD-FLAGS REC @ XREF-START {: flags:n xt:n :}
   TOP-EV-WORD flags HOOK
   REC @ XREF-NULL - XREF-ADDR>INDEX {: idx:n :}
   idx RUN+
   xt execute-floor FLOORED ;

: RUN-WORD ( -- )
   LOOKUP RUN-FOUND ;

\ A keyword is matched before the token is read as a number or a word, and it
\ reads the input after it itself. Each keyword table is a word that runs the
\ token when it is one of its rows and answers whether it was.

\ With the input at its end the refusal names kw lowercase whatever the token's
\ case, at the end's line.
: OPERAND ( ptr u8 n -- ) {: kw:ptr u:n :}
   TOKEN if exit then
   s" E-MISSING-NAME" REFUSAL
   s" reader keyword needs a name: " ERROR-TEXT:TYPE
   kw u ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   RC-NO-NAME throw ;

\ A literal's text starts one byte past the keyword, past the blank that ended
\ it, and runs to a closing quote that INP then passes. A literal with no
\ closing quote refuses with INP still after the keyword, so the refusal names
\ the keyword's line. A literal a program keeps is copied into data space it
\ allots first, after its own refusals, so allot's refusals come before any of
\ it is written.

: BAD-LITERAL ( -- )
   s" E-UNTERMINATED-STRING" REFUSAL
   s" bad string literal" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   RC-BAD-LITERAL throw ;

: TOO-LONG ( -- )
   s" E-COUNTED-STRING-TOO-LONG" REFUSAL
   s" counted string too long (max 255)" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   RC-TOO-LONG throw ;

: PAST ( ptr u8 -- )
   1 + INP-CELL ADDR! ;

: ROOM ( n -- ptr u8 )
   here swap allot ;

: TEXT ( -- ptr u8 n )
   INP-CELL ADDR@ 1 + INE-CELL ADDR@ {: s:ptr e:ptr :}
   s begin dup e < if dup c@ QUOTE <> else false then while 1 + repeat
   {: q:ptr :}
   q e >= if BAD-LITERAL then
   q PAST
   s q s - ;

: COPY-HERE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u ROOM {: d:ptr :}
   a d u BYTE-COPY
   d u ;

: KEEP ( -- ptr u8 n )
   TEXT COPY-HERE ;

\ c": a count byte, then the text. INP has passed the quote when the length
\ is checked, so the refusal names the quote's line.
: COUNTED ( -- ptr u8 )
   TEXT {: a:ptr u:n :}
   u CSTR-MAX > if TOO-LONG then
   u 1 + ROOM {: d:ptr :}
   u d c!
   a d 1 + u BYTE-COPY
   d ;

\ An escaped literal's start, its closing quote and the count of bytes it
\ decodes to. A bad escape refuses as a missing quote does, INP unmoved.
: ESC-SCAN ( -- ptr u8 ptr u8 n )
   INP-CELL ADDR@ 1 + {: s:ptr :}
   s INE-CELL ADDR@ ESCAPE:SPAN {: q:ptr u:n ok:bool :}
   ok 0= if BAD-LITERAL then
   s q u ;

: ESC-KEEP ( -- ptr u8 n )
   ESC-SCAN {: s:ptr q:ptr u:n :}
   q PAST
   u ROOM {: d:ptr :}
   s q d ESCAPE:COPY
   d u ;

\ c\" checks the decoded length before INP moves, so its refusal names the
\ keyword's line.
: ESC-COUNTED ( -- ptr u8 )
   ESC-SCAN {: s:ptr q:ptr u:n :}
   u CSTR-MAX > if TOO-LONG then
   q PAST
   u 1 + ROOM {: d:ptr :}
   u d c!
   s q d 1 + ESCAPE:COPY
   d ;

\ A literal a program keeps is program code: the quotation that pushes it runs
\ under the floor as a word does. The cells it leaves are the program's, so
\ these views state its effect only to project it to the number execute-floor
\ takes.
CAST: STR-LITERAL ( [ -- ptr u8 n ] -- n )
CAST: CSTR-LITERAL ( [ -- ptr u8 ] -- n )
CAST: CELL-LITERAL ( [ -- n ] -- n )
\ A cell a definer takes from the program.
CAST: CELL-TAKE ( [ n -- ] -- n )

: RUN-LITERAL ( n -- )
   execute-floor FLOORED ;

: PUSH-STR ( -- )
   [: KEEP TOP-EV-STR 0 HOOK ;] STR-LITERAL RUN-LITERAL ;

: PUSH-CSTR ( -- )
   [: COUNTED TOP-EV-CSTR 0 HOOK ;] CSTR-LITERAL RUN-LITERAL ;

: TYPE-STR ( -- )
   TEXT type ;

: PUSH-ESC-STR ( -- )
   [: ESC-KEEP TOP-EV-STR 0 HOOK ;] STR-LITERAL RUN-LITERAL ;

: PUSH-ESC-CSTR ( -- )
   [: ESC-COUNTED TOP-EV-CSTR 0 HOOK ;] CSTR-LITERAL RUN-LITERAL ;

: TYPE-ESC-STR ( -- )
   ESC-KEEP type ;

: OPEN-PKG-NAME ( -- ptr u8 n )
   PKG-REC-CELL CELL@ {: rec:n :}
   rec 0= if 0 ROOM 0 exit then
   rec XREF-ADDR>REC XREF-NAME$ {: a:ptr u:n :}
   u ROOM {: d:ptr :}
   u 0 ?do a i + c@ XREF-FOLD-C d i + c! loop
   d u ;

: PUSH-PKG ( -- )
   [: OPEN-PKG-NAME TOP-EV-STR 0 HOOK ;] STR-LITERAL RUN-LITERAL ;

\ No definition is open at top level, so no body text captures the operand.
: FIRST-BYTE ( -- n )
   s" char" OPERAND
   TOKEN$ drop c@ ;

\ The hook sees the operand as the token.
: PUSH-CHAR ( -- )
   [: FIRST-BYTE TOP-EV-CHAR 0 HOOK ;] CELL-LITERAL RUN-LITERAL ;

\ The active checker owner supplies the trusted-only query, not a source name.
: TICK-QUERY-READY? ( -- bool )
   SEAL-NDICT@ 0= if false exit then
   NCOMP-DISPATCH:TARGET-DECL-A @ dup 0= if drop true exit then
   NCOMP-DISPATCH:DECL-A @ = ;

: TICK-QUERY ( -- n )
   NCOMP-DISPATCH:DECL-A @ dup 0= if drop 0 exit then
   NCOMP-DISPATCH:DECL-TRUSTED-TICK-OFF + CELL-VIEW @ ;

\ The owner record's trusted-tick field holds a raw execution token.
CAST: TICK-ACTION ( n -- [ ptr u8 n -- bool ] )

: TRUSTED-TICK? ( -- bool )
   TICK-QUERY-READY? 0= if false exit then
   TICK-QUERY dup 0= if drop false exit then
   TICK-ACTION TOKEN$ rot execute ;

\ A tick runs nothing, so it has no depth gate.
: TICKED ( -- )
   s" '" OPERAND
   LOOKUP
   XT-GATE
   SCOPE-ENTRY-GUARD
   TRUSTED-TICK? if
      s" E-CAP-TRUSTED" REFUSAL  s" trusted-only tick: " REFUSE
   then ;

\ The run log sees the tick's target before the xt is pushed: the file may
\ execute it.
: TICK-OBSERVE ( -- n )
   REC @ XREF-START {: xt:n :}
   REC @ XREF-NULL - XREF-ADDR>INDEX {: idx:n :}
   idx RUN+
   xt ;

\ The hook sees the operand as the token and the record's flags.
: PUSH-XT ( -- )
   [: TICKED TICK-OBSERVE TOP-EV-TICK WORD-FLAGS HOOK ;] CELL-LITERAL RUN-LITERAL ;

: LITERAL? ( -- bool )
   S\" s\q" TOKEN-IS? if PUSH-STR true exit then
   S\" c\q" TOKEN-IS? if PUSH-CSTR true exit then
   S\" .\q" TOKEN-IS? if TYPE-STR true exit then
   S\" s\\\q" TOKEN-IS? if PUSH-ESC-STR true exit then
   S\" c\\\q" TOKEN-IS? if PUSH-ESC-CSTR true exit then
   S\" .\\\q" TOKEN-IS? if TYPE-ESC-STR true exit then
   s" [package]" TOKEN-IS? if PUSH-PKG true exit then
   s" char" TOKEN-IS? if PUSH-CHAR true exit then
   s" '" TOKEN-IS? if PUSH-XT true exit then
   false ;

public

\ RECORD-RUNS … ;RECORD-RUNS logs the words run between them; the log is read
\ before ;RECORD-RUNS releases it.
: RECORD-RUNS ( -- )
   0 RUNS-N !  true RUNS-ON ! ;

: ;RECORD-RUNS ( -- )
   false RUNS-ON !  RUNS-RELEASE  0 RUNS-N !  0 RUNS-CAP ! ;

: RUN-COUNT ( -- n ) RUNS-N @ ;

\ RUNS refuses a negative entry; the capacity past the count is not the log.
: RUN-RECORD@ ( n -- n )
   dup RUN-COUNT >= if E-LAYOUT-BOUNDS throw then
   RUNS @ ;

;package

\ Rows, scope and aliases go through their primitive writers and CUR through
\ set-current. A writer refuses silently, so the words here refuse first where they can.

package INTERPRETER

private

\ The codes for these refusals: $4B a keyword out of its context or a colon in
\ a package name, $4C a long name past the code ceiling, $4D no record slot
\ left and $4E a tail the current wordlist already holds.
75 constant PKG-RC-CONTEXT
76 constant PKG-RC-CODE-FULL
77 constant PKG-RC-DICT-FULL
78 constant PKG-RC-DUPLICATE

\ The code ceiling's distance below the end of the region.
$4000 constant PKG-CODE-RESERVE

: PKG-INDEX ( ptr n -- n ) BYTE-VIEW NULL-PTR BYTE-VIEW - dbase@ - DREC / ;

: PKG-FAIL ( n -- )
   TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  throw ;

: PKG-CONTEXT-FAIL ( -- )
   s" E-PKG-CONTEXT" REFUSAL  PKG-RC-CONTEXT PKG-FAIL ;

: PKG-DUPLICATE ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" E-DUPLICATE-DEFINITION" REFUSAL
   s" duplicate definition: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   PKG-RC-DUPLICATE throw ;

\ With the input at its end the token cells still hold the keyword, which the
\ refusal names.
: PKG-NAME ( -- )
   TOKEN if exit then
   s" E-MISSING-NAME" REFUSAL
   RC-NO-NAME PKG-FAIL ;

\ The refusal names what DEF-TKA and DEF-TKL hold, which `package` never
\ writes: for it, what the last definition or `export` left there.
: PKG-DICT-FULL ( -- )
   s" E-DICTIONARY-FULL" REFUSAL
   s" dictionary full at: " ERROR-TEXT:TYPE
   DEF-TKA-CELL ADDR@ DEF-TKL-CELL CELL@ ERROR-TEXT:TYPE
   NL 1 ERROR-TEXT:TYPE  PKG-RC-DICT-FULL throw ;

: PKG-DICT-ROOM ( -- )
   ndict@ DICT-CAP < if exit then
   PKG-DICT-FULL ;

\ A long name copied to CP takes one whole target code slot. Both names an
\ export writes must fit before its checker declaration becomes pending.
: PKG-CODE-SLOT ( -- n )
   HB-TARGET-LINUX-X86-64? if 16 else 4 then ;

: PKG-CODE-SPAN ( n -- n )
   dup DNAME-INL <= if drop 0 exit then
   PKG-CODE-SLOT {: slot:n :}
   slot 1- + slot negate and ;

: PKG-FITS-AFTER? ( n n -- bool ) {: u:n used:n :}
   u DNAME-INL U> 0= if true exit then
   dbase@ REGION + PKG-CODE-RESERVE - {: ceil:n :}
   ceil cp@ U> 0= if false exit then
   ceil cp@ - {: room:n :}
   room used U> 0= if false exit then
   room used - {: left:n :}
   left u PKG-CODE-SPAN U> ;

: PKG-CODE-ROOM-AFTER ( ptr u8 n n -- ) {: a:ptr u:n used:n :}
   u used PKG-FITS-AFTER? if exit then
   s" E-CODE-SPACE-FULL" REFUSAL
   s" code space full at: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  PKG-RC-CODE-FULL throw ;

: PKG-CODE-ROOM ( ptr u8 n -- ) 0 PKG-CODE-ROOM-AFTER ;

\ The operation at off in the owner record a dispatch cell names, 0 when there
\ is no record or the field is empty: the call is then skipped, not refused.
: PKG-OPERATION ( ptr ptr u8 n -- n )
   {: cell:ptr off:n :}
   cell @ {: rec:ptr :}
   rec 0= if 0 exit then
   rec off + CELL-VIEW @ ;

\ The owner record stores raw execution tokens; these views state the
\ signatures called here.
CAST: PKG-AS-ACTION ( n -- [ -- ] )
CAST: PKG-AS-NAME-ACTION ( n -- [ ptr u8 n -- ] )
CAST: PKG-AS-EXPORT-ACTION ( n -- [ ptr u8 n n -- ] )

\ `package`, `public`, `private` and `;package` notify the target checker alone:
\ a loading checker is the target at once, the source owner only where none was.
: PKG-TARGET ( n -- n )
   NCOMP-DISPATCH:TARGET-DECL-A swap PKG-OPERATION ;

: PKG-NOTIFY ( n -- )
   PKG-TARGET {: xt:n :}
   xt 0= if exit then
   xt PKG-AS-ACTION execute ;

: PKG-NOTIFY-NAME ( n -- )
   PKG-TARGET {: xt:n :}
   xt 0= if exit then
   TOKEN$ xt PKG-AS-NAME-ACTION execute ;

\ `using` notifies the active checker, then the target checker unless its
\ operation is the same one. Each records the name at the using depth before it
\ grows.
: PKG-NOTIFY-USING ( -- )
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-USING-OFF PKG-OPERATION
   {: own:n :}
   own 0<> if TOKEN$ own PKG-AS-NAME-ACTION execute then
   NCOMP-DISPATCH:DECL-USING-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   target own = if exit then
   TOKEN$ target PKG-AS-NAME-ACTION execute ;

\ An actual export names the source checker's declaration callback. A direct
\ call only adds a checker row and has no engine alias to bind.
: PKG-CHECK-EXPORT ( -- )
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-EXPORT-OFF PKG-OPERATION
      {: xt:n :}
   xt 0= if
      s" E-AOT-SEED" REFUSAL
      s" checker-export dispatch unset" ENGINE-ERROR:AOT-SEED FAIL-CLOSED
   then
   TOKEN$ xt PKG-AS-NAME-ACTION execute ;

: PKG-NOTIFY-EXPORT ( ptr u8 n n -- )
   {: operand:ptr operand-u:n ord:n :}
   NCOMP-DISPATCH:TARGET-DECL-A @
      dup NULL-PTR = if drop exit then {: owner:ptr :}
   owner 8 - CELL-VIEW @ CHECKER-OWNER-ABI:EXPORT-ACCEPT-OFF CELL + < if exit then
   owner CHECKER-OWNER-ABI:EXPORT-ACCEPT-OFF + CELL-VIEW @ {: xt:n :}
   xt 0= if exit then
   operand operand-u ord xt PKG-AS-EXPORT-ACTION execute ;

: PKG-ROW ( -- ptr n )
   TOKEN$ BOOT:NAMESPACE-WL WL-PROBE ;

\ A sealed package ends the process when named again; a package is sealed only
\ by the last file that opens it.
: PKG-SEALED? ( -- bool )
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? 0= if false exit then
   row XREF:SEALED? ;

: PKG-SEAL-GUARD ( -- )
   PKG-SEALED? 0= if exit then
   TOKEN$ SEALED-FAIL ;

: PKG-POLICY-GUARD ( -- )
   POLICY-NDICT-CELL CELL@ {: seal:n :}
   seal 0= if exit then
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? 0= if exit then
   row PKG-INDEX seal >= if exit then
   s" E-POLICY" REFUSAL
   s" policy: package " ERROR-TEXT:TYPE TOKEN$ ERROR-TEXT:TYPE
   s"  predates the seal" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   ENGINE-ERROR:POLICY throw ;

: PKG-REOPEN ( ptr n -- n ) {: row:ptr :}
   row PKG-INDEX {: ix:n :}
   row XREF-PKG-PRIVATE 0= if ix namespace-private then
   ix ;

public
\ A file object's link gives package record n the private wordlist its file
\ made.
: PACKAGE-PRIVATE ( n -- )
   {: ix:n :}
   ix 0 <  ix ndict@ >=  or if E-COMP-STATE throw then
   ix XREF-REC XREF-WORDLIST BOOT:NAMESPACE-WL <> if E-COMP-STATE throw then
   ix XREF-REC XREF-PKG-PRIVATE 0<> if E-COMP-STATE throw then
   ix namespace-private ;
private

\ The index of the row `package` opens; a new row takes a public and a private
\ wid.
: PKG-ENSURE ( -- n )
   PKG-ROW {: row:ptr :}
   row XREF-FOUND? if row PKG-REOPEN exit then
   OVERLAY-GUARD
   PKG-DICT-ROOM
   TOKEN$ PKG-CODE-ROOM
   TOKEN$ true namespace-record ;

\ namespace-record's name refusals, as E-COMP-STATE: a colon at either edge
\ and a second colon.
: PKG-PATH-CK ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 0 < if exit then
   q 0=  q 1+ u =  or if E-COMP-STATE throw then
   a u q 1+ COLON-AT 0 < 0= if E-COMP-STATE throw then ;

public
\ A file object's link records package a u, which must lie in the caller's
\ buffer, with its private wordlist right after the public one when the flag is
\ set.
: PACKAGE-RECORD ( ptr u8 n bool -- n )
   {: a:ptr u:n own:bool :}
   u 1 < if E-COMP-STATE throw then
   a u PKG-PATH-CK
   PKG-DICT-ROOM
   a u PKG-CODE-ROOM
   a u BOOT:NAMESPACE-WL WL-PROBE XREF-FOUND? if
      a u PKG-DUPLICATE
   then
   a u own namespace-record ;
private

: PKG-CONTEXT ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc E-PKG-CONTEXT = if PKG-CONTEXT-FAIL then
   rc throw ;

\ Inside package A the token cells name `A:<token>`.
: PKG-PATH ( -- )
   TOKEN$ CHECKER-RESOLVE:PART-PATH TKL-CELL CELL! TKA-CELL ADDR! ;

\ The using depth it opens at is the one `;package` restores, a part's its
\ package's.
: PKG-PACKAGE ( -- )
   [: CHECKER-RESOLVE:PART-GUARD ;] catch PKG-CONTEXT
   PKG-NAME
   TOKEN$ 0 COLON-AT 0 >= if PKG-CONTEXT-FAIL then
   ['] PKG-PATH catch PKG-CONTEXT
   PKG-POLICY-GUARD
   NCOMP-DISPATCH:DECL-PACKAGE-OFF PKG-NOTIFY-NAME
   PKG-SEAL-GUARD
   PKG-ENSURE {: ix:n :}
   USE-DEPTH-CELL CELL@ USE-PKG-SAVE-CELL CELL!
   ix get-current package-scope!
   PKG-PRI-CELL CELL@ set-current
   0 PKG-OUTSIDE-CELL CELL! ;

: PKG-SECTION ( n n -- ) {: cell:n off:n :}
   cell CELL@ 0= if PKG-CONTEXT-FAIL then
   off PKG-NOTIFY
   cell CELL@ set-current ;

: PKG-END-PACKAGE ( -- )
   PKG-PUB-CELL CELL@ 0= if PKG-CONTEXT-FAIL then
   NCOMP-DISPATCH:DECL-END-PACKAGE-OFF PKG-NOTIFY
   PKG-PARENT-CELL CELL@ set-current
   ENCLOSING:REC CELL@ {: outer:n :}
   outer 0= if -1 0 else outer XREF-ADDR>INDEX ENCLOSING:PARENT CELL@ then
   package-scope!
   USE-PKG-SAVE-CELL CELL@ USE-DEPTH-CELL CELL! ;

: PKG-SEAL ( -- )
   PKG-PUB-CELL CELL@ 0= if PKG-CONTEXT-FAIL then
   PKG-REC-CELL CELL@ XREF-ADDR>REC XREF:SEAL ;

: PKG-WID-SEALED? ( n -- bool ) {: wid:n :}
   wid FIRST-DYNAMIC-WID < if false exit then
   wid BOOT:GLOBAL-WL =  wid BOOT:TYPES-WL =  or if false exit then
   PKG-PUB-CELL CELL@ 0<> if
      wid PKG-PUB-CELL CELL@ =  wid PKG-PRI-CELL CELL@ =  or if
         PKG-REC-CELL CELL@ XREF-ADDR>REC XREF:SEALED? exit
      then
   then
   wid XREF:WID-SEALED? ;

\ What the engine's evaluate frame keeps of the package scope beside the using
\ depth, which a throw puts back.
: PKG-STATE ( -- n n n n n n )
   ENCLOSING:REC CELL@  ENCLOSING:PARENT CELL@
   PKG-REC-CELL CELL@  PKG-PARENT-CELL CELL@  get-current  USE-PKG-SAVE-CELL CELL@ ;

\ The sealed writers run only when the failed input changed the scope. A part
\ opens only inside its enclosing package, so that package opens first.
: PKG-RECOVER ( n n n n n n -- )
   {: outer-rec:n outer-parent:n rec:n parent:n cur:n floor:n :}
   floor USE-PKG-SAVE-CELL CELL!
   ENCLOSING:REC CELL@ outer-rec <>  ENCLOSING:PARENT CELL@ outer-parent <> or
   PKG-REC-CELL CELL@ rec <> or  PKG-PARENT-CELL CELL@ parent <> or
   get-current cur <> or if
      outer-rec 0<> if outer-rec XREF-ADDR>INDEX outer-parent package-scope! then
      rec 0= if -1 0 package-scope! else rec XREF-ADDR>INDEX parent package-scope! then
      cur set-current
   then
   NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF PKG-NOTIFY ;

: PKG-USING-FAIL ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   a u ERROR-TEXT:TYPE TOKEN$ ERROR-TEXT:TYPE NL 1 ERROR-TEXT:TYPE rc throw ;

: PKG-USED-WID ( -- n )
   TOKEN 0= if
      s" E-MISSING-NAME" REFUSAL
      s" using: missing package name" ERROR-TEXT:TYPE
      NL 1 ERROR-TEXT:TYPE  ENGINE-ERROR:USING-NO-NAME throw
   then
   TOKEN$ 0 COLON-AT 0 >= if
      s" E-USING-BAD-NAME" REFUSAL
      s" using: package name must not contain ':': "
      ENGINE-ERROR:USING-BAD-NAME PKG-USING-FAIL
   then
   TOKEN$ CHECKER-RESOLVE:HEAD-ROW {: row:ptr :}
   row XREF-FOUND? 0= if
      s" E-USING-UNKNOWN" REFUSAL
      s" using: unknown package: " ENGINE-ERROR:USING-UNKNOWN PKG-USING-FAIL
   then
   row XREF-PKG-PUBLIC ;

\ The using depth the buffer may close down to: its entry depth, lowered to the
\ first using it opens after a `;package` closes a package opened before it.
variable USE-FLOOR

: PKG-USING ( -- )
   PKG-USED-WID {: wid:n :}
   USE-DEPTH-CELL CELL@ {: d:n :}
   d USE-MAX >= if
      s" E-USING-OVERFLOW" REFUSAL
      s" using: too many concurrent usings: "
      ENGINE-ERROR:USING-OVERFLOW PKG-USING-FAIL
   then
   d USE-FLOOR @ < if d USE-FLOOR ! then
   wid  d cells USE-WIDS-OFF +  CELL!
   PKG-NOTIFY-USING
   d 1+ USE-DEPTH-CELL CELL! ;

\ In an open package the most recent using must be the package's own: one
\ opened before `package` sits at or below the depth `;package` restores, so
\ closing it here would come undone there. Nor may a buffer close a using its
\ includer opened, at or below the buffer's floor: it would come back open
\ when the buffer ends.
: PKG-END-USING ( -- )
   USE-DEPTH-CELL CELL@ {: d:n :}
   d 0= if
      s" E-USING-UNBALANCED" REFUSAL
      s" ;using without an open using" ERROR-TEXT:TYPE
      NL 1 ERROR-TEXT:TYPE  ENGINE-ERROR:USING-UNBALANCED throw
   then
   PKG-PUB-CELL CELL@ 0<>  d USE-PKG-SAVE-CELL CELL@ <=  and if
      s" E-USING-OUTER" REFUSAL
      s" ;using would close a using opened outside the package" ERROR-TEXT:TYPE
      NL 1 ERROR-TEXT:TYPE  ENGINE-ERROR:USING-OUTER throw
   then
   d USE-FLOOR @ <= if
      s" E-USING-OUTER" REFUSAL
      s" ;using would close a using opened outside the file" ERROR-TEXT:TYPE
      NL 1 ERROR-TEXT:TYPE  ENGINE-ERROR:USING-OUTER throw
   then
   d 1- USE-DEPTH-CELL CELL! ;

: PKG-TAIL ( -- ptr u8 n )
   TOKEN$ {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 1 <  q 1+ u >=  or if a u exit then
   a q 1+ ZPTR+  u q - 1- ;

\ The source must not be internal: an alias would carry its body past the
\ interpret gate without the DNAME-INT mark, so the gate's refusal is made
\ here. DEF-TKA and DEF-TKL take the operand, which the refusals after them
\ name.
: PKG-EXPORT-SOURCE ( -- ptr n )
   TOKEN$ FIND-SCOPE {: src:ptr :}
   src XREF-FOUND? 0= if s" E-EXPORT-UNDEFINED" REFUSAL  RC-REJECT PKG-FAIL then
   src XREF-FLAGS DNAME-INT and 0<> if
      s" E-EXPORT-UNSAFE" REFUSAL
      s" internal engine word: " REFUSE
   then
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   src ;

\ The record index of the source's does> clause, which its export carries, or
\ -1 for a source that is not a definer.
: PKG-CLAUSE ( ptr n -- n ) {: src:ptr :}
   src PKG-INDEX {: k:n :}
   k XREF-DOES-COMPANION? if k 1+ exit then
   -1 ;

\ The export and its clause each take an occurrence, so two must remain before
\ the checker hears of the export: the allocation's first cell is the last
\ number issued, and -1 the last one that can be, so fewer than two remain
\ after -2 or -1.
: PKG-OCC-ROOM ( -- )
   data-base DEF-OCC:PTR-CELL + 0 ptr-field @ CELL-VIEW @ {: last:n :}
   last -2 =  last -1 =  or if DEF-OCC:E-EXHAUSTED throw then ;

\ The clause takes the slot above the export's own, and its name must not be
\ live in the current wordlist.
: PKG-CLAUSE-ROOM ( n -- ) {: k:n :}
   PKG-OCC-ROOM
   ndict@ 1+ DICT-CAP < 0= if PKG-DICT-FULL then
   k XREF-REC XREF-NAME$ {: a:ptr u:n :}
   a u get-current WL-PROBE XREF-FOUND? if
      a u PKG-DUPLICATE
   then ;

: PKG-CLAUSE-ALIAS ( n -- ) {: k:n :}
   k XREF-REC XREF-NAME$ {: a:ptr u:n :}
   a u k get-current alias-record ;

public
\ A file object's link publishes name a u, which must lie in the caller's
\ buffer, for record src in wordlist wid.
: ALIAS-RECORD ( ptr u8 n n n -- )
   {: a:ptr u:n src:n wid:n :}
   u 1 <  wid 0 <  or if E-COMP-STATE throw then
   SEAL-NDICT@ 0<>  wid XREF-WID-PROTECTED?  and if E-COMP-STATE throw then
   src 0 <  src ndict@ >=  or if E-COMP-STATE throw then
   src XREF-REC {: rec:ptr :}
   rec XREF-WORDLIST {: swid:n :}
   swid XREF:PACKAGE-WL?  swid XREF:RETIRED-WL?  or if E-COMP-STATE throw then
   rec XREF-FLAGS DNAME-INT and 0<> if E-COMP-STATE throw then
   PKG-DICT-ROOM
   a u PKG-CODE-ROOM
   a u wid WL-PROBE XREF-FOUND? if
      a u PKG-DUPLICATE
   then
   a u src wid alias-record ;
private

\ In a package, publish an existing word under its tail into the current
\ wordlist: its code, its immediate, wide and certified-input bits, and a
\ does> definer's clause in the slot above. At top level the name is consumed
\ and nothing else happens: there `export` is a build directive.
: PKG-EXPORT ( -- )
   PKG-PUB-CELL CELL@ 0= if PKG-NAME exit then
   PKG-NAME
   OVERLAY-GUARD
   PKG-EXPORT-SOURCE {: src:ptr :}
   TOKEN$ {: operand:ptr operand-u:n :}
   get-current PKG-WID-SEALED? if operand operand-u SEALED-FAIL then
   PKG-DICT-ROOM
   PKG-TAIL {: ta:ptr tu:n :}
   ta tu get-current WL-PROBE XREF-FOUND? if
      TOKEN$ PKG-DUPLICATE
   then
   src PKG-CLAUSE {: clause:n :}
   clause 0 >= if clause PKG-CLAUSE-ROOM then
   ta tu PKG-CODE-ROOM
   clause 0 >= if
      clause XREF-REC XREF-NAME$ tu PKG-CODE-SPAN PKG-CODE-ROOM-AFTER
   then
   PKG-CHECK-EXPORT
   ndict@ {: ord:n :}
   ta tu src PKG-INDEX get-current alias-record
   clause 0 >= if clause PKG-CLAUSE-ALIAS then
   operand operand-u ord PKG-NOTIFY-EXPORT ;

\ `host` or `target` at top level is the section word; with a word of that name in scope the
\ token could mean either, so it is refused: the word is renamed, or called qualified if public.
: SECTION? ( -- bool )
   s" host" TOKEN-IS?  s" target" TOKEN-IS? or 0= if false exit then
   SEARCH
   REC @ XREF-FOUND? if
      s" E-SECTION-WORD" REFUSAL
      s" '" ERROR-TEXT:TYPE  TOKEN$ ERROR-TEXT:TYPE
      s" ' is the section word and a word of that name is in scope: rename the word or call it qualified"
      ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
      RC-REJECT throw
   then
   s" target" TOKEN-IS? if 1 else 0 then SECTION-CELL CELL!
   true ;

: PACKAGE? ( -- bool )
   s" package" TOKEN-IS? if PKG-PACKAGE true exit then
   s" public" TOKEN-IS? if
      PKG-PUB-CELL NCOMP-DISPATCH:DECL-PUBLIC-OFF PKG-SECTION true exit
   then
   s" private" TOKEN-IS? if
      PKG-PRI-CELL NCOMP-DISPATCH:DECL-PRIVATE-OFF PKG-SECTION true exit
   then
   s" ;package" TOKEN-IS? if PKG-END-PACKAGE true exit then
   SECTION? if true exit then
   s" seal-package" TOKEN-IS? if PKG-SEAL true exit then
   s" using" TOKEN-IS? if PKG-USING true exit then
   s" ;using" TOKEN-IS? if PKG-END-USING true exit then
   s" export" TOKEN-IS? if PKG-EXPORT true exit then
   false ;

: PKG-CLEAR ( -- )
   -1 0 package-scope!
   NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF PKG-NOTIFY ;

public

\ Q runs as at top level: no package open, current where the outer package
\ opened. The scope comes back on either exit, Q's throw rethrown.
: WITHOUT-PACKAGE ( [ -- ] -- )
   {: q :}
   PKG-STATE {: outer-rec:n outer-parent:n rec:n parent:n cur:n floor:n :}
   PKG-OUTSIDE-CELL CELL@ {: outside:n :}
   PKG-CLEAR
   outer-rec 0<> if outer-parent set-current else
      rec 0<> if parent set-current then
   then
   q catch {: rc:n :}
   outer-rec outer-parent rec parent cur floor PKG-RECOVER
   outside PKG-OUTSIDE-CELL CELL!
   rc 0<> if rc throw then ;

;package

\ A definition head refuses in the engine's order and with its text: a
\ definition writer's own refusal exits without a word.

require src/lib/string.f
require src/lib/source-syntax.f
require src/core/checker.f
require src/core/check-hook.f
require src/core/does-clause.f
require src/compiler/fixed-body.f

package INTERPRETER

private

\ The capture reads locals before syntax. These cells describe lexical
\ lifetime only.
LOC-RECS TYPED-BUFFER BODY-LOCAL-DEPTH n
variable BODY-DEPTH
\ The raw capture: 0 none, -1 one raw token (the operand of `[char]` or
\ `[']`), and depth+1 inside a `[type]` span at that type-nesting depth.
variable BODY-RAW-NEXT
variable TOK-BODY-DEPTH
variable TOK-BODY-RAW

: BODY-LEX-RESET ( -- )
   0 BODY-DEPTH !  0 BODY-RAW-NEXT ! ;

76 constant DEF-RC-NO-COMPILER   \ the code of the engine head's first refusal, a pass-2 nesting
76 constant DEF-RC-BAD-SIG       \ `trusted:` or `does>` with no signature
72 constant DEF-RC-PD-FULL       \ no pending slot holds the defer

\ trust-sig! has no INTERPRETER row: it is the `trusted:`, `cast:`, `linear:`
\ and `defer` heads' alone.
TRUSTED: DEF-TRUST-SIG ( ptr u8 n -- ) trust-sig! ;

\ The checker's postponed rows (`postpone`) have no checked row.
TRUSTED: DEF-POSTPONED-CLEAR ( -- ) CHECKER-POSTPONE:CLEAR ;
TRUSTED: DEF-POSTPONED-CUT ( n -- ) CHECKER-POSTPONE:CUT ;
TRUSTED: DEF-POSTPONED-ADD ( n n -- ) CHECKER-POSTPONE:ADD ;

\ The block BODY-ITEMS describes. CAP-CELL is 0 outside a definition, so each
\ definition's first item reserves the block again, as it must after a capture
\ released it.
DYNAMIC-BUFFER ITEM-CELLS n

: ITEMS-RESET ( -- )
   0 BODY-ITEMS:BASE-CELL CELL!  0 BODY-ITEMS:CAP-CELL CELL!
   0 BODY-ITEMS:COUNT-CELL CELL!  0 BODY-ITEMS:RAW-CELL CELL! ;

: ITEM@ ( n n -- n ) {: i:n f:n :}
   i BODY-ITEMS:ROW * f + ITEM-CELLS @ ;

: ITEM! ( n n n -- ) {: v:n i:n f:n :}
   v i BODY-ITEMS:ROW * f + ITEM-CELLS ! ;

\ Drops the rows of runs at or past offset at, as a rolled-back token's
\ capture drops its text.
: ITEMS-CUT ( n -- ) {: at:n :}
   BODY-ITEMS:COUNT-CELL CELL@
   begin
      dup 0 > if dup 1 - BODY-ITEMS:OFF ITEM@ at >= else false then
   while 1 - repeat
   BODY-ITEMS:COUNT-CELL CELL! ;

public
\ Ending or abandoning a definition also ends the capture's lexical state.
\ x86-64's def-close leaves the locals count, so `;` clears it here.
: DEF-CLOSE ( -- )
   def-close  0 LOCN-CELL CELL!  BODY-LEX-RESET  DEF-POSTPONED-CLEAR  ITEMS-RESET ;
: DEF-ABORT ( -- ) def-abort BODY-LEX-RESET  DEF-POSTPONED-CLEAR  ITEMS-RESET ;
private

\ The engine's cells hold the compiler entries, the body hook and the checker's
\ declaration operations as raw execution tokens.
CAST: AS-COMPILE ( n -- [ ptr u8 n -- ] )
CAST: AS-COMPILE-FIXED ( n -- [ n n COMP:body -- ] )
CAST: AS-BODY-HOOK ( n -- [ -- ptr u8 bool ] )
CAST: AS-DECLARE ( n -- [ ptr u8 n ptr u8 n -- ] )

\ A head with no compiler (COMP-CELL 0), or a body token or fixed body whose
\ latch is 0, refuses. `immediate` passes.
: DEF-NO-COMPILER ( -- )
   s" E-NO-COMPILER" REFUSAL
   s" the definition has no compiler: " ERROR-TEXT:TYPE
   DEF-RC-NO-COMPILER PKG-FAIL ;

\ The capture's first token, the definition's name, which a head seeds.
: DEF-CAPTURED-NAME ( -- ptr u8 n )
   BODY-TEXT:BASE-CELL ADDR@ BODYLEN-CELL CELL@ {: b:ptr len:n :}
   0 begin
      dup len < if b over + c@ dup BLANK <> swap 0<> and else false then
   while 1+ repeat
   {: u:n :}
   b u ;

\ The checker declaration xt given takes the name as the capture spells it,
\ qualified, and the signature given.
: DEF-DECLARE ( ptr u8 n n -- )
   {: sig:ptr len:n xt:n :}
   DEF-CAPTURED-NAME sig len xt AS-DECLARE execute ;

: DEF-ENDED ( -- )
   PENDTKA-CELL CELL@ INP-CELL CELL!
   s" E-VS-UNTERMINATED-DEFINITION" REFUSAL
   s" source ended inside definition: " ERROR-TEXT:TYPE
   DEF-CAPTURED-NAME ERROR-TEXT:TYPE
   NL 1 ERROR-TEXT:TYPE  RC-UNCLOSED throw ;

\ body-capture grows the block when the text would pass its capacity, so an
\ address into the text is formed after the capture, never kept across it.
: DEF-CAPTURE ( ptr u8 n -- ) body-capture ;

: ITEM-ROOM ( -- )
   BODY-ITEMS:CAP-CELL CELL@ {: cap:n :}
   BODY-ITEMS:COUNT-CELL CELL@ cap < if exit then
   cap 0= if BODY-ITEMS:FIRST-CAP else cap 2 * then {: grown:n :}
   grown BODY-ITEMS:ROW * ITEM-CELLS-RESERVE
   grown BODY-ITEMS:CAP-CELL CELL!
   0 ITEM-CELLS NULL-PTR - BODY-ITEMS:BASE-CELL CELL! ;

\ Where the next item's run starts when every run so far was an item's.
: ITEMS-END ( -- n )
   BODY-ITEMS:COUNT-CELL CELL@ {: i:n :}
   i 0= if 0 exit then
   i 1 - BODY-ITEMS:OFF ITEM@  i 1 - BODY-ITEMS:LEN ITEM@ + 1 + ;

\ Appends an item and captures its spelling, which the row's off and len span;
\ a run captured between items is text no item spans.
: ITEM+ ( n n n ptr u8 n -- ) {: kind:n a:n b:n s:ptr u:n :}
   BODYLEN-CELL CELL@ {: off:n :}
   off ITEMS-END <> if -1 BODY-ITEMS:RAW-CELL CELL! then
   ITEM-ROOM
   s u DEF-CAPTURE
   BODY-ITEMS:COUNT-CELL CELL@ {: i:n :}
   kind i BODY-ITEMS:KIND ITEM!  a i BODY-ITEMS:A ITEM!  b i BODY-ITEMS:B ITEM!
   off i BODY-ITEMS:OFF ITEM!  u i BODY-ITEMS:LEN ITEM!
   i 1 + BODY-ITEMS:COUNT-CELL CELL! ;

\ Both comparisons are signed, as the engine's head makes them.
: DEF-CODE-ROOM ( -- )
   cp@ dbase@ REGION + PKG-CODE-RESERVE - >= if
      s" E-CODE-SPACE-FULL" REFUSAL
      s" code space full at: " ERROR-TEXT:TYPE PKG-RC-CODE-FULL PKG-FAIL
   then ;

: DEF-ROOM ( -- )
   DEF-CODE-ROOM
   ndict@ DICT-CAP >= if
      s" E-DICTIONARY-FULL" REFUSAL
      s" dictionary full at: " ERROR-TEXT:TYPE PKG-RC-DICT-FULL PKG-FAIL
   then ;

\ With the input at its end `:` and `kernel:` refuse with the engine's colon
\ text, `trusted:` as the reader keyword it is.
: DEF-NAME ( bool -- ) {: trusted:bool :}
   trusted if s" trusted:" OPERAND exit then
   TOKEN if exit then
   s" E-MISSING-NAME" REFUSAL
   s" missing definition name after " ERROR-TEXT:TYPE
   TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-NO-NAME throw ;

\ A head with no row makes a top-level package with a public wordlist
\ and no private one.
: DEF-NAMESPACE ( ptr u8 n -- n bool ) {: a:ptr q:n :}
   a q CHECKER-RESOLVE:HEAD-ROW {: row:ptr :}
   row XREF-FOUND? if row XREF-PKG-PUBLIC  row XREF:SEALED? exit then
   PKG-DICT-ROOM
   a q PKG-CODE-ROOM
   a q false namespace-record XREF-REC XREF-PKG-PUBLIC false ;

\ A wordlist of a sealed package refuses the name token. Checked as source names
\ a definition, not in DEF-RECORD, which a file object's link replays.
: DEF-SEAL-GUARD ( n ptr u8 n -- n ) {: wid:n a:ptr u:n :}
   wid PKG-WID-SEALED? if a u SEALED-FAIL then
   wid ;

\ The tail the record is named and the wordlist it goes into. DEF-TKA and
\ DEF-TKL take the name token, which the refusals after them name. A bare name
\ goes into the current wordlist; the qualifier is the first colon at neither
\ edge (`:a:b` is bare), and a second colon after it refuses.
: DEF-QUALIFY-NAME ( ptr u8 n -- ptr u8 n n ) {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 1 <  q 1+ u >=  or if a u get-current a u DEF-SEAL-GUARD exit then
   a u q 1+ COLON-AT 0 >= if
      s" E-BAD-QUALIFIED-RECORD" REFUSAL PKG-RC-CONTEXT PKG-FAIL
   then
   a q 1+ ZPTR+  u q - 1-  a q DEF-NAMESPACE  if a u SEALED-FAIL then ;

: DEF-QUALIFY ( -- ptr u8 n n )
   OVERLAY-GUARD
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   TOKEN$ DEF-QUALIFY-NAME ;

\ The keywords a body's compiler reads before it looks a word up: a word named
\ by one could never be called from a body.
: DEF-KEYWORDS ( -- ptr u8 n )
   S\" if then else begin until again while repeat case of endof endcase construct match s\q c\q .\q s\\\q c\\\q .\\\q ['] [char] [package] [type] does> [: is ;] do loop i >r r> r@ exit recurse ?do +loop j leave unloop {: postpone" ;

\ A design may define none of the engine's dispatch-row names. The admitted
\ syntax rows other than S\" keep the ordinary reserved-name error; every other
\ row is outside the vocabulary before dictionary duplicate checks run.
: DEF-DESIGN-KEYWORDS ( -- ptr u8 n )
   S\" if else then {: :} s\q [package] package public private ;package using ;using" ;

\ The interpret dispatch rows, plus kernel: and ;match. Ordinary words such as
\ require and include may be published by a package even though a definition
\ cannot take their names at this checked head boundary.
: DEF-INTERPRET-KEYWORDS ( -- ptr u8 n )
   S\" kernel: trusted: cast: linear: immediate create variable constant defer export seal-package char ' ;match" ;

: DEF-NAME-ONLY-KEYWORDS ( -- ptr u8 n )
   s" : 2constant to require include sumtype variant ;variant ;sumtype" ;

: DEF-OP-KEYWORDS ( -- ptr u8 n )
   s" + - * / mod and or xor lshift rshift dup drop swap over nip = <> < > <= >= 1+ 1- 0= 0< negate invert f+ f- f* f/" ;

: DEF-P2-KEYWORDS ( -- ptr u8 n )
   s" tuck rot -rot 2dup 2drop 2swap 2over 2>r 2r> 2r@ @ !" ;

: DEF-KEYWORD-END ( ptr u8 n n -- n ) {: t:ptr tu:n s:n :}
   s begin dup tu < if t over + c@ BLANK <> else false then while 1+ repeat ;

: DEF-IN-TABLE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n t:ptr tu:n :}
   0 begin dup tu < while
      {: s:n :}
      t tu s DEF-KEYWORD-END {: e:n :}
      a u  t s ZPTR+ e s -  FOLDED= if true exit then
      e 1+
   repeat
   drop false ;

: DEF-KEYWORD? ( ptr u8 n -- bool )
   DEF-KEYWORDS DEF-IN-TABLE? ;

: DEF-DESIGN-KEYWORD? ( ptr u8 n -- bool )
   DEF-DESIGN-KEYWORDS DEF-IN-TABLE? ;

: DEF-ROW-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u DEF-KEYWORD? if true exit then
   a u DEF-INTERPRET-KEYWORDS DEF-IN-TABLE? if true exit then
   a u DEF-OP-KEYWORDS DEF-IN-TABLE? if true exit then
   a u DEF-P2-KEYWORDS DEF-IN-TABLE? ;

: DEF-NAME-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u DEF-ROW-KEYWORD? if true exit then
   a u DEF-NAME-ONLY-KEYWORDS DEF-IN-TABLE? ;

\ Every engine's policy writer asks this captured interpreter once per package,
\ before setting its admission bit.
public
: POLICY-KEYWORD ( n -- ptr u8 n bool ) {: wid:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST wid = if
         rec XREF-NAME$ {: a:ptr u:n :}
         a u DEF-DESIGN-KEYWORD? a u DEF-ROW-KEYWORD? or if
            a u true unloop exit
         then
      then
   loop
   s" " false ;

: INSTALL-POLICY ( -- )
   ['] POLICY-KEYWORD data-base POLICY-ABI:KEYWORD-CELL + xt! ;
private

: DEF-KEYWORD-REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" E-RESERVED-DEFINITION" REFUSAL
   s" compile keyword cannot be a definition name: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-REJECT throw ;

: DEF-POLICY-REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" E-POLICY" REFUSAL
   s" not in vocabulary: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  ENGINE-ERROR:POLICY throw ;

: DEF-NUMBER-REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" E-NUMERIC-DEFINITION" REFUSAL
   s" number cannot be a definition name: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-REJECT throw ;

: DEF-WALL ( ptr u8 n -- ) {: a:ptr u:n :}
   POLICY-NDICT-CELL CELL@ 0<> if
      a u DEF-DESIGN-KEYWORD? if a u DEF-KEYWORD-REFUSE then
      a u DEF-NAME-KEYWORD? if a u DEF-POLICY-REFUSE then
   else
      a u DEF-KEYWORD? if a u DEF-KEYWORD-REFUSE then
   then
   a u NUMBER {: v:n flt:bool num:bool range:bool :}
   num range or if a u DEF-NUMBER-REFUSE then ;

\ A signature starts at the first byte past the blanks when that byte is `(`,
\ whole token or not: where it starts, and whether one does.
: DEF-SIG-START ( -- ptr u8 bool )
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:ptr e:ptr :}
   p e BLANKS-END {: s:ptr :}
   s e < if s c@ OPEN-COMMENT = else false then {: open:bool :}
   s open ;

: DEF-SIG-END ( ptr u8 -- ptr u8 bool ) {: s:ptr :}
   INE-CELL ADDR@ {: e:ptr :}
   s begin dup e < if dup c@ CLOSE-COMMENT <> else false then while 1 + repeat
   {: c:ptr :}
   c e < if c 1 + true exit then
   e false ;

: DEF-SIG-PASS ( ptr u8 ptr u8 -- ) {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s end s - DEF-CAPTURE ;

\ trust-sig! takes the text inside the parentheses. The inner length is the
\ engine's, two less than the whole: one less than the text for a signature
\ the input ends inside.
: DEF-SIG-INNER ( ptr u8 ptr u8 -- ) {: s:ptr end:ptr :}
   s 1 + end s - 2 - DEF-TRUST-SIG ;

: DEF-SIG-TAKE ( ptr u8 ptr u8 -- ) {: s:ptr end:ptr :}
   s end DEF-SIG-INNER
   s end DEF-SIG-PASS ;

\ `:` takes one if it is there, to the input's end if it is not closed.
: DEF-MAYBE-SIG ( -- )
   DEF-SIG-START {: s:ptr open:bool :}
   open 0= if exit then
   s DEF-SIG-END drop {: end:ptr :}
   s end DEF-SIG-TAKE ;

: DEF-SIG-BAD ( -- )
   s" E-MISSING-SIGNATURE" REFUSAL
   s" '" ERROR-TEXT:TYPE  TOKEN$ ERROR-TEXT:TYPE
   s" ' has no closed ( -- ) signature" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   DEF-RC-BAD-SIG throw ;

\ A head's signature that the input ends before or inside is the source ending
\ inside the definition.
: DEF-SIG-SPAN ( bool -- ptr u8 ptr u8 ) {: head:bool :}
   DEF-SIG-START {: s:ptr open:bool :}
   head s INE-CELL ADDR@ = and if DEF-ENDED then
   open 0= if DEF-SIG-BAD then
   s DEF-SIG-END {: end:ptr closed:bool :}
   closed 0= if head if DEF-ENDED then DEF-SIG-BAD then
   s end ;

: DEF-REQUIRED-SIG ( -- )
   true DEF-SIG-SPAN DEF-SIG-TAKE ;

: DEF-COMPILER ( -- n )
   NCOMP-DISPATCH:COMP-LATCH-CELL CELL@ 0<> if
      NCOMP-DISPATCH:COMP-CELL CELL@ exit
   then
   0 ;

\ With no compiler in the latched cell the boot is broken, and the process
\ ends.
: DEF-DISPATCH ( -- )
   DEF-COMPILER 0<> if exit then
   s" E-AOT-SEED" REFUSAL
   s" native compiler dispatch unset" ERROR-TEXT:TYPE
   s" " ENGINE-ERROR:AOT-SEED FAIL-CLOSED ;

public
\ The tail passes the wall, room, duplicate, code room and protected-wordlist
\ tests before def-open writes; the wall, code room and protected-wordlist
\ test name the tail, the rest the token. kind: 0 (compiled), DKIND:ADDR, VAL
\ or CAST. def-open refuses a protected wordlist too, but without a word.
: DEF-RECORD ( ptr u8 n n n -- ) {: a:ptr u:n wid:n kind:n :}
   a u DEF-WALL
   PKG-DICT-ROOM
   a u wid WL-PROBE XREF-FOUND? if
      TOKEN$ PKG-DUPLICATE
   then
   a u PKG-CODE-ROOM
   SEAL-NDICT@ 0<>  wid XREF-WID-PROTECTED?  and if a u SEALED-FAIL then
   kind DKIND:MASK and {: dk:n :}
   \ A value or an address in a target section stays unmarked: it answers on the host.
   SECTION-CELL CELL@ 0<>  dk DKIND:ADDR <> and  dk DKIND:VAL <> and
   if kind DNAME-TARGET or else kind then {: k:n :}
   a u wid k def-open ;
private

\ The capture seeded with the name, the spelling the checker is told.
: DEF-SEED ( -- )
   ITEMS-RESET
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE ;

: DEF-START ( n -- )
   {: kind:n :}
   DEF-SEED
   DEF-QUALIFY kind DEF-RECORD ;

\ On true the body hook has installed the definition, the source resumes past
\ its `;` and the record closes.
: DEF-BODY-HOOK? ( -- bool )
   NCOMP-DISPATCH:BODY-HOOK-CELL CELL@ {: xt:n :}
   xt 0= if false exit then
   xt AS-BODY-HOOK execute {: resume:ptr installed:bool :}
   installed 0= if false exit then
   resume INP-CELL ADDR!
   DEF-CLOSE
   true ;

: DEF-HEAD ( bool -- ) {: trusted:bool :}
   NCOMP-DISPATCH:COMP-CELL CELL@ 0= if
      DEF-NO-COMPILER
   then
   DEF-ROOM
   trusted DEF-NAME
   0 DEF-START
   0 LOCN-CELL CELL!
   BODY-LEX-RESET
   trusted if
      1 TRUSTED-CELL CELL!
      DEF-REQUIRED-SIG
   else
      DEF-MAYBE-SIG
   then
   DEF-BODY-HOOK? if exit then
   DEF-DISPATCH ;

\ After a string keyword the body takes the literal's text as written, from one
\ byte past the keyword through its closing quote, in one capture. The scan
\ comes first, so a literal with no closing quote or with a bad escape refuses
\ as the top-level keyword does, before any of it is captured. A counted
\ string's length waits for `;`, which compiles the body.
: DEF-TEXT ( -- )
   TEXT {: a:ptr u:n :}
   a u 1+ DEF-CAPTURE ;

: DEF-ESC-TEXT ( -- )
   ESC-SCAN drop {: s:ptr q:ptr :}
   q PAST
   s q s - 1+ DEF-CAPTURE ;

: DEF-STRING-TEXT ( -- )
   S\" s\q" TOKEN-IS? if DEF-TEXT exit then
   S\" c\q" TOKEN-IS? if DEF-TEXT exit then
   S\" .\q" TOKEN-IS? if DEF-TEXT exit then
   S\" s\\\q" TOKEN-IS? if DEF-ESC-TEXT exit then
   S\" c\\\q" TOKEN-IS? if DEF-ESC-TEXT exit then
   S\" .\\\q" TOKEN-IS? if DEF-ESC-TEXT then ;

\ The capture records each `{:` declaration here; the compiler reads it from the
\ body text at `;`.
: BODY-LOCAL? ( -- bool )
   TOKEN$ {: a:ptr u:n :}
   LOCN-CELL CELL@ 0 ?do
      data-base LOCNAMES + i LOC-REC * + BYTE-VIEW {: row:ptr :}
      row CELL-VIEW @ u = if
         a u row 8 + u STR= if true unloop exit then
      then
   loop
   false ;

: BODY-LOCAL-ONE ( -- )
   TOKEN$ {: a:ptr u:n :}
   LOCN-CELL CELL@ {: idx:n :}
   idx LOC-RECS >= if
      s" E-TOO-MANY-LOCALS" REFUSAL
      s" more than 64 locals in one definition: " ERROR-TEXT:TYPE
      TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-REJECT throw
   then
   a u 0 COLON-AT {: colon:n :}
   colon 0 < if u else colon then {: len:n :}
   len LOC-NAME-CAP > if
      s" E-LOCAL-NAME-TOO-LONG" REFUSAL
      s" local name over 16 bytes: " ERROR-TEXT:TYPE
      TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-REJECT throw
   then
   data-base LOCNAMES + idx LOC-REC * + BYTE-VIEW {: row:ptr :}
   len row CELL-VIEW !
   a row 8 + len BYTE-COPY
   BODY-DEPTH @ idx BODY-LOCAL-DEPTH !
   idx 1+ LOCN-CELL CELL! ;

: BODY-LOCAL-GROUP ( -- )
   begin TOKEN while
      TOKEN$ DEF-CAPTURE
      s" :}" TOKEN-IS? if exit then
      BODY-LOCAL-ONE
   repeat ;

\ A later rejected token can restore the count without reconstructing any row.
: BODY-BLOCK-DROP ( -- )
   begin
      LOCN-CELL CELL@ 0 > if
         LOCN-CELL CELL@ 1- BODY-LOCAL-DEPTH @ BODY-DEPTH @ >=
      else false then
   while
      LOCN-CELL CELL@ 1- LOCN-CELL CELL!
   repeat ;

: BODY-BLOCK-STEP ( -- )
   LOCN-CELL CELL@ 0= if exit then
   TOKEN$ SOURCE-SYNTAX:BLOCK-OPENER? if
      BODY-DEPTH @ 1+ BODY-DEPTH ! exit
   then
   s" else" TOKEN-IS? if BODY-BLOCK-DROP exit then
   TOKEN$ SOURCE-SYNTAX:BLOCK-CLOSER? if
      BODY-BLOCK-DROP  BODY-DEPTH @ 1- BODY-DEPTH !
   then ;

\ The body vocabulary the capture admits.
\ `;` is handled before the capture.
: BODY-POLICY ( -- )
   BODY-LOCAL? if exit then
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   range if UNDEFINED then
   POLICY-NDICT-CELL CELL@ 0= if exit then
   num if exit then
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? if rec POLICY-CHECK-REC exit then
   TOKEN$ FIND {: used:ptr :}
   used XREF-FOUND? if used POLICY-CHECK-REC exit then
   s" if" TOKEN-IS? if exit then
   s" else" TOKEN-IS? if exit then
   s" then" TOKEN-IS? if exit then
   S\" s\q" TOKEN-IS? if exit then
   S\" s\\\q" TOKEN-IS? if exit then
   s" [package]" TOKEN-IS? if exit then
   s" [type]" TOKEN-IS? if exit then
   s" {:" TOKEN-IS? if exit then
   POLICY-REFUSE ;

: DEF-PREFLIGHT ( -- )
   BODY-TEXT:BASE-CELL ADDR@ BODYLEN-CELL CELL@ TOKEN$ TRUSTED-CELL CELL@ 0<>
   CHECK:IMMEDIATE ;

\ NEUTRAL-PARSE-IMM? asks the live checker: the retained one lacks a build window's
\ `parse-imm` rows.
: DEF-IMMEDIATE ( -- ptr n bool )
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if NULL-PTR false exit then
   rec XREF-FLAGS DNAME-IMM and 0= if NULL-PTR false exit then
   rec POLICY-CHECK-REC
   rec TOKEN$ NEUTRAL-PARSE-IMM?
   dup 0= s" does>" TOKEN-IS? 0= and if DEF-PREFLIGHT then ;

\ The occurrence is selected before the preflight, so a preflight that retires
\ it cannot substitute a reused dictionary slot.
: DEF-RUN ( ptr n -- )
   DEF-OCC:SELECT {: slot:n occurrence:n :}
   DEF-PREFLIGHT
   slot occurrence DEF-OCC:RESOLVE {: rec:ptr :}
   rec XREF-NULL - XREF-ADDR>INDEX RUN+
   rec XREF-START
   execute-floor FLOORED ;

: DEF-IMMEDIATE? ( -- bool )
   DEF-IMMEDIATE if DEF-RUN true exit then
   drop false ;

\ `does>` ends a defining word's own part: DOESB takes the capture's length
\ with `does>` in it, where `;` splits the body, and the signature after it,
\ which must be there, opened and closed, is the effect of each word the
\ definer creates. Its copy, `( `, the inside and ` )`, opens the clause in the
\ capture, where created-sig! takes the inside and a clause's scan reads a
\ comment.
: DEF-CREATED ( -- )
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   end s - 2 - {: len:n :}
   BODYLEN-CELL CELL@ 2 + {: at:n :}
   s 1 DEF-CAPTURE
   s 1 + len DEF-CAPTURE
   end 1 - 1 DEF-CAPTURE
   BODY-TEXT:BASE-CELL ADDR@ at + {: copy:ptr :}
   copy len created-sig! ;

\ The pending parent is record NDICT. Its clause will be published under the
\ parent's name plus ;does in the same wordlist. Check the live records before
\ the signature copy or split changes any state, including a folded-case hit.
: DEF-DOES-NAME ( -- )
   ndict@ XREF-REC {: parent:ptr :}
   parent XREF-NAME$ {: stem:ptr stemu:n :}
   parent XREF-WORDLIST {: wid:n :}
   DOES-CLAUSE:SUFFIX$ {: suffix:ptr sufu:n :}
   ndict@ 0 ?do
      i XREF-REC {: row:ptr :}
      row XREF-WORDLIST wid = if
         row XREF-NAME$ {: name:ptr size:n :}
         size stemu sufu + = if
            name stemu stem stemu STR=CI
            name stemu + sufu suffix sufu STR=CI and if
               s" E-DUPLICATE-DEFINITION" REFUSAL
               s" duplicate definition: " ERROR-TEXT:TYPE
               stem stemu ERROR-TEXT:TYPE suffix sufu ERROR-TEXT:TYPE
               NL 1 ERROR-TEXT:TYPE  PKG-RC-DUPLICATE throw
            then
         then
      then
   loop ;

: DEF-DOES ( -- )
   DOESB-CELL CELL@ 0<> if
      s" E-DOES-TWICE" REFUSAL
      s" does>" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-REJECT throw
   then
   DEF-DOES-NAME
   BODYLEN-CELL CELL@ {: at:n :}
   DEF-CREATED
   at DOESB-CELL CELL!
   1 CLAUSE-CELL CELL!
   \ The head's locals end here: the clause cannot name one, and a local it
   \ declares is captured from an empty scope.
   0 LOCN-CELL CELL!  0 BODY-DEPTH ! ;

\ The engine reads `does>` as its keyword, with the token's A-Z folded, unless
\ the scope holds a word of that spelling that is not immediate, which the body
\ calls instead.
: DEF-CALL? ( -- bool )
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if false exit then
   rec XREF-FLAGS DNAME-IMM and 0= ;

: DEF-DOES? ( -- bool )
   s" does>" TOKEN-IS? 0= if false exit then
   DEF-CALL? if false exit then
   DEF-DOES true ;

\ The record pending as EVAL-FRAMED-RANGE's buffer began: another pending
\ record is the buffer's own, and a throw out of the buffer puts this one back
\ in PEND-CELL.
variable ENTRY-PEND

: DEF-SOURCE-CLOSE ( -- )
   PEND-CELL CELL@ ENTRY-PEND @ <> if exit then
   s" E-VS-UNTERMINATED-DEFINITION" REFUSAL
   s" source closed a definition it did not open: " ERROR-TEXT:TYPE
   DEF-CAPTURED-NAME ERROR-TEXT:TYPE
   NL 1 ERROR-TEXT:TYPE  RC-UNCLOSED throw ;

: DEF-SOURCE-END ( -- )
   PEND-CELL CELL@ ENTRY-PEND @ = if exit then
   DEF-ENDED ;

\ An evaluate begun inside this native definition owns only its new tokens.
\ The checker reads the body prefix for binding, leaving the definition's
\ effect, open scopes and publication to its eventual `;`. A refusal lands in
\ evaluate's existing frame, whose token mark removes only the failed token.
TRUSTED: DEF-PREFIX-BIND ( -- )
   ENTRY-PEND @ 0= if exit then
   BODY-TEXT:BASE-CELL ADDR@
   BODYLEN-CELL CELL@ TOKBODY-CELL CELL@
   CHECKER-PREFIX:BIND? {: rollback:n ok:bool :}
   ok 0= if
      \ A parser operand can roll back its opener as well as the operand.
      \ The token mark must then restore the state before that opener.
      rollback TOKBODY-CELL CELL@ < if 0 TOK-BODY-RAW ! then
      rollback TOKBODY-CELL CELL! UNDEFINED
   then ;

\ The latch is checked again here since a body immediate can have cleared it.
\ The end recovers: it declares so in RECOVER-CELL for the compile, and
\ abandons the definition the compiler refused with E-COMP-RECOVERY, which
\ only a multi-error run throws, and reads on.
: DEF-COMPILE ( ptr u8 n -- )
   DEF-DISPATCH DEF-COMPILER AS-COMPILE execute ;

: DEF-END ( -- )
   DEF-SOURCE-CLOSE
   NCOMP-DISPATCH:RECOVER-CELL CELL@ {: prior:n :}
   -1 NCOMP-DISPATCH:RECOVER-CELL CELL!
   [: BODY-TEXT:BASE-CELL ADDR@ BODYLEN-CELL CELL@ DEF-COMPILE ;] catch
   {: rc:n :}
   prior NCOMP-DISPATCH:RECOVER-CELL CELL!
   rc E-COMP-RECOVERY = if DEF-ABORT exit then
   rc 0<> if rc throw then
   DEF-CLOSE ;

\ `;`, the one byte, ends the definition and joins no capture.
: DEF-SEMI? ( -- bool )
   s" ;" TOKEN-IS? 0= if false exit then
   DEF-END true ;

\ ---- definitions built by a running word -------------------------------------
\ `define-word` opens a definition from strings, `body-append` adds the
\ caller's tokens to its capture, `literal` a number and `postpone W` W bound
\ to the record the handle names; `postpone ;` ends it as its source `;`
\ would. All but body-append add items (BODY-ITEMS). The source cursor stays
\ at the running word's call, where a refusal locates.

\ WHO names the operation refused: `define-word`, or a declaration's child.
: DEF-NAME-REFUSE ( ptr u8 n ptr u8 n -- ) {: w:ptr wu:n a:ptr u:n :}
   s" E-MISSING-NAME" REFUSAL
   w wu ERROR-TEXT:TYPE
   s"  needs one token as the name: '" ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE  s" '" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   RC-NO-NAME throw ;

: DEF-OPEN-REFUSE ( ptr u8 n ptr u8 n -- ) {: w:ptr wu:n a:ptr u:n :}
   s" E-DEFINITION-OPEN" REFUSAL
   w wu ERROR-TEXT:TYPE  s"  while a definition is open: " ERROR-TEXT:TYPE
   a u ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-REJECT throw ;

: DEF-GIVEN-SIG-BAD ( ptr u8 n -- ) {: a:ptr u:n :}
   s" E-MISSING-SIGNATURE" REFUSAL
   s" '" ERROR-TEXT:TYPE  a u ERROR-TEXT:TYPE
   s" ' has no closed ( -- ) signature" ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE
   DEF-RC-BAD-SIG throw ;

: DEF-ONE-TOKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   a a u + WORD-END a u + = ;

\ The signature as source writes it, `( n -- n )`, with blanks at most around
\ it: the span from its `(` through its `)`.
: DEF-GIVEN-SIG ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n s:ptr su:n :}
   s su + {: e:ptr :}
   s e BLANKS-END {: p:ptr :}
   p e < if p c@ OPEN-COMMENT = else false then 0= if a u DEF-GIVEN-SIG-BAD then
   p begin dup e < if dup c@ CLOSE-COMMENT <> else false then while 1 + repeat
   {: c:ptr :}
   c e < 0= if a u DEF-GIVEN-SIG-BAD then
   c 1 + e BLANKS-END e <> if a u DEF-GIVEN-SIG-BAD then
   p c 1 + p - ;

\ TSIG is read for a trusted head alone, so the signature lives only in the
\ capture.
: DEFINE-RUN ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n s:ptr su:n :}
   PEND-CELL CELL@ 0<> if s" define-word" a u DEF-OPEN-REFUSE then
   a u DEF-ONE-TOKEN? 0= if s" define-word" a u DEF-NAME-REFUSE then
   a u s su DEF-GIVEN-SIG {: p:ptr pu:n :}
   NCOMP-DISPATCH:COMP-CELL CELL@ 0= if
      DEF-NO-COMPILER
   then
   DEF-ROOM
   OVERLAY-GUARD
   a u DEF-QUALIFY-NAME {: ta:ptr tu:n wid:n :}
   ta tu wid WL-PROBE XREF-FOUND? if a u PKG-DUPLICATE then
   ITEMS-RESET
   0 BODYLEN-CELL CELL!  BODY-ITEM-KIND:NAME 0 0 a u ITEM+
   \ A refused record opens no definition, whose end would drop the item.
   [: ta tu wid 0 DEF-RECORD ;] catch {: rc:n :}
   rc 0<> if ITEMS-RESET rc throw then
   0 LOCN-CELL CELL!  BODY-LEX-RESET
   BODY-ITEM-KIND:SIG 0 0 p pu ITEM+
   DEF-DISPATCH ;

: HANDLE-SPLIT ( ptr u8 n -- ptr u8 n ptr u8 n ) {: a:ptr u:n :}
   a u + {: e:ptr :}
   a e WORD-END {: w:ptr :}
   w e = if a u e 0 exit then
   a w a -  w 1 + e w 1 + - ;

: HANDLE-PROBE ( ptr u8 n n -- ptr n ) {: a:ptr u:n wid:n :}
   a u wid WL-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if a u UNDEFINED-NAME then
   rec ;

\ The record a handle names in this dictionary: `global TAIL`, or `public` or
\ `private` then the package's path and TAIL.
: HANDLE-REC ( ptr u8 n -- ptr n ) {: h:ptr hu:n :}
   h hu HANDLE-SPLIT {: k:ptr ku:n r:ptr ru:n :}
   k ku s" global" STR= if r ru BOOT:GLOBAL-WL HANDLE-PROBE exit then
   r ru HANDLE-SPLIT {: pa:ptr pu:n t:ptr tu:n :}
   pa pu BOOT:NAMESPACE-WL WL-PROBE {: row:ptr :}
   row XREF-FOUND? 0= if pa pu UNDEFINED-NAME then
   k ku s" public" STR= if t tu row XREF-PKG-PUBLIC HANDLE-PROBE exit then
   t tu row XREF-PKG-PRIVATE HANDLE-PROBE ;

: BUILT-PENDING ( ptr u8 n -- ) {: a:ptr u:n :}
   PEND-CELL CELL@ 0<> if exit then
   s" E-NO-DEFINITION" REFUSAL
   a u ERROR-TEXT:TYPE  s"  with no definition open" ERROR-TEXT:TYPE
   NL 1 ERROR-TEXT:TYPE  RC-REJECT throw ;

: APPEND-RUN ( ptr u8 n -- ) {: a:ptr u:n :}
   s" body-append" BUILT-PENDING
   -1 BODY-ITEMS:RAW-CELL CELL!
   a u DEF-CAPTURE ;

\ What `postponed` runs: `end` ends the definition, abandoning it if the end
\ throws; any other handle appends its record's name, which the checker binds.
: POSTPONE-RUN ( ptr u8 n -- ) {: h:ptr hu:n :}
   s" postpone" BUILT-PENDING
   h hu s" end" STR= if
      [: DEF-END ;] catch {: rc:n :}
      rc 0<> if DEF-ABORT rc throw then
      exit
   then
   h hu HANDLE-REC {: rec:ptr :}
   BODYLEN-CELL CELL@ {: at:n :}
   rec XREF-NULL - XREF-ADDR>INDEX {: ix:n :}
   BODY-ITEM-KIND:CALL ix 0 rec XREF-NAME$ ITEM+
   at ix DEF-POSTPONED-ADD ;

\ A cell's decimal spelling, written from the end of LIT-DIGITS: 20 bytes
\ hold any i64 and its sign. `/` truncates, so a negative value's remainders
\ are at most 0.
20 BUFFER: LIT-DIGITS
variable LIT-AT

: LIT-DIGIT ( n -- ) {: c:n :}
   LIT-AT @ 1 - LIT-AT !
   c LIT-DIGITS LIT-AT @ + c! ;

: LIT-REM ( n -- n ) {: v:n :}
   v v 10 / 10 * - {: r:n :}
   r 0 < if 0 r - else r then ;

: LIT$ ( n -- ptr u8 n ) {: v:n :}
   20 LIT-AT !
   v begin dup LIT-REM 48 + LIT-DIGIT  10 / dup 0= until drop
   v 0 < if 45 LIT-DIGIT then
   LIT-DIGITS LIT-AT @ +  20 LIT-AT @ - ;

\ What `literal` runs: an INT item, spelled in decimal.
: LITERAL-RUN ( n -- ) {: v:n :}
   s" literal" BUILT-PENDING
   BODY-ITEM-KIND:INT v 0 v LIT$ ITEM+ ;

: COMPILING? ( -- bool )
   PEND-CELL CELL@ 0= if false exit then
   NCOMP-DISPATCH:COMP-LATCH-CELL CELL@ 0= if DEF-NO-COMPILER then
   BODY-RAW-NEXT @ 0 > if
      TOKEN$ TYPE-SPAN-END? if 0 BODY-RAW-NEXT ! then
   then
   BODY-RAW-NEXT @ 0 > if
      BODY-RAW-NEXT @ 1- TOKEN$ CHECKER-TYPE-SPAN-STEP
      if drop 0 else 1+ then BODY-RAW-NEXT !
      TOKEN$ DEF-CAPTURE  DEF-PREFIX-BIND  true exit
   then
   BODY-RAW-NEXT @ if
      0 BODY-RAW-NEXT !
      TOKEN$ DEF-CAPTURE  DEF-PREFIX-BIND  true exit
   then
   DEF-SEMI? if true exit then
   BODY-LOCAL? if
      TOKEN$ DEF-CAPTURE  DEF-PREFIX-BIND  true exit
   then
   BODY-POLICY
   TOKEN$ DEF-CAPTURE
   s" {:" TOKEN-IS? if BODY-LOCAL-GROUP DEF-PREFIX-BIND true exit then
   BODY-BLOCK-STEP
   DEF-IMMEDIATE? if true exit then
   DEF-DOES? if true exit then
   DEF-STRING-TEXT
   TOKEN$ SOURCE-SYNTAX:BODY-PARSING-KEYWORD? if -1 BODY-RAW-NEXT ! then
   s" [type]" TOKEN-IS? if 1 BODY-RAW-NEXT ! then
   DEF-PREFIX-BIND
   true ;

\ A created word is published whole where its keyword is read.

\ The keyword joins the capture, and the checker reads it, `NAME create` for a
\ created word, its verdict dropped: a definer's word always publishes. The
\ check runs on the program's stack, so neither this word nor its callers keep
\ locals.
: DEF-HOOK ( ptr u8 n -- )
   DEF-CAPTURE
   BODY-TEXT:BASE-CELL ADDR@ BODYLEN-CELL CELL@
   CHECK:DEFINITION drop ;

: DEF-ADDR-HOOK ( -- )
   s" create" DEF-HOOK ;

: DEF-RAW-ACTIVE ( -- n )
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-RAW-OFF PKG-OPERATION {: own:n :}
   own 0<> if own exit then
   NCOMP-DISPATCH:DECL-RAW-OFF PKG-TARGET {: target:n :}
   target 0= if
      s" E-AOT-SEED" REFUSAL
      s" trust-raw dispatch unset" ENGINE-ERROR:AOT-SEED FAIL-CLOSED
   then
   target ;

\ A definer's effect, raw dictionary storage stated by sig; the
\ checker seals it.
: DEF-RAW-PUBLISH ( ptr u8 n -- ) {: sig:ptr su:n :}
   DEF-RAW-ACTIVE {: own:n :}
   sig su own DEF-DECLARE
   NCOMP-DISPATCH:DECL-CELL CELL@ 0= if exit then
   NCOMP-DISPATCH:DECL-RAW-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-RAW-OFF PKG-OPERATION target = if exit then
   sig su target DEF-DECLARE ;

: DEF-ADDR-PUBLISH ( -- )
   s" -- ptr a" DEF-RAW-PUBLISH ;

\ The DATA address a created word's body pushes, as the number NCOMP takes.
: DEF-HERE ( -- n ) here NULL-PTR - ;

\ The pending record's fixed body ( val entry kind ): entry is the routine a
\ routine body calls, 0 for every other kind.
: DEF-FIXED ( n n COMP:body -- )
   NCOMP-DISPATCH:COMP-LATCH-CELL CELL@ {: latch:n :}
   latch 0= if DEF-NO-COMPILER then
   NCOMP-DISPATCH:FIXED-COMP-CELL CELL@ AS-COMPILE-FIXED execute ;

\ The cell-aligned here a created body pushes, kept as the latest body start.
: DEF-ANCHOR ( -- n )
   align DEF-HERE dup COMMA-BASE-CELL CELL! ;

\ The fixed body pushes the anchor, and ends in the slot a `does>` patches.
: DEF-ADDR-TAIL ( -- )
   DEF-ANCHOR 0 COMP-BODY:address DEF-FIXED
   DEF-CLOSE ;

\ No record slot left is refused after the name, naming it. CP at the code
\ ceiling is refused first, naming the keyword, as `:`'s does.
: DEF-ADDR-ROW ( bool -- ) {: cell?:bool :}
   DEF-CODE-ROOM
   s" create" OPERAND
   DKIND:ADDR DNAME-ALLOC or cell? if DNAME-CELL or then DEF-START
   DEF-ADDR-TAIL ;

\ A compiled defining word reaches `create` through the engine primitive,
\ which calls the row through CREATEP-CELL. The keyword's hook belongs to the
\ interpreting `create` only; `-- ptr a` is published at both, or the word
\ made is E-UNDEFINED where it is next read.
: DEF-CREATE-ROW ( -- )
   false DEF-ADDR-ROW
   DEF-ADDR-PUBLISH ;

: DEF-ADDR ( bool -- )
   DEF-ADDR-ROW
   DEF-ADDR-HOOK
   DEF-ADDR-PUBLISH ;

\ A constant's record opens as DKIND:VAL, the stamp a mention of it folds to.
\ It takes the program's top cell after its record opens.
variable DEF-VALUE

: DEF-CONSTANT ( -- )
   s" constant" OPERAND
   DKIND:VAL DEF-START
   depth 0= if UNDERFLOW then
   [: DEF-VALUE ! ;] CELL-TAKE RUN-LITERAL
   DEF-VALUE @ 0 COMP-BODY:value DEF-FIXED
   DEF-CLOSE
   s" constant" DEF-HOOK
   s" -- a" DEF-RAW-PUBLISH ;

\ `cast:` and `linear:` have no body or `;`: each completes where its keyword
\ is read, with a compiler latched, and calls no def hook.

\ With the input at its end the refusal names the keyword as spelled, at the
\ end's line.
: DEF-CAST-NAME ( -- )
   TOKEN if exit then
   s" E-MISSING-NAME" REFUSAL
   s" cast: missing name after " ERROR-TEXT:TYPE
   TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-NO-NAME throw ;

\ The signature must be there, opened and closed, before the qualifier's
\ refusals, and INP passes it first, so those name the line it ends on.
\ def-open clears TSIG, so trust-sig! takes the signature once the record is
\ open.
: DEF-CAST-HEAD ( -- )
   DEF-SEED
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   s end DEF-SIG-PASS
   DEF-QUALIFY DKIND:CAST DEF-RECORD
   s end DEF-SIG-INNER ;

\ A row the checker refuses throws before DEF-FIXED counts the record, so no
\ callable name is left.
: DEF-REGISTER-IDENTITY ( ptr u8 n n -- ) {: sig:ptr su:n off:n :}
   NCOMP-DISPATCH:DECL-A off PKG-OPERATION {: own:n :}
   own 0<> if sig su own DEF-DECLARE then
   off PKG-TARGET {: target:n :}
   target 0= if exit then
   NCOMP-DISPATCH:DECL-A off PKG-OPERATION target = if exit then
   sig su target DEF-DECLARE ;

: DEF-REGISTER-CAST ( -- )
   TSIG-A-CELL ADDR@ TSIG-U-CELL CELL@ NCOMP-DISPATCH:DECL-CAST-OFF
   DEF-REGISTER-IDENTITY ;

\ The record takes what the checker's registrar latched: the wide mark, then
\ the minimum input arity, under which the interpret dispatch refuses a call.
: DEF-CAST-FACTS ( -- )
   REC-WIDE-PUBLISH
   REC-MIN-IN@ {: mi:n :}
   mi 0<> if ndict@ 1- mi min-in-mark then ;

\ Its name folds apart from the def-cast row: a word spelled DEF-CAST here
\ would shadow the row for every later body.
: DEF-CAST-ROW ( -- )
   DEF-ROOM
   DEF-CAST-NAME
   DEF-CAST-HEAD
   DEF-REGISTER-CAST
   0 0 COMP-BODY:cast DEF-FIXED  DEF-CLOSE
   DEF-CAST-FACTS ;

\ ---- child words a declaration makes ------------------------------------------
\ CHILD-RUN makes the word NAME for a declaration, with no text: a fixed body of
\ its CHILD-ABI shape, whose value and effect CHECK:CHILD makes from the shape,
\ parameter and stored type while the record is pending. A storage child's body
\ pushes its own data address, which the declaration fills after the return.

\ The entry of a private routine the engine's dictionary holds; a missing one
\ is refused by name.
: CHILD-ROUTINE ( ptr u8 n -- COMP:body n )
   HANDLE-REC XREF-START COMP-BODY:routine swap ;

\ An unknown shape is the calling declaration's error.
: CHILD-BODY ( n -- COMP:body n ) {: shape:n :}
   shape CHILD-ABI:CELL-OFF =
   shape CHILD-ABI:PTR-OFF = or
   shape CHILD-ABI:BYTE-OFF = or
   shape CHILD-ABI:FIELD = or if COMP-BODY:offset 0 exit then
   shape CHILD-ABI:INDEX = if COMP-BODY:index 0 exit then
   shape CHILD-ABI:VALUE = if COMP-BODY:value 0 exit then
   shape CHILD-ABI:STORED = if COMP-BODY:address 0 exit then
   shape CHILD-ABI:BUFFER = if s" private LBUF ELEMENT" CHILD-ROUTINE exit then
   shape CHILD-ABI:BIND = if s" private LBUF BIND" CHILD-ROUTINE exit then
   shape CHILD-ABI:GROW = if s" private LBUF GROW" CHILD-ROUTINE exit then
   shape CHILD-ABI:DYNAMIC = if
      s" private DYNAMIC-STORAGE ELEMENT" CHILD-ROUTINE exit
   then
   shape CHILD-ABI:RESERVE = if
      s" private DYNAMIC-STORAGE ENSURE" CHILD-ROUTINE exit
   then
   shape CHILD-ABI:RELEASE = if
      s" private DYNAMIC-STORAGE EMPTY" CHILD-ROUTINE exit
   then
   shape CHILD-ABI:SLOT = if s" private TASK SLOT" CHILD-ROUTINE exit then
   shape CHILD-ABI:SLOTS = if s" private TASK SLOT-AT" CHILD-ROUTINE exit then
   RC-REJECT throw ;

: CHILD-ANCHORED? ( n -- bool ) {: shape:n :}
   shape CHILD-ABI:STORED >= shape CHILD-ABI:SLOTS <= and ;

\ A value child is recorded as a constant, which a call folds to its number, a
\ STORED child as an address, which a call folds to an inline push.
: CHILD-KIND ( n -- n ) {: shape:n :}
   shape CHILD-ABI:VALUE = if DKIND:VAL exit then
   shape CHILD-ABI:STORED = if DKIND:ADDR DNAME-ALLOC or exit then
   shape CHILD-ANCHORED? if DNAME-ALLOC exit then
   0 ;

: CHILD-RECORD ( ptr u8 n n -- ) {: a:ptr u:n shape:n :}
   a u DEF-QUALIFY-NAME {: ta:ptr tu:n wid:n :}
   ta tu wid WL-PROBE XREF-FOUND? if a u PKG-DUPLICATE then
   0 BODYLEN-CELL CELL!  a u DEF-CAPTURE
   ta tu wid shape CHILD-KIND DEF-RECORD ;

\ The anchor is taken after CHECK:CHILD, whose facts may open an intern segment
\ at here.
: CHILD-CLOSE ( n n ptr u8 n -- n )
   {: shape:n param:n ta:ptr tu:n :}
   DEF-CAPTURED-NAME shape param ta tu CHECK:CHILD {: val:n h:n :}
   shape CHILD-BODY {: kind:COMP:body entry:n :}
   shape CHILD-ANCHORED? if DEF-ANCHOR entry else val 0 then
   kind DEF-FIXED
   DEF-CLOSE
   DEF-CAST-FACTS
   h ;

: CHILD-DEFINE ( ptr u8 n n n ptr u8 n -- n )
   {: a:ptr u:n shape:n param:n ta:ptr tu:n :}
   a u shape CHILD-RECORD
   0 [: drop shape param ta tu CHILD-CLOSE ;] catch {: h rc:n :}
   rc 0= if h exit then
   DEF-ABORT rc throw ;

\ Answers the child's effect handle. TYPE is empty for the shapes before STORED.
: CHILD-RUN ( ptr u8 n n n ptr u8 n -- n )
   {: a:ptr u:n shape:n param:n ta:ptr tu:n :}
   PEND-CELL CELL@ 0<> if s" child word" a u DEF-OPEN-REFUSE then
   a u DEF-ONE-TOKEN? 0= if s" child word" a u DEF-NAME-REFUSE then
   shape CHILD-BODY 2drop
   NCOMP-DISPATCH:COMP-CELL CELL@ 0= if DEF-NO-COMPILER then
   DEF-ROOM
   OVERLAY-GUARD
   a u shape param ta tu CHILD-DEFINE ;

\ A qualified linear name is refused by the certifier before DEF-QUALIFY can
\ create its namespace. A second colon is left to DEF-QUALIFY's syntax refusal.
: LINEAR-QUALIFIED? ( -- bool )
   TOKEN$ {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 0 <= q 1+ u >= or if false exit then
   a u q 1+ COLON-AT 0< ;

: DEF-LINEAR ( -- )
   DEF-ROOM
   s" linear:" OPERAND
   DEF-SEED
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   s end DEF-SIG-PASS
   LINEAR-QUALIFIED? if
      s 1 + end s - 2 - NCOMP-DISPATCH:DECL-LINEAR-OFF DEF-REGISTER-IDENTITY
      s" E-BAD-QUALIFIED" REFUSAL
      TOKEN$ ERROR-TEXT:TYPE  NL 1 ERROR-TEXT:TYPE  RC-REJECT throw
   then
   DEF-QUALIFY DKIND:CAST DEF-RECORD
   s end DEF-SIG-INNER
   TSIG-A-CELL ADDR@ TSIG-U-CELL CELL@ NCOMP-DISPATCH:DECL-LINEAR-OFF
   DEF-REGISTER-IDENTITY
   0 0 COMP-BODY:cast DEF-FIXED  DEF-CLOSE
   DEF-CAST-FACTS ;

\ `defer NAME ( in -- out )` declares a word that calls the execution token in
\ its dispatch cell, an aligned DATA cell that starts with `defer-unset`'s and
\ that `is` in a body re-points. The cell is allotted before the qualifier,
\ and the checker gets the effect before the body compiles, which asks it
\ whether the defer returns.

\ With the input at its end the token cells still hold the keyword, which the
\ refusal names.
: DEFER-NAME ( -- )
   TOKEN if exit then
   s" E-MISSING-NAME" REFUSAL
   s" defer: missing name after " ERROR-TEXT:TYPE TOKEN$ ERROR-TEXT:TYPE
   NL 1 ERROR-TEXT:TYPE  RC-NO-NAME throw ;

: DEFER-UNSET-XT ( -- n )
   s" defer-unset" FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if s" defer-unset" UNDEFINED-NAME then
   rec XREF-START ;

\ The cell holds an execution token, so xt! declares it one: a snapshot moves
\ it with the code region.
TRUSTED: DEF-XT! ( n n -- ) xt! ;

: DEFER-CELL ( n -- n )
   {: xt:n :}
   align
   DEF-HERE {: cell:n :}
   1 cells allot
   xt cell DEF-XT!
   cell ;

\ The checker's own source declares defers before it defines its trust-decl,
\ so a defer's name and signature wait in the next pending slot until the
\ checker replays both registrations to the target owner.
: DEFER-READY? ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-OFF PKG-TARGET 0<>
   NCOMP-DISPATCH:DECL-DEFER-OFF PKG-TARGET 0<> and ;

\ A full table, or a name or signature past its slot field, ends the process
\ naming the defer.
: PD-FULL ( -- )
   s" E-DEFER-TABLE-FULL" REFUSAL
   s" pre-trust defer table full: " ERROR-TEXT:TYPE TOKEN$ ERROR-TEXT:TYPE
   s" " DEF-RC-PD-FULL FAIL-CLOSED ;

: PD-PUT ( ptr u8 n n n n -- ) {: a:ptr u:n cap:n lenoff:n off:n :}
   u cap > if PD-FULL then
   u lenoff CELL!
   a data-base off + BYTE-VIEW u BYTE-COPY ;

: PD-HOLD ( ptr u8 n -- ) {: sig:ptr su:n :}
   PD-TABLE-OFF CELL@ {: count:n :}
   count PD-CAP >= if PD-FULL then
   PD-TABLE-OFF PD-SLOTS-REL + count PD-SLOT * + {: slot:n :}
   DEF-CAPTURED-NAME PD-NAME-CAP slot PD-NLEN-OFF + slot PD-NAME-OFF + PD-PUT
   sig su PD-SIG-CAP slot PD-SLEN-OFF + slot PD-SIG-OFF + PD-PUT
   count 1 + PD-TABLE-OFF CELL! ;

\ The active owner's operation at off, then the target owner's, 0 when the
\ active owner's field holds the same one.
: DEF-OWNERS ( n -- n n ) {: off:n :}
   NCOMP-DISPATCH:DECL-A off PKG-OPERATION {: own:n :}
   off PKG-TARGET {: target:n :}
   own  target own = if 0 else target then ;

\ Before the target owner is ready only the active owner's two are called, and
\ the pending slot holds the defer for the target's.
: DEFER-REGISTER ( ptr u8 n -- ) {: sig:ptr su:n :}
   DEFER-READY? {: ready:bool :}
   NCOMP-DISPATCH:DECL-EFFECT-OFF DEF-OWNERS {: own:n target:n :}
   own 0<> if sig su own DEF-DECLARE then
   ready target 0<> and if sig su target DEF-DECLARE then
   NCOMP-DISPATCH:DECL-DEFER-OFF DEF-OWNERS {: down:n dtarget:n :}
   down 0<> if DEF-CAPTURED-NAME down PKG-AS-NAME-ACTION execute then
   ready dtarget 0<> and if DEF-CAPTURED-NAME dtarget PKG-AS-NAME-ACTION execute then
   ready 0= if sig su PD-HOLD then ;

\ defer-unset is looked up before DEF-ROOM, so its refusal leaves no capture.
: DEF-DEFER ( -- )
   DEFER-UNSET-XT {: unset:n :}
   DEF-ROOM
   DEFER-NAME
   DEF-SEED
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   s end DEF-SIG-PASS
   unset DEFER-CELL {: cell:n :}
   DEF-QUALIFY 0 DEF-RECORD
   s end DEF-SIG-INNER
   s 1 + end s - 2 - DEFER-REGISTER
   cell 0 COMP-BODY:defer DEF-FIXED
   DEF-CLOSE ;

\ `immediate` marks the newest record, whatever it is, and checks nothing.
: DEFINE? ( -- bool )
   s" :" TOKEN-IS? if false DEF-HEAD true exit then
   s" kernel:" TOKEN-IS? if false DEF-HEAD true exit then
   s" trusted:" TOKEN-IS? if true DEF-HEAD true exit then
   s" create" TOKEN-IS? if false DEF-ADDR true exit then
   s" variable" TOKEN-IS? if true DEF-ADDR 1 cells allot true exit then
   s" constant" TOKEN-IS? if DEF-CONSTANT true exit then
   s" defer" TOKEN-IS? if DEF-DEFER true exit then
   s" cast:" TOKEN-IS? if DEF-CAST-ROW true exit then
   s" linear:" TOKEN-IS? if DEF-LINEAR true exit then
   s" immediate" TOKEN-IS? if imm-mark true exit then
   false ;

public

: INSTALL-CREATE ( -- )
   ['] DEF-CREATE-ROW data-base CREATEP-CELL + xt! ;

;package

require src/lib/memory.f
require src/habu/address-cells.f
require src/core/rollback-cells.f
require src/habu/native-body-abi.f
require src/habu/native-body.f
require src/habu/repl.f
require src/core/compile-reads.f

package INTERPRETER

private

\ A number is pushed and a word run, each under the floor: the token's effect
\ on the stack is the program's, so this row, and every row here that
\ runs program code, states none of it.
: PUSH-NUM ( -- )
   [: VALUE @ TOP-EV-NUM 0 HOOK ;] CELL-LITERAL RUN-LITERAL ;

: DISPATCH ( -- )
   NUMERAL? if PUSH-NUM exit then
   RUN-WORD ;

\ The design seal admits its syntax tokens and numbers. Another top-level
\ token is resolved once here, before a parsing word can consume its operand;
\ STEP then runs that same admitted record. Native compilation checks body
\ tokens, and DEF-IMMEDIATE checks one it executes while capturing a body.
: POLICY-SYNTAX? ( -- bool )
   s" :" TOKEN-IS? if true exit then
   s" if" TOKEN-IS? if true exit then
   s" else" TOKEN-IS? if true exit then
   s" then" TOKEN-IS? if true exit then
   s" {:" TOKEN-IS? if true exit then
   s" :}" TOKEN-IS? if true exit then
   S\" s\q" TOKEN-IS? if true exit then
   S\" s\\\q" TOKEN-IS? if true exit then
   s" [package]" TOKEN-IS? if true exit then
   s" package" TOKEN-IS? if true exit then
   s" public" TOKEN-IS? if true exit then
   s" private" TOKEN-IS? if true exit then
   s" ;package" TOKEN-IS? if true exit then
   s" using" TOKEN-IS? if true exit then
   s" ;using" TOKEN-IS? ;

: POLICY-PRE ( -- bool )
   POLICY-NDICT-CELL CELL@ 0= if false exit then
   PEND-CELL CELL@ 0<> if false exit then
   POLICY-SYNTAX? if false exit then
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   num if false exit then
   SEARCH
   true ;

\ Inside a build a declaration word found in product text is the product's
\ copy, which records into a checker that checks nothing, so hb's word of that
\ spelling runs instead. hb's global wordlist is 0, as every linked program's.
: DECLARATION? ( -- bool )
   s" trust" TOKEN-IS? if true exit then
   s" prim:" TOKEN-IS? if true exit then
   s" pprim:" TOKEN-IS? if true exit then
   s" prim-top-only!" TOKEN-IS? if true exit then
   s" prim-trusted-only!" TOKEN-IS? if true exit then
   s" intrinsic" TOKEN-IS? if true exit then
   s" nominal-provider" TOKEN-IS? if true exit then
   s" parse-imm" TOKEN-IS? if true exit then
   s" names:" TOKEN-IS? if true exit then
   s" parses:" TOKEN-IS? if true exit then
   s" parses-through:" TOKEN-IS? if true exit then
   s" generates:" TOKEN-IS? if true exit then
   s" typed-variable" TOKEN-IS? if true exit then
   s" typed-buffer" TOKEN-IS? if true exit then
   s" layout-buffer" TOKEN-IS? if true exit then
   s" defer-layout-buffer" TOKEN-IS? if true exit then
   s" dynamic-buffer" TOKEN-IS? if true exit then
   s" cwork-buffer" TOKEN-IS? if true exit then
   s" cwork-span" TOKEN-IS? if true exit then
   s" cwork-variable" TOKEN-IS? if true exit then
   s" structure" TOKEN-IS? if true exit then
   s" enum" TOKEN-IS? if true exit then
   s" sumtype" TOKEN-IS? if true exit then
   s" newtype" TOKEN-IS? if true exit then
   s" deftype" TOKEN-IS? if true exit then
   s" deflinear" TOKEN-IS? if true exit then
   s" product" TOKEN-IS? if true exit then
   s" arena" TOKEN-IS? if true exit then
   s" heap" TOKEN-IS? ;

\ A file loader found in product text is the product's, which would record the
\ file in its own registry, outside the build; hb's runs it through the build.
: LOADER? ( -- bool )
   s" include" TOKEN-IS? if true exit then
   s" require" TOKEN-IS? if true exit then
   s" included" TOKEN-IS? if true exit then
   s" required" TOKEN-IS? if true exit then
   s" provided" TOKEN-IS? ;

\ The product's `undefine` would retire the name from the product's checker,
\ which checks nothing; hb's retires it from the checker that checks the build.
: HOST-WORD? ( -- bool )
   data-base BOOT:IMAGE-BASE-CELL + @ 0= if false exit then
   DECLARATION? LOADER? or  s" undefine" TOKEN-IS? or  0= if false exit then
   TOKEN$ 0 XREF-FIND-WL REC !
   REC @ XREF-FOUND? 0= if UNDEFINED then
   true ;

: STEP ( -- )
   \ A parser word owns its next token, even when it spells a comment opener.
   BODY-RAW-NEXT @ if COMPILING? drop exit then
   COMMENT? if exit then
   POLICY-PRE {: found:bool :}
   COMPILING? if exit then
   LITERAL? if exit then
   PACKAGE? if exit then
   DEFINE? if exit then
   HOST-WORD? if RUN-FOUND exit then
   found if RUN-FOUND exit then
   DISPATCH ;

\ The kernel's nested-evaluate recovery rolls an outer definition back to the
\ token it was reading. Mark before TOKEN, including the pass that reaches EOF.
: TOKEN-MARK ( -- )
   BODYLEN-CELL CELL@ TOKBODY-CELL CELL!
   LOCN-CELL CELL@ TOKLOCN-CELL CELL!
   BODY-DEPTH @ TOK-BODY-DEPTH !
   BODY-RAW-NEXT @ TOK-BODY-RAW !
   cp@ dbase@ - TOKCP-CELL CELL!
   VSP-CELL CELL@ TOKVSP-CELL CELL! ;

\ Reading source on a task's thread ends the process: only the main thread
\ compiles.
: TASK-GUARD ( -- )
   TASK-TCB-CELL CELL@ 0= if exit then
   s" E-TASK-COMPILE" REFUSAL
   s" source read on a task's thread; only the main thread compiles"
   RC-TASK-THREAD FAIL-CLOSED ;

: RUN-TOKENS ( -- )
   TASK-GUARD  begin TOKEN-MARK TOKEN while STEP repeat ;

: RUN ( -- )
   RUN-TOKENS
   DEF-SOURCE-END ;

: CLEAR-BODY-TAKE ( -- )
   NATIVE-BODY:BODY-CAPABILITY dup NULL-PTR = if drop exit then
   {: record:ptr :}
   0 record NATIVE-BODY-ABI:PENDING-TAKE cells + CELL-VIEW ! ;

\ An evaluate the buffer caught left its code, which would fail the load.
\ A `package` in it leaves PKG-OUTSIDE-CELL 0, which matters only in a package.
: CLEAN-END ( n -- ) {: outside:n :}
   0 EVALERR-CELL CELL!
   USE-FLOOR @ USE-DEPTH-CELL CELL@ min {: back:n :}
   back USE-DEPTH-CELL CELL!
   USE-PKG-SAVE-CELL CELL@ back > if back USE-PKG-SAVE-CELL CELL! then
   PKG-OUTSIDE-CELL CELL@ 0<> if outside PKG-OUTSIDE-CELL CELL! then ;

\ A throw out of an evaluated buffer reaches the kernel's throw recovery, which
\ pops the frame and reads these fields where they sit. The clean-exit
\ continuation at $10 stays zero: nothing reads it for a buffer this frame
\ bounds.
0 constant FR-INP
8 constant FR-INE
$18 constant FR-BOUND
$20 constant FR-XDS
$28 constant FR-CP
$30 constant FR-NDICT
$38 constant FR-DP

\ The frame on its way into the catch that links it; empty at rest.
TYPED-VARIABLE FRAME-ARG ptr u8

\ The source owner's record end and its cut, raw execution tokens in the owner
\ record.
CAST: AS-ROWS-END ( n -- [ -- n ] )
CAST: AS-ROWS-CUT ( n -- [ n -- ] )

\ The checker's record end as a buffer begins, plus one, or 0 when no owner
\ records rows.
: ROWS-MARK ( -- n )
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-ROWS-END-OFF PKG-OPERATION {: xt:n :}
   xt 0= if 0 exit then
   xt AS-ROWS-END execute 1+ ;

\ Cut the checker's records back to the frame's mark, so no definition the
\ rollback drops keeps a record.
: ROWS-CUT ( ptr u8 -- ) {: f:ptr :}
   f EVAL-FRAME:ROWS FR@ {: mark:n :}
   mark 0= if exit then
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-RETRACT-ROWS-OFF PKG-OPERATION {: xt:n :}
   xt 0= if exit then
   mark 1- xt AS-ROWS-CUT execute ;

\ Past the frame, its mapping holds the mapping's length, then the saved cells.
: FRAME-CELLS ( ptr u8 -- ptr u8 ) EVAL-FRAME-SIZE CELL + + ;

\ The frame of a buffer whose caller leaves n cells on the data stack, all but
\ its link. Its using depth is also the buffer's using floor.
: FRAME-OPEN ( n -- ptr u8 ) {: cells-in:n :}
   EVAL-FRAME-SIZE CELL + ROLLBACK-CELLS:BYTES + {: total:n :}
   total MEM-ALLOC-PTR {: f:ptr :}
   total f EVAL-FRAME-SIZE FR!
   INP-CELL CELL@ f FR-INP FR!
   INE-CELL CELL@ f FR-INE FR!
   SRCLOC:INB-CELL CELL@ f EVAL-INB FR!
   STACK-ABI:BASE-CELL CELL@ cells-in cells + f FR-XDS FR!
   cp@ f FR-CP FR!
   ndict@ f FR-NDICT FR!
   DP-CELL CELL@ f FR-DP FR!
   STACK-ABI:BASE-CELL CELL@ f STACK-ABI:EVAL-BASE FR!
   STACK-ABI:CAP-CELL CELL@ f STACK-ABI:EVAL-CAP FR!
   PEND-CELL CELL@ f EVAL-FRAME:PEND FR!
   f EVAL-PKG + {: snap:ptr :}
   CUR-CELL CELL@ snap PKGSNAP-CUR FR!
   PKG-PUB-CELL CELL@ snap PKGSNAP-PUB FR!
   PKG-PRI-CELL CELL@ snap PKGSNAP-PRI FR!
   PKG-PARENT-CELL CELL@ snap PKGSNAP-PARENT FR!
   PKG-REC-CELL CELL@ snap PKGSNAP-REC FR!
   ENCLOSING:END ENCLOSING:PUB - 0 ?do
      ENCLOSING:PUB i + CELL@  f EVAL-FRAME:ENCLOSING i + FR!
   CELL +loop
   USE-DEPTH-CELL CELL@ snap PKGSNAP-USE FR!
   USE-PKG-SAVE-CELL CELL@ snap PKGSNAP:FLOOR FR!
   PKG-OUTSIDE-CELL CELL@ f EVAL-FRAME:PKG-OUTSIDE FR!
   SECTION-CELL CELL@ f EVAL-FRAME:SECTION FR!
   USE-MAX 0 ?do
      i cells USE-WIDS-OFF + CELL@  f EVAL-FRAME:USE-WIDS i cells + FR!
   loop
   ROWS-MARK f EVAL-FRAME:ROWS FR!
   f FRAME-CELLS ROLLBACK-CELLS:SAVE
   f ;

\ It runs first in the evaluate's catch, so the nearest handler is that
\ catch's: throw recovery pops a frame whose boundary lies at or below the
\ nearest handler, and leaves it for a handler the buffer installs, below.
: FRAME-LINK ( ptr u8 -- ) {: f:ptr :}
   NULL-PTR FRAME-ARG !
   HND-CELL CELL@ f FR-BOUND FR!
   EVAL-TOP-CELL CELL@ f EVAL-PREV FR!
   f NULL-PTR - EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1+ EVALD-CELL CELL! ;

: FRAME-UNLINK ( ptr u8 -- ) {: f:ptr :}
   f EVAL-PREV FR@ EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1- EVALD-CELL CELL!
   f FR-INP FR@ INP-CELL CELL!
   f FR-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL! ;

: FRAME-LEAVE ( ptr u8 -- ) {: f:ptr :}
   f FRAME-UNLINK
   f EVAL-FRAME:SECTION FR@ SECTION-CELL CELL!
   f EVAL-FRAME:PKG-OUTSIDE FR@ CLEAN-END ;


: FRAME-CUT ( ptr u8 -- n )
   {: f:ptr :}
   data-base BYTE-VIEW NULL-PTR BYTE-VIEW - {: base:n :}
   f FR-DP FR@ base DATA-FLOOR-CELL CELL@ + max ;

\ x86 THROW returns with the evaluation frame still linked; arm64's throw
\ recovery has already popped it before catch returns. The top link is the
\ ownership test: exactly one path performs the rollback.
: FRAME-ROLLBACK-CODE ( ptr u8 -- )
   {: f:ptr :}
   DEF-ABORT
   f ROWS-CUT
   \ A buffer may start a task and throw without allocating a definition.
   \ Only changed cursors need their guarded mutation sinks during recovery.
   f FR-NDICT FR@ dup ndict@ <> if ndict! else drop then
   f FR-CP FR@ dup cp@ <> if CODE-RECLAIM:TRUNCATE else drop then
   data-base BYTE-VIEW NULL-PTR BYTE-VIEW - {: base:n :}
   f FRAME-CUT {: cut:n :}
   cut base - ADDRESS-CELLS:KEEP-BELOW
   cut INTERN:KEEP-BELOW
   \ Raise the release high-water to the DP this lowers from, so `allot`
   \ zeroes the reused space it hands out again.
   DP-CELL CELL@ base - DPHW-CELL CELL@ max DPHW-CELL CELL!
   0 COMMA-RUN-CELL CELL!
   cut DP-CELL CELL! ;


: FRAME-ROLLBACK-TOKEN ( -- )
   TOKBODY-CELL CELL@ BODYLEN-CELL CELL!
   TOKBODY-CELL CELL@ DEF-POSTPONED-CUT
   TOKBODY-CELL CELL@ ITEMS-CUT
   TOKLOCN-CELL CELL@ LOCN-CELL CELL!
   TOK-BODY-DEPTH @ BODY-DEPTH !
   TOK-BODY-RAW @ BODY-RAW-NEXT !
   dbase@ TOKCP-CELL CELL@ + CODE-RECLAIM:TRUNCATE
   TOKVSP-CELL CELL@ VSP-CELL CELL! ;

: FRAME-RESTORE-SCOPE ( ptr u8 -- ) {: f:ptr :}
   f EVAL-PKG + {: snap:ptr :}
   snap PKGSNAP-USE FR@ USE-DEPTH-CELL CELL!
   f EVAL-FRAME:PKG-OUTSIDE FR@ PKG-OUTSIDE-CELL CELL!
   f EVAL-FRAME:SECTION FR@ SECTION-CELL CELL!
   USE-MAX 0 ?do
      f EVAL-FRAME:USE-WIDS i cells + FR@
      i cells USE-WIDS-OFF + CELL!
   loop
   f EVAL-FRAME:ENCLOSING ENCLOSING:REC ENCLOSING:PUB - + FR@
   f EVAL-FRAME:ENCLOSING ENCLOSING:PARENT ENCLOSING:PUB - + FR@
   snap PKGSNAP-REC FR@ snap PKGSNAP-PARENT FR@
   snap PKGSNAP-CUR FR@ snap PKGSNAP:FLOOR FR@ PKG-RECOVER ;

: FRAME-RECOVER ( ptr u8 n -- ) {: f:ptr code:n :}
   f EVAL-FRAME:PEND FR@ 0= if f FRAME-ROLLBACK-CODE else FRAME-ROLLBACK-TOKEN then
   f FR-INP FR@ INP-CELL CELL!
   f FR-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL!
   f FRAME-RESTORE-SCOPE
   f EVAL-PREV FR@ EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1- EVALD-CELL CELL!
   code EVALERR-CELL CELL! ;

: FRAME-FREE ( ptr u8 -- )
   dup EVAL-FRAME-SIZE FR@ munmap 0<> if E-MEM-UNMAP throw then ;

\ A rollback that left DP above the cut (a live task's, a pending definition's)
\ kept every body; one that left DP at the cut puts the marked cells back.
: FRAME-CELLS-RESTORE ( ptr u8 -- )
   {: f:ptr :}
   DP-CELL CELL@ f FRAME-CUT = if
      f FRAME-CELLS ROLLBACK-CELLS:RESTORE
   then ;

\ The REPL has one source owner. A line uses a savepoint, but its successful
\ stack, pending definition, package and usings remain for the next line.
create REPL-QNL 63 c, 10 c,
variable REPL-OK

: REPL-STACK-CLEAR ( -- )
   stack-clear ;

: REPL-RECOVER ( ptr u8 n ptr n -- ) {: f:ptr prior:n found:ptr :}
   f FRAME-ROLLBACK-CODE
   f FR-INP FR@ INP-CELL CELL!
   f FR-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL!
   f FRAME-RESTORE-SCOPE
   prior USE-FLOOR !
   found REC !
   0 EVALERR-CELL CELL! ;

: REPL-LINE ( ptr u8 n -- bool ) {: a:ptr u:n :}
   depth FRAME-OPEN {: f:ptr :}
   \ The session, not this line, owns a pending definition from an earlier
   \ line, yet a failed line abandons it through this savepoint's full rollback.
   0 f EVAL-FRAME:PEND FR!
   USE-FLOOR @ {: prior:n :}
   REC @ {: found:ptr :}
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   f FRAME-ARG !
   [: FRAME-ARG @ FRAME-LINK RUN-TOKENS ;] catch {: code:n :}
   code 0= if
      f FRAME-UNLINK
   else
      f NULL-PTR - EVAL-TOP-CELL CELL@ = if f FRAME-UNLINK then
      f prior found REPL-RECOVER
      f FRAME-CELLS-RESTORE
   then
   f FRAME-FREE
   code 0<> if
      REPL-STACK-CLEAR
      2 REPL-QNL 2 write drop
      false exit
   then
   true ;

: REPL-END ( -- )
   ['] DEF-SOURCE-END catch {: code:n :}
   code 0<> if NULL$ code die then ;


public

\ Each line calls REPL-READ anew: genio may replace its reader. A definition
\ may span lines; EOF refuses one still open.
: REPL ( -- )
   0 ENTRY-PEND !
   true REPL-OK !
   begin
      REPL-OK @ if S\"  ok\n" type then
      REPL-READ {: a:ptr u:n :}
      a 0= if REPL-END exit then
      a u REPL-LINE REPL-OK !
   again ;

\ The input base stays the buffer's first byte, so a refusal's line counts
\ from the start of the whole source.
: EVAL-FRAMED-RANGE ( ptr u8 n n n -- ) {: a:ptr u:n first:n end:n :}
   depth FRAME-OPEN {: f:ptr :}
   USE-FLOOR @ {: outer:n :}
   REC @ {: found:ptr :}
   ENTRY-PEND @ {: prior:n :}
   PEND-CELL CELL@ {: entry:n :}
   entry ENTRY-PEND !
   USE-DEPTH-CELL CELL@ USE-FLOOR !
   1 PKG-OUTSIDE-CELL CELL!
   a first + INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a end + INE-CELL ADDR!
   f FRAME-ARG !
   [: FRAME-ARG @ FRAME-LINK RUN ;] catch {: code:n :}
   code 0= if
      f FRAME-LEAVE
   else
      f NULL-PTR - EVAL-TOP-CELL CELL@ = if f code FRAME-RECOVER then
      f FRAME-CELLS-RESTORE
      \ The kernel's engine-cell rollback cannot restore the Habu lexical
      \ capture cells.
      entry 0= if BODY-LEX-RESET else
         TOK-BODY-DEPTH @ BODY-DEPTH !
         TOK-BODY-RAW @ BODY-RAW-NEXT !
      then
   then
   prior ENTRY-PEND !
   code 0<> if
      entry PEND-CELL CELL@ <> if CLEAR-BODY-TAKE then
      entry PEND-CELL CELL!
   then
   outer USE-FLOOR !  found REC !
   f FRAME-FREE
   code 0<> if code throw then ;

: EVAL-FRAMED ( ptr u8 n -- ) {: a:ptr u:n :}
   a u COMPILE-READS:TEXT+
   a u 0 u EVAL-FRAMED-RANGE ;

: INSTALL-EVALUATE ( -- )
   ['] EVAL-FRAMED data-base PROVIDED-XT:EVALUATE-CELL + xt!
   ['] EVAL-FRAMED-RANGE data-base PROVIDED-XT:EVALUATE-RANGE-CELL + xt!
   ['] DEFINE-RUN data-base PROVIDED-XT:DEFINE-CELL + xt!
   ['] APPEND-RUN data-base PROVIDED-XT:APPEND-CELL + xt!
   ['] POSTPONE-RUN data-base PROVIDED-XT:POSTPONE-CELL + xt!
   ['] CHILD-RUN data-base PROVIDED-XT:CHILD-CELL + xt!
   ['] CHECK:STORAGE-LINE data-base PROVIDED-XT:STORAGE-LINE-CELL + xt!
   ['] LITERAL-RUN data-base PROVIDED-XT:LITERAL-CELL + xt!
   ['] CHECK:TYPE-OP data-base PROVIDED-XT:TYPE-OP-CELL + xt! ;

;package

\ Only INTERPRETER saves and puts back the marked cells: once its words above
\ bind these, they are retired.
undefine ROLLBACK-CELLS:BYTES
undefine ROLLBACK-CELLS:SAVE
undefine ROLLBACK-CELLS:RESTORE
