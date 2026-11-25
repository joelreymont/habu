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
\ of the input; a `(` comment the input ends inside is rejected at the `(`.
\
\ A float's exact value, the integer plus the fraction's remainder, is
\ normalized to 53 retained bits and rounded once to nearest, ties to even; the
\ sign applies even to zero.
\
\ Inside a build a declaration in the target's text runs hb's word of that
\ spelling (HOST-WORD?): the target's copy records into a checker that checks
\ nothing. A declaration is known by its binding's id, from parses: through the
\ declaration's own; a top-only word acts on the program's state, so the
\ target's copy runs. `intrinsic`, `nominal-provider`, `root-mover` and
\ `undefine` act on the checker that checks the build, so hb's run by their
\ spelling, as do the file loaders (LOADER?). hb's word is the one where the
\ target's binding comes from, looked up in hb's own wordlists: a build points
\ BOOT:GLOBAL-WL and BOOT:NAMESPACE-WL at the target's.

require src/core/exit-status.f
require src/lib/prelude.f
require src/core/bytes.f
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/wl.f
require src/habu/xref.f
require src/compiler/native/dict.f
require src/habu/t0.f
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


: AT? ( str n n -- bool ) {: s:str idx:n c:n :}
   idx s STR:LEN < if s STR:DATA idx + c@ c = else false then ;


: RUN-END ( str n n -- n ) {: s:str idx:n radix:n :}
   idx begin
      dup s STR:LEN < if dup s STR:DATA + c@ radix DIGIT nip else false then
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


: INTEGER ( ^u8 n n n -- n bool ) {: a:^ lo:n hi:n radix:n :}
   0 false
   hi lo ?do a i + c@ radix DIGIT drop radix INT-STEP loop ;


\ The fraction stays below the scale, so bounding the scale by MAX-N / 10 (18
\ digits) keeps both in a signed cell; past it the latch rejects the token.
: FRAC-STEP ( n n bool n -- n n bool ) {: frac:n scale:n wrapped:bool d:n :}
   frac 10 * d +
   scale 10 *
   scale MAX-N 10 / > wrapped or ;


: FRACTION ( ^u8 n n bool -- n n bool ) {: a:^ lo:n hi:n wrapped:bool :}
   0 1 wrapped
   hi lo ?do a i + c@ 10 DIGIT drop FRAC-STEP loop ;


: FRACTION? ( str n n -- bool ) {: s:str dot:n radix:n :}
   radix 10 <> if false exit then
   s dot [char] . AT? 0= if false exit then
   dot 1+ s STR:LEN <  s dot 1+ 10 RUN-END s STR:LEN =  and ;


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
: PREFIX ( str -- n bool n ) {: s:str :}
   s 0 [char] - AT? {: neg:bool :}
   neg if 1 else 0 then {: idx:n :}
   s idx [char] $ AT? if idx 1+ neg 16 else idx neg 10 then ;

public

\ The token's value (a float's bits), whether it is a float, whether it is a
\ number, and whether its shape is a number's but its value out of range.
\ The value and the float flag are zero unless it is a number.
: NUMBER ( str -- n bool bool bool ) {: s:str :}
   s PREFIX {: lo:n neg:bool radix:n :}
   lo s STR:LEN >= if NOT-NUMBER exit then
   s lo radix RUN-END {: dot:n :}
   s STR:DATA lo dot radix INTEGER {: int:n wrapped:bool :}
   dot s STR:LEN = if int wrapped neg radix INT-FINISH exit then
   s dot radix FRACTION? 0= if NOT-NUMBER exit then
   int  s STR:DATA dot 1+ s STR:LEN wrapped FRACTION  neg FLOAT-FINISH ;

;package

\ FIND answers the row the engine's lookup binds a token to whatever its flags:
\ the interpreter must see an internal row in order to reject it.

package INTERPRETER

private

\ Not the lookup: the row one wordlist holds under a name, whatever its flags.
: WL-PROBE ( str n -- ^n ) CHECKER-RESOLVE:WL-PROBE ;

: COLON-AT ( str n -- n )
   {: s:str from:n :}
   s STR:LEN from ?do
      s STR:DATA i + c@ $3A = if i unloop exit then
   loop
   -1 ;

EXPORT BOOT:data-base
EXPORT CHECKER-RESOLVE:USED-ONLY?
EXPORT CHECKER-RESOLVE:bare-scope-find

\ The scope's answer (open package, enclosing package, global, NAME:tail), never
\ a used public's.
: FIND-SCOPE ( str -- ^n )
   bare-scope-find drop drop {: rec:^ used:^ :}
   rec used USED-ONLY? if XREF-NULL exit then
   rec ;

public

: FIND ( str -- ^n )
   NDICT:SCOPE-REC ;

;package

\ The loop reads the engine's input cells, so a parsing word reads its buffer.

package INTERPRETER

private

EXPORT CHECK:BODY-KEYWORD?
EXPORT CHECK:NAME-KEYWORD?
EXPORT CHECK:DEF-NAME-REJECT?
EXPORT CHECK:BODY-ONLY-RECORD
EXPORT CHECK:CHECKER-USING$
EXPORT CHECK:CHECKER-USING-N
EXPORT CHECK:CLAUSE-NAME
EXPORT CHECK:TYPE-OP
EXPORT CHECK:TYPE-ARG
EXPORT CHECK:EFFECT?
EXPORT CHECK:TYPE-DENSE
EXPORT CHECK:EFFECT-DENSE
EXPORT CHECK:EFFECT-TEXT
EXPORT CHECK:TOP-LEVEL
EXPORT CHECK:TOP-TAKE
EXPORT CHECK:TOP-MOVE
EXPORT CHECK:USING-AMBIGUOUS-REJECT
EXPORT CHECK:USING-SHADOW-REJECT
EXPORT CHECK:CHECKER-VERIFY-TOP-BINDING
EXPORT CHECK:CHECKER-SYM-SOURCE
EXPORT CHECK:SYM-PKG$
EXPORT CHECK:SYM-NAME$
EXPORT CHECK:SYM-ROW
EXPORT CHECK:SYM.VIS
EXPORT CHECK:UNDEFINED-RECORD
EXPORT CHECK:KEYWORD-RECORD
EXPORT CHECKER-TAPE:COUNTED-MAX
EXPORT XREF:RETIRE-ABOVE
EXPORT CHECK:BUILDING?

32 constant BLANK                \ every byte at or below it separates tokens
$0A constant NEWLINE
$5C constant LINE-COMMENT        \ a backslash
$28 constant OPEN-COMMENT        \ (
$29 constant CLOSE-COMMENT       \ )
$22 constant QUOTE               \ a double quote closes a string literal
$20 constant CASE-BIT            \ ORed into A-Z it gives a-z

create NL NEWLINE c,

: CELL@ ( n -- n )
   data-base + @ ;

: CELL! ( n n -- )
   data-base + ! ;

\ The input cells, and a frame's saved copies of them, hold byte addresses.
\ ADDR@, ADDR! and FR-ADDR@ are the file's crossings between such a cell and a
\ byte pointer.
: ADDR@ ( n -- ^u8 ) data-base + 0 ptr-field @ ;
: ADDR! ( ^u8 n -- ) data-base + 0 ptr-field ! ;

\ A cell of a frame the engine holds.
: FR! ( n ^u8 n -- )
   + CELL-VIEW ! ;

: FR@ ( ^u8 n -- n )
   + CELL-VIEW @ ;

: FR-ADDR@ ( ^u8 n -- ^u8 )
   + 0 ptr-field @ ;

: TOKEN$ ( -- str )
   TKA-CELL ADDR@ TKL-CELL CELL@ >str ;

: FOLD ( n -- n ) {: c:n :}
   c [char] A [char] Z BETWEEN? if c CASE-BIT or exit then
   c ;

\ Whether s spells kw, which is lowercase. Only s's A-Z are folded.
: FOLDED= ( str str -- bool ) {: s:str kw:str :}
   s STR:LEN kw STR:LEN <> if false exit then
   s STR:LEN 0 ?do
      s STR:DATA i + c@ FOLD  kw STR:DATA i + c@ <> if false unloop exit then
   loop
   true ;

: TOKEN-IS? ( str -- bool )
   TOKEN$ swap FOLDED= ;

: BLANKS-END ( ^u8 ^u8 -- ^u8 ) {: p:^ e:^ :}
   p begin dup e < if dup c@ BLANK <= else false then while 1 + repeat ;

: WORD-END ( ^u8 ^u8 -- ^u8 ) {: p:^ e:^ :}
   p begin dup e < if dup c@ BLANK > else false then while 1 + repeat ;

\ With no token left INP is the end and the token cells keep the last token.
: TOKEN ( -- bool )
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:^ e:^ :}
   p e BLANKS-END {: s:^ :}
   s e WORD-END {: t:^ :}
   t INP-CELL ADDR!
   s t = if false exit then
   s TKA-CELL ADDR!
   t s - TKL-CELL CELL!
   TOKENS-CELL CELL@ 1 + TOKENS-CELL CELL!
   true ;

: SAY ( str -- ) REJECTION:EMIT-STR ;

\ Code in a rejection's text sits in backticks.
: TICKS ( str -- ) {: s:str :}
   96 REJECTION:EMIT1  s SAY  96 REJECTION:EMIT1 ;


\ The capture's first token, the definition's name, which a head seeds.
: DEF-CAPTURED-NAME ( -- str )
   BODY-TEXT:BASE-CELL ADDR@ BODYLEN-CELL CELL@ {: b:^ len:n :}
   0 begin
      dup len < if b over + c@ dup BLANK <> swap 0<> and else false then
   while 1+ repeat
   {: u:n :}
   b u >str ;

\ The name of the definition the engine holds open, empty when none is.
: OPEN-NAME ( -- str )
   PEND-CELL CELL@ 0= if NULL$ exit then
   DEF-CAPTURED-NAME ;

\ Placed at the name's own line when its bytes lie in the open file: a definer
\ may call a name it read earlier.
: UNDEFINED-NAME ( str -- ) {: s:str :}
   s OPEN-NAME UNDEFINED-RECORD
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: UNDERFLOW ( -- )
   s" E-UNDERFLOW" REJECTION:OPEN
   s" the stack ran out at " SAY  TOKEN$ TICKS
   TOKEN$ REJECTION:AT  s" stack underflow" SAY
   REJECTION:HELP  s" push the values the code at " SAY  TOKEN$ TICKS
   s"  takes before it" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: UNDEFINED ( -- )
   TOKEN$ BODY-KEYWORD? if
      TOKEN$ KEYWORD-RECORD  EXIT-STATUS:REJECTED REJECTION:STOP
   else
      TOKEN$ UNDEFINED-NAME
   then ;

\ The token reads as a number that does not fit in a cell.
: NUMBER-RANGE ( -- )
   TOKEN$ REJECTION:LITERAL-RANGE  EXIT-STATUS:REJECTED REJECTION:STOP ;

: USED-WID ( n -- n )
   cells USE-WIDS-OFF + CELL@ ;

\ Whether using slot s exports the name and no earlier slot uses its
\ package.
: ANSWERS? ( str n -- bool ) {: name:str s:n :}
   name s USED-WID WL-PROBE XREF-FOUND? 0= if false exit then
   s 0 ?do
      i USED-WID s USED-WID = if false unloop exit then
   loop
   true ;

\ xt runs on each using slot that exports the name, in using order.
: ANSWERING ( [ n -- ] str -- ) {: xt name:str :}
   CHECKER-USING-N 0 ?do
      name i ANSWERS? if i xt execute then
   loop ;

variable ANSWERERS   \ bit i set when using slot i exports the ambiguous name

: AMBIGUOUS ( str -- ) {: s:str :}
   0 ANSWERERS !
   [: 1 swap lshift ANSWERERS @ or ANSWERERS ! ;] s ANSWERING
   s ANSWERERS @ USING-AMBIGUOUS-REJECT
   EXIT-STATUS:REJECTED REJECTION:STOP ;

variable ANSWERED

\ The first using slot that exports the name.
: ANSWER ( str -- n ) {: s:str :}
   -1 ANSWERED !
   [: ANSWERED @ 0< if ANSWERED ! else drop then ;] s ANSWERING
   ANSWERED @ ;

: SHADOWED ( str -- ) {: s:str :}
   s  s ANSWER CHECKER-USING$ USING-SHADOW-REJECT
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Ends the open record and the process with status rc; a capture's records go
\ out first, since die-closed runs no exit action.
: KEEP-DIE ( n -- ) {: rc:n :}
   REJECTION:KEEP  REJECTION:RELEASE  s" " rc die-closed ;

: SEALED-FAIL ( str str -- )
   REJECTION:SEALED  EXIT-STATUS:REJECTED KEEP-DIE ;

\ While a checker overlay is open a definer head, a new package row or private
\ wid, or an export throws by name: the overlay's close would drop what it
\ wrote.
: OVERLAY-GUARD ( -- )
   REPLAY-SCOPE:LATCH CELL@ 0= if exit then
   s" E-OVERLAY-OPEN" REJECTION:OPEN
   s" a word is defined at " SAY  TOKEN$ TICKS
   s"  while a checker replay is open" SAY
   TOKEN$ REJECTION:AT  s" replay still open" SAY
   REJECTION:HELP  s" close the checker replay before this definition" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Whether c was found: INP stops past it, else at the end of the input.
: SKIP-PAST ( n -- bool )
   {: c:n :}
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:^ e:^ :}
   p begin dup e < if dup c@ c <> else false then while 1 + repeat
   dup e < {: found:bool :}
   found if 1 + then
   INP-CELL ADDR!
   found ;

\ The cursor goes back to the `(`, so the input's line is the comment's.
: UNCLOSED ( -- )
   TKA-CELL CELL@ INP-CELL CELL!
   s" E-UNTERMINATED-COMMENT" REJECTION:OPEN
   s" the comment " SAY  TOKEN$ TICKS  s"  opens is never closed" SAY
   TOKEN$ REJECTION:AT  s" the comment opens here" SAY
   REJECTION:HELP  s" close the comment with " SAY  s" )" TICKS
   EXIT-STATUS:REJECTED REJECTION:STOP ;

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

\ TOP-LEVEL judges each number after its push and each word MOVE-WORD? does not
\ name before it runs; a check that leaves the stack below its base is rejected.
CAST: HOOK-CALL ( [ -- nominal ] -- n )

\ The call runs under the floor, so it pushes the event itself: a quotation
\ reads no local, and the class waits in this cell.
variable EVENT-CLASS

: HOOK ( n -- )
   EVENT-CLASS !
   [: TOKEN$ EVENT-CLASS @ TOP-LEVEL ;]
   HOOK-CALL execute-floor FLOORED ;

variable VALUE

: NUMERAL? ( -- bool )
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   range if NUMBER-RANGE then
   v VALUE !
   num ;

\ EVAL-FRAMED-RANGE restores it as the buffer ends, since a stripped image
\ would hold a record that outlived its buffer as an undeclared dictionary
\ pointer.
TYPED-VARIABLE REC ^n

\ A sealed source may name a record it defined after the seal or a callable
\ public record in an admitted wordlist.
: POLICY-REJECT ( str str -- ) {: s:str t:str :}
   s" E-POLICY" REJECTION:OPEN
   s TICKS  s"  is not in the vocabulary this source may use" SAY
   t REJECTION:AT  s" not admitted" SAY
   REJECTION:HELP
   s" call only public words of the packages admitted before the seal" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: POLICY-CHECK-REC ( str ^n -- ) {: s:str rec:^ :}
   POLICY-NDICT-CELL CELL@ {: seal:n :}
   seal 0= if exit then
   rec XREF-FOUND? 0= if s s POLICY-REJECT then
   rec XREF-NULL - seal XREF-REC-ADDR < if
      rec XREF-WORDLIST {: wid:n :}
      wid 0 < wid POLICY-WID-MAX >= or if s s POLICY-REJECT then
      wid 6 rshift 8 * data-base POLICY-BITS-OFF + + @
      wid 63 and rshift 1 and 0= if s s POLICY-REJECT then
   then
   rec XREF-START 0= if s s POLICY-REJECT then ;

\ The require check of the record the token s binds; none binds no record.
\ The checker judges every other token the load binds, but not a top-level tick.
: REQUIRE-CHECK ( str ^n -- ) {: s:str rec:^ :}
   rec XREF-FOUND? 0= if exit then
   s rec XREF-NULL - XREF-ADDR>INDEX REQUIRES:CHECK ;

\ At top level the ordinary `require`'s global record, spelled so, is in the
\ vocabulary as well. A body never admits it.
: POLICY-REQUIRE? ( str ^n -- bool ) {: s:str rec:^ :}
   POLICY-REQUIRE-CELL CELL@ {: req:n :}
   req 0= if false exit then
   rec XREF-FOUND? 0= if false exit then
   rec XREF-START req <> if false exit then
   rec XREF-WORDLIST WL:GLOBAL? 0= if false exit then
   s s" require" FOLDED= ;

\ The record, rejected, naming it, where a bare name meets two used publics
\ that export it, or a global and a used public that do.
: USING-CHECK ( str ^n ^n n -- ^n )
   {: s:str rec:^ used:^ flags:n :}
   flags SCOPE-FIND-AMBIGUOUS and 0<> {: amb:bool :}
   rec XREF-FOUND? 0= if amb if s AMBIGUOUS then rec exit then
   rec XREF-WORDLIST WL:GLOBAL? 0= if rec exit then
   amb if s AMBIGUOUS then
   used XREF-FOUND? if s SHADOWED then
   rec ;

\ The record the name binds, rejected, naming it, where a sealed source may
\ not call it, then where USING-CHECK rejects it.
: LOOKUP-NAME ( str -- ^n ) {: s:str :}
   s scope-find {: rec:^ used:^ used2:^ flags:n :}
   s rec POLICY-CHECK-REC
   s rec used flags USING-CHECK ;

\ LOOKUP-NAME for the top-level token, whose policy also admits the exempt
\ require.
: SEARCH ( -- )
   TOKEN$ bare-scope-find {: rec:^ used:^ used2:^ flags:n :}
   TOKEN$ rec POLICY-REQUIRE? 0= if TOKEN$ rec POLICY-CHECK-REC then
   TOKEN$ rec REQUIRE-CHECK
   TOKEN$ rec used flags USING-CHECK REC ! ;

: LOOKUP ( -- )
   SEARCH
   REC @ XREF-FOUND? 0= if UNDEFINED then ;

: INTERNAL ( -- )
   s" E-UNSAFE" REJECTION:OPEN
   TOKEN$ TICKS  s"  is an internal engine word" SAY
   TOKEN$ REJECTION:AT  s" internal to the engine" SAY
   REJECTION:HELP
   s" call a public word instead: an internal word serves the engine alone"
   SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The gate before a record's xt leaves the loop: an internal word. A wide
\ effect passes: the interpret row carries each multi-cell value as one item
\ of its type, and the checker rejects an engine primitive that would move
\ part of one (src/core/check-walk.f TOP-XPORT-GUARD).
: XT-GATE ( -- )
   REC @ XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if INTERNAL then ;

\ Name dispatch and tick reject an exact native code entry, including an alias,
\ when it carries a scope kind. Arbitrary untyped xref xt execution is outside
\ these paths.
: SCOPE-ENTRY-GUARD ( -- )
   REC @ XREF-START CHECK:SCOPE-CODE-KIND 0= if exit then
   s" E-UNSAFE" REJECTION:OPEN
   TOKEN$ TICKS
   s"  is a scoped entry, which only a checked definition may call" SAY
   TOKEN$ REJECTION:AT  s" scoped entry" SAY
   REJECTION:HELP  s" call " SAY  TOKEN$ TICKS
   s"  inside a definition, where the checker proves its scope" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: GATE ( -- )
   XT-GATE
   SCOPE-ENTRY-GUARD ;

\ The dictionary record index of each word run since LOG-BUILD, in order.
DYNAMIC-BUFFER RUNS n
variable RUNS-N
variable RUNS-CAP
TYPED-VARIABLE LOGGING bool

: RUN+ ( n -- ) {: idx:n :}
   LOGGING @ 0= if exit then
   RUNS-N @ RUNS-CAP @ = if
      RUNS-CAP @ 0= if 1024 else RUNS-CAP @ 2 * then
      dup RUNS-RESERVE RUNS-CAP !
   then
   idx RUNS-N @ RUNS !
   RUNS-N @ 1 + RUNS-N ! ;

\ Each record declared since LOG-BUILD: its index and its declaration's token in
\ an open file, empty ones too: a reused index's newest entry is then its own.
DYNAMIC-BUFFER DECL-IDX n
DYNAMIC-BUFFER DECL-A ^u8
DYNAMIC-BUFFER DECL-U n
variable DECLS-N
variable DECLS-CAP

: DECL+ ( n str -- ) {: idx:n t:str :}
   LOGGING @ 0= if exit then
   DECLS-N @ DECLS-CAP @ = if
      DECLS-CAP @ 0= if 1024 else DECLS-CAP @ 2 * then
      dup DECL-IDX-RESERVE  dup DECL-A-RESERVE  dup DECL-U-RESERVE  DECLS-CAP !
   then
   t REJECTION:FILE-TOKEN$ {: f:str :}
   idx DECLS-N @ DECL-IDX !
   f STR:DATA DECLS-N @ DECL-A !
   f STR:LEN DECLS-N @ DECL-U !
   DECLS-N @ 1 + DECLS-N ! ;


\ A literal a program keeps is program code: the quotation that pushes it runs
\ under the floor as a word does. The cells it leaves are the program's, so
\ these views state its effect only to project it to the number execute-floor
\ takes.
CAST: STR-LITERAL ( [ -- str ] -- n )
CAST: CSTR-LITERAL ( [ -- ^u8 ] -- n )
CAST: CELL-LITERAL ( [ -- n ] -- n )
\ A cell taken from the program.
CAST: CELL-TAKE ( [ n -- ] -- n )

: RUN-LITERAL ( n -- )
   execute-floor FLOORED ;

\ The engine's `drop`, `@` or `!` in any case: a user's global word of that
\ name takes the hook.
: MOVE-WORD? ( n -- bool ) {: xt:n :}
   xt ['] drop COMP:XT>N =  xt ['] @ COMP:XT>N = or  xt ['] ! COMP:XT>N = or ;

\ Counts a run of the found word.
: RUN-COUNTED ( -- )
   REC @ XREF-NULL - XREF-ADDR>INDEX RUN+ ;

\ The address of the next cell `@` or `!` moves: a quotation reads no local.
variable MOVE-AT

: PUSH-MOVE-AT ( -- )
   [: MOVE-AT @ ;] CELL-LITERAL RUN-LITERAL ;

\ The engine `@` over a value of w cells fetches cell i from the address plus
\ i cells, cell 0 deepest, as a body's fetch leaves them.
: FETCH-CELLS ( n -- ) {: w:n :}
   [: MOVE-AT ! ;] CELL-TAKE RUN-LITERAL
   w 0 ?do
      PUSH-MOVE-AT  ['] @ COMP:XT>N execute-floor FLOORED
      1 cells MOVE-AT +!
   loop ;

\ The engine `!` over a value of w cells stores cell i at the address plus i
\ cells, where a body's store puts it, taking the top cell first.
: STORE-CELLS ( n -- ) {: w:n :}
   [: MOVE-AT ! ;] CELL-TAKE RUN-LITERAL
   w 1 - cells MOVE-AT +!
   w 0 ?do
      PUSH-MOVE-AT  ['] ! COMP:XT>N execute-floor FLOORED
      -1 cells MOVE-AT +!
   loop ;

\ The checker answers the width of the value the word moves; `drop` runs once
\ per cell.
: RUN-MOVE ( n -- ) {: xt:n :}
   TOP-MOVE {: w:n :}
   RUN-COUNTED
   w 1 > if
      xt ['] @ COMP:XT>N = if w FETCH-CELLS exit then
      xt ['] ! COMP:XT>N = if w STORE-CELLS exit then
   then
   w 0 ?do xt execute-floor FLOORED loop ;

\ The xt is read before the hooks run. The engine `drop`, `@` and `!` move a
\ multi-cell value whole, as in a body; any other word runs once outside a
\ loop, so it has every loop frame.
: RUN-FOUND ( -- )
   GATE
   REC @ XREF-START {: xt:n :}
   xt MOVE-WORD? if xt RUN-MOVE exit then
   TOP-EV-WORD HOOK RUN-COUNTED
   xt execute-floor FLOORED ;

: RUN-WORD ( -- )
   SEARCH
   REC @ XREF-FOUND? 0= if
      TOKEN$ BODY-KEYWORD? if
         TOKEN$ BODY-ONLY-RECORD  EXIT-STATUS:REJECTED REJECTION:STOP
      else
         UNDEFINED
      then
   then
   RUN-FOUND ;

\ A keyword is matched before the token is read as a number or a word, and it
\ reads the input after it itself. Each keyword table is a word that runs the
\ token when it is one of its rows and answers whether it was.

\ The input ended after keyword kw: the token cells still hold the keyword, and
\ the text names kw lowercase whatever the token's case.
: NAME-MISSING ( str -- ) {: kw:str :}
   kw TOKEN$ s" no name follows" REJECTION:MISSING-NAME
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: OPERAND ( str -- ) {: kw:str :}
   TOKEN if exit then
   kw NAME-MISSING ;

\ A literal's text starts one byte past the keyword, past the blank that ended
\ it, and runs to a closing quote that INP then passes. A literal's rejections
\ name its keyword. A literal a program keeps is copied into data space it
\ allots first, after its own rejections, so allot's rejections come before any
\ of it is written.

public
\ Writes the record of the string literal its opener t opens, which has no
\ closing quote or holds a bad escape, through its help and leaves it open: the
\ caller ends it.
: LITERAL-REJECT ( str -- ) {: t:str :}
   s" E-UNTERMINATED-STRING" REJECTION:OPEN
   s" the string " SAY  t TICKS
   s"  opens has no closing quote or holds a bad escape" SAY
   t REJECTION:AT  s" the string opens here" SAY
   REJECTION:HELP  s" end the string with " SAY  S\" \"" TICKS
   s" , and follow each backslash with an escape letter, or with " SAY
   s" x" TICKS  s"  and two hex digits" SAY ;
private

: BAD-LITERAL ( -- )
   TOKEN$ LITERAL-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

: TOO-LONG ( -- )
   TOKEN$ COUNTED-MAX REJECTION:COUNTED-RANGE
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: PAST ( ^u8 -- )
   1 + INP-CELL ADDR! ;

: ROOM ( n -- ^u8 )
   here swap allot ;

: TEXT ( -- str )
   INP-CELL ADDR@ 1 + INE-CELL ADDR@ {: s:^ e:^ :}
   s begin dup e < if dup c@ QUOTE <> else false then while 1 + repeat
   {: q:^ :}
   q e >= if BAD-LITERAL then
   q PAST
   s q s - >str ;

: COPY-HERE ( str -- str ) {: s:str :}
   s STR:LEN ROOM {: d:^ :}
   s STR:DATA d s STR:LEN BYTE-COPY
   d s STR:LEN >str ;

: KEEP ( -- str )
   TEXT {: t:str :}
   t COPY-HERE {: d:str :}
   d t SRCLOC:LITERAL!
   d ;

\ c": a count byte, then the text.
: COUNTED ( -- ^u8 )
   TEXT {: t:str :}
   t STR:LEN COUNTED-MAX > if TOO-LONG then
   t STR:LEN 1 + ROOM {: d:^ :}
   t STR:LEN d c!
   t STR:DATA d 1 + t STR:LEN BYTE-COPY
   d ;

\ An escaped literal's start, its closing quote and the count of bytes it
\ decodes to. A bad escape rejects as a missing quote does, INP unmoved.
: ESC-SCAN ( -- ^u8 ^u8 n )
   INP-CELL ADDR@ 1 + {: s:^ :}
   s INE-CELL ADDR@ ESCAPE:SPAN {: q:^ u:n ok:bool :}
   ok 0= if BAD-LITERAL then
   s q u ;

\ An escaped literal's copy, then its bytes between the quotes as written.
: ESC-COPY ( -- str str )
   ESC-SCAN {: s:^ q:^ u:n :}
   q PAST
   u ROOM {: d:^ :}
   s q d ESCAPE:COPY
   d u >str  s q s - >str ;

: ESC-KEEP ( -- str )
   ESC-COPY {: d:str t:str :}
   d t SRCLOC:LITERAL!
   d ;

: ESC-COUNTED ( -- ^u8 )
   ESC-SCAN {: s:^ q:^ u:n :}
   u COUNTED-MAX > if TOO-LONG then
   q PAST
   u 1 + ROOM {: d:^ :}
   u d c!
   s q d 1 + ESCAPE:COPY
   d ;

: PUSH-STR ( -- )
   [: KEEP TOP-EV-STR HOOK ;] STR-LITERAL RUN-LITERAL ;

: PUSH-CSTR ( -- )
   [: COUNTED TOP-EV-CSTR HOOK ;] CSTR-LITERAL RUN-LITERAL ;

: TYPE-STR ( -- )
   TEXT type ;

: PUSH-ESC-STR ( -- )
   [: ESC-KEEP TOP-EV-STR HOOK ;] STR-LITERAL RUN-LITERAL ;

: PUSH-ESC-CSTR ( -- )
   [: ESC-COUNTED TOP-EV-CSTR HOOK ;] CSTR-LITERAL RUN-LITERAL ;

: TYPE-ESC-STR ( -- )
   ESC-COPY drop type ;

: OPEN-PKG-NAME ( -- str )
   PKG-REC-CELL CELL@ {: rec:n :}
   rec 0= if 0 ROOM 0 >str exit then
   rec XREF-ADDR>REC XREF-NAME$ {: name:str :}
   name STR:LEN ROOM {: d:^ :}
   name STR:LEN 0 ?do
      name STR:DATA i + c@ STR:ASCII-LOWER d i + c!
   loop
   d name STR:LEN >str ;

: PUSH-PKG ( -- )
   [: OPEN-PKG-NAME TOP-EV-STR HOOK ;] STR-LITERAL RUN-LITERAL ;

\ No definition is open at top level, so no body text captures the operand.
: FIRST-BYTE ( -- n )
   s" char" OPERAND
   TOKEN$ STR:DATA c@ ;

\ The hook sees the operand as the token.
: PUSH-CHAR ( -- )
   [: FIRST-BYTE TOP-EV-CHAR HOOK ;] CELL-LITERAL RUN-LITERAL ;

EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
NCOMP-DISPATCH:DECL-CELL RESERVED-PTR-U8-CELL SOURCE-OWNER-A
NCOMP-DISPATCH:TARGET-DECL-CELL RESERVED-PTR-U8-CELL TARGET-OWNER-A

\ The active checker owner supplies the trusted-only query, not a source name.
: TICK-QUERY-READY? ( -- bool )
   SEAL-NDICT@ 0= if false exit then
   TARGET-OWNER-A @ dup 0= if drop true exit then
   SOURCE-OWNER-A @ = ;

: TICK-QUERY ( -- n )
   SOURCE-OWNER-A @ dup 0= if drop 0 exit then
   NCOMP-DISPATCH:DECL-TRUSTED-TICK-OFF + CELL-VIEW @ ;

\ The owner record's trusted-tick field holds a raw execution token.
CAST: TICK-ACTION ( n -- [ str -- bool ] )

: TRUSTED-TICK? ( -- bool )
   TICK-QUERY-READY? 0= if false exit then
   TICK-QUERY dup 0= if drop false exit then
   TICK-ACTION TOKEN$ swap execute ;

\ A tick runs nothing, so it has no depth gate.
: TICKED ( -- )
   s" '" OPERAND
   LOOKUP
   XT-GATE
   SCOPE-ENTRY-GUARD
   TRUSTED-TICK? if
      s" E-CAP-TRUSTED" REJECTION:OPEN
      TOKEN$ TICKS  s"  is a trust-boundary word, whose xt source may not take"
      SAY
      TOKEN$ REJECTION:AT  s" trusted only" SAY
      REJECTION:HELP  s" call a checked word that does what " SAY
      TOKEN$ TICKS  s"  does" SAY
      EXIT-STATUS:REJECTED REJECTION:STOP
   then ;

\ The run log sees the tick's target before the xt is pushed: the file may
\ execute it.
: TICK-OBSERVE ( -- n )
   REC @ XREF-START {: xt:n :}
   REC @ XREF-NULL - XREF-ADDR>INDEX {: idx:n :}
   idx RUN+
   xt ;

\ The hook sees the operand as the token.
: PUSH-XT ( -- )
   [: TICKED TICK-OBSERVE TOP-EV-TICK HOOK ;] CELL-LITERAL RUN-LITERAL ;

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

\ LOG-BUILD … ;LOG-BUILD logs the words run and the records declared between
\ them; the logs are read before ;LOG-BUILD releases them.
: LOG-BUILD ( -- )
   0 RUNS-N !  0 DECLS-N !  true LOGGING ! ;

: ;LOG-BUILD ( -- )
   false LOGGING !
   RUNS-RELEASE  0 RUNS-N !  0 RUNS-CAP !
   DECL-IDX-RELEASE  DECL-A-RELEASE  DECL-U-RELEASE  0 DECLS-N !  0 DECLS-CAP ! ;

: RUN-COUNT ( -- n ) RUNS-N @ ;

\ RUNS rejects a negative entry; the capacity past the count is not the log.
: RUN-RECORD@ ( n -- n )
   dup RUN-COUNT >= if E-LAYOUT-BOUNDS throw then
   RUNS @ ;

: DECL-COUNT ( -- n ) DECLS-N @ ;

\ Entry n of the declaration log: the record index and its token. n is below
\ DECL-COUNT.
: DECL@ ( n -- n str ) {: i:n :}
   i DECL-IDX @  i DECL-A @ i DECL-U @ >str ;

;package

\ Rows, scope and aliases go through their primitive writers. A writer rejects
\ silently, so the words here reject first where they can.

package INTERPRETER

private

: PKG-INDEX ( ^n -- n ) BYTE-VIEW NULL-PTR BYTE-VIEW - dbase@ - DREC / ;

public
\ Writes the record of package keyword t out of its context through its
\ help; the caller ends it.
: PKG-CONTEXT-REJECT ( str -- ) {: t:str :}
   s" E-PKG-CONTEXT" REJECTION:OPEN
   t TICKS  s"  is not allowed in this context" SAY
   t REJECTION:AT  s" out of context" SAY
   REJECTION:HELP  s" write " SAY  t TICKS  s"  only after " SAY
   s" package NAME" TICKS  s"  opens a package" SAY ;
\ Writes the record of a split-str token t anywhere but alone on the first
\ line of a file loaded by name, through its help; the caller ends it.
: SPLIT-STR-REJECT ( str -- ) {: t:str :}
   s" E-SPLIT-STR-LINE" REJECTION:OPEN
   t TICKS  s"  is not allowed in this context" SAY
   t REJECTION:AT  s" it takes effect only alone on " SAY
   s" the first line of a file loaded by name" SAY
   REJECTION:HELP  s" move " SAY  s" split-str" TICKS
   s" , in lower case, alone to the first line of " SAY
   s" a file loaded by name, or delete it" SAY ;
private

: PKG-CONTEXT-FAIL ( -- )
   TOKEN$ PKG-CONTEXT-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

EXPORT CHECKER-RESOLVE:PKG-OUTSIDE-REJECT
: PKG-OUTSIDE-FAIL ( -- )
   PKG-REC-CELL CELL@ XREF-ADDR>REC XREF-NAME$ TOKEN$ PKG-OUTSIDE-REJECT
   EXIT-STATUS:REJECTED REJECTION:STOP ;

EXPORT CHECK:REJECT-DUPLICATE
: PKG-DUPLICATE ( str -- )   dup REJECT-DUPLICATE ;

: PKG-NAME ( -- )
   TOKEN if exit then
   TOKEN$ NAME-MISSING ;

\ Rejects making the name s, placed at t, when no record is left.
: PKG-DICT-ROOM ( str str -- ) {: s:str t:str :}
   ndict@ DICT-CAP < if exit then
   s t REJECTION:DICTIONARY-FULL ;

: CODE-ROOM ( str str n -- ) {: s:str t:str used:n :}
   s STR:DATA s STR:LEN  t STR:DATA t STR:LEN  used CODE-SPACE:ROOM ;

: PKG-CODE-ROOM ( str str -- ) 0 CODE-ROOM ;

\ The operation at off in the owner record, 0 when there is no record or the
\ field is empty: the call is then skipped, not rejected.
: PKG-OPERATION ( ^u8 n -- n )
   {: rec:^ off:n :}
   rec 0= if 0 exit then
   rec off + CELL-VIEW @ ;

\ The owner record stores raw execution tokens; these views state the
\ signatures called here.
CAST: PKG-AS-ACTION ( n -- [ -- ] )
CAST: PKG-AS-NAME-ACTION ( n -- [ str -- ] )
CAST: PKG-AS-PACKAGE-ACTION ( n -- [ str str -- ] )

\ `package`, the section words and `;package` notify the target checker alone:
\ a loading checker is the target at once, the source owner only where none was.
: PKG-TARGET ( n -- n )
   TARGET-OWNER-A @ swap PKG-OPERATION ;

: PKG-NOTIFY ( n -- )
   PKG-TARGET {: xt:n :}
   xt 0= if exit then
   xt PKG-AS-ACTION execute ;

: PKG-NOTIFY-NAME ( str n -- ) {: t:str off:n :}
   off PKG-TARGET {: xt:n :}
   xt 0= if exit then
   TOKEN$ t xt PKG-AS-PACKAGE-ACTION execute ;

\ `using` notifies the active checker, then the target checker unless its
\ operation is the same one. Each records the name at the using depth before it
\ grows.
: PKG-NOTIFY-USING ( -- )
   SOURCE-OWNER-A @ NCOMP-DISPATCH:DECL-USING-OFF PKG-OPERATION
   {: own:n :}
   own 0<> if TOKEN$ own PKG-AS-NAME-ACTION execute then
   NCOMP-DISPATCH:DECL-USING-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   target own = if exit then
   TOKEN$ target PKG-AS-NAME-ACTION execute ;

\ An actual export names the source checker's declaration callback. A direct
\ call only adds a checker row and has no engine alias to bind.
: PKG-CHECK-EXPORT ( -- )
   SOURCE-OWNER-A @ NCOMP-DISPATCH:DECL-EXPORT-OFF PKG-OPERATION
      {: xt:n :}
   xt 0= if
      s" E-DISPATCH-UNSET" REJECTION:OPEN
      TOKEN$ TICKS  s"  has no checker to declare its export to" SAY
      TOKEN$ REJECTION:AT  s" checker-export dispatch unset" SAY
      REJECTION:HELP  s" load the checker before any export" SAY
      EXIT-STATUS:REJECTED KEEP-DIE
   then
   TOKEN$ xt PKG-AS-NAME-ACTION execute ;

: PKG-ROW ( -- ^n )
   TOKEN$ BOOT:NAMESPACE-WL WL-PROBE ;

\ A sealed package ends the process when named again; a package is sealed only
\ by the last file that opens it.
: PKG-SEALED? ( -- bool )
   PKG-ROW {: row:^ :}
   row XREF-FOUND? 0= if false exit then
   row XREF:SEALED? ;

: PKG-SEAL-GUARD ( -- )
   PKG-SEALED? 0= if exit then
   TOKEN$ TOKEN$ SEALED-FAIL ;

: PKG-POLICY-GUARD ( -- )
   POLICY-NDICT-CELL CELL@ {: seal:n :}
   seal 0= if exit then
   PKG-ROW {: row:^ :}
   row XREF-FOUND? 0= if exit then
   row PKG-INDEX seal >= if exit then
   s" E-POLICY" REJECTION:OPEN
   s" package " SAY  TOKEN$ TICKS
   s"  predates the seal, so sealed source cannot reopen it" SAY
   TOKEN$ REJECTION:AT  s" opened before the seal" SAY
   REJECTION:HELP
   s" give the sealed source's package a name of its own" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: PKG-REOPEN ( ^n -- n ) {: row:^ :}
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
   PKG-ROW {: row:^ :}
   row XREF-FOUND? if row PKG-REOPEN exit then
   OVERLAY-GUARD
   TOKEN$ TOKEN$ PKG-DICT-ROOM
   TOKEN$ TOKEN$ PKG-CODE-ROOM
   TOKEN$ true namespace-record ;

\ namespace-record's name rejections, as E-COMP-STATE: a colon at either edge
\ and a second colon.
: PKG-PATH-CK ( str -- )
   {: s:str :}
   s 0 COLON-AT {: q:n :}
   q 0 < if exit then
   q 0=  q 1+ s STR:LEN =  or if E-COMP-STATE throw then
   s q 1+ COLON-AT 0 < 0= if E-COMP-STATE throw then ;

public
\ A file object's link records package s, which must lie in the caller's
\ buffer, with its private wordlist right after the public one when the flag is
\ set.
: PACKAGE-RECORD ( str bool -- n )
   {: s:str own:bool :}
   s STR:LEN 1 < if E-COMP-STATE throw then
   s PKG-PATH-CK
   s s PKG-DICT-ROOM
   s s PKG-CODE-ROOM
   s BOOT:NAMESPACE-WL WL-PROBE XREF-FOUND? if
      s PKG-DUPLICATE
   then
   s own namespace-record ;
private

EXPORT CHECKER-RESOLVE:PART-PATH

\ Inside package A the token cells name `A:<token>`.
: PKG-PATH ( -- )
   TOKEN$ PART-PATH {: p:str :}
   p STR:LEN TKL-CELL CELL!  p STR:DATA TKA-CELL ADDR! ;

EXPORT CHECKER-RESOLVE:PART-GUARD

\ The using depth it opens at is the one `;package` restores, a part's its
\ package's.
: PKG-PACKAGE ( -- )
   PART-GUARD
   PKG-NAME
   TOKEN$ {: t:str :}
   PKG-PATH
   PKG-POLICY-GUARD
   t NCOMP-DISPATCH:DECL-PACKAGE-OFF PKG-NOTIFY-NAME
   PKG-SEAL-GUARD
   PKG-ENSURE {: ix:n :}
   USE-DEPTH-CELL CELL@ USE-PKG-SAVE-CELL CELL!
   ix get-current package-scope!
   PKG-PRI-CELL CELL@ CUR-CELL CELL!
   0 PKG-OUTSIDE-CELL CELL! ;

: PKG-END-PACKAGE ( -- )
   PKG-PUB-CELL CELL@ 0= if PKG-CONTEXT-FAIL then
   PKG-OUTSIDE-CELL CELL@ 0<> if PKG-OUTSIDE-FAIL then
   NCOMP-DISPATCH:DECL-END-PACKAGE-OFF PKG-NOTIFY
   PKG-PARENT-CELL CELL@ CUR-CELL CELL!
   ENCLOSING:REC CELL@ {: outer:n :}
   outer 0= if -1 0 else outer XREF-ADDR>INDEX ENCLOSING:PARENT CELL@ then
   package-scope!
   USE-PKG-SAVE-CELL CELL@ USE-DEPTH-CELL CELL! ;

: PKG-SEAL ( -- )
   PKG-PUB-CELL CELL@ 0= if PKG-CONTEXT-FAIL then
   PKG-REC-CELL CELL@ XREF-ADDR>REC XREF:SEAL ;

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
      cur CUR-CELL CELL!
   then
   NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF PKG-NOTIFY ;

: USING-UNKNOWN ( -- )
   s" E-USING-UNKNOWN" REJECTION:OPEN
   s" no package is named " SAY  TOKEN$ TICKS
   TOKEN$ REJECTION:AT  s" unknown package" SAY
   REJECTION:HELP  s" load the file that defines " SAY  TOKEN$ TICKS
   s"  before this using, or check its spelling" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

EXPORT CHECKER-RESOLVE:HEAD-ROW

: PKG-USED-WID ( -- n )
   TOKEN 0= if s" using" NAME-MISSING then
   TOKEN$ 0 COLON-AT 0 >= if
      TOKEN$ REJECTION:USING-BAD-NAME  EXIT-STATUS:REJECTED REJECTION:STOP
   then
   TOKEN$ HEAD-ROW {: row:^ :}
   row XREF-FOUND? 0= if USING-UNKNOWN then
   row XREF-PKG-PUBLIC ;

: USING-OVERFLOW ( -- )
   s" E-USING-OVERFLOW" REJECTION:OPEN
   s" using " SAY  TOKEN$ TICKS
   s"  opens more usings than may be open at once" SAY
   TOKEN$ REJECTION:AT  s" one using too many" SAY
   USE-MAX s" " REJECTION:LIMIT
   REJECTION:HELP  s" close a using with " SAY  s" ;using" TICKS
   s"  before opening another" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The using depth the buffer may close down to: its entry depth.
variable USE-FLOOR

: PKG-USING ( -- )
   PKG-USED-WID {: wid:n :}
   USE-DEPTH-CELL CELL@ {: d:n :}
   d USE-MAX >= if USING-OVERFLOW then
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
      TOKEN$ REJECTION:USING-UNBALANCED  EXIT-STATUS:REJECTED REJECTION:STOP
   then
   PKG-PUB-CELL CELL@ 0<>  d USE-PKG-SAVE-CELL CELL@ <=  and if
      TOKEN$ s" package" REJECTION:USING-OUTER  EXIT-STATUS:REJECTED REJECTION:STOP
   then
   d USE-FLOOR @ <= if
      TOKEN$ s" file" REJECTION:USING-OUTER  EXIT-STATUS:REJECTED REJECTION:STOP
   then
   d 1- USE-DEPTH-CELL CELL! ;

: PKG-TAIL ( -- str )
   TOKEN$ {: t:str :}
   t 0 COLON-AT {: q:n :}
   q 1 <  q 1+ t STR:LEN >=  or if t exit then
   t STR:DATA q 1+ ZPTR+  t STR:LEN q - 1- >str ;

EXPORT CHECKER-RESOLVE:EXPORT-PRIVATE
EXPORT CHECK:EXPORT-SYM
EXPORT CHECK:EXPORT-MISS
EXPORT CHECK:EXPORT-UNSAFE

\ The source must not be internal: an alias would carry its body past the
\ interpret gate without the DNAME-INT mark, so the gate's rejection is made
\ here. DEF-TKA and DEF-TKL take the operand, which the rejections after them
\ name.
: PKG-EXPORT-SOURCE ( -- ^n )
   TOKEN$ FIND-SCOPE {: found:^ :}
   found XREF-FOUND? if found else TOKEN$ EXPORT-PRIVATE then
   {: src:^ :}
   src XREF-FOUND? 0= if TOKEN$ TOKEN$ EXPORT-SYM drop EXPORT-MISS then
   src XREF-FLAGS DNAME-INT and 0<> if TOKEN$ EXPORT-UNSAFE then
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   src ;

\ The record index of the source's does> clause, which its export carries, or
\ -1 for a source that is not a definer.
: PKG-CLAUSE ( ^n -- n ) {: src:^ :}
   src PKG-INDEX {: k:n :}
   k XREF-DOES-COMPANION? if k 1+ exit then
   -1 ;

\ The clause takes the slot above the export's own, and its name must not be
\ live in the current wordlist: a clash is rejected at the operand's tail.
: PKG-CLAUSE-ROOM ( n -- ) {: k:n :}
   ndict@ 1+ DICT-CAP < 0= if
      PKG-TAIL CLAUSE-NAME TOKEN$ REJECTION:DICTIONARY-FULL
   then
   k XREF-REC XREF-NAME$ get-current WL-PROBE XREF-FOUND? if
      PKG-TAIL DOES-CLAUSE:SUFFIX$ PKG-TAIL REJECTION:CLAUSE-TAKEN
      EXIT-STATUS:REJECTED REJECTION:STOP
   then ;

: PKG-CLAUSE-ALIAS ( n -- ) {: k:n :}
   k XREF-REC XREF-NAME$ k get-current alias-record ;

\ A file object's link publishes name s, which must lie in the caller's
\ buffer, for record src in wordlist wid.
: PUBLISH-ALIAS ( str n n -- )
   {: s:str src:n wid:n :}
   s STR:LEN 1 <  wid 0 <  or if E-COMP-STATE throw then
   SEAL-NDICT@ 0<>  wid XREF-WID-PROTECTED?  and if E-COMP-STATE throw then
   src 0 <  src ndict@ >=  or if E-COMP-STATE throw then
   src XREF-REC {: rec:^ :}
   rec XREF-WORDLIST {: swid:n :}
   swid XREF:PACKAGE-WL?  swid XREF:RETIRED-WL?  or if E-COMP-STATE throw then
   rec XREF-FLAGS DNAME-INT and 0<> if E-COMP-STATE throw then
   s s PKG-DICT-ROOM
   s s PKG-CODE-ROOM
   s wid WL-PROBE XREF-FOUND? if
      s PKG-DUPLICATE
   then
   s src wid alias-record ;

\ It names the `EXPORT` token, before its operand is read.
: EXPORT-NO-PACKAGE ( -- )
   s" E-EXPORT-NO-PACKAGE" REJECTION:OPEN
   s" EXPORT" TICKS  s"  publishes a word from a package, and no package is open here" SAY
   TOKEN$ REJECTION:AT  s" outside a package" SAY
   REJECTION:HELP  s" write " SAY  s" EXPORT NAME" TICKS
   s"  in a package, after " SAY  s" package P" TICKS
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ In a package, publish an existing word under its tail into the current
\ wordlist: its code, its immediate, wide and certified-input bits, and a
\ does> definer's clause in the slot above.
: PKG-EXPORT ( -- )
   PKG-PUB-CELL CELL@ 0= if EXPORT-NO-PACKAGE then
   PKG-NAME
   OVERLAY-GUARD
   TOKEN$ {: operand:str :}
   get-current XREF:SEALED-TARGET? if
      operand dup SEALED-FAIL
   then
   PKG-EXPORT-SOURCE {: src:^ :}
   PKG-TAIL {: tail:str :}
   tail operand PKG-DICT-ROOM
   tail get-current WL-PROBE XREF-FOUND? if
      tail PKG-DUPLICATE
   then
   src PKG-CLAUSE {: clause:n :}
   clause 0 >= if clause PKG-CLAUSE-ROOM then
   \ Both names an export writes must fit before its checker declaration becomes pending.
   tail tail PKG-CODE-ROOM
   clause 0 >= if
      clause XREF-REC XREF-NAME$ dup tail STR:LEN CODE-SPACE:SPAN CODE-ROOM
   then
   PKG-CHECK-EXPORT
   tail src PKG-INDEX get-current alias-record
   clause 0 >= if clause PKG-CLAUSE-ALIAS then ;

\ `host` or `target` at top level is the section word; with a word of that name in scope the
\ token could mean either, so it is rejected: the word is renamed, or called qualified if public.
public
\ Writes the record of section word t, which a word in scope also names,
\ through its help; the caller ends it.
: SECTION-REJECT ( str -- ) {: t:str :}
   s" E-SECTION-WORD" REJECTION:OPEN
   t TICKS
   s"  is the section word, and a word of that name is in scope" SAY
   t REJECTION:AT  s" section word or word in scope" SAY
   REJECTION:HELP
   s" rename the word, or call it by its qualified name if it is public" SAY ;
private

EXPORT CHECK:CHECKER-SPLIT-STR
EXPORT CHECK:CHECKER-TYPE-SPAN-STEP
EXPORT CHECK:NAME-IMM-READS
EXPORT CHECK:TYPE-SPAN-END?
EXPORT CHECKER-RESOLVE:PUBLIC-SECTION
EXPORT CHECKER-RESOLVE:PRIVATE-SECTION
EXPORT CHECKER-RESOLVE:GLOBAL-SECTION
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
   outer-rec 0<> if outer-parent CUR-CELL CELL! else
      rec 0<> if parent CUR-CELL CELL! then
   then
   q catch {: rc:n :}
   outer-rec outer-parent rec parent cur floor PKG-RECOVER
   outside PKG-OUTSIDE-CELL CELL!
   rc 0<> if rc throw then ;

;package

\ A definition head rejects in the engine's order and with its text: a
\ definition writer's own rejection exits without a word.

require src/lib/string.f
require src/lib/source-syntax.f
require src/core/checker.f
require src/core/check-hook.f
require src/core/does-clause.f
require src/compiler/fixed-body.f

package INTERPRETER

private

EXPORT CHECK:IMMEDIATE-CHECK
EXPORT CHECK:CHILD
EXPORT CHECK:STORAGE-LINE
EXPORT CHECK:CELL-LINE
EXPORT CHECK:EFFECT-RULES?
EXPORT CHECKER-RESOLVE:REJECT-QUALIFIED
EXPORT CHECKER-PREFIX:BIND?

\ The capture reads locals before syntax. These cells describe lexical
\ lifetime only.
DYNAMIC-BUFFER BODY-LOCAL-DEPTH n
\ Each local's bare name ends at its LOCAL-END offset in LOCAL-NAMES, which
\ holds the names end to end in binding order.
DYNAMIC-BUFFER LOCAL-NAMES u8
DYNAMIC-BUFFER LOCAL-END n

: DEF-TRUST-SIG ( str -- ) trust-sig! ;

\ The block BODY-ITEMS describes. Each opener zeroes CAP-CELL, so a
\ definition's first item reserves the block again, as it must after a capture
\ released it.
DYNAMIC-BUFFER ITEM-CELLS n

: ITEMS-RESET ( -- )
   0 BODY-ITEMS:BASE-CELL CELL!  0 BODY-ITEMS:CAP-CELL CELL!
   0 BODY-ITEMS:COUNT-CELL CELL!  0 BODY-ITEMS:EFFECT-CELL CELL!
   0 BODY-ITEMS:LOCALS-CELL CELL! ;

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
\ INTERPRETER's bare DEF-CLOSE is its private def-close row: only a qualified
\ call reaches this. x86-64's def-close leaves the locals count, cleared here.
: DEF-CLOSE ( -- )
   def-close  0 LOCN-CELL CELL!  ITEMS-RESET ;
private

\ The engine's cells hold the compiler entries, the body hook and the checker's
\ declaration operations as raw execution tokens.
CAST: AS-COMPILE ( n -- [ str -- ] )
CAST: AS-COMPILE-FIXED ( n -- [ n n COMP:body -- ] )
CAST: AS-BODY-HOOK ( n -- [ -- ^u8 bool ] )
CAST: AS-DECLARE ( n -- [ str str -- ] )

\ A head with no compiler (COMP-CELL 0), or a body token or fixed body whose
\ latch is 0, rejects. `immediate` passes.
: DEF-NO-COMPILER ( -- )
   s" E-NO-COMPILER" REJECTION:OPEN
   s" no compiler is latched for the definition at `" REJECTION:EMIT-STR
   TOKEN$ REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   TOKEN$ REJECTION:AT  s" nothing can compile this" REJECTION:EMIT-STR
   REJECTION:HELP
   s" load the native compiler, as the boot prefix does, before any definition"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The checker declaration xt given takes the name s and the signature given.
: DEF-DECLARE-NAME ( str str n -- ) {: s:str sig:str xt:n :}
   s sig xt AS-DECLARE execute ;

\ DEF-DECLARE-NAME with the name as the capture spells it, qualified.
: DEF-DECLARE ( str n -- ) {: sig:str xt:n :}
   DEF-CAPTURED-NAME sig xt DEF-DECLARE-NAME ;

public
\ The source ends inside the definition of s, whose name stands at the source
\ token t. It is written through its help, and the caller stops it.
: UNCLOSED-REJECT ( str str -- ) {: s:str t:str :}
   s" E-VS-UNTERMINATED-DEFINITION" REJECTION:OPEN
   s" the source ends inside the definition of `" REJECTION:EMIT-STR
   s REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   t REJECTION:AT  s" never closed" REJECTION:EMIT-STR
   REJECTION:HELP
   s" end the definition of `" REJECTION:EMIT-STR  s REJECTION:EMIT-STR
   s" ` with `;` before the source ends" REJECTION:EMIT-STR ;

\ The source ends inside the locals group the source token t, its `{:`,
\ opens. It is written through its help, and the caller stops it.
: GROUP-REJECT ( str -- ) {: t:str :}
   s" E-UNCLOSED-LOCALS" REJECTION:OPEN
   s" the locals group `{:` opens is never closed" REJECTION:EMIT-STR
   t REJECTION:AT  s" never closed" REJECTION:EMIT-STR
   REJECTION:HELP
   s" close the group with `:}` before the body" REJECTION:EMIT-STR ;
private

\ The rejection marks the opener's token in the source, the name a colon
\ definition captures.
: DEF-ENDED ( -- )
   PENDTKA-CELL CELL@ INP-CELL CELL!
   INP-CELL ADDR@ {: p:^ :}
   p INE-CELL ADDR@ WORD-END p - {: pu:n :}
   DEF-CAPTURED-NAME p pu >str UNCLOSED-REJECT
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ body-capture grows the block when the text would pass its capacity, so an
\ address into the text is formed after the capture, never kept across it.
: DEF-CAPTURE ( str -- ) body-capture ;

: ITEM-ROOM ( -- )
   BODY-ITEMS:CAP-CELL CELL@ {: cap:n :}
   BODY-ITEMS:COUNT-CELL CELL@ cap < if exit then
   cap 0= if BODY-ITEMS:FIRST-CAP else cap 2 * then {: grown:n :}
   grown BODY-ITEMS:ROW * ITEM-CELLS-RESERVE
   grown BODY-ITEMS:CAP-CELL CELL!
   0 ITEM-CELLS NULL-PTR - BODY-ITEMS:BASE-CELL CELL! ;

\ Marks a run whose bytes the buffer read lacks as built at the token read last,
\ if the definition opened there; PENDTKA-CELL is set after the name's run
\ (off 0).
: PLACE-RUN ( n -- )
   {: off:n :}
   BODY-TEXT:ROWS-CELL CELL@ 1 - {: i:n :}
   i 0 < if exit then
   BODY-TEXT:BASE-CELL ADDR@ BODY-TEXT:CAP-CELL CELL@ +
   i BODY-SOURCE:ROW * + {: row:^ :}
   row 0 FR@ off <> if exit then
   row 8 FR@ BODY-SOURCE:NONE <> if exit then
   TKA-CELL ADDR@ SRCLOC:INB-CELL ADDR@ INE-CELL ADDR@
   {: t:^ b:^ e:^ :}
   PENDTKA-CELL ADDR@ {: p:^ :}
   off 0 >  p b - 0 <  e p - 0 <= or  and if exit then
   t b - 0 <  e t - 0 <= or if exit then
   t b - BODY-SOURCE:BUILT row 8 FR! ;

\ Appends an item and captures its spelling, which the row's off and len span.
: ITEM+ ( n n n str -- ) {: kind:n a:n b:n s:str :}
   BODYLEN-CELL CELL@ {: off:n :}
   ITEM-ROOM
   s DEF-CAPTURE
   off PLACE-RUN
   BODY-ITEMS:COUNT-CELL CELL@ {: i:n :}
   kind i BODY-ITEMS:KIND ITEM!  a i BODY-ITEMS:A ITEM!  b i BODY-ITEMS:B ITEM!
   off i BODY-ITEMS:OFF ITEM!  s STR:LEN i BODY-ITEMS:LEN ITEM!
   i 1 + BODY-ITEMS:COUNT-CELL CELL! ;

\ The rejections name `:` and `kernel:` as spelled, `trusted:` as the reader
\ keyword it is.
: DEF-NAME ( bool -- ) {: trusted:bool :}
   trusted if s" trusted:" OPERAND  s" trusted:" TOKEN$ REJECTION:NOT-A-NAME exit then
   TOKEN$ {: kw:str :}
   TOKEN 0= if kw NAME-MISSING then
   kw TOKEN$ REJECTION:NOT-A-NAME ;

\ A head with no row makes a top-level package with a public wordlist
\ and no private one.
: DEF-NAMESPACE ( str -- n ) {: s:str :}
   s HEAD-ROW {: row:^ :}
   row XREF-FOUND? if row XREF-PKG-PUBLIC exit then
   s TOKEN$ PKG-DICT-ROOM
   s s PKG-CODE-ROOM
   s false namespace-record XREF-REC XREF-PKG-PUBLIC ;

\ Checked where source names a definition, not in DEF-RECORD, which a file
\ object's link replays.
: DEF-SEAL-GUARD ( n str -- n ) {: wid:n s:str :}
   wid XREF:SEALED-TARGET? if s s SEALED-FAIL then
   wid ;

\ A storage definer's written name is rejected at itself where
\ DEF-QUALIFY-NAME would reject the words it names; no namespace row is made.
\ A second colon is left to DEF-QUALIFY-NAME.
: DEF-SEAL-NAME ( str -- ) {: s:str :}
   s 0 COLON-AT {: q:n :}
   q 1 <  q 1+ s STR:LEN >=  or if get-current s DEF-SEAL-GUARD drop exit then
   s q 1+ COLON-AT 0 >= if exit then
   s STR:DATA q >str HEAD-ROW {: row:^ :}
   row XREF-FOUND? if row XREF-PKG-PUBLIC else 0 then
   XREF:SEALED-TARGET? if s s SEALED-FAIL then ;

\ Records are placed at the name when a source holds it, since the definer may
\ have read past it; a name the definer built is placed at the token.
: DEF-NAME-PLACE ( str -- str ) {: s:str :}
   s STR:DATA REJECTION:PLACES? if s else TOKEN$ then ;

\ The tail the record is named and the wordlist it goes into. DEF-TKA and
\ DEF-TKL take the name token, which a `does>` clause's rejections name. A bare name
\ goes into the current wordlist; the qualifier is the first colon at neither
\ edge (`:a:b` is bare), and a second colon after it is rejected.
: DEF-QUALIFY-NAME ( str -- str n ) {: s:str :}
   s 0 COLON-AT {: q:n :}
   q 1 <  q 1+ s STR:LEN >=  or if s get-current s DEF-SEAL-GUARD exit then
   s q 1+ COLON-AT 0 >= if s  s DEF-NAME-PLACE REJECT-QUALIFIED then
   s STR:DATA q 1+ ZPTR+  s STR:LEN q - 1- >str
   s STR:DATA q >str DEF-NAMESPACE
   dup XREF:SEALED-TARGET? if s s SEALED-FAIL then ;

: DEF-QUALIFY ( -- str n )
   OVERLAY-GUARD
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   TOKEN$ DEF-QUALIFY-NAME ;

: DEF-NAME-ONLY-KEYWORDS ( -- str )
   s" 2constant to require include sumtype variant ;variant ;sumtype" ;

: DEF-OP-KEYWORDS ( -- str )
   s" + - * / mod and or xor lshift rshift dup drop swap over nip = <> < > <= >= 1+ 1- 0= 0< negate invert f+ f- f* f/" ;

: DEF-P2-KEYWORDS ( -- str )
   s" tuck rot -rot 2dup 2drop 2swap 2over 2>r 2r> 2r@ @ !" ;

: DEF-KEYWORD-END ( str n -- n ) {: t:str s:n :}
   s begin
      dup t STR:LEN < if t STR:DATA over + c@ BLANK <> else false then
   while 1+ repeat ;

: DEF-IN-TABLE? ( str str -- bool ) {: name:str t:str :}
   0 begin dup t STR:LEN < while
      {: s:n :}
      t s DEF-KEYWORD-END {: e:n :}
      name  t STR:DATA s ZPTR+ e s - >str  FOLDED= if true exit then
      e 1+
   repeat
   drop false ;

: DEF-ROW-KEYWORD? ( str -- bool ) {: s:str :}
   s DEF-OP-KEYWORDS DEF-IN-TABLE? if true exit then
   s DEF-P2-KEYWORDS DEF-IN-TABLE? ;

: DEF-NAME-KEYWORD? ( str -- bool ) {: s:str :}
   s DEF-ROW-KEYWORD? if true exit then
   s DEF-NAME-ONLY-KEYWORDS DEF-IN-TABLE? ;

\ Every engine's policy writer asks this captured interpreter once per package,
\ before setting its admission bit.
public
: POLICY-KEYWORD ( n -- str bool ) {: wid:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:^ :}
      rec XREF-WORDLIST wid = if
         rec XREF-NAME$ {: name:str :}
         name NAME-KEYWORD? name DEF-ROW-KEYWORD? or if
            name true unloop exit
         then
      then
   loop
   s" " false ;

: INSTALL-POLICY ( -- )
   ['] POLICY-KEYWORD data-base POLICY-ABI:KEYWORD-CELL + xt! ;
private

\ No definition takes a keyword's name; a sealed design also keeps every
\ other dispatch-row name out, before the dictionary's duplicate checks.
: DEF-WALL ( str str -- ) {: s:str t:str :}
   s t DEF-NAME-REJECT? if EXIT-STATUS:REJECTED REJECTION:STOP then
   POLICY-NDICT-CELL CELL@ 0<> if
      s DEF-NAME-KEYWORD? if s t POLICY-REJECT then
   then ;

\ A signature starts at the first byte past the blanks when that byte is `(`,
\ whole token or not: where it starts, and whether one does.
: DEF-SIG-START ( -- ^u8 bool )
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:^ e:^ :}
   p e BLANKS-END {: s:^ :}
   s e < if s c@ OPEN-COMMENT = else false then {: open:bool :}
   s open ;

: DEF-SIG-END ( ^u8 -- ^u8 bool ) {: s:^ :}
   INE-CELL ADDR@ {: e:^ :}
   s begin dup e < if dup c@ CLOSE-COMMENT <> else false then while 1 + repeat
   {: c:^ :}
   c e < if c 1 + true exit then
   e false ;

: DEF-SIG-PASS ( ^u8 ^u8 -- ) {: s:^ end:^ :}
   end INP-CELL ADDR!
   s end s - >str DEF-CAPTURE ;

\ trust-sig! takes the text inside the parentheses. The inner length is the
\ engine's, two less than the whole: one less than the text for a signature
\ the input ends inside.
: DEF-SIG-INNER ( ^u8 ^u8 -- ) {: s:^ end:^ :}
   s 1 + end s - 2 - >str DEF-TRUST-SIG ;

: DEF-SIG-TAKE ( ^u8 ^u8 -- ) {: s:^ end:^ :}
   s end DEF-SIG-INNER
   s end DEF-SIG-PASS ;

\ `:` takes one if it is there, to the input's end if it is not closed.
: DEF-MAYBE-SIG ( -- )
   DEF-SIG-START {: s:^ open:bool :}
   open 0= if exit then
   s DEF-SIG-END drop {: end:^ :}
   s end DEF-SIG-TAKE ;

public
\ Writes the record of t, a definition's name or `does>`, with no closed
\ signature after it, through its help; the caller ends it.
: SIGNATURE-REJECT ( str -- ) {: t:str :}
   s" E-MISSING-SIGNATURE" REJECTION:OPEN
   t TICKS  s"  has no closed `( -- )` signature" SAY
   t REJECTION:AT  s" no closed signature after this" SAY
   REJECTION:HELP
   s" write the stack effect after the name, as `( n -- n )`, with its closing `)`"
   SAY ;
private

: DEF-SIG-BAD ( -- )
   TOKEN$ SIGNATURE-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

\ A head's signature that the input ends before or inside is the source ending
\ inside the definition.
: DEF-SIG-SPAN ( bool -- ^u8 ^u8 ) {: head:bool :}
   DEF-SIG-START {: s:^ open:bool :}
   head s INE-CELL ADDR@ = and if DEF-ENDED then
   open 0= if DEF-SIG-BAD then
   s DEF-SIG-END {: end:^ closed:bool :}
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
   s" E-DISPATCH-UNSET" REJECTION:OPEN
   s" the native compiler dispatch is unset at `" REJECTION:EMIT-STR
   TOKEN$ REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   TOKEN$ REJECTION:AT  s" no compiler latched" REJECTION:EMIT-STR
   REJECTION:HELP
   s" load the native compiler, as the boot prefix does, before any definition"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED KEEP-DIE ;

\ The room, code room and protected-wordlist tests of a name past the wall and
\ the duplicate probe, each rejection placed at t, then the open.
: DEF-OPEN-RECORD ( str str n n -- )
   {: s:str t:str wid:n kind:n :}
   ndict@ DICT-CAP >= if s t REJECTION:DICTIONARY-FULL then
   s t 1 CODE-ROOM
   SEAL-NDICT@ 0<>  wid XREF-WID-PROTECTED?  and if s t SEALED-FAIL then
   kind DKIND:MASK and {: dk:n :}
   \ A value or an address in a target section stays unmarked: it answers on the host.
   SECTION-CELL CELL@ 0<>  dk DKIND:ADDR <> and  dk DKIND:VAL <> and
   if kind DNAME-TARGET or else kind then {: k:n :}
   \ def-open names no word for a protected wordlist; its record stays unpublished, at ndict@.
   s wid k def-open  ndict@ t DECL+ ;

public
\ The name s passes the wall, duplicate, room, code room and protected-wordlist
\ tests, each rejection placed at t. kind: 0 (compiled), DKIND:ADDR, VAL or CAST.
: DEF-RECORD ( str str n n -- )
   {: s:str t:str wid:n kind:n :}
   s t DEF-WALL
   s wid WL-PROBE XREF-FOUND? if
      t PKG-DUPLICATE
   then
   s t wid kind DEF-OPEN-RECORD ;
private

\ The capture seeded with the name, the spelling the checker is told.
: DEF-SEED ( -- )
   ITEMS-RESET
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE ;

: DEF-START ( n -- )
   {: kind:n :}
   DEF-SEED
   DEF-QUALIFY {: name:str wid:n :}
   name TOKEN$ wid kind DEF-RECORD ;

\ On true the body hook has installed the definition, the source resumes past
\ its `;` and the record closes.
: DEF-BODY-HOOK? ( -- bool )
   NCOMP-DISPATCH:BODY-HOOK-CELL CELL@ {: xt:n :}
   xt 0= if false exit then
   xt AS-BODY-HOOK execute {: resume:^ installed:bool :}
   installed 0= if false exit then
   resume INP-CELL ADDR!
   DEF-CLOSE
   true ;

: DEF-HEAD ( bool -- ) {: trusted:bool :}
   NCOMP-DISPATCH:COMP-CELL CELL@ 0= if
      DEF-NO-COMPILER
   then
   trusted DEF-NAME
   0 DEF-START
   0 LOCN-CELL CELL!
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
\ comes first, so a literal with no closing quote or with a bad escape rejects
\ as the top-level keyword does, before any of it is captured. A counted
\ string's length waits for `;`, which compiles the body.
: DEF-TEXT ( -- )
   TEXT {: t:str :}
   t STR:DATA t STR:LEN 1+ >str DEF-CAPTURE ;

: DEF-ESC-TEXT ( -- )
   ESC-SCAN drop {: s:^ q:^ :}
   q PAST
   s q s - 1+ >str DEF-CAPTURE ;

: DEF-STRING-TEXT ( -- )
   S\" s\q" TOKEN-IS? if DEF-TEXT exit then
   S\" c\q" TOKEN-IS? if DEF-TEXT exit then
   S\" .\q" TOKEN-IS? if DEF-TEXT exit then
   S\" s\\\q" TOKEN-IS? if DEF-ESC-TEXT exit then
   S\" c\\\q" TOKEN-IS? if DEF-ESC-TEXT exit then
   S\" .\\\q" TOKEN-IS? if DEF-ESC-TEXT then ;

: LOCAL-START ( n -- n ) {: idx:n :}
   idx 0= if 0 exit then
   idx 1- LOCAL-END @ ;

\ The capture records each `{:` declaration here; the compiler reads it from the
\ body text at `;`.
: BODY-LOCAL? ( -- bool )
   TOKEN$ {: t:str :}
   LOCN-CELL CELL@ 0 ?do
      i LOCAL-END @ i LOCAL-START - t STR:LEN = if
         t  i LOCAL-START LOCAL-NAMES t STR:LEN >str  STR:=
         if true unloop exit then
      then
   loop
   false ;

: BODY-LOCAL-ONE ( -- )
   TOKEN$ {: t:str :}
   LOCN-CELL CELL@ {: idx:n :}
   t 0 COLON-AT {: colon:n :}
   colon 0 < if t STR:LEN else colon then {: len:n :}
   idx LOCAL-START {: at:n :}
   idx 1+ BODY-LOCAL-DEPTH-RESERVE  idx 1+ LOCAL-END-RESERVE
   at len + LOCAL-NAMES-RESERVE
   at len + idx LOCAL-END !
   t STR:DATA  at LOCAL-NAMES  len BYTE-COPY
   CAPDEPTH-CELL CELL@ idx BODY-LOCAL-DEPTH !
   idx 1+ LOCN-CELL CELL! ;

\ The `{:` of the locals group the source ended inside, NULL-PTR while it has
\ not. A REPL line's end leaves the group to the next line; RUN rejects it.
TYPED-VARIABLE GROUP-OPEN ^u8

: BODY-LOCAL-GROUP ( -- )
   TOKEN$ STR:DATA {: at:^ :}
   begin TOKEN while
      TOKEN$ DEF-CAPTURE
      s" :}" TOKEN-IS? if exit then
      BODY-LOCAL-ONE
   repeat
   at GROUP-OPEN ! ;

\ A later rejected token can restore the count without reconstructing any row.
: BODY-BLOCK-DROP ( -- )
   begin
      LOCN-CELL CELL@ 0 > if
         LOCN-CELL CELL@ 1- BODY-LOCAL-DEPTH @ CAPDEPTH-CELL CELL@ >=
      else false then
   while
      LOCN-CELL CELL@ 1- LOCN-CELL CELL!
   repeat ;

: BODY-BLOCK-STEP ( -- )
   LOCN-CELL CELL@ 0= if exit then
   TOKEN$ SOURCE-SYNTAX:BLOCK-OPENER? if
      CAPDEPTH-CELL CELL@ 1+ CAPDEPTH-CELL CELL! exit
   then
   s" else" TOKEN-IS? if BODY-BLOCK-DROP exit then
   TOKEN$ SOURCE-SYNTAX:BLOCK-CLOSER? if
      BODY-BLOCK-DROP  CAPDEPTH-CELL CELL@ 1- CAPDEPTH-CELL CELL!
   then ;

\ The body vocabulary the capture admits.
\ `;` is handled before the capture.
: BODY-POLICY ( -- )
   BODY-LOCAL? if exit then
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   range if NUMBER-RANGE then
   POLICY-NDICT-CELL CELL@ 0= if exit then
   num if exit then
   TOKEN$ FIND-SCOPE {: rec:^ :}
   rec XREF-FOUND? if TOKEN$ rec POLICY-CHECK-REC exit then
   TOKEN$ FIND {: used:^ :}
   used XREF-FOUND? if TOKEN$ used POLICY-CHECK-REC exit then
   s" if" TOKEN-IS? if exit then
   s" else" TOKEN-IS? if exit then
   s" then" TOKEN-IS? if exit then
   S\" s\q" TOKEN-IS? if exit then
   S\" s\\\q" TOKEN-IS? if exit then
   s" [package]" TOKEN-IS? if exit then
   s" [type]" TOKEN-IS? if exit then
   s" {:" TOKEN-IS? if exit then
   TOKEN$ TOKEN$ POLICY-REJECT ;

: DEF-PREFLIGHT ( -- )
   BODY-TEXT:BASE-CELL ADDR@ BODYLEN-CELL CELL@ >str
   TOKEN$ TRUSTED-CELL CELL@ 0<> IMMEDIATE-CHECK ;

\ The immediate the token names, the raw tokens it reads, and true when the
\ checker admits it (NAME-IMM-READS); any other but `does>` meets the preflight.
: DEF-IMMEDIATE ( -- ^n n bool )
   TOKEN$ FIND-SCOPE {: rec:^ :}
   rec XREF-FOUND? 0= if NULL-PTR 0 false exit then
   rec XREF-FLAGS DNAME-IMM and 0= if NULL-PTR 0 false exit then
   TOKEN$ rec POLICY-CHECK-REC
   TOKEN$ NAME-IMM-READS {: cnt:n :}
   rec cnt  cnt 0 >=
   dup 0= s" does>" TOKEN-IS? 0= and if DEF-PREFLIGHT then ;

\ The occurrence is selected before the preflight, so a preflight that retires
\ it cannot substitute a reused dictionary slot. With CNT above 0 the immediate
\ runs with INE at the end of the CNT raw tokens after it, and INP resumes there.
: DEF-RUN ( ^n n -- ) {: cnt:n :}
   DEF-OCC:SELECT {: slot:n occurrence:n :}
   DEF-PREFLIGHT
   slot occurrence DEF-OCC:RESOLVE {: rec:^ :}
   INE-CELL ADDR@ {: ine:^ :}
   INP-CELL ADDR@  cnt 0 ?do ine BLANKS-END ine WORD-END loop {: end:^ :}
   cnt 0 > if end INE-CELL ADDR! then
   rec XREF-NULL - XREF-ADDR>INDEX RUN+
   rec XREF-START
   execute-floor FLOORED
   cnt 0 > if ine INE-CELL ADDR!  end INP-CELL ADDR! then ;

: DEF-IMMEDIATE? ( -- bool )
   DEF-IMMEDIATE if DEF-RUN true exit then
   drop drop false ;

\ `does>` ends a defining word's own part: DOESB takes the capture's length
\ with `does>` in it, where `;` splits the body, and the signature after it,
\ which must be there, opened and closed, is the effect of each word the
\ definer creates. Its copy, `( `, the inside and ` )`, opens the clause in the
\ capture, where created-sig! takes the inside and a clause's scan reads a
\ comment.
: DEF-CREATED ( -- )
   false DEF-SIG-SPAN {: s:^ end:^ :}
   end INP-CELL ADDR!
   end s - 2 - {: len:n :}
   BODYLEN-CELL CELL@ 2 + {: at:n :}
   s 1 >str DEF-CAPTURE
   s 1 + len >str DEF-CAPTURE
   end 1 - 1 >str DEF-CAPTURE
   BODY-TEXT:BASE-CELL ADDR@ at + {: copy:^ :}
   copy len >str created-sig! ;

\ The pending parent is record NDICT. Its clause will be published under the
\ parent's name plus ;does in the same wordlist. Check the live records before
\ the signature copy or split changes any state, including a folded-case hit.
: DEF-DOES-NAME ( -- )
   ndict@ XREF-REC {: parent:^ :}
   parent XREF-NAME$ {: stem:str :}
   stem STR:LEN {: stemu:n :}
   parent XREF-WORDLIST {: wid:n :}
   DOES-CLAUSE:SUFFIX$ {: suffix:str :}
   ndict@ 0 ?do
      i XREF-REC {: row:^ :}
      row XREF-WORDLIST wid = if
         row XREF-NAME$ {: name:str :}
         name STR:LEN stemu suffix STR:LEN + = if
            name STR:DATA stemu >str stem STR:=CI
            name STR:DATA stemu + suffix STR:LEN >str suffix STR:=CI
            and if
               stem suffix
               DEF-TKA-CELL ADDR@ DEF-TKL-CELL CELL@ >str
               REJECTION:CLAUSE-TAKEN
               EXIT-STATUS:REJECTED REJECTION:STOP
            then
         then
      then
   loop ;

: DEF-DOES ( -- )
   DOESB-CELL CELL@ 0<> if
      TOKEN$ REJECTION:DOES-TWICE
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   ndict@ 1+ DICT-CAP < 0= if
      DEF-TKA-CELL ADDR@ DEF-TKL-CELL CELL@ >str
      TOKEN$ REJECTION:DICTIONARY-FULL
   then
   DEF-DOES-NAME
   BODYLEN-CELL CELL@ {: at:n :}
   DEF-CREATED
   at DOESB-CELL CELL!
   1 CLAUSE-CELL CELL!
   \ The head's locals end here: the clause cannot name one, and a local it
   \ declares is captured from an empty scope.
   0 LOCN-CELL CELL!  0 CAPDEPTH-CELL CELL! ;

\ The engine reads `does>` as its keyword, with the token's A-Z folded, unless
\ the scope holds a word of that spelling that is not immediate, which the body
\ calls instead.
: DEF-CALL? ( -- bool )
   TOKEN$ FIND-SCOPE {: rec:^ :}
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
\ TEXT-MARK as the buffer began.
variable DECL-MARK

: DEF-SOURCE-CLOSE ( -- )
   PEND-CELL CELL@ ENTRY-PEND @ <> if exit then
   DEF-CAPTURED-NAME {: name:str :}
   s" E-VS-UNTERMINATED-DEFINITION" REJECTION:OPEN
   s" this source closed `" REJECTION:EMIT-STR  name REJECTION:EMIT-STR
   s" `, a definition it did not open" REJECTION:EMIT-STR
   name REJECTION:AT  s" opened outside this source" REJECTION:EMIT-STR
   REJECTION:HELP
   s" close `" REJECTION:EMIT-STR  name REJECTION:EMIT-STR
   s" ` in the source that opens it" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: DEF-SOURCE-END ( -- )
   PEND-CELL CELL@ ENTRY-PEND @ = if exit then
   DEF-ENDED ;

EXPORT SOURCE-ROOT:TEXT-END
: DECL-SOURCE-END ( -- )
   DECL-MARK @ TEXT-END ;

\ An evaluate begun inside this native definition owns only its new tokens.
\ The checker reads the body prefix for binding, leaving the definition's
\ effect, open scopes and publication to its eventual `;`. A rejection lands in
\ evaluate's existing frame, whose token mark removes only the failed token.
TRUSTED: DEF-PREFIX-BIND ( -- )
   ENTRY-PEND @ 0= if exit then
   BODY-TEXT:BASE-CELL ADDR@
   BODYLEN-CELL CELL@ >str TOKBODY-CELL CELL@
   BIND? {: rollback:n ok:bool :}
   ok 0= if
      \ A parser operand can roll back its opener as well as the operand.
      \ The token mark must then restore the state before that opener.
      rollback TOKBODY-CELL CELL@ < if 0 TOKRAW-CELL CELL! then
      rollback TOKBODY-CELL CELL! UNDEFINED
   then ;

\ The latch is checked again here since a body immediate can have cleared it.
\ The end recovers: it declares so in RECOVER-CELL for the compile, and
\ abandons the definition the compiler rejected with E-COMP-RECOVERY, which
\ only a multi-error run throws, and reads on.
: DEF-COMPILE ( str -- )
   DEF-DISPATCH DEF-COMPILER AS-COMPILE execute ;

: DEF-END ( -- )
   DEF-SOURCE-CLOSE
   NCOMP-DISPATCH:RECOVER-CELL CELL@ {: prior:n :}
   -1 NCOMP-DISPATCH:RECOVER-CELL CELL!
   [: BODY-TEXT:BASE-CELL ADDR@ BODYLEN-CELL CELL@ >str DEF-COMPILE ;]
   catch
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
\ The source cursor stays at the running definer's call, where a rejection of
\ define-word, literal, compile, or ; locates.

\ w names the rejected operation: `define-word`, a declaration's child or an
\ item.
: DEF-NAME-REJECT ( str str -- ) {: w:str s:str :}
   w s s" not one token" REJECTION:MISSING-NAME
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: DEF-OPEN-REJECT ( str str -- ) {: w:str s:str :}
   s" E-DEFINITION-OPEN" REJECTION:OPEN
   s" cannot define `" REJECTION:EMIT-STR  s REJECTION:EMIT-STR
   s" ` while another definition is open" REJECTION:EMIT-STR
   s REJECTION:AT  s" a definition is already open" REJECTION:EMIT-STR
   s" definer" REJECTION:NOTE  w REJECTION:EMIT-STR
   REJECTION:HELP
   s" end the open definition with `;` before defining `" REJECTION:EMIT-STR
   s REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Opens the rejection of an effect value DEFINE-RUN cannot take, such as
\ EFFECT:N or one a local's type widens past the parameter cap, through its
\ label; the quotation writes why, and the caller adds the notes, the help and
\ EXIT-STATUS:REJECTED REJECTION:STOP.
: DEF-NOT-EFFECT ( [ -- ] str -- ) {: s:str :}
   s" E-NOT-EFFECT" REJECTION:OPEN
   s" the effect `define-word` got for `" REJECTION:EMIT-STR  s REJECTION:EMIT-STR
   s" ` " REJECTION:EMIT-STR  execute
   s REJECTION:AT  s" effect rejected for this name" REJECTION:EMIT-STR ;

EXPORT TFAM:TFAM-DECL-PARAM-COUNT

\ Rejects the definition s, whose effect and locals' types have more
\ distinct type parameters than the cap.
: DEF-WIDE ( str -- ) {: s:str :}
   [: s" has too many type parameters" REJECTION:EMIT-STR ;]
   s DEF-NOT-EFFECT
   TFAM-DECL-PARAM-COUNT s" " REJECTION:LIMIT
   REJECTION:HELP
   s" use fewer distinct type parameters in the effect and the locals' types"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: DEF-ONE-TOKEN? ( str -- bool ) {: s:str :}
   s STR:LEN 0= if false exit then
   s STR:DATA s STR:LEN + {: e:^ :}
   s STR:DATA e WORD-END e = ;

\ The tail and wordlist a definition of the name takes, or its rejection.
: DEF-TARGET ( str -- str n ) {: s:str :}
   s DEF-QUALIFY-NAME {: tail:str wid:n :}
   tail  s DEF-NAME-PLACE DEF-WALL
   tail wid WL-PROBE XREF-FOUND? if s PKG-DUPLICATE then
   tail wid ;

\ The checker reads the signature from the effect cell; the SIG item spells
\ the effect for the listing alone, and TSIG is read for a trusted head alone.
: DEFINE-RUN ( str n -- ) {: s:str eff:n :}
   PEND-CELL CELL@ 0<> if s" define-word" s DEF-OPEN-REJECT then
   s DEF-ONE-TOKEN? 0= if s" define-word" s DEF-NAME-REJECT then
   eff EFFECT? 0= if
      [: s" is not an effect a definition can take" REJECTION:EMIT-STR ;]
      s DEF-NOT-EFFECT
      REJECTION:HELP
      s" pass `define-word` an effect, a quotation type with no return clause"
      REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   eff EFFECT-DENSE 0= if s DEF-WIDE then
   {: dense:n :}
   s dense EFFECT-RULES? 0= if
      [: s" is not one a written signature could state" REJECTION:EMIT-STR ;]
      s DEF-NOT-EFFECT
      REJECTION:HELP
      s" give `define-word` an effect a written `( -- )` signature could state"
      REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   NCOMP-DISPATCH:COMP-CELL CELL@ 0= if
      DEF-NO-COMPILER
   then
   OVERLAY-GUARD
   s DEF-TARGET {: tail:str wid:n :}
   ITEMS-RESET
   0 BODYLEN-CELL CELL!  BODY-ITEM-KIND:NAME 0 0 s ITEM+
   \ A rejected record opens no definition, whose end would drop the item.
   [: tail TOKEN$ wid 0 DEF-OPEN-RECORD ;] catch {: rc:n :}
   rc 0<> if ITEMS-RESET rc throw then
   0 LOCN-CELL CELL!
   dense BODY-ITEMS:EFFECT-CELL CELL!
   BODY-ITEM-KIND:SIG 0 0 dense EFFECT-TEXT ITEM+
   DEF-DISPATCH ;

: HANDLE-SPLIT ( str -- str str ) {: s:str :}
   s STR:DATA s STR:LEN + {: e:^ :}
   s STR:DATA e WORD-END {: w:^ :}
   w e = if s e 0 >str exit then
   s STR:DATA w s STR:DATA - >str  w 1 + e w 1 + - >str ;

: HANDLE-PROBE ( str n -- ^n ) {: s:str wid:n :}
   s wid WL-PROBE {: rec:^ :}
   rec XREF-FOUND? 0= if s UNDEFINED-NAME then
   rec ;

\ The current namespace's record of the name in the package path's public
\ section, or its private one.
: HOME-REC ( str bool str -- ^n )
   {: ps:str pub:bool t:str :}
   ps BOOT:NAMESPACE-WL WL-PROBE {: row:^ :}
   row XREF-FOUND? 0= if ps UNDEFINED-NAME then
   pub if t row XREF-PKG-PUBLIC HANDLE-PROBE exit then
   t row XREF-PKG-PRIVATE HANDLE-PROBE ;

\ The record a handle names in this dictionary: `global TAIL`, or `public` or
\ `private` then the package's path and TAIL.
: HANDLE-REC ( str -- ^n ) {: h:str :}
   h HANDLE-SPLIT {: k:str r:str :}
   k s" global" STR:= if r BOOT:GLOBAL-WL HANDLE-PROBE exit then
   r HANDLE-SPLIT {: path:str t:str :}
   path  k s" public" STR:=  t HOME-REC ;

\ The record of the word S run with no definition open, up to its help.
: NO-DEFINITION ( str -- ) {: s:str :}
   s" E-NO-DEFINITION" REJECTION:OPEN
   s" `" REJECTION:EMIT-STR  s REJECTION:EMIT-STR
   s" ` ran with no definition open" REJECTION:EMIT-STR
   TOKEN$ REJECTION:AT  s" no definition is open here" REJECTION:EMIT-STR
   REJECTION:HELP ;

: BUILT-PENDING ( str -- ) {: s:str :}
   PEND-CELL CELL@ 0<> if exit then
   s NO-DEFINITION
   s" open a definition with `define-word` before `" REJECTION:EMIT-STR
   s REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: STRAY-END ( -- )
   PEND-CELL CELL@ 0<> if exit then
   s" ;" NO-DEFINITION
   s" a definition begins with `: NAME` and this `;` closes none: delete it, or write `: NAME` where the definition begins"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ What `;` runs: it ends the definition.
: END-RUN ( -- )
   STRAY-END DEF-END ;

\ A quotation is its code's entry, which a word's tick shares with its record.
CAST: QUOT-ENTRY ( [ R -- S ] -- n )

: NAMELESS-REJECT ( -- )
   s" E-NAMELESS-WORD" REJECTION:OPEN
   s" `compile,` got a quotation no named word enters" REJECTION:EMIT-STR
   TOKEN$ REJECTION:AT  s" quotation with no named word" REJECTION:EMIT-STR
   REJECTION:HELP
   s" pass `compile,` the execution token of a named word, as `'` gives one"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Rejects a call of a retired word, and of a package word whose package is
\ not in the current namespace.
: CALLEE-CHECK ( ^n -- ) {: r:^ :}
   r XREF-WORDLIST {: wid:n :}
   wid XREF-RETIRED-WL = if r XREF-NAME$ UNDEFINED-NAME then
   wid WL:GLOBAL? if exit then
   wid XREF:NS-ROW nip if exit then
   E-COMP-STATE throw ;

\ Appends a call of the word at record index R, spelled S, which the checker
\ binds.
: CALL-ITEM ( str n -- ) {: s:str r:n :}
   r XREF-REC CALLEE-CHECK
   BODY-ITEM-KIND:CALL r 0 s ITEM+ ;

\ Appends a call of the word at entry E.
: COMPILE-CALL ( n -- ) {: e:n :}
   e XREF:ENTRY-REC {: r:n :}
   r -1 = if NAMELESS-REJECT then
   r XREF-REC XREF-NAME$ r CALL-ITEM ;

\ What `compile,` runs: it appends a call of the word whose entry the quotation is.
: COMPILE-RUN ( [ R -- S ] -- )
   s" compile," BUILT-PENDING QUOT-ENTRY COMPILE-CALL ;

\ A cell's decimal spelling, written from the end of LIT-DIGITS: 20 bytes
\ hold any i64 and its sign, and one more a local's `l`. `/` truncates, so a
\ negative value's remainders are at most 0.
21 BUFFER: LIT-DIGITS
variable LIT-AT

: LIT-DIGIT ( n -- ) {: c:n :}
   LIT-AT @ 1 - LIT-AT !
   c LIT-DIGITS LIT-AT @ + c! ;

: LIT-REM ( n -- n ) {: v:n :}
   v v 10 / 10 * - {: r:n :}
   r 0 < if 0 r - else r then ;

: LIT-SPAN ( -- str ) LIT-DIGITS LIT-AT @ +  21 LIT-AT @ - >str ;

: LIT$ ( n -- str ) {: v:n :}
   21 LIT-AT !
   v begin dup LIT-REM 48 + LIT-DIGIT  10 / dup 0= until drop
   v 0 < if 45 LIT-DIGIT then
   LIT-SPAN ;

\ A local's spelling in the listing: `l` and its index.
: LOCAL$ ( n -- str ) LIT$ drop  108 LIT-DIGIT  LIT-SPAN ;

\ What `literal` runs: an INT item, spelled in decimal.
: LITERAL-RUN ( n -- ) {: v:n :}
   s" literal" BUILT-PENDING
   BODY-ITEM-KIND:INT v 0 v LIT$ ITEM+ ;

\ Appends a call of the word a lookup of the name found.
: FOUND-CALL ( str ^n -- ) {: s:str rec:^ :}
   rec XREF-FOUND? 0= if s UNDEFINED-NAME then
   s  rec XREF-NULL - XREF-ADDR>INDEX CALL-ITEM ;

: ITEM-OP-REJECT ( n -- ) {: op:n :}
   s" E-ITEM-OP" REJECTION:OPEN
   s" an item's op is not one " SAY  s" ITEM-ABI" TICKS  s"  names" SAY
   TOKEN$ REJECTION:AT  s" unknown item op" SAY
   s" op" REJECTION:NOTE  op REJECTION:EMIT-N
   REJECTION:HELP  s" pass an op " SAY  s" ITEM-ABI" TICKS  s"  defines" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Rejects the item S unless define-word opened the definition: only its
\ effect numbers the parameters a local's type is read by.
: LOCAL-UNBUILT ( str -- ) {: s:str :}
   BODY-ITEMS:EFFECT-CELL CELL@ 0<> if exit then
   s" E-NO-DEFINITION" REJECTION:OPEN
   s" the local " SAY  s TICKS
   s"  needs a definition " SAY  s" define-word" TICKS  s"  opened" SAY
   TOKEN$ REJECTION:AT  s" not opened by " SAY  s" define-word" TICKS
   REJECTION:HELP  s" open the definition with " SAY  s" define-word" TICKS
   s" , whose effect numbers the local's type parameters" SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Appends a local of type node T, whose index is the LOCAL items before it;
\ its node numbers T's parameters on from the definition's effect.
: LOCAL-ITEM ( str n -- ) TYPE-ARG {: s:str t:n :}
   s LOCAL-UNBUILT
   t TYPE-DENSE 0= if DEF-CAPTURED-NAME DEF-WIDE then
   {: dense:n :}
   BODY-ITEMS:LOCALS-CELL CELL@ {: i:n :}
   BODY-ITEM-KIND:LOCAL dense i i LOCAL$ ITEM+
   i 1 + BODY-ITEMS:LOCALS-CELL CELL! ;

\ Rejects variable s, whose type has more distinct type parameters than the
\ cap.
: VARIABLE-WIDE ( str -- ) {: s:str :}
   s" E-BAD-STORAGE" REJECTION:OPEN
   s" the type `EFFECT:VARIABLE-OF` got for `" REJECTION:EMIT-STR
   s REJECTION:EMIT-STR  s" ` has too many type parameters" REJECTION:EMIT-STR
   s REJECTION:AT  s" type rejected for this name" REJECTION:EMIT-STR
   TFAM-DECL-PARAM-COUNT s" " REJECTION:LIMIT
   REJECTION:HELP
   s" use fewer distinct type parameters in the variable's type"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ Declares variable s of type node T. T's parameters carry the file's
\ numbering, so they are numbered 0, 1 ... as define-word numbers an effect's;
\ an empty name is rejected as one first.
: VARIABLE-RUN ( str n -- ) TYPE-ARG {: s:str t:n :}
   s STR:LEN 0 <> if t EFFECT-DENSE 0= if s VARIABLE-WIDE then else t then
   {: dense:n :}
   s dense LBUF:TYPED-VARIABLE-NODE ;

\ Runs a definer's item: ITEM-ABI op OP, one-token spelling S, and V, a
\ TYPE's or LOCAL's schema node or a LOCAL-REF's index.
: ITEM-RUN ( str n n -- ) {: s:str v:n op:n :}
   s BUILT-PENDING
   s DEF-ONE-TOKEN? 0= if s" item" s DEF-NAME-REJECT then
   op ITEM-ABI:KEYWORD = if BODY-ITEM-KIND:KEYWORD 0 0 s ITEM+ exit then
   op ITEM-ABI:OPERAND = if BODY-ITEM-KIND:OPERAND 0 0 s ITEM+ exit then
   op ITEM-ABI:TYPE = if
      BODY-ITEM-KIND:TYPE v TYPE-ARG 0 s ITEM+ exit
   then
   op ITEM-ABI:LOCAL = if s v LOCAL-ITEM exit then
   op ITEM-ABI:LOCAL-REF = if
      s LOCAL-UNBUILT  BODY-ITEM-KIND:LOCAL-REF v 0 v LOCAL$ ITEM+ exit
   then
   op ITEM-ABI:CALL = if s  s LOOKUP-NAME  FOUND-CALL exit then
   op ITEM-ABI:CALL-EXACT = if
      s XREF-FIND {: rec:^ :}
      s rec POLICY-CHECK-REC
      s rec FOUND-CALL exit
   then
   op ITEM-OP-REJECT ;

: COMPILING? ( -- bool )
   PEND-CELL CELL@ 0= if false exit then
   NCOMP-DISPATCH:COMP-LATCH-CELL CELL@ 0= if DEF-NO-COMPILER then
   RAWNEXT-CELL CELL@ 0 > if
      TOKEN$ TYPE-SPAN-END? if 0 RAWNEXT-CELL CELL! then
   then
   RAWNEXT-CELL CELL@ 0 > if
      RAWNEXT-CELL CELL@ 1- TOKEN$ CHECKER-TYPE-SPAN-STEP
      if drop 0 else 1+ then RAWNEXT-CELL CELL!
      TOKEN$ DEF-CAPTURE  DEF-PREFIX-BIND  true exit
   then
   RAWNEXT-CELL CELL@ 0<> if
      0 RAWNEXT-CELL CELL!
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
   TOKEN$ SOURCE-SYNTAX:BODY-PARSING-KEYWORD? if -1 RAWNEXT-CELL CELL! then
   s" [type]" TOKEN-IS? if 1 RAWNEXT-CELL CELL! then
   DEF-PREFIX-BIND
   true ;

\ A created word is published whole where its keyword is read.

\ The keyword joins the capture, and the checker reads it, `NAME create` for a
\ created word, its verdict dropped: a definer's word always publishes. The
\ check runs on the program's stack, so neither this word nor its callers keep
\ locals.
: DEF-HOOK ( str -- )
   DEF-CAPTURE
   BODY-TEXT:BASE-CELL ADDR@ BODYLEN-CELL CELL@ >str
   CHECK:DEFINITION drop ;

: DEF-ADDR-HOOK ( -- )
   s" create" DEF-HOOK ;

: DEF-RAW-ACTIVE ( -- n )
   SOURCE-OWNER-A @ NCOMP-DISPATCH:DECL-RAW-OFF PKG-OPERATION {: own:n :}
   own 0<> if own exit then
   NCOMP-DISPATCH:DECL-RAW-OFF PKG-TARGET {: target:n :}
   target 0= if
      s" E-DISPATCH-UNSET" REJECTION:OPEN
      s" the trust-raw dispatch is unset at `" REJECTION:EMIT-STR
      TOKEN$ REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
      TOKEN$ REJECTION:AT  s" no trust-raw declaration latched" REJECTION:EMIT-STR
      REJECTION:HELP
      s" load the checker, which sets the trust-raw dispatch, before this definer"
      REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED KEEP-DIE
   then
   target ;

\ A definer's effect, raw dictionary storage stated by sig; the
\ checker seals it.
: DEF-RAW-PUBLISH ( str -- ) {: sig:str :}
   DEF-RAW-ACTIVE {: own:n :}
   sig own DEF-DECLARE
   SOURCE-OWNER-A @ 0= if exit then
   NCOMP-DISPATCH:DECL-RAW-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   SOURCE-OWNER-A @ NCOMP-DISPATCH:DECL-RAW-OFF PKG-OPERATION target = if exit then
   sig target DEF-DECLARE ;

: DEF-ADDR-PUBLISH ( -- )
   s" -- ^a" DEF-RAW-PUBLISH ;

\ The DATA address a created word's body pushes, as a number.
: DEF-HERE ( -- n ) here NULL-PTR - ;

\ The pending record's fixed body ( val entry kind ): entry is the routine a
\ routine body calls, 0 for every other kind.
: DEF-FIXED ( n n COMP:body -- )
   NCOMP-DISPATCH:COMP-LATCH-CELL CELL@ {: latch:n :}
   latch 0= if DEF-NO-COMPILER then
   NCOMP-DISPATCH:FIXED-COMP-CELL CELL@ AS-COMPILE-FIXED execute ;

\ The cell-aligned here a created body pushes, kept as the latest body start.
: DEF-ANCHOR ( -- n )
   POINTER-STORAGE:ALIGN-APART DEF-HERE dup COMMA-BASE-CELL CELL! ;

\ The fixed body pushes the anchor, and ends in the slot a `does>` patches.
: DEF-ADDR-TAIL ( -- )
   DEF-ANCHOR 0 COMP-BODY:ADDRESS DEF-FIXED
   DEF-CLOSE ;

\ No record slot or code space left is rejected after the name, naming it.
: DEF-ADDR-ROW ( str bool -- ) {: kw:str cell?:bool :}
   kw OPERAND  kw TOKEN$ REJECTION:NOT-A-NAME
   DKIND:ADDR DNAME-ALLOC or cell? if DNAME-CELL or then DEF-START
   DEF-ADDR-TAIL ;

\ A compiled defining word reaches `create` through the engine primitive,
\ which calls the row through CREATEP-CELL. The keyword's hook belongs to the
\ interpreting `create` only; `-- ^a` is published at both, or the word
\ made is E-UNDEFINED where it is next read.
: DEF-CREATE-ROW ( -- )
   s" create" false DEF-ADDR-ROW
   DEF-ADDR-PUBLISH ;

: DEF-ADDR ( str bool -- )
   DEF-ADDR-ROW
   DEF-ADDR-HOOK
   DEF-ADDR-PUBLISH ;

\ A constant's record opens as DKIND:VAL, the stamp a mention of it folds to,
\ after the top item is judged and before the constant takes the top cell.
variable DEF-VALUE

: DEF-CONSTANT ( -- )
   TOKEN$ {: kw:str :}
   s" constant" OPERAND  s" constant" TOKEN$ REJECTION:NOT-A-NAME
   kw TOKEN$ TOP-TAKE
   DKIND:VAL DEF-START
   [: DEF-VALUE ! ;] CELL-TAKE RUN-LITERAL
   DEF-VALUE @ 0 COMP-BODY:VALUE DEF-FIXED
   DEF-CLOSE
   s" constant" DEF-HOOK
   s" -- a" DEF-RAW-PUBLISH ;

\ `cast:` and `linear:` have no body or `;`: each completes where its keyword
\ is read, with a compiler latched, and calls no def hook.

\ With the input at its end the rejection names the keyword as spelled, at the
\ end's line.
: DEF-CAST-NAME ( -- )
   TOKEN if exit then
   TOKEN$ NAME-MISSING ;

\ The signature must be there, opened and closed, before the qualifier's
\ rejections, and INP passes it first, so those name the line it ends on.
\ def-open clears TSIG, so trust-sig! takes the signature once the record is
\ open.
: DEF-CAST-HEAD ( -- )
   DEF-SEED
   false DEF-SIG-SPAN {: s:^ end:^ :}
   s end DEF-SIG-PASS
   DEF-QUALIFY {: name:str wid:n :}
   name TOKEN$ wid DKIND:CAST DEF-RECORD
   s end DEF-SIG-INNER ;

\ A row the checker rejects throws before DEF-FIXED counts the record, so no
\ callable name is left.
: DEF-REGISTER-IDENTITY ( str str n -- ) {: s:str sig:str off:n :}
   SOURCE-OWNER-A @ off PKG-OPERATION {: own:n :}
   own 0<> if s sig own DEF-DECLARE-NAME then
   off PKG-TARGET {: target:n :}
   target 0= if exit then
   SOURCE-OWNER-A @ off PKG-OPERATION target = if exit then
   s sig target DEF-DECLARE-NAME ;

: DEF-REGISTER-CAST ( -- )
   DEF-CAPTURED-NAME TSIG-A-CELL ADDR@ TSIG-U-CELL CELL@ >str
   NCOMP-DISPATCH:DECL-CAST-OFF DEF-REGISTER-IDENTITY ;

EXPORT CHECK:REC-WIDE-PUBLISH

\ The record takes the wide mark the checker's registrar latched.
: DEF-CAST-FACTS ( -- )
   REC-WIDE-PUBLISH ;

: DEF-CAST-ROW ( -- )
   TOKEN$ {: kw:str :}
   DEF-CAST-NAME  kw TOKEN$ REJECTION:NOT-A-NAME
   DEF-CAST-HEAD
   DEF-REGISTER-CAST
   0 0 COMP-BODY:CAST DEF-FIXED  DEF-CLOSE
   DEF-CAST-FACTS ;

\ ---- child words a declaration makes ------------------------------------------
\ CHILD-RUN makes the word NAME for a declaration, with no text: a fixed body of
\ its CHILD-ABI shape, whose value and effect CHILD makes from the shape,
\ parameter and stored type while the record is pending. A storage child's body
\ pushes its own data address, which the declaration fills after the return.

\ The handle of a routine child's routine.
: ROUTINE$ ( n -- str ) {: shape:n :}
   shape CHILD-ABI:BUFFER = if s" private LBUF ELEMENT" exit then
   shape CHILD-ABI:DEFERRED = if s" private LBUF DEFERRED-ELEMENT" exit then
   shape CHILD-ABI:BIND = if s" private LBUF BIND" exit then
   shape CHILD-ABI:GROW = if s" private LBUF GROW" exit then
   shape CHILD-ABI:DYNAMIC = if s" private DYNAMIC-STORAGE ELEMENT" exit then
   shape CHILD-ABI:RESERVE = if s" private DYNAMIC-STORAGE ENSURE" exit then
   shape CHILD-ABI:RELEASE = if s" private DYNAMIC-STORAGE EMPTY" exit then
   shape CHILD-ABI:SLOT = if s" private TASK SLOT" exit then
   shape CHILD-ABI:HEAP-NEW = if
      s" private DYNAMIC-STORAGE MAP-RECORD" exit
   then
   shape CHILD-ABI:HEAP-RELEASE = if
      s" private DYNAMIC-STORAGE UNMAP-RECORD" exit
   then
   shape CHILD-ABI:SLOTS = if s" private TASK SLOT-AT" exit then
   shape CHILD-ABI:WORK = if s" private CWORK WORK" exit then
   shape CHILD-ABI:WORKS = if s" private CWORK WORK-AT" exit then
   shape CHILD-ABI:ARENA-NEW = if s" private ARENA-DECL NEW" exit then
   s" private ARENA-DECL NEW-DISPOSE" ;

\ A routine child's entry: in hb's session the definer's tick; in a build the
\ build's own, by name: the definer may be hb's, whose code no object names.
: ROUTINE ( n n -- n ) {: shape:n param:n :}
   data-base BOOT:IMAGE-BASE-CELL + @ 0= if param exit then
   shape ROUTINE$ HANDLE-REC XREF-START ;

\ A shape is a CHILD-ABI constant: those left are the routine children's.
: CHILD-BODY ( n n -- COMP:body n ) {: shape:n param:n :}
   shape CHILD-ABI:CELL-OFF =
   shape CHILD-ABI:PTR-OFF = or
   shape CHILD-ABI:BYTE-OFF = or
   shape CHILD-ABI:FIELD = or if COMP-BODY:OFFSET 0 exit then
   shape CHILD-ABI:INDEX = if COMP-BODY:INDEX 0 exit then
   shape CHILD-ABI:VALUE = if COMP-BODY:VALUE 0 exit then
   shape CHILD-ABI:STORED = if COMP-BODY:ADDRESS 0 exit then
   shape CHILD-ABI:INIT-GET = if COMP-BODY:GETTER 0 exit then
   shape CHILD-ABI:INIT-SET = if COMP-BODY:SETTER 0 exit then
   shape CHILD-ABI:SPAN = if COMP-BODY:BYTES 0 exit then
   shape CHILD-ABI:MAKE =
   shape CHILD-ABI:UNMAKE = or
   shape CHILD-ABI:NOMINAL-IN = or
   shape CHILD-ABI:NOMINAL-OUT = or if COMP-BODY:CAST 0 exit then
   COMP-BODY:ROUTINE shape param ROUTINE ;

: CHILD-ANCHORED? ( n -- bool ) {: shape:n :}
   shape CHILD-ABI:STORED >= shape CHILD-ABI:ARENA-DISPOSE <= and
   shape CHILD-ABI:DEFERRED = or ;

\ A value child is recorded as a constant, which a call folds to its number, a
\ STORED child as an address, which a call folds to an inline push.
: CHILD-KIND ( n -- n ) {: shape:n :}
   shape CHILD-ABI:VALUE = if DKIND:VAL exit then
   shape CHILD-ABI:STORED = if DKIND:ADDR DNAME-ALLOC or exit then
   shape CHILD-ANCHORED? if DNAME-ALLOC exit then
   0 ;

: CHILD-RECORD ( str n -- ) {: s:str shape:n :}
   s DEF-TARGET {: tail:str wid:n :}
   s DEF-NAME-PLACE {: p:str :}
   0 BODYLEN-CELL CELL!  s DEF-CAPTURE
   tail p wid shape CHILD-KIND DEF-OPEN-RECORD ;

\ The anchor is taken after CHILD, whose facts may open an intern segment
\ at here.
: CHILD-CLOSE ( n n str -- n )
   {: shape:n param:n ts:str :}
   DEF-CAPTURED-NAME shape param ts CHILD {: val:n h:n :}
   shape param CHILD-BODY {: kind:COMP:body entry:n :}
   shape CHILD-ANCHORED? if DEF-ANCHOR entry else val 0 then
   kind DEF-FIXED
   DEF-CLOSE
   DEF-CAST-FACTS
   h ;

: CHILD-DEFINE ( str n n str -- n )
   {: s:str shape:n param:n ts:str :}
   s shape CHILD-RECORD  shape param ts CHILD-CLOSE ;

\ Answers the child's effect handle. TYPE is empty for the shapes before STORED.
: CHILD-RUN ( str n n str -- n )
   {: s:str shape:n param:n ts:str :}
   PEND-CELL CELL@ 0<> if s" child word" s DEF-OPEN-REJECT then
   s DEF-ONE-TOKEN? 0= if s" child word" s DEF-NAME-REJECT then
   shape param CHILD-BODY 2drop
   NCOMP-DISPATCH:COMP-CELL CELL@ 0= if DEF-NO-COMPILER then
   OVERLAY-GUARD
   s shape param ts CHILD-DEFINE ;

\ A qualified linear name is rejected by the certifier before DEF-QUALIFY can
\ create its namespace. A second colon is left to DEF-QUALIFY's syntax rejection.
: LINEAR-QUALIFIED? ( -- bool )
   TOKEN$ {: t:str :}
   t 0 COLON-AT {: q:n :}
   q 0 <= q 1+ t STR:LEN >= or if false exit then
   t q 1+ COLON-AT 0< ;

\ A qualified name's certifier gets the name token, which its rejection places:
\ with no pending record the capture maps to no input.
: DEF-LINEAR ( -- )
   s" linear:" OPERAND  s" linear:" TOKEN$ REJECTION:NOT-A-NAME
   DEF-SEED
   false DEF-SIG-SPAN {: s:^ end:^ :}
   s end DEF-SIG-PASS
   LINEAR-QUALIFIED? if
      TOKEN$ s 1 + end s - 2 - >str NCOMP-DISPATCH:DECL-LINEAR-OFF
      DEF-REGISTER-IDENTITY
      s" E-BAD-QUALIFIED" REJECTION:OPEN
      s" the linear type `" REJECTION:EMIT-STR  TOKEN$ REJECTION:EMIT-STR
      s" ` has a qualified name" REJECTION:EMIT-STR
      TOKEN$ REJECTION:AT  s" qualified name" REJECTION:EMIT-STR
      REJECTION:HELP
      s" declare the `linear:` type with a bare name inside its package"
      REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then
   DEF-QUALIFY {: name:str wid:n :}
   name TOKEN$ wid DKIND:CAST DEF-RECORD
   s end DEF-SIG-INNER
   DEF-CAPTURED-NAME TSIG-A-CELL ADDR@ TSIG-U-CELL CELL@ >str
   NCOMP-DISPATCH:DECL-LINEAR-OFF DEF-REGISTER-IDENTITY
   0 0 COMP-BODY:CAST DEF-FIXED  DEF-CLOSE
   DEF-CAST-FACTS ;

\ `defer NAME ( in -- out )` declares a word that calls the execution token in
\ its dispatch cell, an aligned DATA cell that starts with `defer-unset`'s and
\ that `is` in a body re-points. The cell is allotted before the qualifier,
\ and the checker gets the effect before the body compiles, which asks it
\ whether the defer returns.

public
\ `defer`, the source token t, has no name after it. It is written through
\ its help, and the caller stops it.
: DEFER-NO-NAME ( str -- ) {: t:str :}
   t s" a name" t s" no name follows" REJECTION:MISSING-OPERAND
   s" write the defer's name and signature after `" REJECTION:EMIT-STR
   t REJECTION:EMIT-STR  s" `, as `defer NAME ( n -- n )`" REJECTION:EMIT-STR ;
private

\ With the input at its end the token cells still hold the keyword, which the
\ rejection names.
: DEFER-NAME ( -- )
   TOKEN if exit then
   TOKEN$ DEFER-NO-NAME  EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The global word: no package's own defer-unset stands in for it.
: DEFER-UNSET-XT ( -- n )
   s" defer-unset" BOOT:GLOBAL-WL HANDLE-PROBE XREF-START ;

\ The cell holds an execution token, so xt! declares it one: a snapshot moves
\ it with the code region.
TRUSTED: DEF-XT! ( n n -- ) xt! ;

: DEFER-CELL ( n -- n )
   {: xt:n :}
   POINTER-STORAGE:ALIGN-APART
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
   s" E-DEFER-TABLE-FULL" REJECTION:OPEN
   s" the pre-trust defer table has no room for `" REJECTION:EMIT-STR
   TOKEN$ REJECTION:EMIT-STR  s" `" REJECTION:EMIT-STR
   TOKEN$ REJECTION:AT  s" this defer does not fit" REJECTION:EMIT-STR
   REJECTION:HELP
   s" declare this defer after the checker's trust declarations, or shorten its name and signature"
   REJECTION:EMIT-STR
   EXIT-STATUS:FULL KEEP-DIE ;

: PD-PUT ( str n n n -- ) {: s:str cap:n lenoff:n off:n :}
   s STR:LEN cap > if PD-FULL then
   s STR:LEN lenoff CELL!
   s STR:DATA data-base off + BYTE-VIEW s STR:LEN BYTE-COPY ;

: PD-HOLD ( str -- ) {: sig:str :}
   PD-TABLE-OFF CELL@ {: count:n :}
   count PD-CAP >= if PD-FULL then
   PD-TABLE-OFF PD-SLOTS-REL + count PD-SLOT * + {: slot:n :}
   DEF-CAPTURED-NAME PD-NAME-CAP slot PD-NLEN-OFF + slot PD-NAME-OFF + PD-PUT
   sig PD-SIG-CAP slot PD-SLEN-OFF + slot PD-SIG-OFF + PD-PUT
   count 1 + PD-TABLE-OFF CELL! ;

\ The active owner's operation at off, then the target owner's, 0 when the
\ active owner's field holds the same one.
: DEF-OWNERS ( n -- n n ) {: off:n :}
   SOURCE-OWNER-A @ off PKG-OPERATION {: own:n :}
   off PKG-TARGET {: target:n :}
   own  target own = if 0 else target then ;

\ Before the target owner is ready only the active owner's two are called, and
\ the pending slot holds the defer for the target's.
: DEFER-REGISTER ( str -- ) {: sig:str :}
   DEFER-READY? {: ready:bool :}
   NCOMP-DISPATCH:DECL-EFFECT-OFF DEF-OWNERS {: own:n target:n :}
   own 0<> if sig own DEF-DECLARE then
   ready target 0<> and if sig target DEF-DECLARE then
   NCOMP-DISPATCH:DECL-DEFER-OFF DEF-OWNERS {: down:n dtarget:n :}
   down 0<> if DEF-CAPTURED-NAME down PKG-AS-NAME-ACTION execute then
   ready dtarget 0<> and if DEF-CAPTURED-NAME dtarget PKG-AS-NAME-ACTION execute then
   ready 0= if sig PD-HOLD then ;

\ defer-unset is looked up before DEF-SEED, so its rejection leaves no capture.
: DEF-DEFER ( -- )
   DEFER-UNSET-XT {: unset:n :}
   TOKEN$ {: kw:str :}
   DEFER-NAME  kw TOKEN$ REJECTION:NOT-A-NAME
   DEF-SEED
   false DEF-SIG-SPAN {: s:^ end:^ :}
   s end DEF-SIG-PASS
   unset DEFER-CELL {: cell:n :}
   DEF-QUALIFY {: name:str wid:n :}
   name TOKEN$ wid 0 DEF-RECORD
   s end DEF-SIG-INNER
   s 1 + end s - 2 - >str DEFER-REGISTER
   cell 0 COMP-BODY:DEFERRED DEF-FIXED
   DEF-CLOSE ;

EXPORT CHECK:TOP-KEYWORD-ID
EXPORT CHECK:KW-PACKAGE
EXPORT CHECK:KW-PUBLIC
EXPORT CHECK:KW-PRIVATE
EXPORT CHECK:KW-GLOBAL
EXPORT CHECK:KW-END-PACKAGE
EXPORT CHECK:KW-HOST
EXPORT CHECK:KW-TARGET
EXPORT CHECK:KW-SEAL
EXPORT CHECK:KW-USING
EXPORT CHECK:KW-END-USING
EXPORT CHECK:KW-EXPORT
EXPORT CHECK:KW-SPLIT-STR
EXPORT CHECK:KW-COLON
EXPORT CHECK:KW-KERNEL
EXPORT CHECK:KW-TRUSTED
EXPORT CHECK:KW-CREATE
EXPORT CHECK:KW-VARIABLE
EXPORT CHECK:KW-CONSTANT
EXPORT CHECK:KW-DEFER
EXPORT CHECK:KW-CAST
EXPORT CHECK:KW-LINEAR
EXPORT CHECK:KW-IMMEDIATE

\ Runs the token's code if it is a top-level keyword. `immediate` marks the
\ newest record, whatever it is, and checks nothing.
: KEYWORD? ( -- bool )
   TOKEN$ TOP-KEYWORD-ID {: id:n :}
   id 0< if false exit then
   id KW-HOST =  id KW-TARGET = or if
      SEARCH
      REC @ XREF-FOUND? if TOKEN$ SECTION-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP then
      id KW-TARGET = if 1 else 0 then SECTION-CELL CELL!
      true exit
   then
   id case
      KW-PACKAGE of PKG-PACKAGE endof
      KW-PUBLIC of
         PKG-PUB-CELL CELL@ 0= if PKG-CONTEXT-FAIL then
         PUBLIC-SECTION
      endof
      KW-PRIVATE of
         PKG-PRI-CELL CELL@ 0= if PKG-CONTEXT-FAIL then
         PRIVATE-SECTION
      endof
      KW-GLOBAL of
         PKG-PUB-CELL CELL@ 0= if PKG-CONTEXT-FAIL then
         GLOBAL-SECTION
      endof
      KW-END-PACKAGE of PKG-END-PACKAGE endof
      KW-SEAL of PKG-SEAL endof
      KW-USING of PKG-USING endof
      KW-END-USING of PKG-END-USING endof
      KW-EXPORT of PKG-EXPORT endof
      KW-SPLIT-STR of
         CHECKER-SPLIT-STR @ 0= if
            TOKEN$ SPLIT-STR-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP
         then
      endof
      KW-COLON of false DEF-HEAD endof
      KW-KERNEL of false DEF-HEAD endof
      KW-TRUSTED of true DEF-HEAD endof
      KW-CREATE of s" create" false DEF-ADDR endof
      KW-VARIABLE of s" variable" true DEF-ADDR 1 cells allot endof
      KW-CONSTANT of DEF-CONSTANT endof
      KW-DEFER of DEF-DEFER endof
      KW-CAST of DEF-CAST-ROW endof
      KW-LINEAR of DEF-LINEAR endof
      KW-IMMEDIATE of imm-mark endof
   endcase
   true ;

public

: INSTALL-CREATE ( -- )
   ['] DEF-CREATE-ROW data-base CREATEP-CELL + xt! ;

;package

require src/lib/memory.f
require src/habu/address-cells.f
require src/habu/stepper.f
require src/core/compile-reads.f

package INTERPRETER

private

EXPORT CODE-RECLAIM:TRUNCATE
EXPORT SOURCE-ROOT:TEXT-MARK
EXPORT SOURCE-ROOT:TEXT-UNWOUND

\ A number is pushed and a word run, each under the floor: the token's effect
\ on the stack is the program's, so this row, and every row here that
\ runs program code, states none of it.
: PUSH-NUM ( -- )
   [: VALUE @ TOP-EV-NUM HOOK ;] CELL-LITERAL RUN-LITERAL ;

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
   s" global" TOKEN-IS? if true exit then
   s" ;package" TOKEN-IS? if true exit then
   s" using" TOKEN-IS? if true exit then
   s" ;using" TOKEN-IS? ;

: POLICY-PRE ( -- bool )
   POLICY-NDICT-CELL CELL@ 0= if false exit then
   PEND-CELL CELL@ 0<> if false exit then
   POLICY-SYNTAX? if false exit then
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   range if NUMBER-RANGE then
   num if false exit then
   SEARCH
   true ;

\ A file loader found in the target's text is the target's, which would record
\ the file in its own registry, outside the build; hb's runs it through the
\ build.
: LOADER? ( -- bool )
   s" include" TOKEN-IS? if true exit then
   s" require" TOKEN-IS? if true exit then
   s" included" TOKEN-IS? if true exit then
   s" required" TOKEN-IS? if true exit then
   s" provided" TOKEN-IS? ;

\ A row's private wid of 0 means no private section, not hb's global wordlist.
: HOST-PKG-REC ( n -- ^n ) {: src:n :}
   src SYM-PKG$ WL:NAMESPACE WL-PROBE {: row:^ :}
   row XREF-FOUND? 0= if XREF-NULL exit then
   src SYM-ROW SYM.VIS @ SYM-PUBLIC = if
      src SYM-NAME$ row XREF-PKG-PUBLIC XREF-FIND-WL exit
   then
   row XREF-PKG-PRIVATE {: wid:n :}
   wid 0= if XREF-NULL exit then
   src SYM-NAME$ wid XREF-FIND-WL ;

\ hb's record of the word a binding comes from, an EXPORT's source; hb's global
\ wordlist is 0, as every linked program's.
: HOST-REC ( n -- ^n ) {: sym:n :}
   sym 0= if XREF-NULL exit then
   sym CHECKER-SYM-SOURCE {: src:n :}
   src SYM-PKG$ STR:LEN 0<> if src HOST-PKG-REC exit then
   src SYM-NAME$ 0 XREF-FIND-WL ;

\ Whether a build runs hb's own word for the token rather than the target's.
: HOST-WORD? ( -- bool )
   data-base BOOT:IMAGE-BASE-CELL + @ 0= if false exit then
   TOKEN$ CHECKER-VERIFY-TOP-BINDING nip {: sym:n ctl:n :}
   s" nominal-provider" TOKEN-IS?  s" root-mover" TOKEN-IS? or {: annot:bool :}
   annot  s" intrinsic" TOKEN-IS? or  LOADER? or  s" undefine" TOKEN-IS? or
   0= if
      ctl CHECKER-OWNER-ABI:BINDING-ID-MASK and
      CHECKER-OWNER-ABI:BINDING-ID-SHIFT rshift {: id:n :}
      id CHECKER-OWNER-ABI:BINDING-PARSES-ID < if false exit then
      id CHECKER-OWNER-ABI:BINDING-DECLARATION-ID > if false exit then
   then
   sym HOST-REC REC !
   REC @ XREF-FOUND? 0= if annot if false exit then UNDEFINED then
   true ;

: STEP ( -- )
   \ A parser word owns its next token, even when it spells a comment opener.
   RAWNEXT-CELL CELL@ 0<> if COMPILING? drop exit then
   COMMENT? if exit then
   POLICY-PRE {: found:bool :}
   COMPILING? if exit then
   LITERAL? if exit then
   KEYWORD? if exit then
   HOST-WORD? if RUN-FOUND exit then
   found if RUN-FOUND exit then
   DISPATCH ;

\ The kernel's nested-evaluate recovery rolls an outer definition back to the
\ token it was reading. Mark before TOKEN, including the pass that reaches EOF.
: TOKEN-MARK ( -- )
   BODYLEN-CELL CELL@ TOKBODY-CELL CELL!
   LOCN-CELL CELL@ TOKLOCN-CELL CELL!
   CAPDEPTH-CELL CELL@ TOKDEPTH-CELL CELL!
   RAWNEXT-CELL CELL@ TOKRAW-CELL CELL!
   cp@ dbase@ - TOKCP-CELL CELL!
   VSP-CELL CELL@ TOKVSP-CELL CELL! ;

\ Reading source on a task's thread ends the process: only the main thread
\ compiles.
: TASK-GUARD ( -- )
   TASK-TCB-CELL CELL@ 0= if exit then
   s" E-TASK-COMPILE" REJECTION:OPEN
   s" source is read on a task's thread, and only the main thread compiles"
   REJECTION:EMIT-STR
   REJECTION:HELP
   s" run `evaluate`, `include` and `require` on the main thread only"
   REJECTION:EMIT-STR
   EXIT-STATUS:TASK-COMPILE KEEP-DIE ;

: RUN-TOKENS ( -- )
   TASK-GUARD  begin TOKEN-MARK TOKEN while STEP repeat ;

\ A definition this source opened ends inside its locals group.
: GROUP-SOURCE-END ( -- )
   GROUP-OPEN @ {: at:^ :}
   at NULL-PTR = if exit then
   PEND-CELL CELL@ ENTRY-PEND @ = if exit then
   at 2 >str GROUP-REJECT  EXIT-STATUS:REJECTED REJECTION:STOP ;

: RUN ( -- )
   NULL-PTR GROUP-OPEN !
   RUN-TOKENS
   GROUP-SOURCE-END
   DEF-SOURCE-END
   DECL-SOURCE-END ;

\ A `package` in the buffer leaves PKG-OUTSIDE-CELL 0, which matters only in a package.
: CLEAN-END ( n -- ) {: outside:n :}
   USE-FLOOR @ {: back:n :}
   back USE-DEPTH-CELL CELL!
   USE-PKG-SAVE-CELL CELL@ back > if back USE-PKG-SAVE-CELL CELL! then
   PKG-OUTSIDE-CELL CELL@ 0<> if outside PKG-OUTSIDE-CELL CELL! then ;

\ DP-CELL as the buffer began.
$38 constant FR-DP

\ The frame on its way into the catch that links it; empty at rest.
TYPED-VARIABLE FRAME-ARG ^u8

\ The source owner's record end or held facts count, and its cut back to one:
\ raw execution tokens in the owner record.
CAST: AS-ROWS-END ( n -- [ -- n ] )
CAST: AS-ROWS-CUT ( n -- [ n -- ] )

\ The checker's record end as a buffer begins, plus one, or 0 when no owner
\ records rows.
: ROWS-MARK ( -- n )
   SOURCE-OWNER-A @ NCOMP-DISPATCH:DECL-ROWS-END-OFF PKG-OPERATION {: xt:n :}
   xt 0= if 0 exit then
   xt AS-ROWS-END execute 1+ ;

\ Cut the checker's records back to the frame's mark, so no definition the
\ rollback drops keeps a record.
: ROWS-CUT ( ^u8 -- ) {: f:^ :}
   f EVAL-FRAME:ROWS FR@ {: mark:n :}
   mark 0= if exit then
   SOURCE-OWNER-A @ NCOMP-DISPATCH:DECL-RETRACT-ROWS-OFF PKG-OPERATION {: xt:n :}
   xt 0= if exit then
   mark 1- xt AS-ROWS-CUT execute ;

\ The checker's held facts count, plus one, or 0 when no owner holds facts.
: HELD-MARK ( -- n )
   SOURCE-OWNER-A @ NCOMP-DISPATCH:DECL-HELD-END-OFF PKG-OPERATION {: xt:n :}
   xt 0= if 0 exit then
   xt AS-ROWS-END execute 1+ ;

\ The frame of a buffer. Its using depth is also the buffer's using floor.
: FRAME-OPEN ( -- ^u8 )
   EVAL-FRAME-SIZE MEM-ALLOC-PTR {: f:^ :}
   INP-CELL CELL@ f EVAL-INP FR!
   INE-CELL CELL@ f EVAL-INE FR!
   SRCLOC:INB-CELL CELL@ f EVAL-INB FR!
   TKA-CELL CELL@ f EVAL-TKA FR!
   TKL-CELL CELL@ f EVAL-TKL FR!
   ndict@ f EVAL-FRAME:NDICT FR!
   REQUIRE-REG:MARK f EVAL-FRAME:REQUIRES FR!
   DP-CELL CELL@ f FR-DP FR!
   PEND-CELL CELL@ f EVAL-FRAME:PEND FR!
   f EVAL-PKG + {: snap:^ :}
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
   ROWS-MARK f EVAL-FRAME:ROWS FR!
   HELD-MARK f EVAL-FRAME:HELD FR!
   f ;

: FRAME-LINK ( ^u8 -- ) {: f:^ :}
   NULL-PTR FRAME-ARG !
   EVAL-TOP-CELL CELL@ f EVAL-PREV FR!
   f NULL-PTR - EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1+ EVALD-CELL CELL! ;

: FRAME-UNLINK ( ^u8 -- ) {: f:^ :}
   f EVAL-PREV FR@ EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1- EVALD-CELL CELL!
   f EVAL-INP FR@ INP-CELL CELL!
   f EVAL-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL!
   f EVAL-TKA FR@ TKA-CELL CELL!
   f EVAL-TKL FR@ TKL-CELL CELL! ;

: FRAME-LEAVE ( ^u8 -- ) {: f:^ :}
   f FRAME-UNLINK
   f EVAL-FRAME:SECTION FR@ SECTION-CELL CELL!
   f EVAL-FRAME:PKG-OUTSIDE FR@ CLEAN-END ;


\ Whether frame f's text left records above its mark, moved DP from its own or
\ left held facts above its mark.
: REMAINS? ( ^u8 -- bool ) {: f:^ :}
   ndict@ f EVAL-FRAME:NDICT FR@ >  DP-CELL CELL@ f FR-DP FR@ <>  or
   f EVAL-FRAME:HELD FR@ {: mark:n :}
   mark 0<>  HELD-MARK mark >  and  or ;

\ In a build, pends the rejection of frame f's failed text at the line that ran
\ it; the rejection fails the build when the text left something.
: FAILED-PEND ( n ^u8 -- ) {: c:n f:^ :}
   s" E-FAILED" REJECTION:OPEN
   s" a text failed and its failure was caught, so the build writes nothing" SAY
   f EVAL-TKA FR-ADDR@ f EVAL-TKL FR@ >str REJECTION:AT
   s" this ran the text" SAY
   s" code" REJECTION:NOTE  c REJECTION:EMIT-N
   c ERROR:NAME$ {: name:str :}
   name STR:LEN 0<> if  s"  (" SAY  name SAY  s" )" SAY  then
   REQUIRE-REG:MARK f EVAL-FRAME:REQUIRES FR@ ?do
      s" required" REJECTION:NOTE
      i REQUIRE-REG:SLOT i REQUIRE-REG:LEN >str SAY
   loop
   REJECTION:HELP
   s" let the failure end the build, or keep a text that can fail from defining, allotting or undefining" SAY
   f REMAINS? REJECTION:PEND ;

\ A failed text keeps its code, records and data, which older data may point
\ into: its records lose their names and its files leave the require registry.
: FRAME-ROLLBACK-CODE ( n ^u8 -- )
   {: c:n f:^ :}
   DEF-ABORT
   f ROWS-CUT
   BUILDING? if c f FAILED-PEND then
   f EVAL-FRAME:NDICT FR@ RETIRE-ABOVE
   0 COMMA-RUN-CELL CELL!
   f EVAL-FRAME:REQUIRES FR@ REQUIRE-REG:TRUNCATE ;


: FRAME-ROLLBACK-TOKEN ( -- )
   TOKBODY-CELL CELL@ BODYLEN-CELL CELL!
   TOKLOCN-CELL CELL@ LOCN-CELL CELL!
   TOKDEPTH-CELL CELL@ CAPDEPTH-CELL CELL!
   TOKRAW-CELL CELL@ RAWNEXT-CELL CELL!
   dbase@ TOKCP-CELL CELL@ + TRUNCATE
   TOKVSP-CELL CELL@ VSP-CELL CELL! ;

: FRAME-RESTORE-SCOPE ( ^u8 -- ) {: f:^ :}
   f EVAL-PKG + {: snap:^ :}
   snap PKGSNAP-USE FR@ USE-DEPTH-CELL CELL!
   f EVAL-FRAME:PKG-OUTSIDE FR@ PKG-OUTSIDE-CELL CELL!
   f EVAL-FRAME:SECTION FR@ SECTION-CELL CELL!
   f EVAL-FRAME:ENCLOSING ENCLOSING:REC ENCLOSING:PUB - + FR@
   f EVAL-FRAME:ENCLOSING ENCLOSING:PARENT ENCLOSING:PUB - + FR@
   snap PKGSNAP-REC FR@ snap PKGSNAP-PARENT FR@
   snap PKGSNAP-CUR FR@ snap PKGSNAP:FLOOR FR@ PKG-RECOVER ;

: FRAME-RECOVER ( n ^u8 -- ) {: c:n f:^ :}
   f EVAL-FRAME:PEND FR@ 0= if c f FRAME-ROLLBACK-CODE else FRAME-ROLLBACK-TOKEN then
   f EVAL-INP FR@ INP-CELL CELL!
   f EVAL-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL!
   f EVAL-TKA FR@ TKA-CELL CELL!
   f EVAL-TKL FR@ TKL-CELL CELL!
   f FRAME-RESTORE-SCOPE
   f EVAL-PREV FR@ EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1- EVALD-CELL CELL! ;

0 constant FRAME-DONE
1 constant FRAME-LINKED

\ Success is code 0, which the catch answers only when the text ran to its end.
: FRAME-EXIT ( n -- n )
   0= if FRAME-DONE else FRAME-LINKED then ;

: FRAME-FREE ( ^u8 -- )
   EVAL-FRAME-SIZE munmap 0<> if E-MEM-UNMAP throw then ;

\ The REPL has one source owner. A line uses a savepoint, but its successful
\ stack, pending definition, package and usings remain for the next line.
create REPL-QNL 63 c, 10 c,
variable REPL-OK
\ The used publics as a line began, which a failed line puts back: lines share
\ one source, so a line may close an earlier line's using and fill its slot.
USE-MAX TYPED-BUFFER REPL-WIDS n

: REPL-WIDS-BACK ( -- )
   USE-MAX 0 ?do  i REPL-WIDS @  i cells USE-WIDS-OFF +  CELL!  loop ;

: REPL-STACK-CLEAR ( -- )
   stack-clear ;

: REPL-RECOVER ( n ^u8 n ^n -- ) {: c:n f:^ prior:n found:^ :}
   c f FRAME-ROLLBACK-CODE
   f EVAL-INP FR@ INP-CELL CELL!
   f EVAL-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL!
   f EVAL-TKA FR@ TKA-CELL CELL!
   f EVAL-TKL FR@ TKL-CELL CELL!
   REPL-WIDS-BACK
   f FRAME-RESTORE-SCOPE
   prior USE-FLOOR !
   found REC ! ;

\ The REPL keeps its lines while this holds: a definition is pending or a
\ declaration is open.
: LINES-NEEDED? ( -- bool )
   PEND-CELL CELL@ 0<>  SOURCE-ROOT:OPENER$ STR:LEN 0<> or ;

\ The record, at the running token, of a throw of code c that its text did not
\ catch.
: THROWN-AT ( n -- )
   {: c:n :}
   c ERROR:OPEN
   SRCLOC:TOKEN$ >str REJECTION:AT  s" this threw" REJECTION:EMIT-STR
   c ERROR:HELP ;

\ Ends the running text's inner catch with the code c `catch` answered.
: LEFT ( n -- )
   {: c:n :}
   c 0= if exit then
   c 1 255 BETWEEN? if c throw then
   REJECTION:CARRIED? 0= if c THROWN-AT REJECTION:SAVE then
   c REJECTION:PASS ;

: REPL-LINE ( str -- bool ) {: s:str :}
   TEXT-MARK {: mark:n :}
   FRAME-OPEN {: f:^ :}
   \ The session, not this line, owns a pending definition from an earlier
   \ line, yet a failed line abandons it through this savepoint's full rollback.
   0 f EVAL-FRAME:PEND FR!
   USE-FLOOR @ {: prior:n :}
   USE-MAX 0 ?do  i cells USE-WIDS-OFF + CELL@  i REPL-WIDS !  loop
   REC @ {: found:^ :}
   s STR:DATA INP-CELL ADDR!  s STR:DATA SRCLOC:INB-CELL ADDR!
   s STR:DATA s STR:LEN + INE-CELL ADDR!
   TOKENS-CELL CELL@ 1 + TOKENS-CELL CELL!
   f FRAME-ARG !
   [: FRAME-ARG @ FRAME-LINK [: RUN-TOKENS ;] catch LEFT ;]
   catch {: code:n :}
   code FRAME-EXIT {: out:n :}
   out case
      FRAME-DONE of
         f FRAME-UNLINK
      endof
      FRAME-LINKED of
         f FRAME-UNLINK
         code f prior found REPL-RECOVER
      endof
   endcase
   f FRAME-FREE
   out FRAME-DONE = {: done:bool :}
   done 0= if
      code 1 255 BETWEEN? 0= if code ERROR:UNCAUGHT then
      mark TEXT-UNWOUND
   then
   LINES-NEEDED? 0= if REJECTION:DROP-LINES then
   done 0= if
      REPL-STACK-CLEAR
      2 REPL-QNL 2 >str write drop
      false exit
   then
   true ;

\ A pending definition's name is measured up to the end of input, which goes on
\ the name's kept line.
: REPL-END ( -- )
   PEND-CELL CELL@ 0<> if
      PENDTKA-CELL ADDR@ REJECTION:KEPT-LINE {: line:str :}
      line STR:LEN 0<> if
         line STR:DATA SRCLOC:INB-CELL ADDR!
         line STR:DATA line STR:LEN + INE-CELL ADDR!
      then
   then
   [: DEF-SOURCE-END DECL-SOURCE-END ;] catch {: code:n :}
   code 0<> if NULL$ code die then ;

\ A definition may span lines; EOF rejects one still open.
: REPL ( -- )
   0 ENTRY-PEND !
   TEXT-MARK DECL-MARK !
   true REPL-OK !
   begin
      REPL-OK @ if S\"  ok\n" type then
      DEBUG:REPL-READ {: line:str :}
      line STR:DATA 0= if REPL-END exit then
      line REJECTION:KEEP-LINE REPL-LINE REPL-OK !
   again ;

\ The input base stays the buffer's first byte, so lines count from it. A range
\ starting past it continues a run of the buffer and keeps its PKG-OUTSIDE-CELL.
: EVAL-FRAMED-RANGE ( str n n -- ) {: s:str first:n end:n :}
   FRAME-OPEN {: f:^ :}
   USE-FLOOR @ {: outer:n :}
   REC @ {: found:^ :}
   ENTRY-PEND @ {: prior:n :}
   DECL-MARK @ {: prior-mark:n :}
   PEND-CELL CELL@ {: entry:n :}
   entry ENTRY-PEND !
   TEXT-MARK DECL-MARK !
   USE-DEPTH-CELL CELL@ USE-FLOOR !
   first 0= if 1 PKG-OUTSIDE-CELL CELL! then
   s STR:DATA first + INP-CELL ADDR!  s STR:DATA SRCLOC:INB-CELL ADDR!
   s STR:DATA end + INE-CELL ADDR!
   f FRAME-ARG !
   [: FRAME-ARG @ FRAME-LINK [: RUN ;] catch LEFT ;]
   catch {: code:n :}
   code FRAME-EXIT {: out:n :}
   out case
      FRAME-DONE of f FRAME-LEAVE endof
      FRAME-LINKED of
         code f FRAME-RECOVER
         entry 0<> if BODYLEN-CELL CELL@ ITEMS-CUT then
      endof
   endcase
   out FRAME-DONE <> if DECL-MARK @ TEXT-UNWOUND then
   prior ENTRY-PEND !  prior-mark DECL-MARK !
   out FRAME-DONE <> if
      entry PEND-CELL CELL!
   then
   outer USE-FLOOR !  found REC !
   f FRAME-FREE
   out FRAME-DONE <> if code throw then ;

public

: EVAL-FRAMED ( str -- ) {: s:str :}
   s COMPILE-READS:TEXT+
   TOKENS-CELL CELL@ 1 + TOKENS-CELL CELL!
   [: s 0 s STR:LEN EVAL-FRAMED-RANGE ;] REQUIRES:AS-TEXT ;

private
\ `evaluate-closed`'s text runs on a stack of its own (habu1.f B-EVAL-CLOSED),
\ so `depth` after it counts the cells the text left.
: EVAL-CLOSED ( str -- )
   EVAL-FRAMED  depth {: left:n :}
   left 0= if exit then
   s" E-EVAL-RESIDUE" REJECTION:OPEN
   s" an evaluated closed text did not leave the stack as it found it" SAY
   s" cells" REJECTION:NOTE  left REJECTION:EMIT-N
   REJECTION:HELP
   s" a closed text must leave the stack as it found it: drop or consume what it pushes"
   SAY
   EXIT-STATUS:REJECTED REJECTION:STOP ;

public
: INSTALL-EVALUATE ( -- )
   ['] EVAL-FRAMED data-base PROVIDED-XT:EVALUATE-CELL + xt!
   ['] EVAL-CLOSED data-base PROVIDED-XT:EVALUATE-CLOSED-CELL + xt!
   ['] EVAL-FRAMED-RANGE data-base PROVIDED-XT:EVALUATE-RANGE-CELL + xt!
   ['] DEFINE-RUN data-base PROVIDED-XT:DEFINE-CELL + xt!
   ['] COMPILE-RUN data-base PROVIDED-XT:COMPILE-CELL + xt!
   ['] END-RUN data-base PROVIDED-XT:END-CELL + xt!
   ['] ITEM-RUN data-base PROVIDED-XT:ITEM-CELL + xt!
   ['] CHILD-RUN data-base PROVIDED-XT:CHILD-CELL + xt!
   ['] DEF-SEAL-NAME data-base PROVIDED-XT:SEAL-CELL + xt!
   ['] DEF-TARGET data-base PROVIDED-XT:TARGET-CELL + xt!
   ['] STORAGE-LINE data-base PROVIDED-XT:STORAGE-LINE-CELL + xt!
   ['] CELL-LINE data-base PROVIDED-XT:CELL-LINE-CELL + xt!
   ['] LITERAL-RUN data-base PROVIDED-XT:LITERAL-CELL + xt!
   ['] TYPE-OP data-base PROVIDED-XT:TYPE-OP-CELL + xt!
   ['] VARIABLE-RUN data-base PROVIDED-XT:VARIABLE-CELL + xt! ;

;package
