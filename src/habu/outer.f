\ outer.f - the outer interpreter written in Habu: numbers, the dictionary
\ search, then the readers src/habu/interpret.f's loop reads a buffer with.

require src/lib/prelude.f
require src/core/bytes.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/dict.f

\ ---- numbers ----------------------------------------------------------------
\ NUMBER reads a token as the engine's reader does (LNUM, EMIT-NUM in
\ habu1.f): an optional `-`, an optional `$` for radix 16 with digits a-f and
\ A-F, then digits. A radix-10 token may end in one `.` and one or more
\ digits, which makes it a float; its value is then the double's bits.
\
\ A token of that shape can still be out of range, and the interpreter then
\ reports it undefined without looking it up as a name. Its digits may not
\ carry past 2^64 - 1. A decimal integer's magnitude is at most MAX-N, or 2^63
\ when negative; a hex integer keeps all 64 bits and negates modulo 2^64. A
\ float's integer part is at most MAX-N and its fraction at most 18 digits.

package OUTER
private

$7FFFFFFFFFFFFFFF constant MAX-N
$8000000000000000 constant MIN-N


\ Unsigned order: flipping the sign bit maps it onto signed order.
: U> ( n n -- bool )
   MIN-N xor swap MIN-N xor < ;


: BETWEEN? ( n n n -- bool ) {: c:n lo:n hi:n :}
   c lo >= c hi <= and ;


\ The value of byte c as a digit of radix, and whether it is one.
: DIGIT ( n n -- n bool ) {: c:n radix:n :}
   c [char] 0 [char] 9 BETWEEN? if c [char] 0 - true exit then
   radix 16 <> if 0 false exit then
   c [char] a [char] f BETWEEN? if c [char] a - 10 + true exit then
   c [char] A [char] F BETWEEN? if c [char] A - 10 + true exit then
   0 false ;


\ Whether the byte at idx is c.
: AT? ( ptr u8 n n n -- bool ) {: a:ptr u:n idx:n c:n :}
   idx u < if a idx + c@ c = else false then ;


\ The index past the run of radix digits that starts at idx.
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


\ A digit takes int to int * radix + d modulo 2^64; wrapped latches the first
\ carry out of the cell.
: INT-STEP ( n bool n n -- n bool ) {: int:n wrapped:bool d:n radix:n :}
   int radix * d +
   int d radix CARRIES? wrapped or ;


: INTEGER ( ptr u8 n n n -- n bool ) {: a:ptr lo:n hi:n radix:n :}
   0 false
   hi lo ?do a i + c@ radix DIGIT drop radix INT-STEP loop ;


\ A fraction digit takes frac to frac * 10 + d and scale to scale * 10. The
\ fraction stays below the scale, so bounding the scale by MAX-N / 10 (18
\ digits) keeps both in a signed cell; past it the latch refuses the token.
: FRAC-STEP ( n n bool n -- n n bool ) {: frac:n scale:n wrapped:bool d:n :}
   frac 10 * d +
   scale 10 *
   scale MAX-N 10 / > wrapped or ;


: FRACTION ( ptr u8 n n bool -- n n bool ) {: a:ptr lo:n hi:n wrapped:bool :}
   0 1 wrapped
   hi lo ?do a i + c@ 10 DIGIT drop FRAC-STEP loop ;


\ Whether the token from dot on is a float's fraction: a radix-10 `.` and one
\ or more digits that run to the end.
: FRACTION? ( ptr u8 n n n -- bool ) {: a:ptr u:n dot:n radix:n :}
   radix 10 <> if false exit then
   a u dot [char] . AT? 0= if false exit then
   dot 1+ u <  a u dot 1+ 10 RUN-END u =  and ;


: NOT-NUMBER ( -- n bool bool bool )
   0 false false false ;


: OUT-OF-RANGE ( -- n bool bool bool )
   0 false false true ;


\ A carry, or a decimal magnitude past MAX-N (2^63 when negative).
: INT-OUT? ( n bool bool n -- bool ) {: int:n wrapped:bool neg:bool radix:n :}
   wrapped if true exit then
   radix 16 = if false exit then
   neg if int MIN-N U> else int MAX-N U> then ;


: INT-FINISH ( n bool bool n -- n bool bool bool ) {: int:n wrapped:bool neg:bool radix:n :}
   int wrapped neg radix INT-OUT? if OUT-OF-RANGE exit then
   int neg if negate then false true false ;


\ Normalize the exact integer plus fractional remainder to 53 retained bits.
\ frac < scale <= 10^18, so doubling the remainder always fits a cell.
\ Every admitted nonzero value is at least 10^-18: none is subnormal.
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

\ The admitted decimal magnitude is bounded before conversion. Round its
\ exact value once to nearest, ties to even; apply the sign even to zero.
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

\ ---- dictionary search ------------------------------------------------------
\ OUTER:FIND answers the record the engine's own lookup binds one token to, or
\ XREF-NULL: NDICT:SCOPE-REC, which is the `scope-find` primitive (habu2.f
\ LSCOPEREC) - LFIND and then the used publics, the leaves the JIT's call site
\ runs. The row comes back whatever its flags, and the caller reads the
\ immediate, wide, internal and min-in facts from its XREF-FLAGS, as LFIND's
\ flag output folds them: the interpreter must see an internal row in order to
\ refuse it. Two used publics that both export the tail are E-USING-AMBIGUOUS.
\ FIND-SCOPE answers LFIND's record alone, which a body's immediate and the
\ operand of `export` are looked up with.

package OUTER

private

\ Not the lookup: the row one wordlist holds under a name, whatever its flags -
\ the namespace row `package`, `using` and a qualified definition name ask,
\ `checker-export`, and the tail a definition or `export` would add, which must
\ not be there yet.
\ xref-search-wl walks the dictionary's hash index under the key the engine's
\ own probe uses; its row is OUTER's own (src/habu/prims.f).
: WL-PROBE ( ptr u8 n n -- ptr n ) xref-search-wl ;

\ The index of the first colon at or after `from`, or -1: the shape the
\ qualifier readers refuse a token by.
: COLON-AT ( ptr u8 n n -- n )
   {: a:ptr u:n from:n :}
   u from ?do
      a i + c@ $3A = if i unloop exit then
   loop
   -1 ;

\ LFIND's answer: the open package, then the global wordlist, or NAME:tail,
\ never a used public. It is the lookup's answer unless a used public bound it,
\ and the lookup asks the used publics only when LFIND missed: a bound record
\ that is also theirs is LFIND's miss, except in the open package's public
\ wordlist, where a package that uses itself met it in LFIND first.
: FIND-SCOPE ( ptr u8 n -- ptr n )
   scope-find drop drop {: rec:ptr used:ptr :}
   rec used <> if rec exit then
   rec XREF-FOUND? 0= if rec exit then
   rec XREF-WORDLIST NDICT:OPEN-PUB = if rec exit then
   XREF-NULL ;

public

: FIND ( ptr u8 n -- ptr n )
   NDICT:SCOPE-REC ;

;package

\ ---- the interpret loop ------------------------------------------------------
\ OUTER:INTERPRET (src/habu/interpret.f) reads a buffer token by token with the
\ words below, as the engine's interpret loop does (habu2.f EM-COMMENT's LMAIN,
\ EM-INTERPRET-WORDS) for comments, the literal keywords (`s"`, `c"`, `."`,
\ their escaped forms, `char` and `'`), numbers and dictionary words, and with
\ src/habu/packages.f for the package keywords (`package`, `public`, `private`,
\ `;package`, `using`, `;using` and `export`), and with src/habu/definers.f for
\ the definition heads (`:`, `kernel:` and `trusted:`) and the body one opens,
\ with the immediates the body runs, its `does>` and the `;` that ends it, and
\ for `create`, `variable`, `cast:` and `immediate`. The engine's other
\ keywords (`constant`, `defer` and `linear:`) are not read yet: a body
\ captures them as it captures any token, and elsewhere they are not
\ dictionary words, so they refuse as undefined.
\
\ The input is the engine's own. The cursor, its end and the buffer start sit
\ in INP-CELL, INE-CELL and SRCLOC:INB-CELL, and the token in TKA-CELL and
\ TKL-CELL, so a parsing word a token runs reads this buffer, a nested load
\ nests, and a refusal names the token the engine would.
\
\ The data stack and the return stack are the program's while a word runs:
\ the source observer compares the return-stack depth with the floor it
\ recorded at evaluate entry (SX-RFLOOR), so the loop keeps its own state in
\ locals and cells, never on the return stack.

package OUTER

private

70 constant RC-REJECT            \ habu2.f RC-REJECT: a refused token, catchable
74 constant RC-BAD-LITERAL       \ habu2.f C-QUOTE-EOF: no closing quote, or a bad escape
74 constant RC-NO-NAME           \ habu2.f C-DIE-KEYWORD-NAME: a keyword's operand is missing
74 constant RC-UNCLOSED          \ habu2.f LSRCEND, LCOMEND, LSRCCLOSE: the input ends inside a definition or a ( comment, or closes one it did not open
76 constant RC-TOO-LONG          \ habu2.f LCSTR: a counted string past CSTR-MAX bytes
79 constant RC-TASK-LIVE         \ habu2.f C-TASK-LIVE-GUARD's $4F: a keyword while a task is live
52 constant MIN-IN-SHIFT         \ layout.f DNAME-MIN-IN-MASK: record flag bits 52-59
32 constant BLANK                \ LTOK: every byte at or below it separates tokens
$0A constant NEWLINE
$5C constant LINE-COMMENT        \ a backslash
$28 constant OPEN-COMMENT        \ (
$29 constant CLOSE-COMMENT       \ )
$22 constant QUOTE               \ a double quote closes a string literal
$5C constant ESCAPE              \ a backslash, in an escaped literal
$20 constant CASE-BIT            \ ORed into A-Z it gives a-z
255 constant CSTR-MAX            \ a counted string's length is one byte
2 constant ERR-FD

create NL NEWLINE c,

\ ---- the engine's cells --------------------------------------------------------
: CELL@ ( n -- n )
   data-base + @ ;

: CELL! ( n n -- )
   data-base + ! ;

\ The input cells hold byte addresses (habu1.f B-EVAL, LTOK). These two are
\ outer.f's one crossing between such a cell and a byte pointer, through
\ `ptr-field`, the declared door to a DATA cell that holds an address.
: ADDR@ ( n -- ptr u8 ) data-base + 0 ptr-field @ ;
: ADDR! ( ptr u8 n -- ) data-base + 0 ptr-field ! ;

\ A cell of a frame the engine holds (interpret.f FRAME-OPEN, the source observer).
: FR! ( n ptr u8 n -- )
   + CELL-VIEW ! ;

: FR@ ( ptr u8 n -- n )
   + CELL-VIEW @ ;

: TOKEN$ ( -- ptr u8 n )
   TKA-CELL ADDR@ TKL-CELL CELL@ ;

\ ---- the scanner (habu1.f LTOK) -------------------------------------------------
: BLANKS-END ( ptr u8 ptr u8 -- ptr u8 ) {: p:ptr e:ptr :}
   p begin dup e < if dup c@ BLANK <= else false then while 1 + repeat ;

: WORD-END ( ptr u8 ptr u8 -- ptr u8 ) {: p:ptr e:ptr :}
   p begin dup e < if dup c@ BLANK > else false then while 1 + repeat ;

\ The next token lands in TKA and TKL, and INP stops on the byte after it. With
\ none left INP is the end and the token cells keep the last token.
: TOKEN ( -- bool )
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:ptr e:ptr :}
   p e BLANKS-END {: s:ptr :}
   s e WORD-END {: t:ptr :}
   t INP-CELL ADDR!
   s t = if false exit then
   s TKA-CELL ADDR!
   t s - TKL-CELL CELL!
   true ;

\ ---- refusals (habu2.f EM-COMPILE-UNDEF, EM-INTERPRET-UNDERFLOW) ---------------
\ Each writes the engine's text on descriptor 2 and throws its code. Like the
\ engine's own diagnostics, a write that fails is not reported: the throw is.
: SAY ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR-FD a u write drop ;

\ The message, the token and a newline, then the catchable reject.
: REFUSE ( ptr u8 n -- )
   SAY TOKEN$ SAY NL 1 SAY
   RC-REJECT throw ;

: UNDEFINED ( -- )
   s" E-UNDEFINED: " REFUSE ;

\ ---- the refusal's location (habu2.f EM-COMPILE-DIE) ----------------------------
20 constant DIGITS-CAP
create DIGITS DIGITS-CAP allot
variable DIGIT-AT

: DIGIT+ ( n -- ) {: c:n :}
   DIGIT-AT @ 1 - DIGIT-AT !
   c DIGITS DIGIT-AT @ + c! ;

\ A positive number's decimal digits, written from the end of DIGITS.
: DIGITS$ ( n -- ptr u8 n )
   DIGITS-CAP DIGIT-AT !
   begin dup 10 mod $30 + DIGIT+ 10 / dup 0= until drop
   DIGITS DIGIT-AT @ +  DIGITS-CAP DIGIT-AT @ - ;

\ 1 + the newlines in [INB, INP): the line the cursor is on.
: LINE ( -- n )
   SRCLOC:INB-CELL ADDR@ INP-CELL ADDR@ {: b:ptr p:ptr :}
   1 p b - 0 ?do b i + c@ NEWLINE = if 1 + then loop ;

\ ` at <path>:<line>` while a source file is open.
: AT-SOURCE ( -- )
   SRCLOC:PATHLEN-CELL CELL@ {: u:n :}
   u 0= if exit then
   s"  at " SAY
   SRCLOC:PATH-CELL ADDR@ u SAY
   s" :" SAY
   LINE DIGITS$ SAY ;

\ The engine's compile-die tail (habu2.f LCOMPILEDIE) after a site's message:
\ the location, a newline, then the site's code, thrown as the engine throws
\ it inside evaluate.
: THROW-AT ( n -- )
   AT-SOURCE NL 1 SAY throw ;

: AMBIGUOUS ( -- )
   s" hb: ambiguous bare word resolves in multiple used packages: " SAY
   TOKEN$ SAY ENGINE-ERROR:USING-AMBIGUOUS THROW-AT ;

: SHADOWED ( -- )
   s" hb: bare word a global and a used package both export: " SAY
   TOKEN$ SAY ENGINE-ERROR:USING-SHADOW-GLOBAL THROW-AT ;

\ ---- fail-closed exits ----------------------------------------------------------
\ Where the engine ends the process instead of throwing (NR-EXIT-GROUP), the
\ text and a newline go to descriptor 2 and no program code runs: the exit
\ hook (src/habu/layout.f EXIT-HOOK-CELL) is cleared before `die`, which would
\ otherwise call it.
: FAIL-CLOSED ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   a u SAY  NL 1 SAY
   0 EXIT-HOOK-CELL CELL!
   s" " rc die ;

\ A keyword that changes the dictionary or its scope while a task is live
\ ends the process, the keyword its whole diagnostic line (habu2.f
\ C-TASK-LIVE-GUARD).
: TASK-GUARD ( -- )
   TASKS-LIVE-CELL CELL@ 0= if exit then
   TOKEN$ RC-TASK-LIVE FAIL-CLOSED ;

\ While a checker overlay is open a definer head, a new package row or private
\ wid, or an export throws by name: the overlay's close would drop what it
\ wrote (habu1.f OVERLAY-EMIT:GUARD, reads the same latch at the same sites).
\ An `undefine` refuses at its own entry, which both loops run (xref.f
\ XREF-UNDEFINE-GUARD).
: OVERLAY-GUARD ( -- )
   REPLAY-SCOPE:LATCH CELL@ 0= if exit then
   s" hb: definition while a checker replay is open: " SAY
   TOKEN$ SAY ENGINE-ERROR:OVERLAY-OPEN THROW-AT ;

\ Whether wordlist wid is protected (habu1.f EMIT-PROTWID, read as
\ tools/prot-wid-probe.f MEMBER? reads it): the two engine-reserved wordlists
\ by rule, then the wid's bit in the bitmap at PROT-BITS-OFF, which no wid
\ outside [0, PROT-WID-MAX) has.
: PROTECTED? ( n -- bool ) {: wid:n :}
   wid OWNER-API-PUB-WID = wid OWNER-API-PRI-WID = or if true exit then
   wid 0 < wid PROT-WID-MAX >= or if false exit then
   wid 6 rshift cells PROT-BITS-OFF + CELL@
   wid 63 and rshift 1 and 0<> ;

\ ---- comments (habu2.f EM-COMMENT) -----------------------------------------------
\ A backslash or `(` opens a comment only as a whole one-byte token. The comment
\ runs past the next newline or `)`. A backslash comment may also run to the end
\ of the input; a `(` comment the input ends inside is refused at the `(`.
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
   s" hb: source ended inside a ( comment" SAY RC-UNCLOSED THROW-AT ;

: COMMENT? ( -- bool )
   TKL-CELL CELL@ 1 <> if false exit then
   TKA-CELL ADDR@ c@ {: b:n :}
   b LINE-COMMENT = if NEWLINE SKIP-PAST drop true exit then
   b OPEN-COMMENT = if CLOSE-COMMENT SKIP-PAST 0= if UNCLOSED then true exit then
   false ;

\ ---- running program code -------------------------------------------------------
\ A word or the hook runs through execute-floor, which answers whether it left
\ the stack below its base and then resets the stack to the base: the floor the
\ engine checks after every token. The prim's row states no effect for the xt,
\ so it is OUTER's alone (src/habu/prims.f).
: FLOORED ( bool -- )
   if s" E-UNDERFLOW: " REFUSE then ;

\ ---- the top-row hook (habu2.f LTOPHOOK) ------------------------------------------
\ With a hook installed (set-top-check) it gets ( token class flags ) for each
\ number after the push and for each word, past its gates, before it runs. The
\ hook consumes those four cells, as the event protocol states. A hook that
\ leaves the stack below its base on a word event is refused here, before the
\ word runs; the engine runs the word and stops it only where it touches the
\ guard page (the underdepth throw, crash.f ONE FAULT IS NOT AN EXIT), so this
\ loop names the underflow before any of the word has run.
: HOOK@ ( -- n )
   TOP-HOOK-CELL CELL@ ;

\ The hook cell holds a raw execution token; the event protocol is its effect.
CAST: TOP-HOOK ( n -- [ ptr u8 n n n -- ] )
CAST: HOOK-CALL ( [ -- ] -- n )

\ The call runs under the floor, so it pushes the event itself: a quotation
\ reads no local, and the class and flags wait in these cells.
variable EVENT-CLASS
variable EVENT-FLAGS

: HOOK ( n n -- )
   HOOK@ 0= if 2drop exit then
   EVENT-FLAGS ! EVENT-CLASS !
   [: TOKEN$ EVENT-CLASS @ EVENT-FLAGS @ HOOK@ TOP-HOOK execute ;]
   HOOK-CALL execute-floor FLOORED ;

\ ---- the unit hook (habu2.f C-UNIT-HOOK) -----------------------------------------
\ unit-compile-run arms UNIT-COMPILE-CELL with a guard for the one source it
\ runs (tools/native-unit-compile.f GUARD). The guard gets the token, a class
\ and one of these events, and only a package name's answer is read: a
\ nonzero one skips the rest of the input. A token outside a body is classed
\ 0 for a word, 1 for an integer and 2 for a float; a word about to run is
\ classed by its xt; the other events are classed 0.
0 constant UNIT-EV-TOKEN         \ a token outside a body, past a comment
2 constant UNIT-EV-PACKAGE       \ the name `package` has read
3 constant UNIT-EV-IMMEDIATE     \ a body immediate about to run
4 constant UNIT-EV-WORD          \ a word about to run, past its gates
5 constant UNIT-EV-END           \ the end of the source

: UNIT? ( -- bool )
   UNIT-COMPILE-CELL CELL@ 0<> ;

\ The cell holds the guard's xt raw; this view states the guard's effect.
CAST: UNIT-GUARD ( n -- [ ptr u8 n n n -- n ] )

\ The guard's answer, 0 with none armed. The token cells are put back after
\ the guard returns, whatever it parsed.
: UNIT-EVENT ( n n -- n ) {: class:n event:n :}
   UNIT? 0= if 0 exit then
   TKA-CELL CELL@ TKL-CELL CELL@ {: a:n u:n :}
   TOKEN$ class event UNIT-COMPILE-CELL CELL@ UNIT-GUARD execute
   a TKA-CELL CELL!  u TKL-CELL CELL! ;

\ A unit's misuse names the token, then the engine's compile-die tail
\ (habu2.f C-DIE-TOKEN with 70).
: UNIT-REFUSE ( -- )
   TOKEN$ SAY RC-REJECT THROW-AT ;

\ ---- numbers ------------------------------------------------------------------------
variable VALUE

\ A number's value waits in VALUE. One out of range is undefined, and no word
\ of its spelling is asked for.
: NUMERAL? ( -- bool )
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   range if UNDEFINED then
   v VALUE !
   num ;

\ ---- words --------------------------------------------------------------------------
\ The record of the word the step looked up. INTERPRET (interpret.f) puts back
\ what it held when the buffer began, so no record outlives the buffer that
\ looked it up: a stripped image would hold it as an undeclared dictionary
\ pointer.
TYPED-VARIABLE REC ptr n

\ A sealed source may name a record it defined after the seal or a callable
\ public record in an admitted wordlist (habu2.f LPOLICYREC).
: POLICY-REFUSE ( -- )
   s" hb: not in vocabulary: " SAY
   TOKEN$ SAY ENGINE-ERROR:POLICY THROW-AT ;

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

\ A global hit is asked of the used publics as well, as habu2.f
\ INTERP-EMIT:FIND-SHADOW asks them: one that also exports the token refuses
\ it, and two are the lookup's ambiguity. The lookup names the used publics'
\ record whether or not LFIND bound the token.
: LOOKUP-GO ( -- )
   TOKEN$ FIND REC !
   REC @ POLICY-CHECK-REC
   REC @ XREF-FOUND? 0= if exit then
   REC @ XREF-WORDLIST 0<> if exit then
   TOKEN$ scope-find {: rec:ptr used:ptr used2:ptr flags:n :}
   flags SCOPE-FIND-AMBIGUOUS and 0<> if E-USING-AMBIGUOUS throw then
   used XREF-FOUND? if SHADOWED then ;

\ The token's record lands in REC, XREF-NULL on a miss. FIND's ambiguity is
\ the engine's refusal.
: SEARCH ( -- )
   XREF-NULL REC !
   [: LOOKUP-GO ;] catch {: code:n :}
   code E-USING-AMBIGUOUS = if AMBIGUOUS then
   code 0<> if code throw then ;

\ A word to run: a miss is undefined.
: LOOKUP ( -- )
   SEARCH
   REC @ XREF-FOUND? 0= if UNDEFINED then ;

: MIN-IN ( n -- n )
   DNAME-MIN-IN-MASK and MIN-IN-SHIFT rshift ;

\ The gates before a record's xt leaves the loop, in the engine's order: a
\ wide effect, then an internal word.
: XT-GATE ( -- )
   REC @ XREF-FLAGS {: f:n :}
   f DNAME-WIDE and 0<> if s" hb: interpret-mode layout value: " REFUSE then
   f DNAME-INT and 0<> if s" hb: internal engine word: " REFUSE then ;

\ Name dispatch and tick reject an exact native code entry, including an alias,
\ when it carries a baked scope kind. Arbitrary untyped xref xt execution is
\ outside these paths.
: SCOPE-ENTRY-GUARD ( -- )
   REC @ XREF-START scope-kind? 0<> if
      s" hb: internal engine word: " REFUSE
   then ;

\ A word to run passes them, then has its certified inputs on the stack.
: GATE ( -- )
   XT-GATE
   SCOPE-ENTRY-GUARD
   depth REC @ XREF-FLAGS MIN-IN < if s" hb: interpret stack underdepth: " REFUSE then ;

\ LFIND's flag word as the hook reads it (layout.f TOP-EV-*): bit 0 found,
\ bit 1 immediate, bits 8-15 the certified inputs.
: WORD-FLAGS ( -- n )
   REC @ XREF-FLAGS {: f:n :}
   1 f DNAME-IMM and 0<> if 2 or then
   f MIN-IN 8 lshift or ;

\ ---- the source observer (habu2.f C-SOURCE-EXEC-BEFORE, EMIT-SOURCE-EXEC-FINISH) --
\ source-exec-run (habu1.f BSOURCEEXECRUN) pushes one frame on the native
\ stack, named by SOURCE-EXEC-CTX-CELL, and runs a continuation that evaluates
\ the owned range. The engine's evaluate arms the frame when the range it
\ opens is the owned one (habu1.f EMIT-EVAL-BODY); its loop calls the before
\ callback for a word about to run and for a tick's target (SOURCE-EXEC:WORD,
\ SOURCE-EXEC:TICK; numbers, strings and characters have no event) and the
\ after callback at the top of the next pass, and BSOURCEEXECRUN runs the
\ finish once more after its catch. This loop does the same with the same
\ fields, so the engine's predicates (source-exec-boundary?, -loader-cut?,
\ -position, the source-unit-run snapshot) read them unchanged. The fields the
\ engine stores and nothing reads (SX-OP, SX-REC, SX-ENTRY, SX-WRITES) are not
\ stored. The engine closes its PROT window around a callback; this loop never
\ holds one (definers.f: def-open and jit-open close their own).
4 constant OBSERVE-TICK          \ SOURCE-EXEC:TICK (src/habu/source-exec.f)
5 constant OBSERVE-WORD          \ SOURCE-EXEC:WORD

\ The frame of the loop now reading tokens, as EVAL-TOP-CELL holds it, 0 for
\ the frameless INTERPRET-OPEN. The engine tests SX-EVAL = EVAL-TOP-CELL; a
\ frameless loop nested in an action leaves EVAL-TOP-CELL the owner's, so
\ this loop compares its own frame.
variable LOOP-EVAL

\ The frame's callback cells hold raw execution tokens.
CAST: BEFORE-CALL ( n -- [ n n n n ptr u8 n -- n bool ] )
CAST: AFTER-CALL ( n -- [ n n n -- ] )

: OBSERVER ( -- ptr u8 )
   SOURCE-EXEC-CTX-CELL ADDR@ ;

: OWNED? ( ptr u8 -- bool ) {: ctx:ptr :}
   ctx SX-ARMED FR@ 0= if false exit then
   ctx SX-EVAL FR@ LOOP-EVAL @ = ;

\ Arm the frame when the range this evaluate opens is the owned one: the
\ frame, the depth floor and the return-stack floor (habu1.f EMIT-EVAL-BODY).
: OBSERVE-ARM ( ptr u8 n ptr u8 -- ) {: src:ptr len:n f:ptr :}
   OBSERVER dup NULL-PTR = if drop exit then {: ctx:ptr :}
   ctx SX-ARMED FR@ 0<> if exit then
   ctx SX-SRC FR@ src NULL-PTR - <> if exit then
   ctx SX-LEN FR@ len <> if exit then
   f NULL-PTR - ctx SX-EVAL FR!
   depth ctx SX-FLOOR FR!
   RSP-CELL CELL@ ctx SX-RFLOOR FR!
   1 ctx SX-ARMED FR! ;

\ Before an action (habu2.f C-SOURCE-EXEC-BEFORE): the depths, the before
\ callback with the token and input cells put back after it, then the action
\ is pending until the next pass. A callback that throws unwinds through the
\ loop's catch, as the engine's does.
: OBSERVE-BEFORE ( n n n -- ) {: op:n rec:n entry:n :}
   OBSERVER dup NULL-PTR = if drop exit then {: ctx:ptr :}
   ctx OWNED? 0= if exit then
   ctx SX-SUSPEND FR@ 0<> if exit then
   ctx SX-PENDING FR@ 0<> if exit then
   TKA-CELL CELL@ TKL-CELL CELL@ INP-CELL CELL@ INE-CELL CELL@ SRCLOC:INB-CELL CELL@
   {: tka:n tkl:n inp:n ine:n inb:n :}
   depth ctx SX-DEPTH FR!
   RSP-CELL CELL@ ctx SX-RDEPTH FR!
   1 ctx SX-SUSPEND FR!
   ctx SX-UNIT FR@ op rec entry TOKEN$ ctx SX-BEFORE FR@ BEFORE-CALL execute
   drop ctx SX-TICKET FR!
   tka TKA-CELL CELL!  tkl TKL-CELL CELL!  inp INP-CELL CELL!
   ine INE-CELL CELL!  inb SRCLOC:INB-CELL CELL!
   0 ctx SX-SUSPEND FR!
   1 ctx SX-PENDING FR! ;

\ The after callback: ( unit ticket 0 ). A quotation reads no local.
: AFTER-RUN ( -- )
   OBSERVER {: ctx:ptr :}
   ctx SX-UNIT FR@ ctx SX-TICKET FR@ 0 ctx SX-AFTER FR@ AFTER-CALL execute ;

\ At the top of a pass (habu2.f EMIT-SOURCE-EXEC-FINISH from LMAIN): the
\ pending action is done, so the depths and the cursor's offset go to the
\ frame and the after callback runs under catch, the input cells put back
\ after it. Its throw goes on to the loop's catch (the engine's BR EVALREC):
\ an owned loop always runs under EVAL-FRAMED-RANGE's frame.
: OBSERVE-FINISH ( -- )
   OBSERVER dup NULL-PTR = if drop exit then {: ctx:ptr :}
   ctx SX-PENDING FR@ 0= if exit then
   ctx OWNED? 0= if exit then
   TKA-CELL CELL@ TKL-CELL CELL@ INP-CELL CELL@ INE-CELL CELL@ SRCLOC:INB-CELL CELL@
   {: tka:n tkl:n inp:n ine:n inb:n :}
   0 ctx SX-PENDING FR!
   depth ctx SX-DEPTH FR!
   RSP-CELL CELL@ ctx SX-RDEPTH FR!
   inp ctx SX-SRC FR@ - ctx SX-POS FR!
   1 ctx SX-SUSPEND FR!
   [: AFTER-RUN ;] catch {: code:n :}
   tka TKA-CELL CELL!  tkl TKL-CELL CELL!  inp INP-CELL CELL!
   ine INE-CELL CELL!  inb SRCLOC:INB-CELL CELL!
   0 ctx SX-SUSPEND FR!
   code 0<> if code throw then ;

\ The unit hook, then the top-row hook, then the source observer, before the
\ word runs (habu2.f EM-INTERPRET-FIND). The flags and the xt are read before
\ any of them runs.
: RUN-FOUND ( -- )
   GATE
   WORD-FLAGS REC @ NHOST:SELECT-REC {: flags:n xt:n :}
   xt UNIT-EV-WORD UNIT-EVENT drop
   TOP-EV-WORD flags HOOK
   OBSERVE-WORD REC @ XREF-NULL - XREF-ADDR>INDEX xt OBSERVE-BEFORE
   xt execute-floor FLOORED ;

: RUN-WORD ( -- )
   LOOKUP RUN-FOUND ;

\ ---- keywords (habu2.f CF-ENTRY, LKWCMP) --------------------------------------------
\ A keyword is matched before the token is read as a number or a word, and it
\ reads the input after it itself. Each keyword table is a word that runs the
\ token when it is one of its rows and answers whether it was.

: FOLD ( n -- n ) {: c:n :}
   c [char] A [char] Z BETWEEN? if c CASE-BIT or exit then
   c ;

\ Whether the span a u spells kw, which is lowercase. As the engine's LKWCMP
\ does, only the span's A-Z are folded.
: FOLDED= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n kw:ptr v:n :}
   u v <> if false exit then
   u 0 ?do
      a i + c@ FOLD  kw i + c@ <> if false unloop exit then
   loop
   true ;

: TOKEN-IS? ( ptr u8 n -- bool )
   TOKEN$ 2swap FOLDED= ;

\ The token after a reader keyword kw. With the input at its end the refusal
\ names kw as the engine bakes it, lowercase whatever the token's case, at the
\ end's line (habu2.f C-DIE-KEYWORD-NAME).
: OPERAND ( ptr u8 n -- ) {: kw:ptr u:n :}
   TOKEN if exit then
   s" hb: reader keyword needs a name: " SAY
   kw u SAY RC-NO-NAME THROW-AT ;

\ ---- string literals (habu2.f C-ISDQ to C-EIDOTQ) ------------------------------------
\ A literal's text starts one byte past the keyword, past the blank that ended
\ it, and runs to a closing quote that INP then passes. A literal with no
\ closing quote refuses with INP still after the keyword, so the refusal names
\ the keyword's line. A literal a program keeps is copied into data space it
\ allots first, after its own refusals. allot's task-live guard then exits $4F
\ with no output while a task runs, and its DP-CHECK (habu1.f) refuses a literal
\ that does not fit, both before any of it is written, as the engine's copies
\ do.

: BAD-LITERAL ( -- )
   s" hb: bad string literal" SAY RC-BAD-LITERAL THROW-AT ;

: TOO-LONG ( -- )
   s" hb: counted string too long (max 255)" SAY RC-TOO-LONG THROW-AT ;

\ INP passes the closing quote at q.
: PAST ( ptr u8 -- )
   1 + INP-CELL ADDR! ;

: ROOM ( n -- ptr u8 )
   here swap allot ;

\ A plain literal's text.
: TEXT ( -- ptr u8 n )
   INP-CELL ADDR@ 1 + INE-CELL ADDR@ {: s:ptr e:ptr :}
   s begin dup e < if dup c@ QUOTE <> else false then while 1 + repeat
   {: q:ptr :}
   q e >= if BAD-LITERAL then
   q PAST
   s q s - ;

\ The bytes copied to the data space at here, which moves past them.
: COPY-HERE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u ROOM {: d:ptr :}
   a d u BYTE-COPY
   d u ;

: KEEP ( -- ptr u8 n )
   TEXT COPY-HERE ;

\ c": a count byte, then the text. INP has passed the quote when the length
\ is checked (habu2.f C-ICQ), so the refusal names the quote's line.
: COUNTED ( -- ptr u8 )
   TEXT {: a:ptr u:n :}
   u CSTR-MAX > if TOO-LONG then
   u 1 + ROOM {: d:ptr :}
   u d c!
   a d 1 + u BYTE-COPY
   d ;

\ ---- escaped literals (habu2.f EMIT-ESC-SCAN, EMIT-ESC-COPY) ------------------------
\ The byte an escape letter stands for (habu2.f C-ESC-DECODE-BASIC), and
\ whether it is one. `x` and `X` are not: HEX-STEP reads their two digits.
: ESC-BYTE ( n -- n bool )
   case
      QUOTE of QUOTE true endof
      [char] q of QUOTE true endof
      ESCAPE of ESCAPE true endof
      [char] a of 7 true endof
      [char] b of 8 true endof
      [char] e of 27 true endof
      [char] f of 12 true endof
      [char] l of NEWLINE true endof
      [char] n of NEWLINE true endof
      [char] r of 13 true endof
      [char] t of 9 true endof
      [char] v of 11 true endof
      [char] z of 0 true endof
      0 false rot
   endcase ;

\ A backslash at p, `x` and two hex digits before e.
: HEX-STEP ( ptr u8 ptr u8 -- n ptr u8 bool ) {: p:ptr e:ptr :}
   p 4 + e > if 0 p false exit then
   p 2 + c@ 16 DIGIT {: hi:n hi-ok:bool :}
   p 3 + c@ 16 DIGIT {: lo:n lo-ok:bool :}
   hi 4 lshift lo or  p 4 +  hi-ok lo-ok and ;

\ The byte the text at p stands for, where its spelling ends, and whether that
\ spelling is whole before e. A backslash makes the next byte an escape letter.
: ESC-STEP ( ptr u8 ptr u8 -- n ptr u8 bool ) {: p:ptr e:ptr :}
   p c@ ESCAPE <> if p c@ p 1 + true exit then
   p 1 + e >= if 0 p false exit then
   p 1 + c@ {: c:n :}
   c [char] x = c [char] X = or if p e HEX-STEP exit then
   c ESC-BYTE p 2 + swap ;

\ An escaped literal's start, its closing quote and the count of bytes it
\ decodes to. A bad escape refuses as a missing quote does, INP unmoved.
: ESC-SCAN ( -- ptr u8 ptr u8 n )
   INP-CELL ADDR@ 1 + INE-CELL ADDR@ {: s:ptr e:ptr :}
   s 0
   begin
      over e >= if BAD-LITERAL then
      over c@ QUOTE <>
   while
      {: p:ptr k:n :}
      p e ESC-STEP {: nx:ptr ok:bool :} drop
      ok 0= if BAD-LITERAL then
      nx k 1 +
   repeat
   {: q:ptr u:n :}
   s q u ;

\ The bytes the text from s to its quote q decodes to, written from d on. The
\ scan has proved every escape whole.
: ESC-COPY ( ptr u8 ptr u8 ptr u8 -- ) {: s:ptr q:ptr d:ptr :}
   s d
   begin over q < while
      {: p:ptr t:ptr :}
      p q ESC-STEP drop {: b:n nx:ptr :}
      b t c!
      nx t 1 +
   repeat
   2drop ;

: ESC-KEEP ( -- ptr u8 n )
   ESC-SCAN {: s:ptr q:ptr u:n :}
   q PAST
   u ROOM {: d:ptr :}
   s q d ESC-COPY
   d u ;

\ c\" checks the decoded length before INP moves (habu2.f C-EICQ), so its
\ refusal names the keyword's line.
: ESC-COUNTED ( -- ptr u8 )
   ESC-SCAN {: s:ptr q:ptr u:n :}
   u CSTR-MAX > if TOO-LONG then
   q PAST
   u 1 + ROOM {: d:ptr :}
   u d c!
   s q d 1 + ESC-COPY
   d ;

\ ---- running a literal ----------------------------------------------------------------
\ A literal a program keeps is program code: a quotation pushes it, then the
\ hook sees it with the keyword as its token, and the quotation runs under the
\ floor as a word does. The cells it leaves are the program's, so these views
\ state its effect only to project it to the number execute-floor takes.
CAST: STR-LITERAL ( [ -- ptr u8 n ] -- n )
CAST: CSTR-LITERAL ( [ -- ptr u8 ] -- n )
CAST: CELL-LITERAL ( [ -- n ] -- n )
\ A cell a definer takes from the program (definers.f DEF-CONSTANT).
CAST: CELL-TAKE ( [ n -- ] -- n )

: RUN-LITERAL ( n -- )
   execute-floor FLOORED ;

\ ---- the string keywords ---------------------------------------------------------------
\ `."` types its text straight from the input and allots nothing; `.\"` keeps
\ its decoded bytes, as the engine's C-EIDOTQ does.
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

\ ---- char (habu2.f C-CHAR) ---------------------------------------------------------------
\ The operand's first byte. No definition is open at top level, so no body
\ text captures the operand.
: FIRST-BYTE ( -- n )
   s" char" OPERAND
   TOKEN$ drop c@ ;

\ The hook sees the operand as the token.
: PUSH-CHAR ( -- )
   [: FIRST-BYTE TOP-EV-CHAR 0 HOOK ;] CELL-LITERAL RUN-LITERAL ;

\ ---- tick (habu2.f C-TICK) ---------------------------------------------------------------
\ The seal guard (habu2.f C-QUALIFY-SEAL-GUARD): once the engine is sealed, a
\ token qualified by a sealed package ends the process, the token its whole
\ diagnostic line. The qualifier is the first colon when it is at neither
\ edge, as DEF-QUALIFY reads it, but a second colon does not spare the token.
\ The sealed packages are the checker's list (src/core/checker.f
\ CHECKER-SEALED-PKG?, the declared mirror of the engine's own), which folds
\ case as the engine does. The exit is fail-closed, as C-SEAL-PACKAGE-FAIL's
\ is.
: SEAL-GUARD ( -- )
   SEAL-NDICT@ 0= if exit then
   TOKEN$ {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 1 < if exit then
   q 1+ u >= if exit then
   a q CHECKER-SEALED-PKG? 0= if exit then
   a u ENGINE-ERROR:SEAL-PACKAGE FAIL-CLOSED ;

\ The active checker owner supplies the trusted-only query, not a source name.
\ Mirror C-TRUSTED-TICK?'s cold-prefix and replacement-checker windows.
: TICK-OWNER@ ( n -- ptr u8 ) data-base + 0 ptr-field @ ;

: TICK-QUERY-READY? ( -- bool )
   SEAL-NDICT@ 0= if false exit then
   NCOMP-DISPATCH:BUILD-DEPTH-CELL CELL@ 0<> if false exit then
   NCOMP-DISPATCH:TARGET-DECL-CELL TICK-OWNER@ dup 0= if drop true exit then
   NCOMP-DISPATCH:DECL-CELL TICK-OWNER@ = ;

: TICK-QUERY ( -- n )
   NCOMP-DISPATCH:DECL-CELL TICK-OWNER@ dup 0= if drop 0 exit then
   NCOMP-DISPATCH:DECL-TRUSTED-TICK-OFF + CELL-VIEW @ ;

\ The owner record's trusted-tick field holds a raw execution token.
CAST: TICK-ACTION ( n -- [ ptr u8 n -- bool ] )

: TRUSTED-TICK? ( -- bool )
   TICK-QUERY-READY? 0= if false exit then
   TICK-QUERY dup 0= if drop false exit then
   TICK-ACTION TOKEN$ rot execute ;

\ The word ' names lands in REC past the xt gates. A tick runs nothing, so it
\ has no depth gate; a name no word has is undefined, as a word to run is.
: TICKED ( -- )
   s" '" OPERAND
   SEAL-GUARD
   LOOKUP
   XT-GATE
   SCOPE-ENTRY-GUARD
   TRUSTED-TICK? if s" hb: trusted-only tick: " REFUSE then ;

\ The observer sees the tick's target before the xt is pushed (habu2.f C-TICK).
: TICK-OBSERVE ( -- )
   OBSERVE-TICK REC @ XREF-NULL - XREF-ADDR>INDEX REC @ XREF-START OBSERVE-BEFORE ;

\ The hook sees the operand as the token and the record's flags.
: PUSH-XT ( -- )
   [: TICKED TICK-OBSERVE REC @ XREF-START TOP-EV-TICK WORD-FLAGS HOOK ;] CELL-LITERAL RUN-LITERAL ;

\ ---- the literal keywords -----------------------------------------------------------------
\ The engine's EM-INTERPRET-STRING-KEYWORDS, with `'` and `char` from its
\ EM-INTERPRET-DEFINE-KEYWORDS.
: LITERAL? ( -- bool )
   S\" s\q" TOKEN-IS? if PUSH-STR true exit then
   S\" c\q" TOKEN-IS? if PUSH-CSTR true exit then
   S\" .\q" TOKEN-IS? if TYPE-STR true exit then
   S\" s\\\q" TOKEN-IS? if PUSH-ESC-STR true exit then
   S\" c\\\q" TOKEN-IS? if PUSH-ESC-CSTR true exit then
   S\" .\\\q" TOKEN-IS? if TYPE-ESC-STR true exit then
   s" char" TOKEN-IS? if PUSH-CHAR true exit then
   s" '" TOKEN-IS? if PUSH-XT true exit then
   false ;

;package
