\ outer.f - the outer interpreter written in Habu: numbers, the dictionary
\ search, then the loop that reads a buffer with them.

require lib/prelude.f
require lib/ieee754.f
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


\ The integer part converts as a signed cell, so it is at most MAX-N. The
\ value is int + frac / scale, negated last, in the engine's order.
: FLOAT-FINISH ( n n n bool bool -- n bool bool bool )
   {: int:n frac:n scale:n wrapped:bool neg:bool :}
   wrapped int MAX-N U> or if OUT-OF-RANGE exit then
   int s>f  frac s>f scale s>f f/  f+
   neg if fnegate then
   IEEE754:F64>BITS true true false ;


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
\ OUTER:FIND answers the record the engine's LFIND and LFINDUSED resolve for one
\ token (habu1.f EMIT-FIND, habu2.f EMIT-FIND-USED), or XREF-NULL. The caller
\ reads the immediate, wide, internal and min-in facts from the record's
\ XREF-FLAGS, as LFIND's flag output folds them.
\
\ The order. A bare token is asked of the open package's private wordlist, then
\ its public one, then the global one; with no package open, of the global one
\ alone. Only when that chain misses, and only for a token with no colon at all,
\ is every live used public asked: two distinct records there are
\ E-USING-AMBIGUOUS, and one package used twice is one record. NAME:tail asks
\ NAME's public wordlist, found through its namespace row.
\
\ Each wordlist is asked through xref-search-wl (habu1.f WLFIND), which walks the
\ dictionary's hash index under the key LFIND's own probe uses, so one index
\ answers both. The search stops at the first wordlist holding the name and
\ returns that row whatever its flags. `search-wl` would not do: it hides
\ DNAME-INT rows and the engine-helper wordlist, which the interpreter must see
\ in order to refuse them.
\
\ NDICT (src/compiler/native/dict.f) answers a different question: which record
\ a checked program may call. It skips a row the caller may not see and keeps
\ searching, so its chain is not this one; only its open-scope readers are.

package OUTER

private

\ ---- one wordlist -------------------------------------------------------------
\ The record, not an xt: one body can carry several names (EXPORT), so only the
\ record says which wordlist the name resolved in. Trusted-only by the
\ primitive's own row (prims.f).
TRUSTED: FIND-PROBE ( ptr u8 n n -- ptr n ) xref-search-wl ;

\ ---- the token's shape (habu1.f FIND-QSCAN to FIND-QTAILOK) ------------------
-1 constant FIND-BARE
-2 constant FIND-BAD

\ The index of the first colon at or after `from`, or -1.
: FIND-COLON ( ptr u8 n n -- n )
   {: a:ptr u:n from:n :}
   u from ?do
      a i + c@ $3A = if i unloop exit then
   loop
   -1 ;

\ The qualifier is the token's FIRST colon. One at either edge leaves the whole
\ token a bare name, whatever follows it: `:a:b` is bare. A second colon after
\ the qualifier misses. xref.f XREF-QUAL-INDEX calls `:a:b` malformed, so it is
\ not this reader.
: FIND-SPLIT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u 0 FIND-COLON {: q:n :}
   q 1 < if FIND-BARE exit then
   q 1+ u >= if FIND-BARE exit then
   a u q 1+ FIND-COLON 0 >= if FIND-BAD exit then
   q ;

\ ---- the open package, then the global wordlist (habu1.f FIND-DONE) ----------
\ A miss in the open package's private wordlist retries its public one, a miss
\ there retries the global one, and any other miss is final. The retry reads
\ the wordlist that missed, not the one the search began in, so P:tail while P
\ is open falls through to a global tail P does not export.
: FIND-OPEN ( ptr u8 n n -- ptr n )
   {: a:ptr u:n wid:n :}
   a u wid FIND-PROBE {: rec:ptr :}
   rec XREF-FOUND? if rec exit then
   NDICT:OPEN-PRI 0= if XREF-NULL exit then
   wid NDICT:OPEN-PRI = if a u NDICT:OPEN-PUB RECURSE exit then
   wid NDICT:OPEN-PUB = if a u 0 RECURSE exit then
   XREF-NULL ;

\ ---- the used publics (habu2.f EMIT-FIND-USED) --------------------------------
: FIND-USE-DEPTH ( -- n )
   data-base USE-DEPTH-CELL + @ ;

: FIND-USE-WID ( n -- n )
   cells data-base USE-WIDS-OFF + + @ ;

\ Every used public is asked, and the answers must agree on one record.
: FIND-USED ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u 0 FIND-COLON 0 >= if XREF-NULL exit then
   XREF-NULL
   FIND-USE-DEPTH 0 ?do
      a u i FIND-USE-WID FIND-PROBE
      dup XREF-FOUND? if
         over XREF-FOUND? if
            2dup <> if E-USING-AMBIGUOUS throw then
         then
         nip
      else
         drop
      then
   loop ;

: FIND-BARE-REC ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u NDICT:OPEN-PRI FIND-OPEN {: rec:ptr :}
   rec XREF-FOUND? if rec exit then
   a u FIND-USED ;

\ The namespace row carries its package's public wid where a word keeps its start.
: FIND-QUALIFIED ( ptr u8 n n -- ptr n )
   {: a:ptr u:n q:n :}
   a q XREF-NAMESPACE-WL FIND-PROBE {: ns:ptr :}
   ns XREF-FOUND? 0= if XREF-NULL exit then
   a q 1+ ZPTR+ u q - 1- ns XREF-PKG-PUBLIC FIND-OPEN ;

public

: FIND ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u FIND-SPLIT {: q:n :}
   q FIND-BAD = if XREF-NULL exit then
   q FIND-BARE = if a u FIND-BARE-REC exit then
   a u q FIND-QUALIFIED ;

;package

\ ---- the interpret loop ------------------------------------------------------
\ OUTER:INTERPRET reads a buffer token by token as the engine's interpret loop
\ does (habu2.f EM-COMMENT's LMAIN, EM-INTERPRET-NUMBER, EM-INTERPRET-FIND) for
\ comments, numbers and dictionary words. The keywords the engine dispatches
\ ahead of numbers (`:`, `s"`, `'`, `using`, ...) are not read here yet: they
\ are not dictionary words, so they refuse as undefined.
\
\ The input is the engine's own. The cursor, its end and the buffer start sit
\ in INP-CELL, INE-CELL and SRCLOC:INB-CELL, and the token in TKA-CELL and
\ TKL-CELL, so a parsing word a token runs reads this buffer, a nested load
\ nests, and a refusal names the token the engine would.
\
\ The data stack is the program's. Nothing of the loop's is on it while a
\ word or the top-row hook runs, so the words that run them declare no locals
\ and keep their state in cells or, across the hook, on the return stack.

package OUTER

private

70 constant RC-REJECT            \ habu2.f RC-REJECT: a refused token, catchable
52 constant MIN-IN-SHIFT         \ layout.f DNAME-MIN-IN-MASK: record flag bits 52-59
32 constant BLANK                \ LTOK: every byte at or below it separates tokens
$0A constant NEWLINE
$5C constant LINE-COMMENT        \ a backslash
$28 constant OPEN-COMMENT        \ (
$29 constant CLOSE-COMMENT       \ )
2 constant ERR-FD

create NL NEWLINE c,

\ ---- the engine's cells --------------------------------------------------------
: CELL@ ( n -- n )
   data-base + @ ;

: CELL! ( n n -- )
   data-base + ! ;

\ The input cells hold byte addresses as integers (habu1.f B-EVAL, LTOK). These
\ two are outer.f's one crossing between such a cell and a byte pointer.
TRUSTED: ADDR@ ( n -- ptr u8 ) data-base + @ ;
TRUSTED: ADDR! ( ptr u8 n -- ) data-base + ! ;

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

\ ---- comments (habu2.f EM-COMMENT) -----------------------------------------------
\ A backslash or `(` opens a comment only as a whole one-byte token. The comment
\ runs past the next newline or `)`, or to the end of the input.
: SKIP-PAST ( n -- ) {: c:n :}
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:ptr e:ptr :}
   p begin dup e < if dup c@ c <> else false then while 1 + repeat
   dup e < if 1 + then
   INP-CELL ADDR! ;

: COMMENT? ( -- bool )
   TKL-CELL CELL@ 1 <> if false exit then
   TKA-CELL ADDR@ c@ {: b:n :}
   b LINE-COMMENT = if NEWLINE SKIP-PAST true exit then
   b OPEN-COMMENT = if CLOSE-COMMENT SKIP-PAST true exit then
   false ;

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

: AMBIGUOUS ( -- )
   s" hb: ambiguous bare word resolves in multiple used packages: " SAY
   TOKEN$ SAY AT-SOURCE NL 1 SAY
   ENGINE-ERROR:USING-AMBIGUOUS throw ;

\ ---- running program code -------------------------------------------------------
\ A word or the hook runs through execute-floor, which answers whether it left
\ the stack below its base and then resets the stack to the base: the floor the
\ engine checks after every token. The prim's row states no effect for the xt,
\ so only a trusted body calls it.
: FLOORED ( bool -- )
   if s" E-UNDERFLOW: " REFUSE then ;

\ ---- the top-row hook (habu2.f LTOPHOOK) ------------------------------------------
\ With a hook installed (set-top-check) it gets ( token class flags ) for each
\ number after the push and for each word, past its gates, before it runs. The
\ hook consumes those four cells, as the event protocol states. A hook that
\ leaves the stack below its base on a word event is refused here, before the
\ word runs; the engine runs the word first and faults in the guard page (rc
\ 102), so this loop names the underflow where the engine would crash.
: HOOK@ ( -- n )
   TOP-HOOK-CELL CELL@ ;

TRUSTED: HOOK ( n n -- )
   HOOK@ 0= if 2drop exit then
   TOKEN$ 2swap HOOK@ execute-floor FLOORED ;

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
TYPED-VARIABLE REC ptr n

: LOOKUP-GO ( -- )
   TOKEN$ FIND REC ! ;

\ The token's record lands in REC. FIND's ambiguity is the engine's refusal,
\ and a miss is undefined.
: LOOKUP ( -- )
   XREF-NULL REC !
   [: LOOKUP-GO ;] catch {: code:n :}
   code E-USING-AMBIGUOUS = if AMBIGUOUS then
   code 0<> if code throw then
   REC @ XREF-FOUND? 0= if UNDEFINED then ;

: MIN-IN ( n -- n )
   DNAME-MIN-IN-MASK and MIN-IN-SHIFT rshift ;

\ The engine's gates in its order: a wide effect, an internal word, then fewer
\ cells on the stack than the word's certified inputs.
: GATE ( -- )
   REC @ XREF-FLAGS {: f:n :}
   f DNAME-WIDE and 0<> if s" hb: interpret-mode layout value: " REFUSE then
   f DNAME-INT and 0<> if s" hb: internal engine word: " REFUSE then
   depth f MIN-IN < if s" hb: interpret stack underdepth: " REFUSE then ;

\ LFIND's flag word as the hook reads it (layout.f TOP-EV-*): bit 0 found,
\ bit 1 immediate, bits 8-15 the certified inputs.
: WORD-FLAGS ( -- n )
   REC @ XREF-FLAGS {: f:n :}
   1 f DNAME-IMM and 0<> if 2 or then
   f MIN-IN 8 lshift or ;

\ The xt waits on the return stack while the hook runs.
TRUSTED: RUN-WORD ( -- )
   LOOKUP GATE
   REC @ XREF-START >r
   TOP-EV-WORD WORD-FLAGS HOOK
   r> execute-floor FLOORED ;

\ ---- the loop -------------------------------------------------------------------------
\ A number is pushed and a word run: the token's effect on the stack is the
\ program's, so this row, and every row above it, states none of it.
TRUSTED: DISPATCH ( -- )
   NUMERAL? if VALUE @ TOP-EV-NUM 0 HOOK exit then
   RUN-WORD ;

: STEP ( -- )
   COMMENT? if exit then
   DISPATCH ;

: RUN ( -- )
   begin TOKEN while STEP repeat ;

public

\ Interpret the buffer as the engine's evaluate reads it, and put the input
\ cells back after, whether the buffer ends or a token throws.
: INTERPRET ( ptr u8 n -- ) {: a:ptr u:n :}
   INP-CELL CELL@ INE-CELL CELL@ SRCLOC:INB-CELL CELL@ {: p:n e:n b:n :}
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   [: RUN ;] catch {: code:n :}
   p INP-CELL CELL!  e INE-CELL CELL!  b SRCLOC:INB-CELL CELL!
   code 0<> if code throw then ;

;package
