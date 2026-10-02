\ outer.f - the outer interpreter written in Habu: numbers, the dictionary
\ search, then the readers src/habu/interpret.f's loop reads a buffer with.

require lib/prelude.f
require lib/ieee754.f
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
\ OUTER:INTERPRET (src/habu/interpret.f) reads a buffer token by token with the
\ words below, as the engine's interpret loop does (habu2.f EM-COMMENT's LMAIN,
\ EM-INTERPRET-WORDS) for comments, the literal keywords (`s"`, `c"`, `."`,
\ their escaped forms, `char` and `'`), numbers and dictionary words, and with
\ src/habu/packages.f for the package keywords (`package`, `public`, `private`,
\ `;package`, `using`, `;using` and `export`), and with src/habu/definers.f for
\ the definition heads (`:`, `kernel:` and `trusted:`) and the body one opens.
\ The engine's other keywords (`;`, `create`, ...) are not read yet: a body
\ captures them as it captures any token, and elsewhere they are not dictionary
\ words, so they refuse as undefined.
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
74 constant RC-BAD-LITERAL       \ habu2.f C-QUOTE-EOF: no closing quote, or a bad escape
74 constant RC-NO-NAME           \ habu2.f C-DIE-KEYWORD-NAME: a keyword's operand is missing
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

\ The engine's compile-die tail (habu2.f LCOMPILEDIE) after a site's message:
\ the location, a newline, then the site's code, thrown as the engine throws
\ it inside evaluate.
: THROW-AT ( n -- )
   AT-SOURCE NL 1 SAY throw ;

: AMBIGUOUS ( -- )
   s" hb: ambiguous bare word resolves in multiple used packages: " SAY
   TOKEN$ SAY ENGINE-ERROR:USING-AMBIGUOUS THROW-AT ;

\ ---- fail-closed exits ----------------------------------------------------------
\ Where the engine ends the process instead of throwing (NR-EXIT-GROUP), the
\ text goes to descriptor 2 and no program code runs: the exit hook
\ (src/habu/layout.f EXIT-HOOK-CELL) is cleared before `die`, which would
\ otherwise call it.
: FAIL-CLOSED ( ptr u8 n n -- ) {: a:ptr u:n rc:n :}
   a u SAY
   0 EXIT-HOOK-CELL CELL!
   s" " rc die ;

\ A keyword that changes the dictionary or its scope while a task is live
\ ends the process, the keyword its whole diagnostic (habu2.f
\ C-TASK-LIVE-GUARD).
: TASK-GUARD ( -- )
   TASKS-LIVE-CELL CELL@ 0= if exit then
   TOKEN$ RC-TASK-LIVE FAIL-CLOSED ;

\ Whether wordlist wid is protected (habu1.f EMIT-PROTWID, read as
\ tools/prot-wid-probe.f MEMBER? reads it): the two engine-reserved wordlists
\ by rule, then the wid's bit in the bitmap at PROT-BITS-OFF, which no wid
\ outside [0, PROT-WID-MAX) has.
: PROTECTED? ( n -- bool ) {: wid:n :}
   wid OWNER-API-PUB-WID = wid OWNER-API-PRI-WID = or if true exit then
   wid 0 < wid PROT-WID-MAX >= or if false exit then
   wid 6 rshift cells PROT-BITS-OFF + CELL@
   wid 63 and rshift 1 and 0<> ;

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

\ The xt waits on the return stack while the hook runs.
TRUSTED: RUN-WORD ( -- )
   LOOKUP GATE
   REC @ XREF-START >r
   TOP-EV-WORD WORD-FLAGS HOOK
   r> execute-floor FLOORED ;

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

: KEEP ( -- ptr u8 n )
   TEXT {: a:ptr u:n :}
   u ROOM {: d:ptr :}
   a d u BYTE-COPY
   d u ;

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

\ ---- the string keywords ---------------------------------------------------------------
\ A literal a program keeps is pushed, then the hook sees it with the keyword
\ as its token. `."` types its text straight from the input and allots
\ nothing; `.\"` keeps its decoded bytes, as the engine's C-EIDOTQ does.
TRUSTED: PUSH-STR ( -- )
   KEEP TOP-EV-STR 0 HOOK ;

TRUSTED: PUSH-CSTR ( -- )
   COUNTED TOP-EV-CSTR 0 HOOK ;

: TYPE-STR ( -- )
   TEXT type ;

TRUSTED: PUSH-ESC-STR ( -- )
   ESC-KEEP TOP-EV-STR 0 HOOK ;

TRUSTED: PUSH-ESC-CSTR ( -- )
   ESC-COUNTED TOP-EV-CSTR 0 HOOK ;

: TYPE-ESC-STR ( -- )
   ESC-KEEP type ;

\ ---- char (habu2.f C-CHAR) ---------------------------------------------------------------
\ The operand's first byte. No definition is open at top level, so no body
\ text captures the operand.
: FIRST-BYTE ( -- n )
   s" char" OPERAND
   TOKEN$ drop c@ ;

\ The hook sees the operand as the token.
TRUSTED: PUSH-CHAR ( -- )
   FIRST-BYTE TOP-EV-CHAR 0 HOOK ;

\ ---- tick (habu2.f C-TICK) ---------------------------------------------------------------
\ The seal guard (habu2.f C-QUALIFY-SEAL-GUARD): once the engine is sealed, a
\ token qualified by a sealed package ends the process, the token its whole
\ diagnostic. The qualifier is the first colon when it is at neither edge, as
\ FIND-SPLIT reads it, but a second colon does not spare the token. The sealed
\ packages are the checker's list (src/core/checker.f CHECKER-SEALED-PKG?, the
\ declared mirror of the engine's own), which folds case as the engine does.
\ The exit is fail-closed, as C-SEAL-PACKAGE-FAIL's is.
: SEAL-GUARD ( -- )
   SEAL-NDICT@ 0= if exit then
   TOKEN$ {: a:ptr u:n :}
   a u 0 FIND-COLON {: q:n :}
   q 1 < if exit then
   q 1+ u >= if exit then
   a q CHECKER-SEALED-PKG? 0= if exit then
   a u ENGINE-ERROR:SEAL-PACKAGE FAIL-CLOSED ;

\ The active checker owner supplies the trusted-only query, not a source name.
\ Mirror C-TRUSTED-TICK?'s cold-prefix and replacement-checker windows.
TRUSTED: TICK-OWNER@ ( n -- ptr u8 ) data-base + 0 ptr-field @ ;

: TICK-QUERY-READY? ( -- bool )
   SEAL-NDICT@ 0= if false exit then
   NCOMP-DISPATCH:BUILD-DEPTH-CELL CELL@ 0<> if false exit then
   NCOMP-DISPATCH:TARGET-DECL-CELL TICK-OWNER@ dup 0= if drop true exit then
   NCOMP-DISPATCH:DECL-CELL TICK-OWNER@ = ;

: TICK-QUERY ( -- n )
   NCOMP-DISPATCH:DECL-CELL TICK-OWNER@ dup 0= if drop 0 exit then
   NCOMP-DISPATCH:DECL-TRUSTED-TICK-OFF + CELL-VIEW @ ;

TRUSTED: TICK-ACTION ( n -- [ ptr u8 n -- bool ] ) ;

: TRUSTED-TICK? ( -- bool )
   TICK-QUERY-READY? 0= if false exit then
   TICK-QUERY dup 0= if drop false exit then
   TICK-ACTION TOKEN$ rot execute ;

\ Whether ' names a word, which then passes the xt gates. A tick runs nothing,
\ so it has no depth gate, and a name no word has is a quiet miss.
: TICKED ( -- bool )
   s" '" OPERAND
   SEAL-GUARD
   SEARCH
   REC @ XREF-FOUND? dup if
      XT-GATE
      SCOPE-ENTRY-GUARD
      TRUSTED-TICK? if s" hb: trusted-only tick: " REFUSE then
   then ;

\ The hook sees the operand as the token and the record's flags.
TRUSTED: PUSH-XT ( -- )
   TICKED if REC @ XREF-START TOP-EV-TICK WORD-FLAGS HOOK then ;

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
