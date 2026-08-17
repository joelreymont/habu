\ checker-rollback-sig-pool.f - the signature pool's newest-row index across a
\ rollback pop.
\
\ WHAT THIS LOCKS. The pool keeps one cell per SYMBOL ID naming that symbol's
\ newest row (src/core/checker.f ASIG-LAST), and a checker rollback frame REWINDS
\ SYM-N, handing every id above the restored mark back to whatever interns next.
\ An entry left behind therefore answers for a DIFFERENT word: the audit's
\ CHECKER-ASIG-MISSING? reports a signature the pool has not got, and
\ CHECKER-ASIG-ROW-FOR hands a capture the wrong row - one carrying another
\ word's name, for an engine that has no such word. The tree already knew this
\ about the other sym-keyed table (HIDX-SYMS-RETIRE); RBF-POP-WITH now retires
\ both.
\
\ THE PATH IS REACHED TODAY. Every SUMTYPE / STRUCTURE / ENUM inside an armed
\ capture window runs src/core/sumtype.f TDPLAN-PREFLIGHT-CHECKER, which is
\ CHECKER-CANDIDATE-SCOPE-START, one CHECK! per generated definition, and
\ CHECKER-CANDIDATE-SCOPE-DONE. Those three words are what the cases below call:
\ the fixture drives the production entry points, it does not model them.
\
\ AND THE CASES BREAK THE LUCK ON PURPOSE. Left alone the chain re-interns the
\ same names in the same order, so the ids realign and the stale entry happens to
\ name the right word; the byte-identical dedup then drops the real row and
\ nobody notices. So each case interns ONE EXTRA symbol between the pop and the
\ real definitions, which is all it takes to hand the ghosts' ids to different
\ words, and gives the ghosts the same `( -- )` text the real words carry, so the
\ dedup would fire if a stale entry were consulted. Measured on the unfixed
\ engine: RBSIG-SHIM answered a row named `rbsig-ghost-a` and RBSIG-REAL-A a row
\ named `rbsig-ghost-b`, and the pool stayed at two rows for four words.
\
\ THE ROW COUNT IS THE SHARP ASSERTION. A word whose newest-row entry is stale
\ and whose text matches the stale row emits NOTHING, so the unfixed engine keeps
\ a pool of two where this one holds four. Reading the row's NAME back out of the
\ pool is the second half: a count could be right for the wrong reason, a name
\ cannot.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f \
\   test/checker-rollback-sig-pool.f

require lib/errors.f
require lib/string.f
require lib/test.f

package RBSIG-TEST
private

\ ---- reading the pool the way src/habu/aot-capture.f reads it ----------------
\ Through the five published readers, and with the store's own arithmetic: a row
\ is four little-endian u32 (name, sig, package, visibility) and a string is
\ `[len u16][bytes]`.

$0 constant R.NAME

: ROW-U32@ ( n -- n ) {: at:n :}
   at CHECKER-ASIG-ROW-C@
   at 1 + CHECKER-ASIG-ROW-C@ 8 lshift or
   at 2 + CHECKER-ASIG-ROW-C@ 16 lshift or
   at 3 + CHECKER-ASIG-ROW-C@ 24 lshift or ;

: STR-LEN ( n -- n ) {: at:n :}
   at CHECKER-ASIG-STR-C@  at 1 + CHECKER-ASIG-STR-C@ 8 lshift or ;

: STR-AT= ( n ptr u8 n -- bool ) {: at:n a:ptr u:n :}
   at STR-LEN u <> IF false EXIT THEN
   0 BEGIN dup u < WHILE
      dup at 2 + + CHECKER-ASIG-STR-C@  over a + c@ <> IF drop false EXIT THEN
      1 +
   REPEAT drop true ;

\ The pool is not open to a name search: this asks the store for the row it would
\ hand a capture for this word, and then reads that row's OWN name field back.
\ `want` is the interned spelling, which the symbol table lower-cases.
: ROW-NAMES? ( ptr u8 n ptr u8 n -- bool ) {: qa:ptr qu:n wa:ptr wu:n :}
   s" " false qa qu CHECKER-ASIG-ROW-FOR {: p:n :}
   p 0= IF false EXIT THEN
   p 1 - R.NAME + ROW-U32@  wa wu STR-AT= ;

: MISSING? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" " false a u CHECKER-ASIG-MISSING? ;

: HAS-ROW? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   s" " false a u CHECKER-ASIG-ROW-FOR 0 <> ;

\ Certify one definition through the same entry point the generated-declaration
\ preflight uses, and refuse to build a case on a body the checker rejected.
: CERT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u T-LABEL
   a u CHECK! -1 T= ;

\ ---- case one: the popped scope's rows may not answer for anybody ------------

: STALE-ROW-CASE ( -- )
   CHECKER-ASIG-ARM
   CHECKER-CANDIDATE-SCOPE-START
   s" RBSIG-GHOST-A ( -- )" CERT
   s" RBSIG-GHOST-B ( -- )" CERT
   CHECKER-CANDIDATE-SCOPE-DONE

   s" the rolled-back scope's rows stay in the pool" T-LABEL
   CHECKER-ASIG-N 2 T=
   s" ghost A's row answers nobody: its symbol is gone" T-LABEL
   s" RBSIG-GHOST-A" HAS-ROW? TFALSE
   s" ghost B's row answers nobody either" T-LABEL
   s" RBSIG-GHOST-B" HAS-ROW? TFALSE

   \ one extra symbol: the ghosts' ids now belong to the two words after it
   s" RBSIG-SHIM ( -- )" CERT
   s" RBSIG-REAL-A ( -- )" CERT

   s" every word defined after the pop wrote a row of its own" T-LABEL
   CHECKER-ASIG-N 4 T=
   s" the row a capture would take for RBSIG-SHIM names RBSIG-SHIM" T-LABEL
   s" RBSIG-SHIM"   s" rbsig-shim"   ROW-NAMES? TTRUE
   s" ... and RBSIG-REAL-A's names RBSIG-REAL-A" T-LABEL
   s" RBSIG-REAL-A" s" rbsig-real-a" ROW-NAMES? TTRUE
   s" RBSIG-SHIM does not answer ghost A's row, whose id it took" T-LABEL
   s" RBSIG-SHIM"   s" rbsig-ghost-a" ROW-NAMES? TFALSE
   s" ... and RBSIG-REAL-A does not answer ghost B's" T-LABEL
   s" RBSIG-REAL-A" s" rbsig-ghost-b" ROW-NAMES? TFALSE ;

\ ---- case two: the audit that has to fail loudly still can -------------------
\ A word the checker knows and the pool has no row for is the ONE condition a
\ capture must refuse. Outside the arming window every definition is that word,
\ so a stale entry there does not hand over a wrong row - it hands over a false
\ "this one is covered", which is the silent half of the same defect.

: AUDIT-CASE ( -- )
   CHECKER-ASIG-ARM
   CHECKER-CANDIDATE-SCOPE-START
   s" RBSIG-GHOST-C ( -- )" CERT
   s" RBSIG-GHOST-D ( -- )" CERT
   CHECKER-CANDIDATE-SCOPE-DONE
   CHECKER-ASIG-DISARM

   s" RBSIG-OUT-SHIM ( -- )" CERT
   s" RBSIG-OUT-A ( -- )" CERT

   s" the pool collected nothing after the window closed" T-LABEL
   CHECKER-ASIG-N 2 T=
   s" the audit calls RBSIG-OUT-SHIM missing, which it is" T-LABEL
   s" RBSIG-OUT-SHIM" MISSING? TTRUE
   s" ... and RBSIG-OUT-A too" T-LABEL
   s" RBSIG-OUT-A" MISSING? TTRUE ;

\ ---- case three: the retire may not reach below the frame --------------------
\ The mark is the frame's restored SYM-N, so a row written BEFORE the scope
\ opened is a row about a symbol that still exists and must still answer. A
\ retire that started at zero would pass case one and silently drop the whole
\ window.

: KEEP-CASE ( -- )
   CHECKER-ASIG-ARM
   s" RBSIG-KEEP ( -- n ) 0" CERT
   s" the row is the word's own before any scope opens" T-LABEL
   s" RBSIG-KEEP" s" rbsig-keep" ROW-NAMES? TTRUE

   CHECKER-CANDIDATE-SCOPE-START
   s" RBSIG-GHOST-E ( -- )" CERT
   CHECKER-CANDIDATE-SCOPE-DONE

   s" ... and it is still the word's own after the pop" T-LABEL
   s" RBSIG-KEEP" s" rbsig-keep" ROW-NAMES? TTRUE
   s" ... a pop retires the scope's rows and no others" T-LABEL
   CHECKER-ASIG-N 2 T= ;

public

: RUN ( -- )
   STALE-ROW-CASE
   AUDIT-CASE
   KEEP-CASE
   CHECKER-ASIG-DISARM          \ leave the process as this file found it
   T-REPORT
   s" checker-rollback-sig-pool: ok" type cr ;

;package

RBSIG-TEST:RUN
