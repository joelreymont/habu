\ codegen-workload-test.f - the scheduled half of the end-to-end workload
\ measurement. Run: bin/hb --load tools/codegen-workload-test.f
\
\ NOT ONE ASSERTION HERE READS A CLOCK, and that is what makes it safe to
\ schedule. The workload report's numbers are timings, and a timing fails for
\ host load; the standing rule is that the scheduled suites hold facts about
\ compiled code and the hand-run report holds the measurements. So this file
\ never calls CODEGEN-RUN:MEASURE. What it checks is everything the report's
\ numbers would be MEANINGLESS without:
\
\   the inline rule           the predicate that says whether the engine copies
\                             a body is checked against the engine's own
\                             behaviour, on fixtures built to break it in each
\                             direction: a body one instruction under the limit,
\                             the same body one instruction over, and a body
\                             under the limit that contains a branch.
\   the call count            checked on the same fixtures, in both directions:
\                             a copied body has no call site anywhere, a called
\                             one has the site its caller was compiled with.
\   the wiring                every arm's driver enters ITS OWN column's word and
\                             NOT the other column's. This is the mutation the
\                             whole measurement turns on: an after-arm still
\                             calling the before-arm's record would run old code
\                             under a new name, report a delta of nothing, and
\                             look perfectly healthy.
\   the answers               the two arms of every workload compute the same
\                             value, and that value is pinned. Two arms that
\                             disagree ran different programs.
\   the bodies                the two arms of a workload are compiled from ONE
\                             string, so their machine code has to be the same
\                             size when the subjects behind it are: a name lives
\                             in a dictionary record and not in a body.
\
\ THE FIXTURES ARE BUILT TO FOOL THE SCAN, not merely to agree with it. The
\ text-versus-structure trap is the one that matters here: a driver whose SOURCE
\ names two words and whose compiled code contains no call at all, because the
\ engine copied both bodies in. A check that searched the source for a call would
\ pass that; the check here reads the emitted instructions.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require tools/codegen-workload-scan.f
require tools/codegen-workload-run.f

package CODEGEN-WORKLOAD-TEST

private

\ ---- fixtures for the inline rule -------------------------------------------
\ The engine copies a body of at most INL-MAX bytes that contains no branch. The
\ three fixtures below sit either side of both halves of that rule, so the
\ predicate cannot pass by answering the same way for everything.
\
\ Each is published by the same route the workload's subjects are, and the
\ CALLER of each is compiled afterwards - which is the only moment the engine's
\ decision is made and recorded.

: SMALL$ ( -- ptr u8 n )
   s" : FX-SMALL ( n -- n ) 7 and ;" ;

\ The same shape, made long enough that its body passes the limit. Each `1 +`
\ is more instructions; a dozen of them takes a body of a few instructions well
\ past forty bytes.
: BIG$ ( -- ptr u8 n )
   s" : FX-BIG ( n -- n ) 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 7 and ;" ;

\ Small, and with a branch in it. The branch alone is what stops the copy: the
\ engine refuses to move any pc-relative instruction, so this one is a real call
\ however short it is.
: BRANCHY$ ( -- ptr u8 n )
   s" : FX-BRANCH ( n -- n ) dup 7 > if drop 0 then ;" ;

public

: PUBLISH-FIXTURES ( -- )
   SMALL$ CODEGEN-HOT:EV
   BIG$ CODEGEN-HOT:EV
   BRANCHY$ CODEGEN-HOT:EV ;

;package

\ The three fixtures, and then the one caller that names all three. The caller
\ has to be compiled AFTER them: the engine decides call-or-copy while it
\ compiles a caller, so a caller compiled first would call all three and the
\ fixtures would prove nothing.
package FIXTURE
public
CODEGEN-WORKLOAD-TEST:PUBLISH-FIXTURES
: FX-CALLER ( n -- n )
   FX-SMALL FX-BIG FX-BRANCH ;
;package

package CODEGEN-WORKLOAD-TEST

private

using CODEGEN-SCAN

\ ---- the engine's rule, against the engine ----------------------------------
: RULE-CASES ( -- )
   s" a small straight-line body is one the engine copies into its caller" T-LABEL
   s" FIXTURE:FX-SMALL" ENGINE-COPIES? TTRUE
   s" FIXTURE:FX-SMALL" COPY-BYTES 0 > TTRUE

   s" the same shape past the size limit is a call" T-LABEL
   s" FIXTURE:FX-BIG" ENGINE-COPIES? TFALSE
   s" FIXTURE:FX-BIG" COPY-BYTES 0 T=

   s" and a small body with a branch in it is a call too" T-LABEL
   s" FIXTURE:FX-BRANCH" ENGINE-COPIES? TFALSE
   s" FIXTURE:FX-BRANCH" WORD-BYTES  s" FIXTURE:FX-BIG" WORD-BYTES  < TTRUE

   s" a copied body has no call instruction anywhere pointing at it" T-LABEL
   s" FIXTURE:FX-SMALL" CALL-SITES 0 T=
   s" FIXTURE:FX-SMALL" CALLERS-OF 0 T=

   s" and each called one has the site its caller was compiled with" T-LABEL
   s" FIXTURE:FX-CALLER" s" FIXTURE:FX-BIG" CALLS? TTRUE
   s" FIXTURE:FX-CALLER" s" FIXTURE:FX-BRANCH" CALLS? TTRUE
   s" FIXTURE:FX-CALLER" s" FIXTURE:FX-SMALL" CALLS? TFALSE

   s" so the caller's own code holds exactly the two calls it makes" T-LABEL
   s" FIXTURE:FX-CALLER" BLS-IN 2 T= ;

\ ---- the surveyed hot words of the live engine ------------------------------
\ These are facts about the engine bin/hb is running, and they are the reason
\ the compile-shaped workload's delta is nothing: the checker's smallest hot
\ words are copied into every one of their callers, and the ones that ARE called
\ are called from code that was compiled into this binary and is never
\ recompiled.
: SURVEY-CASES ( -- )
   s" the checker's two smallest hot words are copied, never called" T-LABEL
   s" TAG" ENGINE-COPIES? TTRUE
   s" PAY" ENGINE-COPIES? TTRUE
   s" TAG" CALL-SITES 0 T=
   s" PAY" CALL-SITES 0 T=

   s" the fold the checker runs per byte is too big to copy, so it is called" T-LABEL
   s" SYM-FOLD-C" ENGINE-COPIES? TFALSE
   s" SYM-FOLD-C" CALL-SITES 0 > TTRUE

   s" and so is the type-variable walk, because it is a loop" T-LABEL
   s" T-RES-WALK" ENGINE-COPIES? TFALSE
   s" T-RES-WALK" CALL-SITES 0 > TTRUE ;

\ ---- the subjects -----------------------------------------------------------
: SUBJECT-CASES ( -- )
   s" the chain compiled every subject smaller than the engine did" T-LABEL
   s" HOT-CHAIN:FOLD-C" WORD-BYTES    s" HOT-ENGINE:FOLD-C" WORD-BYTES    < TTRUE
   s" HOT-CHAIN:COUNT-CH" WORD-BYTES  s" HOT-ENGINE:COUNT-CH" WORD-BYTES  < TTRUE
   s" HOT-CHAIN:TERM-TAG" WORD-BYTES  s" HOT-ENGINE:TERM-TAG" WORD-BYTES  < TTRUE
   s" HOT-CHAIN:TERM-PAY" WORD-BYTES  s" HOT-ENGINE:TERM-PAY" WORD-BYTES  < TTRUE

   s" the two subjects with control flow are calls in both columns" T-LABEL
   s" HOT-ENGINE:FOLD-C" ENGINE-COPIES? TFALSE
   s" HOT-CHAIN:FOLD-C" ENGINE-COPIES? TFALSE
   s" HOT-ENGINE:COUNT-CH" ENGINE-COPIES? TFALSE
   s" HOT-CHAIN:COUNT-CH" ENGINE-COPIES? TFALSE

   s" and the two the engine inlines are copied in both columns" T-LABEL
   s" HOT-ENGINE:TERM-TAG" ENGINE-COPIES? TTRUE
   s" HOT-CHAIN:TERM-TAG" ENGINE-COPIES? TTRUE
   s" HOT-ENGINE:TERM-PAY" ENGINE-COPIES? TTRUE
   s" HOT-CHAIN:TERM-PAY" ENGINE-COPIES? TTRUE

   s" the control's subjects are the engine's code, byte for byte" T-LABEL
   s" HOT-FIXED:FOLD-C" WORD-BYTES   s" HOT-ENGINE:FOLD-C" WORD-BYTES   T=
   s" HOT-FIXED:COUNT-CH" WORD-BYTES s" HOT-ENGINE:COUNT-CH" WORD-BYTES T=
   s" HOT-FIXED:TERM-TAG" WORD-BYTES s" HOT-ENGINE:TERM-TAG" WORD-BYTES T= ;

\ ---- the wiring, in both directions -----------------------------------------
\ The mutation this whole measurement turns on. An arm that entered the other
\ column's record would time one code generator twice.
: WIRING-CASES ( -- )
   s" each calling arm enters its own column's word" T-LABEL
   s" WORKLOAD:SCAN-OLD" s" HOT-ENGINE:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:SCAN-NEW" s" HOT-CHAIN:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:COUNT-OLD" s" HOT-ENGINE:COUNT-CH" CALLS? TTRUE
   s" WORKLOAD:COUNT-NEW" s" HOT-CHAIN:COUNT-CH" CALLS? TTRUE

   s" and no arm enters the other column's" T-LABEL
   s" WORKLOAD:SCAN-OLD" s" HOT-CHAIN:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:SCAN-NEW" s" HOT-ENGINE:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:COUNT-OLD" s" HOT-CHAIN:COUNT-CH" CALLS? TFALSE
   s" WORKLOAD:COUNT-NEW" s" HOT-ENGINE:COUNT-CH" CALLS? TFALSE

   s" the control's two arms both enter the subject nothing migrated" T-LABEL
   s" WORKLOAD:SCAN-CTL-A" s" HOT-FIXED:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:SCAN-CTL-B" s" HOT-FIXED:FOLD-C" CALLS? TTRUE
   s" WORKLOAD:SCAN-CTL-B" s" HOT-CHAIN:FOLD-C" CALLS? TFALSE
   s" WORKLOAD:COUNT-CTL-B" s" HOT-CHAIN:COUNT-CH" CALLS? TFALSE

   s" a calling arm holds exactly one call, which is its subject's" T-LABEL
   s" WORKLOAD:SCAN-OLD" BLS-IN 1 T=
   s" WORKLOAD:SCAN-NEW" BLS-IN 1 T=
   s" WORKLOAD:COUNT-OLD" BLS-IN 1 T=
   s" WORKLOAD:COUNT-NEW" BLS-IN 1 T=

   s" and the arms over the inlined subjects hold none, in either column" T-LABEL
   s" WORKLOAD:TERM-OLD" BLS-IN 0 T=
   s" WORKLOAD:TERM-NEW" BLS-IN 0 T=
   s" WORKLOAD:TERM-CTL-A" BLS-IN 0 T=
   s" WORKLOAD:TERM-CTL-B" BLS-IN 0 T=

   s" so the after-arm over the inlined subjects is the smaller code" T-LABEL
   s" WORKLOAD:TERM-NEW" WORD-BYTES  s" WORKLOAD:TERM-OLD" WORD-BYTES  < TTRUE
   s" WORKLOAD:TERM-CTL-B" WORD-BYTES s" WORKLOAD:TERM-OLD" WORD-BYTES T= ;

\ ---- one body, two arms ------------------------------------------------------
\ A driver is published under a different name in each arm because two records
\ with one name in one wordlist is a duplicate definition. A name lives in the
\ dictionary record and not in the compiled body, so two arms whose subjects
\ compile to a call in both columns must come out the same number of bytes.
: BODY-CASES ( -- )
   s" the two arms of a calling workload are the same code size" T-LABEL
   s" WORKLOAD:SCAN-NEW" WORD-BYTES  s" WORKLOAD:SCAN-OLD" WORD-BYTES  T=
   s" WORKLOAD:SCAN-CTL-A" WORD-BYTES s" WORKLOAD:SCAN-OLD" WORD-BYTES T=
   s" WORKLOAD:SCAN-CTL-B" WORD-BYTES s" WORKLOAD:SCAN-OLD" WORD-BYTES T=
   s" WORKLOAD:COUNT-NEW" WORD-BYTES  s" WORKLOAD:COUNT-OLD" WORD-BYTES T=
   s" WORKLOAD:COUNT-CTL-B" WORD-BYTES s" WORKLOAD:COUNT-OLD" WORD-BYTES T= ;

;using

\ ---- the answers -------------------------------------------------------------
\ The generated data, and what each workload computes over it. These are pinned
\ so that "the two arms agree" is a statement about the code rather than about
\ two arms that both compute nothing.
: ANSWER-CASES ( -- )
   s" the generated data is the same bytes in every run" T-LABEL
   CODEGEN-HOT:BYTE-SUM 320399 T=
   CODEGEN-HOT:TERM-SUM 2211949911040 T=

   s" the two arms of each workload compute the same answer" T-LABEL
   CODEGEN-RUN:SCAN-OLD-SUM CODEGEN-RUN:SCAN-NEW-SUM T=
   CODEGEN-RUN:COUNT-OLD-SUM CODEGEN-RUN:COUNT-NEW-SUM T=
   CODEGEN-RUN:TERM-OLD-SUM CODEGEN-RUN:TERM-NEW-SUM T=

   s" and the control's two arms do too" T-LABEL
   CODEGEN-RUN:SCAN-CTL-A-SUM CODEGEN-RUN:SCAN-CTL-B-SUM T=
   CODEGEN-RUN:COUNT-CTL-A-SUM CODEGEN-RUN:COUNT-CTL-B-SUM T=
   CODEGEN-RUN:TERM-CTL-A-SUM CODEGEN-RUN:TERM-CTL-B-SUM T=

   s" and the answers are the pinned ones" T-LABEL
   CODEGEN-RUN:SCAN-OLD-SUM 355375 T=
   CODEGEN-RUN:COUNT-OLD-SUM 47 T=
   CODEGEN-RUN:TERM-OLD-SUM 276493745152 T= ;

\ ---- the compile-shaped row --------------------------------------------------
\ Its delta is a timing and is not checked here. What IS checked is the fact
\ without which the delta would mean nothing: both arms compiled the same amount.
\ A batch publishes one record per definition and one for the package it opens,
\ and an arm runs one untimed batch ahead of its timed ones.
: CHECK-ROW-CASES ( -- )
   CODEGEN-HOT:BATCH-DEFS 1+ CODEGEN-RUN:CHECK-ROUNDS 1+ * {: want:n :}
   s" the compile-shaped workload's two arms compiled the same amount" T-LABEL
   s" check-batch" CODEGEN-CLOCK:ROW-OF {: k:n :}
   k 0 >= TTRUE
   k CODEGEN-CLOCK:SAME-ANSWER? TTRUE
   k CODEGEN-CLOCK:OLD-SUM want T=
   k CODEGEN-CLOCK:NEW-SUM want T=

   s" and it is recorded as the one row whose arms could not be interleaved" T-LABEL
   k CODEGEN-CLOCK:INTERLEAVED? TFALSE ;

\ ---- the timing store's own refusals -----------------------------------------
\ An arm handed to a store with no row open. The body is a word rather than a
\ quotation written inline because the arm itself takes one, and the case has to
\ hand the whole call to `catch`.
: NOTHING ( -- ) ;

: ARM-WITH-NO-ROW ( -- )
   1 1 [: NOTHING ;] CODEGEN-CLOCK:ARM-OLD ;

\ The store is what every reported number comes out of, so the ways it can be
\ misused are checked rather than assumed. None of these reads a clock.
: STORE-CASES ( -- )
   s" a row index past the recorded count is refused" T-LABEL
   [: CODEGEN-CLOCK:ROWS CODEGEN-CLOCK:OLD-NS drop ;] CODEGEN-CLOCK:E-WLTIME-ROW TTHROWSQ
   [: -1 CODEGEN-CLOCK:NAME$ drop drop ;] CODEGEN-CLOCK:E-WLTIME-ROW TTHROWSQ

   s" an arm measured with no row open is refused" T-LABEL
   [: ARM-WITH-NO-ROW ;] CODEGEN-CLOCK:E-WLTIME-STATE TTHROWSQ
   [: CODEGEN-CLOCK:CLOSE ;] CODEGEN-CLOCK:E-WLTIME-STATE TTHROWSQ

   s" and a subject the dictionary does not hold is refused by the scan" T-LABEL
   [: s" CODEGEN-WORKLOAD-TEST:NO-SUCH-WORD" CODEGEN-SCAN:WORD-BYTES drop ;]
      CODEGEN-SCAN:E-WLSCAN-SUBJECT TTHROWSQ ;

public

: MAIN ( -- )
   T-RESET
   RULE-CASES
   SURVEY-CASES
   SUBJECT-CASES
   WIRING-CASES
   BODY-CASES
   ANSWER-CASES
   CHECK-ROW-CASES
   STORE-CASES
   T-REPORT
   s" codegen-workload-test: ok" type cr ;

;package

CODEGEN-WORKLOAD-TEST:MAIN
