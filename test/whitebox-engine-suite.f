\ whitebox-engine-suite.f - the gate really handed this suite an unsealed engine,
\ and the product engine beside it is still sealed.
\
\ Run: test/gate-stdlib-cases.f declares it WHITEBOX-SUITE, so the gate builds
\ test/whitebox-engine.f's image once and spawns THAT binary here. The
\ declaration is the only thing under test: if a whitebox suite is ever handed
\ the product engine instead, every case below fails, and it fails in the one
\ place that says why rather than inside whichever suite noticed first.
\
\ WHAT AN UNSEALED ENGINE IS, stated as facts a sealed one denies. The seal
\ (src/core/internal-mark.f) marks every record with no checker-known effect
\ DNAME-INT, and habu2.f then fails closed on that flag in exactly two
\ dispatches: interpret-mode execution and tick.
\
\ USIGS is the engine's own signature arena (src/core/checker.f), a pre-hook
\ global with no checker effect: the canonical sealed name, and the one
\ test/internal-word-gate.f uses to pin the product's refusal.
\
\ AND THE IMAGE IS ASKED WHAT IT IS. Behaviour proves the seal stood down;
\ ENGINE-INTERNAL:IMAGE-CLASS is the same fact written down by the pass that
\ decided it, which is what tools/native-build-core.f reads before it promotes a
\ binary. Both engines are asked here, because a class that answered `whitebox`
\ everywhere would satisfy the first half and mean nothing.

require lib/test.f
require lib/string.f
require lib/engine-id.f
require lib/engine-candidate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package WHITEBOX-ENGINE-SUITE

$1000 constant IO-CAP
30000 constant CHILD-TIMEOUT-MS

create OUT IO-CAP allot
create ERR IO-CAP allot

\ Compile-time tick of an internal word. habu2.f C-TICK carries the same
\ DNAME-INT gate interpret does, so on a sealed engine this definition does not
\ compile at all and the suite dies while loading - which is the failure this
\ file exists to produce.
TRUSTED: INTERNAL-XT ( -- n )
   ['] USIGS ;

\ Interpret-mode execution of the same word, through a name the engine resolves
\ at run time rather than one this file compiled.
: RESOLVED-XT ( -- n )
   s" USIGS" XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-START ;

\ The product engine, asked the same question in its own process. `bin/hb` by
\ name and not ENGINE-CANDIDATE:PATH$, because the point is the OTHER engine:
\ the candidate resolver would hand back this one.
: PRODUCT-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: CLASS-PROGRAM$ ( -- ptr u8 n )
   S\" ENGINE-INTERNAL:IMAGE-CLASS . cr\n" ;

: CLASS-OF$ ( ptr u8 n -- ptr u8 n ) {: eng:ptr engu:n :}
   eng engu >LEN
   CLASS-PROGRAM$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if s" " exit then
   OUT outu LEN>N ;

: PRODUCT-CLASS$ ( -- ptr u8 n )
   PRODUCT-ARGS
   s" bin/hb" CLASS-OF$ ;

\ THE CHILD FOLLOWS THIS ENGINE, AND AN OUTER VARIABLE CANNOT MOVE IT. Every
\ tool a suite forks asks lib/engine-candidate.f which engine to run, and that
\ resolver reads HABU_UNDER_TEST before it falls back to the running engine. A
\ CI that exports HABU_UNDER_TEST=<tree>/bin/hb - the ordinary way to point a
\ gate at a candidate - therefore used to send the children of a whitebox suite
\ to the sealed product while the suite itself ran here: thirteen suites went
\ red with the CHILD answering `hb: internal engine word: DECLARATIONS`.
\ test/gate-stdlib-lib.f names the item's own engine in the environment it hands
\ each suite, and this is the case that says so.
\
\ The spawned child carries HABU_UNDER_TEST pointing at the SEALED product, so
\ the assertion cannot be satisfied by the child resolving the variable for
\ itself: what is under test is the binary the resolver handed this process,
\ and it has to be the whitebox one whatever the environment says afterwards.
: CANDIDATE-CLASS$ ( -- ptr u8 n )
   PROC-ARGV-ENV-RESET
   s" HABU_UNDER_TEST" >LEN s" bin/hb" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ CLASS-OF$ ;

: RUN ( -- )
   s" tick of a sealed name compiles here" T-LABEL
   INTERNAL-XT 0 <> TTRUE

   s" and it is the engine's own record" T-LABEL
   INTERNAL-XT RESOLVED-XT T=

   s" this image says it is the whitebox one" T-LABEL
   ENGINE-INTERNAL:IMAGE-CLASS ENGINE-INTERNAL:IMAGE-WHITEBOX T=

   \ The product engine is a separate binary that keeps its seal: this one is
   \ not it, and nothing here should ever be installed over it.
   s" the whitebox engine is not bin/hb" T-LABEL
   ENGINE-ID:PATH$ s" bin/hb" T$<>

   s" and bin/hb says it is the sealed one" T-LABEL
   PRODUCT-CLASS$ S\" 0\n\n" T$=

   s" a child spawned the way every suite spawns one runs this engine" T-LABEL
   CANDIDATE-CLASS$ S\" 1\n\n" T$=

   T-REPORT ;

RUN
;package
