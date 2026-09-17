\ whitebox-engine-suite.f - the gate really handed this suite an unsealed engine.
\
\ Run: test/gate-stdlib-cases.f declares it WHITEBOX-SUITE, so the gate builds
\ test/whitebox-engine.f's image once and spawns THAT binary here. The
\ declaration is the only thing under test: if a whitebox suite is ever handed
\ the product engine instead, every case below fails, and it fails in the one
\ place that says why rather than inside whichever suite noticed first.
\
\ WHAT AN UNSEALED ENGINE IS, stated as three facts a sealed one denies. The
\ seal (src/core/internal-mark.f) marks every record with no checker-known
\ effect DNAME-INT, and habu2.f then fails closed on that flag in exactly two
\ dispatches: interpret-mode execution and tick. So the difference is visible
\ from source, and only from source - there is no flag word to ask.
\
\ USIGS is the engine's own signature arena (src/core/checker.f), a pre-hook
\ global with no checker effect: the canonical sealed name, and the one
\ test/internal-word-gate.f uses to pin the product's refusal.

require lib/test.f
require lib/engine-id.f

package WHITEBOX-ENGINE-SUITE

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

: RUN ( -- )
   s" tick of a sealed name compiles here" T-LABEL
   INTERNAL-XT 0 <> TTRUE

   s" and it is the engine's own record" T-LABEL
   INTERNAL-XT RESOLVED-XT T=

   \ The product engine is a separate binary that keeps its seal: this one is
   \ not it, and nothing here should ever be installed over it.
   s" the whitebox engine is not bin/hb" T-LABEL
   ENGINE-ID:PATH$ s" bin/hb" T$<>

   T-REPORT ;

RUN
;package
