\ judge-fuzz.f - the hand-run sweep of the code generator judge's differential
\ oracle.
\
\   bin/hb --load tools/judge-fuzz.f
\       Generate the full sweep of straight-line integer programs, compile every
\       one of them through BOTH code generators from one text, and run both on
\       the ends of the signed range and on generated inputs. Prints what it
\       compiled and checked, and exits non-zero on any disagreement.
\
\ WHY THIS IS A HAND RUN AND THE GATE MEMBER IS SMALLER. Nothing here reads a
\ clock, so the sweep is as reproducible at this size as at the gate's - it is
\ only longer. tools/judge-fuzz-test.f runs a small fixed number of the same
\ programs, from the same seed, in the same order, so the gate member is a prefix
\ of this sweep rather than a different test.
\
\ THE SEED IS A CONSTANT. The same programs come out on every host and in every
\ run, so a disagreement found here can be reproduced by anybody, and a green run
\ is a statement about a fixed set rather than about a lucky afternoon.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/judge/fuzz.f

package JUDGE-FUZZ-CLI

private

1 constant FAIL-RC

\ The sweep's size. Bigger than the gate member's for the only reason a hand run
\ exists: it costs more than seconds and finds more programs.
256 constant BODIES
32 constant RANDOMS

: REPORT ( -- )
   s" judge-fuzz: " type JUDGE-FUZZ:BODIES FMT:.U
   s"  program(s), " type JUDGE-FUZZ:CHECKS FMT:.U
   s"  answer(s) compared, " type JUDGE-FUZZ:MISMATCHES FMT:.U
   s"  disagreement(s)" type cr ;

public

: MAIN ( -- )
   BODIES RANDOMS JUDGE-FUZZ:RUN
   REPORT
   JUDGE-FUZZ:MISMATCHES 0<> if
      S\" judge-fuzz: the two code generators do not agree\n" FAIL-RC die
   then ;

;package

JUDGE-FUZZ-CLI:MAIN
