\ dict-lookup-cost.f - what one compile-shaped batch costs at four dictionary
\ sizes. One concern: the driver behind the dot habu-compile-shaped-cost's
\ measurement, kept as a tool so the number can be produced again.
\
\   bin/hb --load tools/dict-lookup-cost.f
\
\ WHAT IT ANSWERS. tools/codegen-workload.f's compile-shaped row was shown to be
\ a measurement of the DICTIONARY rather than of a code generator: the same
\ generated source cost more the longer the dictionary was when it was compiled.
\ This prints that curve directly. Four points, each a batch of 40 checked
\ definitions compiled by the engine's own interpret path, each timed as the
\ FASTEST of several so a busy host can only move a point the wrong way; between
\ points the dictionary is grown with untimed batches of the same shape.
\
\ HOW TO READ IT. The slope - nanoseconds of batch per dictionary record - is the
\ number that matters. A lookup that scans the record table makes it positive and
\ roughly constant; a lookup that does not makes it indistinguishable from zero.
\ Nothing here fails: it is a measurement, not a gate.

require lib/errors.f
require lib/prelude.f
require lib/fmt.f
require tools/codegen-workload-hot.f

package DICTCOST

private

-7240 constant E-DICTCOST-ARMS  \ ran out of distinct batch package names

\ ---- batch coordinates ------------------------------------------------------
\ CODEGEN-HOT names each batch's package from an arm letter and a two-digit
\ round, and a name may be published once, so every batch this file compiles
\ needs its own pair. Rounds run 0..99 and then the arm letter advances.

100 constant ROUNDS-PER-ARM
26 constant ARM-LIMIT           \ A..Z; past that the generated name is not a letter

variable ARM
variable ROUND

: NEXT-COORD ( -- )
   ROUND @ 1+ ROUND !
   ROUND @ ROUNDS-PER-ARM < if exit then
   0 ROUND !
   ARM @ 1+ ARM !
   ARM @ ARM-LIMIT < if exit then
   E-DICTCOST-ARMS throw ;

: BATCH ( -- )
   ARM @ ROUND @ CODEGEN-HOT:CHECK-BATCH
   NEXT-COORD ;

: TIMED-BATCH ( -- n )
   mono-ns {: t0:n :}
   BATCH
   mono-ns t0 - ;

\ ---- one point --------------------------------------------------------------

: PAD-TO ( n -- ) {: target:n :}
   begin ndict@ target < while BATCH repeat ;

variable BEST

: KEEP-FASTEST ( n -- ) {: ns:n :}
   ns BEST @ < if ns BEST ! then ;

: FASTEST ( n -- n ) {: reps:n :}
   TIMED-BATCH BEST !
   reps 1 ?do TIMED-BATCH KEEP-FASTEST loop
   BEST @ ;

1000 constant NS-PER-US

: .ROW ( n n -- ) {: nd:n ns:n :}
   s" ndict " type  nd FMT:.INT
   s"   batch " type  ns NS-PER-US / FMT:.INT
   s" us" type  cr ;

public

5 constant POINT-REPS           \ timed batches per point; the fastest is the point

: POINT ( n -- ) {: target:n :}
   target PAD-TO
   ndict@ {: nd:n :}
   POINT-REPS FASTEST
   nd swap .ROW ;

\ The four sizes the dot measured, so the before and after curves are read off
\ the same abscissa.
: RUN ( -- )
   0 ARM !  0 ROUND !
   s" dict-lookup-cost: batch of " type
   CODEGEN-HOT:BATCH-DEFS FMT:.INT
   s"  checked definitions, fastest of " type
   POINT-REPS FMT:.INT  cr
   11340 POINT
   12240 POINT
   13100 POINT
   13960 POINT ;

;package
