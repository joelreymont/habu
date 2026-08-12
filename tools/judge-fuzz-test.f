\ judge-fuzz-test.f - the scheduled half of the code generator judge's
\ differential oracle.
\ Run: bin/hb --load tools/judge-fuzz-test.f
\
\ WHAT IT ASSERTS, AND WHY IT IS THE SAME WORDS THE HAND RUN RUNS. Everything
\ below goes through tools/judge/fuzz.f, which is what
\ `bin/hb --load tools/judge-fuzz.f` drives, from the same constant seed and in
\ the same order - so this member is a PREFIX of that sweep rather than a second
\ test that agrees with itself.
\
\   the sweep       a small fixed number of generated straight-line integer
\                   programs, each compiled by the engine and by the native chain
\                   from ONE text and run on the ends of the signed range and on
\                   generated inputs. Every answer is compared as a whole cell
\   the oracle      can SEE a difference: two texts that differ by one literal
\                   are handed to the two columns and every comparison must
\                   disagree. Without this the sweep above would pass just as
\                   well if the comparison were broken
\   the columns     are two routines: the two derived words resolve to two
\                   different non-zero addresses, so the sweep is not one
\                   compiler agreeing with itself
\
\ NOTHING HERE READS A CLOCK. The seed is a constant, the programs are the same
\ on every host, and no assertion is a duration - so this belongs in the parallel
\ group beside the other judge members.

require lib/errors.f
require lib/string.f
require lib/test.f
require tools/judge/src.f
require tools/judge/fuzz.f

package JUDGE-FUZZ-TEST

private

\ The gate's size: seconds, and a prefix of the hand run's sweep.
8 constant BODIES
8 constant RANDOMS

: SWEEP-CASES ( -- )
   BODIES RANDOMS JUDGE-FUZZ:RUN
   JUDGE-FUZZ:BODIES BODIES T=
   JUDGE-FUZZ:CHECKS BODIES JUDGE-FUZZ:BOUNDARY-N RANDOMS + * T=
   JUDGE-FUZZ:MISMATCHES 0 T= ;

\ ---- the oracle, attacked ------------------------------------------------------
\ A differential test that cannot see a difference passes every day and means
\ nothing. These two texts are the same program but for one literal, so the two
\ columns must disagree on every single input - and the count is asserted, not
\ just its being non-zero, because a comparison that fired once would look the
\ same as one that fired always.

: PLUS-ONE$ ( -- ptr u8 n )
   s" : JFX ( n -- n ) 1 + ;" ;

: PLUS-TWO$ ( -- ptr u8 n )
   s" : JFX ( n -- n ) 2 + ;" ;

: SEEING-CASES ( -- )
   JUDGE-FUZZ:MISMATCHES {: before:n :}
   JUDGE-FUZZ:CHECKS {: checked:n :}
   PLUS-ONE$ PLUS-TWO$ RANDOMS JUDGE-FUZZ:PAIR
   JUDGE-FUZZ:CHECKS checked JUDGE-FUZZ:BOUNDARY-N RANDOMS + + T=
   JUDGE-FUZZ:MISMATCHES before JUDGE-FUZZ:BOUNDARY-N RANDOMS + + T= ;

\ ---- what the oracle refuses ---------------------------------------------------
\ A text with no single subject has no program to be the two columns' program,
\ and a body the chain declines is a finding here rather than a row.

: NO-DEF$ ( -- ptr u8 n )
   s" 1 2 +" ;

: TWO-DEFS$ ( -- ptr u8 n )
   S\" : JFY ( n -- n ) 1 + ;\n: JFZ ( n -- n ) 2 + ;\n" ;

: REFUSAL-CASES ( -- )
   [: NO-DEF$ NO-DEF$ 0 JUDGE-FUZZ:PAIR ;] E-JUDGE-FUZZ-SOURCE TTHROWSQ
   [: TWO-DEFS$ TWO-DEFS$ 0 JUDGE-FUZZ:PAIR ;] E-JUDGE-FUZZ-SOURCE TTHROWSQ ;

\ What the generator claims about its programs, read back off one of them by the
\ production reader rather than described: a whole definition, one input and one
\ output, no callee and no storage - which is what makes them straight-line and
\ what makes a chain refusal a finding instead of a row.
: PROGRAM-CASES ( -- )
   JUDGE-FUZZ:BODY$ {: a:ptr u:n :}
   a 2 s" : " T$=
   a u + 2 -  2  s"  ;" T$=
   a u JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 T=
   0 JUDGE-SRC:IN 1 T=
   0 JUDGE-SRC:OUT 1 T=
   0 JUDGE-SRC:CALLS 0 T=
   0 JUDGE-SRC:USES 0 T= ;

public

: RUN ( -- )
   T-RESET
   SWEEP-CASES
   SEEING-CASES
   REFUSAL-CASES
   PROGRAM-CASES
   T-REPORT ;

;package

JUDGE-FUZZ-TEST:RUN
