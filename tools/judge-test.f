\ judge-test.f - the scheduled half of the code generator judge.
\ Run: bin/hb --load tools/judge-test.f
\
\ WHAT IT ASSERTS, AND WHY IT IS THE SAME WORDS THE COMMAND RUNS. Everything
\ below goes through tools/judge/check.f, which is what
\ `bin/hb --load tools/judge.f -- --check` drives, so a green run here is the
\ command passing rather than a second implementation of it agreeing with
\ itself.
\
\   the artifact          this tree's judgement and the committed file are the
\                         same bytes
\   no larger row         no subject the chain compiled into more bytes than the
\                         engine's emitter wrote for the same body
\   the two refusals      are E-A64RA-SPILL and nothing else. The old comparison
\                         named these two subjects on a list; here the code is
\                         checked, so a refusal that changed its reason would
\                         fail even while the row still read REFUSED
\   the derived text      is the corpus file's own program, checked on the row
\                         whose body the chain refuses - the one row whose text
\                         no compiled artifact can vouch for
\
\ NOTHING HERE READS A CLOCK. Every column of the artifact is exact: a byte
\ count off a dictionary record, a refusal code from the compiler, and a verdict
\ that follows from the two. That is what lets this run in a parallel group
\ beside the other codegen members.

require lib/errors.f
require lib/string.f
require lib/test.f
require tools/judge/check.f

package JUDGE-TEST

private

: SPILL-ROWS ( -- n )
   0
   JUDGE-ROW:ROWS 0 ?do
      i JUDGE-ROW:REFUSED? if
         i JUDGE-ROW:NEW-RC@ E-A64RA-SPILL = if 1+ then
      then
   loop ;

: ROW-OF ( ptr u8 n -- n )
   JUDGE-ROW:FIND ;

: ARTIFACT-CASES ( -- )
   JUDGE-CHECK:DIFF-AT -1 T=
   JUDGE-ROW:LARGER-ROWS 0 T= ;

\ Every refusal in the table is the allocator declining to spill inside a loop,
\ which is dot habu-spill-from-a-4145325c. A refusal for some other reason is a
\ different finding and must not read as this one.
: REFUSAL-CASES ( -- )
   JUDGE-ROW:REFUSED-ROWS SPILL-ROWS T=
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" ROW-OF JUDGE-ROW:NEW-RC@ E-A64RA-SPILL T=
   s" CODEGEN-CORPUS4:CALL-PRESSURE" ROW-OF JUDGE-ROW:NEW-RC@ E-A64RA-SPILL T= ;

\ A compiled row vouches for its own text: the chain read it, the checker
\ certified it and a routine came out. A REFUSED row has no such witness, so the
\ text the chain was handed is checked here against the corpus's own program.
: TEXT-CASES ( -- )
   s" PRESSURE-LOOP" JUDGE-SRC:FIND s" -J4" JUDGE-SRC:TEXT$
   S\" : PRESSURE-LOOP-J4 ( ptr n n -- n ) {: base:ptr len:n :}\n   0\n   len 0 ?do\n      base @  base 8 + @  base 16 + @  base 24 + @  base 32 + @\n      base 40 + @  base 48 + @  base 56 + @  base 64 + @  base 72 + @\n      base 80 + @  base 88 + @  base 96 + @  base 104 + @\n      + + + + + + + + + + + + + +\n   loop ;" T$= ;

public

: RUN ( -- )
   T-RESET
   JUDGE-CHECK:JUDGE-ALL
   ARTIFACT-CASES
   REFUSAL-CASES
   TEXT-CASES
   T-REPORT ;

;package

JUDGE-TEST:RUN
