\ suite-test.f - focused tests for lib/test.f TEST:* suite framework.
\ Run: bin/hb --load lib/test/suite-test.f

require lib/test.f

package TEST-FRAMEWORK-TEST

variable SETUP-N
variable TEARDOWN-N
variable DRAIN-N
variable RUN-N
variable STDIN-N
variable ARG-N
variable ARGS-BEGIN-N
variable ALPHA-N
variable BETA-N
variable THROW-N
variable THROW-LATE-N
variable STDIN-LABEL-N
variable SELECT-SKIP-N

: RESET-COUNTS ( -- )
   0 SETUP-N !
   0 TEARDOWN-N !
   0 DRAIN-N !
   0 RUN-N !
   0 STDIN-N !
   0 ARG-N !
   0 ARGS-BEGIN-N !
   0 ALPHA-N !
   0 BETA-N !
   0 THROW-N !
   0 THROW-LATE-N !
   0 STDIN-LABEL-N !
   0 SELECT-SKIP-N ! ;

: SETUP ( -- )
   1 SETUP-N +! ;

: TEARDOWN ( -- )
   1 TEARDOWN-N +! ;

: DRAIN ( -- )
   1 DRAIN-N +! ;

: ARGS-BEGIN ( -- )
   1 ARGS-BEGIN-N +! ;

: ARG+ ( ptr u8 n -- )
   2drop
   1 ARG-N +! ;

: SELECT? ( -- bool )
   s" skip-me" TEST:LABEL= if
      1 SELECT-SKIP-N +!
      0 0= 0=
      exit
   then
   0 0= ;

: RUNNER ( ptr u8 n -- ) {: label:ptr labelu:n :}
   1 RUN-N +!
   label labelu s" alpha" STR= if 1 ALPHA-N +! exit then
   label labelu s" beta" STR= if 1 BETA-N +! exit then ;

: THROWER ( -- )
   E-STR-BOUNDS throw ;

: THROW-CAUGHT ( -- )
   [: THROWER ;] E-STR-BOUNDS TTHROWSQ ;

: THROW-RUNNER ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu s" throw-caught" STR= if
      THROW-CAUGHT
      1 THROW-N +!
      exit
   then
   label labelu s" throw-late" STR= if
      1 THROW-LATE-N +!
      exit
   then ;

: STDIN-RUNNER ( ptr u8 n ptr u8 n -- )
   {: in:ptr inu:n label:ptr labelu:n :}
   1 STDIN-N +!
   label labelu s" stdin-case" STR= if 1 STDIN-LABEL-N +! then ;

: INSTALL ( -- )
   [: SETUP ;] TEST:SETUP!
   [: TEARDOWN ;] TEST:TEARDOWN!
   [: DRAIN ;] TEST:DRAIN!
   [: ARGS-BEGIN ;] TEST:ARGS-BEGIN!
   [: ARG+ ;] TEST:ARG+!
   [: SELECT? ;] TEST:SELECT?!
   [: RUNNER ;] TEST:RUNNER!
   [: STDIN-RUNNER ;] TEST:STDIN-RUNNER! ;

: INSTALL-THROW ( -- )
   [: THROW-RUNNER ;] TEST:RUNNER! ;

T-RESET
RESET-COUNTS
INSTALL
TEST:RESET

TEST:GROUP-SEQUENTIAL seq
TEST:SUITE alpha
   a.f -- one two
TEST:END-SUITE
TEST:END-GROUP

TEST:GROUP-PARALLEL par
TEST:SUITE beta
   b.f
TEST:END-SUITE
TEST:SUITE-STDIN stdin-case DATA
   c.f -- arg
TEST:END-SUITE
TEST:SUITE skip-me
   skip.f
TEST:END-SUITE
TEST:END-GROUP

TEST:RUN

SETUP-N @ 1 T=
TEARDOWN-N @ 1 T=
RUN-N @ 2 T=
STDIN-N @ 1 T=
ALPHA-N @ 1 T=
BETA-N @ 1 T=
STDIN-LABEL-N @ 1 T=
SELECT-SKIP-N @ 1 T=
ARG-N @ 8 T=
ARGS-BEGIN-N @ 3 T=
DRAIN-N @ 6 T=

INSTALL-THROW
TEST:RESET
TEST:GROUP-SEQUENTIAL throw-loop
TEST:SUITE throw-caught
   throw-caught.f
TEST:END-SUITE
TEST:SUITE throw-late
   throw-late.f
TEST:END-SUITE
TEST:END-GROUP
TEST:RUN

THROW-N @ 1 T=
THROW-LATE-N @ 1 T=
T-REPORT

end-package
