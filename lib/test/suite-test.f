\ suite-test.f - focused tests for lib/test.f TEST:* suite framework.
\ Run: bin/hb --load lib/test/suite-test.f

require lib/test.f

package TEST-FRAMEWORK-TEST

variable SETUP-N
variable TEARDOWN-N
variable TEARDOWN-RC
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

: RESET-COUNTS ( -- )
   0 SETUP-N !
   0 TEARDOWN-N !
   0 TEARDOWN-RC !
   0 DRAIN-N !
   0 RUN-N !
   0 STDIN-N !
   0 ARG-N !
   0 ARGS-BEGIN-N !
   0 ALPHA-N !
   0 BETA-N !
   0 THROW-N !
   0 THROW-LATE-N !
   0 STDIN-LABEL-N ! ;

: SETUP ( -- )
   1 SETUP-N +! ;

: TEARDOWN ( n -- )
   TEARDOWN-RC !
   1 TEARDOWN-N +! ;

: DRAIN ( -- )
   1 DRAIN-N +! ;

: ARGS-BEGIN ( -- )
   1 ARGS-BEGIN-N +! ;

: ARG+ ( ptr u8 n -- )
   2drop
   1 ARG-N +! ;

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

: FAIL-RUNNER ( ptr u8 n -- )
   2drop
   E-STR-BOUNDS throw ;

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
   [: RUNNER ;] TEST:RUNNER!
   [: STDIN-RUNNER ;] TEST:STDIN-RUNNER! ;

: INSTALL-THROW ( -- )
   [: THROW-RUNNER ;] TEST:RUNNER! ;

: INSTALL-FAIL ( -- )
   [: FAIL-RUNNER ;] TEST:RUNNER! ;

: EXPECT-RUN-FAIL ( -- )
   [: TEST:RUN ;] E-STR-BOUNDS TTHROWSQ ;

T-RESET
RESET-COUNTS
INSTALL
using TEST

RESET

GROUP SEQ seq-grp
SUITE alpha
   a.f -- one two
;SUITE
;GROUP

GROUP PARA par
SUITE beta
   b.f
;SUITE
SUITE-STDIN stdin-case DATA
   c.f -- arg
;SUITE
;GROUP

RUN

;using

SETUP-N @ 1 T=
TEARDOWN-N @ 1 T=
TEARDOWN-RC @ 0 T=
RUN-N @ 2 T=
STDIN-N @ 1 T=
ALPHA-N @ 1 T=
BETA-N @ 1 T=
STDIN-LABEL-N @ 1 T=
ARG-N @ 8 T=
ARGS-BEGIN-N @ 3 T=
DRAIN-N @ 6 T=
TEST:ITEMS-REGISTERED 3 T=
TEST:ITEMS-RUN 3 T=

INSTALL-THROW

using TEST

RESET
GROUP SEQ throw-loop
SUITE throw-caught
   throw-caught.f
;SUITE
SUITE throw-late
   throw-late.f
;SUITE
;GROUP
RUN

;using

THROW-N @ 1 T=
THROW-LATE-N @ 1 T=

INSTALL-FAIL

using TEST

RESET
SUITE uncaught-runner
   fail.f
;SUITE
EXPECT-RUN-FAIL

;using

TEARDOWN-N @ 3 T=
TEARDOWN-RC @ E-STR-BOUNDS T=
T-REPORT

;package

\ Grammar regression: block terminators are FOO … ;FOO and GROUP takes a
\ positional SEQ|PARA mode before the name. Reopen package TEST to reach the
\ private ;SUITE? recognizer and the mode/name validators.
package TEST

: T-MODE-BAD ( -- )   s" NOPE"   MODE-OF drop ;
: T-MODE-EMPTY ( -- ) s" "       MODE-OF drop ;
: T-NAME-KW ( -- )    s" ;GROUP" CHECK-NAME 2drop ;
: T-NAME-EMPTY ( -- ) s" "       CHECK-NAME 2drop ;

\ Capacity probe: ITEM-ALLOC is the shared suite-table counter. Fill it to a
\ target so the next registration exercises the raised ITEM-MAX wall.
: T-FILL-ITEMS ( n -- ) {: target:n :}
   target 0 ?do ITEM-ALLOC drop loop ;

: T-ITEM-OVERFLOW ( -- )
   ITEM-ALLOC drop ;

T-RESET

\ terminator recognizer: qualified TEST:;SUITE and bare ;SUITE (under `using TEST`)
\ both match; END-SUITE/END-GROUP were never the spelling.
s" TEST:;SUITE"    ;SUITE? TTRUE
s" ;SUITE"         ;SUITE? TTRUE
s" TEST:END-SUITE" ;SUITE? TFALSE
s" TEST:END-GROUP" ;SUITE? TFALSE

\ GROUP mode token maps SEQ/PARA; missing or unknown mode throws E-SUITE-MODE
s" SEQ"  MODE-OF GROUP-SEQUENTIAL T=
s" PARA" MODE-OF GROUP-PARALLEL   T=
' T-MODE-BAD   E-SUITE-MODE TTHROWS
' T-MODE-EMPTY E-SUITE-MODE TTHROWS

\ GROUP name rejects reserved DSL keywords and empty names (E-SUITE-NAME)
s" GROUP"       RESERVED-NAME? TTRUE
s" SUITE-STDIN" RESERVED-NAME? TTRUE
s" ;SUITE"      RESERVED-NAME? TTRUE
s" PARA"        RESERVED-NAME? TTRUE
s" alpha"       RESERVED-NAME? TFALSE
' T-NAME-KW    E-SUITE-NAME TTHROWS
' T-NAME-EMPTY E-SUITE-NAME TTHROWS

\ positive end-to-end: SEQ and PARA groups set the mode (read via GROUP-MODE@)
RESET
GROUP SEQ grp-seq
GROUP-CUR @ GROUP-MODE@ GROUP-SEQUENTIAL T=
;GROUP
GROUP PARA grp-par
GROUP-CUR @ GROUP-MODE@ GROUP-PARALLEL T=
;GROUP

\ The (ITEM-MAX+1)th registration fails closed loudly.
RESET
ITEM-MAX T-FILL-ITEMS
ITEM-N @ ITEM-MAX T=
' T-ITEM-OVERFLOW E-TBL-BOUNDS TTHROWS

T-REPORT

;package
