\ suite-test.f - focused tests for lib/test.f TEST:* suite framework.
\ Run: bin/hb --load lib/test/suite-test.f

require lib/test.f
require lib/string.f
require lib/process.f
require lib/test/subject.f

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

\ Row-refusal cases run in disposable SUBJECT forks. A row parser reads the live
\ input stream, so a bad row cannot be staged in this file without swallowing the
\ assertions after it, and the refusal ends the process it runs in.
2048 constant T-SUBJ-CAP
30000 constant T-SUBJ-TIMEOUT-MS
67 constant T-THROW-RC      \ engine uncaught-throw exit; measured: a bare `-4201 throw` in a SUBJECT fork exits 67

create T-SUBJ-OUT T-SUBJ-CAP allot
create T-SUBJ-ERR T-SUBJ-CAP allot
variable T-SUBJ-OUT-U
variable T-SUBJ-ERR-U
variable T-SUBJ-EXITED
variable T-SUBJ-RC

: T-SUBJ-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF T-SUBJ-RC ! 0 0= T-SUBJ-EXITED ! ENDOF
     signaled OF T-SUBJ-RC ! 0 0= 0= T-SUBJ-EXITED ! ENDOF
     timeout OF 0 T-SUBJ-RC ! 0 0= 0= T-SUBJ-EXITED ! ENDOF
   ;MATCH
   LEN>N T-SUBJ-ERR-U !  LEN>N T-SUBJ-OUT-U ! ;

: T-SUBJ-OUT$ ( -- ptr u8 n )
   T-SUBJ-OUT T-SUBJ-OUT-U @ ;

: T-SUBJ-ERR$ ( -- ptr u8 n )
   T-SUBJ-ERR T-SUBJ-ERR-U @ ;

: T-RUN-SUBJECT ( ptr u8 n -- )
   T-SUBJ-OUT T-SUBJ-CAP >LEN T-SUBJ-ERR T-SUBJ-CAP >LEN
   T-SUBJ-TIMEOUT-MS >MS SUBJECT:RUN T-SUBJ-STORE! ;

\ The refusal names the row on stdout (OVERSIZE's manner) and leaves E-SUITE-ROW
\ uncaught, so the child dies before it can register or run anything further.
: T-ROW-REFUSED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   src srcu T-RUN-SUBJECT
   T-SUBJ-EXITED @ TTRUE
   T-SUBJ-RC @ T-THROW-RC T=
   T-SUBJ-OUT$ s" test: row " CONTAINS? TTRUE
   T-SUBJ-OUT$ name nameu CONTAINS? TTRUE
   T-SUBJ-OUT$ s"  has no ;SUITE" CONTAINS? TTRUE
   T-SUBJ-ERR$ s" hb: uncaught throw code -4202" CONTAINS? TTRUE ;   \ -4202 is E-SUITE-ROW

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

\ A row opener is never an argument, bare or TEST:-qualified, in any case. The
\ terminator is tested before this one, and SEQ/PARA open nothing.
s" SUITE"             ROW-KEYWORD? TTRUE
s" TEST:;GROUP"       ROW-KEYWORD? TTRUE
s" test:suite-stdin"  ROW-KEYWORD? TTRUE
s" ;SUITE"            ROW-KEYWORD? TFALSE
s" a.f"               ROW-KEYWORD? TFALSE
s" seq"               ROW-KEYWORD? TFALSE

\ Chain FH: an os-memory row without ;SUITE took `SUITE shadow-lint <paths>` as
\ its own arguments, so the shadow-lint row was never registered and the suite
\ count fell by one with nothing naming the missing terminator. The row now
\ refuses by name and never reaches the count.
s" TEST:RESET TEST:SUITE os-memory a.f TEST:SUITE next b.f TEST:;SUITE TEST:ITEMS-REGISTERED ."
s" os-memory" T-ROW-REFUSED
T-SUBJ-OUT$ s" 1" CONTAINS? TFALSE

\ Same text with the terminator restored: both rows register.
s" TEST:RESET TEST:SUITE os-memory a.f TEST:;SUITE TEST:SUITE next b.f TEST:;SUITE TEST:ITEMS-REGISTERED ."
T-RUN-SUBJECT
T-SUBJ-EXITED @ TTRUE
T-SUBJ-RC @ 0 T=
T-SUBJ-OUT$ s" 2" CONTAINS? TTRUE

\ A row that reaches the end of input is the same refusal.
s" TEST:RESET TEST:SUITE row-eof a.f" s" row-eof" T-ROW-REFUSED

\ One case per remaining row keyword.
s" TEST:RESET TEST:SUITE row-wb a.f TEST:WHITEBOX-SUITE next b.f TEST:;SUITE"
s" row-wb" T-ROW-REFUSED
s" TEST:RESET TEST:SUITE row-stdin a.f TEST:SUITE-STDIN next DATA b.f TEST:;SUITE"
s" row-stdin" T-ROW-REFUSED
s" TEST:RESET TEST:SUITE row-group a.f TEST:GROUP SEQ g TEST:;SUITE"
s" row-group" T-ROW-REFUSED
s" TEST:RESET TEST:SUITE row-endgroup a.f TEST:;GROUP"
s" row-endgroup" T-ROW-REFUSED

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
