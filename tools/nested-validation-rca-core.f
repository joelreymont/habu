\ nested-validation-rca-core.f - reproduce the resident validation process tree.
\
\ The runner preserves the native gate's process boundaries and deadlines:
\ resident fork -> nested pool fork -> candidate validation capture ->
\ candidate-validation.f -> protection-span.f SUBJECT forks.  It calls the
\ existing validation slice, so the candidate source order and per-capture
\ deadlines have one owner and cannot drift in this diagnostic.

require test/run-lib.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f
require test/gate-common-lib.f
require test/gate-engine-lib.f

package NESTED-VALIDATION-RCA

160 constant EVENT-CAP
512 constant STAT-CAP
32 constant STAT-SPACE
41 constant STAT-RPAREN
45 constant STAT-MINUS

create EVENT-BUF EVENT-CAP allot
create CANDIDATE-BUF FS-PATH-CAP allot
create STAT-BUF STAT-CAP allot

variable EVENT-U
variable CANDIDATE-U
variable STAT-U
variable STAT-I
variable STAT-LAST
variable STAT-NEG
variable STAT-VALUE

defer VALIDATION ( -- )

: EVENT-CHECK ( n -- ) {: add:n :}
   add 0 < if E-STR-BOUNDS throw then
   EVENT-U @ add + EVENT-CAP > if E-STR-CAPACITY throw then ;

: EVENT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u EVENT-CHECK
   a EVENT-BUF EVENT-U @ + u BYTE-COPY
   EVENT-U @ u + EVENT-U ! ;

: EVENT-C+ ( n -- ) {: c:n :}
   1 EVENT-CHECK
   c EVENT-BUF EVENT-U @ + c!
   EVENT-U @ 1+ EVENT-U ! ;

: EVENT-N+ ( n -- ) {: v:n :}
   v 0 < if E-STR-BOUNDS throw then
   v 10 >= if v 10 / RECURSE then
   v 10 mod STR-ZERO + EVENT-C+ ;

: EVENT-SIGNED-N+ ( n -- ) {: v:n :}
   v 0 < if
      STAT-MINUS EVENT-C+
      v negate EVENT-N+
      exit
   then
   v EVENT-N+ ;

: EVENT$ ( -- ptr u8 n )
   EVENT-BUF EVENT-U @ ;

: STAT-SPACE? ( n -- bool )
   STAT-SPACE = ;

: STAT-DIGIT? ( n -- bool ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and ;

: STAT-FIND-RPAREN ( -- )
   -1 STAT-LAST !
   0 begin dup STAT-U @ < while
      dup STAT-BUF + c@ STAT-RPAREN = if dup 1+ STAT-LAST ! then
      1+
   repeat drop
   STAT-LAST @ 0 < if E-STR-BOUNDS throw then
   STAT-LAST @ STAT-I ! ;

: STAT-SKIP-SPACES ( -- )
   begin
      STAT-I @ STAT-U @ < if
         STAT-BUF STAT-I @ + c@ STAT-SPACE?
      else
         0 0= 0=
      then
   while
      STAT-I @ 1+ STAT-I !
   repeat ;

: STAT-SKIP-TOKEN ( -- )
   STAT-SKIP-SPACES
   begin
      STAT-I @ STAT-U @ < if
         STAT-BUF STAT-I @ + c@ STAT-SPACE? 0=
      else
         0 0= 0=
      then
   while
      STAT-I @ 1+ STAT-I !
   repeat ;

: STAT-NUM ( -- n )
   STAT-SKIP-SPACES
   STAT-I @ STAT-U @ >= if E-STR-BOUNDS throw then
   0 STAT-NEG !
   STAT-BUF STAT-I @ + c@ STAT-MINUS = if
      -1 STAT-NEG !
      STAT-I @ 1+ STAT-I !
   then
   STAT-I @ STAT-U @ >= if E-STR-BOUNDS throw then
   STAT-BUF STAT-I @ + c@ STAT-DIGIT? 0= if E-STR-BOUNDS throw then
   0 STAT-VALUE !
   begin
      STAT-I @ STAT-U @ < if
         STAT-BUF STAT-I @ + c@ STAT-DIGIT?
      else
         0 0= 0=
      then
   while
      STAT-VALUE @ 10 * STAT-BUF STAT-I @ + c@ STR-ZERO - + STAT-VALUE !
      STAT-I @ 1+ STAT-I !
   repeat
   STAT-NEG @ if STAT-VALUE @ negate else STAT-VALUE @ then ;

\ Linux /proc/PID/stat fields 5 and 8 are respectively the process group and
\ the foreground process group of the controlling terminal.  A tpgid of -1
\ means there is no controlling terminal.  The command name is parenthesized
\ and may contain spaces, so parsing starts after its final right parenthesis.
: PROCESS-GROUPS ( -- n n )
   HB-TARGET-LINUX? 0= if
      s" nested-validation-rca: /proc process state requires Linux" 64 die
   then
   s" /proc/self/stat" STAT-BUF STAT-CAP READ-ALL STAT-U !
   STAT-FIND-RPAREN
   STAT-SKIP-TOKEN
   STAT-NUM drop
   STAT-NUM {: pgrp:n :}
   STAT-NUM drop
   STAT-NUM drop
   STAT-NUM {: tpgid:n :}
   pgrp tpgid ;

: STEP ( ptr u8 n -- ) {: stage:ptr stageu:n :}
   PROCESS-GROUPS {: pgrp:n tpgid:n :}
   0 EVENT-U !
   s" gen=" EVENT+
   GS-GEN$ EVENT+
   s" 	pid=" EVENT+
   getpid EVENT-N+
   s" 	pgrp=" EVENT+
   pgrp EVENT-SIGNED-N+
   s" 	tpgid=" EVENT+
   tpgid EVENT-SIGNED-N+
   s" 	stage=" EVENT+
   stage stageu EVENT+
   s" nested-validation-step" EVENT$ GS-EVENT-FIELD ;

: CANDIDATE! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a u EXECUTABLE? 0= if E-FS-OPEN throw then
   a CANDIDATE-BUF u BYTE-COPY
   u CANDIDATE-U ! ;

: CANDIDATE$ ( -- ptr u8 n )
   CANDIDATE-BUF CANDIDATE-U @ ;

: RUN-VALIDATION ( -- )
   CANDIDATE$ GE-HB!
   CANDIDATE$ GE-CANDIDATE-PATH!
   GENG-VALIDATE-SLICE ;

: RUN-PROBE ( -- ) ;

: VALIDATION-WORKER ( -- )
   s" validation-enter" STEP
   VALIDATION
   s" validation-leave" STEP ;

using TEST

: RESIDENT-WORKER ( -- )
   s" resident-enter" STEP
   DEFAULT-NESTED-POOL-SLOTS GT-POOL-SLOTS!
   GT-POOL-RESET
   s" native engine candidate validation slice" TIMEOUT-MS
      [: VALIDATION-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   s" resident-leave" STEP ;

: RUN-TOPOLOGY ( -- )
   GS-ON? 0= if
      s" nested-validation-rca: HABU_GATE_STATS is required" 64 die
   then
   s" nested-validation-rca" GT-START
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" root-enter" STEP
   s" native engine post-candidate group" TIMEOUT-MS
      [: RESIDENT-WORKER ;] GT-POOL-START-FORK
   GT-POOL-DRAIN
   s" root-leave" STEP
   GT-CLEANUP ;

;using

public

: RUN ( ptr u8 n -- )
   CANDIDATE!
   [: RUN-VALIDATION ;] is VALIDATION
   RUN-TOPOLOGY ;

: PROBE ( -- )
   [: RUN-PROBE ;] is VALIDATION
   RUN-TOPOLOGY ;

;package
