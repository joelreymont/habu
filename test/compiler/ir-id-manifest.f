\ Compiler identity boundary vectors and require-replay behavior.

require lib/test.f
require lib/test/outcome.f
require lib/process-command.f
require test/compiler/ir-id-schema.f

package IR-ID-MANIFEST-TEST
using COMPILER-ID-PROOF
private

\ ---- vector rows through the production surface ------------------------------

: PACK-ROW-RUN ( n -- ) {: idx:n :}
   idx PACK-LOCAL {: local:n :}
   idx PACK-CLASS {: class:n :}
   s" pack vector reaches its recorded outcome" T-LABEL
   local PACK-OUTCOME class T=
   class 0 <> if exit then
   s" accepted local index round-trips out of the packed identity" T-LABEL
   local PACK-LOCAL-BACK local T=
   s" accepted packed identity keeps its minting module" T-LABEL
   local PACK-OWNER-KEPT? TTRUE ;

: PACK-VECTOR-RUN ( -- )
   PACK-ROWS 0 ?do i PACK-ROW-RUN loop ;

: CHECK-ROW-RUN ( n -- ) {: idx:n :}
   s" check vector reaches its recorded outcome" T-LABEL
   idx CHECK-LOCAL idx CHECK-BOUND idx CHECK-OWNER CHECK-OUTCOME
      idx CHECK-CLASS T= ;

: CHECK-VECTOR-RUN ( -- )
   CHECK-ROWS 0 ?do i CHECK-ROW-RUN loop ;

: SCALAR-IDENTITY ( n -- ) {: value:n :}
   s" count keeps the whole value" T-LABEL
   value COUNT-BACK value T=
   s" pool offset keeps the whole value" T-LABEL
   value POOL-BACK value T= ;

: SCALAR-ROW-RUN ( n -- ) {: idx:n :}
   idx SCALAR-VALUE {: value:n :}
   idx SCALAR-CLASS {: class:n :}
   s" count vector reaches its recorded outcome" T-LABEL
   value COUNT-OUTCOME class T=
   s" pool-offset vector reaches its recorded outcome" T-LABEL
   value POOL-OUTCOME class T=
   class 0 <> if exit then
   value SCALAR-IDENTITY ;

: SCALAR-VECTOR-RUN ( -- )
   SCALAR-ROWS 0 ?do i SCALAR-ROW-RUN loop ;

: REPLAY-OUTCOME ( bool -- outcome ) {: forced:bool :}
   PROC-CMD:RESET
   forced if
      s" HABU_IR_ID_REPLAY_FORCE" >LEN s" 1" >LEN PROC-CMD:ENV+
   then
   s" --load" >LEN PROC-CMD:ARG+
   s" test/compiler/ir-id-replay.f" >LEN PROC-CMD:ARG+
   s" bin/hb" >LEN 60000 >MS PROC-CMD:RUN-OUTCOME ;

: REPLAY-COVERAGE ( -- )
   s" the module allocator survives a require replay in a child load" T-LABEL
   false REPLAY-OUTCOME 0 T-OUTCOME-EXITED=
   s" a forced replay of the identity source is refused by the seal" T-LABEL
   true REPLAY-OUTCOME ENGINE-ERROR:SEAL-PACKAGE T-OUTCOME-EXITED= ;

public

: RUN ( -- )
   T-RESET
   BUILD
   PACK-VECTOR-RUN
   CHECK-VECTOR-RUN
   SCALAR-VECTOR-RUN
   REPLAY-COVERAGE
   T-REPORT ;

;using
;package

IR-ID-MANIFEST-TEST:RUN
