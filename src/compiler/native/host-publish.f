\ host-publish.f - take owned implementation facts before the code window and
\ expose them only after native publication succeeds.

require src/compiler/native/host.f
require src/compiler/session/emission.f
require src/compiler/binding.f

package NHOST
private

variable TAKEN-BASE
variable TAKEN-EDGES
variable TAKEN-FUNS
TYPED-VARIABLE TAKEN-EMISSION NART:emission
variable TAKEN-AT

: FUN-END ( NART:emission n -- n )
   {: e:NART:emission ordinal:n :}
   ordinal 1+ e NART:FUNCTIONS < if
      e ordinal 1+ NART:FUNCTION-OFFSET@ exit
   then
   e NART:SIZE ;

\ Internal function targets are resolved against the pending emission's own
\ ordered entry offsets. External targets keep the identity filed at emission.
: INTERNAL-ID ( NART:emission n n -- n )
   {: e:NART:emission at:n target:n :}
   target at < target at e NART:SIZE + >= or if 0 exit then
   target at - {: off:n :}
   e NART:FUNCTIONS 0 ?do
      e i NART:FUNCTION-OFFSET@ off = if
         TAKEN-BASE @ i + 1+ unloop exit
      then
   loop
   E-STATE throw ;

: EDGE-TARGET ( NART:emission n n -- n )
   {: e:NART:emission at:n site:n :}
   e at e site NART:CALL-TARGET@ INTERNAL-ID {: internal:n :}
   internal 0<> if internal exit then
   e site NART:CALL-IMPL@ ;

: TAKE-EDGES ( NART:emission n n n -- )
   {: e:NART:emission at:n ordinal:n id:n :}
   e ordinal NART:FUNCTION-OFFSET@ {: start:n :}
   e ordinal FUN-END {: end:n :}
   e NART:CALL-SITES 0 ?do
      e i NART:CALL-SITE@ {: site:n :}
      site start >= site end < and if
         id e at i EDGE-TARGET site start -
         e i NART:CALL-KIND@ e i NART:CALL-LOC@ EDGE+
      then
   loop ;

: FACTS ( n -- n n n n )
   {: ordinal:n :}
   SOURCE-REASON@ {: reason:n loc:n :}
   ordinal SOURCE-ARITY@ {: din:n dout:n :}
   din 0 < dout 0 < or if -1 -1 UNKNOWN 0 exit then
   din dout reason loc ;

: TAKE-ONE ( NART:emission n n -- )
   {: e:NART:emission at:n ordinal:n :}
   e ordinal NART:FUNCTION-OFFSET@ {: off:n :}
   e ordinal FUN-END off - {: len:n :}
   at off + len e NART:BINDING CBIND:TARGET@ ordinal FACTS
      PREPARE {: id:n :}
   id TAKEN-BASE @ ordinal + 1+ <> if E-STATE throw then
   e at ordinal id TAKE-EDGES ;

: TAKE-BODY ( -- )
   TAKEN-EMISSION @ {: e:NART:emission :}
   TAKEN-AT @ {: at:n :}
   TAKEN-FUNS @ 0 ?do e at i TAKE-ONE loop ;

public

: ABANDON-TAKEN ( -- )
   TAKEN-FUNS @ 0= if exit then
   TAKEN-BASE @ IMPL-N !
   TAKEN-EDGES @ EDGE-N !
   0 TAKEN-FUNS ! ;

\ The observer has run, and no code window has opened. Every reserve and row
\ copy that may throw happens here. An incomplete take is abandoned on refusal.
: TAKE ( NART:emission n -- )
   {: e:NART:emission at:n :}
   TAKEN-FUNS @ 0<> if E-STATE throw then
   IMPL-N @ TAKEN-BASE !
   EDGE-N @ TAKEN-EDGES !
   e NART:FUNCTIONS dup 0 <= if E-STATE throw then TAKEN-FUNS !
   e TAKEN-EMISSION !
   at TAKEN-AT !
   [: TAKE-BODY ;] catch {: rc:n :}
   rc 0<> if ABANDON-TAKEN rc throw then ;

\ PREPARE wrote all cells; the code publisher only flips liveness now.
: PUBLISH-TAKEN ( -- )
   TAKEN-FUNS @ 0 ?do
      TAKEN-BASE @ i + 1+ PUBLISH
   loop
   0 TAKEN-FUNS ! ;

;package
