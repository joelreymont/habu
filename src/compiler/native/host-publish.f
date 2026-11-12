\ host-publish.f - take owned implementation facts before the code window and
\ expose them only after native publication succeeds.

require src/compiler/native/host.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
require src/compiler/binding.f

package NHOST
private

0 CWORK-VARIABLE TAKEN-BASE n
0 CWORK-VARIABLE TAKEN-EDGES n
0 CWORK-VARIABLE TAKEN-TYPES n
0 CWORK-VARIABLE TAKEN-FUNS n
0 CWORK-VARIABLE TAKEN-AT n

: FUN-END ( n -- n ) {: ordinal:n :}
   ordinal 1+ NEMIT:FUNCTIONS < if
      ordinal 1+ NEMIT:FUNCTION-OFFSET@ exit
   then
   NEMIT:SIZE ;

\ Internal function targets are resolved against the pending emission's own
\ ordered entry offsets. External targets keep the identity filed at emission.
: INTERNAL-ID ( n n -- n ) {: at:n target:n :}
   target at < target at NEMIT:SIZE + >= or if 0 exit then
   target at - {: off:n :}
   NEMIT:FUNCTIONS 0 ?do
      i NEMIT:FUNCTION-OFFSET@ off = if
         TAKEN-BASE @ i + 1+ unloop exit
      then
   loop
   E-STATE throw ;

: EDGE-TARGET ( n n -- n ) {: at:n site:n :}
   at site NEMIT:CALL-TARGET@ INTERNAL-ID {: internal:n :}
   internal 0<> if internal exit then
   site NEMIT:CALL-IMPL@ ;

: TAKE-EDGES ( n n n -- ) {: at:n ordinal:n id:n :}
   ordinal NEMIT:FUNCTION-OFFSET@ {: start:n :}
   ordinal FUN-END {: end:n :}
   NEMIT:CALL-SITES 0 ?do
      i NEMIT:CALL-SITE@ {: site:n :}
      site start >= site end < and if
         id at i EDGE-TARGET site start -
         i NEMIT:CALL-KIND@ i NEMIT:CALL-LOC@ EDGE+
      then
   loop ;

\ A function row's facts: its arity, then the definition's host reason and
\ location. A contract (HOST-ROOT-CONTRACT, the exact constructors a construction
\ call matches) is a separate fact, a negative type count for a row whose
\ declaration holds a pointer, a quotation or a variable. An unknown arity or a
\ body whose facts were abandoned forces UNKNOWN.
: FACTS ( n -- n n n n )
   {: ordinal:n :}
   SOURCE-REASON@ {: reason:n loc:n :}
   ordinal SOURCE-ARITY@ {: din:n dout:n :}
   din 0 < dout 0 < or if din dout UNKNOWN 0 exit then
   din dout reason loc ;

: TAKE-ONE ( n n -- ) {: at:n ordinal:n :}
   ordinal NEMIT:FUNCTION-OFFSET@ {: off:n :}
   ordinal FUN-END off - {: len:n :}
   at off + len NABI:BINDING CBIND:TARGET@ ordinal FACTS
      PREPARE {: id:n :}
   ordinal id SOURCE-CONTRACT-COPY
   id TAKEN-BASE @ ordinal + 1+ <> if E-STATE throw then
   at ordinal id TAKE-EDGES ;

: TAKE-BODY ( -- )
   TAKEN-AT @ {: at:n :}
   TAKEN-FUNS @ 0 ?do at i TAKE-ONE loop ;

public

: ABANDON-TAKEN ( -- )
   TAKEN-FUNS @ 0= if exit then
   TAKEN-BASE @ IMPL-N !
   TAKEN-EDGES @ EDGE-N !
   TAKEN-TYPES @ IMPL-TYPE-N !
   0 TAKEN-FUNS ! ;

\ Take the emission NEMIT holds sealed, placed at `at`, before any code window
\ opens. Every reserve and row copy that may throw runs before the observer; a
\ refused take is abandoned.
: TAKE ( n -- ) {: at:n :}
   TAKEN-FUNS @ 0<> if E-STATE throw then
   IMPL-N @ TAKEN-BASE !
   EDGE-N @ TAKEN-EDGES !
   IMPL-TYPE-N @ TAKEN-TYPES !
   NEMIT:FUNCTIONS dup 0 <= if E-STATE throw then TAKEN-FUNS !
   at TAKEN-AT !
   [: TAKE-BODY ;] catch {: rc:n :}
   0 TAKEN-AT !
   rc 0<> if ABANDON-TAKEN rc throw then ;

\ PREPARE wrote every cell; this only flips liveness.
: PUBLISH-TAKEN ( -- )
   TAKEN-FUNS @ 0 ?do
      1 TAKEN-BASE @ i + IMPL-LIVE !
   loop
   0 TAKEN-FUNS ! ;

;package
