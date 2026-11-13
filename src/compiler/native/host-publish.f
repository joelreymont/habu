\ host-publish.f - take owned implementation facts before the code window and
\ expose them only after native publication succeeds.

require src/compiler/native/host.f
require src/compiler/native/emission.f

package NHOST
private

0 CWORK-VARIABLE TAKEN-BASE n
0 CWORK-VARIABLE TAKEN-FUNS n
0 CWORK-VARIABLE TAKEN-AT n

: FUN-END ( n -- n ) {: ordinal:n :}
   ordinal 1+ NEMIT:FUNCTIONS < if
      ordinal 1+ NEMIT:FUNCTION-OFFSET@ exit
   then
   NEMIT:SIZE ;

\ A function row's facts: the definition's host reason and location. An unknown
\ arity or a body whose facts were abandoned forces UNKNOWN.
: FACTS ( n -- n n )
   {: ordinal:n :}
   SOURCE-REASON@ {: reason:n loc:n :}
   ordinal SOURCE-ARITY@ {: din:n dout:n :}
   din 0 < dout 0 < or if UNKNOWN 0 exit then
   reason loc ;

: TAKE-ONE ( n n -- ) {: at:n ordinal:n :}
   ordinal NEMIT:FUNCTION-OFFSET@ {: off:n :}
   ordinal FUN-END off - {: len:n :}
   at off + len ordinal FACTS
      PREPARE {: id:n :}
   id TAKEN-BASE @ ordinal + 1+ <> if E-STATE throw then ;

: TAKE-BODY ( -- )
   TAKEN-AT @ {: at:n :}
   TAKEN-FUNS @ 0 ?do at i TAKE-ONE loop ;

public

: ABANDON-TAKEN ( -- )
   TAKEN-FUNS @ 0= if exit then
   TAKEN-BASE @ IMPL-N !
   0 TAKEN-FUNS ! ;

\ Take the emission NEMIT holds sealed, placed at `at`, before any code window
\ opens; a refused take is abandoned.
: TAKE ( n -- ) {: at:n :}
   TAKEN-FUNS @ 0<> if E-STATE throw then
   IMPL-N @ TAKEN-BASE !
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
