\ A live build action must refuse nested public entries before setup changes
\ the outer build's output, source callbacks, or unit selection.
require src/lib/test.f
require tools/native-build-core.f

package NATIVE-BUILD
private

CAST: XT>N ( [ -- ] -- n )
: HIT ( -- ) ;
: QUERY ( n n -- n ) drop ;

: SENTINELS ( -- )
   s" /tmp/habu-action-outer" OUTPUT!
   ['] HIT ['] HIT ['] HIT SOURCE-POLICY!
   ['] HIT SOURCE-TAIL!
   CLASS-WHITEBOX CLASS-WANTED ! ;

: INTACT ( -- )
   OUTPUT$ s" /tmp/habu-action-outer" T$=
   TEMP$ s" /tmp/habu-action-outer.native-build.tmp" T$=
   SOURCE-BIND @ XT>N ['] HIT XT>N T=
   SOURCE-CHECK @ XT>N ['] HIT XT>N T=
   SOURCE-CLOSE @ XT>N ['] HIT XT>N T=
   SOURCE-TAIL @ XT>N ['] HIT XT>N T=
   CLASS-WANTED @ CLASS-WHITEBOX T= ;

: NESTED-PATH ( -- )
   s" /tmp/habu-action-nested" ['] QUERY false RUN-PATH-RC drop ;
: NESTED-DIRECT ( -- ) ['] QUERY false ['] SOURCE-WRITER-DISPATCH RUN-READY-RC drop ;

: ACTIVE-CASE ( RTARGET:build-action -- )
   drop
   [: NESTED-PATH ;] RTARGET:E-ACTIVE TTHROWSQ
   INTACT
   [: NESTED-DIRECT ;] RTARGET:E-ACTIVE TTHROWSQ
   INTACT ;

: ACTIVE-THROW ( RTARGET:build-action -- )
   ACTIVE-CASE -3123 throw ;

: THROW-CASE ( -- )
   RTARGET:HOST BUILD-TARGET:CURRENT RTARGET:FOR-OUTPUT
      [: ACTIVE-THROW ;] BUILD-TARGET:WITH ;

public
: ACTION-PROOF ( -- )
   T-RESET
   s" nested entry refusal leaves active action state intact" T-LABEL
   SENTINELS
   RTARGET:HOST BUILD-TARGET:CURRENT RTARGET:FOR-OUTPUT
      [: ACTIVE-CASE ;] BUILD-TARGET:WITH
   INTACT
   [: THROW-CASE ;] -3123 TTHROWSQ
   INTACT
   BUILD-TARGET:IDLE-CK
   T-REPORT
   s" native-build-action: ok" type cr ;
;package

NATIVE-BUILD:ACTION-PROOF
