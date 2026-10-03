\ A live build action must refuse nested public entries before setup changes
\ the outer build's output, source callbacks, or unit selection.
require lib/test.f
require tools/native-unit-build-core.f

package NATIVE-BUILD
private

CAST: XT>N ( [ -- ] -- n )
: HIT ( -- ) ;
: QUERY ( n n -- n ) drop ;

: SENTINELS ( -- )
   s" /tmp/habu-action-outer" OUTPUT!
   ['] HIT ['] HIT ['] HIT SOURCE-POLICY!
   ['] HIT SOURCE-TAIL!
   ['] HIT UNIT-SOURCE !
   UNIT-EXPORT UNIT-MODE !
   31 UNIT-SEEN !
   s" /tmp/habu-action-unit" UNIT-ARTIFACT!
   CLASS-WHITEBOX CLASS-WANTED ! ;

: INTACT ( -- )
   OUTPUT$ s" /tmp/habu-action-outer" T$=
   TEMP$ s" /tmp/habu-action-outer.native-build.tmp" T$=
   SOURCE-BIND @ XT>N ['] HIT XT>N T=
   SOURCE-CHECK @ XT>N ['] HIT XT>N T=
   SOURCE-CLOSE @ XT>N ['] HIT XT>N T=
   SOURCE-TAIL @ XT>N ['] HIT XT>N T=
   UNIT-SOURCE @ XT>N ['] HIT XT>N T=
   UNIT-MODE @ UNIT-EXPORT T=
   UNIT-SEEN @ 31 T=
   UNIT-ARTIFACT$ s" /tmp/habu-action-unit" T$=
   CLASS-WANTED @ CLASS-WHITEBOX T= ;

: NESTED-PATH ( -- )
   s" /tmp/habu-action-nested" ['] QUERY false RUN-PATH-RC drop ;
: NESTED-UNIT ( -- ) ['] QUERY false RUN-UNIT ;
: NESTED-DIRECT ( -- ) ['] QUERY false ['] SOURCE-WRITER-DISPATCH RUN-READY-RC drop ;

: ACTIVE-CASE ( RTARGET:build-action -- )
   drop
   [: NESTED-PATH ;] RTARGET:E-ACTIVE TTHROWSQ
   INTACT
   [: NESTED-UNIT ;] RTARGET:E-ACTIVE TTHROWSQ
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
