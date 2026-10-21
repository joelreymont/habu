\ Bind the retained compiler's producer words that object import calls.

require src/lib/errors.f
require src/compiler/native/publish.f
require src/compiler/session/emission.f
require src/compiler/native/string.f
require src/habu/xref.f
require src/habu/aot-arm.f
require src/lib/string.f

package NUNIT-CAPTURE

private

DYNAMIC-BUFFER PRODUCER-NAME ptr u8
DYNAMIC-BUFFER PRODUCER-LEN n
DYNAMIC-BUFFER PRODUCER-XT n
variable PRODUCER-N

CAST: WITH-UNIT-XT ( n -- [ [ n n n n -- ] [ -- ] -- ] )

: BIND-ONE ( ptr u8 n bool -- ) {: a:ptr u:n optional:bool :}
   a u XREF-FIND dup XREF-FOUND? if XREF-START
   else drop optional 0= if E-NUNIT-PROFILE throw then 0 then
      {: xt:n :}
   PRODUCER-N @ {: row:n :}
   row 1+ PRODUCER-NAME-RESERVE
   row 1+ PRODUCER-LEN-RESERVE
   row 1+ PRODUCER-XT-RESERVE
   a row PRODUCER-NAME !
   u row PRODUCER-LEN !
   xt row PRODUCER-XT !
   1 PRODUCER-N +! ;

: BOUND-XT ( ptr u8 n -- n ) {: a:ptr u:n :}
   PRODUCER-N @ 0= if E-NUNIT-PROFILE throw then
   PRODUCER-N @ 0 ?do
      i PRODUCER-NAME @ i PRODUCER-LEN @ a u STR= if
         i PRODUCER-XT @ unloop exit
      then
   loop
   E-NUNIT-PROFILE throw ;

: TARGET-XT ( ptr u8 n -- n )
   BOUND-XT dup 0= if E-NUNIT-PROFILE throw then ;

public

: BIND-PRODUCER ( -- )
   0 PRODUCER-N !
   s" NPUB:WITH-UNIT" false BIND-ONE
   s" NART:CALL-SITES" false BIND-ONE
   s" NART:CALL-SITE@" false BIND-ONE
   s" NART:CALL-KIND@" false BIND-ONE
   s" NART:CALL-TARGET@" false BIND-ONE
   s" NART:ADDR-SITES" false BIND-ONE
   s" NART:ADDR-SITE@" false BIND-ONE
   s" NART:ADDR-SITE-KIND@" false BIND-ONE
   s" NART:ADDR-SITE-ORIGIN@" true BIND-ONE
   s" NART:BYTES" false BIND-ONE
   s" NART:FUNCTION-OFFSET@" false BIND-ONE
   s" NART:CALL-IMPL@" false BIND-ONE
   s" NART:CALL-LOC@" false BIND-ONE
   s" NHOST:TAKEN-FUNCTIONS" true BIND-ONE
   s" NHOST:TAKEN-FACTS" true BIND-ONE
   s" NHOST:TAKEN-TYPE@" true BIND-ONE
   s" NHOST:PREPARE-IMPORT" true BIND-ONE
   s" NHOST:ABANDON-TAKEN" true BIND-ONE
   s" NPUB:IMPORT-PENDING" true BIND-ONE
   s" NCOMP:DEFINITION-CACHEABLE?" true BIND-ONE
   s" NCOMP:DEFINITION-HISTORY" true BIND-ONE
   s" NCOMP:REPLAY-DEFINITION" true BIND-ONE
   s" NSTR:PREPARE-BEGIN" true BIND-ONE
   s" NSTR:PREPARE-INTERN" true BIND-ONE
   s" NSTR:PREPARE-DATA-END" true BIND-ONE
   s" NSTR:PREPARE-CLOSE" true BIND-ONE
   s" NSTR:INTERN" true BIND-ONE
   s" NSTR:SOURCE-BEGIN" true BIND-ONE
   s" NSTR:SOURCE-END" true BIND-ONE
   s" NSTR:SOURCE-COUNT" true BIND-ONE
   s" NSTR:SOURCE@" true BIND-ONE
   s" NSTR:SOURCE-CLOSE" true BIND-ONE ;

: BOUND-PRODUCER-XT ( ptr u8 n -- n ) TARGET-XT ;
: OPTIONAL-PRODUCER-XT ( ptr u8 n -- n ) BOUND-XT ;

: WITH-OBSERVER ( [ n n n n -- ] [ -- ] -- )
   s" NPUB:WITH-UNIT" TARGET-XT WITH-UNIT-XT execute ;

: CLOSE ( -- )
   PRODUCER-NAME-RELEASE PRODUCER-LEN-RELEASE PRODUCER-XT-RELEASE
   0 PRODUCER-N ! ;

;package
