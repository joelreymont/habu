\ A resident replacement loader changes the tier before consuming its path.
require src/habu/verify-source.f

undefine require
: require ( -- ) 1 set-tier parse-name required ;

: TICK-PROVIDER-REPLACE-TEST ( -- )
   0 set-tier
   tick-order@ VERIFY:ENTRY-TICK-ORDER!
   VERIFY:REPORT-DEFERRALS
   s" require test/tick-provider-dep.f : CVT-A ( -- ) drop ['] patch32 drop ;"
   s" test/tick-provider-subject.f" VERIFY:SOURCE-COMPOSE-IN-SCOPE
   VERIFY:DEFERRED? 0= IF 70 throw THEN
   s" deferred-ok" type cr
   0 set-tier
   s" require test/tick-provider-dep.f : CVT-A ( -- ) drop ['] patch32 drop ;" evaluate-closed ;

TICK-PROVIDER-REPLACE-TEST
