\ The original require entry still reaches a replaceable source-unit callback.
require src/habu/verify-source.f

TRUSTED: TICK-CHANGE-UNIT ( ptr u8 n ptr u8 n ptr u8 [ -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr q :}
   1 set-tier
   q execute ;

[: TICK-CHANGE-UNIT ;] SOURCE-UNIT:USE
0 set-tier
tick-order@ VERIFY:ENTRY-TICK-ORDER!
VERIFY:REPORT-DEFERRALS
s" require test/tick-provider-dep.f : CVT-A ( -- ) drop ['] patch32 drop ;"
s" test/tick-provider-subject.f" VERIFY:SOURCE-COMPOSE-IN-SCOPE
VERIFY:DEFERRED? 0= IF 70 throw THEN
s" deferred-ok" type cr

0 set-tier
s" require test/tick-provider-dep.f : CVT-A ( -- ) drop ['] patch32 drop ;" evaluate
