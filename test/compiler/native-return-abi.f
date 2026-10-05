\ Native return rows use the same per-thread return stack as ordinary words.
require lib/test.f
require lib/task.f
require lib/adt/result.f

1 set-tier

package NRABI-FIXTURE
public

: TAKE ( | n -- n | ) r> ;
: DIRECT ( -- n ) 41 >r TAKE ;

\ The callee exchanges a data value with the caller's return value.
: REPLACE ( n | n -- n | n ) r> swap >r ;
: REPLACED ( -- n n ) 7 >r 11 REPLACE r> ;

defer MOVE ( n | -- | n )
: PUSH ( n | -- | n ) >r ;
: BIND ( -- ) ['] PUSH is MOVE ;
BIND
: THROUGH-DEFER ( -- n ) 42 MOVE r> ;
: THROUGH-EXEC ( -- n ) 43 ['] PUSH execute r> ;

: REPLACE-THROW ( | n -- | n ) r> 100 + >r 77 throw ;
TRUSTED: CATCH-REPLACE ( -- n n ) 5 >r ['] REPLACE-THROW catch r> ;

: ID-R ( | n -- | n ) exit ;
: EXIT-CALL ( -- n ) 19 >r ID-R r> ;

: CHANGE-DIV ( | n -- | n ) r> 100 + >r 1 0 / drop ;
TRUSTED: DIV-CATCH ( -- n n ) 5 >r ['] CHANGE-DIV catch r> ;
: DIV-OK ( -- n n ) 8 >r 21 3 / r> ;

: CHANGE-MOD ( | n -- | n ) r> 200 + >r 3 0 mod drop ;
TRUSTED: MOD-CATCH ( -- n n ) 5 >r ['] CHANGE-MOD catch r> ;

: FINAL ( n -- n ) 1+ ;
: BRIDGED-TAIL ( n -- n ) >r 21 3 / r> + FINAL ;
: FINAL-R ( n | n -- n | n ) 1+ ;
: BRIDGED-FINAL ( n | n -- n | n ) 21 3 / drop FINAL-R ;
: USE-BRIDGED-FINAL ( -- n n ) 5 >r 42 BRIDGED-FINAL r> ;
: QUOTED ( n -- n ) [: ( n -- n ) >r 21 3 / r> + FINAL ;] execute ;
: MAKER ( -- ) create does> ( -- n ) drop 5 >r 21 3 / r> + ;
MAKER MADE

TASK:MIN-STACK TASK:TASK WORKER
: WORK ( -- ) 57 >r TAKE TASK:RETURN ;
: TASK-RESULT ( -- n n )
   ['] WORK WORKER TASK:ACTIVATE
   WORKER TASK:JOIN MATCH result ok OF 0 ENDOF err OF 1 ENDOF ;MATCH ;

;package

T-RESET
s" direct native call consumes the caller's return value" T-LABEL
NRABI-FIXTURE:DIRECT 41 T=
s" a native call publishes its new return value" T-LABEL
NRABI-FIXTURE:REPLACED 11 T= 7 T=
s" a defer moves a return cell through its target" T-LABEL
NRABI-FIXTURE:THROUGH-DEFER 42 T=
s" execute preserves the quotation's return effect" T-LABEL
NRABI-FIXTURE:THROUGH-EXEC 43 T=
s" catch reloads the cell a throwing body replaced" T-LABEL
NRABI-FIXTURE:CATCH-REPLACE 105 T= 77 T=
s" an explicit exit publishes its declared return row" T-LABEL
NRABI-FIXTURE:EXIT-CALL 19 T=
s" a dividing throw publishes a replaced return cell" T-LABEL
NRABI-FIXTURE:DIV-CATCH 105 T= -6400 T=
s" a returning divide restores the native return row" T-LABEL
NRABI-FIXTURE:DIV-OK 8 T= 7 T=
s" a modulo throw publishes a replaced return cell" T-LABEL
NRABI-FIXTURE:MOD-CATCH 205 T= -6400 T=
s" a generated bridge call precedes a final ordinary call" T-LABEL
8 NRABI-FIXTURE:BRIDGED-TAIL 16 T=
s" return publication after a final call prevents tail lowering" T-LABEL
NRABI-FIXTURE:USE-BRIDGED-FINAL 5 T= 43 T=
s" a quotation's generated call shares the module call accounting" T-LABEL
8 NRABI-FIXTURE:QUOTED 16 T=
s" a does clause's generated call shares the module call accounting" T-LABEL
NRABI-FIXTURE:MADE 12 T=
s" a task calls through its own physical return stack" T-LABEL
NRABI-FIXTURE:TASK-RESULT 0 T= 57 T=
T-REPORT
