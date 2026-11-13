\ sparql.f - one synchronous query over caller-owned data.
\ Request JSON is defined by the native header. READ's returned span is
\ copied immediately; WRITE receives status 0 result chunks, or one
\ positive-status diagnostic. Discard partial output when QUERY throws.
\ Callbacks run on the caller's thread. Keep their data in CONTEXT; callback
\ quotations may not capture locals. No callback span may outlive its call.
\ STORAGE CLASS. CALL-OWNED: the private frame is released by QUERY.

require src/lib/error.f
require src/lib/prelude.f
require src/lib/ffi-abi.f
require src/lib/ffi-callback.f
require src/lib/task.f

package SPARQL
public

-10000 constant E-FIRST
-10099 constant E-LAST
-10000 s" SPARQL query failed" ERROR:CODE E-QUERY

private

STRUCTURE call DERIVE addr
   FIELD context ptr u8
   FIELD read [ ptr u8 ptr u8 n -- ptr u8 n ]
   FIELD write [ ptr u8 n ptr u8 n -- ]
   FIELD fault n
;STRUCTURE
HEAP CALLS call
CAST: >CALL ( ptr u8 -- ptr call )

HB-TARGET-MACOS? [if]
LIBRARY libhabu_sparql.dylib
[then]
HB-TARGET-MACOS? 0= [if]
LIBRARY libhabu_sparql.so
[then]

FUNCTION: NATIVE-RUN hb_sparql_run
   ( ptr u8 n ptr u8 n n -- i32 ) ;FUNCTION
FUNCTION: NATIVE-REPLY hb_sparql_reply_set
   ( ptr u8 ptr u8 n -- ) ;FUNCTION

: SPAN-CHECK ( ptr u8 n -- )
   {: bytes length:n :}
   length 0< if E-QUERY throw then
   length 0> bytes NULL-PTR = and if E-QUERY throw then ;

: READ-REPLY ( ptr call ptr u8 n ptr u8 -- )
   {: call request length:n reply :}
   call CALL-CONTEXT @ request length
      call CALL-READ @ execute
   {: bytes size:n :}
   bytes size SPAN-CHECK
   reply bytes size NATIVE-REPLY ;

: WRITE-CHUNK ( ptr call n ptr u8 n -- )
   {: call status:n bytes length:n :}
   call CALL-CONTEXT @ status bytes length
      call CALL-WRITE @ execute ;

: REMEMBER ( ptr call n -- n )
   {: call fault:n :}
   fault call CALL-FAULT !
   fault 0<> if -1 else 0 then ;

: PROVIDE ( ptr u8 ptr u8 n ptr u8 -- n )
   {: context request length:n reply :}
   context >CALL {: call :}
   call CALL-FAULT @ 0<> if -1 exit then
   call [: call request length reply READ-REPLY ;]
      catch REMEMBER ;

: EMIT ( ptr u8 n ptr u8 n -- n )
   {: context status:n bytes length:n :}
   context >CALL {: call :}
   call CALL-FAULT @ 0<> if -1 exit then
   call [: call status bytes length WRITE-CHUNK ;]
      catch REMEMBER ;

CALLBACK: READ-SOURCE ( ptr u8 ptr u8 n ptr u8 -- i32 )
   -1 FALLBACK
;CALLBACK
' PROVIDE READ-SOURCE-BODY !

CALLBACK: WRITE-RESULT ( ptr u8 i32 ptr u8 n -- i32 )
   -1 FALLBACK
;CALLBACK
' EMIT WRITE-RESULT-BODY !

: SOURCE-ENTRY ( -- n )
   READ-SOURCE TASK:CALLER-CONTEXT FFI-CB:ENTRY ;

: RESULT-ENTRY ( -- n )
   WRITE-RESULT TASK:CALLER-CONTEXT FFI-CB:ENTRY ;

: FINISH ( n ptr call -- )
   {: status:n call :}
   call CALL-FAULT @ dup 0<> if throw then drop
   status 0<> if E-QUERY throw then ;

: RUN ( ptr call ptr u8 n -- )
   {: call request length:n :}
   request length call BYTE-VIEW SOURCE-ENTRY RESULT-ENTRY
      NATIVE-RUN call FINISH ;

: START ( ptr u8 [ ptr u8 ptr u8 n -- ptr u8 n ]
      [ ptr u8 n ptr u8 n -- ] -- ptr call )
   {: context read write :}
   CALLS-NEW {: call :}
   context call CALL-CONTEXT !
   read call CALL-READ ! write call CALL-WRITE !
   call ;

public

: QUERY ( ptr u8 n ptr u8
      [ ptr u8 ptr u8 n -- ptr u8 n ]
      [ ptr u8 n ptr u8 n -- ] -- )
   {: request length:n context read write :}
   request length SPAN-CHECK
   context read write START {: call :}
   [: call request length RUN ;]
   [: call CALLS-RELEASE ;] finally ;

seal-package
;package
