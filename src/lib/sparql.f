\ sparql.f - standard SPARQL Query over a caller's RDF dataset.
\ The native bridge owns parsing and evaluation; a source supplies authorized
\ quads and graph names from one stable snapshot. It opens no store or network.
\ PARSE copies the query and base IRI. BIND copies a variable name and a
\ SPARQL Results JSON term. RESULT$ is Results JSON for SELECT/ASK and
\ N-Triples for CONSTRUCT/DESCRIBE; DATASET$ describes FROM/FROM NAMED.
\ Returned spans last until the next operation on that query or CLOSE.
\ A provider receives its context, a JSON request and a borrowed reply token;
\ REPLY! copies its JSON response. The request and token live only in that
\ callback. A provider throw is caught there and rethrown by QUERY.
\ STORAGE CLASS. CALLER-OWNED query and source records, initially zero-filled;
\ one task uses each at a time. CLOSE is idempotent. Records are never copied
\ while live, and CLOSE precedes image capture. The callback descriptor is
\ PROCESS-WIDE; provider errors and busy state are in the source record.

require src/lib/error.f
require src/lib/prelude.f
require src/lib/ffi-abi.f
require src/lib/ffi-callback.f
require src/lib/task.f

package SPARQL
public

-10000 constant E-FIRST
-10099 constant E-LAST
-10000 s" invalid SPARQL query" ERROR:CODE E-SYNTAX
-10001 s" the RDF source failed" ERROR:CODE E-SOURCE
-10002 s" SPARQL evaluation failed" ERROR:CODE E-EVALUATION
-10003 s" invalid SPARQL argument" ERROR:CODE E-ARGUMENT
-10004 s" the SPARQL native bridge failed" ERROR:CODE E-NATIVE
-10005 s" SPARQL record is closed, busy or uninitialized"
   ERROR:CODE E-STATE

ENUM form select ask construct-query describe ;ENUM
NEWTYPE reply

STRUCTURE query OPAQUE DERIVE addr
   FIELD native ptr u8
   FIELD busy bool
;STRUCTURE

STRUCTURE source OPAQUE DERIVE addr
   FIELD context ptr u8
   FIELD read [ ptr u8 ptr u8 n reply -- ]
   FIELD ready bool
   FIELD busy bool
   FIELD fault n
;STRUCTURE

private

CAST: >SOURCE ( ptr u8 -- ptr source )
CAST: >REPLY ( ptr u8 -- reply )
CAST: REPLY>PTR ( reply -- ptr u8 )

HB-TARGET-MACOS? [if]
LIBRARY libhabu_sparql.dylib
[then]
HB-TARGET-MACOS? 0= [if]
LIBRARY libhabu_sparql.so
[then]

FUNCTION: NATIVE-NEW hb_sparql_new ( -- ptr u8 )
;FUNCTION
FUNCTION: NATIVE-FREE hb_sparql_free ( ptr u8 -- )
;FUNCTION
FUNCTION: NATIVE-PARSE hb_sparql_parse
   ( ptr u8 ptr u8 n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: NATIVE-FORM hb_sparql_form ( ptr u8 -- i32 )
;FUNCTION
FUNCTION: NATIVE-BIND hb_sparql_bind
   ( ptr u8 ptr u8 n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: NATIVE-QUERY hb_sparql_query
   ( ptr u8 ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: NATIVE-REPLY hb_sparql_reply_set
   ( ptr u8 ptr u8 n -- ) ;FUNCTION
FUNCTION: RESULT-PTR hb_sparql_result_ptr
   ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: RESULT-LEN hb_sparql_result_len
   ( ptr u8 -- n ) ;FUNCTION
FUNCTION: ERROR-PTR hb_sparql_error_ptr
   ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: ERROR-LEN hb_sparql_error_len
   ( ptr u8 -- n ) ;FUNCTION
FUNCTION: DATASET-PTR hb_sparql_dataset_ptr
   ( ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: DATASET-LEN hb_sparql_dataset_len
   ( ptr u8 -- n ) ;FUNCTION

: SPAN-CHECK ( ptr u8 n -- )
   {: a u:n :}
   u 0< if E-ARGUMENT throw then
   u 0> a NULL-PTR = and if E-ARGUMENT throw then ;

: IDLE ( ptr query -- )
   QUERY-BUSY @ if E-STATE throw then ;

: SOURCE-IDLE ( ptr source -- )
   SOURCE-BUSY @ if E-STATE throw then ;

: HANDLE ( ptr query -- ptr u8 )
   QUERY-NATIVE @ dup NULL-PTR = if
      drop E-STATE throw
   then ;

: OPEN ( ptr query -- ptr u8 )
   {: q :}
   q QUERY-NATIVE @ NULL-PTR <> if q HANDLE exit then
   NATIVE-NEW dup NULL-PTR = if drop E-NATIVE throw then
   dup q QUERY-NATIVE ! ;

: STATUS ( n -- )
   dup 0= if drop exit then
   dup 1 = if drop E-SYNTAX throw then
   dup 2 = if drop E-SOURCE throw then
   dup 3 = if drop E-EVALUATION throw then
   dup 4 = if drop E-ARGUMENT throw then
   6 = if E-STATE throw then
   E-NATIVE throw ;

: >FORM ( n -- form )
   dup 1 = if drop construct form select exit then
   dup 2 = if drop construct form ask exit then
   dup 3 = if drop construct form construct-query exit then
   4 = if construct form describe exit then
   E-STATE throw ;

: SOURCE-CALL ( ptr source ptr u8 n reply -- )
   {: src request length:n out :}
   src SOURCE-CONTEXT @ request length out
      src SOURCE-READ @ execute ;

: PROVIDE ( ptr u8 ptr u8 n ptr u8 -- n )
   {: context request length:n out :}
   context >SOURCE {: src :}
   src SOURCE-FAULT @ 0<> if -1 exit then
   [: src request length out >REPLY SOURCE-CALL ;] catch
   dup src SOURCE-FAULT ! 0<> if -1 else 0 then ;

CALLBACK: READ-SOURCE ( ptr u8 ptr u8 n ptr u8 -- i32 )
   -1 FALLBACK
;CALLBACK
' PROVIDE READ-SOURCE-BODY !

: SOURCE-ENTRY ( -- n )
   READ-SOURCE TASK:CALLER-CONTEXT FFI-CB:ENTRY ;

: QUERY-RUN ( ptr query ptr source -- )
   {: q src :}
   q HANDLE src BYTE-VIEW SOURCE-ENTRY NATIVE-QUERY
      {: code:n :}
   src SOURCE-FAULT @ dup 0<> if throw then drop
   code STATUS ;

: QUERY-END ( ptr query ptr source -- )
   false swap SOURCE-BUSY !
   false swap QUERY-BUSY ! ;

: QUERY-START ( ptr query ptr source -- )
   {: q src :}
   q IDLE src SOURCE-IDLE
   src SOURCE-READY @ 0= if E-STATE throw then
   q HANDLE drop
   0 src SOURCE-FAULT !
   true src SOURCE-BUSY ! true q QUERY-BUSY ! ;

: SCOPED ( R ptr query [ R ptr query -- S ] -- S )
   execute ;

public

: PARSE ( ptr query ptr u8 n ptr u8 n -- )
   {: q text length:n base base-length:n :}
   q IDLE text length SPAN-CHECK
   base base-length SPAN-CHECK
   q OPEN text length base base-length NATIVE-PARSE STATUS ;

: FORM ( ptr query -- form )
   dup IDLE HANDLE NATIVE-FORM >FORM ;

: BIND ( ptr query ptr u8 n ptr u8 n -- )
   {: q name name-length:n term term-length:n :}
   q IDLE name name-length SPAN-CHECK
   term term-length SPAN-CHECK
   q HANDLE name name-length term term-length
      NATIVE-BIND STATUS ;

: SOURCE! ( ptr source ptr u8
      [ ptr u8 ptr u8 n reply -- ] -- )
   {: src context read :}
   src SOURCE-IDLE
   context src SOURCE-CONTEXT !
   read src SOURCE-READ ! true src SOURCE-READY ! ;

: REPLY! ( reply ptr u8 n -- )
   {: out text length:n :}
   text length SPAN-CHECK
   out REPLY>PTR text length NATIVE-REPLY ;

: QUERY ( ptr query ptr source -- )
   {: q src :}
   q src QUERY-START
   [: q src QUERY-RUN ;] [: q src QUERY-END ;] finally ;

: RESULT$ ( ptr query -- ptr u8 n )
   dup IDLE HANDLE {: handle :}
   handle RESULT-PTR handle RESULT-LEN ;

: ERROR$ ( ptr query -- ptr u8 n )
   dup IDLE QUERY-NATIVE @ {: handle :}
   handle NULL-PTR = if NULL-PTR 0 exit then
   handle ERROR-PTR handle ERROR-LEN ;

: DATASET$ ( ptr query -- ptr u8 n )
   dup IDLE HANDLE {: handle :}
   handle DATASET-PTR handle DATASET-LEN ;

: CLOSE ( ptr query -- )
   {: q :}
   q IDLE
   q QUERY-NATIVE @ dup NULL-PTR <> if
      NATIVE-FREE
   else drop then
   NULL-PTR q QUERY-NATIVE ! ;

: WITH-QUERY ( R ptr query [ R ptr query -- S ] -- S )
   {: q body :}
   q IDLE
   q body [: SCOPED ;] [: q CLOSE ;] finally ;

seal-package
;package
