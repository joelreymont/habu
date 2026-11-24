split-str
\ Read-only SPARQL, compiled in Habu and evaluated by PostgreSQL.
\ QUERY and QUERY-ROWS take query text, its base IRI, a trusted backend schema and an
\ absolute monotonic deadline (0 means none). The schema is never query input.
\ QUERY-ROWS calls the host with the compiled statement and consumes borrowed
\ database rows through WRITE-ROW. QUERY supports a byte interface. Its
\ READ receives a parameterized SQL descriptor, then {"op":"next"}; it
\ returns a JSON array of row values, [] at end. A returned span lasts until
\ the next READ. SELECT rows are binding objects, ASK one boolean, graph rows
\ objects with s/p/o RDF terms. WRITE receives result chunks synchronously.
\ READ and WRITE are stable quotations; per-call state goes through context.
\ The caller owns authorization, one read-only snapshot, cursor cleanup and
\ its output bound. Discard partial output when QUERY throws.
\ STORAGE CLASS. CALLER-OWNED: all compilation and result storage is released
\ by QUERY. No native bridge, process state, connection or dataset copy.

require src/lib/sparql/result.f

package SPARQL
private

\ A call borrows its statement, parameters, and scoped writer until return.
: NEW-CALL ( ^u8 n ^u8 n ^u8 n n ^u8 [ ^u8 ^u8 n -- ] ^ARENA:arena -- ^call )
   {: query qu:n base bu:n schema su:n deadline:n context write arena :}
   query qu base bu deadline arena PARSE-UNTIL schema su COMPILE {: plan :}
   arena ARENA:ALLOCATOR [type] call ALLOC:NEW {: call :}
   plan call CALL-PLAN ! context call CALL-CONTEXT ! write call CALL-WRITE ! call ;

: RUN ( ^u8 n ^u8 n ^u8 n n ^u8
      [ ^u8 ^u8 n -- ^u8 n ] [ ^u8 ^u8 n -- ] ^ARENA:arena -- )
   {: query qu:n base bu:n schema su:n deadline:n context read write arena :}
   query qu base bu schema su deadline context write arena NEW-CALL {: call :}
   call CALL-PLAN @ {: plan :}
   read call CALL-READ !
   call BEGIN-RESULT
   call plan PLAN-REQUEST$ READ-ROWS
   begin call -rot BATCH while
      call s\" {\"op\":\"next\"}" READ-ROWS
   repeat
   call END-RESULT ;

: RUN-ROWS ( ^u8 n ^u8 n ^u8 n n ^u8
      [ ^u8 ^call -- ] [ ^u8 ^u8 n -- ] ^ARENA:arena -- )
   {: query qu:n base bu:n schema su:n deadline:n context execute-plan write arena :}
   query qu base bu schema su deadline context write arena NEW-CALL {: call :}
   call BEGIN-RESULT context call execute-plan execute call END-RESULT ;

public

: STATEMENT$ ( ^call -- ^u8 n ) CALL-PLAN @ SQL$ ;

: EACH-PARAMETER ( R ^call [ R ^u8 n -- R ] -- R )
   {: call body :}
   call CALL-PLAN @ PLAN-ARGUMENTS @ {: args :}
   args VEC:COUNT 0 ?do
      args i VEC:AT {: arg :}
      arg ARGUMENT-TEXT @ arg ARGUMENT-SIZE @ body execute
   loop ;

: GRAPH-RESULT? ( ^call -- bool )
   FORM dup N-CONSTRUCT = swap N-DESCRIBE = or ;

: WRITE-ROW ( ^call ^u8 n -- )
   {: call row size:n :}
   call CHECK-RESULT
   call row size call GRAPH-RESULT? if [: ROW ;] ARENA:WITH
   else call CALL-PLAN @ TREE SYNTAX-ARENA @ ROW then ;

: QUERY-ROWS ( ^u8 n ^u8 n ^u8 n n ^u8
      [ ^u8 ^call -- ] [ ^u8 ^u8 n -- ] -- )
   [: RUN-ROWS ;] ARENA:WITH ;

: QUERY ( ^u8 n ^u8 n ^u8 n n ^u8
      [ ^u8 ^u8 n -- ^u8 n ]
      [ ^u8 ^u8 n -- ] -- )
   [: RUN ;] ARENA:WITH ;

seal-package
;package
