\ Read-only SPARQL, compiled in Habu and evaluated by PostgreSQL.
\ QUERY takes query text, its base IRI, a trusted backend schema and an
\ absolute monotonic deadline (0 means none). The schema is never query input.
\ READ receives a parameterized SQL descriptor, then {"op":"next"}; it
\ returns a JSON array of row values, [] at end. A returned span lasts until
\ the next READ. SELECT rows are binding objects, ASK one boolean, graph rows
\ objects with s/p/o RDF terms. WRITE receives result chunks synchronously.
\ The caller owns authorization, one read-only snapshot, cursor cleanup and
\ its output bound. Discard partial output when QUERY throws.
\ STORAGE CLASS. CALLER-OWNED: all compilation and result storage is released
\ by QUERY. No native bridge, process state, connection or dataset copy.

require src/lib/sparql/result.f

package SPARQL
private

: RUN ( ptr u8 n ptr u8 n ptr u8 n n ptr u8
      [ ptr u8 ptr u8 n -- ptr u8 n ]
      [ ptr u8 ptr u8 n -- ] ptr ARENA:arena -- )
   {: query qu:n base bu:n schema su:n deadline:n context read write arena :}
   query qu base bu deadline arena PARSE-UNTIL
      schema su COMPILE {: plan :}
   arena ARENA:ALLOCATOR [type] call ALLOC:NEW {: call :}
   plan call CALL-PLAN ! context call CALL-CONTEXT !
   read call CALL-READ ! write call CALL-WRITE !
   call BEGIN-RESULT
   call plan PLAN-REQUEST$ READ-ROWS
   begin call -rot BATCH while
      call s\" {\"op\":\"next\"}" READ-ROWS
   repeat
   call END-RESULT ;

public

: QUERY ( ptr u8 n ptr u8 n ptr u8 n n ptr u8
      [ ptr u8 ptr u8 n -- ptr u8 n ]
      [ ptr u8 ptr u8 n -- ] -- )
   [: RUN ;] ARENA:WITH ;

seal-package
;package
