\ Result batches and callback failures through the public Habu interface.
\ This checks the host boundary; the W3C suite checks query evaluation.
require src/lib/test.f
require src/lib/json.f
require src/lib/codegen.f
require src/lib/sparql.f
require src/lib/time.f

package SPARQL-TEST
private

4096 CODEGEN:BUFFER OUTPUT
variable READS
variable KIND

: REPLY$ ( -- ptr u8 n )
   KIND @ 0= if s" [false]" exit then
   KIND @ 1 = if
      s\" [{\"x\":{\"type\":\"literal\",\"value\":\"one\"}}]" exit
   then
   s\" [{\"s\":{\"type\":\"bnode\",\"value\":\"a/b\"},\"p\":{\"type\":\"uri\",\"value\":\"urn:p\"},\"o\":{\"type\":\"literal\",\"value\":\"x\\n\\\"\"}}]" ;

: SOURCE ( ptr u8 ptr u8 n -- ptr u8 n )
   2drop drop
   READS @ 0= {: first:bool :}
   1 READS +!
   first if REPLY$ else s" []" then ;

: COLLECT ( ptr u8 ptr u8 n -- )
   {: context bytes length:n :}
   bytes length OUTPUT CODEGEN:APPEND-STRING ;

: FAIL-READ ( ptr u8 ptr u8 n -- ptr u8 n )
   2drop drop E-FS-IO throw ;

: FAIL-WRITE ( ptr u8 ptr u8 n -- )
   2drop drop E-FS-IO throw ;

: RESET ( -- ) 0 READS ! OUTPUT CODEGEN:RESET ;

: RUN-QUERY ( ptr u8 n -- )
   s" " s" test_backend" 0 NULL-PTR [: SOURCE ;] [: COLLECT ;]
      SPARQL:QUERY ;

public

: RUN ( -- )
   T-RESET RESET 0 KIND !
   s" one boolean row answers ASK" T-LABEL
   s" ASK { ?s ?p ?o }" RUN-QUERY
   OUTPUT CODEGEN:CONTENTS
   s\" {\"head\":{},\"boolean\":false}" JSON:VALUE= TTRUE

   RESET 1 KIND !
   s" SELECT rows are standard result bindings" T-LABEL
   s" SELECT ?x WHERE { VALUES ?x { 'one' } }" RUN-QUERY
   OUTPUT CODEGEN:CONTENTS
   s\" {\"head\":{\"vars\":[\"x\"]},\"results\":{\"bindings\":[{\"x\":{\"type\":\"literal\",\"value\":\"one\"}}]}}"
      JSON:VALUE= TTRUE

   RESET 2 KIND !
   s" graph terms escape literals and preserve blank node identity" T-LABEL
   s\" CONSTRUCT { _:a <urn:p> \"x\\n\\\"\" } WHERE {}" RUN-QUERY
   OUTPUT CODEGEN:CONTENTS
   s\" _:b612f62 <urn:p> \"x\\u000a\\\"\" .\n" T$=

   s" source failure propagates through QUERY cleanup" T-LABEL
   [: s" ASK {}" s" " s" test_backend" 0 NULL-PTR
      [: FAIL-READ ;] [: COLLECT ;] SPARQL:QUERY ;]
      E-FS-IO TTHROWSQ

   s" output failure propagates through QUERY cleanup" T-LABEL
   [: s" ASK {}" s" " s" test_backend" 0 NULL-PTR
      [: SOURCE ;] [: FAIL-WRITE ;] SPARQL:QUERY ;]
      E-FS-IO TTHROWSQ

   s" an expired deadline ends CPU-only parsing" T-LABEL
   [: s" ASK {}" s" " s" test_backend" TIME:MONO-NS 1- NULL-PTR
      [: SOURCE ;] [: COLLECT ;] SPARQL:QUERY ;]
      SPARQL:E-DEADLINE TTHROWSQ ;

;package

SPARQL-TEST:RUN
T-REPORT
