\ sparql-test.f - streaming output and callback failures through Habu.
\ Run with the native library on the loader path:
\   hb --load src/lib/sparql-test.f

require src/lib/test.f
require src/lib/json.f
require src/lib/codegen.f
require src/lib/sparql.f

package SPARQL-TEST

1024 CODEGEN:BUFFER OUTPUT

: REQUEST$ ( -- ptr u8 n )
   s\" {\"query\":\"ASK { ?s ?p ?o }\",\"timeout_ms\":5000,\"max_rows\":1000,\"max_bytes\":1048576,\"max_scan_rows\":100000}" ;

: EMPTY ( ptr u8 ptr u8 n -- ptr u8 n )
   2drop drop s\" {\"quads\":[],\"next\":null}" ;

: COLLECT ( ptr u8 n ptr u8 n -- )
   {: context status:n bytes length:n :}
   status 0<> if SPARQL:E-QUERY throw then
   bytes length OUTPUT CODEGEN:APPEND-STRING ;

: FAIL-READ ( ptr u8 ptr u8 n -- ptr u8 n )
   2drop drop E-FS-IO throw ;

: FAIL-WRITE ( ptr u8 n ptr u8 n -- )
   2drop 2drop E-FS-IO throw ;

public

: RUN ( -- )
   T-RESET OUTPUT CODEGEN:RESET
   s" an empty source answers ASK false" T-LABEL
   REQUEST$ NULL-PTR [: EMPTY ;] [: COLLECT ;] SPARQL:QUERY
   OUTPUT CODEGEN:CONTENTS
   s\" {\"head\":{},\"boolean\":false}" JSON:VALUE= TTRUE
   s" source failure is rethrown after C returns" T-LABEL
   [: REQUEST$ NULL-PTR [: FAIL-READ ;] [: COLLECT ;]
      SPARQL:QUERY ;] E-FS-IO TTHROWSQ
   s" output failure is rethrown after C returns" T-LABEL
   [: REQUEST$ NULL-PTR [: EMPTY ;] [: FAIL-WRITE ;]
      SPARQL:QUERY ;] E-FS-IO TTHROWSQ ;

;package

SPARQL-TEST:RUN
T-REPORT
