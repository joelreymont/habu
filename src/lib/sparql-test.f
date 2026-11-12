\ sparql-test.f - native query semantics through the checked Habu boundary,
\ provider error propagation and cleanup. Requires libhabu_sparql on the
\ dynamic loader path. Run: bin/hb --load src/lib/sparql-test.f

require src/lib/test.f
require src/lib/json.f
require src/lib/sparql.f

package SPARQL-TEST

TYPED-VARIABLE Q SPARQL:query
TYPED-VARIABLE SOURCE SPARQL:source

: FAIL-SOURCE ( ptr u8 ptr u8 n SPARQL:reply -- )
   2drop 2drop E-FS-IO throw ;

: EMPTY-SOURCE ( ptr u8 ptr u8 n SPARQL:reply -- )
   {: context request length:n out :}
   out s\" {\"quads\":[]}" SPARQL:REPLY! ;

: PARSE ( ptr u8 n -- )
   {: text length:n :}
   Q text length s" https://example.test/" SPARQL:PARSE ;

: ABS-QUERY$ ( -- ptr u8 n )
   s" SELECT ?v WHERE { VALUES ?x { -7.25 } BIND(ABS(?x) AS ?v) }" ;

: ABS-RESULT$ ( -- ptr u8 n )
   s\" {\"head\":{\"vars\":[\"v\"]},\"results\":{\"bindings\":[{\"v\":{\"type\":\"literal\",\"value\":\"7.25\",\"datatype\":\"http://www.w3.org/2001/XMLSchema#decimal\"}}]}}" ;

: ABS-QUERY ( -- )
   s" standard ABS through Habu, without a dataset read" T-LABEL
   SOURCE NULL-PTR [: FAIL-SOURCE ;] SPARQL:SOURCE!
   ABS-QUERY$ PARSE
   Q SPARQL:FORM MATCH SPARQL:form
      select OF ENDOF
      ask OF false TTRUE ENDOF
      construct-query OF false TTRUE ENDOF
      describe OF false TTRUE ENDOF
   ;MATCH
   Q SOURCE SPARQL:QUERY
   Q SPARQL:RESULT$ ABS-RESULT$ JSON:VALUE= TTRUE ;

: MALFORMED ( -- )
   s" a syntax error invalidates the old query" T-LABEL
   [: s" SELECT WHERE {" PARSE ;]
      SPARQL:E-SYNTAX TTHROWSQ
   Q SPARQL:ERROR$ nip 0> TTRUE
   [: Q SPARQL:FORM drop ;] SPARQL:E-STATE TTHROWSQ ;

: EMPTY-DATA ( -- )
   s" a successful provider reply is read by the evaluator" T-LABEL
   SOURCE NULL-PTR [: EMPTY-SOURCE ;] SPARQL:SOURCE!
   s" ASK { ?s ?p ?o }" PARSE
   Q SOURCE SPARQL:QUERY
   Q SPARQL:RESULT$
   s\" {\"head\":{},\"boolean\":false}" JSON:VALUE= TTRUE ;

: PROVIDER-THROW ( -- )
   s" a provider throw survives the native callback" T-LABEL
   SOURCE NULL-PTR [: FAIL-SOURCE ;] SPARQL:SOURCE!
   s" SELECT * WHERE { ?s ?p ?o }" PARSE
   [: Q SOURCE SPARQL:QUERY ;] E-FS-IO TTHROWSQ
   Q SPARQL:CLOSE Q SPARQL:CLOSE ;

: THROW-IN-SCOPE ( ptr SPARQL:query -- )
   drop ABS-QUERY$ PARSE E-FS-IO throw ;

: CLEANUP ( -- )
   s" a query scope closes native ownership after a throw" T-LABEL
   [: Q [: THROW-IN-SCOPE ;] SPARQL:WITH-QUERY ;]
      E-FS-IO TTHROWSQ
   [: Q SPARQL:FORM drop ;] SPARQL:E-STATE TTHROWSQ
   Q SPARQL:CLOSE ;

public

: RUN ( -- )
   T-RESET
   [: ABS-QUERY MALFORMED EMPTY-DATA PROVIDER-THROW CLEANUP ;]
   [: Q SPARQL:CLOSE ;] finally ;

;package

SPARQL-TEST:RUN
T-REPORT
