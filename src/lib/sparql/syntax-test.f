\ Query syntax through the parser's load path. The W3C syntax profile is
\ the release gate; these are the model-facing migration examples.

require src/lib/test.f
require src/lib/sparql/parse.f

package SPARQL

: PARSE-ONE ( ptr u8 n ptr ARENA:arena -- )
   {: query size:n arena :}
   query size s" https://example.org/base/"
      arena PARSE drop ;

: ACCEPT ( ptr u8 n -- )
   [: PARSE-ONE ;] ARENA:WITH ;

: MIGRATION ( -- )
   s" PREFIX zen: <urn:zen:> SELECT (COUNT(*) AS ?n) WHERE { ?f a zen:Fact }"
      ACCEPT
   s" PREFIX zen: <urn:zen:> SELECT ?s ?v WHERE { ?s zen:retracted true ; zen:version ?v . FILTER(?v = <urn:zen:7>) }"
      ACCEPT
   s" SELECT ?s WHERE { VALUES (?s ?v) { (<urn:one> 1) (<urn:two> UNDEF) } OPTIONAL { ?s <urn:p> ?v } FILTER NOT EXISTS { ?s <urn:q> ?x } }"
      ACCEPT
   s" ASK { GRAPH ?g { ?s (<urn:p>|^<urn:q>)+ ?o } }"
      ACCEPT
   s" CONSTRUCT { ?s <urn:p> ?o } WHERE { ?s <urn:p> ?o } LIMIT 10"
      ACCEPT
   s" DESCRIBE <urn:one> WHERE { <urn:one> ?p ?o }"
      ACCEPT ;

MIGRATION

;package
