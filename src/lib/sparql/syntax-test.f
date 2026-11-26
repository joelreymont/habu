\ Query syntax through the parser's load path: the model-facing examples
\ and the grammar's other forms parse, and each rejected query, a form this
\ endpoint drops included, reads exactly its text. Flat chains of 10,000
\ parse, variables under nested groups make at most 10 nodes per token,
\ relative IRIs take little more arena than their text, and only nesting is
\ bounded.

require src/lib/test/assert.f
require src/lib/sparql/parse.f
require src/lib/sparql/syntax.f
require src/lib/vector.f
require src/lib/reason.f

package SPARQL

: PARSE-IN ( str ^ARENA:arena -- )
   PARSE drop ;

: ACCEPT ( str -- )
   [: PARSE-IN ;] ARENA:WITH ;

: MIGRATION ( -- )
   s" PREFIX zen: <urn:zen:> SELECT (COUNT(*) AS ?n) WHERE { ?f a zen:Fact }"
      ACCEPT
   s" PREFIX zen: <urn:zen:> SELECT ?s ?v WHERE { ?s zen:retracted true ; zen:version ?v . FILTER(?v = <urn:zen:7>) }"
      ACCEPT
   s" SELECT ?s WHERE { VALUES (?s ?v) { (<urn:one> 1) (<urn:two> UNDEF) } OPTIONAL { ?s <urn:p> ?v } FILTER NOT EXISTS { ?s <urn:q> ?x } }"
      ACCEPT
   s" ASK { GRAPH ?g { ?s (<urn:p>|^<urn:q>)+ ?o } }"
      ACCEPT ;

MIGRATION

\ The first triple of the query's group.
: FIRST-TRIPLE ( ^syntax -- ^syntax n )
   dup dup SYNTAX-ROOT @ NODE-B@
   over swap NODE-A@
   over swap NODE-A@ ;

: PREDICATE-K ( ^syntax n -- n )
   over swap NODE-B@ NODE-K ;

: OBJECT$ ( ^syntax n -- str )
   over swap NODE-C@ TEXT$ ;

\ The query's first triple has a predicate of kind n and an object with the
\ second text.
: PARSES-TO ( str n str -- )
   {: query:str kind:n object:str :}
   [: query swap PARSE FIRST-TRIPLE
      2dup PREDICATE-K kind T=
      OBJECT$ object T$= ;] ARENA:WITH ;

\ Forms the examples do not show.
: GRAMMAR ( -- )
   s" SELECT ?x ?x projects one ?x" T-LABEL
   [: s" SELECT ?x ?x WHERE { ?x <urn:p> ?o }" swap PARSE
      dup dup SYNTAX-ROOT @ QUERY-NAMES LIST-LENGTH ;] ARENA:WITH
      1 T=
   s" a relative IRI resolves against BASE" T-LABEL
   s" BASE <https://example.org/base/> SELECT * WHERE { ?s <p> ?o }" ACCEPT
   s" a variable may start with a Hebrew letter" T-LABEL
   s" SELECT ?א WHERE { }" ACCEPT
   s" a prefix may start with a Hebrew letter" T-LABEL
   s" PREFIX א: <urn:x:> SELECT * WHERE { ?s א:p ?o }" ACCEPT
   s" a sign right before a number is the object's" T-LABEL
   s" SELECT * { ?s <urn:p> +1 }" N-IRI s" +1" PARSES-TO
   s" a + apart from the number is the path's" T-LABEL
   s" SELECT * { ?s <urn:p>+ 1 }" N-PATH s" 1" PARSES-TO
   s" a relative BASE resolves against the BASE before it" T-LABEL
   s" BASE <http://a/b/> BASE <c/> SELECT * { ?s <p> <d> }"
      N-IRI s" http://a/b/c/d" PARSES-TO
   s" a relative IRI takes no dot segments from BASE" T-LABEL
   s" BASE <http://h/a/../b/c> SELECT * { ?s <urn:p> <d> }"
      N-IRI s" http://h/b/d" PARSES-TO
   s" a word ends at a dot" T-LABEL
   s" SELECT * { ?s <urn:p> true.FILTER(true) }"
      N-IRI s" true" PARSES-TO ;

GRAMMAR

: STAR-ORDER-IN ( ^ARENA:arena -- )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" SELECT * { ?b ?a ?c }" arena PARSE {: state :}
   state state SYNTAX-ROOT @ QUERY-NAMES begin dup 0<> while
      {: list:n :}
      state state list NODE-A@ TEXT$ out VEC:APPEND
      state list NODE-B@
   repeat drop
   out VEC:BYTES s" bac" T$= ;

: STAR-ORDER ( -- )
   s" SELECT * names variables in order of first use" T-LABEL
   [: STAR-ORDER-IN ;] ARENA:WITH ;

STAR-ORDER

: NAMES ( -- )
   s" a prefix does not end in a dot" T-LABEL
   s" PREFIX : <urn:> SELECT * { ?s <urn:p> true.:p <urn:q> 1 }"
      N-IRI s" true" PARSES-TO
   s" nor does a blank node label" T-LABEL
   s" PREFIX : <urn:> SELECT * { ?s <urn:p> _:b.:p <urn:q> 1 }"
      N-IRI s" b" PARSES-TO
   s" a blank node label has no colon" T-LABEL
   s" PREFIX : <urn:> SELECT * { _:b:p 1 }"
      N-IRI s" 1" PARSES-TO
   s" a word ends at a minus" T-LABEL
   s" SELECT * { ?s a-1 }" N-IRI s" -1" PARSES-TO
   s" a prefix holds a dot" T-LABEL
   s" PREFIX x.y: <urn:> SELECT * { ?s <urn:p> x.y:z }"
      N-IRI s" urn:z" PARSES-TO
   s" a local name holds a colon" T-LABEL
   s" PREFIX a: <urn:> SELECT * { ?s <urn:p> a:b:c }"
      N-IRI s" urn:b:c" PARSES-TO
   s" a local name does not end in a dot" T-LABEL
   s" PREFIX a: <urn:> SELECT * { ?s <urn:p> a:b.}"
      N-IRI s" urn:b" PARSES-TO
   s" nor does it start with one or with a minus" T-LABEL
   s" PREFIX : <urn:> SELECT * { ?s <urn:p> :._:b <urn:q> 1 }"
      N-IRI s" urn:" PARSES-TO
   s" PREFIX : <urn:> ASK { ?s ?p ?o FILTER(:-1 = 2) }" ACCEPT
   s" a number's exponent has a digit" T-LABEL
   s" PREFIX e: <urn:> SELECT * { ?s <urn:p> 1.e:b <urn:q> 2 }"
      N-IRI s" 1" PARSES-TO
   s" PREFIX e: <urn:> ASK { ?s ?p (1e:b) }" ACCEPT
   s" SELECT * { ?s <urn:p> 1.5E+2, 1.e2, .5e-1 }"
      N-IRI s" 1.5E+2" PARSES-TO ;

NAMES

\ The last IRI of the query: its WHERE holds one relative reference.
: RESOLVED ( ^syntax -- str )
   {: state :}
   0 state SYNTAX-NODES @ VEC:COUNT 1 ?do
      state i NODE-K N-IRI = if drop i then
   loop
   state swap TEXT$ ;

: RESOLVES-IN ( str str ^ARENA:arena -- )
   {: ref:str want:str arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" BASE <http://a/b/c/d;p?q> ASK { ?s ?p <" out VEC:APPEND
   ref out VEC:APPEND s" > }" out VEC:APPEND
   out VEC:BYTES arena PARSE RESOLVED want T$= ;

: RESOLVES ( str str -- )
   [: RESOLVES-IN ;] ARENA:WITH ;

: NORMAL-REFERENCES ( -- )
   s" RFC 3986 section 5.4.1 resolves as written" T-LABEL
   s" g:h" s" g:h" RESOLVES
   s" g" s" http://a/b/c/g" RESOLVES
   s" ./g" s" http://a/b/c/g" RESOLVES
   s" g/" s" http://a/b/c/g/" RESOLVES
   s" /g" s" http://a/g" RESOLVES
   s" //g" s" http://g" RESOLVES
   s" ?y" s" http://a/b/c/d;p?y" RESOLVES
   s" g?y" s" http://a/b/c/g?y" RESOLVES
   s" #s" s" http://a/b/c/d;p?q#s" RESOLVES
   s" g#s" s" http://a/b/c/g#s" RESOLVES
   s" g?y#s" s" http://a/b/c/g?y#s" RESOLVES
   s" ;x" s" http://a/b/c/;x" RESOLVES
   s" g;x" s" http://a/b/c/g;x" RESOLVES
   s" g;x?y#s" s" http://a/b/c/g;x?y#s" RESOLVES
   s" " s" http://a/b/c/d;p?q" RESOLVES
   s" ." s" http://a/b/c/" RESOLVES
   s" ./" s" http://a/b/c/" RESOLVES
   s" .." s" http://a/b/" RESOLVES
   s" ../" s" http://a/b/" RESOLVES
   s" ../g" s" http://a/b/g" RESOLVES
   s" ../.." s" http://a/" RESOLVES
   s" ../../" s" http://a/" RESOLVES
   s" ../../g" s" http://a/g" RESOLVES ;

NORMAL-REFERENCES

: ABNORMAL-REFERENCES ( -- )
   s" RFC 3986 section 5.4.2 resolves strictly" T-LABEL
   s" ../../../g" s" http://a/g" RESOLVES
   s" ../../../../g" s" http://a/g" RESOLVES
   s" /./g" s" http://a/g" RESOLVES
   s" /../g" s" http://a/g" RESOLVES
   s" g." s" http://a/b/c/g." RESOLVES
   s" .g" s" http://a/b/c/.g" RESOLVES
   s" g.." s" http://a/b/c/g.." RESOLVES
   s" ..g" s" http://a/b/c/..g" RESOLVES
   s" ./../g" s" http://a/b/g" RESOLVES
   s" ./g/." s" http://a/b/c/g/" RESOLVES
   s" g/./h" s" http://a/b/c/g/h" RESOLVES
   s" g/../h" s" http://a/b/c/h" RESOLVES
   s" g;x=1/./y" s" http://a/b/c/g;x=1/y" RESOLVES
   s" g;x=1/../y" s" http://a/b/c/y" RESOLVES
   s" g?y/./x" s" http://a/b/c/g?y/./x" RESOLVES
   s" g?y/../x" s" http://a/b/c/g?y/../x" RESOLVES
   s" g#s/./x" s" http://a/b/c/g#s/./x" RESOLVES
   s" g#s/../x" s" http://a/b/c/g#s/../x" RESOLVES
   s" http:g" s" http:g" RESOLVES ;

ABNORMAL-REFERENCES

: DOTTED-AUTHORITY ( -- )
   s" a reference with an authority loses its dot segments" T-LABEL
   s" //g/a/../b" s" http://g/b" RESOLVES ;

DOTTED-AUTHORITY

\ The query is rejected with exactly the text.
: REJECTS ( str str -- )
   {: query:str want:str :}
   [: query [: PARSE-IN ;] ARENA:WITH ;] E-QUERY TTHROWSQ
   E-QUERY REASON:TEXT$ want T$= ;

: REJECTIONS ( -- )
   s" a rejection shows its place, what was expected and the fix" T-LABEL
   \ On line 1.
   s" SELECT ?s WHERE { ?s ?p }"
   s\" 1:25: expected a variable, IRI, prefixed name, literal or blank node, found `}`; write a term such as ?x or <iri> here\n1 | SELECT ?s WHERE { ?s ?p }\n                            ^"
      REJECTS
   \ On a later line.
   s\" SELECT ?s\nWHERE { ?s ?p ?o\n  FILTER(?s = ) }"
   s\" 3:15: expected a variable, IRI, prefixed name, literal or blank node, found `)`; write a term such as ?x or <iri> here\n3 |   FILTER(?s = ) }\n                  ^"
      REJECTS
   \ Found cut at REASON:QUOTE-MOST bytes.
   s" SELECT * WHERE { ?s ?p ?o } <http://example.org/a-very-long-iri-name/x>"
   s\" 1:29: expected the end of the query, found `<http://example.org/a-very-long-...`; remove the text from here on\n1 | SELECT * WHERE { ?s ?p ?o } <http://example.org/a-very-long-iri-name/x>\n                                ^"
      REJECTS
   \ At the end of the query.
   s" SELECT * WHERE { ?s ?p ?o"
   s\" 1:26: expected a variable, IRI, prefixed name, literal or blank node, found the end of the query; write a term such as ?x or <iri> here\n1 | SELECT * WHERE { ?s ?p ?o\n                             ^"
      REJECTS
   \ A line over 80 bytes, cut on both sides.
   s" SELECT * WHERE { ?subject0123456789 <http://example.org/predicate/one/two/three> ?o2 ?o3 <http://example.org/more/stuff/here/and/there> }"
   s\" 1:86: expected . or } after a triple, found `?o3`; end the triple before with .\n1 | ...xample.org/predicate/one/two/three> ?o2 ?o3 <http://example.org/more/stuff/here/...\n                                               ^"
      REJECTS
   \ At a lexer byte.
   s\" SELECT * WHERE { ?s ?p \"abc }"
   s\" 1:30: expected the string's closing quote, found the end of the query; close the string with the quote that opens it\n1 | SELECT * WHERE { ?s ?p \"abc }\n                                 ^"
      REJECTS
   \ A byte that is not UTF-8.
   s\" ASK { ?s ?p \"\xFF\" }"
   s\" 1:14: expected UTF-8 text, found `\\xFF`; write the query in UTF-8\n1 | ASK { ?s ?p \"\\xFF\" }\n                 ^"
      REJECTS
   \ U+0000 in a string, as a byte and as an escape.
   s\" ASK { ?s ?p \"a\x00b\" }"
   s\" 1:15: expected a string without U+0000, found `U+0000`; remove it\n1 | ASK { ?s ?p \"aU+0000b\" }\n                  ^"
      REJECTS
   s\" ASK { ?s ?p \"a\\u0000b\" }"
   s\" 1:15: expected a string without U+0000, found `U+0000`; remove it\n1 | ASK { ?s ?p \"a\\u0000b\" }\n                  ^"
      REJECTS
   \ A carriage return alone does not end the line.
   s\" SELECT *\x0DWHERE { ?s ?p }\x0D"
   s\" 1:24: expected a variable, IRI, prefixed name, literal or blank node, found `}`; write a term such as ?x or <iri> here\n1 | SELECT *U+000DWHERE { ?s ?p }U+000D\n                                ^"
      REJECTS
   \ A short \u escape.
   s\" ASK { ?s ?p \"\\u12\" }"
   s\" 1:18: expected a hex digit (\\u takes 4, \\U 8), found `\"`; write \\u with 4 hex digits, as in \\u00E9\n1 | ASK { ?s ?p \"\\u12\" }\n                     ^"
      REJECTS
   \ An escaped surrogate half.
   s\" ASK { ?s ?p \"\\uD800\" }"
   s\" 1:14: expected a Unicode scalar value: at most 10FFFF, not D800 to DFFF, found `\\`; write the character itself or one \\U escape, as in \\U0001F600\n1 | ASK { ?s ?p \"\\uD800\" }\n                 ^"
      REJECTS
   \ A percent escape without two hex digits.
   s\" ASK { ?s ?p ex:a%2x }"
   s\" 1:19: expected a hex digit after %, found `x`; write % as \\% or follow it with two hex digits\n1 | ASK { ?s ?p ex:a%2x }\n                      ^"
      REJECTS
   \ A string escape SPARQL lacks.
   s\" ASK { ?s ?p \"a\\q\" }"
   s\" 1:16: expected an escape: \\t \\b \\n \\r \\f \\\" \\' or \\\\, found `q`; write a backslash as \\\\\n1 | ASK { ?s ?p \"a\\q\" }\n                   ^"
      REJECTS
   \ A name right after a number.
   s\" ASK { ?s ?p 1ex }"
   s\" 1:14: expected a variable, IRI, prefixed name, literal or blank node, found `ex`; write a term such as ?x or <iri> here\n1 | ASK { ?s ?p 1ex }\n                 ^"
      REJECTS
   \ A variable name starting with a symbol.
   s\" ASK { ?× ?p ?o }"
   s\" 1:8: expected a keyword, name, variable, IRI, literal or punctuation, found `×`; remove the character\n1 | ASK { ?× ?p ?o }\n           ^"
      REJECTS
   \ A local name escape of a letter.
   s\" ASK { ?s ?p ex:a\\q }"
   s\" 1:18: expected a character a local name escapes: _~.-!$&'()*+,;=/?#@%, found `q`; remove the backslash\n1 | ASK { ?s ?p ex:a\\q }\n                     ^"
      REJECTS
   \ A word, not a prefix, before a dot.
   s\" ASK { ?s ex.:p ?o }"
   s\" 1:10: expected a predicate: an IRI, prefixed name, variable or a, found `ex`; write a predicate and an object after the subject\n1 | ASK { ?s ex.:p ?o }\n             ^"
      REJECTS
   \ A CRLF query's line, shown without its CR.
   s\" SELECT *\r\nWHERE { ?s ?p }\r\n"
   s\" 2:15: expected a variable, IRI, prefixed name, literal or blank node, found `}`; write a term such as ?x or <iri> here\n2 | WHERE { ?s ?p }\n                  ^"
      REJECTS
   \ A line break in a string.
   s\" ASK { ?s ?p \"a\nb\" }"
   s\" 1:15: expected the closing quote before the line ends, found `U+000A`; write the line break as \\n\n1 | ASK { ?s ?p \"a\n                  ^"
      REJECTS
   \ The same, as CRLF.
   s\" ASK { ?s ?p \"a\r\nb\" }"
   s\" 1:15: expected the closing quote before the line ends, found `U+000D`; write the line break as \\n\n1 | ASK { ?s ?p \"a\n                  ^"
      REJECTS
   \ A blank node without a label.
   s\" ASK { ?s ?p _: }"
   s\" 1:15: expected a blank node label after _:, found ` `; write a label after _:, as in _:b1\n1 | ASK { ?s ?p _: }\n                  ^"
      REJECTS
   \ An escape in a prefix.
   s\" ASK { ?s e\\-x:p ?o }"
   s\" 1:11: expected a name character; escapes go only after the colon, found `\\`; remove the escape\n1 | ASK { ?s e\\-x:p ?o }\n              ^"
      REJECTS
   \ A prefix starting with a low line.
   s\" ASK { ?s _ex:p ?o }"
   s\" 1:10: expected a name character, found `_`; remove the character\n1 | ASK { ?s _ex:p ?o }\n             ^"
      REJECTS
   \ A language tag ending in a minus.
   s\" ASK { ?s ?p \"a\"@en- }"
   s\" 1:20: expected a letter or digit after - in a language tag, found ` `; follow the - with letters or digits, as in @en-GB\n1 | ASK { ?s ?p \"a\"@en- }\n                       ^"
      REJECTS
   \ A name starting with a symbol.
   s\" ASK { ?s ?p × }"
   s\" 1:13: expected a keyword, name, variable, IRI, literal or punctuation, found `×`; remove the character\n1 | ASK { ?s ?p × }\n                ^"
      REJECTS
   \ A language tag starting with a digit.
   s\" ASK { ?s ?p \"a\"@1 }"
   s\" 1:17: expected a letter after @, found `1`; write a language tag after @, as in @en\n1 | ASK { ?s ?p \"a\"@1 }\n                    ^"
      REJECTS
   \ A character no token starts with.
   s\" ASK { ?s ?p ~ }"
   s\" 1:13: expected a keyword, name, variable, IRI, literal or punctuation, found `~`; remove the character\n1 | ASK { ?s ?p ~ }\n                ^"
      REJECTS
   \ At a node: an ungrouped variable.
   s" SELECT ?s (COUNT(?o) AS ?n) WHERE { ?s ?p ?o } GROUP BY ?p"
   s\" 1:8: expected a variable in GROUP BY or in an aggregate, found `?s`; add ?s to GROUP BY or wrap it in SAMPLE(?s)\n1 | SELECT ?s (COUNT(?o) AS ?n) WHERE { ?s ?p ?o } GROUP BY ?p\n           ^"
      REJECTS
   \ At a built-in call: an aggregate where none is allowed.
   s" SELECT ?s WHERE { ?s ?p ?o FILTER(COUNT(?o) > 1) }"
   s\" 1:35: expected an expression without an aggregate, found `COUNT`; use aggregates only in SELECT, HAVING and ORDER BY, unnested\n1 | SELECT ?s WHERE { ?s ?p ?o FILTER(COUNT(?o) > 1) }\n                                      ^"
      REJECTS
   \ After an escape, placed and shown as written.
   s\" SELECT ?s\nWHERE { ?s <urn:\\u0041> ?o ?x }"
   s\" 2:28: expected . or } after a triple, found `?x`; end the triple before with .\n2 | WHERE { ?s <urn:\\u0041> ?o ?x }\n                               ^"
      REJECTS
   \ From parse.f.
   s" SELEKT * WHERE { ?s ?p ?o }"
   s\" 1:1: expected SELECT or ASK, found `SELEKT`; start the query with SELECT or ASK\n1 | SELEKT * WHERE { ?s ?p ?o }\n    ^"
      REJECTS
   s" SELECT WHERE { ?s ?p ?o }"
   s\" 1:8: expected *, a variable or (expression AS ?name), found `WHERE`; write * or the variables to select\n1 | SELECT WHERE { ?s ?p ?o }\n           ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY LIMIT 1"
   s\" 1:39: expected a variable or expression to group by, found `LIMIT`; write the variables to group by after GROUP BY\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY LIMIT 1\n                                          ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s HAVING LIMIT 1"
   s\" 1:49: expected a condition in parentheses or a function call, found `LIMIT`; write a condition in parentheses after HAVING\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s HAVING LIMIT 1\n                                                    ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } ORDER BY LIMIT 1"
   s\" 1:39: expected a variable or expression to order by, found `LIMIT`; write the variables to order by after ORDER BY\n1 | SELECT ?s WHERE { ?s ?p ?o } ORDER BY LIMIT 1\n                                          ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } LIMIT 99999999999999999999"
   s\" 1:36: expected an integer that fits 64 bits, found `99999999999999999999`; write at most 9223372036854775807\n1 | SELECT ?s WHERE { ?s ?p ?o } LIMIT 99999999999999999999\n                                       ^"
      REJECTS
   s" SELECT ?s WERE { ?s ?p ?o }"
   s\" 1:11: expected WHERE or {, found `WERE`; write the pattern as WHERE { ... }\n1 | SELECT ?s WERE { ?s ?p ?o }\n              ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } LIMIT 1 ORDER BY ?s"
   s\" 1:38: expected the end of the query, found `ORDER`; write GROUP BY, HAVING, ORDER BY, LIMIT and OFFSET in that order, each once\n1 | SELECT ?s WHERE { ?s ?p ?o } LIMIT 1 ORDER BY ?s\n                                         ^"
      REJECTS
   \ The forms this endpoint drops, each at its first token.
   s" CONSTRUCT { ?s <urn:p> ?o } WHERE { ?s <urn:p> ?o } LIMIT 10"
   s\" 1:1: expected SELECT or ASK, found `CONSTRUCT`; write SELECT ?s ?p ?o WHERE { ... } to read the triples\n1 | CONSTRUCT { ?s <urn:p> ?o } WHERE { ?s <urn:p> ?o } LIMIT 10\n    ^"
      REJECTS
   s" DESCRIBE <urn:one> WHERE { <urn:one> ?p ?o }"
   s\" 1:1: expected SELECT or ASK, found `DESCRIBE`; write SELECT ?p ?o WHERE { <iri> ?p ?o } to read what is said about <iri>\n1 | DESCRIBE <urn:one> WHERE { <urn:one> ?p ?o }\n    ^"
      REJECTS
   s" SELECT * FROM <urn:g> WHERE { ?s ?p ?o }"
   s\" 1:10: expected WHERE or {, found `FROM`; remove FROM: the base you query is the dataset\n1 | SELECT * FROM <urn:g> WHERE { ?s ?p ?o }\n             ^"
      REJECTS
   s" ASK FROM NAMED <urn:g> { ?s ?p ?o }"
   s\" 1:5: expected WHERE or {, found `FROM`; remove FROM: the base you query is the dataset\n1 | ASK FROM NAMED <urn:g> { ?s ?p ?o }\n        ^"
      REJECTS
   s" SELECT * WHERE { SERVICE <urn:s> { ?s ?p ?o } }"
   s\" 1:18: expected a graph pattern, found `SERVICE`; remove SERVICE: a query reads only its base\n1 | SELECT * WHERE { SERVICE <urn:s> { ?s ?p ?o } }\n                     ^"
      REJECTS
   s" ASK { SERVICE SILENT <urn:s> { ?s ?p ?o } }"
   s\" 1:7: expected a graph pattern, found `SERVICE`; remove SERVICE: a query reads only its base\n1 | ASK { SERVICE SILENT <urn:s> { ?s ?p ?o } }\n          ^"
      REJECTS
   s" SELECT ?b WHERE { ?s ?p ?o BIND(BNODE() AS ?b) }"
   s\" 1:33: expected a function this endpoint provides, found `BNODE`; bind a variable instead: a query cannot make blank nodes\n1 | SELECT ?b WHERE { ?s ?p ?o BIND(BNODE() AS ?b) }\n                                    ^"
      REJECTS
   s" SELECT ?h WHERE { ?s ?p ?o BIND(SHA1(STR(?o)) AS ?h) }"
   s\" 1:33: expected a function this endpoint provides, found `SHA1`; use SHA256\n1 | SELECT ?h WHERE { ?s ?p ?o BIND(SHA1(STR(?o)) AS ?h) }\n                                    ^"
      REJECTS
   \ At the `*` of a grouped query, not at SELECT.
   s" SELECT DISTINCT * WHERE { ?s ?p ?o } GROUP BY ?s"
   s\" 1:17: expected SELECT with named variables in a grouped query, found `*`; replace * with the variables to select\n1 | SELECT DISTINCT * WHERE { ?s ?p ?o } GROUP BY ?s\n                    ^"
      REJECTS
   \ From pattern.f.
   s\" SELECT * WHERE { GRAPH \"g\" { ?s ?p ?o } }"
   s\" 1:24: expected a variable, IRI or prefixed name, found `\"g\"`; replace it with a variable or an IRI\n1 | SELECT * WHERE { GRAPH \"g\" { ?s ?p ?o } }\n                           ^"
      REJECTS
   s" SELECT * WHERE { ?s }"
   s\" 1:21: expected a predicate: an IRI, prefixed name, variable or a, found `}`; write a predicate and an object after the subject\n1 | SELECT * WHERE { ?s }\n                        ^"
      REJECTS
   s" SELECT * WHERE { VALUES ?x { ?y } }"
   s\" 1:30: expected an IRI, literal or UNDEF, found `?y`; replace it with an IRI, literal or UNDEF\n1 | SELECT * WHERE { VALUES ?x { ?y } }\n                                 ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?x ?y) { (1) } }"
   s\" 1:35: expected a row with one value per variable, found `(`; write one value or UNDEF per variable in each row\n1 | SELECT * WHERE { VALUES (?x ?y) { (1) } }\n                                      ^"
      REJECTS
   \ A blank node label in two groups.
   s" SELECT * WHERE { { _:a <urn:p> ?o } { ?s <urn:q> _:a } }"
   s\" 1:50: expected a blank node label not used in another group, found `_:a`; rename the label\n1 | SELECT * WHERE { { _:a <urn:p> ?o } { ?s <urn:q> _:a } }\n                                                     ^"
      REJECTS
   \ A variable bound twice.
   s" SELECT * WHERE { ?s ?p ?w BIND(1 AS ?w) }"
   s\" 1:37: expected a variable not already in scope, found `?w`; use a new variable instead of ?w\n1 | SELECT * WHERE { ?s ?p ?w BIND(1 AS ?w) }\n                                        ^"
      REJECTS
   \ From expression.f.
   s" SELECT * WHERE { ?s ?p ?o FILTER(STRLEN(?o, ?s)) }"
   s\" 1:34: expected 1 argument, found `STRLEN`; call STRLEN with 1 argument\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STRLEN(?o, ?s)) }\n                                     ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(_:b) }"
   s\" 1:34: expected a variable, IRI, prefixed name or literal, found `_:b`; replace the blank node with a variable\n1 | SELECT * WHERE { ?s ?p ?o FILTER(_:b) }\n                                     ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(?a = ?b = ?c) }"
   s\" 1:42: expected one comparison per operand pair, found `=`; put the first comparison in parentheses\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?a = ?b = ?c) }\n                                             ^"
      REJECTS
   \ A * given to an IRI.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(<urn:f>(*)) }"
   s\" 1:34: expected arguments without * or SEPARATOR, found `<urn:f>`; remove the * or SEPARATOR\n1 | SELECT * WHERE { ?s ?p ?o FILTER(<urn:f>(*)) }\n                                     ^"
      REJECTS
   \ A SEPARATOR given to an IRI.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(<urn:f>(?o; SEPARATOR=\",\")) }"
   s\" 1:34: expected arguments without * or SEPARATOR, found `<urn:f>`; remove the * or SEPARATOR\n1 | SELECT * WHERE { ?s ?p ?o FILTER(<urn:f>(?o; SEPARATOR=\",\")) }\n                                     ^"
      REJECTS
   \ A * outside COUNT.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(STR(*)) }"
   s\" 1:34: expected COUNT, the only function that takes *, found `STR`; replace * with the arguments\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STR(*)) }\n                                     ^"
      REJECTS
   \ DISTINCT outside an aggregate.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(STR(DISTINCT ?o)) }"
   s\" 1:34: expected an aggregate before (DISTINCT, found `STR`; remove DISTINCT\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STR(DISTINCT ?o)) }\n                                     ^"
      REJECTS
   \ SEPARATOR outside GROUP_CONCAT.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(STR(?o; SEPARATOR=\",\")) }"
   s\" 1:34: expected GROUP_CONCAT, the only function that takes SEPARATOR, found `STR`; remove the SEPARATOR\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STR(?o; SEPARATOR=\",\")) }\n                                     ^"
      REJECTS
   \ BOUND of a literal.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(BOUND(1)) }"
   s\" 1:40: expected a variable, found `1`; pass BOUND a variable\n1 | SELECT * WHERE { ?s ?p ?o FILTER(BOUND(1)) }\n                                           ^"
      REJECTS
   \ A FILTER without parentheses.
   s\" SELECT * WHERE { ?s ?p ?o FILTER ?o }"
   s\" 1:34: expected a condition in parentheses or a function call, found `?o`; put the condition in parentheses\n1 | SELECT * WHERE { ?s ?p ?o FILTER ?o }\n                                     ^"
      REJECTS
   \ From terms.f.
   s" SELECT * WHERE { ?s ex:p ?o }"
   s\" 1:21: expected a declared prefix, found `ex:p`; declare it with PREFIX ex: <...>\n1 | SELECT * WHERE { ?s ex:p ?o }\n                        ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 }"
   s\" 1:41: expected ), found `}`; write ) here\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 }\n                                            ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o } LIMIT x"
   s\" 1:35: expected a non-negative integer, found `x`; write a non-negative integer here\n1 | SELECT * WHERE { ?s ?p ?o } LIMIT x\n                                      ^"
      REJECTS
   s" PREFIX ex:a <urn:x> SELECT * WHERE { ?s ?p ?o }"
   s\" 1:8: expected a prefix such as ex:, found `ex:a`; end the prefix name at its colon\n1 | PREFIX ex:a <urn:x> SELECT * WHERE { ?s ?p ?o }\n           ^"
      REJECTS
   \ A prefix declared twice.
   s\" PREFIX ex: <urn:x> PREFIX ex: <urn:y> ASK { }"
   s\" 1:27: expected a prefix not declared before, found `ex:`; remove this declaration\n1 | PREFIX ex: <urn:x> PREFIX ex: <urn:y> ASK { }\n                              ^"
      REJECTS
   s" BASE <p> SELECT * WHERE { ?s ?p ?o }"
   s\" 1:6: expected an absolute IRI with a scheme, found `<p>`; start the IRI with a scheme such as http:\n1 | BASE <p> SELECT * WHERE { ?s ?p ?o }\n         ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p + 1 }"
   s\" 1:24: expected a number after + or -, found `+`; write the number right after the sign\n1 | SELECT * WHERE { ?s ?p + 1 }\n                           ^"
      REJECTS
   \ From iri.f: a relative IRI with no BASE before it.
   s" SELECT * WHERE { ?s <p> ?o }"
   s\" 1:21: expected an absolute IRI, found `<p>`; write the full IRI or declare BASE <...>\n1 | SELECT * WHERE { ?s <p> ?o }\n                        ^"
      REJECTS ;

REJECTIONS

\ Appends n copies of the text.
: TIMES ( ^VEC:vector<u8> str n -- )
   {: out text:str count:n :}
   count 0 ?do text out VEC:APPEND loop ;

: NESTED ( ^ARENA:arena -- )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" ASK " out VEC:APPEND
   out s" { " 300 TIMES
   out s" } " 300 TIMES
   out 0 VEC:AT out VEC:COUNT >str arena PARSE-IN ;

: LONG-PREFIX ( ^ARENA:arena -- )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" SELECT * { ?s " out VEC:APPEND
   out s" p" 60000 TIMES
   s" :x ?o }" out VEC:APPEND
   out 0 VEC:AT out VEC:COUNT >str arena PARSE-IN ;

\ Binds a variable of a 60,000-byte name twice.
: LONG-FRESH ( ^ARENA:arena -- )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" SELECT * { ?s ?p ?" out VEC:APPEND
   out s" v" 60000 TIMES
   s"  BIND(1 AS ?" out VEC:APPEND
   out s" v" 60000 TIMES
   s" ) }" out VEC:APPEND
   out VEC:BYTES arena PARSE-IN ;

\ Selects an ungrouped variable of a 60,000-byte name.
: LONG-GROUPED ( ^ARENA:arena -- )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" SELECT ?" out VEC:APPEND
   out s" v" 60000 TIMES
   s"  { ?s ?p ?o } GROUP BY ?s" out VEC:APPEND
   out VEC:BYTES arena PARSE-IN ;

\ The last rejection's text is under 1,000 bytes.
: SHORT-TEXT ( -- )
   E-QUERY REASON:TEXT$ str> nip 1000 < TTRUE ;

: CHAINS ( ^ARENA:arena -- )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" SELECT * WHERE { ?s <urn:p>" out VEC:APPEND
   out s" |<urn:p>" 9999 TIMES
   s"  ?o . ?s !(<urn:p>" out VEC:APPEND
   out s" |^<urn:p>" 9999 TIMES
   s" ) ?o FILTER(?s" out VEC:APPEND
   out s"  && ?o" 9999 TIMES
   s" ) }" out VEC:APPEND
   out 0 VEC:AT out VEC:COUNT >str arena PARSE-IN ;

\ Appends ` ?` and the index in three letters.
: DEEP-VAR ( ^VEC:vector<u8> n -- )
   {: out index:n :}
   s"  ?" out VEC:APPEND
   index 676 / 97 + out VEC:PUSH c!
   index 26 / 26 mod 97 + out VEC:PUSH c!
   index 26 mod 97 + out VEC:PUSH c! ;

\ Parses 250 nested groups around 4,000 triples over 12,000 variables.
: DEEP ( ^ARENA:arena -- ^syntax )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" SELECT *" out VEC:APPEND
   out s" {" 250 TIMES
   12000 0 ?do
      out i DEEP-VAR
      i 3 mod 2 = if s" ." out VEC:APPEND then
   loop
   out s" }" 250 TIMES
   out 0 VEC:AT out VEC:COUNT >str arena PARSE ;

\ Rounded up.
: NODES-PER-TOKEN ( ^syntax -- n )
   {: state :}
   state SYNTAX-TOKENS @ VEC:COUNT {: tokens:n :}
   state SYNTAX-NODES @ VEC:COUNT tokens 1- + tokens / ;

25000000 constant RESOLVED-MOST-BYTES

\ 2,100 relative references against a base of 4,000 segments.
: WIDE-BASE ( ^VEC:vector<u8> -- )
   {: out :}
   s" BASE <http://h" out VEC:APPEND
   out s" /a" 4000 TIMES
   s" /> ASK{" out VEC:APPEND
   out s" <a> <a> <a>." 700 TIMES
   s" }" out VEC:APPEND ;

\ Arena bytes the parse takes. The first block makes a chunk of
\ RESOLVED-MOST-BYTES follow; a parse that leaves that chunk ends below
\ its start or past its end.
: RESOLVED-BYTES ( ^ARENA:arena -- n )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   out WIDE-BASE
   arena RESOLVED-MOST-BYTES 2 / ARENA:ALLOC 2drop
   arena 8 ARENA:ALLOC drop {: from :}
   out VEC:BYTES arena PARSE-IN
   arena 8 ARENA:ALLOC drop from - ;

: LIMITS ( -- )
   s" flat chains of 10,000 alternatives and terms parse" T-LABEL
   [: CHAINS ;] ARENA:WITH
   s" 300 nested groups are rejected at the 257th" T-LABEL
   [: [: NESTED ;] ARENA:WITH ;] E-QUERY TTHROWSQ
   E-QUERY REASON:TEXT$
   s\" 1:517: expected at most 256 nested groups or expressions, found `{`; nest fewer groups or expressions\n1 | ...{ { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { { ...\n                                               ^"
      T$=
   s" a 60,000-byte prefix is rejected in under 1,000 bytes" T-LABEL
   [: [: LONG-PREFIX ;] ARENA:WITH ;] E-QUERY TTHROWSQ
   SHORT-TEXT
   s" and so is a 60,000-byte variable bound twice" T-LABEL
   [: [: LONG-FRESH ;] ARENA:WITH ;] E-QUERY TTHROWSQ
   SHORT-TEXT
   s" and an ungrouped one" T-LABEL
   [: [: LONG-GROUPED ;] ARENA:WITH ;] E-QUERY TTHROWSQ
   SHORT-TEXT
   s" 12,000 variables under 250 groups make at most 10 nodes per token" T-LABEL
   [: DEEP NODES-PER-TOKEN ;] ARENA:WITH 10 <= TTRUE
   s" 2,100 references on an 8 KB base take under 25 MB" T-LABEL
   [: RESOLVED-BYTES ;] ARENA:WITH
      dup 0 > swap RESOLVED-MOST-BYTES < and TTRUE ;

LIMITS
T-REPORT

;package
