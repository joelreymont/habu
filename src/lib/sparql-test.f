\ sparql-test.f - SPARQL:COMPILE against a live PostgreSQL server.
\ test/db/pg-cluster.f starts a private cluster and runs this file with the
\ cluster's conninfo as its one script argument (`bin/hb --load
\ test/db/pg-cluster.f -- src/lib/sparql-test.f`). The file loads
\ src/lib/sparql/runtime.sql and test/sparql/host.sql. A case compiles a
\ query on the description sparql_test.store answers, runs the statement
\ with its parameter and compares the document with the expected one as
\ JSON, the bindings as a multiset or, for ORDERED, in order; an expected
\ document writes `'` for `"`.
\ Loaded without the argument, the file dies naming the harness.

require src/lib/test/assert.f
require src/lib/fmt.f
require src/lib/string.f
require src/lib/json.f
require src/lib/sparql.f
require test/sparql/server.f
require src/lib/vector.f
require src/lib/json/value.f
require src/lib/json/equal.f

package SPARQL-TEST
private

\ The text the statement answers for the answer limit bytes, in decimal.
: DESCRIBED ( str str ^ARENA:arena -- ^VEC:vector<u8> )
   {: sql:str bytes:str a:^ :}
   a TEXT-VEC {: out:^ :}
   sql bytes out RUN-TEXT TTRUE out ;

: STORE ( str ^ARENA:arena -- ^VEC:vector<u8> )
   {: bytes:str a:^ :}
   s" select sparql_test.store($1::bigint)::text" bytes a DESCRIBED ;

\ The store's description with its two node sources alone.
: NODE-STORE ( ^ARENA:arena -- ^VEC:vector<u8> )
   {: a:^ :}
   s" select jsonb_set(s,'{sources}',jsonb_path_query_array(s,'$.sources[0 to 1]'))::text from sparql_test.store($1::bigint) s"
   s" 1048576" a DESCRIBED ;

\ The store's description with its value source alone.
: VALUE-STORE ( ^ARENA:arena -- ^VEC:vector<u8> )
   {: a:^ :}
   s" select jsonb_set(s,'{sources}',jsonb_path_query_array(s,'$.sources[3]'))::text from sparql_test.store($1::bigint) s"
   s" 1048576" a DESCRIBED ;

: PREFIXES ( -- str )
   s" PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX t: <urn:test:> " ;

: PREFIXED ( str ^ARENA:arena -- ^VEC:vector<u8> )
   {: q:str a:^ :}
   a TEXT-VEC {: out:^ :}
   PREFIXES out VEC:APPEND q out VEC:APPEND out ;

: COMPILED ( str ^VEC:vector<u8> ^ARENA:arena -- ^VEC:vector<u8> bool )
   {: q:str store:^ a:^ :}
   a TEXT-VEC {: sql:^ :}
   sql q store $ sql COMPILE ;

\ The expected document with each `'` read as `"`.
: UNQUOTE ( str ^ARENA:arena -- ^VEC:vector<u8> )
   {: w:str a:^ :}
   a TEXT-VEC {: out:^ :}
   w STR:LEN 0 ?do
      w STR:DATA i + c@ dup [char] ' = if drop [char] " then
      out VEC:PUSH c!
   loop out ;

STRUCTURE row DERIVE addr FIELD text str ;STRUCTURE

: ADD-ROW ( ^VEC:vector<row> JSON:value -- ^VEC:vector<row> )
   {: rows:^ v :}
   v JSON:TEXT$ rows VEC:PUSH ROW-TEXT ! rows ;

: ROWS ( JSON:value ^ARENA:arena -- ^VEC:vector<row> )
   {: doc a:^ :}
   a ARENA:ALLOCATOR 0 [type] row VEC:NEW
   doc s" results" JSON:GET s" bindings" JSON:GET
   [: ADD-ROW ;] JSON:EACH ;

: TALLY ( ^VEC:vector<row> str -- n )
   {: rows:^ t:str :}
   0 rows VEC:COUNT 0 ?do
      rows i VEC:AT ROW-TEXT @ t JSON:VALUE= if 1+ then
   loop ;

: SAME-ROWS? ( ^VEC:vector<row> ^VEC:vector<row> -- bool )
   {: want:^ got:^ :}
   want VEC:COUNT got VEC:COUNT <> if false exit then
   want VEC:COUNT 0 ?do
      want i VEC:AT ROW-TEXT @ {: t:str :}
      want t TALLY got t TALLY <> if false unloop exit then
   loop true ;

: SAME-ORDER? ( ^VEC:vector<row> ^VEC:vector<row> -- bool )
   {: want:^ got:^ :}
   want VEC:COUNT got VEC:COUNT <> if false exit then
   want VEC:COUNT 0 ?do
      want i VEC:AT ROW-TEXT @ got i VEC:AT ROW-TEXT @ JSON:VALUE=
      0= if false unloop exit then
   loop true ;

: MEMBER= ( JSON:value JSON:value str -- bool )
   {: got want name:str :}
   got name JSON:GET JSON:TEXT$
   want name JSON:GET JSON:TEXT$ JSON:VALUE= ;

: SAME? ( str str ^ARENA:arena bool -- bool )
   {: g:str w:str a:^ ordered:bool :}
   g s" {" STR:STARTS-WITH? 0= if false exit then
   g JSON:PARSE {: got :} w JSON:PARSE {: want :}
   got want s" head" MEMBER= 0= if false exit then
   want s" boolean" HAS? {: ask:bool :}
   got s" boolean" HAS? ask xor if false exit then
   ask if got want s" boolean" MEMBER= exit then
   ordered if want a ROWS got a ROWS SAME-ORDER? exit then
   want a ROWS got a ROWS SAME-ROWS? ;

\ Labels the case with the query, cut to what a label holds.
: LABEL ( str -- )
   {: q:str :} q STR:DATA q STR:LEN T-LABEL-CAP min >str T-LABEL ;

\ The query's statement and document on the store; the document is the
\ failure when the statement fails.
: ANSWER-ON ( str ^VEC:vector<u8> ^ARENA:arena -- ^VEC:vector<u8> ^VEC:vector<u8> )
   {: q:str store:^ a:^ :}
   q LABEL
   q a PREFIXED $ store a COMPILED {: sql:^ ok:bool :}
   ok TTRUE a TEXT-VEC {: doc:^ :}
   ok if sql $ doc RUN drop then
   sql doc ;

: ANSWER ( str str ^ARENA:arena -- ^VEC:vector<u8> ^VEC:vector<u8> )
   {: q:str bytes:str a:^ :} q bytes a STORE a ANSWER-ON ;

: EXPECT-IN ( str str ^ARENA:arena bool -- )
   {: q:str w:str a:^ ordered:bool :}
   q s" 1048576" a ANSWER {: sql:^ doc:^ :}
   doc $ w a UNQUOTE $ a ordered SAME? dup TTRUE
   0= if doc $ type cr then ;

\ The query answers the expected document.
: EXPECT ( str str -- ) [: false EXPECT-IN ;] ARENA:WITH ;

\ The query answers the expected document, its bindings in order.
: ORDERED ( str str -- ) [: true EXPECT-IN ;] ARENA:WITH ;

: WITHOUT-IN ( str str str ^ARENA:arena -- )
   {: q:str gone:str w:str a:^ :}
   q s" 1048576" a ANSWER {: sql:^ doc:^ :}
   sql $ gone STR:CONTAINS? TFALSE
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ The query answers the expected document and its statement never names
\ gone.
: WITHOUT ( str str str -- ) [: WITHOUT-IN ;] ARENA:WITH ;

\ The query answers no rows and its statement never names gone.
: DEAD ( str str -- )
   s" {'head':{'vars':['l']},'results':{'bindings':[]}}" WITHOUT ;

: KEYED-IN ( str str ^ARENA:arena -- )
   {: q:str w:str a:^ :}
   q s" 1048576" a ANSWER {: sql:^ doc:^ :}
   sql $ s" = ANY(E'" STR:CONTAINS? TTRUE
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ The query answers the expected document, matching a key column.
: KEYED ( str str -- ) [: KEYED-IN ;] ARENA:WITH ;

: TIMES-ON ( str str n str ^VEC:vector<u8> ^ARENA:arena -- )
   {: q:str t:str n:n w:str store:^ a:^ :}
   q store a ANSWER-ON {: sql:^ doc:^ :}
   sql $ t STR:COUNT-SUB n T=
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

: TIMES-IN ( str str n str ^ARENA:arena -- )
   {: a:^ :} s" 1048576" a STORE a TIMES-ON ;

\ The query answers the expected document and its statement names t n
\ times.
: TIMES ( str str n str -- ) [: TIMES-IN ;] ARENA:WITH ;

: ON-NODES-IN ( str str n str ^ARENA:arena -- )
   {: a:^ :} a NODE-STORE a TIMES-ON ;

\ On the node sources alone, the query answers the expected document and
\ its statement names t n times.
: ON-NODES ( str str n str -- ) [: ON-NODES-IN ;] ARENA:WITH ;

: ON-VALUES-IN ( str str n str ^ARENA:arena -- )
   {: a:^ :} a VALUE-STORE a TIMES-ON ;

\ On the value source alone, the query answers the expected document and
\ its statement names t n times.
: ON-VALUES ( str str n str -- ) [: ON-VALUES-IN ;] ARENA:WITH ;

\ On the node sources alone, the query answers the expected document and
\ its statement names their table n times.
: NODES ( str n str -- )
   {: q:str n:n w:str :} q s" sparql_test.node" n w ON-NODES ;

\ The query answers the expected document and its statement names t once.
: ONCE ( str str str -- )
   {: q:str t:str w:str :} q t 1 w TIMES ;

: HOLDS-IN ( str str str ^ARENA:arena -- )
   {: q:str t:str w:str a:^ :}
   q s" 1048576" a ANSWER {: sql:^ doc:^ :}
   sql $ t STR:CONTAINS? TTRUE
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ The query answers the expected document and its statement names t.
: HOLDS ( str str str -- ) [: HOLDS-IN ;] ARENA:WITH ;

: HOISTED-IN ( str str ^ARENA:arena -- )
   {: q:str w:str a:^ :}
   q s" 1048576" a ANSWER {: sql:^ doc:^ :}
   sql $ s" WITH " STR:STARTS-WITH? TTRUE
   sql $ s"  AS MATERIALIZED (SELECT DISTINCT " STR:CONTAINS? TTRUE
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ The query answers the expected document and its statement opens with
\ the materialized key CTEs.
: HOISTED ( str str -- ) [: HOISTED-IN ;] ARENA:WITH ;

: ALIKE-IN ( str str str ^ARENA:arena -- )
   {: q:str r:str w:str a:^ :}
   q s" 1048576" a ANSWER {: qsql:^ qdoc:^ :}
   r s" 1048576" a ANSWER {: rsql:^ rdoc:^ :}
   rsql $ qsql $ T$=
   rdoc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ Queries q and r compile to one statement, which answers the expected
\ document.
: ALIKE ( str str str -- ) [: ALIKE-IN ;] ARENA:WITH ;

: ALONE-IN ( str str str str ^ARENA:arena -- )
   {: q:str keep:str gone:str w:str a:^ :}
   q s" 1048576" a ANSWER {: sql:^ doc:^ :}
   sql $ keep STR:CONTAINS? TTRUE
   sql $ gone STR:CONTAINS? TFALSE
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ The query answers the expected document and its statement names keep
\ but never gone.
: ALONE ( str str str str -- ) [: ALONE-IN ;] ARENA:WITH ;

: NEITHER-IN ( str str str str ^ARENA:arena -- )
   {: q:str x:str y:str w:str a:^ :}
   q s" 1048576" a ANSWER {: sql:^ doc:^ :}
   sql $ x STR:CONTAINS? TFALSE
   sql $ y STR:CONTAINS? TFALSE
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ The query answers the expected document and its statement names neither
\ x nor y.
: NEITHER ( str str str str -- ) [: NEITHER-IN ;] ARENA:WITH ;

: BOTH-IN ( str str str str ^ARENA:arena -- )
   {: q:str x:str y:str w:str a:^ :}
   q s" 1048576" a ANSWER {: sql:^ doc:^ :}
   sql $ x STR:CONTAINS? TTRUE
   sql $ y STR:CONTAINS? TTRUE
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ The query answers the expected document and its statement names both x
\ and y.
: BOTH ( str str str str -- ) [: BOTH-IN ;] ARENA:WITH ;

\ A part before its parent, a list of datatypes one with an escape, a
\ shape of any term.
: SPELLED-SHAPES ( -- str )
   s" 'shapes':{'class':{'within':'id'},'id':{'in':'sparql_test.id','out':'sparql_test.id_term','holds':'uri'},'text':{'in':'sparql_test.string','out':'sparql_test.string_term','holds':['urn:none','http://www.w3.org/2001/XMLSchema#string']},'number':{'in':'sparql_test.number','out':'sparql_test.number_term'}}" ;

\ The second of two key sets.
: SPELLED-KEYS ( -- str )
   s" 'keys':{'none':{'type':'bigint[]','values':{}},'facts':{'type':'bigint[]','values':{'urn:test:70':'{700,701}'}}}" ;

\ A FROM with an escape; its subject column is never NULL.
: SPELLED-LABELS ( -- str )
   s" {'from':'\'sparql_test\'.node','graph':null,'s':{'col':'id','shape':'id','notnull':true},'p':{'term':{'type':'uri','value':'http://www.w3.org/2000/01/rdf-schema#label'}},'o':{'col':'label','shape':'text'}}" ;

: SPELLED-FACTS ( -- str )
   s" {'from':'sparql_test.fact','graph':null,'s':{'col':'s','shape':'id'},'p':{'col':'p','shape':'id','key':{'col':'pid','set':'facts'}},'o':{'col':'o','shape':'id'}}" ;

\ Two columns of a subject and two of an object.
: SPELLED-PAIRS ( -- str )
   s" {'from':'(SELECT * FROM (VALUES (1::bigint,NULL::bigint,2::bigint,NULL::numeric),(1,NULL,NULL,5),(NULL,10,2,NULL),(NULL,10,NULL,5)) t(sa,sb,oa,ob))','graph':null,'s':{'cols':[{'col':'sa','shape':'id'},{'col':'sb','shape':'class'}]},'p':{'term':{'type':'uri','value':'urn:test:90'}},'o':{'cols':[{'col':'oa','shape':'id'},{'col':'ob','shape':'number'}]}}" ;

\ A constant literal, its members in another order and one with an escape.
: SPELLED-CONSTANT ( -- str )
   s" {'from':'(SELECT 1::bigint AS id)','graph':null,'s':{'col':'id','shape':'id'},'p':{'term':{'type':'uri','value':'urn:test:91'}},'o':{'term':{'datatype':'http://www.w3.org/2001/XMLSchema#integer','value':'\u0035','type':'literal'}}}" ;

\ A named graph and a constant literal holding `'` and `\`.
: SPELLED-QUOTED ( -- str )
   s" {'from':'(SELECT 1::bigint AS id)','graph':{'type':'uri','value':'urn:test:o\u0027k'},'s':{'col':'id','shape':'id'},'p':{'term':{'type':'uri','value':'urn:test:92'}},'o':{'term':{'type':'literal','value':'it\u0027s \\ here'}}}" ;

\ A constant literal that spells a call of RAND's SQL.
: SPELLED-CALL ( -- str )
   s" {'from':'(SELECT 1::bigint AS id)','graph':null,'s':{'col':'id','shape':'id'},'p':{'term':{'type':'uri','value':'urn:test:93'}},'o':{'term':{'type':'literal','value':'sparql.rand('}}}" ;

: SPELLED-STORE ( ^ARENA:arena -- ^VEC:vector<u8> )
   {: a:^ :}
   a TEXT-VEC {: out:^ :}
   s" {'bytes':1048576," out VEC:APPEND
   SPELLED-SHAPES out VEC:APPEND s" ," out VEC:APPEND
   SPELLED-KEYS out VEC:APPEND s" ,'sources':[" out VEC:APPEND
   SPELLED-LABELS out VEC:APPEND s" ," out VEC:APPEND
   SPELLED-FACTS out VEC:APPEND s" ," out VEC:APPEND
   SPELLED-PAIRS out VEC:APPEND s" ," out VEC:APPEND
   SPELLED-CONSTANT out VEC:APPEND s" ," out VEC:APPEND
   SPELLED-QUOTED out VEC:APPEND s" ," out VEC:APPEND
   SPELLED-CALL out VEC:APPEND s" ]}" out VEC:APPEND
   out $ a UNQUOTE ;

: SPELLED-IN ( str str ^ARENA:arena -- )
   {: q:str w:str a:^ :}
   q a SPELLED-STORE a ANSWER-ON {: sql:^ doc:^ :}
   doc $ w a UNQUOTE $ a false SAME? dup TTRUE
   0= if doc $ type cr then ;

\ On SPELLED-STORE the query answers the expected document.
: SPELLED ( str str -- ) [: SPELLED-IN ;] ARENA:WITH ;

: SPELLED-ALONE-IN ( str str str str ^ARENA:arena -- )
   {: q:str keep:str gone:str w:str a:^ :}
   q a SPELLED-STORE a ANSWER-ON {: sql:^ doc:^ :}
   sql $ keep STR:CONTAINS? TTRUE
   sql $ gone STR:CONTAINS? TFALSE
   doc $ w a UNQUOTE $ a false SAME? TTRUE ;

\ On SPELLED-STORE the query answers the expected document and its
\ statement names keep but never gone.
: SPELLED-ALONE ( str str str str -- ) [: SPELLED-ALONE-IN ;] ARENA:WITH ;

\ The query's document on a limit of so many bytes.
: ANSWER-WITHIN ( str n ^ARENA:arena -- ^VEC:vector<u8> )
   {: q:str bytes:n a:^ :}
   a TEXT-VEC {: text:^ :}
   bytes FMT:INT$ text VEC:APPEND
   q text $ a ANSWER {: sql:^ doc:^ :}
   doc ;

: LIMITED-IN ( str ^ARENA:arena -- )
   {: q:str a:^ :}
   q 1048576 a ANSWER-WITHIN {: doc:^ :}
   doc VEC:COUNT {: len:n :}
   q len a ANSWER-WITHIN $ doc $ a false SAME? TTRUE
   q len 1- a ANSWER-WITHIN $ s" 54000: " STR:STARTS-WITH? TTRUE ;

\ The query answers on a limit of its document's length and fails on one a
\ byte shorter.
: LIMITED ( str -- ) [: LIMITED-IN ;] ARENA:WITH ;

: FAIL-IN ( str str ^ARENA:arena -- )
   {: q:str text:str a:^ :}
   q 1048576 a ANSWER-WITHIN $ text STR:STARTS-WITH? TTRUE ;

\ The query's statement fails with a text that starts so.
: FAILS ( str str -- ) [: FAIL-IN ;] ARENA:WITH ;

: REJECT-IN ( str str ^ARENA:arena -- )
   {: q:str line:str a:^ :}
   q T-LABEL
   q s" 1048576" a STORE a COMPILED {: sql:^ ok:bool :}
   ok TFALSE sql $ line STR:STARTS-WITH? TTRUE ;

\ The query is rejected with a text whose first line is line.
: REJECTS ( str str -- ) [: REJECT-IN ;] ARENA:WITH ;

\ The forms of a description that host.sql does not write.
: SPELLINGS ( -- )
   s" SELECT ?s WHERE { ?s rdfs:label 'Lathe' }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}}]}}"
   SPELLED
   s" SELECT ?s ?o WHERE { ?s t:70 ?o }"
   s" {'head':{'vars':['s','o']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:40'},'o':{'type':'uri','value':'urn:test:41'}},{'s':{'type':'uri','value':'urn:test:42'},'o':{'type':'uri','value':'urn:test:43'}}]}}"
   SPELLED
   s" SELECT ?s ?o WHERE { ?s t:90 ?o }"
   s" {'head':{'vars':['s','o']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'o':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:1'},'o':{'type':'literal','value':'5','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'s':{'type':'uri','value':'urn:test:10'},'o':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:10'},'o':{'type':'literal','value':'5','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   SPELLED
   s" SELECT ?s WHERE { ?s t:90 5 }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:10'}}]}}"
   SPELLED
   s" SELECT ?o WHERE { t:1 t:91 ?o }"
   s" {'head':{'vars':['o']},'results':{'bindings':[{'o':{'type':'literal','value':'5','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   SPELLED
   s" SELECT ?s WHERE { ?s t:91 5 }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}}]}}"
   SPELLED
   s" SELECT ?g ?o WHERE { GRAPH ?g { t:1 t:92 ?o } }"
   s" {'head':{'vars':['g','o']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:o\u0027k'},'o':{'type':'literal','value':'it\u0027s \\ here'}}]}}"
   SPELLED
   \ A variable needs no test that a column never NULL holds a value.
   s" SELECT ?s ?l WHERE { ?s rdfs:label ?l FILTER(?l = 'Lathe') }"
   s" .label IS NOT NULL" s" .id IS NOT NULL"
   s" {'head':{'vars':['s','l']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'l':{'type':'literal','value':'Lathe'}}]}}"
   SPELLED-ALONE ;

: SLOTS ( -- )
   s" SELECT ?l WHERE { t:1 rdfs:label ?l }"
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s a t:11 }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'}},{'s':{'type':'uri','value':'urn:test:4'}},{'s':{'type':'uri','value':'urn:test:5'}}]}}"
   EXPECT
   s" SELECT ?s ?o WHERE { ?s t:20 ?o }"
   s" {'head':{'vars':['s','o']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'},'o':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:4'},'o':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:5'},'o':{'type':'uri','value':'urn:test:1'}}]}}"
   EXPECT ;

\ A source joins only where its columns' shapes hold the pattern's
\ constants and meet the other patterns' shapes: a part its root, never
\ another part of it.
: SHAPES ( -- )
   s" SELECT ?s WHERE { ?s ?p 12 }"
   s" sparql_test.node"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'}}]}}"
   WITHOUT
   s" SELECT ?l WHERE { ?s a ?l . ?l t:20 ?o }"
   s" sparql_test.node" DEAD
   s" SELECT ?s ?l WHERE { ?s t:20 t:1 . ?s rdfs:label ?l }"
   s" {'head':{'vars':['s','l']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'},'l':{'type':'literal','value':'Spindle'}}]}}"
   EXPECT ;

\ t:1's triples as bindings of ?p and ?o.
: LATHE ( -- str )
   s" {'head':{'vars':['p','o']},'results':{'bindings':[{'p':{'type':'uri','value':'http://www.w3.org/2000/01/rdf-schema#label'},'o':{'type':'literal','value':'Lathe'}},{'p':{'type':'uri','value':'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'},'o':{'type':'uri','value':'urn:test:10'}},{'p':{'type':'uri','value':'urn:test:21'},'o':{'type':'uri','value':'urn:test:2'}},{'p':{'type':'uri','value':'urn:test:30'},'o':{'type':'literal','value':'1200','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'p':{'type':'uri','value':'urn:test:31'},'o':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}" ;

\ A variable predicate reads every default-graph source.
: ALL-SOURCES ( -- )
   s" SELECT ?p ?o WHERE { t:1 ?p ?o }" LATHE EXPECT ;

\ A listed predicate matches through its key column; an unlisted one
\ reads no branch of the keyed source.
: KEY-SETS ( -- )
   s" SELECT ?s ?o WHERE { ?s t:70 ?o }"
   s" {'head':{'vars':['s','o']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:40'},'o':{'type':'uri','value':'urn:test:41'}},{'s':{'type':'uri','value':'urn:test:42'},'o':{'type':'uri','value':'urn:test:43'}}]}}"
   KEYED
   s" SELECT ?l WHERE { ?s t:72 ?l }"
   s" sparql_test.fact" DEAD ;

\ A slot over typed columns reads its table once; a constant or a join
\ reads only the columns that can hold it, any one of several.
: TYPED-COLUMNS ( -- )
   s" SELECT ?s ?o WHERE { ?s t:80 ?o }"
   s" sparql_test.typed"
   s" {'head':{'vars':['s','o']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:40'},'o':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:41'},'o':{'type':'literal','value':'a'}},{'s':{'type':'uri','value':'urn:test:42'},'o':{'type':'literal','value':'0.25','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}},{'s':{'type':'uri','value':'urn:test:43'},'o':{'type':'uri','value':'urn:test:10'}}]}}"
   ONCE
   s" SELECT ?s WHERE { ?s t:80 'a' }"
   s" o_text = sparql_test.string(" s"  OR "
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:41'}}]}}"
   ALONE
   s" SELECT ?x ?o WHERE { ?s t:80 ?o . ?x t:20 ?o }"
   s" o_kind" s" o_text"
   s" {'head':{'vars':['x','o']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:3'},'o':{'type':'uri','value':'urn:test:1'}},{'x':{'type':'uri','value':'urn:test:5'},'o':{'type':'uri','value':'urn:test:1'}}]}}"
   NEITHER
   s" SELECT ?s WHERE { ?s t:80 t:10 }"
   s" u.o_id = sparql_test.id($2)) OR (u.o_kind =" s" o_text"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:43'}}]}}"
   ALONE
   s" SELECT ?o ?l WHERE { ?s t:80 ?o . ?o rdfs:label ?l }"
   s" COALESCE(u.o_id,u.o_kind)" s" o_text"
   s" {'head':{'vars':['o','l']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:1'},'l':{'type':'literal','value':'Lathe'}},{'o':{'type':'uri','value':'urn:test:10'},'l':{'type':'literal','value':'Machine'}}]}}"
   ALONE ;

\ A keyed union reads its natively bound branches once per key, the
\ others beside them; a key that drives no union has no CTE.
: DRIVEN ( -- )
   s" SELECT ?p ?o WHERE { ?s rdfs:label 'Lathe' . ?s ?p ?o }"
   s"  CROSS JOIN LATERAL (" s"  IN (SELECT " LATHE ALONE
   s" SELECT ?p ?o WHERE { ?s rdfs:label 'Lathe' . ?s ?p ?o }"
   LATHE HOISTED
   s" SELECT ?p ?o WHERE { ?s ?p ?o . ?s rdfs:label 'Lathe' }"
   s" SELECT ?p ?o WHERE { ?s rdfs:label 'Lathe' . ?s ?p ?o }"
   LATHE ALIKE
   s" SELECT ?p ?o WHERE { ?s rdfs:label 'Lathe' { ?s ?p ?o } }"
   s" SELECT ?p ?o WHERE { ?s rdfs:label 'Lathe' . ?s ?p ?o }"
   LATHE ALIKE
   s" SELECT ?p ?o WHERE { VALUES ?p { rdfs:label <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> t:21 t:30 t:31 } t:1 ?p ?o }"
   s"  LATERAL " LATHE WITHOUT
   s" SELECT ?p ?o WHERE { VALUES ?s { t:1 } ?s ?p ?o }"
   s" = sparql_test.id(" s" .k)" LATHE BOTH
   s" SELECT ?s ?o WHERE { VALUES ?p { t:21 } ?s ?p ?o }"
   s" .* FROM " s"  CROSS JOIN LATERAL ("
   s" {'head':{'vars':['s','o']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'o':{'type':'uri','value':'urn:test:2'}}]}}"
   BOTH
   s" SELECT ?p WHERE { ?s rdfs:label 'Lathe' . ?o rdfs:label 'Mill' . ?s ?p ?o }"
   s"  AS MATERIALIZED (" 1
   s" {'head':{'vars':['p']},'results':{'bindings':[{'p':{'type':'uri','value':'urn:test:21'}}]}}"
   TIMES
   s" SELECT ?p ?o WHERE { ?x rdfs:label 'Spindle' . ?x t:20 ?s . ?s ?p ?o . ?y rdfs:label 'Mill' }"
   s" .label = sparql_test.string($1)" 2 LATHE TIMES
   s" SELECT ?p ?o WHERE { ?x rdfs:label 'Spindle' . ?x t:20 ?s . ?s ?p ?o . ?y rdfs:label 'Mill' }"
   s" .label = sparql_test.string($3)" LATHE ONCE
   s" SELECT (COUNT(*) AS ?n) WHERE { ?s rdfs:label 'Lathe' ; a t:10 ; t:31 2 . ?x rdfs:label 'Mill' . ?s ?p ?o . ?s ?q ?r . ?x ?t ?u . ?x ?v ?w }"
   s"  AS MATERIALIZED (" 2
   s" {'head':{'vars':['n']},'results':{'bindings':[{'n':{'type':'literal','value':'225','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   TIMES
   s" SELECT (COUNT(*) AS ?n) WHERE { ?x rdfs:label 'Spindle' . ?x t:20 ?s . ?s ?p ?o . ?x ?q ?r }"
   s" .label = sparql_test.string($1)" 2
   s" {'head':{'vars':['n']},'results':{'bindings':[{'n':{'type':'literal','value':'20','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   TIMES
   s" SELECT ?x WHERE { ?x rdfs:label 'Spindle' FILTER EXISTS { ?x t:20 ?s . ?s ?p ?o } }"
   s"  LATERAL "
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:3'}}]}}"
   WITHOUT ;

: JOINS ( -- )
   s" SELECT ?l WHERE { ?s rdfs:label ?l . ?s a t:10 }"
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}},{'l':{'type':'literal','value':'Mill'}}]}}"
   EXPECT
   s" SELECT ?l WHERE { ?s rdfs:label ?l . ?l a ?k }"
   s" label" DEAD
   s" SELECT ?l WHERE { ?l a 'Machine' }"
   s" {'head':{'vars':['l']},'results':{'bindings':[]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s ?p ?s }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:4'}}]}}"
   EXPECT
   \ A chain joins from its given end, whatever the written order.
   s" SELECT ?c WHERE { ?b t:25 ?c . ?a t:25 ?b . t:50 t:25 ?a }"
   s"  WHERE t1.s = sparql_test.id($1) AND t1.p = sparql_test.id($2) AND t1.o IS NOT NULL AND t2.s IS NOT NULL AND t2.p = sparql_test.id($2) AND t2.o IS NOT NULL AND t1.o = t2.s AND "
   s" {'head':{'vars':['c']},'results':{'bindings':[{'c':{'type':'uri','value':'urn:test:50'}}]}}"
   HOLDS
   s" SELECT ?l ?w WHERE { ?s rdfs:label ?l { ?s t:30 ?w FILTER(?w < 100) } }"
   s" {'head':{'vars':['l','w']},'results':{'bindings':[{'l':{'type':'literal','value':'Spindle'},'w':{'type':'literal','value':'12','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'l':{'type':'literal','value':'Bolt'},'w':{'type':'literal','value':'0.25','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}}]}}"
   EXPECT ;

\ Patterns of one subject read its row once where the source's subject
\ column is unique, while the FROM holds that row.
: SUBJECT-ROWS ( -- )
   s" SELECT ?s ?l WHERE { ?s rdfs:label ?l ; a t:11 }" 1
   s" {'head':{'vars':['s','l']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'},'l':{'type':'literal','value':'Spindle'}},{'s':{'type':'uri','value':'urn:test:4'},'l':{'type':'literal','value':'Bolt'}}]}}"
   NODES
   s" SELECT ?l ?k WHERE { t:1 rdfs:label ?l ; a ?k }" 1
   s" {'head':{'vars':['l','k']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'},'k':{'type':'uri','value':'urn:test:10'}}]}}"
   NODES
   s" SELECT ?s WHERE { ?s rdfs:label 'Lathe', 'Mill' }" 1
   s" {'head':{'vars':['s']},'results':{'bindings':[]}}"
   NODES
   s" SELECT ?s ?x WHERE { ?s rdfs:label 'Lathe' ; a ?k . ?x a ?k ; rdfs:label 'Mill' }" 2
   s" {'head':{'vars':['s','x']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'x':{'type':'uri','value':'urn:test:2'}}]}}"
   NODES
   \ A subject of two rows: edge's column is not unique.
   s" SELECT ?s WHERE { ?s t:20 t:2 ; t:22 t:4 }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:4'}}]}}"
   EXPECT
   s" ASK { t:1 a ?k . t:2 a ?k }" 2 s" {'head':{},'boolean':true}" NODES
   s" SELECT ?l ?k WHERE { ?s rdfs:label 'Lathe' OPTIONAL { ?s rdfs:label ?l } ?s a ?k }" 2
   s" {'head':{'vars':['l','k']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'},'k':{'type':'uri','value':'urn:test:10'}}]}}"
   NODES
   s" SELECT ?x ?y ?w WHERE { ?x rdfs:label 'Lathe' . ?x t:21* ?y OPTIONAL { ?x t:30 ?w } ?x rdfs:label 'Lathe' }"
   s" {'head':{'vars':['x','y','w']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:1'},'y':{'type':'uri','value':'urn:test:1'},'w':{'type':'literal','value':'1200','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'x':{'type':'uri','value':'urn:test:1'},'y':{'type':'uri','value':'urn:test:2'},'w':{'type':'literal','value':'1200','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s a t:10 FILTER EXISTS { ?s rdfs:label 'Lathe' ; a t:10 } }" 2
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}}]}}"
   NODES ;

: FILTERS ( -- )
   s" SELECT ?s ?w WHERE { ?s t:30 ?w FILTER(?w > 100) }"
   s" {'head':{'vars':['s','w']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'w':{'type':'literal','value':'1200','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'s':{'type':'uri','value':'urn:test:2'},'w':{'type':'literal','value':'950.5','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}}]}}"
   EXPECT
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(STRSTARTS(?l, 'M')) }"
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Mill'}},{'l':{'type':'literal','value':'Machine'}}]}}"
   EXPECT ;

: FILTER-EQUALITIES ( -- )
   \ A conjunct adds the native equality it implies.
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(?s = t:1) }"
   s"  AND t1.v0 = sparql_test.id("
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}}]}}"
   HOLDS
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(t:1 = ?s) }"
   s"  AND t1.id = sparql_test.id(" 1
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}}]}}"
   ON-NODES
   s" SELECT ?s WHERE { ?s rdfs:label ?l FILTER(?l = 'Lathe') }"
   s"  AND t1.label = sparql_test.string(" 1
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}}]}}"
   ON-NODES
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER(sameTerm(?w, 12)) }"
   s"  AND t1.v = sparql_test.number(" 1
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'}}]}}"
   ON-VALUES
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER(sameTerm(0.25, ?w)) }"
   s"  AND t1.v = sparql_test.number(" 1
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:4'}}]}}"
   ON-VALUES
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(?s IN (t:1, t:2)) }"
   s"  OR t1.id = sparql_test.id(" 1
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}},{'l':{'type':'literal','value':'Mill'}}]}}"
   ON-NODES
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(STRLEN(?l) < 6 && (?s = t:1 && ?l != 'Mill')) }"
   s"  AND t1.id = sparql_test.id(" 1
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}}]}}"
   ON-NODES
   s" SELECT ?l ?m WHERE { ?s a t:10 ; rdfs:label ?l OPTIONAL { ?s rdfs:label ?m FILTER(?s = t:1) } }"
   s"  AND t2.id = sparql_test.id(" 1
   s" {'head':{'vars':['l','m']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'},'m':{'type':'literal','value':'Lathe'}},{'l':{'type':'literal','value':'Mill'}}]}}"
   ON-NODES
   s" SELECT ?l WHERE { ?s rdfs:label ?l OPTIONAL { ?s a ?k } FILTER(?k = t:10) }"
   s"  AND t3.v2 = sparql_test.id(" 1
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}},{'l':{'type':'literal','value':'Mill'}}]}}"
   ON-NODES
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER EXISTS { ?s a ?k FILTER(?s = t:1) } }"
   s"  AND t1.id = sparql_test.id(" 1
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}}]}}"
   ON-NODES
   s" SELECT ?k (COUNT(*) AS ?n) WHERE { ?s a ?k } GROUP BY ?k HAVING(?k = t:10)"
   s"  AND t1.kind = sparql_test.id(" 1
   s" {'head':{'vars':['k','n']},'results':{'bindings':[{'k':{'type':'uri','value':'urn:test:10'},'n':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   ON-NODES
   \ No column holds the constant.
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(?l = 'Lathe'@en) }"
   s"  AND false" 1
   s" {'head':{'vars':['l']},'results':{'bindings':[]}}"
   ON-NODES
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(?l = t:1) }"
   s"  AND false" 1
   s" {'head':{'vars':['l']},'results':{'bindings':[]}}"
   ON-NODES
   \ `=` of a number is true of other terms, and a disjunct or a negation
   \ implies no equality.
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER(?w = 12.0) }"
   s" sparql_test.number(" 0
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'}}]}}"
   ON-VALUES
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER(?w IN (t:1, 12.0)) }"
   s" sparql_test.number(" 0
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'}}]}}"
   ON-VALUES
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(?s = t:1 || ?s = t:2) }"
   s" t1.id = sparql_test.id(" 0
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}},{'l':{'type':'literal','value':'Mill'}}]}}"
   ON-NODES
   s" SELECT ?l WHERE { ?s a t:10 ; rdfs:label ?l FILTER(?s NOT IN (t:1, t:3)) }"
   s" t1.id = sparql_test.id(" 0
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Mill'}}]}}"
   ON-NODES
   s" SELECT ?l WHERE { ?s a t:10 ; rdfs:label ?l FILTER(!(?s = t:1)) }"
   s" t1.id = sparql_test.id(" 0
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Mill'}}]}}"
   ON-NODES
   s" SELECT ?l WHERE { ?s rdfs:label ?l FILTER(?s IN ()) }"
   s" t1.id = sparql_test.id(" 0
   s" {'head':{'vars':['l']},'results':{'bindings':[]}}"
   ON-NODES ;

: GRAPHS ( -- )
   s" SELECT * WHERE { GRAPH <urn:test:notes> { t:1 rdfs:comment ?c } }"
   s" {'head':{'vars':['c']},'results':{'bindings':[{'c':{'type':'literal','value':'oiled'}}]}}"
   EXPECT
   s" SELECT ?g ?c WHERE { GRAPH ?g { t:3 ?p ?c } }"
   s" {'head':{'vars':['g','c']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'c':{'type':'literal','value':'worn'}}]}}"
   EXPECT
   s" SELECT ?g ?s ?c WHERE { GRAPH ?g { ?s rdfs:comment ?c } }"
   s" {'head':{'vars':['g','s','c']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'s':{'type':'uri','value':'urn:test:1'},'c':{'type':'literal','value':'oiled'}},{'g':{'type':'uri','value':'urn:test:notes'},'s':{'type':'uri','value':'urn:test:3'},'c':{'type':'literal','value':'worn'}}]}}"
   EXPECT
   s" SELECT ?g WHERE { GRAPH ?g { ?s rdfs:label ?l } }"
   s" {'head':{'vars':['g']},'results':{'bindings':[]}}"
   EXPECT ;

\ GRAPH ?g { P } is P in each named graph, ?g bound after P to that graph.
: NAMED-GRAPHS ( -- )
   s" SELECT * WHERE { GRAPH ?g {} }"
   s" {'head':{'vars':['g']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'}},{'g':{'type':'uri','value':'urn:test:7'}}]}}"
   EXPECT
   s" SELECT * WHERE { GRAPH ?g { FILTER(true) } }"
   s" {'head':{'vars':['g']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'}},{'g':{'type':'uri','value':'urn:test:7'}}]}}"
   EXPECT
   s" SELECT ?g ?c WHERE { GRAPH ?g { GRAPH <urn:test:notes> { t:1 rdfs:comment ?c } } }"
   s" {'head':{'vars':['g','c']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'c':{'type':'literal','value':'oiled'}},{'g':{'type':'uri','value':'urn:test:7'},'c':{'type':'literal','value':'oiled'}}]}}"
   EXPECT
   \ The OPTIONAL reads the active graph; its ?g then meets that graph.
   s" SELECT ?g ?s WHERE { GRAPH ?g { ?s ?p ?o OPTIONAL { ?s t:23 ?g } } }"
   s" {'head':{'vars':['g','s']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'s':{'type':'uri','value':'urn:test:1'}},{'g':{'type':'uri','value':'urn:test:notes'},'s':{'type':'uri','value':'urn:test:3'}},{'g':{'type':'uri','value':'urn:test:7'},'s':{'type':'uri','value':'urn:test:1'}}]}}"
   EXPECT
   \ The MINUS shares no variable with its left side: it removes nothing.
   s" SELECT ?g ?s WHERE { GRAPH ?g { ?s ?p ?o MINUS { ?x t:23 ?y } } }"
   s" {'head':{'vars':['g','s']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'s':{'type':'uri','value':'urn:test:1'}},{'g':{'type':'uri','value':'urn:test:notes'},'s':{'type':'uri','value':'urn:test:3'}},{'g':{'type':'uri','value':'urn:test:7'},'s':{'type':'uri','value':'urn:test:1'}},{'g':{'type':'uri','value':'urn:test:7'},'s':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   s" SELECT * WHERE { GRAPH <urn:test:unknown> {} }"
   s" {'head':{'vars':[]},'results':{'bindings':[]}}"
   EXPECT ;

: OPTIONALS ( -- )
   \ No source matches the OPTIONAL, so the 'Gone' its nested group wrote is
   \ a parameter nothing in the statement reads.
   s" SELECT ?l WHERE { t:1 rdfs:label ?l OPTIONAL { ?s a 'Machine' { ?s rdfs:label 'Gone' OPTIONAL { ?s t:30 ?w } } } }"
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}}]}}"
   EXPECT
   s" SELECT ?s ?w WHERE { ?s a t:11 OPTIONAL { ?s t:30 ?w } }"
   s" {'head':{'vars':['s','w']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'},'w':{'type':'literal','value':'12','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'s':{'type':'uri','value':'urn:test:4'},'w':{'type':'literal','value':'0.25','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}},{'s':{'type':'uri','value':'urn:test:5'}}]}}"
   EXPECT
   \ The FILTER reads ?l, which only the left side binds.
   s" SELECT ?l ?w WHERE { ?s a t:10 ; rdfs:label ?l OPTIONAL { ?s t:30 ?w FILTER(STRSTARTS(?l, 'M')) } }"
   s" {'head':{'vars':['l','w']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'}},{'l':{'type':'literal','value':'Mill'},'w':{'type':'literal','value':'950.5','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}}]}}"
   EXPECT
   \ ?m is maybe bound when the second OPTIONAL meets it.
   s" SELECT ?s ?m ?n WHERE { ?s t:30 ?w OPTIONAL { ?s t:20 ?m } OPTIONAL { ?m t:21 ?n } }"
   s" {'head':{'vars':['s','m','n']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'m':{'type':'uri','value':'urn:test:1'},'n':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:2'},'m':{'type':'uri','value':'urn:test:1'},'n':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:3'},'m':{'type':'uri','value':'urn:test:1'},'n':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:4'},'m':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   s" SELECT ?s ?m ?n WHERE { ?s t:30 ?w OPTIONAL { ?s t:20 ?m } ?m t:21 ?n }"
   s" {'head':{'vars':['s','m','n']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'m':{'type':'uri','value':'urn:test:1'},'n':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:2'},'m':{'type':'uri','value':'urn:test:1'},'n':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:3'},'m':{'type':'uri','value':'urn:test:1'},'n':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT ;

: UNIONS ( -- )
   s" SELECT ?a ?b WHERE { { t:1 t:21 ?a } UNION { t:4 t:22 ?b } }"
   s" {'head':{'vars':['a','b']},'results':{'bindings':[{'a':{'type':'uri','value':'urn:test:2'}},{'b':{'type':'uri','value':'urn:test:4'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { { t:1 rdfs:label ?x } UNION { t:1 t:30 ?x } }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'literal','value':'Lathe'}},{'x':{'type':'literal','value':'1200','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT ;

: MINUSES ( -- )
   s" SELECT ?s WHERE { ?s a t:11 MINUS { ?s t:30 ?w } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:5'}}]}}"
   EXPECT
   \ No shared variable: MINUS removes nothing.
   s" SELECT ?s WHERE { ?s a t:10 MINUS { ?x t:21 ?y } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT ;

: FILTER-EXISTS ( -- )
   s" SELECT ?s WHERE { ?s a t:11 FILTER EXISTS { ?s t:20 t:1 } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'}},{'s':{'type':'uri','value':'urn:test:5'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s a t:11 FILTER NOT EXISTS { ?s t:20 t:1 } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:4'}}]}}"
   EXPECT ;

\ EXISTS substitutes the outer solution in its whole pattern.
: SUBSTITUTIONS ( -- )
   \ A nested group's FILTER reads the outer ?w.
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER EXISTS { ?s rdfs:label ?l { FILTER(?w > 100) } } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   \ The OPTIONAL's triple meets the outer ?m, so a part of another
   \ machine still exists.
   s" SELECT ?s ?m WHERE { ?s a t:11 . ?m a t:10 FILTER EXISTS { ?s rdfs:label ?l OPTIONAL { ?s t:20 ?m } } }"
   s" {'head':{'vars':['s','m']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'},'m':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:3'},'m':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:4'},'m':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:4'},'m':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   \ Substituted, the MINUS shares no variable and removes nothing.
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER NOT EXISTS { ?s t:20 ?o MINUS { ?s t:30 ?v } } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   \ ?m is maybe bound: substituted where bound, the pattern's where not.
   s" SELECT ?s ?m WHERE { ?s t:30 ?w OPTIONAL { ?s t:20 ?m } FILTER EXISTS { ?m t:21 ?n { FILTER(!BOUND(?m)) } } }"
   s" {'head':{'vars':['s','m']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   \ GRAPH's ?g, a VALUES column, a subquery's column and BIND's variable
   \ each meet the outer binding.
   s" SELECT ?g WHERE { VALUES ?g { <urn:test:notes> t:7 } FILTER EXISTS { GRAPH ?g { t:3 ?p ?o } } }"
   s" {'head':{'vars':['g']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER EXISTS { VALUES ?s { t:2 t:3 } } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:3'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER EXISTS { SELECT ?s WHERE { ?s a t:10 } } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}},{'s':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s t:30 ?w FILTER EXISTS { BIND(12 AS ?w) } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'}}]}}"
   EXPECT
   \ BIND's expression reads the outer ?l, as does an EXISTS in it.
   s" SELECT ?s WHERE { ?s rdfs:label ?l FILTER EXISTS { ?s a ?k BIND(?l AS ?x) FILTER(?x = 'Lathe') } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s rdfs:label ?l FILTER EXISTS { ?s a ?k BIND(EXISTS { ?s t:30 ?w FILTER(?l = 'Lathe') } AS ?b) FILTER(?b) } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}}]}}"
   EXPECT ;

: V+ ( ^VEC:vector<u8> n -- )
   {: v:^ k:n :}
   s" v" v VEC:APPEND k FMT:INT$ v VEC:APPEND ;

\ SELECT * of t:1's label as v0, then pre, vK and post for each K under n.
: WIDE-QUERY ( str str n ^VEC:vector<u8> -- )
   {: pre:str post:str n:n v:^ :}
   s" SELECT * WHERE { t:1 rdfs:label ?v0" v VEC:APPEND
   n 1 ?do
      pre v VEC:APPEND s" ?" v VEC:APPEND v i V+
      post v VEC:APPEND
   loop s"  }" v VEC:APPEND ;

\ The results of one row, its n first variables 'Lathe'.
: WIDE-DOC ( n ^VEC:vector<u8> -- )
   {: n:n v:^ :}
   s" {'results':{'bindings':[{" v VEC:APPEND
   n 0 ?do
      i 0<> if s" ," v VEC:APPEND then
      s" '" v VEC:APPEND v i V+
      s" ':{'type':'literal','value':'Lathe'}" v VEC:APPEND
   loop
   s" }]}}" v VEC:APPEND ;

: WIDE-IN ( str str n n ^ARENA:arena -- )
   {: pre:str post:str n:n bound:n a:^ :}
   a TEXT-VEC {: q:^ :} pre post n q WIDE-QUERY
   a TEXT-VEC {: w:^ :} bound w WIDE-DOC
   q $ s" 1048576" a ANSWER {: sql:^ doc:^ :}
   doc $ s" {" STR:STARTS-WITH? dup TTRUE 0= if doc $ type cr exit then
   w $ a UNQUOTE $ JSON:PARSE a ROWS
   doc $ JSON:PARSE a ROWS SAME-ROWS? TTRUE ;

: MANY-IN ( ^ARENA:arena -- )
   {: a:^ :}
   a TEXT-VEC {: q:^ :} s"  . ?s t:20" s" " 300 q WIDE-QUERY
   s" a query of 600 reads" T-LABEL
   q $ a PREFIXED $ s" 1048576" a STORE a COMPILED {: sql:^ ok:bool :}
   ok TFALSE
   sql $ s" : expected at most 512 patterns, sources they match and variables, found the end of the query; split the query, or state predicates so that fewer sources match"
   STR:CONTAINS? TTRUE ;

\ 300 triples and their variables pass what a statement may read.
: MANY ( -- ) [: MANY-IN ;] ARENA:WITH ;

\ One constant more than a PostgreSQL call takes parameters.
: CONSTANTS-IN ( ^ARENA:arena -- )
   {: a:^ :}
   a TEXT-VEC {: q:^ :}
   s" SELECT ?n WHERE { VALUES ?n {" q VEC:APPEND
   65536 0 do s"  " q VEC:APPEND i FMT:INT$ q VEC:APPEND loop
   s"  } }" q VEC:APPEND
   s" a query of 65536 constants" T-LABEL
   q $ s" 1048576" a STORE a COMPILED {: sql:^ ok:bool :}
   ok TFALSE
   sql $ s" : expected at most 65535 constants, found the end of the query; split the query"
   STR:CONTAINS? TTRUE ;

: CONSTANTS ( -- ) [: CONSTANTS-IN ;] ARENA:WITH ;

\ A binding object of more variables than one PostgreSQL call takes, all
\ bound, or all but the first unbound; a `*` fixes no order of its names.
: WIDE ( -- )
   s" ," s" " 51 51 [: WIDE-IN ;] ARENA:WITH
   s"  OPTIONAL { t:5 rdfs:label " s"  }" 101 1 [: WIDE-IN ;] ARENA:WITH ;

: FORMS ( -- )
   s" ASK { t:3 t:20 t:1 }" s" {'head':{},'boolean':true}" EXPECT
   s" ASK { t:3 t:20 t:2 }" s" {'head':{},'boolean':false}" EXPECT
   s" SELECT ?k WHERE { ?s a ?k FILTER(?k = t:11) } LIMIT 2 OFFSET 2"
   s" {'head':{'vars':['k']},'results':{'bindings':[{'k':{'type':'uri','value':'urn:test:11'}}]}}"
   EXPECT
   s" SELECT ?l WHERE { ?s rdfs:label ?l }" LIMITED
   s" SELECT ?l WHERE { t:999 rdfs:label ?l }" LIMITED
   s" ASK { t:3 t:20 t:1 }" LIMITED
   s\" SELECT ?s WHERE { ?s rdfs:label ?l FILTER(REGEX(?l, '^\\\\p{Lu}')) }"
   s\" 0A000: expected an XPath escape PostgreSQL has, found `\\p`" FAILS
   s" SELECT ?s WHERE { ?s ?p }"
   s\" 1:25: expected a variable, IRI, prefixed name, literal or blank node, found `}`; write a term such as ?x or <iri> here\n"
   REJECTS ;

\ A triple meets BIND's term on its native column; an error leaves the
\ variable unbound; BIND's variable is new to its group.
: BINDS ( -- )
   s" SELECT ?s ?m ?l WHERE { ?s t:31 ?c BIND(IRI(CONCAT('urn:test:', STR(?c))) AS ?m) ?m rdfs:label ?l }"
   s" {'head':{'vars':['s','m','l']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'m':{'type':'uri','value':'urn:test:2'},'l':{'type':'literal','value':'Mill'}}]}}"
   EXPECT
   s" SELECT ?s ?z WHERE { ?s t:30 ?w FILTER(?w IN (1200, 12)) BIND(IF(?w = 12, 1/0, ?w) AS ?z) }"
   s" {'head':{'vars':['s','z']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'z':{'type':'literal','value':'1200','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'s':{'type':'uri','value':'urn:test:3'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s ?p ?w BIND(1 AS ?w) }"
   s\" 1:38: expected a variable not already in scope, found `?w`; use a new variable instead of ?w\n"
   REJECTS ;

: +N ( ^VEC:vector<u8> str n -- )
   {: q:^ t:str n:n :} t q VEC:APPEND n FMT:INT$ q VEC:APPEND ;

\ `BIND(IF(?vK = 1, ?vK, 2) AS ?vL)`, K n and L the next.
: +DOUBLE ( ^VEC:vector<u8> n -- )
   {: q:^ n:n :}
   q s"  BIND(IF(?v" n +N q s"  = 1, ?v" n +N
   q s" , 2) AS ?v" n 1+ +N s" )" q VEC:APPEND ;

: SETTING ( str ^ARENA:arena -- )
   {: sql:str a:^ :}
   a TEXT-VEC {: out:^ :} sql out RUN TFALSE out VEC:COUNT 0 T= ;

\ The query answers ?v30 1; a server that plans 2^30 copies stops after 5 s.
: CHAIN ( ^VEC:vector<u8> ^ARENA:arena -- )
   {: q:^ a:^ :}
   s" set statement_timeout = '5s'" a SETTING
   q $ s" {'head':{'vars':['v30']},'results':{'bindings':[{'v30':{'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   a false EXPECT-IN
   s" reset statement_timeout" a SETTING ;

\ BINDs of one group, and of groups each nested in the next.
: CHAINS-IN ( ^ARENA:arena -- )
   {: a:^ :}
   a TEXT-VEC {: q:^ :}
   s" SELECT ?v30 WHERE { VALUES ?v0 { 1 }" q VEC:APPEND
   30 0 do q i +DOUBLE loop
   s"  }" q VEC:APPEND q a CHAIN
   a TEXT-VEC {: g:^ :}
   s" SELECT ?v30 WHERE" g VEC:APPEND
   30 0 do s"  {" g VEC:APPEND loop
   s"  VALUES ?v0 { 1 }" g VEC:APPEND
   30 0 do g i +DOUBLE s"  }" g VEC:APPEND loop
   g a CHAIN ;

\ A chain of BINDs each reading the one before it twice.
: CHAINS ( -- ) [: CHAINS-IN ;] ARENA:WITH ;

\ The query counts ?n 2.
: TWO ( str -- )
   s" {'head':{'vars':['n']},'results':{'bindings':[{'n':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT ;

\ Each solution has a UUID of its own, whatever else it joins.
: UUIDS ( -- )
   s" SELECT (COUNT(DISTINCT ?u) AS ?n) WHERE { VALUES ?s { 1 2 } BIND(STRUUID() AS ?u) }"
   TWO
   s" SELECT (COUNT(DISTINCT ?u) AS ?n) WHERE { ?s rdfs:label ?l BIND(IF(BOUND(?s), STRUUID(), UUID()) AS ?u) ?x t:20 ?s }"
   TWO
   s" SELECT (COUNT(DISTINCT ?id) AS ?n) WHERE { { SELECT ?s (STRUUID() AS ?id) WHERE { VALUES ?s { 1 2 } } } }"
   TWO ;

\ A BIND that calls RAND is evaluated once per solution; one whose term
\ spells RAND's SQL is copied.
: VOLATILE-BINDS ( -- )
   s" SELECT (COUNT(*) AS ?n) WHERE { VALUES ?s { 1 2 } BIND(RAND() AS ?r) FILTER(?r = ?r) }"
   TWO
   s" SELECT ?x WHERE { ?s t:93 ?o BIND(STR(?o) AS ?x) }"
   s" sparql.rand(" s" MATERIALIZED"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'literal','value':'sparql.rand('}}]}}"
   SPELLED-ALONE ;

\ Inline VALUES joins a native column, UNDEF matching any term; no rows is
\ no solution; a trailing VALUES joins the groups.
: DATA-BLOCKS ( -- )
   s" SELECT ?s ?l WHERE { VALUES (?s ?l) { (t:1 UNDEF) (t:3 'Spindle') (UNDEF 'Bolt') (t:2 'Lathe') } ?s rdfs:label ?l }"
   s" {'head':{'vars':['s','l']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'l':{'type':'literal','value':'Lathe'}},{'s':{'type':'uri','value':'urn:test:3'},'l':{'type':'literal','value':'Spindle'}},{'s':{'type':'uri','value':'urn:test:4'},'l':{'type':'literal','value':'Bolt'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { ?s a t:10 VALUES ?s { } }"
   s" {'head':{'vars':['s']},'results':{'bindings':[]}}"
   EXPECT
   s" SELECT ?o (COUNT(*) AS ?n) WHERE { ?s t:20 ?o } GROUP BY ?o VALUES ?o { t:2 t:3 }"
   s" {'head':{'vars':['o','n']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:2'},'n':{'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT ;

\ SELECT's expressions; a later one reads an earlier one's variable.
: PROJECTIONS ( -- )
   s" SELECT ?s (?w * 2 AS ?d) (?d + 1 AS ?e) WHERE { ?s t:30 ?w FILTER(?w < 100) }"
   s" {'head':{'vars':['s','d','e']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'},'d':{'type':'literal','value':'24','datatype':'http://www.w3.org/2001/XMLSchema#integer'},'e':{'type':'literal','value':'25','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'s':{'type':'uri','value':'urn:test:4'},'d':{'type':'literal','value':'0.5','datatype':'http://www.w3.org/2001/XMLSchema#decimal'},'e':{'type':'literal','value':'1.5','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}}]}}"
   EXPECT ;

: GROUPING ( -- )
   s" SELECT ?o (COUNT(*) AS ?n) (COUNT(?s) AS ?m) WHERE { ?s t:20 ?o } GROUP BY ?o"
   s" {'head':{'vars':['o','n','m']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:1'},'n':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'},'m':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'o':{'type':'uri','value':'urn:test:2'},'n':{'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'},'m':{'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT
   s" SELECT (COUNT(*) AS ?n) WHERE { ?s t:20 t:4 }"
   s" {'head':{'vars':['n']},'results':{'bindings':[{'n':{'type':'literal','value':'0','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT
   s" SELECT ?o (COUNT(*) AS ?n) WHERE { ?s t:20 ?o } GROUP BY ?o HAVING (COUNT(*) > 1)"
   s" {'head':{'vars':['o','n']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:1'},'n':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT ;

: AGGREGATES ( -- )
   s" SELECT (SUM(?w) AS ?t) (AVG(?w) AS ?a) WHERE { ?s t:30 ?w FILTER(?w IN (1200, 12)) }"
   s" {'head':{'vars':['t','a']},'results':{'bindings':[{'t':{'type':'literal','value':'1212','datatype':'http://www.w3.org/2001/XMLSchema#integer'},'a':{'type':'literal','value':'606','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}}]}}"
   EXPECT
   s" SELECT (SUM(?w) AS ?t) (AVG(?w) AS ?a) WHERE { ?s t:30 ?w }"
   s" {'head':{'vars':['t','a']},'results':{'bindings':[{'t':{'type':'literal','value':'2162.75','datatype':'http://www.w3.org/2001/XMLSchema#decimal'},'a':{'type':'literal','value':'540.6875','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}}]}}"
   EXPECT
   s" SELECT (MIN(?o) AS ?lo) (MAX(?o) AS ?hi) WHERE { t:1 ?p ?o }"
   s" {'head':{'vars':['lo','hi']},'results':{'bindings':[{'lo':{'type':'uri','value':'urn:test:10'},'hi':{'type':'literal','value':'Lathe'}}]}}"
   EXPECT
   s" SELECT (STRLEN(GROUP_CONCAT(DISTINCT ?o; SEPARATOR='||')) AS ?c) WHERE { ?s t:20 ?o }"
   s" {'head':{'vars':['c']},'results':{'bindings':[{'c':{'type':'literal','value':'22','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT
   s" SELECT ?o ?l WHERE { { SELECT ?o (MIN(?s) AS ?m) WHERE { ?s t:20 ?o } GROUP BY ?o } ?m rdfs:label ?l }"
   s"  IS NULL OR "
   s" {'head':{'vars':['o','l']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:1'},'l':{'type':'literal','value':'Spindle'}},{'o':{'type':'uri','value':'urn:test:2'},'l':{'type':'literal','value':'Bolt'}}]}}"
   WITHOUT
   \ An OPTIONAL binds ?w; t:1's group leaves ?m unbound.
   s" SELECT ?o ?l WHERE { { SELECT ?o (MIN(?w) AS ?m) WHERE { ?s t:20 ?o OPTIONAL { ?s t:22 ?w } } GROUP BY ?o } ?m rdfs:label ?l ; a t:11 }"
   s"  IS NULL OR "
   s" {'head':{'vars':['o','l']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:1'},'l':{'type':'literal','value':'Spindle'}},{'o':{'type':'uri','value':'urn:test:1'},'l':{'type':'literal','value':'Bolt'}},{'o':{'type':'uri','value':'urn:test:2'},'l':{'type':'literal','value':'Bolt'}}]}}"
   HOLDS
   \ Without GROUP BY no solution is still one group.
   s" SELECT (MAX(?s) AS ?m) WHERE { ?s t:20 t:4 }"
   s" {'head':{'vars':['m']},'results':{'bindings':[{}]}}"
   EXPECT ;

\ Numbers by subject: whole and fractional, wide, long fractions, scales
\ apart.
: EDGE-NUMBERS ( -- str )
   s" (SELECT * FROM (VALUES (1::bigint,1::numeric),(1,2.5),(1,-3),(2,4),(2,6.000),(3,1e30),(3,0.1),(3,-0.1),(6,1),(6,2),(6,2),(7,0.3333333333333333333333333),(7,1),(8,1.50),(8,2.50)) t(s,x))" ;

: MORE-NUMBERS ( -- str )
   s" (SELECT * FROM (VALUES (1::bigint,-7.25::numeric),(2,10),(10,0.5)) t(s,x))" ;

\ Strings by subject that order by code point: cases, an accent, the
\ empty string, a character past U+FFFF.
: EDGE-STRINGS ( -- str )
   s" (SELECT * FROM (VALUES (1::bigint,$$b$$),(1,$$B$$),(1,$$é$$),(1,$$a$$),(2,$$$$),(2,$$😀$$),(2,$$z$$)) t(s,x))" ;

: MORE-STRINGS ( -- str )
   s" (SELECT * FROM (VALUES (1::bigint,$$A$$),(3,$$c$$)) t(s,x))" ;

\ Appends a source of from's rows, predicate t:n, objects of the shape.
: +EDGE ( str str str ^VEC:vector<u8> -- )
   {: from:str n:str shape:str out:^ :}
   s" {'from':'" out VEC:APPEND from out VEC:APPEND
   s" ','graph':null,'s':{'col':'s','shape':'id'},'p':{'term':{'type':'uri','value':'urn:test:" out VEC:APPEND
   n out VEC:APPEND s" '}},'o':{'col':'x','shape':'" out VEC:APPEND
   shape out VEC:APPEND s" '}}" out VEC:APPEND ;

\ t:97 holds EDGE-NUMBERS, t:95 them and MORE-NUMBERS, t:94 them of shape
\ `decimal`, t:96 EDGE-STRINGS, t:98 them and MORE-STRINGS.
: EDGE-SOURCES ( ^ARENA:arena -- ^VEC:vector<u8> )
   {: a:^ :}
   a TEXT-VEC {: out:^ :}
   s" [" out VEC:APPEND
   EDGE-NUMBERS s" 97" s" number" out +EDGE s" ," out VEC:APPEND
   EDGE-NUMBERS s" 95" s" number" out +EDGE s" ," out VEC:APPEND
   MORE-NUMBERS s" 95" s" number" out +EDGE s" ," out VEC:APPEND
   EDGE-NUMBERS s" 94" s" decimal" out +EDGE s" ," out VEC:APPEND
   EDGE-STRINGS s" 96" s" string" out +EDGE s" ," out VEC:APPEND
   EDGE-STRINGS s" 98" s" string" out +EDGE s" ," out VEC:APPEND
   MORE-STRINGS s" 98" s" string" out +EDGE s" ]" out VEC:APPEND
   out $ a UNQUOTE ;

: TYPED-SQL ( -- str )
   s" select jsonb_set(s,'{sources}',$1::jsonb)::text from sparql_test.store(1048576) s" ;

: UNTYPED-SQL ( -- str )
   s" select jsonb_set(jsonb_set(s,'{sources}',$1::jsonb),'{shapes}',(select jsonb_object_agg(k,v-'type') from jsonb_each(s->'shapes') e(k,v)))::text from sparql_test.store(1048576) s" ;

\ The description of EDGE-SOURCES that the statement sql answers.
: EDGE-STORE ( str ^ARENA:arena -- ^VEC:vector<u8> )
   {: sql:str a:^ :}
   a TEXT-VEC {: out:^ :}
   sql a EDGE-SOURCES $ out RUN-TEXT TTRUE out ;

: TYPED-IN ( str bool ^ARENA:arena -- )
   {: q:str generic:bool a:^ :}
   TYPED-SQL a EDGE-STORE {: typed:^ :}
   UNTYPED-SQL a EDGE-STORE {: plain:^ :}
   q typed a ANSWER-ON {: tsql:^ tdoc:^ :}
   q plain a ANSWER-ON {: psql:^ pdoc:^ :}
   tsql $ s" sparql.aggregate(" STR:CONTAINS? generic xor TFALSE
   psql $ s" sparql.aggregate(" STR:CONTAINS? TTRUE
   tdoc $ pdoc $ a false SAME? dup TTRUE
   0= if tdoc $ type cr pdoc $ type cr then ;

\ The query answers alike on EDGE-SOURCES with and without the shapes'
\ `type`; with it, its statement calls sparql.aggregate when generic.
: TYPED ( str bool -- ) [: TYPED-IN ;] ARENA:WITH ;

\ SUM, AVG, MIN and MAX of a numeric column and MIN and MAX of a text
\ one aggregate natively, alone, in a union's groups and regrouped from
\ a union's partials.
: TYPED-AGGREGATES ( -- )
   s" SELECT ?s (SUM(?x) AS ?t) (AVG(?x) AS ?a) (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:97 ?x } GROUP BY ?s"
   false TYPED
   s" SELECT (SUM(?x) AS ?t) (AVG(?x) AS ?a) (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:97 ?x }"
   false TYPED
   s" SELECT (SUM(?x) AS ?t) (AVG(?x) AS ?a) (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:97 ?x }"
   false TYPED
   s" SELECT (SUM(?x) AS ?t) (AVG(?x) AS ?a) (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:97 ?x FILTER(?s = t:99) }"
   false TYPED
   \ A native SUM is always bound: it meets the column as a term.
   s" SELECT ?s WHERE { { SELECT (SUM(?x) AS ?t) WHERE { ?r t:97 ?x FILTER(?r = t:6) } } ?s t:97 ?t }"
   false TYPED
   s" SELECT ?s (SUM(?x) AS ?t) (AVG(?x) AS ?a) (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:95 ?x } GROUP BY ?s"
   false TYPED
   s" SELECT ?s (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) (COUNT(*) AS ?n) WHERE { ?s t:95 ?x } GROUP BY ?s"
   false TYPED
   s" SELECT (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:95 ?x }"
   false TYPED
   s" SELECT (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:95 ?x FILTER(?s = t:99) }"
   false TYPED
   s" SELECT ?s (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:96 ?x } GROUP BY ?s"
   false TYPED
   s" SELECT (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:96 ?x }"
   false TYPED
   s" SELECT ?s (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:98 ?x } GROUP BY ?s"
   false TYPED
   s" SELECT (MIN(?x) AS ?lo) (MAX(?x) AS ?hi) WHERE { ?s t:98 ?x FILTER(?s = t:99) }"
   false TYPED
   \ DISTINCT, SUM of strings, GROUP_CONCAT and a shape holding xsd:decimal
   \ alone stay generic.
   s" SELECT ?s (SUM(?x) AS ?t) WHERE { ?s t:94 ?x } GROUP BY ?s"
   true TYPED
   s" SELECT ?s (SUM(DISTINCT ?x) AS ?t) (MIN(DISTINCT ?x) AS ?lo) WHERE { ?s t:97 ?x } GROUP BY ?s"
   true TYPED
   s" SELECT ?s (SUM(?x) AS ?t) (GROUP_CONCAT(?x) AS ?c) WHERE { ?s t:96 ?x } GROUP BY ?s"
   true TYPED ;

\ A union grouped branch by branch answers as the whole union does when a
\ branch has an error or no solution. STRLEN of an IRI is an error.
: BRANCH-GROUPING ( -- )
   \ ?g is apart: each graph's branch holds its own groups.
   s" SELECT ?g (SUM(STRLEN(?o)) AS ?n) WHERE { GRAPH ?g { ?s ?p ?o } } GROUP BY ?g"
   s"  GROUP BY " 2
   s" {'head':{'vars':['g','n']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'n':{'type':'literal','value':'9','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'g':{'type':'uri','value':'urn:test:7'}}]}}"
   TIMES
   s" SELECT ?g (COUNT(*) AS ?n) WHERE { GRAPH ?g { ?s ?p ?o FILTER(?s = t:3) } } GROUP BY ?g"
   s"  GROUP BY " 2
   s" {'head':{'vars':['g','n']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'n':{'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   TIMES
   \ HAVING drops urn:test:7's group of one.
   s" SELECT ?g (COUNT(*) AS ?n) WHERE { GRAPH ?g { ?s ?p ?o FILTER(?s != t:2) } } GROUP BY ?g HAVING (COUNT(*) > 1)"
   s"  GROUP BY " 2
   s" {'head':{'vars':['g','n']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'n':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   TIMES
   \ The second branch leaves ?o unbound: its solutions are one group.
   s" SELECT ?o (COUNT(*) AS ?n) WHERE { { ?s t:20 ?o } UNION { ?s t:30 ?w } } GROUP BY ?o"
   s"  GROUP BY " 3
   s" {'head':{'vars':['o','n']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:1'},'n':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'o':{'type':'uri','value':'urn:test:2'},'n':{'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'n':{'type':'literal','value':'4','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   TIMES
   \ A group of ?s spans both graphs: the partials combine.
   s" SELECT ?s (MIN(STRLEN(?o)) AS ?lo) (MAX(STRLEN(?o)) AS ?hi) WHERE { GRAPH ?g { ?s ?p ?o } } GROUP BY ?s"
   s"  GROUP BY " 3
   s" {'head':{'vars':['s','lo','hi']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'hi':{'type':'literal','value':'5','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'s':{'type':'uri','value':'urn:test:2'}},{'s':{'type':'uri','value':'urn:test:3'},'lo':{'type':'literal','value':'4','datatype':'http://www.w3.org/2001/XMLSchema#integer'},'hi':{'type':'literal','value':'4','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   TIMES
   s" SELECT (COUNT(*) AS ?n) (MIN(?o) AS ?lo) WHERE { GRAPH ?g { ?s ?p ?o FILTER(?s = t:3) } }"
   s"  HAVING count(*)>0" 2
   s" {'head':{'vars':['n','lo']},'results':{'bindings':[{'n':{'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'},'lo':{'type':'literal','value':'worn'}}]}}"
   TIMES
   s" SELECT (COUNT(*) AS ?n) (MIN(?o) AS ?lo) WHERE { GRAPH ?g { ?s ?p ?o FILTER(?s = t:9) } }"
   s"  HAVING count(*)>0" 2
   s" {'head':{'vars':['n','lo']},'results':{'bindings':[{'n':{'type':'literal','value':'0','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   TIMES ;

\ ORDER BY, then OFFSET and LIMIT; strings order by code point.
: ORDERS ( -- )
   s" SELECT ?s ?w WHERE { ?s t:30 ?w } ORDER BY DESC(?w) LIMIT 2 OFFSET 1"
   s" {'head':{'vars':['s','w']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:2'},'w':{'type':'literal','value':'950.5','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}},{'s':{'type':'uri','value':'urn:test:3'},'w':{'type':'literal','value':'12','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   ORDERED
   s" SELECT ?l WHERE { ?s a t:12 . ?s rdfs:label ?l } ORDER BY ?l"
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'B'}},{'l':{'type':'literal','value':'a'}},{'l':{'type':'literal','value':'b'}},{'l':{'type':'literal','value':'é'}}]}}"
   ORDERED ;

: DISTINCTS ( -- )
   s" SELECT DISTINCT ?o WHERE { ?s t:20 ?o }"
   s" {'head':{'vars':['o']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:1'}},{'o':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   s" SELECT DISTINCT ?o WHERE { ?s t:20 ?o } ORDER BY DESC(?o)"
   s" {'head':{'vars':['o']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:2'}},{'o':{'type':'uri','value':'urn:test:1'}}]}}"
   ORDERED ;

\ A subquery binds only what it projects; its ?s below is not the outer ?s.
: SUBQUERIES ( -- )
   s" SELECT ?s ?l WHERE { ?s rdfs:label ?l { SELECT ?s WHERE { ?s a t:10 } } }"
   s" {'head':{'vars':['s','l']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'},'l':{'type':'literal','value':'Lathe'}},{'s':{'type':'uri','value':'urn:test:2'},'l':{'type':'literal','value':'Mill'}}]}}"
   EXPECT
   s" SELECT ?l ?n WHERE { ?o rdfs:label ?l { SELECT ?o (COUNT(?s) AS ?n) WHERE { ?s t:20 ?o } GROUP BY ?o } }"
   s" {'head':{'vars':['l','n']},'results':{'bindings':[{'l':{'type':'literal','value':'Lathe'},'n':{'type':'literal','value':'2','datatype':'http://www.w3.org/2001/XMLSchema#integer'}},{'l':{'type':'literal','value':'Mill'},'n':{'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT
   s" SELECT ?l WHERE { ?s rdfs:label ?l { SELECT ?s WHERE { ?s t:30 ?w } ORDER BY DESC(?w) LIMIT 2 OFFSET 1 } }"
   s" {'head':{'vars':['l']},'results':{'bindings':[{'l':{'type':'literal','value':'Mill'}},{'l':{'type':'literal','value':'Spindle'}}]}}"
   EXPECT
   s" SELECT ?s ?k WHERE { ?s t:20 ?o { SELECT ?k WHERE { ?s a ?k FILTER(?s = t:1) } } }"
   s" {'head':{'vars':['s','k']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'},'k':{'type':'uri','value':'urn:test:10'}},{'s':{'type':'uri','value':'urn:test:4'},'k':{'type':'uri','value':'urn:test:10'}},{'s':{'type':'uri','value':'urn:test:5'},'k':{'type':'uri','value':'urn:test:10'}}]}}"
   EXPECT ;

: PATHS ( -- )
   \ ^p swaps the ends; p/q joins on a hidden variable; p|q is a UNION.
   s" SELECT ?s WHERE { t:1 ^t:20 ?s }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:3'}},{'s':{'type':'uri','value':'urn:test:5'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { t:3 t:20/t:21 ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:2'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { t:1 t:21|t:30 ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:2'}},{'x':{'type':'literal','value':'1200','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT
   \ A member outside a column's shape excludes nothing there.
   s" SELECT ?o WHERE { t:4 !(t:20|rdfs:label) ?o }"
   s" {'head':{'vars':['o']},'results':{'bindings':[{'o':{'type':'uri','value':'urn:test:4'}},{'o':{'type':'uri','value':'urn:test:11'}},{'o':{'type':'literal','value':'0.25','datatype':'http://www.w3.org/2001/XMLSchema#decimal'}}]}}"
   EXPECT
   s" SELECT ?s WHERE { t:2 !(^t:20) ?s }"
   s" {'head':{'vars':['s']},'results':{'bindings':[{'s':{'type':'uri','value':'urn:test:1'}}]}}"
   EXPECT
   \ Each node once, around the cycle too.
   s" SELECT ?x WHERE { t:50 t:25+ ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:51'}},{'x':{'type':'uri','value':'urn:test:52'}},{'x':{'type':'uri','value':'urn:test:50'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { t:50 t:25* ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:50'}},{'x':{'type':'uri','value':'urn:test:51'}},{'x':{'type':'uri','value':'urn:test:52'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { ?x t:26* t:62 }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:62'}},{'x':{'type':'uri','value':'urn:test:61'}},{'x':{'type':'uri','value':'urn:test:60'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { t:60 t:26? ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:60'}},{'x':{'type':'uri','value':'urn:test:61'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { t:99 t:26* ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:99'}}]}}"
   EXPECT
   s" SELECT ?x ?y WHERE { ?x t:26+ ?y }"
   s" {'head':{'vars':['x','y']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:60'},'y':{'type':'uri','value':'urn:test:61'}},{'x':{'type':'uri','value':'urn:test:60'},'y':{'type':'uri','value':'urn:test:62'}},{'x':{'type':'uri','value':'urn:test:61'},'y':{'type':'uri','value':'urn:test:62'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { t:50 (t:25/t:25)+ ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:52'}},{'x':{'type':'uri','value':'urn:test:51'}},{'x':{'type':'uri','value':'urn:test:50'}}]}}"
   EXPECT
   \ A walk of a walk: written once, however deep.
   s" SELECT ?x WHERE { t:50 ((((((((((((((((t:25+)+)+)+)+)+)+)+)+)+)+)+)+)+)+)+) ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:51'}},{'x':{'type':'uri','value':'urn:test:52'}},{'x':{'type':'uri','value':'urn:test:50'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { t:50 ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((t:25)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)* ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:50'}},{'x':{'type':'uri','value':'urn:test:51'}},{'x':{'type':'uri','value':'urn:test:52'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { t:60 ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((!t:25)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)* ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:60'}},{'x':{'type':'uri','value':'urn:test:61'}},{'x':{'type':'uri','value':'urn:test:62'}}]}}"
   EXPECT
   s" SELECT ?x ?y WHERE { ?x (t:26/t:26?)+ ?y }"
   s" {'head':{'vars':['x','y']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:60'},'y':{'type':'uri','value':'urn:test:61'}},{'x':{'type':'uri','value':'urn:test:60'},'y':{'type':'uri','value':'urn:test:62'}},{'x':{'type':'uri','value':'urn:test:61'},'y':{'type':'uri','value':'urn:test:62'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { ?m t:26 t:62 . ?m ^(t:26|t:26*)+ ?x }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:61'}},{'x':{'type':'uri','value':'urn:test:60'}}]}}"
   EXPECT
   \ The walk starts from the ?m the triple before it binds.
   s" SELECT ?m ?y WHERE { t:60 t:26 ?m . ?m t:26* ?y }"
   s" {'head':{'vars':['m','y']},'results':{'bindings':[{'m':{'type':'uri','value':'urn:test:61'},'y':{'type':'uri','value':'urn:test:61'}},{'m':{'type':'uri','value':'urn:test:61'},'y':{'type':'uri','value':'urn:test:62'}}]}}"
   EXPECT
   s" SELECT ?m ?y WHERE { t:60 t:26 ?m { ?m t:26* ?y } }"
   s" SELECT ?m ?y WHERE { t:60 t:26 ?m . ?m t:26* ?y }"
   s" {'head':{'vars':['m','y']},'results':{'bindings':[{'m':{'type':'uri','value':'urn:test:61'},'y':{'type':'uri','value':'urn:test:61'}},{'m':{'type':'uri','value':'urn:test:61'},'y':{'type':'uri','value':'urn:test:62'}}]}}"
   ALIKE
   \ A walk of a GRAPH's group steps in that graph, from the solutions of
   \ the group the GRAPH is in.
   s" SELECT ?x ?y WHERE { t:3 t:20 ?x GRAPH t:7 { ?x t:23+ ?y } }"
   s" {'head':{'vars':['x','y']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:1'},'y':{'type':'uri','value':'urn:test:7'}}]}}"
   EXPECT
   s" SELECT ?x ?y WHERE { t:3 t:20 ?x GRAPH t:7 { ?x t:23* ?y } }"
   s"  WHERE (EXISTS (SELECT 1"
   s" {'head':{'vars':['x','y']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:1'},'y':{'type':'uri','value':'urn:test:1'}},{'x':{'type':'uri','value':'urn:test:1'},'y':{'type':'uri','value':'urn:test:7'}}]}}"
   HOLDS
   \ A zero-length path from a seed the group binds answers it only when
   \ it is a node of the active graph: t:999 is in no triple.
   s" SELECT ?v WHERE { VALUES ?v { t:1 t:999 } ?v t:25? ?v }"
   s" {'head':{'vars':['v']},'results':{'bindings':[{'v':{'type':'uri','value':'urn:test:1'}}]}}"
   EXPECT
   \ With neither end bound, each of the default graph's 37 nodes, the
   \ labels and numbers included, answers itself once; the cycle adds 6.
   s" SELECT (COUNT(*) AS ?n) WHERE { ?x t:25* ?y }"
   s" {'head':{'vars':['n']},'results':{'bindings':[{'n':{'type':'literal','value':'43','datatype':'http://www.w3.org/2001/XMLSchema#integer'}}]}}"
   EXPECT
   \ The nodes of urn:test:7, where no t:25 triple is.
   s" SELECT ?x ?y WHERE { GRAPH t:7 { ?x t:25* ?y } }"
   s" {'head':{'vars':['x','y']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:1'},'y':{'type':'uri','value':'urn:test:1'}},{'x':{'type':'uri','value':'urn:test:2'},'y':{'type':'uri','value':'urn:test:2'}},{'x':{'type':'uri','value':'urn:test:7'},'y':{'type':'uri','value':'urn:test:7'}},{'x':{'type':'uri','value':'urn:test:8'},'y':{'type':'uri','value':'urn:test:8'}}]}}"
   EXPECT
   \ Under GRAPH ?g a walk steps in each named graph; its zero-length
   \ path answers the seed in every one.
   s" SELECT ?g ?y WHERE { GRAPH ?g { t:1 t:23* ?y } }"
   s" {'head':{'vars':['g','y']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:notes'},'y':{'type':'uri','value':'urn:test:1'}},{'g':{'type':'uri','value':'urn:test:7'},'y':{'type':'uri','value':'urn:test:1'}},{'g':{'type':'uri','value':'urn:test:7'},'y':{'type':'uri','value':'urn:test:7'}}]}}"
   EXPECT
   s" SELECT ?g ?x ?y WHERE { GRAPH ?g { ?x t:23+ ?y } }"
   s" {'head':{'vars':['g','x','y']},'results':{'bindings':[{'g':{'type':'uri','value':'urn:test:7'},'x':{'type':'uri','value':'urn:test:1'},'y':{'type':'uri','value':'urn:test:7'}},{'g':{'type':'uri','value':'urn:test:7'},'x':{'type':'uri','value':'urn:test:2'},'y':{'type':'uri','value':'urn:test:8'}}]}}"
   EXPECT
   \ Inside EXISTS the outer ?x seeds the walk, or meets its far end.
   s" SELECT ?x WHERE { ?x t:26 ?y FILTER EXISTS { ?x t:26+ t:61 } }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:60'}}]}}"
   EXPECT
   s" SELECT ?x WHERE { ?x t:26 ?y FILTER EXISTS { t:60 t:26+ ?x } }"
   s" {'head':{'vars':['x']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:61'}}]}}"
   EXPECT
   \ A BIND reads the end of the walk before it.
   s" SELECT ?x ?z WHERE { t:60 t:26+ ?x BIND(STR(?x) AS ?z) }"
   s" {'head':{'vars':['x','z']},'results':{'bindings':[{'x':{'type':'uri','value':'urn:test:61'},'z':{'type':'literal','value':'urn:test:61'}},{'x':{'type':'uri','value':'urn:test:62'},'z':{'type':'literal','value':'urn:test:62'}}]}}"
   EXPECT ;

: SESSION ( -- )
   s" src/lib/sparql/runtime.sql" RUN-SQL
   s" test/sparql/host.sql" RUN-SQL
   SPELLINGS SLOTS SHAPES ALL-SOURCES KEY-SETS TYPED-COLUMNS DRIVEN JOINS
   SUBJECT-ROWS FILTERS FILTER-EQUALITIES GRAPHS NAMED-GRAPHS OPTIONALS
   UNIONS MINUSES FILTER-EXISTS
   SUBSTITUTIONS MANY CONSTANTS WIDE FORMS
   PROJECTIONS GROUPING AGGREGATES TYPED-AGGREGATES BRANCH-GROUPING
   ORDERS DISTINCTS
   SUBQUERIES
   BINDS CHAINS UUIDS VOLATILE-BINDS DATA-BLOCKS PATHS ;

T-RESET
s" sparql-test" ' SESSION MAIN
T-REPORT

;package
