\ SPARQL:COMPILE compiles a read-only SPARQL query to one PostgreSQL
\ statement on the store a host describes (src/lib/sparql/store.f). It takes
\ the query text, the description, a vector and a quotation. True: the
\ statement is appended to the vector and the quotation is run on each of
\ its parameters' texts, $1's first; a text is valid until the quotation
\ returns. False: the query is rejected, the vector holds the text `L:C:
\ expected X, found Y; FIX`, the query's line L and a caret under column
\ C, and the quotation is not run. A description not in the format throws
\ JSON:E-MEMBER.
\ CONTRACT. The statement takes its parameters as text and answers one row of
\ one text, the SPARQL Results JSON document; once the document passes the
\ description's `bytes` it raises SQLSTATE 54000. The host runs it in a
\ transaction where the sources are scoped and schema sparql holds
\ src/lib/sparql/runtime.sql from this tree. Each source row whose slot
\ columns are non-null, for a `cols` slot the one holding its value, is one
\ triple of its graph, none twice within a graph. A shape's `in` is strict
\ and immutable, NULL for every term outside its `holds`, its `out`
\ canonical, the two inverse, native `=` term identity; a shape `within`
\ another is a part of it, and different shapes hold disjoint terms unless
\ one is within the other, so parts of one parent are disjoint. A term is
\ a SPARQL Results JSON object, its language tag lowercase, with no
\ datatype for xsd:string. A column slot with a `key` holds an IRI exactly
\ in the rows whose key column is one of the IRI's keys in the key set, and
\ an IRI the set does not list in none. At most one of a `cols` slot's
\ columns is non-null in a row, and it holds the slot's value. `from`,
\ `in`, `out`, every `col` and a key set's `type` are host SQL, emitted as
\ written; query text reaches SQL only inside parameters.
\ STORAGE CLASS. CALLER-OWNED: COMPILE releases all it allocates.

require src/lib/sparql/emit-expression.f
require src/lib/sparql/syntax.f
require src/lib/reason.f
require src/lib/sparql/emit.f
require src/lib/vector.f

package SPARQL
public

: COMPILE ( str str ^VEC:vector<u8> [ str -- ] -- bool )
   {: query:str store:str sql:^ params :}
   [: query store sql params [: COMPILE-IN ;] ARENA:WITH ;] catch
   {: rc:n :}
   rc 0= if true exit then
   rc E-QUERY <> if rc throw then
   E-QUERY REASON:TEXT$ sql VEC:APPEND false ;

seal-package
;package
