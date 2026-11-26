\ w3c-test.f - the W3C SPARQL 1.0 and 1.1 query cases of test/sparql/w3c/
\ through SPARQL:COMPILE against a live PostgreSQL server.
\ test/db/pg-cluster.f starts a private cluster and runs this file with the
\ cluster's conninfo as its one script argument (`bin/hb --load
\ test/db/pg-cluster.f -- test/sparql/w3c-test.f`). The file loads
\ src/lib/sparql/runtime.sql and test/sparql/w3c.sql.
\
\ test/sparql/w3c/manifest.txt has a line `kind name` per case. name.rq is
\ the query, read below a BASE of its URL in the suite, which resolves its
\ relative IRIs as the suite's data does; an evaluation case has its quads
\ in name.jsonl and its expected document in name.srj. Each case loads its
\ quads, none outside evaluation, in place of the last case's and compiles
\ its query over the description the load answers. A `reject` case passes
\ when COMPILE answers false, an `accept` case when the statement it
\ compiles runs, a `results` or `ordered` case when the statement answers
\ the expected document.
\
\ Both documents first pass through w3c.sql, which spells each numeric
\ literal in one form per value. Two documents agree when both answer the
\ same boolean, or both name the same variables in any order and their
\ solutions pair one to one: each pair binds the same variables to the
\ same terms, compared as JSON values, one blank node of the expected
\ document standing for one of the answer across the whole result. In an
\ `ordered` case the n-th solution pairs with the n-th. Otherwise the
\ solutions without blank nodes compare as multisets, and those with them
\ are paired by a search that backtracks, trying each distinct candidate
\ solution once.
\ Loaded without the argument, the file dies naming the harness.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/json.f
require src/lib/sparql.f
require test/sparql/server.f
require src/lib/json/value.f
require src/lib/json/equal.f
require src/lib/vector.f

package W3C-TEST
private

$10000 constant FILE-CAP
FILE-CAP BUFFER: MANIFEST
FILE-CAP BUFFER: QUERY
FILE-CAP BUFFER: DATA
FILE-CAP BUFFER: WANT

0 constant REJECT
1 constant ACCEPT
2 constant RESULTS
3 constant ORDERED

STRUCTURE tally DERIVE addr FIELD cases n FIELD failed n ;STRUCTURE
4 TYPED-BUFFER TALLIES tally

TYPED-VARIABLE PASSED bool


\ ---- comparison --------------------------------------------------------

\ A blank node of the expected document and the answer's it stands for.
STRUCTURE pair DERIVE addr FIELD want str FIELD got str ;STRUCTURE

STRUCTURE row DERIVE addr FIELD value JSON:value FIELD used bool ;STRUCTURE

: SAME-TEXT? ( JSON:value JSON:value -- bool ) {: a b :}
   a JSON:TEXT$ b JSON:TEXT$ JSON:VALUE= ;

: BNODE? ( JSON:value -- bool ) s" type" JSON:GET s" bnode" JSON:STRING= ;

: LABEL$ ( JSON:value -- str ) s" value" JSON:GET JSON:TEXT$ ;

\ Whether the labels pair under the map, pairing them when neither has a
\ partner yet.
: PAIRED? ( ^VEC:vector<pair> str str -- bool )
   {: map:^ w:str g:str :}
   map VEC:COUNT 0 ?do
      map i VEC:AT {: p:^ :}
      p PAIR-WANT @ w JSON:VALUE= {: a:bool :}
      p PAIR-GOT @ g JSON:VALUE= {: b:bool :}
      a b or if a b and unloop exit then
   loop
   map VEC:PUSH {: p:^ :}
   w p PAIR-WANT !  g p PAIR-GOT !  true ;

: TERM-SAME? ( JSON:value ^VEC:vector<pair> JSON:value -- bool )
   {: g map:^ w :}
   w BNODE? {: wb:bool :}  g BNODE? {: gb:bool :}
   wb gb or 0= if w g SAME-TEXT? exit then
   wb gb and 0= if false exit then
   map w LABEL$ g LABEL$ PAIRED? ;

: COUNT-MEMBER ( n JSON:value JSON:value -- n ) {: k:n name v :} k 1+ ;

: COUNT-ELEMENT ( n JSON:value -- n ) {: k:n v :} k 1+ ;

: MEMBERS ( JSON:value -- n ) {: v :}
   0 v [: COUNT-MEMBER ;] JSON:EACH-MEMBER ;

: ELEMENTS ( JSON:value -- n ) {: v :}
   0 v [: COUNT-ELEMENT ;] JSON:EACH ;

\ The solution's binding of the variable a string value names.
: BINDING ( JSON:value JSON:value -- option<JSON:value> ) {: g name :}
   g name [: JSON:FIND ;] JSON:WITH-STRING ;

: MEMBER-SAME ( bool JSON:value JSON:value ^VEC:vector<pair> JSON:value -- bool bool )
   {: ok:bool name v map:^ g :}
   g name BINDING MATCH option
      none OF false ENDOF
      some OF map v TERM-SAME? ENDOF
   ;MATCH ok and dup ;

\ Whether the solutions bind the same variables to terms that pair under
\ the map; a failed pairing may leave pairs the caller cuts.
: ROW-SAME? ( ^VEC:vector<pair> JSON:value JSON:value -- bool )
   {: map:^ w g :}
   w MEMBERS g MEMBERS <> if false exit then
   true w [: map g MEMBER-SAME ;] JSON:EACH-MEMBER-WHILE ;

: GROUND-STEP ( bool JSON:value JSON:value -- bool bool )
   {: ok:bool name v :}
   v BNODE? 0= ok and dup ;

: GROUND? ( JSON:value -- bool ) {: v :}
   true v [: GROUND-STEP ;] JSON:EACH-MEMBER-WHILE ;

: ADD-ROW ( ^VEC:vector<row> JSON:value -- ^VEC:vector<row> )
   {: rows:^ v :}
   v rows VEC:PUSH ROW-VALUE ! rows ;

: ROWS ( JSON:value ^ARENA:arena -- ^VEC:vector<row> ) {: doc a:^ :}
   a ARENA:ALLOCATOR 0 [type] row VEC:NEW
   doc s" results" JSON:GET s" bindings" JSON:GET
   [: ADD-ROW ;] JSON:EACH ;

: BLANKS ( ^VEC:vector<row> ^ARENA:arena -- ^VEC:vector<row> )
   {: rows:^ a:^ :}
   a ARENA:ALLOCATOR 0 [type] row VEC:NEW {: out:^ :}
   rows VEC:COUNT 0 ?do
      rows i VEC:AT ROW-VALUE @ {: v :}
      v GROUND? 0= if out v ADD-ROW drop then
   loop out ;

: TALLY ( ^VEC:vector<row> JSON:value -- n ) {: rows:^ v :}
   0 rows VEC:COUNT 0 ?do
      rows i VEC:AT ROW-VALUE @ v SAME-TEXT? if 1+ then
   loop ;

\ Whether the rows without blank nodes are the same multiset; want and got
\ hold the same number of rows.
: GROUND-SAME? ( ^VEC:vector<row> ^VEC:vector<row> ^ARENA:arena -- bool )
   {: want:^ got:^ a:^ :}
   want a BLANKS VEC:COUNT got a BLANKS VEC:COUNT <> if false exit then
   want VEC:COUNT 0 ?do
      want i VEC:AT ROW-VALUE @ {: v :}
      v GROUND? if
         want v TALLY got v TALLY <> if false unloop exit then
      then
   loop true ;

\ Whether an unused row before row k has its text: tried already, since
\ rows are tried in order.
: TRIED? ( ^VEC:vector<row> n -- bool ) {: got:^ k:n :}
   got k VEC:AT ROW-VALUE @ {: v :}
   k 0 ?do
      got i VEC:AT {: r:^ :}
      r ROW-USED @ 0= if
         r ROW-VALUE @ v SAME-TEXT? if true unloop exit then
      then
   loop false ;

\ Whether want's rows from row at on pair with got's unused rows.
: MATCH-FROM? ( ^VEC:vector<pair> ^VEC:vector<row> ^VEC:vector<row> n -- bool )
   {: map:^ want:^ got:^ at:n :}
   at want VEC:COUNT = if true exit then
   want at VEC:AT ROW-VALUE @ {: w :}
   map VEC:COUNT {: mark:n :}
   got VEC:COUNT 0 ?do
      got i VEC:AT {: r:^ :}
      r ROW-USED @ 0= if got i TRIED? 0= if
         map w r ROW-VALUE @ ROW-SAME? if
            true r ROW-USED !
            map want got at 1+ RECURSE if true unloop exit then
            false r ROW-USED !
         then
         map mark VEC:RESIZE
      then then
   loop false ;

: IN-ORDER? ( ^VEC:vector<pair> ^VEC:vector<row> ^VEC:vector<row> -- bool )
   {: map:^ want:^ got:^ :}
   want VEC:COUNT 0 ?do
      map want i VEC:AT ROW-VALUE @ got i VEC:AT ROW-VALUE @ ROW-SAME?
      0= if false unloop exit then
   loop true ;

: ROWS-SAME? ( ^VEC:vector<row> ^VEC:vector<row> bool ^ARENA:arena -- bool )
   {: want:^ got:^ ordered:bool a:^ :}
   want VEC:COUNT got VEC:COUNT <> if false exit then
   a ARENA:ALLOCATOR 0 [type] pair VEC:NEW {: map:^ :}
   ordered if map want got IN-ORDER? exit then
   want got a GROUND-SAME? 0= if false exit then
   map want a BLANKS got a BLANKS 0 MATCH-FROM? ;

: IN-STEP ( bool JSON:value JSON:value -- bool bool )
   {: found:bool e x :}
   e x SAME-TEXT? dup 0= ;

: IN? ( JSON:value JSON:value -- bool ) {: list x :}
   false list [: x IN-STEP ;] JSON:EACH-WHILE ;

: VAR-STEP ( bool JSON:value JSON:value -- bool bool )
   {: ok:bool e list :}
   list e IN? ok and dup ;

: VARS ( JSON:value -- JSON:value ) s" head" JSON:GET s" vars" JSON:GET ;

: VARS-SAME? ( JSON:value JSON:value -- bool ) {: w g :}
   w VARS {: wv :}  g VARS {: gv :}
   wv ELEMENTS gv ELEMENTS <> if false exit then
   true wv [: gv VAR-STEP ;] JSON:EACH-WHILE ;

: SAME? ( str str bool ^ARENA:arena -- bool )
   {: g:str w:str ordered:bool a:^ :}
   g JSON:PARSE {: got :}  w JSON:PARSE {: want :}
   want s" boolean" SPARQL-TEST:HAS? {: ask:bool :}
   got s" boolean" SPARQL-TEST:HAS? ask xor if false exit then
   ask if got s" boolean" JSON:GET want s" boolean" JSON:GET SAME-TEXT? exit then
   want got VARS-SAME? 0= if false exit then
   want a ROWS got a ROWS ordered a ROWS-SAME? ;


\ ---- cases -------------------------------------------------------------

: KIND$ ( n -- str )
   dup REJECT = if drop s" reject" exit then
   dup ACCEPT = if drop s" accept" exit then
   RESULTS = if s" results" exit then
   s" ordered" ;

: KIND ( str -- n ) {: k:str :}
   4 0 do k i KIND$ STR:= if i unloop exit then loop
   s" w3c-test: manifest.txt names an unknown kind: " type k type cr
   s" " EXIT-STATUS:FAILURE die ;

: EVALUATION? ( n -- bool ) RESULTS >= ;

\ The text of the case's file with the extension, in buf.
: READ ( str str ^u8 ^ARENA:arena -- str )
   {: name:str ext:str buf:^ a:^ :}
   a SPARQL-TEST:TEXT-VEC {: path:^ :}
   s" test/sparql/w3c/" path VEC:APPEND
   name path VEC:APPEND  ext path VEC:APPEND
   path SPARQL-TEST:$ buf FILE-CAP FS:READ-ALL buf swap >str ;

\ Replaces the dataset with the quads; true with the store description in
\ out, else the failure there.
: LOAD ( str ^VEC:vector<u8> -- bool ) {: q:str out:^ :}
   s" select sparql_w3c.load($1)::text" q out SPARQL-TEST:RUN-TEXT ;

\ COMPILE over the description: true with the statement in sql, else the
\ rejection text goes to answer.
: COMPILED ( str ^VEC:vector<u8> ^VEC:vector<u8> ^VEC:vector<u8> -- bool )
   {: q:str store:^ sql:^ answer:^ :}
   q store SPARQL-TEST:$ sql SPARQL-TEST:COMPILE dup if exit then
   sql SPARQL-TEST:$ answer VEC:APPEND ;

\ The document with each number in one form per value, in out; false
\ with the failure there and after the answer.
: NUMBER-FORMS ( str ^VEC:vector<u8> ^VEC:vector<u8> -- bool )
   {: d:str out:^ answer:^ :}
   s" select sparql_w3c.number_forms($1::jsonb)::text" d out SPARQL-TEST:RUN-TEXT
   dup 0= if out SPARQL-TEST:$ answer VEC:APPEND then ;

\ Whether the answer agrees with the case's expected document.
: AGREES? ( str ^VEC:vector<u8> n ^ARENA:arena -- bool )
   {: name:str answer:^ kind:n a:^ :}
   a SPARQL-TEST:TEXT-VEC {: got:^ :}  a SPARQL-TEST:TEXT-VEC {: want:^ :}
   answer SPARQL-TEST:$ got answer NUMBER-FORMS 0= if false exit then
   name s" .srj" WANT a READ want answer NUMBER-FORMS
   0= if false exit then
   got SPARQL-TEST:$ want SPARQL-TEST:$ kind ORDERED = a SAME? ;

\ The case's verdict; its answer goes to answer.
: VERDICT ( str str n ^VEC:vector<u8> ^ARENA:arena -- bool )
   {: name:str q:str kind:n answer:^ a:^ :}
   a SPARQL-TEST:TEXT-VEC {: store:^ :}
   kind EVALUATION? if name s" .jsonl" DATA a READ else s" " then
   store LOAD 0= if store SPARQL-TEST:$ answer VEC:APPEND false exit then
   a SPARQL-TEST:TEXT-VEC {: sql:^ :}
   q store sql answer COMPILED {: ok:bool :}
   kind REJECT = if ok if sql SPARQL-TEST:$ answer VEC:APPEND then ok 0= exit then
   ok 0= if false exit then
   sql SPARQL-TEST:$ answer SPARQL-TEST:RUN {: ran:bool :}
   kind ACCEPT = ran 0= or if ran exit then
   name answer kind a AGREES? ;

: EXPECTED. ( str n ^ARENA:arena -- ) {: name:str kind:n a:^ :}
   kind REJECT = if s" a rejection" type cr exit then
   kind ACCEPT = if s" a statement that runs" type cr exit then
   name s" .srj" WANT a READ type ;

: FAILED. ( str str n ^VEC:vector<u8> ^ARENA:arena -- )
   {: name:str q:str kind:n answer:^ a:^ :}
   s" query:" type cr q type
   s" expected:" type cr name kind a EXPECTED.
   s" answered:" type cr answer SPARQL-TEST:$ type cr ;

: COUNT+ ( n bool -- ) {: kind:n ok:bool :}
   kind TALLIES {: t:^ :}
   1 t TALLY-CASES +!
   ok 0= if 1 t TALLY-FAILED +! then ;

\ A throw out of the case is its answer; the case fails.
: CASE-IN ( str n ^ARENA:arena -- ) {: name:str kind:n a:^ :}
   name T-LABEL
   a SPARQL-TEST:TEXT-VEC {: answer:^ :}
   name s" .rq" QUERY a READ {: body:str :}
   a SPARQL-TEST:TEXT-VEC {: text:^ :}
   s" BASE <https://w3c.github.io/rdf-tests/sparql/" text VEC:APPEND
   name text VEC:APPEND  s\" .rq>\n" text VEC:APPEND
   body text VEC:APPEND
   text SPARQL-TEST:$ {: q:str :}
   false PASSED !
   [: name q kind answer a VERDICT PASSED ! ;] catch {: rc:n :}
   rc 0<> if
      s" threw " answer VEC:APPEND rc FMT:INT$ answer VEC:APPEND
      s"  " answer VEC:APPEND rc ERROR:NAME$ answer VEC:APPEND
   then
   kind PASSED @ COUNT+
   PASSED @ TTRUE
   PASSED @ 0= if name q kind answer a FAILED. then ;

: CASE-LINE ( str -- ) {: line:str :}
   line 32 STR:INDEX-OF MATCH option
      none OF
         s" w3c-test: manifest.txt has a line without a kind: " type
         line type cr s" " EXIT-STATUS:FAILURE die
      ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: sp:n :}
   line str> {: from:^ u:n :}
   from sp + 1+ u sp - 1- >str  from sp >str KIND [: CASE-IN ;] ARENA:WITH ;

: CASES ( -- )
   s" test/sparql/w3c/manifest.txt" MANIFEST FILE-CAP FS:READ-ALL
   MANIFEST swap >str {: text:str :}
   0 begin >r text 10 r> STR:SPLIT-NEXT while
      >r dup STR:LEN 0 > if CASE-LINE else drop then r>
   repeat drop drop ;

: SUMMARY ( -- )
   4 0 do
      i TALLIES {: t:^ :}
      s" w3c-test: " type i KIND$ type s"  " type
      t TALLY-CASES @ FMT:.INT s"  cases, " type
      t TALLY-FAILED @ FMT:.INT s"  failed" type cr
   loop ;

: SESSION ( -- )
   s" src/lib/sparql/runtime.sql" SPARQL-TEST:RUN-SQL
   s" test/sparql/w3c.sql" SPARQL-TEST:RUN-SQL
   T-LABEL-CLEAR
   CASES SUMMARY ;

T-RESET
s" w3c-test" ' SESSION SPARQL-TEST:MAIN
T-REPORT

;package
