\ Every compile of a query of at most 16,384 characters maps at most 1 MiB,
\ the arenas made in its arena included, on test/sparql/wide-store.json.
\ Each family below is the heaviest query shape found; each shape after
\ them fills the cap with one construct and is checked at its largest size.
\ Bisection finds the largest size k a family accepts within the cap and
\ the largest size p it parses, each falling once with size; the peak is
\ checked at k, at k+1, at p and at the largest size whose text fits.

require src/lib/test/assert.f
require src/lib/reason.f
require src/lib/arena.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fmt.f
require src/lib/json/value.f
require src/lib/sparql/syntax.f
require src/lib/sparql/parse.f
require src/lib/sparql/emit.f
require src/lib/sparql/emit-expression.f

package SPARQL

16384 constant HEAVY-CHARS
1048576 constant HEAVY-MAP
\ Whether the run threw nothing; an E-QUERY rejection is released.
: HEAVY-OK? ( n -- bool )
   {: rc:n :}
   rc 0<> if rc E-QUERY <> if rc throw then REASON:RELEASE then
   rc 0= ;

: HEAVY-MEASURE ( str str ^VEC:vector<u8> ^ARENA:arena -- bool n )
   {: query:str store:str sql:^ arena:^ :}
   sql 0 VEC:RESIZE
   [: query store sql [: drop ;] arena COMPILE-IN ;] catch
   HEAVY-OK? arena ARENA:PEAK ;

\ Whether the query was accepted, and the bytes its arena mapped at most.
\ sql holds the statement outside that arena, as COMPILE's caller does.
: HEAVY-COMPILE ( str str ^VEC:vector<u8> -- bool n )
   [: HEAVY-MEASURE ;] ARENA:WITH ;

: HEAVY-PARSE ( str ^ARENA:arena -- bool )
   {: query:str arena:^ :}
   [: query arena PARSE drop ;] catch HEAVY-OK? ;

\ The zen: predicates of the store's key set, in its order: their bytes
\ in names, each one's end in ends.
: HEAVY-NAME ( JSON:value ^VEC:vector<u8> ^VEC:vector<n> -- )
   {: name names:^ ends:^ :}
   name [: s" zen:" STR:STARTS-WITH? ;] JSON:WITH-STRING if
      name names JSON:APPEND-STRING
      names VEC:COUNT ends VEC:PUSH !
   then ;

: HEAVY-NAMES ( str ^VEC:vector<u8> ^VEC:vector<n> -- )
   {: store:str names:^ ends:^ :}
   store JSON:PARSE s" keys" JSON:GET
   s" predicates" JSON:GET s" values" JSON:GET
   [: drop names ends HEAVY-NAME ;] JSON:EACH-MEMBER ;

\ The predicate k, counted modulo their number.
: HEAVY-PREDICATE ( ^VEC:vector<u8> ^VEC:vector<n> n -- str )
   {: names:^ ends:^ k:n :}
   k ends VEC:COUNT mod {: j:n :}
   j 0= if 0 else ends j 1- VEC:AT @ then {: start:n :}
   names start VEC:AT  ends j VEC:AT @ start -  >str ;

: HEAVY-REPEAT ( ^VEC:vector<u8> str n -- )
   {: q:^ text:str k:n :}
   k 0 ?do text q VEC:APPEND loop ;

: HEAVY-PREFIXES ( ^VEC:vector<u8> -- ) {: q:^ :}
   s\" PREFIX zen: <zen:>\nPREFIX sized: <sized:>\n" q VEC:APPEND ;

: HEAVY-S ( ^VEC:vector<u8> -- ) {: q:^ :}
   q HEAVY-PREFIXES  s" SELECT * WHERE { ?s " q VEC:APPEND ;

: HEAVY-O ( ^VEC:vector<u8> -- ) {: q:^ :}
   s"  ?o }" q VEC:APPEND ;

\ k opening parentheses, the middle, then k closings.
: HEAVY-STAR ( ^VEC:vector<u8> n str str -- )
   {: q:^ k:n mid:str close:str :}
   q s" (" k HEAVY-REPEAT  mid q VEC:APPEND  q close k HEAVY-REPEAT ;

: HEAVY-NEG-S ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   q HEAVY-S  q k s" !zen:of" s" )*" HEAVY-STAR  q HEAVY-O ;

: HEAVY-S-BOTH ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   q HEAVY-PREFIXES  s" SELECT * WHERE { <urn:zen:1> " q VEC:APPEND
   q k s" zen:of" s" )*" HEAVY-STAR
   s"  <urn:zen:2> }" q VEC:APPEND ;

: HEAVY-SEQ-S ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   q HEAVY-S  q k s" zen:of" s" /zen:has)*" HEAVY-STAR  q HEAVY-O ;

: HEAVY-ALT ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   q HEAVY-S  q s" (zen:has|" k HEAVY-REPEAT  s" zen:of" q VEC:APPEND
   k 0 ?do
      s" )" q VEC:APPEND
      s" +?*" STR:DATA i 3 mod + 1 >str q VEC:APPEND
   loop  q HEAVY-O ;

: HEAVY-SEGMENT ( ^VEC:vector<u8> n ^VEC:vector<u8> ^VEC:vector<n> -- )
   {: q:^ k:n names:^ ends:^ :}
   s" (^" q VEC:APPEND  names ends k HEAVY-PREDICATE q VEC:APPEND
   s" |" q VEC:APPEND  names ends k 1+ HEAVY-PREDICATE q VEC:APPEND
   s" +)*/" q VEC:APPEND  names ends k 2 + HEAVY-PREDICATE q VEC:APPEND
   s" ?" q VEC:APPEND ;

: HEAVY-MIXED ( ^VEC:vector<u8> n ^VEC:vector<u8> ^VEC:vector<n> -- )
   {: q:^ k:n names:^ ends:^ :}
   q HEAVY-S
   k 0 ?do
      i 0<> if s" /" q VEC:APPEND then
      q i names ends HEAVY-SEGMENT
   loop  q HEAVY-O ;

: HEAVY-LEVEL ( ^VEC:vector<u8> str n -- ) {: q:^ word:str k:n :}
   s"  " q VEC:APPEND  word q VEC:APPEND
   s"  { ?o" q VEC:APPEND  k FMT:INT$ q VEC:APPEND
   s"  zen:has ?o" q VEC:APPEND  k 1+ FMT:INT$ q VEC:APPEND ;

\ k groups, each inside the one before it after the word.
: HEAVY-NEST ( ^VEC:vector<u8> n str -- ) {: q:^ k:n word:str :}
   s\" PREFIX zen: <zen:>\nSELECT * WHERE { ?s zen:has ?o0" q VEC:APPEND
   k 0 ?do q word i HEAVY-LEVEL loop
   q s"  }" k HEAVY-REPEAT  s"  }" q VEC:APPEND ;

: HEAVY-TRIPLES ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   k 0 ?do
      i 0<> if s" ." q VEC:APPEND then
      s" ?s?p?o" q VEC:APPEND  i FMT:INT$ q VEC:APPEND
   loop ;

: HEAVY-JOIN ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT * WHERE{" q VEC:APPEND  q k HEAVY-TRIPLES
   s" }" q VEC:APPEND ;

: HEAVY-DRIVEN ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT * WHERE{?s<urn:zen:state>?x." q VEC:APPEND
   q k HEAVY-TRIPLES  s" }" q VEC:APPEND ;

: HEAVY-UNION ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT * WHERE{" q VEC:APPEND
   k 0 ?do
      i 0<> if s" UNION" q VEC:APPEND then
      s" {?s?p?o}" q VEC:APPEND
   loop  s" }" q VEC:APPEND ;

: HEAVY-CONST ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT * WHERE{" q VEC:APPEND
   k 0 ?do
      s" ?s ?p " q VEC:APPEND  i 1+ FMT:INT$ q VEC:APPEND
      s" ." q VEC:APPEND
   loop  s" }" q VEC:APPEND ;

: HEAVY-LIST ( ^VEC:vector<u8> n str [ ^VEC:vector<u8> n -- ] -- )
   {: q:^ k:n sep:str item :}
   k 0 ?do
      i 0<> if sep q VEC:APPEND then
      q i item execute
   loop ;

: HEAVY-SUM ( ^VEC:vector<u8> n str -- ) {: q:^ k:n term:str :}
   k 0 ?do
      i 0<> if s" +" q VEC:APPEND then
      term q VEC:APPEND
   loop ;

: HEAVY-INT ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   k FMT:INT$ q VEC:APPEND ;

\ k as a bijective base-52 numeral: a, ..., z, A, ..., Z, aa, ab, ...
: HEAVY-LOCAL ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   k 52 >= if q k 52 / 1- RECURSE then
   s" abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
   STR:DATA k 52 mod + 1 >str q VEC:APPEND ;

: HEAVY-PREFIXED ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" p:" q VEC:APPEND  q k HEAVY-LOCAL ;

: HEAVY-RELATIVE ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" <" q VEC:APPEND  q k HEAVY-LOCAL  s" >" q VEC:APPEND ;

\ ?s, ?p or ?o, cycling with k.
: HEAVY-SPO-VAR ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" ?s?p?o" STR:DATA k 3 mod 2 * + 2 >str q VEC:APPEND ;

\ An IRI of 1,000 characters.
: HEAVY-LONG ( ^VEC:vector<u8> -- ) {: q:^ :}
   s" <http://example.org/" q VEC:APPEND  q s" x" 981 HEAVY-REPEAT
   s\" >\n" q VEC:APPEND ;

: HEAVY-OPT-VAR ( ^VEC:vector<u8> n n -- ) {: q:^ i:n pad:n :}
   s" ?o" q VEC:APPEND  i FMT:INT$ q VEC:APPEND
   s" _" q VEC:APPEND  q s" x" pad HEAVY-REPEAT ;

: HEAVY-OPT-PATTERN ( ^VEC:vector<u8> n n -- ) {: q:^ i:n pad:n :}
   s" ?s ?p " q VEC:APPEND  q i pad HEAVY-OPT-VAR ;

: HEAVY-OPT-BIND ( ^VEC:vector<u8> n n -- ) {: q:^ i:n pad:n :}
   q i pad HEAVY-OPT-PATTERN  s"  BIND(" q VEC:APPEND
   q i pad HEAVY-OPT-VAR  s"  AS ?b" q VEC:APPEND
   i FMT:INT$ q VEC:APPEND  s" )" q VEC:APPEND ;

\ 126 OPTIONALs, each inside the one before it: body appends group i's
\ pattern, tail what follows the outermost group.
: HEAVY-OPT-NEST
   ( ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] [ ^VEC:vector<u8> -- ] -- )
   {: q:^ body tail :}
   s" SELECT * WHERE { ?s ?p ?o" q VEC:APPEND
   127 1 ?do  s"  OPTIONAL { " q VEC:APPEND  q i body execute  loop
   q s"  }" 126 HEAVY-REPEAT  q tail execute  s"  }" q VEC:APPEND ;

: HEAVY-OPT-NESTED ( ^VEC:vector<u8> n -- ) {: q:^ pad:n :}
   q [: pad HEAVY-OPT-PATTERN ;] [: drop ;] HEAVY-OPT-NEST ;

: HEAVY-OPT-NESTED-BIND ( ^VEC:vector<u8> n -- ) {: q:^ pad:n :}
   q [: pad HEAVY-OPT-BIND ;] [: drop ;] HEAVY-OPT-NEST ;

: HEAVY-FILTER-SUM ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s"  FILTER(" q VEC:APPEND  q k s" ?a" HEAVY-SUM
   s"  = 0)" q VEC:APPEND ;

: HEAVY-OPT-NESTED-SUM ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   q [: 0 HEAVY-OPT-PATTERN ;] [: k HEAVY-FILTER-SUM ;] HEAVY-OPT-NEST ;

: HEAVY-OPT-SIBLING ( ^VEC:vector<u8> n -- ) {: q:^ pad:n :}
   s" SELECT * WHERE { ?s ?p ?o" q VEC:APPEND
   127 1 ?do
      s"  OPTIONAL { " q VEC:APPEND  q i pad HEAVY-OPT-PATTERN
      s"  }" q VEC:APPEND
   loop  s"  }" q VEC:APPEND ;

: HEAVY-VALUES ( ^VEC:vector<u8> n [ ^VEC:vector<u8> n -- ] -- )
   {: q:^ k:n item :}
   s" SELECT * WHERE { ?s ?p ?o VALUES ?o { " q VEC:APPEND
   q k s"  " item HEAVY-LIST  s"  } }" q VEC:APPEND ;

: HEAVY-VALUES-INT ( ^VEC:vector<u8> n -- ) [: HEAVY-INT ;] HEAVY-VALUES ;

: HEAVY-IN-INT ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT * WHERE { ?s ?p ?o FILTER(?o IN (" q VEC:APPEND
   q k s" , " [: HEAVY-INT ;] HEAVY-LIST  s" )) }" q VEC:APPEND ;

: HEAVY-VAR-SUM ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT * WHERE { ?a ?p ?o" q VEC:APPEND  q k HEAVY-FILTER-SUM
   s"  }" q VEC:APPEND ;

: HEAVY-ONE-SUM ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT * WHERE { ?s ?p ?o FILTER(?o = " q VEC:APPEND
   q k s" 1" HEAVY-SUM  s" ) }" q VEC:APPEND ;

: HEAVY-PREFIX-NAMES ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" PREFIX p: " q VEC:APPEND  q HEAVY-LONG
   q k [: HEAVY-PREFIXED ;] HEAVY-VALUES ;

: HEAVY-BASE-RELATIVE ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" BASE " q VEC:APPEND  q HEAVY-LONG
   q k [: HEAVY-RELATIVE ;] HEAVY-VALUES ;

: HEAVY-ORDER-BY ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT * WHERE { ?s ?p ?o } ORDER BY " q VEC:APPEND
   q k s"  " [: HEAVY-SPO-VAR ;] HEAVY-LIST ;

: HEAVY-AGGREGATE ( ^VEC:vector<u8> n str -- ) {: q:^ i:n call:str :}
   s" (" q VEC:APPEND  call q VEC:APPEND  s" (?o) AS ?c" q VEC:APPEND
   i FMT:INT$ q VEC:APPEND  s" )" q VEC:APPEND ;

\ SELECT with k calls of the aggregate over ?o.
: HEAVY-SELECT ( ^VEC:vector<u8> n str -- ) {: q:^ k:n call:str :}
   s" SELECT " q VEC:APPEND
   q k s"  " [: call HEAVY-AGGREGATE ;] HEAVY-LIST ;

: HEAVY-COUNTS ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   q k s" COUNT" HEAVY-SELECT  s"  WHERE { ?s ?p ?o }" q VEC:APPEND ;

: HEAVY-KEYS ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   s" SELECT ?k0 WHERE { ?s ?p ?o } GROUP BY" q VEC:APPEND
   k 0 ?do
      s"  (?o AS ?k" q VEC:APPEND  i FMT:INT$ q VEC:APPEND
      s" )" q VEC:APPEND
   loop ;

\ ORDER BY with k equal keys over one source.
: HEAVY-ORDERED ( ^VEC:vector<u8> n str str str -- )
   {: q:^ k:n head:str group:str key:str :}
   q HEAVY-PREFIXES  head q VEC:APPEND
   s"  WHERE { ?s zen:has ?o }" q VEC:APPEND  group q VEC:APPEND
   s"  ORDER BY" q VEC:APPEND  q key k HEAVY-REPEAT ;

: HEAVY-COUNT-KEYS ( ^VEC:vector<u8> n -- )
   s" SELECT (COUNT(?o) AS ?c)" s"  GROUP BY ?s" s"  COUNT(?o)"
   HEAVY-ORDERED ;

: HEAVY-STR-KEYS ( ^VEC:vector<u8> n -- )
   s" SELECT *" s" " s"  STR(?o)" HEAVY-ORDERED ;

: HEAVY-SAMPLES ( ^VEC:vector<u8> n -- ) {: q:^ k:n :}
   q k s" SAMPLE" HEAVY-SELECT
   s"  WHERE { ?s ?p ?o } GROUP BY ?s" q VEC:APPEND ;

: HEAVY-ONE-COUNTS ( ^VEC:vector<u8> n ^VEC:vector<u8> ^VEC:vector<n> -- )
   {: q:^ k:n names:^ ends:^ :}
   q HEAVY-PREFIXES  q k s" COUNT" HEAVY-SELECT
   s"  WHERE { ?s " q VEC:APPEND  names ends 0 HEAVY-PREDICATE q VEC:APPEND
   q HEAVY-O ;

: HEAVY-TEXT ( n ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] -- str )
   {: k:n q:^ make :}
   q 0 VEC:RESIZE  q k make execute  q VEC:BYTES ;

: HEAVY-FITS? ( n ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] -- bool )
   HEAVY-TEXT STR:LEN HEAVY-CHARS <= ;

: HEAVY-ACCEPTS?
   ( n str ^VEC:vector<u8> ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] -- bool )
   {: k:n store:str q:^ sql:^ make :}
   k q make HEAVY-TEXT store sql HEAVY-COMPILE drop ;

: HEAVY-PARSES? ( n ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] -- bool )
   HEAVY-TEXT [: HEAVY-PARSE ;] ARENA:WITH ;

\ The largest n from lo below hi the quotation holds for, given it holds
\ for lo, not for hi, and falls once.
: HEAVY-LARGEST ( n n [ n -- bool ] -- n ) {: pred :}
   begin 2dup swap - 1 > while
      2dup + 2 / dup pred execute if rot drop swap else nip then
   repeat drop ;

\ hi when the quotation holds for it, else HEAVY-LARGEST from lo.
: HEAVY-UPTO ( n n [ n -- bool ] -- n )
   {: lo:n hi:n pred :}
   hi pred execute if hi exit then
   lo hi pred HEAVY-LARGEST ;

\ The largest size whose text fits.
: HEAVY-TOP ( ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] -- n )
   {: q:^ make :}
   1 begin dup q make HEAVY-FITS? while 2 * repeat
   0 swap [: q make HEAVY-FITS? ;] HEAVY-LARGEST ;

: HEAVY-LABEL ( str n bool n ^VEC:vector<u8> -- str )
   {: name:str k:n ok:bool peak:n q:^ :}
   q 0 VEC:RESIZE  name q VEC:APPEND
   s"  k=" q VEC:APPEND  k FMT:INT$ q VEC:APPEND
   ok if s"  accepted" else s"  rejected" then q VEC:APPEND
   s"  peak=" q VEC:APPEND  peak FMT:INT$ q VEC:APPEND
   q VEC:BYTES ;

: HEAVY-CHECK
   ( n str str ^VEC:vector<u8> ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] -- )
   {: k:n name:str store:str q:^ sql:^ make :}
   k q make HEAVY-TEXT store sql HEAVY-COMPILE
   {: ok:bool peak:n :}
   name k ok peak q HEAVY-LABEL T-LABEL
   peak HEAVY-MAP <= TTRUE ;

\ Runs check once on each distinct size among k, k+1, p and top.
: HEAVY-SIZES ( n n n [ n -- ] -- )
   {: k:n p:n top:n check :}
   k check execute
   k top < if k 1+ check execute then
   top k 1+ > if top check execute then
   p k 1+ > p top < and if p check execute then ;

: HEAVY-FAMILY
   ( str str ^VEC:vector<u8> ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] -- )
   {: name:str store:str q:^ sql:^ make :}
   q make HEAVY-TOP {: top:n :}
   0 top [: store q sql make HEAVY-ACCEPTS? ;] HEAVY-UPTO {: k:n :}
   k  k top [: q make HEAVY-PARSES? ;] HEAVY-UPTO  top
   [: name store q sql make HEAVY-CHECK ;] HEAVY-SIZES ;

: HEAVY-SHAPE
   ( str str ^VEC:vector<u8> ^VEC:vector<u8> [ ^VEC:vector<u8> n -- ] -- )
   {: name:str store:str q:^ sql:^ make :}
   q make HEAVY-TOP  name store q sql make HEAVY-CHECK ;

: TEST-HEAVY ( ^ARENA:arena -- ) {: arena:^ :}
   arena ARENA:ALLOCATOR {: al:^ :}
   al 0 [type] u8 VEC:NEW {: file:^ :}
   s" test/sparql/wide-store.json read" T-LABEL
   s" test/sparql/wide-store.json" file FS:READ-FILE+ TTRUE
   file VEC:BYTES {: store:str :}
   al 0 [type] u8 VEC:NEW {: q:^ :}
   al 0 [type] u8 VEC:NEW {: sql:^ :}
   al 0 [type] u8 VEC:NEW {: names:^ :}
   al 0 [type] n VEC:NEW {: ends:^ :}
   store names ends HEAVY-NAMES
   s" join" store q sql [: HEAVY-JOIN ;] HEAVY-FAMILY
   s" driven" store q sql [: HEAVY-DRIVEN ;] HEAVY-FAMILY
   s" union" store q sql [: HEAVY-UNION ;] HEAVY-FAMILY
   s" const" store q sql [: HEAVY-CONST ;] HEAVY-FAMILY
   s" negS" store q sql [: HEAVY-NEG-S ;] HEAVY-FAMILY
   s" Sboth" store q sql [: HEAVY-S-BOTH ;] HEAVY-FAMILY
   s" seqS" store q sql [: HEAVY-SEQ-S ;] HEAVY-FAMILY
   s" alt" store q sql [: HEAVY-ALT ;] HEAVY-FAMILY
   s" mixed" store q sql [: names ends HEAVY-MIXED ;] HEAVY-FAMILY
   s" exists" store q sql [: s" FILTER EXISTS" HEAVY-NEST ;] HEAVY-FAMILY
   s" minus" store q sql [: s" MINUS" HEAVY-NEST ;] HEAVY-FAMILY
   s" opt" store q sql [: s" OPTIONAL" HEAVY-NEST ;] HEAVY-FAMILY
   s" counts" store q sql [: HEAVY-COUNTS ;] HEAVY-FAMILY
   s" samples" store q sql [: HEAVY-SAMPLES ;] HEAVY-FAMILY
   s" one-counts" store q sql [: names ends HEAVY-ONE-COUNTS ;] HEAVY-FAMILY
   s" keys" store q sql [: HEAVY-KEYS ;] HEAVY-FAMILY
   s" count-keys" store q sql [: HEAVY-COUNT-KEYS ;] HEAVY-FAMILY
   s" str-keys" store q sql [: HEAVY-STR-KEYS ;] HEAVY-FAMILY
   s" opt-nested" store q sql [: HEAVY-OPT-NESTED ;] HEAVY-SHAPE
   s" opt-nested-bind" store q sql [: HEAVY-OPT-NESTED-BIND ;] HEAVY-SHAPE
   s" opt-nested-sum" store q sql [: HEAVY-OPT-NESTED-SUM ;] HEAVY-SHAPE
   s" opt-sibling" store q sql [: HEAVY-OPT-SIBLING ;] HEAVY-SHAPE
   s" values-int" store q sql [: HEAVY-VALUES-INT ;] HEAVY-SHAPE
   s" in-int" store q sql [: HEAVY-IN-INT ;] HEAVY-SHAPE
   s" var-sum" store q sql [: HEAVY-VAR-SUM ;] HEAVY-SHAPE
   s" one-sum" store q sql [: HEAVY-ONE-SUM ;] HEAVY-SHAPE
   s" prefix-names" store q sql [: HEAVY-PREFIX-NAMES ;] HEAVY-SHAPE
   s" base-relative" store q sql [: HEAVY-BASE-RELATIVE ;] HEAVY-SHAPE
   s" order-by" store q sql [: HEAVY-ORDER-BY ;] HEAVY-SHAPE ;

: TEST-EMIT ( -- ) [: TEST-HEAVY ;] ARENA:WITH ;

TEST-EMIT

;package

T-REPORT
