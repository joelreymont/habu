\ Query syntax through the parser's load path: the model-facing examples
\ and the grammar's other forms parse, and each rejected query, a form this
\ endpoint drops included, reads exactly its text and following its fixes
\ takes one per mistake it holds. Flat chains of 10,000 parse, variables
\ under nested groups make at most 10 nodes per token, relative IRIs take
\ little more arena than their text, and only nesting is bounded.

require src/lib/test/assert.f
require src/lib/sparql/parse.f
require src/lib/sparql/lex.f
require src/lib/sparql/expression.f
require src/lib/sparql/syntax.f
require src/lib/sparql/lex.f
require src/lib/sparql/iri.f
require src/lib/sparql/segs.f
require src/lib/vector.f
require src/lib/reason.f
require src/lib/utf8-scalar.f

package SPARQL
package SYNTAX-TEST

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
   {: state triple:n :}
   state triple NODE-C@ {: object:n :}
   state object NODE-K N-IRI = if state object IRI$ exit then
   state object TEXT$ ;

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
   s" BASE <https://example.org/base/> SELECT * WHERE { ?s <p> ?o }"
      ACCEPT
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
   0 state SYNTAX-NODES @ SEG-COUNT 1 ?do
      state i NODE-K N-IRI = if drop i then
   loop
   state swap IRI$ ;

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

32 constant MOST-FIXES

\ The bytes of s from offset a up to offset b.
: SUB$ ( str n n -- str )
   {: s:str a:n b:n :}
   s STR:DATA a +  b a -  >str ;

: FROM$ ( str n -- str )
   {: s:str a:n :}
   s a s STR:LEN SUB$ ;

: FIRST-LINE ( str -- str )
   {: s:str :}
   s 0  s 0 T-LINE-END  SUB$ ;

\ The first line of q's rejection, empty when q parses.
: REJECTION ( str -- str )
   {: q:str :}
   [: q [: PARSE-IN ;] ARENA:WITH ;] catch {: code:n :}
   code 0= if s" " exit then
   code E-QUERY <> if code throw then
   E-QUERY REASON:TEXT$ FIRST-LINE ;

: PLACE-NUMBER ( str -- n )
   STR:PARSE-POS MATCH option
     none OF 0 ENDOF
     some OF ENDOF
   ;MATCH ;

\ The offset where line k of q starts; a line ends at LF.
: LINE-START ( str n -- n )
   {: q:str k:n :}
   k 1 <= if 0 exit then
   q  q k 1- RECURSE  T-LINE-END 1+ ;

\ The offset in q of the place `L:C:` that the line r starts with.
: FIX-PLACE ( str str -- n )
   {: q:str r:str :}
   r 0 T-DIGITS-END {: a:n :}
   r 0 a SUB$ PLACE-NUMBER {: line:n :}
   r a 1+  r a 1+ T-DIGITS-END  SUB$
      PLACE-NUMBER {: col:n :}
   q line LINE-START col + 1-  q STR:LEN min ;

\ The offset of the last t in s at or after k, else -1.
: LAST-FROM ( str str n -- n )
   {: s:str t:str k:n :}
   s t k T-FROM {: j:n :}
   j 0< if -1 exit then
   s t j 1+ RECURSE  dup 0< if drop j then ;

\ The found text and the fix of the line r; the end of the query is
\ found as no text.
: FIX-PARTS ( str -- str str )
   {: r:str :}
   r s" , found " 0 T-FROM 8 + {: f:n :}
   s" the end of the query; " {: end:str :}
   r f FROM$ end STR:STARTS-WITH? if
      s" "  r f end STR:LEN + FROM$ exit then
   r s" `; " f LAST-FROM {: j:n :}
   r f 1+ j SUB$  r j 3 + FROM$ ;

\ Whether q holds t at offset k.
: HOLDS-AT? ( str n str -- bool )
   {: q:str k:n t:str :}
   k t STR:LEN +  q STR:LEN > if false exit then
   q k  k t STR:LEN +  SUB$ t STR:= ;

\ Where q's text from offset k resumes past blanks and comments.
: RESUME ( str n -- n )
   {: q:str k:n :}
   k q STR:LEN >= if k exit then
   q STR:DATA k + c@ {: c:n :}
   c 35 = if q  q k T-LINE-END  RECURSE exit then
   c WHITE? if q k 1+ RECURSE exit then
   k ;

\ Whether d has no space and starts with no lowercase letter or holds a
\ colon, as a prefixed name does.
: LITERAL? ( str -- bool )
   {: d:str :}
   d s"  " STR:CONTAINS? if false exit then
   d s" :" STR:CONTAINS? if true exit then
   d STR:DATA c@ dup STR:ASCII-UPPER = ;

\ Where the first item of the list t, `X, Y or Z`, ends and where the rest
\ starts; both are t's length for the last item.
: ITEM ( str -- n n )
   {: t:str :}
   t s" , " 0 T-FROM {: c:n :}
   t s"  or " 0 T-FROM {: o:n :}
   c 0< 0=  o 0<  c o <  or  and if c c 2 + exit then
   o 0< 0= if o o 4 + exit then
   t STR:LEN dup ;

\ Whether d is a list of tokens, `X, Y or Z`, or one token.
: TOKEN-LIST? ( str -- bool )
   {: d:str :}
   d ITEM {: i:n rest:n :}
   d 0 i SUB$ LITERAL? 0= if false exit then
   rest d STR:LEN = if true exit then
   d rest FROM$ RECURSE ;

\ Whether the description d is query text, which stands for itself.
: VERBATIM? ( str -- bool )
   {: d:str :}
   d s" ?" STR:STARTS-WITH?  d s" (" STR:STARTS-WITH? or ;

\ The stand-in for a fix's description d, each ?x in it a variable to
\ number, and whether it has one.
: CONCRETE ( str -- str bool )
   {: d:str :}
   d VERBATIM? if d true exit then
   d s" a term such as ?x or <iri>" STR:= if s" ?x" true exit then
   d s" a variable" STR:= if s" ?x" true exit then
   d s" a string" STR:= if s\" \"s\"" true exit then
   d s" a non-negative integer" STR:= if s" 1" true exit then
   d s" a variable or an IRI" STR:= if s" ?x" true exit then
   d s" an IRI, literal or UNDEF" STR:= if s" <urn:x>" true exit then
   d s" a row in parentheses" STR:= if s" (UNDEF)" true exit then
   d s" an operator" STR:= if s" &&" true exit then
   d s" an IRI or prefixed name" STR:= if s" <urn:x>" true exit then
   d s" the arguments" STR:= if s" ?x" true exit then
   d s" a predicate and an object" STR:= if s" ?x ?x" true exit then
   d s" the variables to order by" STR:= if s" ?x" true exit then
   d s" BY and the variables to order by" STR:= if s" BY ?x" true exit then
   d s" a condition in parentheses" STR:= if s" (true)" true exit then
   d s" a prefix such as ex:" STR:= if s" ex:" true exit then
   d s" an IRI in <...>" STR:= if s" <urn:x:>" true exit then
   s" " false ;

\ The largest N of the variables pN, p `?x` or `$x`, that q holds from
\ offset k, else 0.
: LAST-X ( str str n -- n )
   {: q:str p:str k:n :}
   q p k T-FROM {: j:n :}
   j 0< if 0 exit then
   q  j 2 +  q j 2 + T-DIGITS-END  SUB$ PLACE-NUMBER
   q p j 1+ RECURSE max ;

\ The number N of the first variable ?xN q does not use: one past the
\ largest it does.
: FRESH ( str -- n )
   {: q:str :}
   q s" ?x" 0 LAST-X  q s" $x" 0 LAST-X  max 1+ ;

\ Appends n's decimal digits.
: DIGITS ( n ^VEC:vector<u8> -- )
   {: n:n out :}
   n 9 > if n 10 / out RECURSE then
   n 10 mod 48 + out VEC:PUSH c! ;

\ Appends t with each ?x in it numbered from n on.
: NUMBERED ( str n ^VEC:vector<u8> -- )
   {: t:str n:n out :}
   t s" ?x" 0 T-FROM {: j:n :}
   j 0< if t out VEC:APPEND exit then
   t 0 j 2 + SUB$ out VEC:APPEND  n out DIGITS
   t j 2 + FROM$  n 1+  out RECURSE ;

\ Fails the case naming the fix the follower cannot apply.
: CANNOT ( str -- bool )
   s" the follower cannot apply the fix" T-ASSERT-DETAIL type cr false ;

\ Whether the fix f is one a follower cannot apply without a choice of its
\ own: a name, a variable, an encoding or a query to write.
: UNEDITED? ( str -- bool )
   {: f:str :}
   f s" use a new variable instead of " STR:STARTS-WITH?
   f s" call " STR:STARTS-WITH?  f s"  argument" STR:CONTAINS? and or
   f s" write SELECT " STR:STARTS-WITH?  f s"  to read " STR:CONTAINS? and or
   f s" rename the label" STR:= or
   f s" write the query in UTF-8" STR:= or
   f s" use aggregates only in SELECT, HAVING and ORDER BY, unnested" STR:= or
   f s" write GROUP BY, HAVING, ORDER BY, LIMIT and OFFSET in that order, each once" STR:= or
   f s" bind a variable instead: a query cannot make blank nodes" STR:= or
   f s" write the full IRI or declare BASE <...>" STR:= or
   f s" nest fewer groups or expressions" STR:= or ;

\ Fails the case naming a description the follower knows no token for.
: UNKNOWN ( str -- bool )
   s" the follower knows no token for the description" T-ASSERT-DETAIL
   type cr false ;

: KEEP-HEAD ( str n ^VEC:vector<u8> -- )
   {: q:str k:n out :}
   q 0 k SUB$ out VEC:APPEND ;

: KEEP-TAIL ( str n ^VEC:vector<u8> -- )
   {: q:str k:n out :}
   q k FROM$ out VEC:APPEND ;

\ Appends q with t written at offset k.
: PUT ( str n str ^VEC:vector<u8> -- )
   {: q:str k:n t:str out :}
   q k out KEEP-HEAD  t out VEC:APPEND  q k out KEEP-TAIL ;

\ Appends q with ` t ` written at offset k.
: SPACED ( str n str ^VEC:vector<u8> -- )
   {: q:str k:n t:str out :}
   q k out KEEP-HEAD  s"  " out VEC:APPEND  t out VEC:APPEND
   s"  " out VEC:APPEND  q k out KEEP-TAIL ;

\ Appends q with its bytes from offset a up to offset b replaced by t
\ between pads, each ?x in t a variable q does not use unless t names the
\ query's own variables; answers where q's rest starts in what it appended.
: REWRITE ( str n n str str bool ^VEC:vector<u8> -- n )
   {: q:str a:n b:n t:str pad:str own:bool out :}
   out VEC:COUNT {: from:n :}
   q a out KEEP-HEAD  pad out VEC:APPEND
   own if t out VEC:APPEND else t  q FRESH  out NUMBERED then
   pad out VEC:APPEND
   out VEC:COUNT from -  q b out KEEP-TAIL ;

\ Whether the query in out past its first `from` bytes parses, or is
\ rejected past the token at offset e of it.
: FITS? ( ^VEC:vector<u8> n n -- bool )
   {: out from:n e:n :}
   out VEC:BYTES from FROM$ {: c:str :}
   c REJECTION {: r:str :}
   r STR:LEN 0= if true exit then
   c r FIX-PLACE  c e RESUME > ;

\ Appends q as REWRITE does with the first token of the list d after
\ which the query FITS?, else with its last.
: MEMBER ( str n n str str ^VEC:vector<u8> -- )
   {: q:str a:n b:n d:str pad:str out :}
   out VEC:COUNT {: from:n :}
   d ITEM {: i:n rest:n :}
   q a b  d 0 i SUB$  pad false out REWRITE {: e:n :}
   rest d STR:LEN = if exit then
   out from e FITS? if exit then
   out from VEC:RESIZE
   q a b  d rest FROM$  pad out RECURSE ;

\ Appends q as REWRITE does with what the description d stands for: of
\ a list of tokens, the one MEMBER picks.
: STAND-IN ( str n n str str ^VEC:vector<u8> -- bool )
   {: q:str a:n b:n d:str pad:str out :}
   d TOKEN-LIST? if q a b d pad out MEMBER true exit then
   d CONCRETE {: t:str known:bool :}
   known 0= if d UNKNOWN exit then
   q a b t pad  d VERBATIM?  out REWRITE drop true ;

\ Appends q with ` T ` written at offset k, T what d stands for.
: INSERT ( str n str ^VEC:vector<u8> -- bool )
   {: q:str k:n d:str out :}
   q k k d s"  " out STAND-IN ;

\ Appends q with its bytes from offset a up to offset b replaced by what d
\ stands for.
: SUBSTITUTE ( str n n str ^VEC:vector<u8> -- bool )
   {: q:str a:n b:n d:str out :}
   q a b d s" " out STAND-IN ;

\ Appends q without its bytes from offset a up to offset b.
: OMIT ( str n n ^VEC:vector<u8> -- )
   {: q:str a:n b:n out :}
   q a out KEEP-HEAD  q b out KEEP-TAIL ;

\ Appends q without its bytes from offset a up to offset b; false for no
\ span, a negative a.
: CUT ( str n n ^VEC:vector<u8> -- bool )
   {: q:str a:n b:n out :}
   a 0< if false exit then
   q a b out OMIT  true ;

: SPACE-BEFORE ( str n -- n )
   {: q:str k:n :}
   k begin dup 0 > while
      dup 1- q STR:DATA + c@ STR:SPACE > if exit then
      1-
   repeat ;

\ Where the character at offset k of q ends: its \u or \U escape, or its
\ UTF-8 bytes.
: SCALAR-END ( str n -- n )
   {: q:str k:n :}
   q k s" \u" HOLDS-AT? if k 6 + exit then
   q k s" \U" HOLDS-AT? if k 10 + exit then
   q k UTF8:SUBPART-END ;

\ The span of q the found text takes at offset k, else -1 -1; a found
\ run of `U+XXXX` is as many characters as q holds there.
: FOUND ( str n str -- n n )
   {: q:str k:n found:str :}
   q k found HOLDS-AT? if k  k found STR:LEN +  exit then
   found s" U+" STR:STARTS-WITH? 0= if -1 -1 exit then
   q k SCALAR-END {: e:n :}
   found s" U+" 2 T-FROM {: j:n :}
   j 0< if k e exit then
   q e  found j FROM$  RECURSE nip  k swap ;

\ Token j, lexed again from the query's start.
: NTH-TOKEN ( ^syntax n -- ^token )
   {: state j:n :}
   state FIRST-AT  j 0 ?do state swap PAST-TOKEN loop
   state swap LEX-AT ;

: TOKEN ( ^syntax n -- str )
   NTH-TOKEN TOKEN-TEXT @ ;

\ Whether the lexer reads the token at offset n.
: LEXES? ( ^syntax n -- bool )
   {: state at:n :}
   [: state at LEX-AT drop ;] catch {: code:n :}
   code 0<>  code E-QUERY <>  and if code throw then
   code 0= ;

\ How many tokens the lexer reads from the query's start, up to one it
\ rejects.
: TOKENS ( ^syntax -- n )
   {: state :}
   0 state FIRST-AT begin
      state over LEXES? if
         state SYNTAX-NEXT TOKEN-KIND @ 0<>
      else false then
   while drop 1+ state SYNTAX-POSITION @ repeat drop ;

\ The offset in the query as written where token j starts.
: TOKEN-START ( ^syntax n -- n )
   {: state j:n :}
   state j TOKEN STR:DATA  state SYNTAX-SOURCE @ STR:DATA -
   state swap WRITTEN ;

\ The offset in the query as written where token j ends.
: TOKEN-END ( ^syntax n -- n )
   {: state j:n :}
   state j TOKEN {: t:str :}
   t STR:DATA  state SYNTAX-SOURCE @ STR:DATA -  t STR:LEN +
   state swap WRITTEN ;

\ The first token that starts at or after offset k, else the token count.
: TOKEN-AT ( ^syntax n -- n )
   {: state k:n :}
   state TOKENS 0 ?do
      state i TOKEN-START k >= if i unloop exit then
   loop state TOKENS ;

: OPENER? ( str -- bool )
   {: t:str :}
   t s" (" STR:=  t s" [" STR:= or  t s" {" STR:= or ;

: CLOSER? ( str -- bool )
   {: t:str :}
   t s" )" STR:=  t s" ]" STR:= or  t s" }" STR:= or ;

\ The closer of the innermost bracketed construct holding token j, past
\ depth closers of constructs opened after it, else the token count.
: CONSTRUCT-END ( ^syntax n n -- n )
   {: state j:n depth:n :}
   j state TOKENS = if j exit then
   state j TOKEN {: t:str :}
   t CLOSER? if
      depth 0= if j exit then
      state j 1+ depth 1- RECURSE exit
   then
   t OPENER? if state j 1+ depth 1+ RECURSE exit then
   state j 1+ depth RECURSE ;

\ The token after those from j, before limit, that spell the words of w in
\ any case, else -1.
: SPELLS ( ^syntax n n str -- n )
   {: state j:n limit:n w:str :}
   j limit >= if -1 exit then
   w s"  " 0 T-FROM {: s:n :}
   s 0< if w else w 0 s SUB$ then {: word:str :}
   state j TOKEN word STR:=CI 0= if -1 exit then
   s 0< if j 1+ exit then
   state j 1+ limit  w s 1+ FROM$  RECURSE ;

\ The token after the first item of the list t that the tokens from j,
\ before limit, spell, else -1.
: SPELLS-ITEM ( ^syntax n n str -- n )
   {: state j:n limit:n t:str :}
   t ITEM {: a:n rest:n :}
   state j limit  t 0 a SUB$  SPELLS {: e:n :}
   e 0< 0=  rest t STR:LEN =  or if e exit then
   state j limit  t rest FROM$  RECURSE ;

\ The first and after-last tokens of the earliest item of the list t spelled,
\ in any case, from offset k to the innermost closer holding k, else -1 -1.
: FIRST ( ^syntax n str -- n n )
   {: state k:n t:str :}
   state k TOKEN-AT {: from:n :}
   state from 0 CONSTRUCT-END {: limit:n :}
   limit from ?do
      state i limit t SPELLS-ITEM
      dup 0< 0= if i swap unloop exit then drop
   loop -1 -1 ;

\ The span in the query as written of what FIRST finds.
: EARLIEST ( ^syntax n str -- n n )
   {: state k:n t:str :}
   state k t FIRST {: a:n b:n :}
   a 0< if -1 -1 exit then
   state a TOKEN-START  state b 1- TOKEN-END ;

\ The span of the SEPARATOR keyword from offset k with the `=` and string
\ after it, else -1 -1.
: SEPARATOR ( ^syntax n -- n n )
   {: state k:n :}
   state k s" SEPARATOR" FIRST {: a:n b:n :}
   a 0< if -1 -1 exit then
   state a TOKEN-START  state b 1+ TOKEN-END ;

\ Whether token t starts a call's clause: `;`, SEPARATOR or ORDER.
: CLAUSE-START? ( str -- bool )
   {: t:str :}
   t s" ;" STR:=  t s" SEPARATOR" STR:=CI or  t s" ORDER" STR:=CI or ;

\ The token from j that ends a call's clause: the first `;`, SEPARATOR or
\ ORDER outside brackets opened after j, else the closer holding j.
: CLAUSE-END ( ^syntax n n -- n )
   {: state j:n depth:n :}
   j state TOKENS = if j exit then
   state j TOKEN {: t:str :}
   t CLOSER? if
      depth 0= if j exit then
      state j 1+ depth 1- RECURSE exit
   then
   t OPENER? if state j 1+ depth 1+ RECURSE exit then
   depth 0=  t CLAUSE-START?  and if j exit then
   state j 1+ depth RECURSE ;

\ The span of the call clause whose keyword w is spelled first from offset
\ k: a `;` just before it, w and what w takes; -1 -1 where none is.
: CALL-CLAUSE ( ^syntax n str -- n n )
   {: state k:n w:str :}
   state k w FIRST {: a:n b:n :}
   a 0< if -1 -1 exit then
   a 0 > if state a 1- TOKEN s" ;" STR:= else false then
   if a 1- else a then {: from:n :}
   state from TOKEN-START  state  state b 0 CLAUSE-END 1-  TOKEN-END ;

\ The token that starts the comparison ending before token j: back past
\ depth closers to a `,`, `||`, `&&` or an opener the comparison is in.
: OPERAND-START ( ^syntax n n -- n )
   {: state j:n depth:n :}
   j 0= if 0 exit then
   state j 1- TOKEN {: t:str :}
   t CLOSER? if state j 1- depth 1+ RECURSE exit then
   t OPENER? if
      depth 0= if j exit then
      state j 1- depth 1- RECURSE exit
   then
   t s" ," STR:=  t s" ||" STR:= or  t s" &&" STR:= or  depth 0= and if
      j exit then
   state j 1- depth RECURSE ;

\ The span of the comparison before the comparison operator at offset k.
: COMPARISON ( ^syntax n -- n n )
   {: state k:n :}
   state k TOKEN-AT {: op:n :}
   state  state op 0 OPERAND-START  TOKEN-START
   state op 1- TOKEN-END ;

\ Whether t joins two operands of an expression.
: INFIX? ( str -- bool )
   s" || && = != < > <= >= IN NOT + - * /" swap LISTS? ;

\ The token after the operand at token j: past a unary `!`, `-` or `+`, a
\ bracketed group, a call with its arguments, else the one token.
: OPERAND-END ( ^syntax n -- n )
   {: state j:n :}
   j 1+ state TOKENS < if
      s" ! - +" state j TOKEN LISTS? if state j 1+ RECURSE exit then
   then
   state j TOKEN OPENER? if state j 1+ 0 CONSTRUCT-END 1+ exit then
   j 1+ state TOKENS < if
      state j 1+ TOKEN s" (" STR:= if
         state j 2 + 0 CONSTRUCT-END 1+ exit
      then
   then j 1+ ;

\ The token after the expression at token j: operands joined by operators.
: EXPRESSION-END ( ^syntax n -- n )
   {: state j:n :}
   state j OPERAND-END  state TOKENS min {: e:n :}
   e state TOKENS = if e exit then
   state e TOKEN INFIX? 0= if e exit then
   state e 1+ RECURSE ;

\ The span of the condition that starts at offset k.
: CONDITION ( ^syntax n -- n n )
   {: state k:n :}
   state k TOKEN-AT {: j:n :}
   state j TOKEN-START  state  state j EXPRESSION-END 1-  TOKEN-END ;

\ Whether t is a keyword that starts a pattern other than triples.
: PATTERN-KEYWORD? ( str -- bool )
   s" FILTER BIND VALUES OPTIONAL MINUS GRAPH SERVICE" swap LISTS? ;

\ The first of the triples that end before token j: back past depth
\ closers to the opener of the group they are in, a } before them, or a
\ pattern's keyword at depth 0, whose pattern ends before token m, the
\ token after the last closer the walk met at depth 0.
: TRIPLES-START ( ^syntax n n n -- n )
   {: state j:n depth:n m:n :}
   j 0= if 0 exit then
   state j 1- TOKEN {: t:str :}
   depth 0=  t s" }" STR:= and if j exit then
   depth 0=  t PATTERN-KEYWORD? and if m exit then
   t CLOSER? if
      state j 1-  depth 1+  depth 0= if j else m then  RECURSE exit
   then
   t OPENER? if
      depth 0= if j exit then
      state j 1- depth 1- m RECURSE exit
   then state j 1- depth m RECURSE ;

\ The span of the triples that end before the token at offset k.
: TRIPLES-SPAN ( ^syntax n -- n n )
   {: state k:n :}
   state k TOKEN-AT {: j:n :}
   state  state j 0 j TRIPLES-START  TOKEN-START
   state j 1- TOKEN-END ;

\ What a fix's target t names: t without a leading `the `.
: NAMED ( str -- str )
   {: t:str :}
   t s" the " STR:STARTS-WITH? if t 4 FROM$ else t then ;

\ Whether a fix's target t names the found text.
: NAMES-FOUND? ( str str -- bool )
   {: t:str found:str :}
   t s" it" STR:=  t s" the character" STR:= or
   t s" the escape" STR:= or  t s" the blank node" STR:= or
   t NAMED found STR:= or ;

\ The keyword of the call clause a fix's target t names, the `=` of one
\ without its keyword, else empty.
: CLAUSE-KEYWORD ( str -- str )
   {: t:str :}
   t s" any ;, SEPARATOR, its = and its string" STR:=
   t s" the ;, the second SEPARATOR, its = and its string" STR:= or
   t s" the second SEPARATOR, its = and its string" STR:= or
   if s" SEPARATOR" exit then
   t s" the ;, its = and its string" STR:= if s" =" exit then
   t s" any ;, ORDER BY and its conditions" STR:= if
      s" ORDER BY" exit then
   s" " ;

\ The span from the token at offset k up to the closer holding it.
: UP-TO-CLOSER ( ^syntax n -- n n )
   {: state k:n :}
   state k TOKEN-AT {: j:n :}
   state j 0 CONSTRUCT-END {: e:n :}
   state j TOKEN-START  state e 1- TOKEN-END ;

\ The span of the tokens that spell w right before offset k, else -1 -1.
: PRECEDING ( ^syntax n str -- n n )
   {: state k:n w:str :}
   state k TOKEN-AT {: j:n :}
   j 0 ?do
      state i j w SPELLS j = if
         state i TOKEN-START  state j 1- TOKEN-END  unloop exit
      then
   loop -1 -1 ;

\ The span of the token at offset k that a found text cut short to
\ `head...` names, else -1 -1.
: CUT-FOUND ( ^syntax n str -- n n )
   {: state k:n found:str :}
   found 0 found STR:LEN 3 - SUB$ {: head:str :}
   state SYNTAX-ORIGINAL @ k head HOLDS-AT? 0= if -1 -1 exit then
   state k TOKEN-AT {: j:n :}
   j state TOKENS = if -1 -1 exit then
   state j TOKEN-START k <> if -1 -1 exit then
   k state j TOKEN-END ;

\ The span a fix's target t names at offset k: the found text, a call
\ clause, what follows k up to its closer, t's earliest item, else t's
\ text right before k; -1 -1 where the query has none.
: TARGET ( ^syntax n str str -- n n )
   {: state k:n found:str t:str :}
   t found NAMES-FOUND? if
      found s" ..." STR:ENDS-WITH? if state k found CUT-FOUND exit then
      state SYNTAX-ORIGINAL @ k found FOUND exit then
   t s" the ; and what follows it up to )" STR:= if
      state k UP-TO-CLOSER exit then
   t CLAUSE-KEYWORD {: w:str :}
   w STR:LEN 0<> if state k w CALL-CLAUSE exit then
   state k t NAMED EARLIEST {: a:n b:n :}
   a 0< 0= if a b exit then
   state k t NAMED PRECEDING ;

\ Whether the target t is a list whose first item is the found keyword.
: CLAUSE? ( str str -- bool )
   {: t:str found:str :}
   t ITEM drop {: a:n :}
   a t STR:LEN <  t 0 a SUB$ found STR:=CI  and ;

\ Where the first part of the list t, `X, Y and Z`, ends and where the rest
\ starts; both are t's length for the last part.
: PART ( str -- n n )
   {: t:str :}
   t s" , " 0 T-FROM {: c:n :}
   c 0< 0= if c c 2 + exit then
   t s"  and " 0 T-FROM {: a:n :}
   a 0< 0= if a a 5 + exit then
   t STR:LEN dup ;

\ The token after j when token j is an IRI or a prefixed name, or with var?
\ a variable; else j.
: IRI-END ( ^syntax n bool -- n )
   {: state j:n var?:bool :}
   j state TOKENS = if j exit then
   state j NTH-TOKEN TOKEN-KIND @ {: kind:n :}
   kind T-IRI =  kind T-NAME = or  var? kind T-VAR = and  or
   if j 1+ else j then ;

\ The token after part p from token j, at most the token count; -1 for a
\ part the follower does not know.
: PART-END ( ^syntax n str -- n )
   {: state j:n p:str :}
   p s" any " STR:STARTS-WITH? if
      state j  state TOKENS  p 4 FROM$  SPELLS
      dup 0< if drop j then exit then
   p s" its IRI" STR:= if state j false IRI-END exit then
   p s" its IRI or variable" STR:= if
      state j true IRI-END exit then
   p s" its group" STR:= 0= if -1 exit then
   j state TOKENS = if j exit then
   state j TOKEN s" {" STR:= 0= if j exit then
   state j 1+ 0 CONSTRUCT-END 1+ state TOKENS min ;

\ The token after the parts of the list t a clause holds from token j, else
\ -1 for a part the follower does not know.
: PARTS-END ( ^syntax n str -- n )
   {: state j:n t:str :}
   t PART {: a:n rest:n :}
   state j  t 0 a SUB$  PART-END {: e:n :}
   e 0<  rest t STR:LEN =  or if e exit then
   state e  t rest FROM$  RECURSE ;

\ The span of the clause the list t names from its keyword at offset k:
\ the keyword through the last part the query holds, else -1 -1.
: CLAUSE ( ^syntax n str -- n n )
   {: state k:n t:str :}
   state k TOKEN-AT {: j:n :}
   state j 1+  t  t ITEM nip FROM$  PARTS-END {: e:n :}
   e 0< if -1 -1 exit then
   state j TOKEN-START  state e 1- TOKEN-END ;

\ Appends q without the `\` before offset k.
: UNESCAPE ( str n ^VEC:vector<u8> -- )
   {: q:str k:n out :}
   q k 1- k out OMIT ;

\ Appends q without `PREFIX name <iri>`, the name from offset k up to e.
: UNDECLARE ( str n n ^VEC:vector<u8> -- )
   {: q:str k:n e:n out :}
   q  q k SPACE-BEFORE 6 -  q s" >" e T-FROM 1+  out
   OMIT ;

\ Appends the query without what `remove T[: why]` names at offset k: the
\ `\` before k, the prefix's declaration, the clause T lists, or T's text.
: REMOVE ( ^syntax n str str ^VEC:vector<u8> -- bool )
   {: state k:n found:str fix:str out :}
   state SYNTAX-ORIGINAL @ {: q:str :}
   fix 7 FROM$ {: rest:str :}
   rest s" :" 0 T-FROM {: c:n :}
   c 0< if rest else rest 0 c SUB$ then {: t:str :}
   t s" the backslash" STR:= if q k out UNESCAPE true exit then
   t s" this declaration" STR:= if
      q k  k found STR:LEN +  out UNDECLARE true exit then
   t found CLAUSE? if state k t CLAUSE else state k found t TARGET then
   {: a:n b:n :}
   q a b out CUT if true exit then
   fix CANNOT ;

\ Appends the query with what T of the fix `replace T with D` names at
\ offset k replaced by the token D stands for.
: REPLACE ( ^syntax n str str ^VEC:vector<u8> -- bool )
   {: state k:n found:str fix:str out :}
   fix s"  with " 8 T-FROM {: w:n :}
   state k found  fix 8 w SUB$  TARGET {: a:n b:n :}
   a 0< if fix CANNOT exit then
   state SYNTAX-ORIGINAL @ a b  fix w 6 + FROM$  out
   SUBSTITUTE ;

\ Appends q with its bytes from offset a up to offset b between the texts
\ o and c; for no span, a negative a, fails the case naming the fix.
: WRAP ( str n n str str str ^VEC:vector<u8> -- bool )
   {: q:str a:n b:n o:str c:str fix:str out :}
   a 0< if fix CANNOT exit then
   q a out KEEP-HEAD  o out VEC:APPEND
   q a b SUB$ out VEC:APPEND  c out VEC:APPEND
   q b out KEEP-TAIL  true ;

: ENCLOSE ( str n n str ^VEC:vector<u8> -- bool )
   {: q:str a:n b:n fix:str out :}
   q a b s" (" s" )" fix out WRAP ;

: WRITE? ( str -- bool )
   {: fix:str :}
   fix s" write " STR:STARTS-WITH?  fix s"  here" STR:ENDS-WITH? and ;

\ D of a fix `write D after X` without `, as in`, and whether the follower
\ knows what D stands for.
: AFTER-TOKEN ( str -- str bool )
   {: fix:str :}
   fix s"  after " 0 T-FROM {: a:n :}
   fix s" write " STR:STARTS-WITH? 0=  a 0< or
   fix s" , as in" STR:CONTAINS? or if s" " false exit then
   fix 6 a SUB$ {: d:str :}
   d  d TOKEN-LIST?  d CONCRETE nip  or ;

\ Appends q after `PREFIX p: <urn:x:> `, p the prefix the fix declares.
: DECLARE ( str str ^VEC:vector<u8> -- )
   {: q:str fix:str out :}
   fix 16  fix STR:LEN 5 -  SUB$ out VEC:APPEND
   s" <urn:x:> " out VEC:APPEND  q out VEC:APPEND ;

\ The quote, one or three bytes, opening the string after the tokens.
: OPENING ( ^syntax -- str )
   {: state :}
   state SYNTAX-ORIGINAL @ {: q:str :}
   state TOKENS dup 0 > if state swap 1- TOKEN-END then
   q swap RESUME {: a:n :}
   q a  a 1+ SUB$ {: c:str :}
   q a 1+ c HOLDS-AT?  q a 2 + c HOLDS-AT?  and if
      q a  a 3 + SUB$ exit then
   c ;

\ Appends q with the fix's scheme written after the `<` found at k.
: SCHEME ( str n str str ^VEC:vector<u8> -- bool )
   {: q:str k:n found:str fix:str out :}
   q k found FOUND drop {: a:n :}
   a 0< if fix CANNOT exit then
   fix s" such as " 0 T-FROM 8 + {: s:n :}
   q a 1+  fix s FROM$  out PUT  true ;

\ The last token before j that spells w in any case, else -1.
: LAST-SPELLED ( ^syntax n str -- n )
   {: state j:n w:str :}
   j 0 ?do
      state j 1- i - TOKEN w STR:=CI if j 1- i - unloop exit then
   loop -1 ;

\ The opener of the construct that ends before token j, past depth
\ closers of constructs it holds, else -1.
: OPENING-TOKEN ( ^syntax n n -- n )
   {: state j:n depth:n :}
   j 0= if -1 exit then
   state j 1- TOKEN {: t:str :}
   t OPENER? if
      depth 1 = if j 1- exit then
      state j 1- depth 1- RECURSE exit
   then
   t CLOSER? if state j 1- depth 1+ RECURSE exit then
   state j 1- depth RECURSE ;

\ The span of the OPTIONAL, MINUS, GRAPH or SERVICE pattern that ends
\ before the token at offset k, else -1 -1.
: PATTERN-SPAN ( ^syntax n -- n n )
   {: state k:n :}
   state k TOKEN-AT {: j:n :}
   state j 0 OPENING-TOKEN {: o:n :}
   o 0< if -1 -1 exit then
   state o s" OPTIONAL" LAST-SPELLED  state o s" MINUS" LAST-SPELLED max
   state o s" GRAPH" LAST-SPELLED max  state o s" SERVICE" LAST-SPELLED max
   {: w:n :}
   w 0< if -1 -1 exit then
   state w TOKEN-START  state j 1- TOKEN-END ;

\ After the BY of the first depth-0 GROUP BY from j, before depth -1.
: GROUP-BY-END ( ^syntax n n -- n )
   {: state j:n depth:n :}
   j state TOKENS =  depth 0<  or if -1 exit then
   depth 0= if
      state j  state TOKENS  s" GROUP BY" SPELLS
      dup 0< 0= if exit then drop
   then
   state j TOKEN {: t:str :}
   t OPENER? if state j 1+ depth 1+ RECURSE exit then
   t CLOSER? if state j 1+ depth 1- RECURSE exit then
   state j 1+ depth RECURSE ;

\ Appends the query with the fix's ` ?v` after k's SELECT's GROUP BY.
: REGROUP ( ^syntax n str ^VEC:vector<u8> -- bool )
   {: state k:n fix:str out :}
   state  state k TOKEN-AT  s" SELECT" LAST-SPELLED {: s:n :}
   s 0< if fix CANNOT exit then
   state s 1+ 0 GROUP-BY-END {: e:n :}
   e 0< if fix CANNOT exit then
   state SYNTAX-ORIGINAL @  state e 1- TOKEN-END
   fix 3  fix s"  to " 0 T-FROM  SUB$  out PUT  true ;

\ Appends q with w and bytes a to b as `WT; ` at o, bytes c to b cut;
\ o<=c<=a.
: MOVE-BEFORE ( str str n n n n ^VEC:vector<u8> -- )
   {: q:str w:str o:n c:n a:n b:n out :}
   q o out KEEP-HEAD  w out VEC:APPEND  q a b SUB$ out VEC:APPEND
   s" ; " out VEC:APPEND
   q o c SUB$ out VEC:APPEND  q b out KEEP-TAIL ;

\ The span of the `=` at offset k and the string after it, else -1 -1.
: EQUALS-STRING ( ^syntax n -- n n )
   {: state k:n :}
   state k TOKEN-AT {: j:n :}
   j 1+ state TOKENS < if state j TOKEN s" =" STR:= else false then
   0= if -1 -1 exit then
   state j TOKEN-START  state j 1+ TOKEN-END ;

\ Appends the query with `SEPARATOR = string` at k, and any `;` just
\ before it, moved before ORDER; SEPARATOR is written where k is the `=`.
: SEPARATE ( ^syntax n str ^VEC:vector<u8> -- bool )
   {: state k:n fix:str out :}
   state k EQUALS-STRING {: e:n f:n :}
   e 0< if state k SEPARATOR else e f then {: a:n b:n :}
   e 0< if s" " else s" SEPARATOR " then {: w:str :}
   state k TOKEN-AT 1- {: j:n :}
   state j s" ORDER" LAST-SPELLED {: o:n :}
   a 0<  o 0< or if fix CANNOT exit then
   state j TOKEN s" ;" STR:= if state j TOKEN-START else a then {: c:n :}
   state SYNTAX-ORIGINAL @ w  state o TOKEN-START  c  a b
   out MOVE-BEFORE  true ;

\ Appends the query without `ORDER BY` at k, and the `;` just before it,
\ joining two ORDER BYs.
: JOIN-ORDER ( ^syntax n str ^VEC:vector<u8> -- bool )
   {: state k:n fix:str out :}
   state k TOKEN-AT {: o:n :}
   state o  state TOKENS  s" ORDER BY" SPELLS {: e:n :}
   o 1 <  e 0< or if fix CANNOT exit then
   state o 1- TOKEN s" ;" STR:= if o 1- else o then {: j:n :}
   state SYNTAX-ORIGINAL @  state j TOKEN-START  state e 1- TOKEN-END
   out OMIT  true ;

\ Appends q with its bytes from offset a up to offset b replaced by t;
\ for no span, a negative a, fails the case naming the fix.
: RESPELL ( str n n str str ^VEC:vector<u8> -- bool )
   {: q:str a:n b:n t:str fix:str out :}
   a 0< if fix CANNOT exit then
   q a out KEEP-HEAD  t out VEC:APPEND  q b out KEEP-TAIL  true ;

\ The X of a fix ending `, as in X`, and whether it has one.
: EXAMPLE ( str -- str bool )
   {: fix:str :}
   fix s" , as in " 0 T-FROM {: j:n :}
   j 0< if s" " false exit then
   fix j 8 + FROM$ true ;

\ Appends q with the found text at k replaced by the example x, which
\ starts with its first byte, else fails the case naming the fix.
: EXEMPLIFY ( str n str str str ^VEC:vector<u8> -- bool )
   {: q:str k:n found:str x:str fix:str out :}
   q k found FOUND {: a:n b:n :}
   a 0< if fix CANNOT exit then
   x  q a  a 1+ b min  SUB$  STR:STARTS-WITH? 0= if
      fix CANNOT exit then
   q a b x fix out RESPELL ;

\ The first item of the list t.
: FIRST-ITEM ( str -- str )
   {: t:str :}
   t ITEM drop {: a:n :}
   t 0 a SUB$ ;

\ The Y of a fix `write X as Y` up to ` or `, and whether it has one.
: WRITE-AS ( str -- str bool )
   {: fix:str :}
   fix s"  as " 0 T-FROM {: j:n :}
   fix s" write " STR:STARTS-WITH? 0=  j 0< or if s" " false exit then
   fix j 4 + FROM$ FIRST-ITEM true ;

\ The text a fix writes in place of the found text, and whether the fix
\ is one that does.
: RESPELLING ( str -- str bool )
   {: fix:str :}
   fix s" write at most " STR:STARTS-WITH? if fix 14 FROM$ true exit then
   fix s" start the query with " STR:STARTS-WITH? if
      fix 21 FROM$ FIRST-ITEM true exit then
   fix 4 FROM$ {: w:str :}
   fix s" use " STR:STARTS-WITH?  w s"  " STR:CONTAINS? 0=  and if
      w true exit then
   fix WRITE-AS ;

\ Whether the fix asks for a variable in place of the found text.
: PASS-VARIABLE? ( str -- bool )
   {: fix:str :}
   fix s" pass " STR:STARTS-WITH?  fix s"  a variable" STR:ENDS-WITH? and ;

\ Appends q with its bytes from offset a up to offset b replaced by a
\ variable q does not use, else fails the case naming the fix.
: REVARIABLE ( str n n str ^VEC:vector<u8> -- bool )
   {: q:str a:n b:n fix:str out :}
   a 0< if fix CANNOT exit then
   q a b s" a variable" out SUBSTITUTE ;

\ Appends q with the found text at k cut after its first colon, else fails
\ the case naming the fix.
: UNSUFFIX ( str n str str ^VEC:vector<u8> -- bool )
   {: q:str k:n found:str fix:str out :}
   q k found FOUND {: a:n b:n :}
   a 0< if fix CANNOT exit then
   q s" :" a T-FROM 1+ {: c:n :}
   c 1 <  c b < 0= or if fix CANNOT exit then
   q c b out OMIT  true ;

\ Where the blanks of q from offset k end.
: BLANKS-END ( str n -- n )
   {: q:str k:n :}
   k q STR:LEN >= if k exit then
   q STR:DATA k + c@ WHITE? 0= if k exit then
   q k 1+ RECURSE ;

\ Appends q without the blanks after the sign found at k, else fails the
\ case naming the fix.
: JOIN-SIGN ( str n str str ^VEC:vector<u8> -- bool )
   {: q:str k:n found:str fix:str out :}
   q k found FOUND {: a:n b:n :}
   a 0< if fix CANNOT exit then
   q b BLANKS-END {: e:n :}
   e b = if fix CANNOT exit then
   q b e out OMIT  true ;

\ Whether token j of a VALUES row starts a value: it is no datatype or
\ language tag, and follows no `^^` or sign.
: VALUE-TOKEN? ( ^syntax n -- bool )
   {: state j:n :}
   state j TOKEN {: t:str :}
   state j NTH-TOKEN TOKEN-KIND @ T-LANG =
   t s" ^^" STR:= or if false exit then
   state j 1- TOKEN {: p:str :}
   p s" ^^" STR:=  p s" +" STR:= or  p s" -" STR:= or 0= ;

\ The values of a VALUES row in tokens j up to limit.
: VALUE-COUNT ( ^syntax n n -- n )
   {: state j:n limit:n :}
   0 limit j ?do state i VALUE-TOKEN? if 1+ then loop ;

\ The token where value m, counting from 0, of tokens from j starts.
: VALUE-AT ( ^syntax n n -- n )
   {: state j:n m:n :}
   state j VALUE-TOKEN? 0= if state j 1+ m RECURSE exit then
   m 0= if j exit then
   state j 1+ m 1- RECURSE ;

\ The `)` closing the row that opens at token j, else -1.
: ROW-END ( ^syntax n -- n )
   {: state j:n :}
   j state TOKENS = if -1 exit then
   state j TOKEN s" (" STR:= 0= if -1 exit then
   state j 1+ 0 CONSTRUCT-END {: e:n :}
   e state TOKENS = if -1 exit then
   state e TOKEN s" )" STR:= if e else -1 then ;

\ The variables of the VALUES block holding the row that opens at token
\ j, else -1 where they are not in parentheses.
: BLOCK-WIDTH ( ^syntax n -- n )
   {: state j:n :}
   state j s" {" LAST-SPELLED 1- {: c:n :}
   c 0< if -1 exit then
   state c TOKEN s" )" STR:= 0= if -1 exit then
   c  state c s" (" LAST-SPELLED -  1- ;

\ The span of the row of values that starts at offset k, up to the first
\ bracket after it.
: ROW-SPAN ( ^syntax n -- n n )
   {: state k:n :}
   state k TOKEN-AT {: j:n :}
   j 1+ begin
      dup state TOKENS < if
         state over TOKEN  dup OPENER?  swap CLOSER?  or 0=
      else false then
   while 1+ repeat {: e:n :}
   state j TOKEN-START  state e 1- TOKEN-END ;

\ Appends q with c UNDEFs written at offset k.
: UNDEFS ( str n n ^VEC:vector<u8> -- )
   {: q:str k:n c:n out :}
   q k out KEEP-HEAD
   c 0 ?do s"  UNDEF" out VEC:APPEND loop
   q k out KEEP-TAIL ;

\ Appends the query with the row of tokens j to its closer e given w
\ values: UNDEFs for those missing, the extra ones removed.
: RESIZE ( ^syntax n n n ^VEC:vector<u8> -- )
   {: state j:n e:n w:n out :}
   state SYNTAX-ORIGINAL @ {: q:str :}
   state j 1+ e VALUE-COUNT {: c:n :}
   c w < if q  state e TOKEN-START  w c -  out UNDEFS exit then
   q  state  state j 1+ w VALUE-AT 1-  TOKEN-END
   state e TOKEN-START  out OMIT ;

\ Appends the query with the row holding offset k given one value or
\ UNDEF per variable of its block, else fails the case naming the fix.
: REFILL ( ^syntax n str ^VEC:vector<u8> -- bool )
   {: state k:n fix:str out :}
   state  state k TOKEN-AT  s" (" LAST-SPELLED {: j:n :}
   j 0< if fix CANNOT exit then
   state j ROW-END {: e:n :}
   e 0< if fix CANNOT exit then
   state j BLOCK-WIDTH {: w:n :}
   w 0<  state j 1+ e VALUE-COUNT w =  or if fix CANNOT exit then
   state j e w out RESIZE  true ;

\ Appends q with the } that ends the block holding the token at offset k
\ moved before that token as `} `; where a ) or ] ends it or nothing
\ does, fails the case naming the fix.
: RECLOSE ( ^syntax n str ^VEC:vector<u8> -- bool )
   {: state k:n fix:str out :}
   state SYNTAX-ORIGINAL @ {: q:str :}
   state  state k TOKEN-AT  0 CONSTRUCT-END {: e:n :}
   e state TOKENS = if fix CANNOT exit then
   state e TOKEN s" }" STR:= 0= if fix CANNOT exit then
   q k out KEEP-HEAD  s" } " out VEC:APPEND
   q k  state e TOKEN-START  SUB$ out VEC:APPEND
   q  state e TOKEN-END  out KEEP-TAIL  true ;

\ Appends the query with the fix of its rejection's first line r applied;
\ false ends the chain, failing the case at a fix UNEDITED? does not hold.
: FIX! ( ^syntax str ^VEC:vector<u8> -- bool )
   {: state r:str out :}
   state SYNTAX-ORIGINAL @ {: q:str :}
   q r FIX-PLACE {: k:n :}
   r FIX-PARTS {: found:str fix:str :}
   fix s" remove the text from here on" STR:= if
      q k out KEEP-HEAD  true exit then
   fix s" end the triple before with ." STR:= if
      q k s" ." out INSERT exit then
   fix s" write , before it" STR:= if
      q k s" ," out INSERT exit then
   fix s" put the condition in parentheses" STR:= if
      q  state k CONDITION  fix out ENCLOSE exit then
   fix s" put the row in parentheses" STR:= if
      q  state k ROW-SPAN  fix out ENCLOSE exit then
   fix s" put the first comparison in parentheses" STR:= if
      q  state k COMPARISON  fix out ENCLOSE exit then
   fix s" put the triples before it in braces" STR:= if
      q  state k TRIPLES-SPAN  s" { " s"  }" fix out WRAP exit then
   fix s" put the pattern before it in braces" STR:= if
      q  state k PATTERN-SPAN  s" { " s"  }" fix out WRAP exit then
   fix s" write the pattern as WHERE { ... }" STR:= if
      q k s" WHERE { }" out SPACED  true exit then
   fix s" write WHERE { here" STR:= if
      q k s" WHERE {" out SPACED  true exit then
   fix WRITE? if
      q k  fix 6  fix STR:LEN 5 -  SUB$  out INSERT
      exit then
   fix s" replace " STR:STARTS-WITH? if
      state k found fix out REPLACE exit then
   fix s" remove " STR:STARTS-WITH? if
      state k found fix out REMOVE exit then
   fix AFTER-TOKEN {: t:str after:bool :}
   after if q k t out INSERT exit then
   fix s" write * or the variables to select" STR:= if
      q k s" *" out INSERT exit then
   fix s" declare it with PREFIX " STR:STARTS-WITH? if
      q fix out DECLARE  true exit then
   fix s" close the string with the quote that opens it" STR:= if
      q k state OPENING out PUT  true exit then
   fix s" start the IRI with a scheme such as " STR:STARTS-WITH? if
      q k found fix out SCHEME exit then
   fix s" add " STR:STARTS-WITH?
   fix s"  to GROUP BY or wrap it in SAMPLE(" STR:CONTAINS? and if
      state k fix out REGROUP exit then
   fix s" write SEPARATOR before ORDER BY" STR:=
   fix s" write SEPARATOR before the =, then move SEPARATOR, the = and its string before ORDER BY"
   STR:= or if
      state k fix out SEPARATE exit then
   fix s" write the conditions in one ORDER BY" STR:= if
      state k fix out JOIN-ORDER exit then
   fix EXAMPLE {: x:str example:bool :}
   example if q k found x fix out EXEMPLIFY exit then
   fix PASS-VARIABLE? if
      q  q k found FOUND  fix out REVARIABLE exit then
   fix RESPELLING {: t:str respelt:bool :}
   respelt if q  q k found FOUND  t fix out RESPELL exit then
   fix s" end the prefix name at its colon" STR:= if
      q k found fix out UNSUFFIX exit then
   fix s" write the number right after the sign" STR:= if
      q k found fix out JOIN-SIGN exit then
   fix s" write one value or UNDEF per variable in each row" STR:= if
      state k fix out REFILL exit then
   fix s" move the } that ends this block here" STR:= if
      state k fix out RECLOSE exit then
   fix UNEDITED? if false exit then
   fix CANNOT ;

\ q's tokens; where the lexer rejects q, those before the rejected one.
: LEXED ( str ^ARENA:arena -- ^syntax )
   {: q:str arena :}
   q arena SYNTAX-NEW {: state :}
   [: state LEX ;] catch {: code:n :}
   code 0<>  code E-QUERY <>  and if code throw then
   state ;

\ The stage that rejects q, which does not parse: 1 the lexer, 2 the
\ parser, 3 the validator.
: STAGE ( str ^ARENA:arena -- n )
   {: q:str arena :}
   q arena SYNTAX-NEW {: lexed :}
   [: lexed LEX ;] catch {: a:n :}
   a 0<>  a E-QUERY <>  and if a throw then
   a 0<> if 1 exit then
   q arena SYNTAX-NEW {: read :}
   [: read READ-QUERY ;] catch {: b:n :}
   b 0<>  b E-QUERY <>  and if b throw then
   b 0<> if 2 else 3 then ;

\ How many bytes a and b end with in common.
: SHARED-TAIL ( str str -- n )
   {: a:str b:str :}
   a STR:LEN b STR:LEN min {: most:n :}
   most 0 ?do
      a STR:DATA a STR:LEN + i - 1- c@
      b STR:DATA b STR:LEN + i - 1- c@ <> if i unloop exit then
   loop most ;

\ How many bytes a and b start with in common, at most n.
: SHARED-HEAD ( str str n -- n )
   {: a:str b:str most:n :}
   most 0 ?do
      a STR:DATA i + c@  b STR:DATA i + c@ <> if i unloop exit then
   loop most ;

\ Where f stands in p among the bytes the fix wrote, -1 when the fix
\ wrote no f: it replaced q's bytes between the head and the last t
\ bytes p shares with q.
: KEPT-AT ( str str str n -- n )
   {: q:str p:str f:str t:n :}
   q p  q STR:LEN p STR:LEN min t -  SHARED-HEAD {: h:n :}
   p h  p STR:LEN t -  SUB$  f 0 T-FROM {: i:n :}
   i 0< if -1 else h i + then ;

\ Whether p's rejection at offset j is past the found text f, which the
\ fix kept at offset k of q writing only before it; at it is enough when
\ f ends a construct and the fix wrote something.
: PAST-KEPT? ( str str str n n -- bool )
   {: q:str p:str f:str k:n j:n :}
   k  p STR:LEN +  q STR:LEN - {: at:n :}
   f STR:LEN 0=  f CLOSER? or  at k > and if j at >= else j at > then ;

\ Whether p's rejection s at offset j finds a closer among the bytes the
\ fix wrote at offset k of q, writing only there.
: WRITTEN-CLOSER? ( str str str n n -- bool )
   {: q:str p:str s:str k:n j:n :}
   s FIX-PARTS drop CLOSER? 0= if false exit then
   q p  k p STR:LEN min  SHARED-HEAD k <> if false exit then
   j k >=  j  k p STR:LEN + q STR:LEN -  <  and ;

\ Whether the rejection s of p, which a fix of q's rejection r made,
\ comes past the found token the fix kept, or at it as PAST-KEPT? allows,
\ or at a closer the fix wrote before it, or not before where the token
\ stood when the fix removed or replaced it.
: LATER? ( str str str str -- bool )
   {: q:str r:str p:str s:str :}
   q r FIX-PLACE {: k:n :}
   p s FIX-PLACE {: j:n :}
   r FIX-PARTS drop {: f:str :}
   q p SHARED-TAIL {: t:n :}
   q STR:LEN t -  k <= if
      q p f k j PAST-KEPT?  q p s k j WRITTEN-CLOSER?  or exit
   then
   q p f t KEPT-AT {: at:n :}
   at 0< if j k >= else j at > then ;

\ Whether the fix that made p of q, rejected with r, removed its mistake:
\ p parses, a later stage rejects it, or the stage that rejected q or an
\ earlier one rejects it LATER?.
: REMOVED? ( str str str ^ARENA:arena -- bool )
   {: q:str r:str p:str arena :}
   p REJECTION arena ARENA:COPY {: s:str :}
   s STR:LEN 0= if true exit then
   p arena STAGE  q arena STAGE  > if true exit then
   q r p s LATER? ;

\ How many fixes q's chain takes, k fixes in, logging each first line;
\ MOST-FIXES and one where fixes still apply after MOST-FIXES. q is lexed
\ first and r copied, as a lexer rejection or a fix's trial parse
\ replaces the text r is in. A fix that leaves its mistake fails the case.
: CHAIN ( str n ^VEC:vector<u8> ^ARENA:arena -- n )
   {: q:str k:n log arena :}
   q arena LEXED {: state :}
   q REJECTION arena ARENA:COPY {: r:str :}
   r STR:LEN 0= if k exit then
   r log VEC:APPEND  s\" \n" log VEC:APPEND
   k MOST-FIXES = if k 1+ exit then
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   state r out FIX! 0= if k 1+ exit then
   q r  out VEC:BYTES  arena REMOVED? 0= if
      s" a fix leaves its mistake; each step's rejection and the fixed query:"
      T-ASSERT-DETAIL
      log VEC:BYTES type  out VEC:BYTES type cr  k 1+ exit
   then
   out VEC:BYTES k 1+ log arena RECURSE ;

\ q's chain takes n fixes; a case CHAIN failed is not failed again.
: FOLLOW ( str n ^ARENA:arena -- )
   {: q:str want:n arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: log :}
   T-NEXT
   T-FAILURES {: failed:n :}
   q 0 log arena CHAIN {: got:n :}
   T-FAILURES failed > if exit then
   got MOST-FIXES > if
      s" fixes still apply after 32; each step's rejection:" T-ASSERT-DETAIL
      log VEC:BYTES type exit
   then
   got want = if exit then
   s" the chain takes another count of fixes; each step's rejection:"
   T-ASSERT-DETAIL
   log VEC:BYTES type ;

\ The query is rejected with exactly the text, and its chain takes n
\ fixes, one per mistake it holds.
: REJECTS-FIXES ( str str n -- )
   {: query:str want:str n:n :}
   [: query [: PARSE-IN ;] ARENA:WITH ;] E-QUERY TTHROWSQ
   E-QUERY REASON:TEXT$ want T$=
   query n [: FOLLOW ;] ARENA:WITH ;

\ The query, holding one mistake, is rejected with exactly the text, and
\ its chain takes one fix.
: REJECTS ( str str -- )
   1 REJECTS-FIXES ;

: REJECTIONS ( -- )
   s" a rejection shows its place, what was expected and the fix" T-LABEL
   \ On line 1.
   s" SELECT ?s WHERE { ?s ?p }"
   s\" 1:25: expected a variable, IRI, prefixed name, literal or blank node, found `}`; write a term such as ?x or <iri> here\n1 | SELECT ?s WHERE { ?s ?p }\n                            ^"
      REJECTS
   \ On a later line.
   s\" SELECT ?s\nWHERE { ?s ?p ?o\n  FILTER(?s = ) }"
   s\" 3:15: expected a variable, IRI, prefixed name or literal, found `)`; write a term such as ?x or <iri> here\n3 |   FILTER(?s = ) }\n                  ^"
      REJECTS
   \ Found cut at REASON:QUOTE-MOST bytes.
   s" SELECT * WHERE { ?s ?p ?o } <http://example.org/a-very-long-iri-name/x>"
   s\" 1:29: expected the end of the query, found `<http://example.org/a-very-long-...`; remove it\n1 | SELECT * WHERE { ?s ?p ?o } <http://example.org/a-very-long-iri-name/x>\n                                ^"
      REJECTS
   \ At the end of the query.
   s" SELECT * WHERE { ?s ?p ?o"
   s\" 1:26: expected }, found the end of the query; write } here\n1 | SELECT * WHERE { ?s ?p ?o\n                             ^"
      REJECTS
   \ A line over 80 bytes, cut on both sides.
   s" SELECT * WHERE { ?subject0123456789 <http://example.org/predicate/one/two/three> ?o2 ?o3 <http://example.org/more/stuff/here/and/there> }"
   s\" 1:86: expected . or } after a triple, found `?o3`; end the triple before with .\n1 | ...xample.org/predicate/one/two/three> ?o2 ?o3 <http://example.org/more/stuff/here/...\n                                               ^"
      2 REJECTS-FIXES
   \ At a lexer byte.
   s\" SELECT * WHERE { ?s ?p \"abc }"
   s\" 1:29: expected the string's closing quote, found `}`; close the string with the quote that opens it\n1 | SELECT * WHERE { ?s ?p \"abc }\n                                ^"
      REJECTS
   \ A long string no quote closes.
   s\" ASK { ?s ?p \"\"\"a }"
   s\" 1:18: expected the string's closing quote, found `}`; close the string with the quote that opens it\n1 | ASK { ?s ?p \"\"\"a }\n                     ^"
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
   s\" 1:14: expected a hex digit (\\u takes 4, \\U 8), found `\\u12`; write \\u with 4 hex digits, as in \\u00E9\n1 | ASK { ?s ?p \"\\u12\" }\n                 ^"
      REJECTS
   \ An escaped surrogate half.
   s\" ASK { ?s ?p \"\\uD800\" }"
   s\" 1:14: expected a Unicode scalar value: at most 10FFFF, not D800 to DFFF, found `\\uD800`; write the character itself or one \\U escape, as in \\U0001F600\n1 | ASK { ?s ?p \"\\uD800\" }\n                 ^"
      REJECTS
   \ A bad escape after good ones, placed as written.
   s\" ASK { ?s ?p \"\\u00E9\\U0001F600\\uD800\" }"
   s\" 1:30: expected a Unicode scalar value: at most 10FFFF, not D800 to DFFF, found `\\uD800`; write the character itself or one \\U escape, as in \\U0001F600\n1 | ASK { ?s ?p \"\\u00E9\\U0001F600\\uD800\" }\n                                 ^"
      REJECTS
   \ A percent escape without two hex digits.
   s\" ASK { ?s ?p ex:a%2x }"
   s\" 1:17: expected a hex digit after %, found `%`; write % as \\% or follow it with two hex digits\n1 | ASK { ?s ?p ex:a%2x }\n                    ^"
      2 REJECTS-FIXES
   \ A string escape SPARQL lacks.
   s\" ASK { ?s ?p \"a\\q\" }"
   s\" 1:15: expected an escape: \\t \\b \\n \\r \\f \\\" \\' or \\\\, found `\\`; write a backslash as \\\\\n1 | ASK { ?s ?p \"a\\q\" }\n                  ^"
      REJECTS
   \ A name right after a number.
   s\" ASK { ?s ?p 1ex }"
   s\" 1:14: expected . or } after a triple, found `ex`; remove it\n1 | ASK { ?s ?p 1ex }\n                 ^"
      REJECTS
   \ A variable name starting with a symbol.
   s\" ASK { ?× ?p ?o }"
   s\" 1:7: expected a variable name after ?, found `?×`; write a name after ?, as in ?x\n1 | ASK { ?× ?p ?o }\n          ^"
      REJECTS
   \ The same after $.
   s\" ASK { $× ?p ?o }"
   s\" 1:7: expected a variable name after $, found `$×`; write a name after $, as in $x\n1 | ASK { $× ?p ?o }\n          ^"
      REJECTS
   \ A local name escape of a letter.
   s\" ASK { ?s ?p ex:a\\q }"
   s\" 1:18: expected a character a local name escapes: _~.-!$&'()*+,;=/?#@%, found `q`; remove the backslash\n1 | ASK { ?s ?p ex:a\\q }\n                     ^"
      2 REJECTS-FIXES
   \ A prefix ending in a dot.
   s\" PREFIX ex: <urn:x:> ASK { ?s ex.:p ?o }"
   s\" 1:30: expected a prefix that does not end in ., found `ex.:p`; replace it with ex:p\n1 | PREFIX ex: <urn:x:> ASK { ?s ex.:p ?o }\n                                 ^"
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
   s\" 1:15: expected the closing quote before the line ends, found `U+000DU+000A`; write the line break as \\n\n1 | ASK { ?s ?p \"a\n                  ^"
      REJECTS
   \ A line break in a string no quote closes.
   s\" ASK { ?s ?p \"a\n}"
   s\" 1:15: expected the string's closing quote, found `U+000A`; close the string with the quote that opens it\n1 | ASK { ?s ?p \"a\n                  ^"
      REJECTS
   \ A blank node without a label.
   s\" ASK { ?s ?p _: }"
   s\" 1:13: expected a blank node label after _:, found `_:`; write a label after _:, as in _:b1\n1 | ASK { ?s ?p _: }\n                ^"
      REJECTS
   \ An escape in a prefix.
   s\" ASK { ?s e\\-x:p ?o }"
   s\" 1:11: expected a name character; escapes go only after the colon, found `\\`; remove the escape\n1 | ASK { ?s e\\-x:p ?o }\n              ^"
      2 REJECTS-FIXES
   \ A prefix starting with a low line.
   s\" ASK { ?s _ex:p ?o }"
   s\" 1:10: expected a name character, found `_`; remove the character\n1 | ASK { ?s _ex:p ?o }\n             ^"
      2 REJECTS-FIXES
   \ A language tag ending in a minus.
   s\" ASK { ?s ?p \"a\"@en- }"
   s\" 1:16: expected a letter or digit after - in a language tag, found `@en-`; follow the - with letters or digits, as in @en-GB\n1 | ASK { ?s ?p \"a\"@en- }\n                   ^"
      REJECTS
   \ A name starting with a symbol.
   s\" ASK { ?s ?p × }"
   s\" 1:13: expected a keyword, name, variable, IRI, literal or punctuation, found `×`; remove the character\n1 | ASK { ?s ?p × }\n                ^"
      2 REJECTS-FIXES
   \ A language tag starting with a digit.
   s\" ASK { ?s ?p \"a\"@1 }"
   s\" 1:16: expected a letter after @, found `@1`; write a language tag after @, as in @en\n1 | ASK { ?s ?p \"a\"@1 }\n                   ^"
      REJECTS
   \ A character no token starts with.
   s\" ASK { ?s ?p ~ }"
   s\" 1:13: expected a keyword, name, variable, IRI, literal or punctuation, found `~`; remove the character\n1 | ASK { ?s ?p ~ }\n                ^"
      2 REJECTS-FIXES
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
   s\" 2:28: expected . or } after a triple, found `?x`; write , before it\n2 | WHERE { ?s <urn:\\u0041> ?o ?x }\n                               ^"
      REJECTS
   \ From parse.f.
   s" SELEKT * WHERE { ?s ?p ?o }"
   s\" 1:1: expected SELECT or ASK, found `SELEKT`; start the query with SELECT or ASK\n1 | SELEKT * WHERE { ?s ?p ?o }\n    ^"
      REJECTS
   s" SELECT WHERE { ?s ?p ?o }"
   s\" 1:8: expected *, a variable or (expression AS ?name), found `WHERE`; write * or the variables to select\n1 | SELECT WHERE { ?s ?p ?o }\n           ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY LIMIT 1"
   s\" 1:39: expected a variable or expression to group by, found `LIMIT`; write ?s after GROUP BY\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY LIMIT 1\n                                          ^"
      REJECTS
   s" SELECT (COUNT(*) AS ?n) WHERE { } GROUP BY LIMIT 1"
   s\" 1:44: expected a variable or expression to group by, found `LIMIT`; remove GROUP BY\n1 | SELECT (COUNT(*) AS ?n) WHERE { } GROUP BY LIMIT 1\n                                               ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s HAVING LIMIT 1"
   s\" 1:49: expected a condition in parentheses or a function call, found `LIMIT`; write a condition in parentheses after HAVING\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s HAVING LIMIT 1\n                                                    ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } ORDER BY LIMIT 1"
   s\" 1:39: expected a variable or expression to order by, found `LIMIT`; write the variables to order by after ORDER BY\n1 | SELECT ?s WHERE { ?s ?p ?o } ORDER BY LIMIT 1\n                                          ^"
      REJECTS
   \ An ORDER followed by a clause, a closer or the end.
   s" SELECT * WHERE { ?s ?p ?o } ORDER LIMIT 1"
   s\" 1:35: expected BY, found `LIMIT`; write BY and the variables to order by after ORDER\n1 | SELECT * WHERE { ?s ?p ?o } ORDER LIMIT 1\n                                      ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o } ORDER FOO"
   s\" 1:35: expected BY, found `FOO`; replace it with BY\n1 | SELECT * WHERE { ?s ?p ?o } ORDER FOO\n                                      ^"
      2 REJECTS-FIXES
   s" SELECT * WHERE { ?s ?p ?o } ORDER ?s"
   s\" 1:35: expected BY, found `?s`; write BY here\n1 | SELECT * WHERE { ?s ?p ?o } ORDER ?s\n                                      ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o } ORDER BY FOO"
   s\" 1:38: expected a variable or expression to order by, found `FOO`; replace it with the variables to order by\n1 | SELECT * WHERE { ?s ?p ?o } ORDER BY FOO\n                                         ^"
      REJECTS
   \ An operator after an ORDER BY, GROUP BY or HAVING condition.
   s" SELECT * WHERE { ?s ?p ?o } ORDER BY ?s = 1"
   s\" 1:38: expected a condition in parentheses or a function call, found `?s`; put the condition in parentheses\n1 | SELECT * WHERE { ?s ?p ?o } ORDER BY ?s = 1\n                                         ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s = 1"
   s\" 1:39: expected a condition in parentheses or a function call, found `?s`; put the condition in parentheses\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s = 1\n                                          ^"
      2 REJECTS-FIXES
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY (?s) = 1"
   s\" 1:39: expected a condition in parentheses or a function call, found `(`; put the condition in parentheses\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY (?s) = 1\n                                          ^"
      2 REJECTS-FIXES
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s HAVING (?s) = 1"
   s\" 1:49: expected a condition in parentheses or a function call, found `(`; put the condition in parentheses\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s HAVING (?s) = 1\n                                                    ^"
      REJECTS
   s" SELECT ?o WHERE { ?s ?p ?o } GROUP BY ?o HAVING ?o = 1"
   s\" 1:49: expected a condition in parentheses or a function call, found `?o`; put the condition in parentheses\n1 | SELECT ?o WHERE { ?s ?p ?o } GROUP BY ?o HAVING ?o = 1\n                                                    ^"
      REJECTS
   \ A modifier list ends before a token no item starts with.
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s FOO"
   s\" 1:42: expected the end of the query, found `FOO`; remove it\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s FOO\n                                             ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s HAVING (?s) FOO"
   s\" 1:54: expected the end of the query, found `FOO`; remove it\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s HAVING (?s) FOO\n                                                         ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o } ORDER BY ?s FOO"
   s\" 1:41: expected the end of the query, found `FOO`; remove it\n1 | SELECT * WHERE { ?s ?p ?o } ORDER BY ?s FOO\n                                            ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o } ORDER BY ?s AS"
   s\" 1:41: expected the end of the query, found `AS`; remove it\n1 | SELECT * WHERE { ?s ?p ?o } ORDER BY ?s AS\n                                            ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s FOO ORDER BY ?s"
   s\" 1:42: expected the end of the query, found `FOO`; remove it\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s FOO ORDER BY ?s\n                                             ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s FOO LIMIT 10"
   s\" 1:42: expected the end of the query, found `FOO`; remove it\n1 | SELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s FOO LIMIT 10\n                                             ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s FOO HAVING (COUNT(?o) > 1)"
   s\" 1:42: expected the end of the query, found `FOO`; remove it\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s FOO HAVING (COUNT(?o) > 1)\n                                             ^"
      REJECTS
   \ More than one stray token.
   s" SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s FOO BAR"
   s\" 1:42: expected the end of the query, found `FOO`; remove the text from here on\n1 | SELECT ?s WHERE { ?s ?p ?o } GROUP BY ?s FOO BAR\n                                             ^"
      REJECTS
   s" SELECT ?s WHERE { ?s ?p ?o } LIMIT 99999999999999999999"
   s\" 1:36: expected an integer that fits 64 bits, found `99999999999999999999`; write at most 9223372036854775807\n1 | SELECT ?s WHERE { ?s ?p ?o } LIMIT 99999999999999999999\n                                       ^"
      REJECTS
   s" SELECT ?s WERE { ?s ?p ?o }"
   s\" 1:11: expected WHERE or {, found `WERE`; replace it with WHERE\n1 | SELECT ?s WERE { ?s ?p ?o }\n              ^"
      REJECTS
   s" ASK ?s ?p ?o }"
   s\" 1:5: expected WHERE or {, found `?s`; write WHERE { here\n1 | ASK ?s ?p ?o }\n        ^"
      REJECTS
   \ A query without its pattern.
   s" SELECT * LIMIT 1"
   s\" 1:10: expected WHERE or {, found `LIMIT`; write the pattern as WHERE { ... }\n1 | SELECT * LIMIT 1\n             ^"
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
   s\" 1:10: expected WHERE or {, found `FROM`; remove FROM, any NAMED and its IRI: the base you query is the dataset\n1 | SELECT * FROM <urn:g> WHERE { ?s ?p ?o }\n             ^"
      REJECTS
   s" ASK FROM NAMED <urn:g> { ?s ?p ?o }"
   s\" 1:5: expected WHERE or {, found `FROM`; remove FROM, any NAMED and its IRI: the base you query is the dataset\n1 | ASK FROM NAMED <urn:g> { ?s ?p ?o }\n        ^"
      REJECTS
   s" SELECT * WHERE { SERVICE <urn:s> { ?s ?p ?o } }"
   s\" 1:18: expected a graph pattern, found `SERVICE`; remove SERVICE, any SILENT, its IRI or variable and its group: a query reads only its base\n1 | SELECT * WHERE { SERVICE <urn:s> { ?s ?p ?o } }\n                     ^"
      REJECTS
   s" ASK { SERVICE SILENT <urn:s> { ?s ?p ?o } }"
   s\" 1:7: expected a graph pattern, found `SERVICE`; remove SERVICE, any SILENT, its IRI or variable and its group: a query reads only its base\n1 | ASK { SERVICE SILENT <urn:s> { ?s ?p ?o } }\n          ^"
      REJECTS
   s" ASK { SERVICE <urn:s> { ?s ?p ?o"
   s\" 1:7: expected a graph pattern, found `SERVICE`; remove SERVICE, any SILENT, its IRI or variable and its group: a query reads only its base\n1 | ASK { SERVICE <urn:s> { ?s ?p ?o\n          ^"
      2 REJECTS-FIXES
   s" SELECT ?b WHERE { ?s ?p ?o BIND(BNODE() AS ?b) }"
   s\" 1:33: expected a function this endpoint provides, found `BNODE`; bind a variable instead: a query cannot make blank nodes\n1 | SELECT ?b WHERE { ?s ?p ?o BIND(BNODE() AS ?b) }\n                                    ^"
      REJECTS
   s" SELECT ?h WHERE { ?s ?p ?o BIND(SHA1(STR(?o)) AS ?h) }"
   s\" 1:33: expected a function this endpoint provides, found `SHA1`; use SHA256\n1 | SELECT ?h WHERE { ?s ?p ?o BIND(SHA1(STR(?o)) AS ?h) }\n                                    ^"
      REJECTS
   \ At the `*` of a grouped query, not at SELECT.
   s" SELECT DISTINCT * WHERE { ?s ?p ?o } GROUP BY ?s"
   s\" 1:17: expected SELECT with named variables in a grouped query, found `*`; replace * with ?s\n1 | SELECT DISTINCT * WHERE { ?s ?p ?o } GROUP BY ?s\n                    ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o } HAVING (COUNT(*) > 1)"
   s\" 1:8: expected SELECT with named variables in a grouped query, found `*`; replace * with (COUNT(*) AS ?n)\n1 | SELECT * WHERE { ?s ?p ?o } HAVING (COUNT(*) > 1)\n           ^"
      REJECTS
   \ From pattern.f.
   s\" SELECT * WHERE { GRAPH \"g\" { ?s ?p ?o } }"
   s\" 1:24: expected a variable, IRI or prefixed name, found `\"g\"`; replace it with a variable or an IRI\n1 | SELECT * WHERE { GRAPH \"g\" { ?s ?p ?o } }\n                           ^"
      REJECTS
   \ A graph name before its group.
   s" SELECT * WHERE { GRAPH { ?s ?p ?o } }"
   s\" 1:24: expected a variable, IRI or prefixed name, found `{`; write a variable or an IRI here\n1 | SELECT * WHERE { GRAPH { ?s ?p ?o } }\n                           ^"
      REJECTS
   \ A graph name at the end of the query.
   s" SELECT * { GRAPH"
   s\" 1:17: expected a variable, IRI or prefixed name, found the end of the query; write a variable or an IRI here\n1 | SELECT * { GRAPH\n                    ^"
      4 REJECTS-FIXES
   s" SELECT * WHERE { ?s }"
   s\" 1:21: expected a predicate: an IRI, prefixed name, variable or a, found `}`; write a predicate and an object after the subject\n1 | SELECT * WHERE { ?s }\n                        ^"
      REJECTS
   \ A subject missing before a predicate, and a token no subject is.
   s" ASK { a <urn:C> }"
   s\" 1:7: expected a variable, IRI, prefixed name, literal or blank node, found `a`; write a term such as ?x or <iri> here\n1 | ASK { a <urn:C> }\n          ^"
      REJECTS
   s" ASK { UNDEF ?p ?o }"
   s\" 1:7: expected a variable, IRI, prefixed name, literal or blank node, found `UNDEF`; replace it with a term such as ?x or <iri>\n1 | ASK { UNDEF ?p ?o }\n          ^"
      REJECTS
   s" ASK { UNDEF <urn:p>/<urn:q> ?o }"
   s\" 1:7: expected a variable, IRI, prefixed name, literal or blank node, found `UNDEF`; replace it with a term such as ?x or <iri>\n1 | ASK { UNDEF <urn:p>/<urn:q> ?o }\n          ^"
      REJECTS
   \ A token no predicate is after a subject.
   s" ASK { ?s = }"
   s\" 1:10: expected a predicate: an IRI, prefixed name, variable or a, found `=`; replace it with a predicate and an object\n1 | ASK { ?s = }\n             ^"
      REJECTS
   \ An object before a token no term may precede.
   s" SELECT * { ?s ?p ? }"
   s\" 1:18: expected a variable, IRI, prefixed name, literal or blank node, found `?`; replace it with a term such as ?x or <iri>\n1 | SELECT * { ?s ?p ? }\n                     ^"
      REJECTS
   \ Where a subject goes, a token that is neither a subject nor a
   \ predicate.
   s" SELECT * WHERE { ?s ?p ?o . ) }"
   s\" 1:29: expected a variable, IRI, prefixed name, literal or blank node, found `)`; remove it\n1 | SELECT * WHERE { ?s ?p ?o . ) }\n                                ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o . FOO }"
   s\" 1:29: expected a variable, IRI, prefixed name, literal or blank node, found `FOO`; remove it\n1 | SELECT * WHERE { ?s ?p ?o . FOO }\n                                ^"
      REJECTS
   s" SELECT * WHERE { ) ?s ?p ?o }"
   s\" 1:18: expected a variable, IRI, prefixed name, literal or blank node, found `)`; remove it\n1 | SELECT * WHERE { ) ?s ?p ?o }\n                     ^"
      REJECTS
   \ After a triple, a token that may neither start nor follow one.
   s" SELECT * { ?s ?p ?o ) }"
   s\" 1:21: expected . or } after a triple, found `)`; remove it\n1 | SELECT * { ?s ?p ?o ) }\n                        ^"
      REJECTS
   \ After a triple, a literal with a tag, as another object.
   s\" SELECT * { ?s ?p ?o \"a\"@en }"
   s\" 1:21: expected . or } after a triple, found `\"a\"`; write , before it\n1 | SELECT * { ?s ?p ?o \"a\"@en }\n                        ^"
      REJECTS
   \ After a triple, a token that may start a predicate but no triple.
   s" ASK { ?s ?p ?o a <urn:C> }"
   s\" 1:16: expected ; before a predicate after an object, found `a`; write ; here\n1 | ASK { ?s ?p ?o a <urn:C> }\n                   ^"
      REJECTS
   s" ASK { ?s ?p ?o ^<urn:q> ?z }"
   s\" 1:16: expected ; before a predicate after an object, found `^`; write ; here\n1 | ASK { ?s ?p ?o ^<urn:q> ?z }\n                   ^"
      REJECTS
   s" ASK { ?s ?p ?o !<urn:q> ?z }"
   s\" 1:16: expected ; before a predicate after an object, found `!`; write ; here\n1 | ASK { ?s ?p ?o !<urn:q> ?z }\n                   ^"
      REJECTS
   \ A UNION after triples.
   s" SELECT * WHERE { ?s ?p ?o UNION { ?s ?q ?o } }"
   s\" 1:27: expected . or } after a triple, found `UNION`; put the triples before it in braces\n1 | SELECT * WHERE { ?s ?p ?o UNION { ?s ?q ?o } }\n                              ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o . ?o ?q ?r UNION { ?s ?q ?o } }"
   s\" 1:38: expected . or } after a triple, found `UNION`; put the triples before it in braces\n1 | SELECT * WHERE { ?s ?p ?o . ?o ?q ?r UNION { ?s ?q ?o } }\n                                         ^"
      REJECTS
   \ A UNION where a pattern starts.
   s" SELECT * WHERE { ?s ?p ?o . UNION { ?s ?q ?o } }"
   s\" 1:29: expected a variable, IRI, prefixed name, literal or blank node, found `UNION`; put the triples before it in braces\n1 | SELECT * WHERE { ?s ?p ?o . UNION { ?s ?q ?o } }\n                                ^"
      REJECTS
   s" SELECT * WHERE { OPTIONAL { ?s ?p ?o } UNION { ?s ?q ?o } }"
   s\" 1:40: expected a variable, IRI, prefixed name, literal or blank node, found `UNION`; put the pattern before it in braces\n1 | SELECT * WHERE { OPTIONAL { ?s ?p ?o } UNION { ?s ?q ?o } }\n                                           ^"
      REJECTS
   s" SELECT * WHERE { UNION { ?s ?q ?o } }"
   s\" 1:18: expected a variable, IRI, prefixed name, literal or blank node, found `UNION`; remove it\n1 | SELECT * WHERE { UNION { ?s ?q ?o } }\n                     ^"
      REJECTS
   \ A UNION after triples after a FILTER.
   s" SELECT * WHERE { ?a ?b ?c FILTER(?a) ?s ?p ?o UNION { ?s ?q ?o } }"
   s\" 1:47: expected . or } after a triple, found `UNION`; put the triples before it in braces\n1 | SELECT * WHERE { ?a ?b ?c FILTER(?a) ?s ?p ?o UNION { ?s ?q ?o } }\n                                                  ^"
      REJECTS
   \ An object before a token that may follow one.
   s" SELECT * { ?s ?p . }"
   s\" 1:18: expected a variable, IRI, prefixed name, literal or blank node, found `.`; write a term such as ?x or <iri> here\n1 | SELECT * { ?s ?p . }\n                     ^"
      REJECTS
   \ In a collection whose ) is ahead, a token no member starts with.
   s" SELECT * { ( ?a | ?b ) ?p ?o }"
   s\" 1:17: expected a term or ), found `|`; remove it\n1 | SELECT * { ( ?a | ?b ) ?p ?o }\n                    ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ( ?a , ?b ) }"
   s\" 1:29: expected a term or ), found `,`; remove it\n1 | SELECT * WHERE { ?s ?p ( ?a , ?b ) }\n                                ^"
      REJECTS
   \ A collection before a token that may follow it.
   s" SELECT * { ?s ?p ( ?a }"
   s\" 1:23: expected a term or ), found `}`; write ) here\n1 | SELECT * { ?s ?p ( ?a }\n                          ^"
      REJECTS
   \ A collection whose ) is not ahead, before a token that cannot follow it.
   s" SELECT * { ?s ?p ( ?a UNION ?b }"
   s\" 1:23: expected a variable, IRI, prefixed name, literal or blank node, found `UNION`; replace it with a term such as ?x or <iri>\n1 | SELECT * { ?s ?p ( ?a UNION ?b }\n                          ^"
      2 REJECTS-FIXES
   \ A path element after a path in ( ).
   s" SELECT * { ?s (<urn:p> <urn:q>) ?o }"
   s\" 1:24: expected ), / or |, found `<urn:q>`; write / or | here\n1 | SELECT * { ?s (<urn:p> <urn:q>) ?o }\n                           ^"
      REJECTS
   \ A member after a member of a negated set.
   s" SELECT * WHERE { ?s !(<urn:a> <urn:b>) ?o }"
   s\" 1:31: expected | or ), found `<urn:b>`; write | here\n1 | SELECT * WHERE { ?s !(<urn:a> <urn:b>) ?o }\n                                  ^"
      REJECTS
   \ A token no ) may precede after a path group nested two deep, and after a
   \ member of a negated set: ) replaces it.
   s" SELECT * WHERE { ?s ((<urn:p>)FOO ?o }"
   s\" 1:31: expected ), found `FOO`; replace it with )\n1 | SELECT * WHERE { ?s ((<urn:p>)FOO ?o }\n                                  ^"
      REJECTS
   s" SELECT * WHERE { ?s !(<urn:a> FOO ?o }"
   s\" 1:31: expected ), found `FOO`; replace it with )\n1 | SELECT * WHERE { ?s !(<urn:a> FOO ?o }\n                                  ^"
      REJECTS
   \ A token that ends what holds an empty negated set: ) is written
   \ before it.
   s" SELECT * WHERE { ?s !( }"
   s\" 1:24: expected an IRI, prefixed name or ), found `}`; write ) here\n1 | SELECT * WHERE { ?s !( }\n                           ^"
      2 REJECTS-FIXES
   \ A missing datatype IRI: in place of a token no object may precede,
   \ before one a collection member may.
   s\" SELECT * WHERE { ?s ?p \"x\"^^ ?o . }"
   s\" 1:30: expected an IRI or prefixed name, found `?o`; replace it with an IRI or prefixed name\n1 | SELECT * WHERE { ?s ?p \"x\"^^ ?o . }\n                                 ^"
      REJECTS
   s\" SELECT * WHERE { ?s ?p ( \"x\"^^ ?o ) }"
   s\" 1:32: expected an IRI or prefixed name, found `?o`; write an IRI or prefixed name here\n1 | SELECT * WHERE { ?s ?p ( \"x\"^^ ?o ) }\n                                   ^"
      REJECTS
   \ An IRI missing in a path: written before a token that may follow it
   \ there, else in place of the token.
   s" SELECT * WHERE { ?s <urn:p>/ . }"
   s\" 1:30: expected an IRI or prefixed name, found `.`; replace it with an IRI or prefixed name\n1 | SELECT * WHERE { ?s <urn:p>/ . }\n                                 ^"
      2 REJECTS-FIXES
   s" SELECT * WHERE { ?s <urn:p>/ true }"
   s\" 1:30: expected an IRI or prefixed name, found `true`; write an IRI or prefixed name here\n1 | SELECT * WHERE { ?s <urn:p>/ true }\n                                 ^"
      REJECTS
   s" SELECT * WHERE { ?s (<urn:p>/ ?o) ?o }"
   s\" 1:31: expected an IRI or prefixed name, found `?o`; replace it with an IRI or prefixed name\n1 | SELECT * WHERE { ?s (<urn:p>/ ?o) ?o }\n                                  ^"
      REJECTS
   s" SELECT * WHERE { ?s !( <urn:a> | ?x ) ?o }"
   s\" 1:34: expected an IRI or prefixed name, found `?x`; replace it with an IRI or prefixed name\n1 | SELECT * WHERE { ?s !( <urn:a> | ?x ) ?o }\n                                     ^"
      REJECTS
   s" SELECT * WHERE { VALUES ?x { ?y } }"
   s\" 1:30: expected a value or }, found `?y`; remove it\n1 | SELECT * WHERE { VALUES ?x { ?y } }\n                                 ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?x ?y) { (1) } }"
   s\" 1:37: expected a row with one value per variable, found `)`; write one value or UNDEF per variable in each row\n1 | SELECT * WHERE { VALUES (?x ?y) { (1) } }\n                                        ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?s ?o) { (<urn:a>) } }"
   s\" 1:43: expected a row with one value per variable, found `)`; write one value or UNDEF per variable in each row\n1 | SELECT * WHERE { VALUES (?s ?o) { (<urn:a>) } }\n                                              ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?s) { (<urn:a> <urn:b>) } }"
   s\" 1:41: expected a row with one value per variable, found `<urn:b>`; write one value or UNDEF per variable in each row\n1 | SELECT * WHERE { VALUES (?s) { (<urn:a> <urn:b>) } }\n                                            ^"
      REJECTS
   \ A row holding a token that is no value past its last value.
   s" SELECT * WHERE { VALUES (?s) { (<urn:a> FOO) } }"
   s\" 1:41: expected ), found `FOO`; remove it\n1 | SELECT * WHERE { VALUES (?s) { (<urn:a> FOO) } }\n                                            ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?s ?v { } }"
   s\" 1:32: expected a variable or ), found `{`; write ) here\n1 | SELECT * WHERE { VALUES (?s ?v { } }\n                                   ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?s <urn:x>) { } }"
   s\" 1:29: expected a variable, found `<urn:x>`; replace it with a variable\n1 | SELECT * WHERE { VALUES (?s <urn:x>) { } }\n                                ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?s ?v) { (<urn:one> 1 (<urn:two> UNDEF) } }"
   s\" 1:48: expected a value or ), found `(`; write ) here\n1 | SELECT * WHERE { VALUES (?s ?v) { (<urn:one> 1 (<urn:two> UNDEF) } }\n                                                   ^"
      REJECTS
   \ A row } ends, one value short: its ) is written, then its width
   \ is rejected at that ).
   s" SELECT * WHERE { VALUES (?s ?o) { (<urn:a> } }"
   s\" 1:44: expected a value or ), found `}`; write ) here\n1 | SELECT * WHERE { VALUES (?s ?o) { (<urn:a> } }\n                                               ^"
      2 REJECTS-FIXES
   s" SELECT * WHERE { VALUES (?s) { (FOO) } }"
   s\" 1:33: expected an IRI, literal or UNDEF, found `FOO`; replace it with an IRI, literal or UNDEF\n1 | SELECT * WHERE { VALUES (?s) { (FOO) } }\n                                    ^"
      REJECTS
   s" SELECT * WHERE { VALUES ?s { FOO } }"
   s\" 1:30: expected a value or }, found `FOO`; remove it\n1 | SELECT * WHERE { VALUES ?s { FOO } }\n                                 ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?s) { (<urn:a>) FOO } }"
   s\" 1:42: expected ( or }, found `FOO`; remove it\n1 | SELECT * WHERE { VALUES (?s) { (<urn:a>) FOO } }\n                                             ^"
      REJECTS
   s" SELECT * { } VALUES ?s { <urn:a> ?x }"
   s\" 1:34: expected a value or }, found `?x`; remove it\n1 | SELECT * { } VALUES ?s { <urn:a> ?x }\n                                     ^"
      REJECTS
   \ Rows before a token no row starts with.
   s" SELECT * WHERE { VALUES (?s) { (<urn:a>) . } }"
   s\" 1:42: expected ( or }, found `.`; remove it\n1 | SELECT * WHERE { VALUES (?s) { (<urn:a>) . } }\n                                             ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?s) { (<urn:a>) FILTER(true) } }"
   s\" 1:42: expected ( or }, found `FILTER`; move the } that ends this block here\n1 | SELECT * WHERE { VALUES (?s) { (<urn:a>) FILTER(true) } }\n                                             ^"
      REJECTS
   s" SELECT * WHERE { VALUES (?s) { (<urn:a>) FILTER(true) }"
   s\" 1:42: expected ( or }, found `FILTER`; write } here\n1 | SELECT * WHERE { VALUES (?s) { (<urn:a>) FILTER(true) }\n                                             ^"
      REJECTS
   \ A value where a row starts.
   s" SELECT * WHERE { VALUES (?s ?o) { <urn:a> } }"
   s\" 1:35: expected ( or }, found `<urn:a>`; put the row in parentheses\n1 | SELECT * WHERE { VALUES (?s ?o) { <urn:a> } }\n                                      ^"
      2 REJECTS-FIXES
   \ One variable's values before a token no value starts with.
   s" SELECT * WHERE { VALUES ?s { <urn:a> . } }"
   s\" 1:38: expected a value or }, found `.`; remove it\n1 | SELECT * WHERE { VALUES ?s { <urn:a> . } }\n                                         ^"
      REJECTS
   \ A variable list at the end of the query.
   s" SELECT * { VALUES (?s ?v"
   s\" 1:25: expected a variable or ), found the end of the query; write ) here\n1 | SELECT * { VALUES (?s ?v\n                            ^"
      4 REJECTS-FIXES
   \ A row at the end of the query.
   s" SELECT * { VALUES (?s ?v) { (<urn:one> 1"
   s\" 1:41: expected a value or ), found the end of the query; write ) here\n1 | SELECT * { VALUES (?s ?v) { (<urn:one> 1\n                                            ^"
      3 REJECTS-FIXES
   \ Rows at the end of the query.
   s" SELECT * { VALUES ?s { <urn:one>"
   s\" 1:33: expected }, found the end of the query; write } here\n1 | SELECT * { VALUES ?s { <urn:one>\n                                    ^"
      2 REJECTS-FIXES
   \ A blank node label in two groups.
   s" SELECT * WHERE { { _:a <urn:p> ?o } { ?s <urn:q> _:a } }"
   s\" 1:50: expected a blank node label not used in another group, found `_:a`; rename the label\n1 | SELECT * WHERE { { _:a <urn:p> ?o } { ?s <urn:q> _:a } }\n                                                     ^"
      REJECTS
   \ A variable bound twice.
   s" SELECT * WHERE { ?s ?p ?w BIND(1 AS ?w) }"
   s\" 1:37: expected a variable not already in scope, found `?w`; use a new variable instead of ?w\n1 | SELECT * WHERE { ?s ?p ?w BIND(1 AS ?w) }\n                                        ^"
      REJECTS
   \ A variable missing after AS: the stand-in is numbered past ?x1.
   s" SELECT * WHERE { ?x1 ?p ?o BIND(1 AS ) }"
   s\" 1:38: expected a variable, found `)`; write a variable here\n1 | SELECT * WHERE { ?x1 ?p ?o BIND(1 AS ) }\n                                         ^"
      REJECTS
   \ From expression.f.
   s" SELECT * WHERE { ?s ?p ?o FILTER(STRLEN(?o, ?s)) }"
   s\" 1:34: expected 1 argument, found `STRLEN`; call STRLEN with 1 argument\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STRLEN(?o, ?s)) }\n                                     ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(_:b) }"
   s\" 1:34: expected a variable, IRI, prefixed name or literal, found `_:b`; replace the blank node with a variable\n1 | SELECT * WHERE { ?s ?p ?o FILTER(_:b) }\n                                     ^"
      REJECTS
   \ A word no term may precede.
   s" SELECT * WHERE { ?s ?p ?o FILTER(?s = FOO) }"
   s\" 1:39: expected a variable, IRI, prefixed name or literal, found `FOO`; replace it with a term such as ?x or <iri>\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?s = FOO) }\n                                          ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(?a = ?b = ?c) }"
   s\" 1:42: expected one comparison per operand pair, found `=`; put the first comparison in parentheses\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?a = ?b = ?c) }\n                                             ^"
      REJECTS
   \ A * given to an IRI.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(<urn:f>(*)) }"
   s\" 1:34: expected arguments without *, SEPARATOR or ORDER BY, found `<urn:f>`; remove the *\n1 | SELECT * WHERE { ?s ?p ?o FILTER(<urn:f>(*)) }\n                                     ^"
      REJECTS
   \ An ORDER BY after COUNT's *.
   s" SELECT (COUNT(* ORDER BY ?s) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:9: expected GROUP_CONCAT, the only function that takes ORDER BY, found `COUNT`; remove any ;, ORDER BY and its conditions\n1 | SELECT (COUNT(* ORDER BY ?s) AS ?c) WHERE { ?s ?p ?o }\n            ^"
      REJECTS
   \ A SEPARATOR given to an IRI.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(<urn:f>(?o; SEPARATOR=\",\")) }"
   s\" 1:34: expected arguments without *, SEPARATOR or ORDER BY, found `<urn:f>`; remove any ;, SEPARATOR, its = and its string\n1 | SELECT * WHERE { ?s ?p ?o FILTER(<urn:f>(?o; SEPARATOR=\",\")) }\n                                     ^"
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
   s\" 1:34: expected GROUP_CONCAT, the only function that takes SEPARATOR, found `STR`; remove any ;, SEPARATOR, its = and its string\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STR(?o; SEPARATOR=\",\")) }\n                                     ^"
      REJECTS
   s\" SELECT * WHERE { ?s ?p ?o } ORDER BY DESC(STR(?s; SEPARATOR=\",\"))"
   s\" 1:43: expected GROUP_CONCAT, the only function that takes SEPARATOR, found `STR`; remove any ;, SEPARATOR, its = and its string\n1 | SELECT * WHERE { ?s ?p ?o } ORDER BY DESC(STR(?s; SEPARATOR=\",\"))\n                                              ^"
      REJECTS
   s\" SELECT * WHERE { ?s ?p ?o } ORDER BY DESC(<urn:f>(?s SEPARATOR=\",\"))"
   s\" 1:43: expected arguments without *, SEPARATOR or ORDER BY, found `<urn:f>`; remove any ;, SEPARATOR, its = and its string\n1 | SELECT * WHERE { ?s ?p ?o } ORDER BY DESC(<urn:f>(?s SEPARATOR=\",\"))\n                                              ^"
      REJECTS
   \ ORDER BY outside GROUP_CONCAT.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(STR(?o; ORDER BY ?s)) }"
   s\" 1:34: expected GROUP_CONCAT, the only function that takes ORDER BY, found `STR`; remove any ;, ORDER BY and its conditions\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STR(?o; ORDER BY ?s)) }\n                                     ^"
      REJECTS
   \ ORDER BY outside GROUP_CONCAT, with no ; before it.
   s" SELECT * WHERE { ?s ?p ?o FILTER(STR(?o ORDER BY ?s)) }"
   s\" 1:34: expected GROUP_CONCAT, the only function that takes ORDER BY, found `STR`; remove any ;, ORDER BY and its conditions\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STR(?o ORDER BY ?s)) }\n                                     ^"
      REJECTS
   \ A clause other than SEPARATOR or ORDER BY outside GROUP_CONCAT.
   s" SELECT * WHERE { ?s ?p ?o FILTER(STR(?o; BY ?s)) }"
   s\" 1:40: expected ), found `;`; remove the ; and what follows it up to )\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STR(?o; BY ?s)) }\n                                           ^"
      REJECTS
   \ A ; for a , in a call that takes no clause.
   s" SELECT (CONCAT(?a; ?b) AS ?c) WHERE { ?s ?p ?a , ?b }"
   s\" 1:18: expected , or ), found `;`; replace it with ,\n1 | SELECT (CONCAT(?a; ?b) AS ?c) WHERE { ?s ?p ?a , ?b }\n                     ^"
      REJECTS
   \ GROUP_CONCAT's ORDER BY without a condition.
   s" SELECT (GROUP_CONCAT(?o; ORDER BY) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:34: expected a variable or expression to order by, found `)`; write the variables to order by after ORDER BY\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY) AS ?c) WHERE { ?s ?p ?o }\n                                     ^"
      REJECTS
   \ An ORDER followed by the call's ).
   s" SELECT (GROUP_CONCAT(?o; ORDER) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:31: expected BY, found `)`; write BY and the variables to order by after ORDER\n1 | SELECT (GROUP_CONCAT(?o; ORDER) AS ?g) WHERE { ?s ?p ?o }\n                                  ^"
      REJECTS
   \ An ORDER followed by neither BY nor a condition.
   s" SELECT (GROUP_CONCAT(?o; ORDER FOO) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:32: expected BY, found `FOO`; replace it with BY\n1 | SELECT (GROUP_CONCAT(?o; ORDER FOO) AS ?g) WHERE { ?s ?p ?o }\n                                   ^"
      2 REJECTS-FIXES
   \ GROUP_CONCAT's ORDER BY ends before a token no condition starts with.
   s" SELECT (GROUP_CONCAT(?o; ORDER BY ?s = 1) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:35: expected a condition in parentheses or a function call, found `?s`; put the condition in parentheses\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY ?s = 1) AS ?c) WHERE { ?s ?p ?o }\n                                      ^"
      REJECTS
   s" SELECT (GROUP_CONCAT(?o; ORDER BY ?s FOO) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:38: expected ), found `FOO`; remove it\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY ?s FOO) AS ?c) WHERE { ?s ?p ?o }\n                                         ^"
      REJECTS
   \ SEPARATOR after ORDER BY.
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY ?s; SEPARATOR=\",\") AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:39: expected ), found `SEPARATOR`; write SEPARATOR before ORDER BY\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY ?s; SEPARATOR=\",\") AS ?c) WHERE { ?s ?p ?o }\n                                          ^"
      REJECTS
   \ SEPARATOR after ORDER BY, with no ; before it.
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY ?s SEPARATOR=\",\") AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:38: expected ), found `SEPARATOR`; write SEPARATOR before ORDER BY\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY ?s SEPARATOR=\",\") AS ?c) WHERE { ?s ?p ?o }\n                                         ^"
      REJECTS
   \ A separator without its keyword after ORDER BY, in a call without one.
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY ?s; = \",\") AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:39: expected ), found `=`; write SEPARATOR before the =, then move SEPARATOR, the = and its string before ORDER BY\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY ?s; = \",\") AS ?g) WHERE { ?s ?p ?o }\n                                          ^"
      REJECTS
   \ ORDER BY with no ; before it.
   s" SELECT (GROUP_CONCAT(?o ORDER BY ?s) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:25: expected ;, found `ORDER`; write ; here\n1 | SELECT (GROUP_CONCAT(?o ORDER BY ?s) AS ?c) WHERE { ?s ?p ?o }\n                            ^"
      REJECTS
   \ A ; after ORDER BY's conditions.
   s" SELECT (GROUP_CONCAT(?o; ORDER BY ?s;) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:37: expected ), found `;`; remove the ;\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY ?s;) AS ?c) WHERE { ?s ?p ?o }\n                                        ^"
      REJECTS
   \ A ; between ORDER BY's conditions.
   s" SELECT (GROUP_CONCAT(?o; ORDER BY ?s; ?p) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:37: expected ), found `;`; remove the ;\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY ?s; ?p) AS ?c) WHERE { ?s ?p ?o }\n                                        ^"
      REJECTS
   \ A second ORDER BY.
   s" SELECT (GROUP_CONCAT(?o; ORDER BY ?s; ORDER BY ?p) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:39: expected ), found `ORDER`; write the conditions in one ORDER BY\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY ?s; ORDER BY ?p) AS ?c) WHERE { ?s ?p ?o }\n                                          ^"
      REJECTS
   \ A second ORDER BY with no ; before it.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; ORDER BY ?s ORDER BY ?o) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:53: expected ), found `ORDER`; write the conditions in one ORDER BY\n1 | ...P_CONCAT(?o; SEPARATOR=\",\"; ORDER BY ?s ORDER BY ?o) AS ?c) WHERE { ?s ?p ?o }\n                                               ^"
      REJECTS
   \ A second SEPARATOR.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; SEPARATOR=\",\") AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:41: expected ORDER, found `SEPARATOR`; remove the ;, the second SEPARATOR, its = and its string\n1 | SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; SEPARATOR=\",\") AS ?c) WHERE { ?s ?p ?o }\n                                            ^"
      REJECTS
   \ A second SEPARATOR with no ; before it.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\" SEPARATOR=\"x\") AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:40: expected ; or ), found `SEPARATOR`; remove the second SEPARATOR, its = and its string\n1 | SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\" SEPARATOR=\"x\") AS ?g) WHERE { ?s ?p ?o }\n                                           ^"
      REJECTS
   \ A second separator without its keyword.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; = \",\") AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:41: expected ORDER, found `=`; remove the ;, its = and its string\n1 | SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; = \",\") AS ?c) WHERE { ?s ?p ?o }\n                                            ^"
      REJECTS
   \ A separator that is no string.
   s" SELECT (GROUP_CONCAT(?o; SEPARATOR=1) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:36: expected a string, found `1`; replace it with a string\n1 | SELECT (GROUP_CONCAT(?o; SEPARATOR=1) AS ?c) WHERE { ?s ?p ?o }\n                                       ^"
      REJECTS
   \ A ; after SEPARATOR's string.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\";) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:39: expected ), found `;`; remove the ;\n1 | SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\";) AS ?c) WHERE { ?s ?p ?o }\n                                          ^"
      REJECTS
   \ A ; with no clause after it.
   s" SELECT (GROUP_CONCAT(?o;) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:24: expected ), found `;`; remove the ;\n1 | SELECT (GROUP_CONCAT(?o;) AS ?c) WHERE { ?s ?p ?o }\n                           ^"
      REJECTS
   \ A clause after SEPARATOR other than ORDER BY.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; BY ?s) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:41: expected ORDER, found `BY`; write ORDER here\n1 | SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; BY ?s) AS ?c) WHERE { ?s ?p ?o }\n                                            ^"
      REJECTS
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; FOO) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:39: expected ), found `;`; remove the ; and what follows it up to )\n1 | SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; FOO) AS ?g) WHERE { ?s ?p ?o }\n                                          ^"
      REJECTS
   \ A first clause other than SEPARATOR or ORDER BY.
   s" SELECT (GROUP_CONCAT(?o; BY ?s) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:26: expected SEPARATOR or ORDER, found `BY`; write ORDER here\n1 | SELECT (GROUP_CONCAT(?o; BY ?s) AS ?c) WHERE { ?s ?p ?o }\n                             ^"
      REJECTS
   s\" SELECT (GROUP_CONCAT(?o; = \",\") AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:26: expected SEPARATOR or ORDER, found `=`; write SEPARATOR here\n1 | SELECT (GROUP_CONCAT(?o; = \",\") AS ?c) WHERE { ?s ?p ?o }\n                             ^"
      REJECTS
   s" SELECT (GROUP_CONCAT(?o; FOO) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:24: expected ), found `;`; remove the ; and what follows it up to )\n1 | SELECT (GROUP_CONCAT(?o; FOO) AS ?g) WHERE { ?s ?p ?o }\n                           ^"
      REJECTS
   s" SELECT (GROUP_CONCAT(?o; ?s) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:24: expected ), found `;`; remove the ; and what follows it up to )\n1 | SELECT (GROUP_CONCAT(?o; ?s) AS ?g) WHERE { ?s ?p ?o }\n                           ^"
      REJECTS
   \ Two ; after SEPARATOR's string.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\";;) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:39: expected ), found `;`; remove the ;\n1 | SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\";;) AS ?c) WHERE { ?s ?p ?o }\n                                          ^"
      2 REJECTS-FIXES
   \ A second SEPARATOR after ORDER BY.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; ORDER BY ?s; SEPARATOR=\",\") AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:54: expected ), found `SEPARATOR`; remove the ;, the second SEPARATOR, its = and its string\n1 | ..._CONCAT(?o; SEPARATOR=\",\"; ORDER BY ?s; SEPARATOR=\",\") AS ?c) WHERE { ?s ?p ?o }\n                                               ^"
      REJECTS
   \ A second SEPARATOR after ORDER BY, with no ; before it.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; ORDER BY ?s SEPARATOR=\"x\") AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:53: expected ), found `SEPARATOR`; remove the second SEPARATOR, its = and its string\n1 | ...P_CONCAT(?o; SEPARATOR=\",\"; ORDER BY ?s SEPARATOR=\"x\") AS ?g) WHERE { ?s ?p ?o }\n                                               ^"
      REJECTS
   \ A separator without its keyword after ORDER BY.
   s\" SELECT (GROUP_CONCAT(?o; SEPARATOR=\",\"; ORDER BY ?s; = \",\") AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:52: expected ), found `;`; remove the ;, its = and its string\n1 | ...UP_CONCAT(?o; SEPARATOR=\",\"; ORDER BY ?s; = \",\") AS ?c) WHERE { ?s ?p ?o }\n                                               ^"
      REJECTS
   \ An aggregate in an ORDER BY condition.
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY COUNT(?s)) AS ?c) WHERE { ?s ?p ?o }"
   s\" 1:35: expected an expression without an aggregate, found `COUNT`; use aggregates only in SELECT, HAVING and ORDER BY, unnested\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY COUNT(?s)) AS ?c) WHERE { ?s ?p ?o }\n                                      ^"
      REJECTS
   \ BOUND of a literal.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(BOUND(1)) }"
   s\" 1:40: expected a variable, found `1`; pass BOUND a variable\n1 | SELECT * WHERE { ?s ?p ?o FILTER(BOUND(1)) }\n                                           ^"
      REJECTS
   \ BOUND of a term the query writes before it.
   s\" SELECT * WHERE { ?s ?p 1 FILTER(BOUND(1)) }"
   s\" 1:39: expected a variable, found `1`; pass BOUND a variable\n1 | SELECT * WHERE { ?s ?p 1 FILTER(BOUND(1)) }\n                                          ^"
      REJECTS
   \ BOUND of a string.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(BOUND(\"abc\")) }"
   s\" 1:40: expected a variable, found `\"abc\"`; pass BOUND a variable\n1 | SELECT * WHERE { ?s ?p ?o FILTER(BOUND(\"abc\")) }\n                                           ^"
      REJECTS
   \ BOUND of an operation.
   s\" SELECT * WHERE { ?s ?p ?o FILTER(BOUND(?s + 1)) }"
   s\" 1:40: expected a variable, found `?s + 1`; pass BOUND a variable\n1 | SELECT * WHERE { ?s ?p ?o FILTER(BOUND(?s + 1)) }\n                                           ^"
      REJECTS
   \ A call without its (.
   s" SELECT * WHERE { ?s ?p ?o FILTER(BOUND ?o)) }"
   s\" 1:40: expected (, found `?o`; write ( here\n1 | SELECT * WHERE { ?s ?p ?o FILTER(BOUND ?o)) }\n                                           ^"
      REJECTS
   \ A FILTER without parentheses.
   s\" SELECT * WHERE { ?s ?p ?o FILTER ?o }"
   s\" 1:34: expected a condition in parentheses or a function call, found `?o`; put the condition in parentheses\n1 | SELECT * WHERE { ?s ?p ?o FILTER ?o }\n                                     ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER ?o = 1 }"
   s\" 1:34: expected a condition in parentheses or a function call, found `?o`; put the condition in parentheses\n1 | SELECT * WHERE { ?s ?p ?o FILTER ?o = 1 }\n                                     ^"
      REJECTS
   \ The same before a unary !.
   s" SELECT * WHERE { ?s ?p ?o FILTER !BOUND(?o) }"
   s\" 1:34: expected a condition in parentheses or a function call, found `!`; put the condition in parentheses\n1 | SELECT * WHERE { ?s ?p ?o FILTER !BOUND(?o) }\n                                     ^"
      REJECTS
   \ An operator after a FILTER's condition.
   s" SELECT * WHERE { ?s ?p ?o FILTER (?o) = 1 }"
   s\" 1:34: expected a condition in parentheses or a function call, found `(`; put the condition in parentheses\n1 | SELECT * WHERE { ?s ?p ?o FILTER (?o) = 1 }\n                                     ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER STR(?o) = 1 }"
   s\" 1:34: expected a condition in parentheses or a function call, found `STR`; put the condition in parentheses\n1 | SELECT * WHERE { ?s ?p ?o FILTER STR(?o) = 1 }\n                                     ^"
      REJECTS
   \ A , missing between a call's arguments or a list's members.
   s" SELECT * WHERE { ?s ?p ?o FILTER(CONCAT(?o ?s)) }"
   s\" 1:44: expected , or ), found `?s`; write , here\n1 | SELECT * WHERE { ?s ?p ?o FILTER(CONCAT(?o ?s)) }\n                                               ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(?o IN (1 2)) }"
   s\" 1:43: expected , or ), found `2`; write , here\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?o IN (1 2)) }\n                                              ^"
      REJECTS
   \ An argument past the most a call takes.
   s" SELECT (COUNT(?o ?p) AS ?n) WHERE { ?s ?p ?o }"
   s\" 1:18: expected ), found `?p`; remove it\n1 | SELECT (COUNT(?o ?p) AS ?n) WHERE { ?s ?p ?o }\n                     ^"
      REJECTS
   \ An operand where ) is expected, which an operator continues.
   s" SELECT (COUNT(?o ?p + 1) AS ?n) WHERE { ?s ?p ?o }"
   s\" 1:18: expected ), found `?p`; write an operator here\n1 | SELECT (COUNT(?o ?p + 1) AS ?n) WHERE { ?s ?p ?o }\n                     ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(?s ?o > 1) }"
   s\" 1:37: expected ), found `?o`; write an operator here\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?s ?o > 1) }\n                                        ^"
      REJECTS
   \ A word no ) may precede.
   s" SELECT * WHERE { ?s ?p ?o FILTER(STR(?o FOO)) }"
   s\" 1:41: expected ), found `FOO`; remove it\n1 | SELECT * WHERE { ?s ?p ?o FILTER(STR(?o FOO)) }\n                                            ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER((?o = 1 FOO)) }"
   s\" 1:42: expected ), found `FOO`; remove it\n1 | SELECT * WHERE { ?s ?p ?o FILTER((?o = 1 FOO)) }\n                                             ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 FOO) }"
   s\" 1:41: expected ), found `FOO`; remove it\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 FOO) }\n                                            ^"
      REJECTS
   \ A , between two conditions.
   s" SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 , ?s = 2) }"
   s\" 1:41: expected ), found `,`; replace it with an operator\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 , ?s = 2) }\n                                            ^"
      REJECTS
   s" SELECT (?o AS ?n FOO) WHERE { ?s ?p ?o }"
   s\" 1:18: expected ), found `FOO`; remove it\n1 | SELECT (?o AS ?n FOO) WHERE { ?s ?p ?o }\n                     ^"
      REJECTS
   \ A FILTER's ) is written only before a token that may follow a pattern.
   s" SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 WHERE }"
   s\" 1:41: expected ), found `WHERE`; replace it with )\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 WHERE }\n                                            ^"
      REJECTS
   \ A call's ORDER BY condition's ) is written before its SEPARATOR.
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY DESC(?s SEPARATOR=\",\") AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:43: expected ), found `SEPARATOR`; write SEPARATOR before ORDER BY\n1 | ...LECT (GROUP_CONCAT(?o; ORDER BY DESC(?s SEPARATOR=\",\") AS ?g) WHERE { ?s ?p ?o }\n                                               ^"
      2 REJECTS-FIXES
   \ A SEPARATOR after an operator in an ORDER BY condition.
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY DESC(?s + SEPARATOR=\",\")) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:45: expected ), found `SEPARATOR`; write SEPARATOR before ORDER BY\n1 | ...CT (GROUP_CONCAT(?o; ORDER BY DESC(?s + SEPARATOR=\",\")) AS ?g) WHERE { ?s ?p ?o ...\n                                               ^"
      2 REJECTS-FIXES
   \ A SEPARATOR inside an ORDER BY condition's parentheses.
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY DESC(?s SEPARATOR=\",\")) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:43: expected ), found `SEPARATOR`; write SEPARATOR before ORDER BY\n1 | ...LECT (GROUP_CONCAT(?o; ORDER BY DESC(?s SEPARATOR=\",\")) AS ?g) WHERE { ?s ?p ?o ...\n                                               ^"
      REJECTS
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY (?s SEPARATOR=\",\")) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:39: expected ), found `SEPARATOR`; write SEPARATOR before ORDER BY\n1 | SELECT (GROUP_CONCAT(?o; ORDER BY (?s SEPARATOR=\",\")) AS ?g) WHERE { ?s ?p ?o }\n                                          ^"
      REJECTS
   \ A SEPARATOR in a call nested in an ORDER BY condition.
   s\" SELECT (GROUP_CONCAT(?o; ORDER BY DESC(STR(?s SEPARATOR=\",\"))) AS ?g) WHERE { ?s ?p ?o }"
   s\" 1:47: expected ), found `SEPARATOR`; write SEPARATOR before ORDER BY\n1 | ... (GROUP_CONCAT(?o; ORDER BY DESC(STR(?s SEPARATOR=\",\"))) AS ?g) WHERE { ?s ?p ?o...\n                                               ^"
      REJECTS
   \ From terms.f.
   s" SELECT * WHERE { ?s ex:p ?o }"
   s\" 1:21: expected a declared prefix, found `ex:p`; declare it with PREFIX ex: <...>\n1 | SELECT * WHERE { ?s ex:p ?o }\n                        ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 }"
   s\" 1:41: expected ), found `}`; write ) here\n1 | SELECT * WHERE { ?s ?p ?o FILTER(?o = 1 }\n                                            ^"
      REJECTS
   s" SELECT * WHERE { ?s ?p ?o } LIMIT x"
   s\" 1:35: expected a non-negative integer, found `x`; replace it with a non-negative integer\n1 | SELECT * WHERE { ?s ?p ?o } LIMIT x\n                                      ^"
      REJECTS
   s" PREFIX ex:a <urn:x> SELECT * WHERE { ?s ?p ?o }"
   s\" 1:8: expected a prefix such as ex:, found `ex:a`; end the prefix name at its colon\n1 | PREFIX ex:a <urn:x> SELECT * WHERE { ?s ?p ?o }\n           ^"
      REJECTS
   \ A declaration without its prefix, and one without its IRI.
   s" PREFIX <urn:x:> ASK { ?s ex:p ?o }"
   s\" 1:8: expected a prefix such as ex:, found `<urn:x:>`; write a prefix such as ex: here\n1 | PREFIX <urn:x:> ASK { ?s ex:p ?o }\n           ^"
      REJECTS
   s" PREFIX ex: ASK { ?s ex:p ?o }"
   s\" 1:12: expected an IRI in <...>, found `ASK`; write an IRI in <...> here\n1 | PREFIX ex: ASK { ?s ex:p ?o }\n               ^"
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

\ A fix whose query is rejected at the same place with the same text
\ leaves its mistake.
: STUCK ( ^ARENA:arena -- )
   {: arena :}
   s" ASK { ?s ?p }" {: q:str :}
   q REJECTION arena ARENA:COPY {: r:str :}
   q r q arena REMOVED? TFALSE ;

: PROGRESS ( -- )
   s" the progress check fails a fix that leaves its rejection" T-LABEL
   [: STUCK ;] ARENA:WITH ;

PROGRESS

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

\ Parses 250 nested groups around 512 triples over 1,536 variables.
: DEEP ( ^ARENA:arena -- ^syntax )
   {: arena :}
   arena ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   s" SELECT *" out VEC:APPEND
   out s" {" 250 TIMES
   1536 0 ?do
      out i DEEP-VAR
      i 3 mod 2 = if s" ." out VEC:APPEND then
   loop
   out s" }" 250 TIMES
   out 0 VEC:AT out VEC:COUNT >str arena PARSE ;

\ Rounded up.
: NODES-PER-TOKEN ( ^syntax -- n )
   {: state :}
   state TOKENS {: tokens:n :}
   state SYNTAX-NODES @ SEG-COUNT tokens 1- + tokens / ;

25000000 constant RESOLVED-MOST-BYTES

\ 2,100 relative references in one VALUES block against a base of 4,000
\ segments.
: WIDE-BASE ( ^VEC:vector<u8> -- )
   {: out :}
   s" BASE <http://h" out VEC:APPEND
   out s" /a" 4000 TIMES
   s" /> ASK{VALUES ?x {" out VEC:APPEND
   out s" <a> " 2100 TIMES
   s" }}" out VEC:APPEND ;

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
   s" 1,536 variables under 250 groups make at most 10 nodes per token" T-LABEL
   [: DEEP NODES-PER-TOKEN ;] ARENA:WITH 10 <= TTRUE
   s" 2,100 references on an 8 KB base take under 25 MB" T-LABEL
   [: RESOLVED-BYTES ;] ARENA:WITH
      dup 0 > swap RESOLVED-MOST-BYTES < and TTRUE ;

LIMITS
T-REPORT

;package
;package
