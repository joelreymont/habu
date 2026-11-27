\ write.f - a statement's text, written from its rel into a sink. A sink
\ counts the bytes it is given and appends them to its vector, unless that
\ is NULL-PTR, so one writing sizes the text and the next writes it.
\ The writer takes no plan and cannot reject.
\ STORAGE CLASS. CALLER-OWNED: the sink and its vector are the caller's.

require src/lib/allocator.f
require src/lib/vector.f
require src/lib/sparql/statement.f

package SPARQL
private

STRUCTURE sink DERIVE addr
   FIELD count n
   FIELD out ^VEC:vector<u8>
;STRUCTURE

: NEW-SINK ( ^ALLOC:allocator ^VEC:vector<u8> -- ^sink )
   {: al:^ out:^ :}
   al [type] sink ALLOC:NEW {: k:^ :}
   0 k SINK-COUNT ! out k SINK-OUT ! k ;

: SINK+ ( ^sink str -- )
   {: k:^ s:str :}
   s STR:LEN k SINK-COUNT +!
   k SINK-OUT @ NULL-PTR <> if s k SINK-OUT @ VEC:APPEND then ;

\ `WITH ` or `WITH RECURSIVE ` before the rel's first CTE, a comma
\ before any other.
: CTE-SEP$ ( ^rel ^cte -- str )
   {: r:^ c:^ :}
   c r REL-WITH @ <> if s" ," exit then
   r REL-RECURSIVE @ if s" WITH RECURSIVE " else s" WITH " then ;

\ The AS after the CTE's name, which its body follows.
: CTE-AS$ ( ^cte -- str )
   CTE-MATERIALIZED @ if s"  AS MATERIALIZED " else s"  AS " then ;

\ The piece after p in the rel's text, NULL-PTR after its last, which a
\ view's next may pass.
: NEXT-PIECE ( ^rel ^piece -- ^piece )
   {: r:^ p:^ :}
   p r REL-LAST @ = if NULL-PTR else p PIECE-NEXT @ then ;

\ The rel's WITH and a space when it has CTEs, its pieces, its open text; one
\ word writes CTE bodies and child rels, since either may hold the other.
: WRITE-REL ( ^sink ^rel -- )
   {: k:^ r:^ :}
   r REL-WITH @ begin dup NULL-PTR <> while
      {: c:^ :} k r c CTE-SEP$ SINK+ k c CTE-NAME @ SINK+
      k c CTE-AS$ SINK+ k c CTE-BODY @ RECURSE c CTE-NEXT @
   repeat drop
   r REL-WITH @ NULL-PTR <> if k s"  " SINK+ then
   r REL-FIRST @ begin dup NULL-PTR <> while
      {: p:^ :} p PIECE-REL @ {: ch:^ :}
      ch NULL-PTR = if k p PIECE-TEXT @ SINK+ else k ch RECURSE then
      r p NEXT-PIECE
   repeat drop
   r OPEN? if k r OPEN$ SINK+ then ;

\ The bytes WRITE-REL writes of the rel, counted without writing or
\ allocating.
: SQL-LEN ( ^rel -- n )
   {: r:^ :}
   0 r REL-WITH @ begin dup NULL-PTR <> while
      {: c:^ :} r c CTE-SEP$ STR:LEN + c CTE-NAME @ STR:LEN +
      c CTE-AS$ STR:LEN + c CTE-BODY @ RECURSE + c CTE-NEXT @
   repeat drop
   r REL-WITH @ NULL-PTR <> if 1+ then
   r REL-FIRST @ begin dup NULL-PTR <> while
      {: p:^ :} p PIECE-REL @ {: ch:^ :}
      ch NULL-PTR = if p PIECE-TEXT @ STR:LEN else ch RECURSE then +
      r p NEXT-PIECE
   repeat drop
   r REL-LEN @ + ;

\ The rel's copies and those of each CTE body and child rel it holds, as
\ SQL-LEN walks them, counted without writing or allocating.
: COPIES ( ^rel -- n )
   {: r:^ :}
   r REL-COPIES @ r REL-WITH @ begin dup NULL-PTR <> while
      {: c:^ :} c CTE-BODY @ RECURSE + c CTE-NEXT @
   repeat drop
   r REL-FIRST @ begin dup NULL-PTR <> while
      {: p:^ :} p PIECE-REL @ {: ch:^ :}
      ch NULL-PTR <> if ch RECURSE + then
      r p NEXT-PIECE
   repeat drop ;

\ The rel's WITH and text: a leaf's open text in place, any other rel
\ written into a new vector, which nothing appends to.
: SQL$ ( ^rel -- str )
   {: r:^ :}
   r LEAF? if
      r OPEN? 0= if s" " exit then
      r OPEN$ exit
   then
   r REL-TEXTS @ TEXTS-AL @ {: al:^ :}
   al 0 [type] u8 VEC:NEW {: v:^ :}
   al v NEW-SINK r WRITE-REL v VEC:BYTES ;

\ Whether the rels write one text. Two leaves compare their open text;
\ any other pair of distinct rels of one length is written and compared.
: REL= ( ^rel ^rel -- bool )
   {: a:^ b:^ :}
   a b = if true exit then
   a LEAF? b LEAF? and if a SQL$ b SQL$ STR:= exit then
   a SQL-LEN b SQL-LEN <> if false exit then
   a SQL$ b SQL$ STR:= ;

;package
