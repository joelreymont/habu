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
: CTE-SEP ( ^sink ^rel ^cte -- )
   {: k:^ r:^ c:^ :}
   c r REL-WITH @ <> if k s" ," SINK+ exit then
   r REL-RECURSIVE @ if s" WITH RECURSIVE " else s" WITH " then
   k swap SINK+ ;

\ The CTE's name and AS, which its body follows.
: CTE-HEAD ( ^sink ^cte -- )
   {: k:^ c:^ :}
   k c CTE-NAME @ SINK+
   c CTE-MATERIALIZED @ if s"  AS MATERIALIZED " else s"  AS " then
   k swap SINK+ ;

\ The rel's WITH and a space, when it has CTEs, then its pieces from
\ first through last, which a view's next may pass, then its open text.
\ It writes each CTE's body and child rel too, as one word, since either
\ may hold the other.
: WRITE-REL ( ^sink ^rel -- )
   {: k:^ r:^ :}
   r REL-WITH @ begin dup NULL-PTR <> while
      {: c:^ :} k r c CTE-SEP k c CTE-HEAD
      k c CTE-BODY @ RECURSE c CTE-NEXT @
   repeat drop
   r REL-WITH @ NULL-PTR <> if k s"  " SINK+ then
   r REL-FIRST @ begin dup NULL-PTR <> while
      {: p:^ :} p PIECE-REL @ {: ch:^ :}
      ch NULL-PTR = if k p PIECE-TEXT @ SINK+ else k ch RECURSE then
      p r REL-LAST @ = if NULL-PTR else p PIECE-NEXT @ then
   repeat drop
   r REL-OPEN @ NULL-PTR <> if k r REL-OPEN @ VEC:BYTES SINK+ then ;

\ The rel's WITH and text. The bytes hold until the rel is appended to
\ again, as a vector's do.
: SQL$ ( ^rel -- str )
   {: r:^ :}
   r REL-FIRST @ NULL-PTR = r REL-WITH @ NULL-PTR = and if
      r REL-OPEN @ NULL-PTR = if s" " exit then
      r REL-OPEN @ VEC:BYTES exit
   then
   r REL-AL @ 0 [type] u8 VEC:NEW {: v:^ :}
   r REL-AL @ v NEW-SINK r WRITE-REL v VEC:BYTES ;

;package
