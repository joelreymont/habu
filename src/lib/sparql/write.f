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

\ The rel's WITH and a space when it has CTEs, its pieces, each its text
\ then its child, its open text; one word writes CTE bodies and child rels,
\ since either may hold the other.
: WRITE-REL ( ^sink ^rel -- )
   {: k:^ r:^ :}
   r REL-WITH @ begin dup NULL-PTR <> while
      {: c:^ :} k r c CTE-SEP$ SINK+ k c CTE-NAME @ SINK+
      k c CTE-AS$ SINK+ k c CTE-BODY @ RECURSE c CTE-NEXT @
   repeat drop
   r REL-WITH @ NULL-PTR <> if k s"  " SINK+ then
   r REL-FIRST @ begin dup NULL-PTR <> while
      {: p:^ :} k p PIECE-TEXT @ SINK+ p PIECE-REL @ {: ch:^ :}
      ch NULL-PTR <> if k ch RECURSE then
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
      {: p:^ :} p PIECE-TEXT @ STR:LEN + p PIECE-REL @ {: ch:^ :}
      ch NULL-PTR <> if ch RECURSE + then
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

\ The string's bytes from n on.
: STR-SKIP ( str n -- str )
   {: s:str k:n :} s STR:DATA k + s STR:LEN k - SPAN:MAKE SPAN:$ ;

\ The string's first n bytes.
: STR-TAKE ( str n -- str )
   {: s:str k:n :} s STR:DATA k SPAN:MAKE SPAN:$ ;

\ The bytes WRITE-REL writes of the child, none for NULL-PTR.
: CHILD-LEN ( ^rel -- n )
   {: c:^ :} c NULL-PTR = if 0 else c SQL-LEN then ;

\ SEG answers where byte n of a rel's text is as a string, a child rel and
\ an offset: the string's bytes from n, NULL-PTR and 0 when the byte is in
\ a string; the empty string, the child and n within it when in a child;
\ the empty string, NULL-PTR and n past the end when in neither.

\ Byte n of a string then a child, NULL-PTR for none, which ends after it.
: ITEM-BYTE ( str ^rel n -- str ^rel n )
   {: s:str c:^ k:n :}
   k s STR:LEN < if s k STR-SKIP NULL-PTR 0 exit then
   s" " c k s STR:LEN - ;

: CTE-LEN ( ^rel ^cte -- n )
   {: r:^ c:^ :}
   r c CTE-SEP$ STR:LEN c CTE-NAME @ STR:LEN +
   c CTE-AS$ STR:LEN + c CTE-BODY @ SQL-LEN + ;

\ Byte n of the CTE of the rel's WITH, which ends after it.
: CTE-BYTE ( ^rel ^cte n -- str ^rel n )
   {: r:^ c:^ k:n :}
   r c CTE-SEP$ {: sep:str :} c CTE-NAME @ {: name:str :}
   k sep STR:LEN < if sep NULL-PTR k ITEM-BYTE exit then
   k sep STR:LEN - {: j:n :}
   j name STR:LEN < if name NULL-PTR j ITEM-BYTE exit then
   c CTE-AS$ c CTE-BODY @ j name STR:LEN - ITEM-BYTE ;

\ Byte n of the rel's WITH and the space after it.
: WITH-BYTE ( ^rel n -- str ^rel n )
   {: r:^ n:n :}
   n r REL-WITH @ begin dup NULL-PTR <> while
      {: k:n c:^ :} r c CTE-LEN {: m:n :}
      k m < if r c k CTE-BYTE exit then
      k m - c CTE-NEXT @
   repeat drop {: j:n :}
   r REL-WITH @ NULL-PTR = if s" " NULL-PTR j exit then
   s"  " NULL-PTR j ITEM-BYTE ;

\ Byte n of the rel's pieces and open text.
: PIECES-BYTE ( ^rel n -- str ^rel n )
   {: r:^ n:n :}
   n r REL-FIRST @ begin dup NULL-PTR <> while
      {: k:n p:^ :} p PIECE-TEXT @ {: t:str :} p PIECE-REL @ {: c:^ :}
      t STR:LEN c CHILD-LEN + {: m:n :}
      k m < if t c k ITEM-BYTE exit then
      k m - r p NEXT-PIECE
   repeat drop {: j:n :}
   r OPEN? 0= if s" " NULL-PTR j exit then
   r OPEN$ NULL-PTR j ITEM-BYTE ;

\ Byte n of the rel's text, not looking into its children.
: FIND ( ^rel n -- str ^rel n )
   {: r:^ k:n :}
   r k WITH-BYTE {: s:str c:^ j:n :}
   s STR:LEN 0<> c NULL-PTR <> or if s c j exit then
   r j PIECES-BYTE ;

\ The rel's bytes from n to the end of the string holding byte n; empty
\ from its length on.
: SEG ( ^rel n -- str )
   FIND {: s:str c:^ k:n :}
   c NULL-PTR = if s exit then c k RECURSE ;

\ The bytes from n both rels write in one string each, and whether they
\ are the same; n is less than their length.
: SAME-RUN ( ^rel ^rel n -- n bool )
   {: a:^ b:^ i:n :}
   a i SEG b i SEG {: s:str t:str :}
   s STR:LEN t STR:LEN min {: m:n :}
   m s m STR-TAKE t m STR-TAKE STR:= ;

\ Whether the rels write one text, compared run by run, allocating nothing.
: REL= ( ^rel ^rel -- bool )
   {: a:^ b:^ :}
   a b = if true exit then
   a SQL-LEN {: n:n :}
   n b SQL-LEN <> if false exit then
   0 begin dup n < while
      {: i:n :} a b i SAME-RUN {: m:n same:bool :}
      same 0= if false exit then
      i m +
   repeat drop true ;

;package
