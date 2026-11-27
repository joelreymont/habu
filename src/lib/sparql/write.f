\ write.f - a statement's text, written from its rel into a sink. A sink
\ counts the bytes it is given and appends them to its vector, unless that
\ is NULL-PTR, so one writing sizes the text and the next writes it.
\ The writer takes no plan and cannot reject.
\ REL= reads two rels' texts at once, with a cursor each: a stack of
\ frames, one per rel the cursor is inside, the innermost last, each the
\ part of its rel reached and the bytes of it passed. It compares the
\ shorter of the two strings the cursors stand in, then moves both past
\ it, so it reads each byte once.
\ STORAGE CLASS. CALLER-OWNED: the sink, its vector and REL='s cursors
\ are the caller's.

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

\ Where in its rel a cursor stands: its CTE's separator, name, AS or
\ body, the space after the WITH, its piece's text or child, the open
\ text.
ENUM part sep name as body space text child open ;ENUM

\ A rel a cursor is inside: the part it stands at, of the CTE or piece
\ named, and the bytes of that part passed.
STRUCTURE frame DERIVE addr
   FIELD rel ^rel
   FIELD cte ^cte
   FIELD piece ^piece
   FIELD part part
   FIELD off n
;STRUCTURE

\ REL='s two cursors, emptied at each call.
STRUCTURE cursors DERIVE addr
   FIELD a ^VEC:vector<frame>
   FIELD b ^VEC:vector<frame>
;STRUCTURE

: NEW-CURSORS ( ^ALLOC:allocator -- ^cursors )
   {: al:^ :}
   al [type] cursors ALLOC:NEW {: cs:^ :}
   al 0 [type] frame VEC:NEW cs CURSORS-A !
   al 0 [type] frame VEC:NEW cs CURSORS-B ! cs ;

: INNER ( ^VEC:vector<frame> -- ^frame )
   {: v:^ :} v v VEC:COUNT 1- VEC:AT ;

\ The frame stands at the CTE's separator, or past the last CTE at the
\ space after the WITH.
: AT-CTE ( ^frame ^cte -- )
   {: f:^ c:^ :}
   c f FRAME-CTE !
   c NULL-PTR = if PART-SPACE else PART-SEP then f FRAME-PART ! ;

\ The frame stands at the piece's text, or past the last piece at the
\ open text.
: AT-PIECE ( ^frame ^piece -- )
   {: f:^ p:^ :}
   p f FRAME-PIECE !
   p NULL-PTR = if PART-OPEN else PART-TEXT then f FRAME-PART ! ;

\ Moves the cursor into the rel, to its first byte; NULL-PTR leaves it.
: ENTER-REL ( ^VEC:vector<frame> ^rel -- )
   {: v:^ r:^ :}
   r NULL-PTR = if exit then
   v VEC:PUSH {: f:^ :} r f FRAME-REL ! 0 f FRAME-OFF !
   r REL-WITH @ NULL-PTR = if f r REL-FIRST @ AT-PIECE exit then
   f r REL-WITH @ AT-CTE ;

\ Moves the cursor past the part it stands at: from a body or child into
\ its rel, from the open text out of the rel.
: NEXT-PART ( ^VEC:vector<frame> -- )
   {: v:^ :}
   v INNER {: f:^ :} 0 f FRAME-OFF !
   f FRAME-REL @ {: r:^ :} f FRAME-CTE @ {: c:^ :}
   f FRAME-PIECE @ {: p:^ :}
   f FRAME-PART @ MATCH part
      sep OF PART-NAME f FRAME-PART ! ENDOF
      name OF PART-AS f FRAME-PART ! ENDOF
      as OF PART-BODY f FRAME-PART ! ENDOF
      body OF f c CTE-NEXT @ AT-CTE v c CTE-BODY @ ENTER-REL ENDOF
      space OF f r REL-FIRST @ AT-PIECE ENDOF
      text OF PART-CHILD f FRAME-PART ! ENDOF
      child OF
         f r p NEXT-PIECE AT-PIECE v p PIECE-REL @ ENTER-REL
      ENDOF
      open OF v v VEC:COUNT 1- VEC:RESIZE ENDOF
   ;MATCH ;

\ The part's string past the bytes passed; empty at a body or child.
: PART$ ( ^frame -- str )
   {: f:^ :}
   f FRAME-REL @ {: r:^ :} f FRAME-CTE @ {: c:^ :}
   f FRAME-PART @ MATCH part
      sep OF r c CTE-SEP$ ENDOF
      name OF c CTE-NAME @ ENDOF
      as OF c CTE-AS$ ENDOF
      body OF s" " ENDOF
      space OF s"  " ENDOF
      text OF f FRAME-PIECE @ PIECE-TEXT @ ENDOF
      child OF s" " ENDOF
      open OF r OPEN? if r OPEN$ else s" " then ENDOF
   ;MATCH f FRAME-OFF @ STR-SKIP ;

\ The bytes from the cursor to the end of the string it stands in, the
\ cursor first moved past ended parts; empty past the text's end.
: LEFT$ ( ^VEC:vector<frame> -- str )
   {: v:^ :}
   begin v VEC:COUNT 0<> while
      v INNER PART$ {: s:str :}
      s STR:LEN 0<> if s exit then
      v NEXT-PART
   repeat s" " ;

: PASS ( ^VEC:vector<frame> n -- )
   {: v:^ k:n :} k v INNER FRAME-OFF +! ;

\ The bytes both cursors stand before in one string each, whether they
\ are the same, and the cursors moved past them; 0 past either's end.
: SAME-STRETCH ( ^VEC:vector<frame> ^VEC:vector<frame> -- n bool )
   {: x:^ y:^ :}
   x LEFT$ y LEFT$ {: s:str t:str :}
   s STR:LEN t STR:LEN min {: m:n :}
   m 0= if 0 s STR:LEN t STR:LEN = exit then
   x m PASS y m PASS
   m s m STR-TAKE t m STR-TAKE STR:= ;

\ Whether the rels write one text.
: REL= ( ^cursors ^rel ^rel -- bool )
   {: cs:^ a:^ b:^ :}
   a b = if true exit then
   a SQL-LEN b SQL-LEN <> if false exit then
   cs CURSORS-A @ {: x:^ :} cs CURSORS-B @ {: y:^ :}
   x 0 VEC:RESIZE x a ENTER-REL
   y 0 VEC:RESIZE y b ENTER-REL
   begin
      x y SAME-STRETCH {: m:n same:bool :}
      same 0= if false exit then
      m 0=
   until true ;

;package
