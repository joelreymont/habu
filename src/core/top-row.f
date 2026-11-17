\ top-row.f - the typed top level: the checker's row over the interpret stream.
\ The interpreter calls TOP-LEVEL with ( token-a token-u class ) per
\ interpreted token, a word's before the word runs, or TOP-MOVE in its place
\ for a word that moves a whole value; per token because a line may define a
\ word a later token calls.
\ The checker keeps one row per evaluator stack base and evaluate level; a
\ rejected word or tick throws rc 70.

package CHECK

: TR-PUSHED ( n -- n ) {: cls:n :}
   cls TOP-EV-WORD = IF 0 EXIT THEN
   cls TOP-EV-STR = IF 2 EXIT THEN
   1 ;

\ `depth` is the anchor: each event fits the row to the live depth less the
\ cells the event pushed, so a step the row cannot model leaves unknown cells,
\ which a typed consumer rejects.
: TOP-LEVEL ( ^u8 n n -- )
   {: a:^ u:n cls:n :}
   data-base STACK-ABI:BASE-CELL + @ CHECKER-TOP-SEGMENT
   depth cls TR-PUSHED - {: live:n :}
   cls TOP-EV-WORD <>  cls TOP-EV-TICK <>  and IF a u cls live CHECKER-TOP-PUSH EXIT THEN
   cls TOP-EV-WORD = IF a u live CHECKER-TOP-WORD ELSE a u live CHECKER-TOP-TICK THEN
   CHECKER-TOP-SAVE ;

\ Before keyword a u takes the top cell for the name w v: an empty stack, a
\ multi-cell value or a cell its row rejects is rejected, rc 70, before it is taken.
: TOP-TAKE ( ^u8 n ^u8 n -- )
   {: a:^ u:n w:^ v:n :}
   data-base STACK-ABI:BASE-CELL + @ CHECKER-TOP-SEGMENT
   depth {: live:n :}
   a u w v live CHECKER-TOP-TAKE ;

\ Judges the engine `drop`, `@` or `!` as a body's (a linear item rejected,
\ rc 70), steps the row and answers the width in cells of the value it moves.
: TOP-MOVE ( -- n )
   data-base STACK-ABI:BASE-CELL + @ CHECKER-TOP-SEGMENT
   depth CHECKER-TOP-MOVE ;

;package
