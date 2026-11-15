\ top-row.f - the typed top level: the checker's row over the interpret stream.
\ The interpreter calls CHECK:TOP-LEVEL with ( token-a token-u class flags ) per
\ interpreted token but the engine `drop`, which calls TOP-DROP
\ (src/habu/interpreter.f HOOK; a WORD event fires before the word runs), per
\ token because a line may define a word a later token calls.
\ The checker keeps one row per evaluator stack base and evaluate level
\ (src/core/checker.f "the interpret row"); a rejected word or tick throws rc 70.

package CHECK

: TR-PUSHED ( n -- n ) {: cls:n :}
   cls TOP-EV-WORD = IF 0 EXIT THEN
   cls TOP-EV-STR = IF 2 EXIT THEN
   1 ;

public

\ `depth` is the anchor: each event fits the row to the live depth less the
\ cells the event pushed, so a step the row cannot model leaves unknown cells,
\ which a typed consumer rejects.
: TOP-LEVEL ( ^u8 n n n -- )
   {: a:^ u:n cls:n flg:n :}
   data-base STACK-ABI:BASE-CELL + @ CHECKER-TOP-SEGMENT
   depth cls TR-PUSHED - {: live:n :}
   cls TOP-EV-WORD <>  cls TOP-EV-TICK <>  and IF a u cls live CHECKER-TOP-PUSH EXIT THEN
   cls TOP-EV-WORD = IF a u live CHECKER-TOP-WORD ELSE a u live CHECKER-TOP-TICK THEN
   CHECKER-TOP-SAVE ;

\ Before a keyword spelled a u takes the program's top cell: a multi-cell value
\ there is rejected, rc 70, before any cell of it is taken.
: TOP-TAKE ( ^u8 n -- )
   {: a:^ u:n :}
   data-base STACK-ABI:BASE-CELL + @ CHECKER-TOP-SEGMENT
   a u depth CHECKER-TOP-TAKE ;

\ Judges the engine `drop` as a body's (a linear item rejected, rc 70), takes the
\ top item off the row and answers its width in cells.
: TOP-DROP ( -- n )
   data-base STACK-ABI:BASE-CELL + @ CHECKER-TOP-SEGMENT
   depth CHECKER-TOP-DROP ;

private

\ set-top-check is a guarded-deref trust-boundary prim. CHECK's private row
\ (src/habu/prims.f) takes a hook of TOP-LEVEL's shape, so this install is
\ checked; outside CHECK only a TRUSTED: body may call it. The audited escape
\ window is the set-top-check install itself: only TOP-LEVEL may be installed
\ (checked-boundary lint UB-TOP-HOOK-ALLOWED? audit row). As check-hook.f
\ INSTALL, it leaves a builder's tracker in place inside a build.
: TR-INSTALL ( -- )
   BUILDING? IF EXIT THEN
   ['] TOP-LEVEL set-top-check ;

TR-INSTALL

;package
