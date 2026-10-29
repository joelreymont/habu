\ top-row.f - the typed top level: the checker's row over the interpret stream.
\ The engine emits ( token-a token-u class flags ) per interpreted token to the
\ installed hook (habu2.f LTOPHOOK; a WORD event fires before the word runs,
\ C-TOPHOOK-CALL); per token because a line may define a word a later token calls.
\ The checker keeps one row per evaluator stack base and evaluate level
\ (src/core/checker.f "the interpret row"); a refused word or tick throws rc 70.

package TOP-ROW

\ ---- event-class ABI (src/habu/layout.f TOP-EV-*, loaded after this file) -----
1 constant TR-EV-NUM     2 constant TR-EV-STR     3 constant TR-EV-CSTR
4 constant TR-EV-CHAR    5 constant TR-EV-TICK    6 constant TR-EV-WORD

: TR-PUSHED ( n -- n ) {: cls:n :}
   cls TR-EV-WORD = IF 0 EXIT THEN
   cls TR-EV-STR = IF 2 EXIT THEN
   1 ;
\ `depth` is the anchor: each event fits the row to the live depth less the
\ cells the event pushed, so a step the row cannot model leaves unknown cells,
\ which a typed consumer refuses. Inside a `0 set-check` window the hook steps
\ nothing and forgets its row, so the cells the window left are unknown; the
\ engine's `evaluate` still widens the row's segment there, so the text it reads
\ continues the row.

: TR-HOOK ( ptr u8 n n n -- )
   {: a:ptr u:n cls:n flg:n :}
   data-base STACK-ABI:BASE-CELL + @ CHECKER-TOP-SEGMENT
   check@ 0= IF
      CHECKER-TOP-FORGET  cls TR-EV-WORD = IF a u CHECKER-TOP-WIDEN THEN  EXIT
   THEN
   depth cls TR-PUSHED - {: live:n :}
   cls TR-EV-WORD <>  cls TR-EV-TICK <>  and IF a u cls live CHECKER-TOP-PUSH EXIT THEN
   cls TR-EV-WORD = IF a u live CHECKER-TOP-WORD ELSE a u live CHECKER-TOP-TICK THEN
   CHECKER-TOP-SAVE ;

\ set-top-check is a guarded-deref trust-boundary prim. TOP-ROW's private row
\ (src/habu/prims.f) takes a hook of TR-HOOK's shape, so this install is
\ checked; outside TOP-ROW only a TRUSTED: body may call it. The audited escape
\ window is the set-top-check install itself: only TR-HOOK may be installed
\ (checked-boundary lint UB-TOP-HOOK-ALLOWED? audit row).
: TR-INSTALL ( -- )
   ['] TR-HOOK set-top-check ;

TR-INSTALL

;package
