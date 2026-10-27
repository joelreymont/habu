\ top-row.f - the typed top level: the checker's row over the interpret stream.
\
\ The engine emits one event per interpreted token to the installed top-row hook
\ (habu2.f LTOPHOOK; a WORD event fires before the word runs, C-TOPHOOK-CALL):
\ ( token-a token-u class flags ). Per token, not per line, because top-level
\ tokens resolve as they execute: a line may define a word a later token calls.
\ The checker keeps the row between events (src/core/checker.f "the interpret
\ row"), one per evaluator stack base and evaluate level, and judges each event
\ as a colon body judges the same token: a refused word or tick throws the
\ checker's rc 70 before the word runs, with the body's diagnostic.
\
\ `depth` is the anchor: each event fits the row to the live depth less the
\ cells the event already pushed, so a step the row cannot model leaves unknown
\ cells, which a typed consumer refuses. A word or tick event loads the row,
\ steps it and saves it; a literal over a saved row that fits the live depth
\ pushes its constant cells onto that row, at a cost independent of the row's
\ depth, and otherwise takes the load, step and save (CHECKER-TOP-PUSH). Inside a
\ `0 set-check` window the hook steps nothing and forgets its row, so the cells
\ the window left are unknown; the rows it does not run on keep their cells. The
\ engine's `evaluate` still widens its row's segment there, so the text it reads
\ continues the row.

package TOP-ROW

\ ---- event-class ABI (src/habu/layout.f TOP-EV-*, loaded after this file) -----
1 constant TR-EV-NUM     2 constant TR-EV-STR     3 constant TR-EV-CSTR
4 constant TR-EV-CHAR    5 constant TR-EV-TICK    6 constant TR-EV-WORD

\ The cells an event pushed before it fired.
: TR-PUSHED ( n -- n ) {: cls:n :}
   cls TR-EV-WORD = IF 0 EXIT THEN
   cls TR-EV-STR = IF 2 EXIT THEN
   1 ;

: TR-HOOK ( ptr u8 n n n -- ) {: a:ptr u:n cls:n flg:n :}
   data-base STACK-ABI:BASE-CELL + @ CHECKER-TOP-SEGMENT
   check@ 0= IF
      CHECKER-TOP-FORGET  cls TR-EV-WORD = IF a u CHECKER-TOP-WIDEN THEN  EXIT
   THEN
   depth cls TR-PUSHED - {: live:n :}
   cls TR-EV-WORD <>  cls TR-EV-TICK <>  and IF a u cls live CHECKER-TOP-PUSH EXIT THEN
   live CHECKER-TOP-LOAD
   cls TR-EV-WORD = IF a u CHECKER-TOP-WORD ELSE a u CHECKER-TOP-TICK THEN
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
