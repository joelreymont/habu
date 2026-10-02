\ interpret.f - the interpret loop written in Habu, OUTER:INTERPRET: it reads a
\ buffer token by token with the readers src/habu/outer.f defines, the
\ package keywords src/habu/packages.f defines and the definition heads and
\ body capture src/habu/definers.f defines.

require src/habu/stack-abi.f
require src/habu/outer.f
require src/habu/packages.f
require src/habu/definers.f

package OUTER

private

\ ---- the loop -------------------------------------------------------------------------
\ A number is pushed and a word run: the token's effect on the stack is the
\ program's, so this row, and every row in outer.f that runs program code,
\ states none of it.
TRUSTED: DISPATCH ( -- )
   NUMERAL? if VALUE @ TOP-EV-NUM 0 HOOK exit then
   RUN-WORD ;

: STEP ( -- )
   COMMENT? if exit then
   COMPILING? if exit then
   LITERAL? if exit then
   PACKAGE? if exit then
   DEFINE? if exit then
   DISPATCH ;

\ Whether the buffer is a closed text's (INTERPRET-CLOSED) and the definition
\ open as it began, PEND-CELL then.
variable CLOSED?   0 CLOSED? !
variable CLOSED-PEND   0 CLOSED-PEND !

\ The end of a closed text (habu2.f C-CLOSED-SOURCE-END), with the input cells
\ still the buffer's: a definition the buffer opened and left open is refused,
\ named, at the buffer's end, and the throw rolls it back (habu2.f LEVALREC).
\ One already open when the buffer began may still be open.
: CLOSED-END ( -- )
   CLOSED? @ 0= if exit then
   PEND-CELL CELL@ {: now:n :}
   now 0= if exit then
   now CLOSED-PEND @ = if exit then
   s" hb: closed text ended inside a definition: " SAY
   DEF-CAPTURED-NAME SAY
   STACK-ABI:E-EVAL-UNFINISHED THROW-AT ;

: RUN ( -- )
   begin TOKEN while STEP repeat
   CLOSED-END ;

\ Run the buffer under catch and give its code. A throw also puts back the d
\ used publics below depth d, the includer's: a buffer that closes the
\ includer's package and opens a using writes into one of them (habu1.f B-EVAL
\ keeps the engine's in its frame). Each call keeps one slot.
: RUN-CAUGHT ( n -- n ) {: d:n :}
   d 0= if [: RUN ;] catch exit then
   d 1- cells USE-WIDS-OFF + CELL@ {: wid:n :}
   d 1- RECURSE {: code:n :}
   code 0<> if wid d 1- cells USE-WIDS-OFF + CELL! then
   code ;

\ Interpret the buffer as the engine's evaluate reads it, and put the input
\ cells and the using depth back after, whether the buffer ends or a token
\ throws: usings are file-local (habu1.f B-EVAL, habu2.f EM-EVAL-CLEAN-EXIT).
\ A throw also puts back the package scope the buffer entered with, its using
\ floor included (habu2.f LEVALREC), so a package a throwing file opened is
\ closed and one it closed is open again, and the used publics RUN-CAUGHT
\ kept, which the checker's resync names. A clean end puts the depth back to
\ the buffer's using floor (packages.f USE-FLOOR), which is the entry depth
\ unless a `;package` closed a package opened before the buffer and restored
\ a lower one (habu2.f EM-EVAL-CLEAN-EXIT). A package the buffer left open
\ keeps none of its usings: the package's using floor drops to the restored
\ depth. A clean end also clears the evaluate error cell, as the engine's does:
\ an `evaluate` the buffer caught recorded its code there, and include.f reads
\ a nonzero cell after the buffer as a failed evaluation.
: INTERPRET-AS ( ptr u8 n bool -- )
   {: a:ptr u:n closed:bool :}
   INP-CELL CELL@ INE-CELL CELL@ SRCLOC:INB-CELL CELL@ USE-DEPTH-CELL CELL@
   {: p:n e:n b:n d:n :}
   PKG-STATE {: rec:n parent:n cur:n floor:n :}
   USE-FLOOR @ {: outer:n :}
   CLOSED? @ CLOSED-PEND @ {: closed0:n pend0:n :}
   closed CLOSED? !  PEND-CELL CELL@ CLOSED-PEND !
   d USE-FLOOR !
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   d RUN-CAUGHT {: code:n :}
   closed0 CLOSED? !  pend0 CLOSED-PEND !
   USE-FLOOR @ USE-DEPTH-CELL CELL@ min {: back:n :}
   outer USE-FLOOR !
   p INP-CELL CELL!  e INE-CELL CELL!  b SRCLOC:INB-CELL CELL!
   code 0<> if d USE-DEPTH-CELL CELL!  rec parent cur floor PKG-RECOVER  code throw then
   back USE-DEPTH-CELL CELL!
   0 EVALERR-CELL CELL!
   USE-PKG-SAVE-CELL CELL@ back > if back USE-PKG-SAVE-CELL CELL! then ;

public

: INTERPRET ( ptr u8 n -- )
   false INTERPRET-AS ;

\ The buffer as a closed text's, as evaluate-closed reads one: INTERPRET, and a
\ definition the buffer opens is refused at its end if it is still open
\ (CLOSED-END above).
: INTERPRET-CLOSED ( ptr u8 n -- )
   true INTERPRET-AS ;

;package
