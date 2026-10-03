\ interpret.f - the interpret loop written in Habu, OUTER:INTERPRET: it reads a
\ buffer token by token with the readers src/habu/outer.f defines, the
\ package keywords src/habu/packages.f defines and the definition heads and
\ body capture src/habu/definers.f defines.

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

: RUN ( -- )
   begin TOKEN while STEP repeat ;

\ Run the buffer under catch, refusing at its end a definition it opened, and
\ give its code. A throw also puts back the d used publics below depth d, the
\ includer's: a buffer that closes the includer's package and opens a using
\ writes into one of them (habu1.f B-EVAL keeps the engine's in its frame).
\ Each call keeps one slot.
: RUN-CAUGHT ( n -- n )
   {: d:n :}
   d 0= if [: RUN DEF-SOURCE-END ;] catch exit then
   d 1- cells USE-WIDS-OFF + CELL@ {: wid:n :}
   d 1- RECURSE {: code:n :}
   code 0<> if wid d 1- cells USE-WIDS-OFF + CELL! then
   code ;

public

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
\ a nonzero cell after the buffer as a failed evaluation. A definition the
\ buffer opened and left open is refused at its end, while the input cells
\ still name the buffer, and a throw gives up a definition the buffer opened:
\ the record pending as it began is pending again (definers.f ENTRY-PEND).
: INTERPRET ( ptr u8 n -- )
   {: a:ptr u:n :}
   INP-CELL CELL@ INE-CELL CELL@ SRCLOC:INB-CELL CELL@ USE-DEPTH-CELL CELL@
   ENTRY-PEND @
   {: p:n e:n b:n d:n o:n :}
   PKG-STATE {: rec:n parent:n cur:n floor:n :}
   USE-FLOOR @ {: outer:n :}
   d USE-FLOOR !
   PEND-CELL CELL@ {: entry:n :}
   entry ENTRY-PEND !
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   d RUN-CAUGHT {: code:n :}
   USE-FLOOR @ USE-DEPTH-CELL CELL@ min {: back:n :}
   outer USE-FLOOR !
   p INP-CELL CELL!  e INE-CELL CELL!  b SRCLOC:INB-CELL CELL!
   o ENTRY-PEND !
   code 0<> if entry PEND-CELL CELL!  d USE-DEPTH-CELL CELL!  rec parent cur floor PKG-RECOVER  code throw then
   back USE-DEPTH-CELL CELL!
   0 EVALERR-CELL CELL!
   USE-PKG-SAVE-CELL CELL@ back > if back USE-PKG-SAVE-CELL CELL! then ;

;package
