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

public

\ Interpret the buffer as the engine's evaluate reads it, and put the input
\ cells and the using depth back after, whether the buffer ends or a token
\ throws: usings are file-local (habu1.f B-EVAL, habu2.f EM-EVAL-CLEAN-EXIT).
: INTERPRET ( ptr u8 n -- ) {: a:ptr u:n :}
   INP-CELL CELL@ INE-CELL CELL@ SRCLOC:INB-CELL CELL@ USE-DEPTH-CELL CELL@
   {: p:n e:n b:n d:n :}
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   [: RUN ;] catch {: code:n :}
   p INP-CELL CELL!  e INE-CELL CELL!  b SRCLOC:INB-CELL CELL!  d USE-DEPTH-CELL CELL!
   code 0<> if code throw then ;

;package
