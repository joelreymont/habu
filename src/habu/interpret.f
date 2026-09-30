\ interpret.f - the interpret loop written in Habu, OUTER:INTERPRET: it reads a
\ buffer token by token with the readers src/habu/outer.f defines.

require src/habu/outer.f

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
   LITERAL? if exit then
   DISPATCH ;

: RUN ( -- )
   begin TOKEN while STEP repeat ;

public

\ Interpret the buffer as the engine's evaluate reads it, and put the input
\ cells back after, whether the buffer ends or a token throws.
: INTERPRET ( ptr u8 n -- ) {: a:ptr u:n :}
   INP-CELL CELL@ INE-CELL CELL@ SRCLOC:INB-CELL CELL@ {: p:n e:n b:n :}
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   [: RUN ;] catch {: code:n :}
   p INP-CELL CELL!  e INE-CELL CELL!  b SRCLOC:INB-CELL CELL!
   code 0<> if code throw then ;

;package
