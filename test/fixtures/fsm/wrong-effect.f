split-str
\ wrong-effect.f - the ACTION on line 15, `bump`, leaves a cell an action may
\ not, so the line's word is rejected there, the action named as the table
\ spells it.
require src/lib/fsm.f
package FSM-WRONG
ENUM ph idle busy ;ENUM
ENUM sig go halt ;ENUM
: S ( n -- sig ) drop construct sig go ;
: BUMP ( n -- n ) 1 + ;
using FSM
DEFINE M ( n ) ph sig
IN idle S
   halt - >idle
   go
   bump
   >busy
FINAL busy -
;DEFINE
;using
;package
