\ undefined-action.f - src/lib/fsm-test.f PLACED: the ACTION on line 14 names
\ no word; DEFINE builds the line's word after reading the whole table, and
\ its rejection names that line, not the `;DEFINE` below it.
require src/lib/fsm.f
package FSM-UNDEF
ENUM ph idle busy ;ENUM
ENUM sig go halt ;ENUM
: S ( n -- sig ) drop construct sig go ;
using FSM
DEFINE M ( n ) ph sig
IN idle S
   halt - >idle
   go
   NO-SUCH-ACTION
   >busy
FINAL busy -
;DEFINE
;using
;package
